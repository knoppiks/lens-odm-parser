(ns lens-odm-parser.core
  "The two main functions are parse-file and unparse-file."
  (:require
    [camel-snake-kebab.core :refer [->kebab-case-keyword]]
    [clj-time.coerce :as tc]
    [clj-time.format :as tf]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [clojure.string :as str]
    [odm.clinical-data :as clinical-data]
    [odm.code-list :as code-list]
    [odm.code-list-item :as code-list-item]
    [odm.data-formats]
    [odm.file :as file]
    [odm.form-data :as form-data]
    [odm.form-def :as form-def]
    [odm.item-data :as item-data]
    [odm.item-def :as item-def]
    [odm.item-group-data :as item-group-data]
    [odm.item-group-def :as item-group-def]
    [odm.item-group-ref :as item-group-ref]
    [odm.item-ref :as item-ref]
    [odm.metadata-version :as metadata-version]
    [odm.study :as study]
    [odm.study-event-data :as study-event-data]
    [odm.subject-data :as subject-data])
  (:import
    [java.math MathContext]
    [java.time Instant]))

(set! *warn-on-reflection* true)

(defn- conform-lc-kw
  "Conforms string x to a lower-case keyword."
  [x]
  (if (string? x)
    (-> x str/lower-case keyword)
    ::s/invalid))

(defn- unform-cap-str
  "Unforms keyword x to a capitalized string."
  [x]
  (-> x name str/capitalize))

(defmacro conformer
  "Like s/conformer but takes an optional gen.

  Can be removed when CLJ-1985 is resolved."
  [f unf & {:keys [gen]}] `(s/spec-impl '~f ~f ~gen true ~unf))

;; ---- Specs -----------------------------------------------------------------

(s/def ::tag keyword?)
(s/def ::attrs (s/map-of keyword? string?))
(s/def ::content (s/coll-of (s/or :elem ::element :str string?)))

;; XML element
(s/def ::element
  (s/keys :req-un [::tag ::attrs ::content]))

;; ---- Parsing ---------------------------------------------------------------

(defn- validation-ex [e value error]
  (ex-info (format "Invalid value %s" value)
           {:type ::validation-error :element e :value value :error error}))

(defn- coerce [spec e s]
  (let [v (s/conform spec s)]
    (if (= ::s/invalid v)
      (throw (validation-ex e s (s/explain-data spec s)))
      v)))

(defn string-value [e]
  (apply str (:content e)))

(defn- conform-int [x]
  (cond
    (integer? x) x
    (string? x)
    (try
      (Long/parseLong x)
      (catch Exception _
        (try
          (bigint x)
          (catch Exception _
            ::s/invalid))))
    :else ::s/invalid))

(s/def :odm.xml/integer
  (s/conformer conform-int))

(defn integer-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce :odm.xml/integer e)))

(defn- parse-double-or-bigdec [s]
  (let [bigdec (bigdec s)
        round-to-double (.round bigdec MathContext/DECIMAL64)]
    (if (= round-to-double bigdec)
      (.doubleValue bigdec)
      bigdec)))

(defn- conform-float [x]
  (cond
    (number? x) x
    (string? x)
    (try
      (Long/parseLong x)
      (catch Exception _
        (try
          (parse-double-or-bigdec x)
          (catch Exception _
            ::s/invalid))))
    :else ::s/invalid))

(s/def :odm.xml/float
  (s/conformer conform-float))

(defn float-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce :odm.xml/float e)))

(defn- conform-date-time [x]
  (if (string? x)
    (try
      (tc/to-date (tf/parse (tf/formatters :date-time) x))
      (catch Exception _
        (try
          (tc/to-date (tf/parse (tf/formatters :date-time-no-ms) x))
          (catch Exception _
            ::s/invalid))))
    ::s/invalid))

(defn- unform-date-time [t]
  (if (instance? Instant t)
    (str t)
    (tf/unparse (tf/formatters :date-time) (tc/to-date-time t))))

(defn xml-date-time-gen []
  (gen/fmap (fn [t] (tf/unparse (tf/formatters :date-time) (tc/from-date t)))
            (s/gen inst?)))

(s/def :odm.xml/date-time
  (conformer conform-date-time unform-date-time :gen xml-date-time-gen))

(defn date-time-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce :odm.xml/date-time e)))

(defn boolean-value [e]
  (-> (string-value e)
      (str/trim)
      (case
        "true" true
        "false" false
        "1" true
        "0" false)))

(defn- conform-tx-type [x]
  (->> (conform-lc-kw x)
       (s/conform :odm/tx-type)))

(defn- tag= [tag]
  #(= tag (:tag %)))

(defn- check-tag [tag e]
  (when-not (= tag (:tag e))
    (throw
      (ex-info
        (format "Expected tag %s but got %s." (name tag) (name (:tag e)))
        {:type ::validation-error :element e :expected-tag tag}))))

(defn- check-sub-tag [tag e]
  (when-not (some (tag= tag) (:content e))
    (throw
      (ex-info
        (format "Expected to find element with tag %s as child of element with tag %s."
                (name tag) (name (:tag e)))
        {:type ::validation-error :element e :expected-child-tag tag}))))

(defn- parse-children [tag parse-fn content]
  (into [] (comp (filter (tag= tag)) (map parse-fn)) content))

(defn parse-global-variables
  [{:keys [content] :as global-variables}]
  (check-sub-tag :StudyName global-variables)
  (check-sub-tag :StudyDescription global-variables)
  (check-sub-tag :ProtocolName global-variables)
  (let [string-value #(string-value (first (filter (tag= %) content)))]
    #:odm.study
        {:name (string-value :StudyName)
         :description (string-value :StudyDescription)
         :protocol-name (string-value :ProtocolName)}))

(defn- conform-yes-no [x]
  (case x "Yes" true "No" false ::s/invalid))

(s/def :odm.xml/yes-no
  (s/conformer conform-yes-no #(if % "Yes" "No")))

;; TODO: language tag
(defn parse-translated-text
  [{:keys [attrs] :as translated-text}]
  {:text (string-value translated-text)})

(s/fdef parse-description
  :args (s/cat :description (s/and ::element (tag= :Description)))
  :ret :odm/description)

(defn parse-description
  [{:keys [content]}]
  (parse-children :TranslatedText parse-translated-text content))

(s/fdef parse-question
  :args (s/cat :question (s/and ::element (tag= :Question)))
  :ret ::item-def/question)

(defn parse-question
  [{:keys [content]}]
  (parse-children :TranslatedText parse-translated-text content))

(defn- coerce-oid [e s]
  (coerce :odm.data-formats/oid e s))

(defn- coerce-integer [e s]
  (coerce :odm.xml/integer e s))

(defn- coerce-float [e s]
  (coerce :odm.xml/float e s))

(s/fdef parse-code-list-ref
  :args (s/cat :code-list-ref (s/and ::element (tag= :CodeListRef)))
  :ret :odm/code-list-ref)

(defn parse-code-list-ref
  [{:keys [attrs] :as code-list-ref}]
  #:odm.code-list-ref
      {:code-list-oid (coerce-oid code-list-ref (:CodeListOID attrs))})

(s/fdef parse-alias
  :args (s/cat :alias (s/and ::element (tag= :Alias)))
  :ret :odm/alias)

(defn parse-alias
  [{:keys [attrs] :as alias}]
  #:odm.alias
      {:context (coerce :odm.data-formats/text alias (:Context attrs))
       :name (coerce :odm.data-formats/text alias (:Name attrs))})

(s/fdef parse-item-group-ref
  :args (s/cat :item-group-ref (s/and ::element (tag= :ItemGroupRef)))
  :ret :odm/item-group-ref)

(defn parse-item-group-ref
  [{:keys [attrs content] :as item-group-ref}]
  (cond-> #::item-group-ref
      {:item-group-oid (coerce-oid item-group-ref (:ItemGroupOID attrs))
       :odm/mandatory (coerce :odm.xml/yes-no item-group-ref (:Mandatory attrs))}

    (:OrderNumber attrs)
    (assoc :odm/order-number (coerce-integer item-group-ref (:OrderNumber attrs)))

    (:CollectionExceptionConditionOID attrs)
    (assoc :odm.ref/collection-exception-condition-oid
           (coerce-oid item-group-ref (:CollectionExceptionConditionOID attrs)))))

(s/fdef parse-item-ref
  :args (s/cat :item-ref (s/and ::element (tag= :ItemRef)))
  :ret :odm/item-ref)

(defn parse-item-ref
  [{:keys [attrs content] :as item-ref}]
  (cond-> #::item-ref
      {:item-oid (coerce-oid item-ref (:ItemOID attrs))
       :odm/mandatory (coerce :odm.xml/yes-no item-ref (:Mandatory attrs))}

    (:OrderNumber attrs)
    (assoc :odm/order-number (coerce-integer item-ref (:OrderNumber attrs)))

    (:CollectionExceptionConditionOID attrs)
    (assoc :odm.ref/collection-exception-condition-oid
           (coerce-oid item-ref (:CollectionExceptionConditionOID attrs)))))

(s/fdef parse-form-def
  :args (s/cat :form-def (s/and ::element (tag= :FormDef)))
  :ret :odm/form-def)

(defn parse-form-def
  [{:keys [attrs content] :as form-def}]
  (let [parse-children #(parse-children %1 %2 content)
        description (first (parse-children :Description parse-description))
        item-group-refs (parse-children :ItemGroupRef parse-item-group-ref)]
    (cond-> #::form-def
        {:oid (coerce-oid form-def (:OID attrs))
         :name (coerce :odm.data-formats/name form-def (:Name attrs))
         :odm.def/repeating (coerce :odm.xml/yes-no form-def (:Repeating attrs))}

      description
      (assoc :odm/description description)

      (not (empty? item-group-refs))
      (assoc ::form-def/item-group-refs item-group-refs))))

(s/fdef parse-item-group-def
  :args (s/cat :item-group-def (s/and ::element (tag= :ItemGroupDef)))
  :ret :odm/item-group-def)

(defn parse-item-group-def
  [{:keys [attrs content] :as item-group-def}]
  (let [parse-children #(parse-children %1 %2 content)
        description (first (parse-children :Description parse-description))
        item-refs (parse-children :ItemRef parse-item-ref)]
    (cond-> #::item-group-def
        {:oid (coerce-oid item-group-def (:OID attrs))
         :name (coerce :odm.data-formats/name item-group-def (:Name attrs))
         :odm.def/repeating (coerce :odm.xml/yes-no item-group-def (:Repeating attrs))}

      description
      (assoc :odm/description description)

      (not (empty? item-refs))
      (assoc ::item-group-def/item-refs item-refs))))

(defn- conform-data-type [x]
  (case x
    ("text" "integer" "float" "date" "time" "string" "boolean" "double")
    (keyword x)
    "datetime"
    :date-time
    "partialDatetime"
    :partial-date-time
    "durationDatetime"
    :duration-date-time
    "intervalDatetime"
    :interval-date-time
    "incompleteDatetime"
    :incomplete-date-time
    "base64Binary"
    :base64-binary
    "base64Float"
    :base64-float
    ("hexBinary" "hexFloat" "partialDate" "partialTime" "incompleteDate"
      "incompleteTime" "URI")
    (->kebab-case-keyword x)
    ::s/invalid))

(s/def :odm.xml/data-type
  (s/conformer conform-data-type name))

(s/fdef parse-item-def
  :args (s/cat :item-def (s/and ::element (tag= :ItemDef)))
  :ret :odm/item-def)

(defn parse-item-def
  [{:keys [attrs content] :as item-def}]
  (let [parse-children #(parse-children %1 %2 content)
        description (first (parse-children :Description parse-description))
        question (first (parse-children :Question parse-question))
        code-list-ref (first (parse-children :CodeListRef parse-code-list-ref))
        aliases (parse-children :Alias parse-alias)]
    (cond-> #::item-def
        {:oid (coerce-oid item-def (:OID attrs))
         :name (coerce :odm.data-formats/name item-def (:Name attrs))
         :data-type (coerce :odm.xml/data-type item-def (:DataType attrs))}

      (:Length attrs)
      (assoc ::item-def/length (coerce-integer item-def (:Length attrs)))

      description
      (assoc :odm/description description)

      question
      (assoc ::item-def/question question)

      code-list-ref
      (assoc :odm/code-list-ref code-list-ref)

      (not (empty? aliases))
      (assoc :odm/aliases aliases))))

(s/fdef parse-decode
  :args (s/cat :decode (s/and ::element (tag= :Decode)))
  :ret ::code-list-item/decode)

(defn parse-decode
  [{:keys [content]}]
  (parse-children :TranslatedText parse-translated-text content))

(s/fdef parse-code-list-item
  :args (s/cat :code-list-item (s/and ::element (tag= :CodeListItem)))
  :ret :odm/code-list-item)

(defn parse-code-list-item
  [{:keys [attrs content] :as code-list-item}]
  (let [parse-children #(parse-children %1 %2 content)
        aliases (parse-children :Alias parse-alias)]
    (cond-> #::code-list-item
        {:coded-value (coerce-oid code-list-item (:CodedValue attrs))
         :decode (first (parse-children :Decode parse-decode))}

      (:Rank attrs)
      (assoc ::code-list-item/rank (coerce-float code-list-item (:Rank attrs)))

      (:OrderNumber attrs)
      (assoc :odm/order-number (coerce-integer code-list-item (:OrderNumber attrs)))

      (not (empty? aliases))
      (assoc :odm/aliases aliases))))

(defn- conform-code-list-data-type [x]
  (if (#{"integer" "float" "text" "string"} x)
    (keyword x)
    ::s/invalid))

(s/def :odm.xml.code-list/data-type
  (s/conformer conform-code-list-data-type name))

(s/fdef parse-code-list
  :args (s/cat :code-list (s/and ::element (tag= :CodeList)))
  :ret :odm/code-list)

(defn parse-code-list
  [{:keys [attrs content] :as code-list}]
  (let [parse-children #(parse-children %1 %2 content)
        description (first (parse-children :Description parse-description))
        code-list-items (parse-children :CodeListItem parse-code-list-item)
        aliases (parse-children :Alias parse-alias)]
    (cond-> #::code-list
        {:oid (coerce-oid code-list (:OID attrs))
         :name (coerce :odm.data-formats/name code-list (:Name attrs))
         :data-type (coerce :odm.xml.code-list/data-type code-list (:DataType attrs))}

      description
      (assoc :odm/description description)

      (not (empty? code-list-items))
      (assoc ::code-list/code-list-items code-list-items)

      (not (empty? aliases))
      (assoc :odm/aliases aliases))))

(s/fdef parse-metadata-version
  :args (s/cat :metadata-version (s/and ::element (tag= :MetaDataVersion)))
  :ret :odm/metadata-version)

(defn parse-metadata-version
  [{:keys [attrs content] :as metadata-version}]
  (let [parse-children #(parse-children %1 %2 content)
        form-defs (parse-children :FormDef parse-form-def)
        item-group-defs (parse-children :ItemGroupDef parse-item-group-def)
        item-defs (parse-children :ItemDef parse-item-def)
        code-lists (parse-children :CodeList parse-code-list)]
    (cond-> #::metadata-version
        {:oid (coerce-oid metadata-version (:OID attrs))
         :name (coerce :odm.data-formats/name metadata-version (:Name attrs))}

      (:Description attrs)
      (assoc ::metadata-version/description (:Description attrs))

      (not (empty? form-defs))
      (assoc ::metadata-version/form-defs form-defs)

      (not (empty? item-group-defs))
      (assoc ::metadata-version/item-group-defs item-group-defs)

      (not (empty? item-defs))
      (assoc ::metadata-version/item-defs item-defs)

      (not (empty? code-lists))
      (assoc ::metadata-version/code-lists code-lists))))

(s/fdef parse-study
  :args (s/cat :study (s/and ::element (tag= :Study)))
  :ret :odm/study)

(defn parse-study
  [{:keys [attrs content] :as study}]
  (check-sub-tag :GlobalVariables study)
  (let [parse-children #(parse-children %1 %2 content)
        global-variables (first (parse-children :GlobalVariables parse-global-variables))
        metadata-versions (parse-children :MetaDataVersion parse-metadata-version)]
    (cond-> (merge {::study/oid (:OID attrs)} global-variables)

      (not (empty? metadata-versions))
      (assoc :odm.study/metadata-versions metadata-versions))))

(s/def :odm.xml/tx-type
  (conformer conform-tx-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen :odm/tx-type))))

(defn tx-type [e]
  (some->> (-> e :attrs :TransactionType)
           (coerce :odm.xml/tx-type e)))

(defn- data [e]
  (if-let [tx-type (tx-type e)]
    {:odm/tx-type tx-type}
    {}))

(defn- assoc-when [m k v]
  (if v (assoc m k v) m))

(defn- data-item
  [{:keys [attrs] :as item-data} data-type]
  (-> (data item-data)
      (assoc ::item-data/item-oid (:ItemOID attrs)
             ::item-data/data-type data-type)))

(defmulti parse-item-data*
  {:arglists '([item-data])}
  :tag)

(defmethod parse-item-data* :ItemDataString
  [item-data]
  (-> (data-item item-data :string)
      (assoc ::item-data/string-value (string-value item-data))))

(defmethod parse-item-data* :ItemDataInteger
  [{:keys [attrs] :as item-data}]
  (-> (data-item item-data :integer)
      (assoc ::item-data/integer-value (integer-value item-data))
      (assoc-when ::item-data/measurement-unit-oid (:MeasurementUnitOID attrs))))

(defmethod parse-item-data* :ItemDataFloat
  [{:keys [attrs] :as item-data}]
  (-> (data-item item-data :float)
      (assoc ::item-data/float-value (float-value item-data))
      (assoc-when ::item-data/measurement-unit-oid (:MeasurementUnitOID attrs))))

(defmethod parse-item-data* :ItemDataDatetime
  [item-data]
  (-> (data-item item-data :date-time)
      (assoc ::item-data/date-time-value (date-time-value item-data))))

(defmethod parse-item-data* :ItemDataBoolean
  [item-data]
  (-> (data-item item-data :boolean)
      (assoc ::item-data/boolean-value (boolean-value item-data))))

(s/fdef parse-item-data
  :args (s/cat :item-data ::element)
  :ret :odm/item-data)

(defn parse-item-data [item-data]
  (parse-item-data* item-data))

(s/fdef parse-item-group-data
  :args (s/cat :item-group-data (s/and ::element (tag= :ItemGroupData)))
  :ret :odm/item-group-data)

(defn parse-item-group-data
  [{:keys [attrs content] :as item-group-data}]
  (check-tag :ItemGroupData item-group-data)
  (-> (data item-group-data)
      (assoc ::item-group-data/item-group-oid (:ItemGroupOID attrs))
      (assoc-when ::item-group-data/item-group-repeat-key (:ItemGroupRepeatKey attrs))
      (assoc-when ::item-group-data/item-data (seq (map parse-item-data content)))))

(s/fdef parse-form-data
  :args (s/cat :form-data (s/and ::element (tag= :FormData)))
  :ret :odm/form-data)

(defn parse-form-data
  [{:keys [attrs content] :as form-data}]
  (check-tag :FormData form-data)
  (-> (data form-data)
      (assoc ::form-data/form-oid (:FormOID attrs))
      (assoc-when ::form-data/form-repeat-key (:FormRepeatKey attrs))
      (assoc-when ::form-data/item-group-data (seq (map parse-item-group-data content)))))

(s/fdef parse-study-event-data
  :args (s/cat :study-event-data (s/and ::element (tag= :StudyEventData)))
  :ret :odm/study-event-data)

(defn parse-study-event-data
  [{:keys [attrs content] :as study-event-data}]
  (check-tag :StudyEventData study-event-data)
  (-> (data study-event-data)
      (assoc ::study-event-data/study-event-oid (:StudyEventOID attrs))
      (assoc-when ::study-event-data/study-event-repeat-key (:StudyEventRepeatKey attrs))
      (assoc-when ::study-event-data/form-data (seq (map parse-form-data content)))))

(s/fdef parse-subject-data
  :args (s/cat :subject-data (s/and ::element (tag= :SubjectData)))
  :ret :odm/subject-data)

(defn parse-subject-data
  [{:keys [attrs content] :as subject-data}]
  (check-tag :SubjectData subject-data)
  (-> (data subject-data)
      (assoc ::subject-data/subject-key (:SubjectKey attrs))
      (assoc-when ::subject-data/study-event-data (seq (map parse-study-event-data content)))))

(s/fdef parse-clinical-data
  :args (s/cat :clinical-data (s/and ::element (tag= :ClinicalData)))
  :ret :odm/clinical-data)

(defn parse-clinical-data
  [{:keys [attrs content] :as clinical-data}]
  (check-tag :ClinicalData clinical-data)
  (-> {::clinical-data/study-oid (:StudyOID attrs)
       ::clinical-data/metadata-version-oid (:MetaDataVersionOID attrs)}
      (assoc-when ::clinical-data/subject-data (seq (map parse-subject-data content)))))

(defn- conform-file-type [x]
  (->> (conform-lc-kw x)
       (s/conform :odm.file/type)))

(s/def :odm.xml/file-type
  (conformer conform-file-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen :odm.file/type))))

(s/fdef parse-file
  :args (s/cat :file (s/and ::element (tag= :ODM)))
  :ret :odm/file)

(defn parse-file
  "Parses a ODM file from its root element.

  Throws an exception with :type ::validation-error and other keys like
  :element :value and :error in ex-data."
  [{:keys [attrs content] :as file}]
  (check-tag :ODM file)
  (let [parse-children #(parse-children %1 %2 content)
        studies (parse-children :Study parse-study)
        clinical-data (parse-children :ClinicalData parse-clinical-data)]
    (cond-> #:odm.file
        {:type (coerce :odm.xml/file-type file (:FileType attrs))
         :oid (:FileOID attrs)
         :creation-date-time (coerce :odm.xml/date-time file (:CreationDateTime attrs))}

      (not (empty? studies))
      (assoc ::file/studies studies)

      (not (empty? clinical-data))
      (assoc ::file/clinical-data clinical-data))))

(comment
  (require '[clojure.data.xml :as xml])
  (require '[clojure.java.io :as io])
  (def file
    (-> (io/input-stream "../../z/metadata/life-20170608-4.xml")
        (xml/parse)
        (parse-file)))

  (->> (s/explain-data :odm/file file)
       :clojure.spec/problems
       #_(map :pred)
       #_(remove #(when (sequential? (:pred %)) (= 'distinct-order-numbers? (first (:pred %))))))

  (->> (get-in file [:odm.file/studies
                     0
                     :odm.study/metadata-versions
                     0
                     :odm.metadata-version/item-group-defs
                     53
                     ;:odm.item-group-def/item-refs
                     ])
       (sort-by :odm/order-number))

  (.getCause *e)
  )

;; ---- Unparsing -------------------------------------------------------------

(defn- assoc-tx-type [m tx-type]
  (assoc-when m :TransactionType (some-> tx-type name str/capitalize)))

(defn- item-data-attrs
  [{:keys [odm.item-data/item-oid odm/tx-type]}]
  (-> {:ItemOID item-oid}
      (assoc-tx-type tx-type)))

(s/fdef unparse-item-data*
  :args (s/cat :item-data :odm/item-data))

(defmulti unparse-item-data*
  {:arglists '([item-data])}
  (fn [{:keys [odm.item-data/data-type]}] data-type))

(defmethod unparse-item-data* :string
  [{value ::item-data/string-value :as item-data}]
  [:ItemDataString (item-data-attrs item-data) value])

(defn assoc-measurement-unit-oid
  [attrs {:keys [odm.item-data/measurement-unit-oid]}]
  (assoc-when attrs :MeasurementUnitOID measurement-unit-oid))

(defmethod unparse-item-data* :integer
  [{value ::item-data/integer-value :as item-data}]
  [:ItemDataInteger
   (-> (item-data-attrs item-data)
       (assoc-measurement-unit-oid item-data))
   value])

(defmethod unparse-item-data* :float
  [{value ::item-data/float-value :as item-data}]
  [:ItemDataFloat
   (-> (item-data-attrs item-data)
       (assoc-measurement-unit-oid item-data))
   (if (double? value)
     (if (Double/isFinite value)
       (str (bigdec value))
       (throw (Exception. (str "Unable to unparse non-finite double " value))))
     (str value))])

(defmethod unparse-item-data* :date-time
  [{value ::item-data/date-time-value :as item-data}]
  [:ItemDataDatetime (item-data-attrs item-data)
   (s/unform :odm.xml/date-time value)])

(defmethod unparse-item-data* :boolean
  [{value ::item-data/boolean-value :as item-data}]
  [:ItemDataBoolean (item-data-attrs item-data) value])

(s/fdef unparse-item-data
  :args (s/cat :item-data :odm/item-data))

(defn unparse-item-data [item-data]
  (unparse-item-data* item-data))

(defn- item-group-data-attrs
  [{:keys [odm.item-group-data/item-group-oid
           odm.item-group-data/item-group-repeat-key odm/tx-type]}]
  (-> {:ItemGroupOID item-group-oid}
      (assoc-when :ItemGroupRepeatKey item-group-repeat-key)
      (assoc-tx-type tx-type)))

(s/fdef unparse-item-group-data
  :args (s/cat :item-group-data :odm/item-group-data))

(defn unparse-item-group-data
  [{:keys [odm.item-group-data/item-data] :as item-group-data}]
  (into
    [:ItemGroupData
     (item-group-data-attrs item-group-data)]
    (map unparse-item-data)
    item-data))

(defn- form-data-attrs
  [{:keys [odm.form-data/form-oid odm.form-data/form-repeat-key odm/tx-type]}]
  (-> {:FormOID form-oid}
      (assoc-when :FormRepeatKey form-repeat-key)
      (assoc-tx-type tx-type)))

(s/fdef unparse-form-data
  :args (s/cat :form-data :odm/form-data))

(defn unparse-form-data
  [{:keys [odm.form-data/item-group-data] :as form-data}]
  (into
    [:FormData
     (form-data-attrs form-data)]
    (map unparse-item-group-data)
    item-group-data))

(defn- study-event-data-attrs
  [{:keys [odm.study-event-data/study-event-oid
           odm.study-event-data/study-event-repeat-key odm/tx-type]}]
  (-> {:StudyEventOID study-event-oid}
      (assoc-when :StudyEventRepeatKey study-event-repeat-key)
      (assoc-tx-type tx-type)))

(s/fdef unparse-study-event-data
  :args (s/cat :study-event-data :odm/study-event-data))

(defn unparse-study-event-data
  [{:keys [odm.study-event-data/form-data] :as study-event-data}]
  (into
    [:StudyEventData
     (study-event-data-attrs study-event-data)]
    (map unparse-form-data)
    form-data))

(defn- subject-data-attrs
  [{:keys [odm.subject-data/subject-key odm/tx-type]}]
  (-> {:SubjectKey subject-key}
      (assoc-tx-type tx-type)))

(s/fdef unparse-subject-data
  :args (s/cat :subject :odm/subject-data))

(defn unparse-subject-data
  [{:keys [odm.subject-data/study-event-data] :as subject-data}]
  (into
    [:SubjectData
     (subject-data-attrs subject-data)]
    (map unparse-study-event-data)
    study-event-data))

(s/fdef unparse-clinical-data
  :args (s/cat :clinical-data :odm/clinical-data))

(defn unparse-clinical-data
  [{:keys [odm.clinical-data/study-oid odm.clinical-data/metadata-version-oid
           odm.clinical-data/subject-data]}]
  (into
    [:ClinicalData
     {:StudyOID study-oid :MetaDataVersionOID metadata-version-oid}]
    (map unparse-subject-data)
    subject-data))

(s/def :xml/sexp
  (s/spec (s/cat :tag keyword?
                 :attr (s/? (s/map-of keyword? any?))
                 :content (s/* (s/alt :sexpr :xml/sexp
                                      :text any?)))))

(s/fdef unparse-file
  :args (s/cat :file :odm/file)
  :ret :xml/sexp)

(defn unparse-file
  "Takes a parsed ODM file and unparses it into an sexp."
  [{:keys [odm.file/clinical-data] :as file}]
  (into
    [:ODM
     {:FileType (s/unform :odm.xml/file-type (::file/type file))
      :FileOID (::file/oid file)
      :CreationDateTime (s/unform :odm.xml/date-time (::file/creation-date-time file))}]
    (map unparse-clinical-data)
    clinical-data))
