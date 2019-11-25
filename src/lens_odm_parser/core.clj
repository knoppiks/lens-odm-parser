(ns lens-odm-parser.core
  "The two main functions are parse-elem and unparse-file.

  Example: `(-> (io/input-stream \"odm-file.xml\")
            (xml/parse)
            (parse-elem))`"
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

(defn- validation-ex [value error]
  (ex-info (format "Invalid value %s" value)
           {:type ::validation-error :value value :error error}))

(defn- coerce [spec s]
  (let [v (s/conform spec s)]
    (if (= ::s/invalid v)
      (throw (validation-ex s (s/explain-data spec s)))
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

(defn coerce-integer [s]
  (coerce :odm.xml/integer s))

(defn integer-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce-integer)))

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

(defn coerce-float [s]
  (coerce :odm.xml/float s))

(defn float-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce-float)))

(defn- conform-date-time [x]
  (if (string? x)
    (try
      (tc/to-date (tf/parse (tf/formatters :date-time) x))
      (catch Exception _
        (try
          (tc/to-date (tf/parse (tf/formatters :date-time-no-ms) x))
          (catch Exception _
            (try
              (tc/to-date (tf/parse (tf/formatters :local-date-opt-time) x))
              (catch Exception _
                ::s/invalid))))))
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

(defn coerce-date-time [value]
  (coerce :odm.xml/date-time value))

(defn date-time-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce-date-time)))

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

(defn coerce-oid [s]
  (coerce :odm.data-formats/oid s))

(defmulti parse-elem*
  "Parses a single XML Element. Dispatches by :tag. Should return a map with a
  single key-value pair which will be merged together with the maps from other
  parses. Multiple values with the same key are merged with `into`."
  {:arglists '([element])}
  :tag)

(s/fdef parse-elem
  :args (s/cat :element ::element))

(defn parse-elem [element]
  (parse-elem* element))

(defmethod parse-elem* :default [_])

(defn parse-children
  "Parses every child using `parse-elem` and merges the results. Keys occuring
   more than once will be merged using `into`."
  [content]
  (apply (partial merge-with into) (map parse-elem content)))

(defmulti parse-attr*
  "Parses XML Attribute. Dispatches on a vector of element tag and attribute
   name with a fallback on attribute name only. Should return a map with a
   single key-value pair which will be merged together with the maps from other
   parses."
  {:arglists '([tag name value])}
  (fn [tag name _] (if tag [tag name] name)))

(defmethod parse-attr* :default [tag name value]
  (when tag (parse-attr* nil name value)))

(s/fdef parse-attr
  :args (s/cat :tag keyword? :name keyword? :value string?))

(defn parse-attr [tag key value]
  (parse-attr* tag key value))

(defmethod parse-attr* [:CodeListRef :CodeListOID]
  [_ _ value]
  {:odm.code-list-ref/code-list-oid (coerce-oid value)})

(defn parse-attrs [tag attrs]
  (into {} (map (fn [[name value]] (parse-attr tag name value)) attrs)))

(defmethod parse-elem* :StudyName
  [element]
  {::study/name (string-value element)})

(defmethod parse-elem* :StudyDescription
  [element]
  {::study/description (string-value element)})

(defmethod parse-elem* :ProtocolName
  [element]
  {::study/protocol-name (string-value element)})

(defmethod parse-elem* :GlobalVariables
  [{:keys [content]}]
  (parse-children content))

(defn- conform-yes-no [x]
  (case x "Yes" true "No" false ::s/invalid))

(s/def :odm.xml/yes-no
  (s/conformer conform-yes-no #(if % "Yes" "No")))

;; TODO: language tag
(defmethod parse-elem* :TranslatedText
  [{:keys [attrs] :as translated-text}]
  [{:text (string-value translated-text)}])

(defmethod parse-elem* :Description
  [{:keys [content]}]
  {:odm/description (parse-children content)})

(defmethod parse-elem* :Question
  [{:keys [content]}]
  {::item-def/question (parse-children content)})

(defmethod parse-elem* :CodeListRef
  [{:keys [attrs]}]
  {:odm/code-list-ref (parse-attrs :CodeListRef attrs)})

(defmethod parse-attr* [:Alias :Context]
  [_ _ value]
  {:odm.alias/context (coerce :odm.data-formats/text value)})

(defmethod parse-attr* [:Alias :Name]
  [_ _ value]
  {:odm.alias/name (coerce :odm.data-formats/text value)})

(defmethod parse-elem* :Alias
  [{:keys [attrs]}]
  {:odm/aliases [(parse-attrs :Alias attrs)]})

(defmethod parse-attr* [:ItemGroupRef :ItemGroupOID]
  [_ _ value]
  {::item-group-ref/item-group-oid (coerce-oid value)})

(defmethod parse-attr* :Mandatory
  [_ _ value]
  {:odm/mandatory (coerce :odm.xml/yes-no value)})

(defmethod parse-attr* :OrderNumber
  [_ _ value]
  {:odm/order-number (coerce-integer value)})

(defmethod parse-attr* :CollectionExceptionConditionOID
  [_ _ value]
  (coerce-oid value))

(defmethod parse-elem* :ItemGroupRef
  [{:keys [attrs]}]
  {::form-def/item-group-refs
   [(parse-attrs :ItemGroupRef attrs)]})

(defmethod parse-attr* [:ItemRef :ItemOID]
  [_ _ value]
  {::item-ref/item-oid (coerce-oid value)})

(defmethod parse-elem* :ItemRef
  [{:keys [attrs]}]
  {::item-group-def/item-refs
   [(parse-attrs :ItemRef attrs)]})

(defmethod parse-attr* [:FormDef :OID]
  [_ _ value]
  {::form-def/oid (coerce-oid value)})

(defmethod parse-attr* [:FormDef :Name]
  [_ _ value]
  {::form-def/name (coerce :odm.data-formats/name value)})

(defmethod parse-attr* :Repeating
  [_ _ value]
  {:odm.def/repeating (coerce :odm.xml/yes-no value)})

(defmethod parse-elem* :FormDef
  [{:keys [attrs content]}]
  {::metadata-version/form-defs
   [(merge (parse-children content)
           (parse-attrs :FormDef attrs))]})

(defmethod parse-attr* [:ItemGroupDef :OID]
  [_ _ value]
  {::item-group-def/oid (coerce-oid value)})

(defmethod parse-attr* [:ItemGroupDef :Name]
  [_ _ value]
  {::item-group-def/name (coerce :odm.data-formats/name value)})

(defmethod parse-elem* :ItemGroupDef
  [{:keys [attrs content]}]
  {::metadata-version/item-group-defs
   [(merge (parse-children content)
           (parse-attrs :ItemGroupDef attrs))]})

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

(defmethod parse-attr* [:ItemDef :OID]
  [_ _ value]
  {::item-def/oid (coerce-oid value)})

(defmethod parse-attr* [:ItemDef :Name]
  [_ _ value]
  {::item-def/name (coerce :odm.data-formats/name value)})

(defmethod parse-attr* [:ItemDef :DataType]
  [_ _ value]
  {::item-def/data-type (coerce :odm.xml/data-type value)})

(defmethod parse-attr* [:ItemDef :Length]
  [_ _ value]
  {::item-def/length (coerce-integer value)})

(defmethod parse-elem* :ItemDef
  [{:keys [attrs content]}]
  {::metadata-version/item-defs
   [(merge (parse-children content)
           (parse-attrs :ItemDef attrs))]})

(defmethod parse-elem* :Decode
  [{:keys [content]}]
  {::code-list-item/decode (parse-children content)})

(defmethod parse-attr* [:CodeListItem :CodedValue]
  [_ _ value]
  {::code-list-item/coded-value (coerce-oid value)})

(defmethod parse-attr* [:CodeListItem :Rank]
  [_ _ value]
  {::code-list-item/rank (coerce-float value)})

(defmethod parse-elem* :CodeListItem
  [{:keys [attrs content]}]
  {::code-list/code-list-items
   [(merge (parse-children content)
           (parse-attrs :CodeListItem attrs))]})

(defn- conform-code-list-data-type [x]
  (if (#{"integer" "float" "text" "string"} x)
    (keyword x)
    ::s/invalid))

(s/def :odm.xml.code-list/data-type
  (s/conformer conform-code-list-data-type name))

(defmethod parse-attr* [:CodeList :OID]
  [_ _ value]
  {::code-list/oid (coerce-oid value)})

(defmethod parse-attr* [:CodeList :Name]
  [_ _ value]
  {::code-list/name (coerce :odm.data-formats/name value)})

(defmethod parse-attr* [:CodeList :DataType]
  [_ _ value]
  {::code-list/data-type (coerce :odm.xml.code-list/data-type value)})

(defmethod parse-elem* :CodeList
  [{:keys [attrs content]}]
  {::metadata-version/code-lists
   [(merge (parse-children content)
           (parse-attrs :CodeList attrs))]})

(defmethod parse-attr* [:MetaDataVersion :OID]
  [_ _ value]
  {::metadata-version/oid (coerce-oid value)})

(defmethod parse-attr* [:MetaDataVersion :Name]
  [_ _ value]
  {::metadata-version/name (coerce :odm.data-formats/name value)})

(defmethod parse-attr* [:MetaDataVersion :Description]
  [_ _ value]
  {::metadata-version/description value})

(defmethod parse-elem* :MetaDataVersion
  [{:keys [attrs content]}]
  {:odm.study/metadata-versions
   [(merge (parse-children content)
           (parse-attrs :MetaDataVersion attrs))]})

(defmethod parse-attr* [:Study :OID]
  [_ _ value]
  {::study/oid (coerce-oid value)})

(defmethod parse-elem* :Study
  [{:keys [attrs content]}]
  {::file/studies
   [(merge (parse-children content)
           (parse-attrs :Study attrs))]})

(defn- assoc-when [m k v]
  (if v (assoc m k v) m))

(defmethod parse-attr* [:ItemData :ItemOID]
  [_ _ value]
  {::item-data/item-oid value})

(defmethod parse-attr* [:ItemData :MeasurementUnitOID]
  [_ _ value]
  {::item-data/measurement-unit-oid value})

(s/def :odm.xml/tx-type
  (conformer conform-tx-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen :odm/tx-type))))

(defmethod parse-attr* :TransactionType
  [_ _ value]
  {:odm/tx-type (coerce :odm.xml/tx-type value)})

(defmethod parse-elem* :ItemDataString
  [{:keys [attrs] :as item-data}]
  {::item-group-data/item-data
   [(assoc (parse-attrs :ItemData attrs)
      ::item-data/data-type :string
      ::item-data/string-value (string-value item-data))]})

(defmethod parse-elem* :ItemDataInteger
  [{:keys [attrs] :as item-data}]
  {::item-group-data/item-data
   [(assoc (parse-attrs :ItemData attrs)
      ::item-data/data-type :integer
      ::item-data/integer-value (integer-value item-data))]})

(defmethod parse-elem* :ItemDataFloat
  [{:keys [attrs] :as item-data}]
  {::item-group-data/item-data
   [(assoc (parse-attrs :ItemData attrs)
      ::item-data/data-type :float
      ::item-data/float-value (float-value item-data))]})

(defmethod parse-elem* :ItemDataDatetime
  [{:keys [attrs] :as item-data}]
  {::item-group-data/item-data
   [(assoc (parse-attrs :ItemData attrs)
      ::item-data/data-type :date-time
      ::item-data/date-time-value (date-time-value item-data))]})

(defmethod parse-elem* :ItemDataBoolean
  [{:keys [attrs] :as item-data}]
  {::item-group-data/item-data
   [(assoc (parse-attrs :ItemData attrs)
      ::item-data/data-type :boolean
      ::item-data/boolean-value (boolean-value item-data))]})

(defmethod parse-attr* [:ItemGroupData :ItemGroupOID]
  [_ _ value]
  {::item-group-data/item-group-oid value})

(defmethod parse-attr* [:ItemGroupData :ItemGroupRepeatKey]
  [_ _ value]
  {::item-group-data/item-group-repeat-key value})

(defmethod parse-elem* :ItemGroupData
  [{:keys [attrs content]}]
  {::form-data/item-group-data
   [(merge (parse-children content)
           (parse-attrs :ItemGroupData attrs))]})

(defmethod parse-attr* [:FormData :FormOID]
  [_ _ value]
  {::form-data/form-oid value})

(defmethod parse-attr* [:FormData :FormRepeatKey]
  [_ _ value]
  {::form-data/form-repeat-key value})

(defmethod parse-elem* :FormData
  [{:keys [attrs content]}]
  {::study-event-data/form-data
   [(merge (parse-children content)
           (parse-attrs :FormData attrs))]})

(defmethod parse-attr* [:StudyEventData :StudyEventOID]
  [_ _ value]
  {::study-event-data/study-event-oid value})

(defmethod parse-attr* [:StudyEventData :StudyEventRepeatKey]
  [_ _ value]
  {::study-event-data/study-event-repeat-key value})

(defmethod parse-elem* :StudyEventData
  [{:keys [attrs content]}]
  {::subject-data/study-event-data
   [(merge (parse-children content)
           (parse-attrs :StudyEventData attrs))]})

(defmethod parse-attr* [:SubjectData :SubjectKey]
  [_ _ value]
  {::subject-data/subject-key value})

(defmethod parse-elem* :SubjectData
  [{:keys [attrs content]}]
  {::clinical-data/subject-data
   [(merge (parse-children content)
           (parse-attrs :SubjectData attrs))]})

(defmethod parse-attr* [:ClinicalData :StudyOID]
  [_ _ value]
  {::clinical-data/study-oid value})

(defmethod parse-attr* [:ClinicalData :MetaDataVersionOID]
  [_ _ value]
  {::clinical-data/metadata-version-oid value})

(defmethod parse-elem* :ClinicalData
  [{:keys [attrs content]}]
  {::file/clinical-data
   [(merge (parse-children content)
           (parse-attrs :ClinicalData attrs))]})

(defn- conform-file-type [x]
  (->> (conform-lc-kw x)
       (s/conform :odm.file/type)))

(s/def :odm.xml/file-type
  (conformer conform-file-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen :odm.file/type))))

(defmethod parse-attr* [:ODM :FileType]
  [_ _ value]
  {:odm.file/type (coerce :odm.xml/file-type value)})

(defmethod parse-attr* [:ODM :FileOID]
  [_ _ value]
  {:odm.file/oid value})

(defmethod parse-attr* [:ODM :CreationDateTime]
  [_ _ value]
  {:odm.file/creation-date-time (coerce-date-time value)})

(defmethod parse-elem* :ODM
  [{:keys [attrs content]}]
  (merge (parse-children content)
         (parse-attrs :ODM attrs)))

(comment
  (require '[clojure.data.xml :as xml])
  (require '[clojure.java.io :as io])
  (def file
    (-> (io/input-stream "../../z/metadata/life-20170608-4.xml")
        (xml/parse)
        (parse-elem)))

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
