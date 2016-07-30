(ns lens-odm-parser.core
  "The two main functions are parse-odm-file and unparse-odm-file."
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.string :as str]
            [odm]))

(defmacro for-map*
  "Like for-map but returns nil instead of empty maps."
  [[sym coll] key-expr val-expr]
  `(reduce (fn [r# ~sym] (assoc r# ~key-expr ~val-expr)) nil ~coll))

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

(s/def ::data-type
  keyword?)

(s/def ::string-value string?)
(s/def ::integer-value :odm/integer)
(s/def ::float-value :odm/float)
(s/def ::date-time-value :odm/date-time)

(defmacro item-spec* [value-spec]
  `(s/keys :req-un [::data-type ~value-spec] :opt-un [:odm/tx-type]))

(defmulti item-spec :data-type)

(defmethod item-spec :string [_]
  (item-spec* ::string-value))

(defmethod item-spec :integer [_]
  (item-spec* ::integer-value))

(defmethod item-spec :float [_]
  (item-spec* ::float-value))

(defmethod item-spec :date-time [_]
  (item-spec* ::date-time-value))

(s/def ::item
  (s/multi-spec item-spec :data-type))

(s/def ::items
  (s/map-of :odm/item-def-oid ::item :min-count 1 :gen-max 20))

(s/def ::item-group
  (s/keys :opt-un [:odm/tx-type ::items]))

(s/def ::item-groups
  (s/map-of :odm/item-group-def-oid ::item-group :min-count 1 :gen-max 5))

(s/def ::form
  (s/keys :opt-un [:odm/tx-type ::item-groups]))

(s/def ::forms
  (s/map-of :odm/form-def-oid ::form :min-count 1 :gen-max 20))

(s/def ::study-event
  (s/keys :opt-un [:odm/tx-type ::forms]))

(s/def ::study-events
  (s/map-of :odm/study-event-oid ::study-event :min-count 1 :gen-max 5))

(s/def ::subject
  (s/keys :opt-un [:odm/tx-type ::study-events]))

(s/def ::subjects
  (s/map-of :odm/subject-key ::subject :min-count 1 :gen-max 100))

(s/def ::clinical-datum
  (s/keys :opt-un [::subjects]))

(s/def ::clinical-data
  (s/map-of :odm/study-oid ::clinical-datum :min-count 1 :gen-max 5))

(s/def ::file
  (s/keys :req-un [:odm/file-type :odm/file-oid :odm/creation-date-time]
          :opt-un [::clinical-data]))

(s/def ::snapshot-file
  (s/and ::file #(= :snapshot (:file-type %))))

(s/def ::transactional-file
  (s/and ::file #(= :transactional (:file-type %))))

;; ---- Parsing ---------------------------------------------------------------

(defn- validation-ex [e value error]
  (ex-info (format "Invalid value %s" value)
           {:type ::validation-error :element e :value value :error error}))

(defn- validate [explainer e v]
  (if-let [error (explainer v)]
    (throw (validation-ex e v error))
    v))

(defn- coerce [spec e s]
  (let [v (s/conform spec s)]
    (if (= ::s/invalid v)
      (throw (validation-ex e s (s/explain-data spec s)))
      v)))

(def oid-explainer (partial s/explain-data :odm/oid))

(defn oid [e attr]
  (->> (-> e :attrs attr)
       (validate oid-explainer e)))

(def subject-key-explainer (partial s/explain-data :odm/subject-key))

(defn subject-key [e]
  (->> (-> e :attrs :SubjectKey)
       (validate subject-key-explainer e)))

(defn string-value [e]
  (apply str (:content e)))

(defn- conform-int [x]
  (cond
    (integer? x) x
    (string? x)
    (try
      (bigint x)
      (catch Exception _
        ::s/invalid))
    :else ::s/invalid))

(def integer-conformer (s/conformer conform-int))

(defn integer-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce integer-conformer e)))

(defn- conform-float [x]
  (cond
    (decimal? x) x
    (string? x)
    (try
      (bigdec x)
      (catch Exception _
        ::s/invalid))
    :else ::s/invalid))

(def float-conformer (s/conformer conform-float))

(defn float-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce float-conformer e)))

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
  (tf/unparse (tf/formatters :date-time) (tc/to-date-time t)))

(defn xml-date-time-gen []
  (gen/fmap (fn [t] (tf/unparse (tf/formatters :date-time) (tc/from-date t)))
            (s/gen inst?)))

(s/def :odm.xml/date-time
  (conformer conform-date-time unform-date-time :gen xml-date-time-gen))

(defn date-time-value [e]
  (->> (string-value e)
       (str/trim)
       (coerce :odm.xml/date-time e)))

(defn- conform-tx-type [x]
  (->> (conform-lc-kw x)
       (s/conform (s/form :odm/tx-type))))

(s/def :odm.xml/tx-type
  (conformer conform-tx-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen :odm/tx-type))))

(defn tx-type [e]
  (some->> (-> e :attrs :TransactionType)
           (coerce :odm.xml/tx-type e)))

(defn- data [e]
  (if-let [tx-type (tx-type e)]
    {:tx-type tx-type}
    {}))

(defn- value-kw [data-type]
  (keyword (str (name data-type) "-value")))

(defn parse-item [data-type value-fn e]
  (assoc (data e) :data-type data-type (value-kw data-type) (value-fn e)))

(defn- tag= [tag]
  (filter #(= tag (:tag %))))

(defn- elements [tag e]
  (eduction (tag= tag) (:content e)))

(defn- parse-items* [item-group-data tag data-type value-fn]
  (for-map* [item-data (elements tag item-group-data)]
    (oid item-data :ItemOID)
    (parse-item data-type value-fn item-data)))

(defn parse-items [item-group-data]
  (merge
    (parse-items* item-group-data :ItemDataString :string string-value)
    (parse-items* item-group-data :ItemDataInteger :integer integer-value)
    (parse-items* item-group-data :ItemDataFloat :float float-value)
    (parse-items* item-group-data :ItemDataDatetime :date-time date-time-value)))

(defn- assoc-when [m k v]
  (if v (assoc m k v) m))

(s/fdef parse-item-group
  :args (s/cat :item-group-data ::element)
  :ret ::item-group)

(defn parse-item-group [item-group-data]
  (-> (data item-group-data)
      (assoc-when :items (parse-items item-group-data))))

(defn parse-item-groups [form-data]
  (for-map* [item-group-data (elements :ItemGroupData form-data)]
    (oid item-group-data :ItemGroupOID)
    (parse-item-group item-group-data)))

(s/fdef parse-form
  :args (s/cat :form-data ::element)
  :ret ::form)

(defn parse-form [form-data]
  (-> (data form-data)
      (assoc-when :item-groups (parse-item-groups form-data))))

(defn parse-forms [study-event-data]
  (for-map* [form-data (elements :FormData study-event-data)]
    (oid form-data :FormOID)
    (parse-form form-data)))

(s/fdef parse-study-event
  :args (s/cat :study-event-data ::element)
  :ret ::study-event)

(defn parse-study-event [study-event-data]
  (-> (data study-event-data)
      (assoc-when :forms (parse-forms study-event-data))))

(defn parse-study-events [subject-data]
  (for-map* [study-event-data (elements :StudyEventData subject-data)]
    (oid study-event-data :StudyEventOID)
    (parse-study-event study-event-data)))

(s/fdef parse-subject
  :args (s/cat :subject-data ::element)
  :ret ::subject)

(defn parse-subject [subject-data]
  (-> (data subject-data)
      (assoc-when :study-events (parse-study-events subject-data))))

(defn parse-subjects [clinical-data]
  (for-map* [subject-data (elements :SubjectData clinical-data)]
    (subject-key subject-data)
    (parse-subject subject-data)))

(s/fdef parse-clinical-datum
  :args (s/cat :clinical-data ::element)
  :ret ::clinical-datum)

(defn parse-clinical-datum [clinical-data]
  (assoc-when {} :subjects (parse-subjects clinical-data)))

(defn parse-clinical-data [odm-file]
  (for-map* [clinical-data (elements :ClinicalData odm-file)]
    (oid clinical-data :StudyOID)
    (parse-clinical-datum clinical-data)))

(defn- conform-file-type [x]
  (->> (conform-lc-kw x)
       (s/conform (s/form :odm/file-type))))

(s/def :odm.xml/file-type
  (conformer conform-file-type unform-cap-str
             :gen #(gen/fmap unform-cap-str (s/gen ::file))))

(defn file-type [e]
  (->> (-> e :attrs :FileType)
       (coerce :odm.xml/file-type e)))

(defn creation-date-time [e]
  (->> (-> e :attrs :CreationDateTime)
       (coerce :odm.xml/date-time e)))

(s/fdef parse-odm-file
  :args (s/cat :odm-file ::element)
  :ret ::file)

(defn parse-odm-file
  "Parses a ODM file from its root element.

  Throws an exception with :type ::validation-error and other keys like
  :element :value and :error in ex-data."
  [odm-file]
  (-> {:file-type (file-type odm-file)
       :file-oid (oid odm-file :FileOID)
       :creation-date-time (creation-date-time odm-file)}
      (assoc-when :clinical-data (parse-clinical-data odm-file))))

;; ---- Unparsing -------------------------------------------------------------

(defn assoc-tx-type [m tx-type]
  (assoc-when m :TransactionType (some-> tx-type name str/capitalize)))

(defn item-attrs [oid {:keys [tx-type]}]
  (-> {:ItemOID oid}
      (assoc-tx-type tx-type)))

(defmulti unparse-item* (fn [_ {:keys [data-type]}] data-type))

(defmethod unparse-item* :string
  [oid {:keys [string-value] :as item}]
  [:ItemDataString (item-attrs oid item) string-value])

(defmethod unparse-item* :integer
  [oid {:keys [integer-value] :as item}]
  [:ItemDataInteger (item-attrs oid item) integer-value])

(defmethod unparse-item* :float
  [oid {:keys [float-value] :as item}]
  [:ItemDataFloat (item-attrs oid item) float-value])

(defmethod unparse-item* :date-time
  [oid {:keys [date-time-value] :as item}]
  [:ItemDataDatetime (item-attrs oid item)
   (s/unform :odm.xml/date-time date-time-value)])

(s/fdef unparse-item
  :args (s/cat :oid :odm/oid :item ::item))

(defn unparse-item [oid item]
  (unparse-item* oid item))

(defn item-group-attrs [oid {:keys [tx-type]}]
  (-> {:ItemGroupOID oid}
      (assoc-tx-type tx-type)))

(s/fdef unparse-item-group
  :args (s/cat :oid :odm/oid :item-group ::item-group))

(defn unparse-item-group
  [oid {:keys [items] :as item-group}]
  [:ItemGroupData
   (item-group-attrs oid item-group)
   (for [[oid item] (sort-by first items)]
     (unparse-item oid item))])

(defn form-attrs [oid {:keys [tx-type]}]
  (-> {:FormOID oid}
      (assoc-tx-type tx-type)))

(s/fdef unparse-form
  :args (s/cat :oid :odm/oid :form ::form))

(defn unparse-form [oid {:keys [item-groups] :as form}]
  [:FormData
   (form-attrs oid form)
   (for [[oid item-group] (sort-by first item-groups)]
     (unparse-item-group oid item-group))])

(defn study-event-attrs [oid {:keys [tx-type]}]
  (-> {:StudyEventOID oid}
      (assoc-tx-type tx-type)))

(s/fdef unparse-study-event
  :args (s/cat :oid :odm/oid :study-event ::study-event))

(defn unparse-study-event [oid {:keys [forms] :as study-event}]
  [:StudyEventData
   (study-event-attrs oid study-event)
   (for [[oid form] (sort-by first forms)]
     (unparse-form oid form))])

(defn subject-attrs [key {:keys [tx-type]}]
  (-> {:SubjectKey key}
      (assoc-tx-type tx-type)))

(s/fdef unparse-subject
  :args (s/cat :subject-key :odm/subject-key :subject ::subject))

(defn unparse-subject [subject-key {:keys [study-events] :as subject}]
  [:SubjectData
   (subject-attrs subject-key subject)
   (for [[oid study-event] (sort-by first study-events)]
     (unparse-study-event oid study-event))])

(s/fdef unparse-clinical-datum
  :args (s/cat :study-oid :odm/oid :clinical-datum ::clinical-datum))

(defn unparse-clinical-datum [study-oid {:keys [subjects]}]
  [:ClinicalData {:StudyOID study-oid}
   (for [[key subject] (sort-by first subjects)]
     (unparse-subject key subject))])

(s/fdef unparse-odm-file
  :args (s/cat :odm-file ::file))

(defn unparse-odm-file [odm-file]
  [:ODM
   {:FileType (s/unform :odm.xml/file-type (:file-type odm-file))
    :FileOID (:file-oid odm-file)
    :CreationDateTime (s/unform :odm.xml/date-time (:creation-date-time odm-file))}
   (for [[study-oid clinical-datum] (sort-by first (:clinical-data odm-file))]
     (unparse-clinical-datum study-oid clinical-datum))])
