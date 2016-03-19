(ns lens-odm-parser.core
  "The two main functions are parse-odm-file and unparse-odm-file."
  (:use plumbing.core)
  (:require [clj-time.core :refer [now]]
            [clj-time.format :as tf]
            [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as xml :refer [xml-> xml1-> attr text]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [schema.coerce :as c]
            [schema.core :as s :refer [Str Keyword]]
            [schema.utils :as su])
  (:import [org.joda.time DateTime]))

;; ---- Schemas ---------------------------------------------------------------

(def Element
  "An XML element."
  (s/pred zip/branch? 'element?))

(def NonBlankStr
  (s/constrained Str (complement str/blank?) 'non-blank?))

(def OID
  NonBlankStr)

(def SubjectKey
  NonBlankStr)

(def TransactionType
  (s/enum "Insert" "Update" "Remove" "Upsert" "Context"))

(def TxType
  (s/enum :insert :update :remove :upsert :context))

(def DataType
  (s/enum :string :integer :float :date-time))

(s/defn item [data-type :- DataType value-schema]
  {(s/optional-key :tx-type) TxType
   :data-type (s/eq data-type)
   :value value-schema})

(def StringItem
  (item :string Str))

(def IntegerItem
  (item :integer Long))

(def FloatItem
  (item :float (s/constrained Double #(Double/isFinite %) 'finite?)))

(def DateTimeItem
  (item :date-time DateTime))

(def Item
  (s/conditional
    #(= :string (:data-type %)) StringItem
    #(= :integer (:data-type %)) IntegerItem
    #(= :float (:data-type %)) FloatItem
    #(= :date-time (:data-type %)) DateTimeItem
    'item?))

(def ItemData
  {(s/named OID "item-oid") Item})

(def ItemGroup
  {(s/optional-key :tx-type) TxType
   :items ItemData})

(def ItemGroupData
  {(s/named OID "item-group-oid") ItemGroup})

(def Form
  {(s/optional-key :tx-type) TxType
   :item-groups ItemGroupData})

(def FormData
  {(s/named OID "form-oid") Form})

(def StudyEvent
  {(s/optional-key :tx-type) TxType
   :forms FormData})

(def StudyEventData
  {(s/named OID "study-event-oid") StudyEvent})

(def Subject
  {(s/optional-key :tx-type) TxType
   :study-events StudyEventData})

(def SubjectData
  {SubjectKey Subject})

(def ClinicalDatum
  {:subjects SubjectData})

(def ClinicalData
  {(s/named OID "study-oid") ClinicalDatum})

(def FileType
  (s/enum :snapshot :transactional))

(def ODMFile
  {:file-type FileType
   :file-oid OID
   :creation-date-time DateTime
   :clinical-data ClinicalData})

(def SnapshotODMFile
  (s/constrained ODMFile #(#{:snapshot} (:file-type %)) 'snapshot?))

(def TransactionalODMFile
  (s/constrained ODMFile #(#{:transactional} (:file-type %)) 'transactional?))

;; ---- Parsing ---------------------------------------------------------------

(defn- validation-ex [loc value error]
  (ex-info (format "Invalid value %s: %s" value (pr-str error))
           {:type ::validation-error :loc loc :value value :error error}))

(defn- validate [checker loc v]
  (if-let [error (checker v)]
    (throw (validation-ex loc v error))
    v))

(defn- coerce [coercer loc s]
  (let [v (coercer s)]
    (if-let [error (su/error-val v)]
      (throw (validation-ex loc s error))
      v)))

(def oid-checker (s/checker OID))

(s/defn oid [e :- Element attr :- Keyword]
  (->> (xml1-> e (xml/attr attr))
       (validate oid-checker e)))

(def subject-key-checker (s/checker SubjectKey))

(s/defn subject-key [e :- Element]
  (->> (xml1-> e (attr :SubjectKey))
       (validate subject-key-checker e)))

(defn text-with-whitespace [e]
  (apply str (xml-> e dz/descendants zip/node string?)))

(defn string-value [e]
  (xml1-> e text-with-whitespace))

(def integer-coercer
  (c/coercer Long {Long (c/safe #(Long/parseLong %))}))

(defn integer-value [e]
  (->> (xml1-> e text)
       (coerce integer-coercer e)))

(def float-coercer
  (c/coercer Double {Double (c/safe #(Double/parseDouble %))}))

(defn float-value [e]
  (->> (xml1-> e text)
       (coerce float-coercer e)))

(defn parse-date-time [s]
  (try
    (tf/parse (tf/formatters :date-time) s)
    (catch Exception _
      (tf/parse (tf/formatters :date-time-no-ms) s))))

(def date-time-coercer
  (c/coercer DateTime {DateTime (c/safe parse-date-time)}))

(defn date-time-value [e]
  (->> (xml1-> e text)
       (coerce date-time-coercer e)))

(def transaction-type-checker (s/checker TransactionType))

(s/defn tx-type :- (s/maybe TxType)
  [e :- Element]
  (some->> (xml1-> e (attr :TransactionType))
           (validate transaction-type-checker e)
           (str/lower-case)
           (keyword)))

(s/defn data [e :- Element]
  (if-let [tx-type (tx-type e)]
    {:tx-type tx-type}
    {}))

(s/defn parse-items* :- ItemData
  [item-group-data :- Element tag data-type :- DataType value-fn]
  (for-map [item-data (xml-> item-group-data tag)]
    (oid item-data :ItemOID)
    (-> (data item-data)
        (assoc :data-type data-type :value (value-fn item-data)))))

(s/defn parse-items :- ItemData
  [item-group-data :- Element]
  (merge
    (parse-items* item-group-data :ItemDataString :string string-value)
    (parse-items* item-group-data :ItemDataInteger :integer integer-value)
    (parse-items* item-group-data :ItemDataFloat :float float-value)
    (parse-items* item-group-data :ItemDataDatetime :date-time date-time-value)))

(s/defn parse-item-group :- ItemGroup [item-group-data :- Element]
  (-> (data item-group-data)
      (assoc :items (parse-items item-group-data))))

(s/defn parse-item-groups :- ItemGroupData [form-data :- Element]
  (for-map [item-group-data (xml-> form-data :ItemGroupData)]
    (oid item-group-data :ItemGroupOID)
    (parse-item-group item-group-data)))

(s/defn parse-form :- Form [form-data :- Element]
  (-> (data form-data)
      (assoc :item-groups (parse-item-groups form-data))))

(s/defn parse-forms :- FormData [study-event-data :- Element]
  (for-map [form-data (xml-> study-event-data :FormData)]
    (oid form-data :FormOID)
    (parse-form form-data)))

(s/defn parse-study-event :- StudyEvent [study-event-data :- Element]
  (-> (data study-event-data)
      (assoc :forms (parse-forms study-event-data))))

(s/defn parse-study-events :- StudyEventData [subject-data :- Element]
  (for-map [study-event-data (xml-> subject-data :StudyEventData)]
    (oid study-event-data :StudyEventOID)
    (parse-study-event study-event-data)))

(s/defn parse-subject :- Subject [subject-data :- Element]
  (-> (data subject-data)
      (assoc :study-events (parse-study-events subject-data))))

(s/defn parse-subjects :- SubjectData [clinical-data :- Element]
  (for-map [subject-data (xml-> clinical-data :SubjectData)]
    (subject-key subject-data)
    (parse-subject subject-data)))

(s/defn parse-clinical-datum :- ClinicalDatum [clinical-data :- Element]
  {:subjects (parse-subjects clinical-data)})

(s/defn parse-clinical-data :- ClinicalData [odm-file :- Element]
  (for-map [clinical-data (xml-> odm-file :ClinicalData)]
    (oid clinical-data :StudyOID)
    (parse-clinical-datum clinical-data)))

(def file-type-coercer
  (c/coercer FileType {FileType (c/safe #(-> % str/lower-case keyword))}))

(defn file-type [loc]
  (->> (xml1-> loc (attr :FileType))
       (coerce file-type-coercer loc)))

(defn creation-date-time [loc]
  (->> (xml1-> loc (attr :CreationDateTime))
       (coerce date-time-coercer loc)))

(s/defn parse-odm-file :- ODMFile
  [odm-file :- Element]
  {:file-type (file-type odm-file)
   :file-oid (oid odm-file :FileOID)
   :creation-date-time (creation-date-time odm-file)
   :clinical-data (parse-clinical-data odm-file)})

;; ---- Unparsing -------------------------------------------------------------

(defn unparse-date-time [date-time]
  (tf/unparse (tf/formatters :date-time) date-time))

(defn assoc-tx-type [m tx-type]
  (assoc-when m :TransactionType (some-> tx-type name str/capitalize)))

(defn item-attrs [oid {:keys [tx-type]}]
  (-> {:ItemOID oid}
      (assoc-tx-type tx-type)))

(defmulti unparse-item* (fn [_ {:keys [data-type]}] data-type))

(defmethod unparse-item* :string
  [oid {:keys [value] :as item}]
  [:ItemDataString (item-attrs oid item) value])

(defmethod unparse-item* :integer
  [oid {:keys [value] :as item}]
  [:ItemDataInteger (item-attrs oid item) value])

(defmethod unparse-item* :float
  [oid {:keys [value] :as item}]
  [:ItemDataFloat (item-attrs oid item) value])

(defmethod unparse-item* :date-time
  [oid {:keys [value] :as item}]
  [:ItemDataDatetime (item-attrs oid item) (unparse-date-time value)])

(s/defn unparse-item [oid :- OID item :- Item]
  (unparse-item* oid item))

(defn item-group-attrs [oid {:keys [tx-type]}]
  (-> {:ItemGroupOID oid}
      (assoc-tx-type tx-type)))

(s/defn unparse-item-group
  [oid :- OID {:keys [items] :as item-group} :- ItemGroup]
  [:ItemGroupData
   (item-group-attrs oid item-group)
   (for [[oid item] items]
     (unparse-item oid item))])

(defn form-attrs [oid {:keys [tx-type]}]
  (-> {:FormOID oid}
      (assoc-tx-type tx-type)))

(s/defn unparse-form
  [oid :- OID {:keys [item-groups] :as form} :- Form]
  [:FormData
   (form-attrs oid form)
   (for [[oid item-group] item-groups]
     (unparse-item-group oid item-group))])

(defn study-event-attrs [oid {:keys [tx-type]}]
  (-> {:StudyEventOID oid}
      (assoc-tx-type tx-type)))

(s/defn unparse-study-event
  [oid :- OID {:keys [forms] :as study-event} :- StudyEvent]
  [:StudyEventData
   (study-event-attrs oid study-event)
   (for [[oid form] forms]
     (unparse-form oid form))])

(defn subject-attrs [key {:keys [tx-type]}]
  (-> {:SubjectKey key}
      (assoc-tx-type tx-type)))

(s/defn unparse-subject
  [key :- SubjectKey {:keys [study-events] :as subject} :- Subject]
  [:SubjectData
   (subject-attrs key subject)
   (for [[oid study-event] study-events]
     (unparse-study-event oid study-event))])

(s/defn unparse-clinical-datum
  [study-oid :- OID {:keys [subjects]} :- ClinicalDatum]
  [:ClinicalData {:StudyOID study-oid}
   (for [[key subject] subjects]
     (unparse-subject key subject))])

(s/defn unparse-odm-file [odm-file :- ODMFile]
  [:ODM
   {:FileType (str/capitalize (name (:file-type odm-file)))
    :FileOID (:file-oid odm-file)
    :CreationDateTime (unparse-date-time (:creation-date-time odm-file))}
   (for [[study-oid clinical-datum] (:clinical-data odm-file)]
     (unparse-clinical-datum study-oid clinical-datum))])
