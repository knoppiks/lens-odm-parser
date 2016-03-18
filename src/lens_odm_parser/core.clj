(ns lens-odm-parser.core
  (:use plumbing.core)
  (:require [clj-time.core :refer [now]]
            [clj-time.format :as tf]
            [clojure.data.zip.xml :as xml :refer [xml-> xml1-> attr text]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [schema.coerce :as c]
            [schema.core :as s :refer [Str Int Num]]
            [schema.utils :as su])
  (:import [org.joda.time DateTime]))

;; ---- Schemas ---------------------------------------------------------------

(def Element
  "An XML element."
  (s/pred zip/branch?))

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

(def Value
  (s/cond-pre Str Num DateTime))

(def Item
  {(s/optional-key :tx-type) TxType
   :data-type DataType
   :value Value})

(def ItemData
  {(s/named OID "item-oid") Item})

(def ItemGroup
  {(s/optional-key :tx-type) TxType
   (s/optional-key :items) ItemData})

(def ItemGroupData
  {(s/named OID "item-group-oid") ItemGroup})

(def Form
  {(s/optional-key :tx-type) TxType
   (s/optional-key :item-groups) ItemGroupData})

(def FormData
  {(s/named OID "form-oid") Form})

(def StudyEvent
  {(s/optional-key :tx-type) TxType
   (s/optional-key :forms) FormData})

(def StudyEventData
  {(s/named OID "study-event-oid") StudyEvent})

(def Subject
  {(s/optional-key :tx-type) TxType
   (s/optional-key :study-events) StudyEventData})

(def SubjectData
  {SubjectKey Subject})

(def ClinicalDatum
  {(s/optional-key :subjects) SubjectData})

(def ClinicalData
  {(s/named OID "study-oid") ClinicalDatum})

(def FileType
  (s/enum :snapshot :transactional))

(def ODMFile
  {:file-type FileType
   :file-oid OID
   :creation-date-time DateTime
   (s/optional-key :clinical-data) ClinicalData})

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

(defn oid [loc attr]
  (->> (xml1-> loc (xml/attr attr))
       (validate oid-checker loc)))

(def subject-key-checker (s/checker SubjectKey))

(defn subject-key [loc]
  (->> (xml1-> loc (attr :SubjectKey))
       (validate subject-key-checker loc)))

(defn string-data [loc]
  (xml1-> loc text))

(def integer-coercer
  (c/coercer Long {Long (c/safe #(Long/parseLong %))}))

(defn integer-data [loc]
  (->> (xml1-> loc text)
       (coerce integer-coercer loc)))

(def float-coercer
  (c/coercer Double {Double (c/safe #(Double/parseDouble %))}))

(defn float-data [loc]
  (->> (xml1-> loc text)
       (coerce float-coercer loc)))

(defn parse-date-time [s]
  (try
    (tf/parse (tf/formatters :date-time) s)
    (catch Exception _
      (tf/parse (tf/formatters :date-time-no-ms) s))))

(def date-time-coercer
  (c/coercer DateTime {DateTime (c/safe parse-date-time)}))

(defn date-time-data [loc]
  (->> (xml1-> loc text)
       (coerce date-time-coercer loc)))

(def transaction-type-checker (s/checker TransactionType))

(s/defn tx-type :- (s/maybe TxType)
  "Tries to determine the transaction type of a loc.

  Does so recursive to parents. Returns nil if none was found. Throws a
  validation error on invalid transaction type."
  [loc :- Element]
  (if-let [t (xml1-> loc (attr :TransactionType))]
    (-> (validate transaction-type-checker loc t)
        (str/lower-case)
        (keyword))
    (when-let [parent (zip/up loc)]
      (tx-type parent))))

(s/defn parse-string-item :- ItemData
  [r :- (s/maybe ItemData) item-data :- Element]
  (->> (-> {:data-type :string
            :value (string-data item-data)}
           (assoc-when :tx-type (tx-type item-data)))
       (assoc r (oid item-data :ItemOID))))

(s/defn parse-integer-item :- ItemData
  [r :- (s/maybe ItemData) item-data :- Element]
  (->> (-> {:data-type :integer
            :value (integer-data item-data)}
           (assoc-when :tx-type (tx-type item-data)))
       (assoc r (oid item-data :ItemOID))))

(s/defn parse-float-item :- ItemData
  [r :- (s/maybe ItemData) item-data :- Element]
  (->> (-> {:data-type :float
            :value (float-data item-data)}
           (assoc-when :tx-type (tx-type item-data)))
       (assoc r (oid item-data :ItemOID))))

(s/defn parse-date-time-item :- ItemData
  [r :- (s/maybe ItemData) item-data :- Element]
  (->> (-> {:data-type :date-time
            :value (date-time-data item-data)}
           (assoc-when :tx-type (tx-type item-data)))
       (assoc r (oid item-data :ItemOID))))

(s/defn parse-items :- (s/maybe ItemData)
  [item-group-data :- Element]
  (as-> nil items
        (reduce parse-string-item items (xml-> item-group-data :ItemDataString))
        (reduce parse-integer-item items (xml-> item-group-data :ItemDataInteger))
        (reduce parse-float-item items (xml-> item-group-data :ItemDataFloat))
        (reduce parse-date-time-item items (xml-> item-group-data :ItemDataDatetime))))

(s/defn parse-item-group :- ItemGroupData
  [r :- (s/maybe ItemGroupData) item-group-data :- Element]
  (->> (-> (assoc-when nil :tx-type (tx-type item-group-data))
           (assoc-when :items (parse-items item-group-data)))
       (assoc r (oid item-group-data :ItemGroupOID))))

(s/defn parse-item-groups :- (s/maybe ItemGroupData)
  [form-data :- Element]
  (reduce parse-item-group nil (xml-> form-data :ItemGroupData)))

(s/defn parse-form :- FormData
  [r :- (s/maybe FormData) form-data :- Element]
  (->> (-> (assoc-when nil :tx-type (tx-type form-data))
           (assoc-when :item-groups (parse-item-groups form-data)))
       (assoc r (oid form-data :FormOID))))

(s/defn parse-forms :- (s/maybe FormData)
  [study-event-data :- Element]
  (reduce parse-form nil (xml-> study-event-data :FormData)))

(s/defn parse-study-event :- StudyEventData
  [r :- (s/maybe StudyEventData) study-event-data :- Element]
  (->> (-> (assoc-when nil :tx-type (tx-type study-event-data))
           (assoc-when :forms (parse-forms study-event-data)))
       (assoc r (oid study-event-data :StudyEventOID))))

(s/defn parse-study-events :- (s/maybe StudyEventData)
  [subject-data :- Element]
  (reduce parse-study-event nil (xml-> subject-data :StudyEventData)))

(s/defn parse-subject :- SubjectData
  [r :- (s/maybe SubjectData) subject-data :- Element]
  (->> (-> (assoc-when nil :tx-type (tx-type subject-data))
           (assoc-when :study-events (parse-study-events subject-data)))
       (assoc r (subject-key subject-data))))

(s/defn parse-subjects :- (s/maybe SubjectData)
  [clinical-data :- Element]
  (reduce parse-subject nil (xml-> clinical-data :SubjectData)))

(s/defn parse-clinical-datum :- ClinicalData
  [r :- (s/maybe ClinicalData) clinical-data :- Element]
  (->> (assoc-when nil :subjects (parse-subjects clinical-data))
       (assoc r (oid clinical-data :StudyOID))))

(s/defn parse-clinical-data :- (s/maybe ClinicalData)
  [odm-file :- Element]
  (reduce parse-clinical-datum nil (xml-> odm-file :ClinicalData)))

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
  (-> {:file-type (file-type odm-file)
       :file-oid (oid odm-file :FileOID)
       :creation-date-time (creation-date-time odm-file)}
      (assoc-when :clinical-data (parse-clinical-data odm-file))))
