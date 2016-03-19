(ns lens-odm-parser.core
  "The two main functions are parse-odm-file and unparse-odm-file."
  (:require [clj-time.core :refer [now]]
            [clj-time.format :as tf]
            [clojure.data.zip :as dz]
            [clojure.data.zip.xml :as xml :refer [xml-> xml1-> attr text]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [plumbing.core :refer [assoc-when]]
            [schema.coerce :as c]
            [schema.core :as s :refer [Str]]
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
  (s/constrained {(s/named OID "item-oid") Item} seq 'non-empty?))

(def ItemGroup
  {(s/optional-key :tx-type) TxType
   (s/optional-key :items) ItemData})

(def ItemGroupData
  (s/constrained {(s/named OID "item-group-oid") ItemGroup} seq 'non-empty?))

(def Form
  {(s/optional-key :tx-type) TxType
   (s/optional-key :item-groups) ItemGroupData})

(def FormData
  (s/constrained {(s/named OID "form-oid") Form} seq 'non-empty?))

(def StudyEvent
  {(s/optional-key :tx-type) TxType
   (s/optional-key :forms) FormData})

(def StudyEventData
  (s/constrained {(s/named OID "study-event-oid") StudyEvent} seq 'non-empty?))

(def Subject
  {(s/optional-key :tx-type) TxType
   (s/optional-key :study-events) StudyEventData})

(def SubjectData
  (s/constrained {SubjectKey Subject} seq 'non-empty?))

(def ClinicalDatum
  {(s/optional-key :subjects) SubjectData})

(def ClinicalData
  (s/constrained {(s/named OID "study-oid") ClinicalDatum} seq 'non-empty?))

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

(defn text-with-whitespace [loc]
  (apply str (xml-> loc dz/descendants zip/node string?)))

(defn string-data [loc]
  (xml1-> loc text-with-whitespace))

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
  [loc :- Element]
  (some->> (xml1-> loc (attr :TransactionType))
           (validate transaction-type-checker loc)
           (str/lower-case)
           (keyword)))

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
