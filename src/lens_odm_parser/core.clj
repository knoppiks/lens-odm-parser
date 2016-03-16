(ns lens-odm-parser.core
  (:require [clj-time.coerce :as tc]
            [clj-time.format :as tf]
            [clojure.data.zip.xml :refer [xml-> xml1-> attr text]]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [schema.coerce :as c]
            [schema.core :as s :refer [Uuid Str Int Any Num]]
            [schema.utils :as su])
  (:import [java.util Date]))

;; ---- Schemas ---------------------------------------------------------------

(def NonBlankStr
  (s/constrained s/Str (complement str/blank?) 'non-blank?))

(def OID
  NonBlankStr)

(def SubjectKey
  NonBlankStr)

(def RepeatKey
  NonBlankStr)

(def TransactionType
  (s/enum "Insert" "Update" "Remove" "Upsert" "Context"))

(def TxType
  (s/enum :insert :update :remove :upsert :context))

(def DataType
  (s/enum :string :integer :float :datetime))

(def ItemData
  {:item-oid OID
   :tx-type TxType
   :data-type DataType
   :value Any})

(def ItemGroupData
  {:item-group-oid OID
   :tx-type TxType
   :items [ItemData]})

(def FormData
  {:form-oid OID
   :tx-type TxType
   :item-groups [ItemGroupData]})

(def StudyEventData
  {:study-event-oid OID
   :tx-type TxType
   :forms [FormData]})

(def SubjectData
  {:subject-key SubjectKey
   :tx-type TxType
   :study-events [StudyEventData]})

(def ClinicalData
  {:study-oid OID
   :subjects [SubjectData]})

;; ---- Parsing ---------------------------------------------------------------

(defn- validation-ex [{:keys [schema value]} loc]
  (ex-info (format "Invalid value %s should be a %s" value schema)
           {:type ::validation-error :schema schema :value value :loc loc}))

(defn- validate [checker loc value]
  (if-let [error (checker value)]
    (throw (validation-ex error loc))
    value))

(defn- coerce [coercer loc value]
  (let [value (coercer value)]
    (if-let [error (su/error-val value)]
      (throw (validation-ex error loc))
      value)))

(def oid-checker (s/checker OID))

(defn study-oid [loc]
  (->> (xml1-> loc (attr :StudyOID))
       (validate oid-checker loc)))

(def subject-key-checker (s/checker SubjectKey))

(defn subject-key [loc]
  (->> (xml1-> loc (attr :SubjectKey))
       (validate subject-key-checker loc)))

(defn study-event-oid [loc]
  (->> (xml1-> loc (attr :StudyEventOID))
       (validate oid-checker loc)))

(defn form-oid [loc]
  (->> (xml1-> loc (attr :FormOID))
       (validate oid-checker loc)))

(defn item-group-oid [loc]
  (->> (xml1-> loc (attr :ItemGroupOID))
       (validate oid-checker loc)))

(defn item-oid [loc]
  (->> (xml1-> loc (attr :ItemOID))
       (validate oid-checker loc)))

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

(defn parse-date [s]
  (tc/to-date (tf/parse (tf/formatters :date-time) s)))

(def datetime-coercer
  (c/coercer Date {Date (c/safe parse-date)}))

(defn datetime-data [loc]
  (->> (xml1-> loc text)
       (coerce datetime-coercer loc)))

(def transaction-type-checker (s/checker TransactionType))

(s/defn tx-type :- TxType
  "Tries to determine the transaction type of a loc. Does so recursive to
  parents. Throws a validation error on invalid or missing transaction type."
  [loc]
  (if-let [t (xml1-> loc (attr :TransactionType))]
    (-> (validate transaction-type-checker loc t)
        (str/lower-case)
        (keyword))
    (if-let [parent (zip/up loc)]
      (tx-type parent)
      (validate transaction-type-checker loc nil))))

(s/defn parse-string-item [item-data]
  {:item-oid (item-oid item-data)
   :tx-type (tx-type item-data)
   :data-type :string
   :value (string-data item-data)})

(s/defn parse-integer-item [item-data]
  {:item-oid (item-oid item-data)
   :tx-type (tx-type item-data)
   :data-type :integer
   :value (integer-data item-data)})

(s/defn parse-float-item [item-data]
  {:item-oid (item-oid item-data)
   :tx-type (tx-type item-data)
   :data-type :float
   :value (float-data item-data)})

(s/defn parse-datetime-item [item-data]
  {:item-oid (item-oid item-data)
   :tx-type (tx-type item-data)
   :data-type :datetime
   :value (datetime-data item-data)})

(s/defn parse-item-group [item-group-data]
  {:item-group-oid (item-group-oid item-group-data)
   :tx-type (tx-type item-group-data)
   :items
   (-> []
       (into (map parse-string-item)
             (xml-> item-group-data :ItemDataString))
       (into (map parse-integer-item)
             (xml-> item-group-data :ItemDataInteger))
       (into (map parse-float-item)
             (xml-> item-group-data :ItemDataFloat))
       (into (map parse-datetime-item)
             (xml-> item-group-data :ItemDataDatetime)))})

(s/defn parse-form [form-data]
  {:form-oid (form-oid form-data)
   :tx-type (tx-type form-data)
   :item-groups (mapv parse-item-group (xml-> form-data :ItemGroupData))})

(s/defn parse-study-event [study-event-data]
  {:study-event-oid (study-event-oid study-event-data)
   :tx-type (tx-type study-event-data)
   :forms (mapv parse-form (xml-> study-event-data :FormData))})

(s/defn parse-subject [subject-data]
  {:subject-key (subject-key subject-data)
   :tx-type (tx-type subject-data)
   :study-events (mapv parse-study-event (xml-> subject-data :StudyEventData))})

(s/defn parse-clinical-data :- ClinicalData [clinical-data]
  {:study-oid (study-oid clinical-data)
   :subjects (mapv parse-subject (xml-> clinical-data :SubjectData))})

