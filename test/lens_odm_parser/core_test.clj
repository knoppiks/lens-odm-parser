(ns lens-odm-parser.core-test
  (:require [clj-time.core :refer [now date-time]]
            [clojure.data.xml :as xml]
            [clojure.data.zip.xml :refer [xml1->]]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.zip :as zip]
            [lens-odm-parser.core :refer :all]
            [schema.core :as s]
            [schema.test :refer [validate-schemas]]
            [schema.experimental.generators :as g]
            [schema.utils :as su])
  (:import [org.joda.time DateTime]))

(use-fixtures :once validate-schemas)

(deftest SnapshotODMFile-test
  (is (nil? (s/check SnapshotODMFile {:file-type :snapshot :file-oid "F1" :creation-date-time (now) :clinical-data {}})))
  (is (some? (s/check SnapshotODMFile {:file-type :transactional :file-oid "F1"}))))

(deftest TransactionalODMFile-test
  (is (nil? (s/check TransactionalODMFile {:file-type :transactional :file-oid "F1" :creation-date-time (now) :clinical-data {}})))
  (is (some? (s/check TransactionalODMFile {:file-type :snapshot :file-oid "F1"}))))

(defn- zipper [sexp]
  (zip/xml-zip (xml/sexp-as-element sexp)))

(deftest tx-type-test
  (testing "valid transaction types"
    (are [transaction-type tx] (= tx (tx-type (zipper [:Foo {:TransactionType transaction-type}])))
      "Insert" :insert
      "Update" :update
      "Remove" :remove
      "Context" :context))
  (testing "invalid transaction type"
    (is (thrown? Exception (tx-type (zipper [:Foo {:TransactionType "Foo"}]))))))

(deftest parse-string-item-test
  (are [s v] (= v (string-value (zipper [:X s])))
    "1" "1"
    "a" "a"
    " " " "
    "  " "  "
    " x " " x "))

(deftest parse-integer-item-test
  (are [s v] (= v (integer-value (zipper [:X s])))
    "1" 1)
  (is (thrown? Exception (integer-value (zipper [:X "a"]))))
  (is (thrown? Exception (integer-value (zipper [:X "1.1"])))))

(deftest parse-float-item-test
  (are [s v] (= v (float-value (zipper [:X s])))
    "1.1" 1.1
    "1" 1.0)
  (is (thrown? Exception (float-value (zipper [:X "a"])))))

(deftest date-time-value-test
  (are [s v] (= v (date-time-value (zipper [:X s])))
    "2016-03-18T14:41:00Z" (date-time 2016 3 18 14 41)
    "2016-03-18T14:41:00.23Z" (date-time 2016 3 18 14 41 0 230)
    "2016-03-18T14:41:00.23+01:00" (date-time 2016 3 18 13 41 0 230)))

(deftest parse-item-group-test
  (are [sexp item-group] (= item-group (parse-item-group (zipper sexp)))
    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}]
    {:tx-type :insert :items {}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :string :value "1"}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataInteger {:ItemOID "I1"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :integer :value 1}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataFloat {:ItemOID "I1"} "1.1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :float :value 1.1}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]
     [:ItemDataInteger {:ItemOID "I2"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :string :value "1"}
      "I2" {:data-type :integer :value 1}}}))

(deftest parse-form-test
  (are [sexp form-data] (= form-data (parse-form (zipper sexp)))
    [:FormData {:FormOID "F1"}]
    {:item-groups {}}

    [:FormData {:FormOID "F1" :TransactionType "Insert"}]
    {:tx-type :insert :item-groups {}}

    [:FormData {:FormOID "F1"}
     [:ItemGroupData {:ItemGroupOID "IG1"}]]
    {:item-groups
     {"IG1" {:items {}}}}))

(deftest parse-study-event-test
  (are [sexp study-event-data] (= study-event-data (parse-study-event (zipper sexp)))
    [:StudyEventData {:StudyEventOID "SE1"}]
    {:forms {}}

    [:StudyEventData {:StudyEventOID "SE1" :TransactionType "Insert"}]
    {:tx-type :insert :forms {}}

    [:StudyEventData {:StudyEventOID "SE1"}
     [:FormData {:FormOID "F1"}]]
    {:forms
     {"F1" {:item-groups {}}}}))

(deftest parse-subject-test
  (are [sexp subject-data] (= subject-data (parse-subject (zipper sexp)))
    [:SubjectData {:SubjectKey "SK1"}]
    {:study-events {}}

    [:SubjectData {:SubjectKey "SK1" :TransactionType "Insert"}]
    {:tx-type :insert :study-events {}}

    [:SubjectData {:SubjectKey "SK1"}
     [:StudyEventData {:StudyEventOID "SE1"}]]
    {:study-events
     {"SE1" {:forms {}}}}))

(deftest parse-clinical-datum-test
  (are [sexp clinical-datum] (= clinical-datum (parse-clinical-datum (zipper sexp)))
    [:ClinicalData {:StudyOID "S1"}]
    {:subjects {}}

    [:ClinicalData
     {:StudyOID "S1"}
     [:SubjectData {:SubjectKey "SK1"}]]
    {:subjects
     {"SK1" {:study-events {}}}}))

(deftest file-type-coercer-test
  (is (= :snapshot (file-type-coercer "Snapshot")))
  (is (= :transactional (file-type-coercer "Transactional")))
  (is (= "(not (#{:transactional :snapshot} :foo))" (pr-str (su/error-val (file-type-coercer "foo")))))
  (is (= "(not (#{:transactional :snapshot} nil))" (pr-str (su/error-val (file-type-coercer nil))))))

(deftest parse-odm-file-test
  (are [sexp odm-file] (= odm-file (parse-odm-file (zipper sexp)))
    [:ODM
     {:FileType "Snapshot"
      :FileOID "F1"
      :CreationDateTime "2016-03-18T14:41:00Z"}]
    {:file-type :snapshot
     :file-oid "F1"
     :creation-date-time (date-time 2016 3 18 14 41)
     :clinical-data {}}
    [:ODM
     {:FileType "Snapshot"
      :FileOID "F1"
      :CreationDateTime "2016-03-18T14:41:00Z"}
     [:ClinicalData {:StudyOID "S1"}]]
    {:file-type :snapshot
     :file-oid "F1"
     :creation-date-time (date-time 2016 3 18 14 41)
     :clinical-data {"S1" {:subjects {}}}}))

(def date-time-generator (gen/return (date-time 2016 3 18 14 41)))

(defspec parse-unparse-check 10
  (prop/for-all [file (g/generator ODMFile {DateTime date-time-generator})]
    (= file (parse-odm-file (zipper (unparse-odm-file file))))))
