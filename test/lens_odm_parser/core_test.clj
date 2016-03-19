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
  (is (nil? (s/check SnapshotODMFile {:file-type :snapshot :file-oid "F1" :creation-date-time (now)})))
  (is (some? (s/check SnapshotODMFile {:file-type :transactional :file-oid "F1"}))))

(deftest TransactionalODMFile-test
  (is (nil? (s/check TransactionalODMFile {:file-type :transactional :file-oid "F1" :creation-date-time (now)})))
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
  (are [sexp item] (= item (parse-string-item nil (zipper sexp)))
    [:ItemDataString {:ItemOID "I1" :TransactionType "Insert"} "1"]
    {"I1" {:tx-type :insert :data-type :string :value "1"}}))

(deftest parse-integer-item-test
  (are [sexp item] (= item (parse-integer-item nil (zipper sexp)))
    [:ItemDataInteger {:ItemOID "I1" :TransactionType "Insert"} "1"]
    {"I1" {:tx-type :insert :data-type :integer :value 1}})
  (let [sexp [:ItemDataInteger {:ItemOID "I1" :TransactionType "Insert"} "a"]]
    (is (thrown? Exception (parse-integer-item nil (zipper sexp))))))

(deftest parse-float-item-test
  (are [sexp item] (= item (parse-float-item nil (zipper sexp)))
    [:ItemDataFloat {:ItemOID "I1" :TransactionType "Insert"} "1.1"]
    {"I1" {:tx-type :insert :data-type :float :value 1.1}})
  (let [sexp [:ItemDataFloat {:ItemOID "I1" :TransactionType "Insert"} "1"]]
    (is (float? (:value ((parse-float-item nil (zipper sexp)) "I1")))))
  (let [sexp [:ItemDataFloat {:ItemOID "I1" :TransactionType "Insert"} "a"]]
    (is (thrown? Exception (parse-float-item nil (zipper sexp))))))

(deftest parse-date-time-item-test
  (testing "UTC no millis"
    (are [sexp item] (= item (parse-date-time-item nil (zipper sexp)))
      [:ItemDataDatetime {:ItemOID "I1"} "2016-03-18T14:41:00Z"]
      {"I1" {:data-type :date-time :value (date-time 2016 3 18 14 41)}}))
  (testing "UTC with millis"
    (are [sexp item] (= item (parse-date-time-item nil (zipper sexp)))
      [:ItemDataDatetime {:ItemOID "I1"} "2016-03-18T14:41:00.23Z"]
      {"I1" {:data-type :date-time :value (date-time 2016 3 18 14 41 0 230)}}))
  (testing "+01:00 with millis"
    (are [sexp item] (= item (parse-date-time-item nil (zipper sexp)))
      [:ItemDataDatetime {:ItemOID "I1"} "2016-03-18T14:41:00.23+01:00"]
      {"I1" {:data-type :date-time :value (date-time 2016 3 18 13 41 0 230)}})))

(deftest parse-item-group-test
  (are [sexp item-group] (= item-group (parse-item-group nil (zipper sexp)))
    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}]
    {"IG1" {:tx-type :insert}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]]
    {"IG1"
     {:tx-type :insert
      :items
      {"I1" {:data-type :string :value "1"}}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataInteger {:ItemOID "I1"} "1"]]
    {"IG1"
     {:tx-type :insert
      :items
      {"I1" {:data-type :integer :value 1}}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataFloat {:ItemOID "I1"} "1.1"]]
    {"IG1"
     {:tx-type :insert
      :items
      {"I1" {:data-type :float :value 1.1}}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]
     [:ItemDataInteger {:ItemOID "I2"} "1"]]
    {"IG1"
     {:tx-type :insert
      :items
      {"I1" {:data-type :string :value "1"}
       "I2" {:data-type :integer :value 1}}}}))

(deftest parse-form-test
  (are [sexp form-data] (= form-data (parse-form nil (zipper sexp)))
    [:FormData {:FormOID "F1"}]
    {"F1" {}}
    [:FormData {:FormOID "F1" :TransactionType "Insert"}]
    {"F1" {:tx-type :insert}}
    [:FormData {:FormOID "F1"}
     [:ItemGroupData {:ItemGroupOID "IG1"}]]
    {"F1"
     {:item-groups
      {"IG1" {}}}}))

(deftest parse-study-event-test
  (are [sexp study-event-data] (= study-event-data (parse-study-event nil (zipper sexp)))
    [:StudyEventData {:StudyEventOID "SE1"}]
    {"SE1" {}}
    [:StudyEventData {:StudyEventOID "SE1" :TransactionType "Insert"}]
    {"SE1" {:tx-type :insert}}
    [:StudyEventData {:StudyEventOID "SE1"}
     [:FormData {:FormOID "F1"}]]
    {"SE1"
     {:forms
      {"F1" {}}}}))

(deftest parse-subject-test
  (are [sexp subject-data] (= subject-data (parse-subject nil (zipper sexp)))
    [:SubjectData {:SubjectKey "SK1"}]
    {"SK1" {}}
    [:SubjectData {:SubjectKey "SK1" :TransactionType "Insert"}]
    {"SK1" {:tx-type :insert}}
    [:SubjectData {:SubjectKey "SK1"}
     [:StudyEventData {:StudyEventOID "SE1"}]]
    {"SK1"
     {:study-events
      {"SE1" {}}}}))

(deftest parse-clinical-datum-test
  (are [sexp clinical-datum] (= clinical-datum (parse-clinical-datum nil (zipper sexp)))
    [:ClinicalData {:StudyOID "S1"}]
    {"S1" {}}
    [:ClinicalData
     {:StudyOID "S1"}
     [:SubjectData {:SubjectKey "SK1"}]]
    {"S1"
     {:subjects
      {"SK1" {}}}}))

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
     :creation-date-time (date-time 2016 3 18 14 41)}
    [:ODM
     {:FileType "Snapshot"
      :FileOID "F1"
      :CreationDateTime "2016-03-18T14:41:00Z"}
     [:ClinicalData {:StudyOID "S1"}]]
    {:file-type :snapshot
     :file-oid "F1"
     :creation-date-time (date-time 2016 3 18 14 41)
     :clinical-data {"S1" {}}}))

(def date-time-generator (gen/return (date-time 2016 3 18 14 41)))

(defspec parse-unparse-check 15
  (prop/for-all [file (g/generator ODMFile {DateTime date-time-generator})]
    (= file (parse-odm-file (zipper (unparse-odm-file file))))))
