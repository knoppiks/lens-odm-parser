(ns lens-odm-parser.core-test
  (:require [clj-time.core :as t]
            [clojure.data.xml :as xml]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [juxt.iota :refer [given]]
            [lens-odm-parser.core :as p :refer :all])
  (:import [java.util Date]))

(st/instrument)

(defmacro given-problems [spec val & body]
  `(given (::s/problems (s/explain-data ~spec ~val))
     ~@body))

(deftest snapshot-file-test
  (is (s/valid? ::p/snapshot-file {:file-type :snapshot :file-oid "F1"
                                   :creation-date-time (Date.)}))
  (given-problems ::p/snapshot-file {:file-type :transactional
                                     :file-oid "F1"
                                     :creation-date-time (Date.)}
    [0 :pred] := `(fn [~'%] (= :snapshot (:file-type ~'%)))))

(deftest transactional-file-test
  (is (s/valid? ::p/transactional-file {:file-type :transactional :file-oid "F1"
                                        :creation-date-time (Date.)}))
  (given-problems ::p/transactional-file {:file-type :snapshot
                                          :file-oid "F1"
                                          :creation-date-time (Date.)}
    [0 :pred] := `(fn [~'%] (= :transactional (:file-type ~'%)))))

(deftest item-spec-test
  (testing "valid items"
    (are [x] (s/valid? ::p/item x)
      {:data-type :string :string-value "foo"}
      {:data-type :integer :integer-value 1}
      {:data-type :float :float-value 1.1M}
      {:data-type :date-time :date-time-value (Date.)}))
  (testing "invalid data type"
    (given-problems ::p/item {:data-type :foo}
      [0 :pred] := `item-spec
      [0 :path] := [:foo]
      [0 :via] := [::p/item]))
  (testing "invalid string value"
    (given-problems ::p/item {:data-type :string :string-value 1}
      [0 :pred] := `string?
      [0 :path] := [:string :string-value]
      [0 :via] := [::p/item ::p/string-value]))
  (testing "invalid transaction type"
    (given-problems ::p/item {:data-type :string :string-value "foo"
                              :tx-type :foo}
      [0 :pred] := #{:remove :insert :update :context :upsert}
      [0 :path] := [:string :tx-type]
      [0 :via] := [::p/item :odm/tx-type])))

(deftest item-group-spec
  (testing "invalid transaction type"
    (given-problems ::p/item-group {:tx-type :foo}
      [0 :pred] := #{:remove :insert :update :context :upsert}
      [0 :path] := [:tx-type]
      [0 :via] := [::p/item-group :odm/tx-type])))

(deftest form-spec
  (testing "invalid transaction type"
    (given-problems ::p/form {:tx-type :foo}
      [0 :pred] := #{:remove :insert :update :context :upsert}
      [0 :path] := [:tx-type]
      [0 :via] := [::p/form :odm/tx-type])))

(deftest study-event-spec
  (testing "invalid transaction type"
    (given-problems ::p/study-event {:tx-type :foo}
      [0 :pred] := #{:remove :insert :update :context :upsert}
      [0 :path] := [:tx-type]
      [0 :via] := [::p/study-event :odm/tx-type])))

(deftest subject-spec
  (testing "invalid transaction type"
    (given-problems ::p/subject {:tx-type :foo}
      [0 :pred] := #{:remove :insert :update :context :upsert}
      [0 :path] := [:tx-type]
      [0 :via] := [::p/subject :odm/tx-type])))

(defn- element [sexp]
  (xml/sexp-as-element sexp))

(deftest oid-test
  (testing "valid OID's"
    (are [s-expr attr oid] (= oid (p/oid (element s-expr) attr))
      [:ODM {:FileOID "f-191926"}] :FileOID "f-191926"
      [:Study {:StudyOID "s-191935"}] :StudyOID "s-191935"))
  (testing "invalid OID"
    (is (thrown-with-msg? Exception #"Invalid value"
                          (oid (element [:Study {:StudyOID ""}]) :StudyOID)))))

(deftest subject-key-test
  (testing "valid subject key"
    (are [s-expr subject-key] (= subject-key (p/subject-key (element s-expr)))
      [:SubjectData {:SubjectKey "sub-192052"}] "sub-192052"))
  (is (thrown-with-msg? Exception #"Invalid value"
                        (subject-key (element [:SubjectData {:SubjectKey ""}])))))

(deftest tx-type-test
  (testing "valid transaction types"
    (are [transaction-type tx] (= tx (tx-type (element [:Foo {:TransactionType transaction-type}])))
      "Insert" :insert
      "Update" :update
      "Remove" :remove
      "Context" :context))
  (testing "invalid transaction type"
    (is (thrown-with-msg? Exception #"Invalid value" (tx-type (element [:Foo {:TransactionType "Foo"}]))))))

(deftest parse-string-item-test
  (are [s v] (= v (string-value (element [:X s])))
    "1" "1"
    "a" "a"
    " " " "
    "  " "  "
    " x " " x "))

(deftest parse-integer-item-test
  (are [s v] (= v (integer-value (element [:X s])))
    "1" 1)
  (is (thrown-with-msg? Exception #"Invalid value" (integer-value (element [:X "a"]))))
  (is (thrown-with-msg? Exception #"Invalid value" (integer-value (element [:X "1.1"])))))

(deftest parse-float-item-test
  (are [s v] (= v (float-value (element [:X s])))
    "1.1" 1.1M
    "1" 1M)
  (is (thrown-with-msg? Exception #"Invalid value" (float-value (element [:X "a"])))))

(deftest date-time-value-test
  (are [s v] (= v (date-time-value (element [:X s])))
    "2016-03-18T14:41:00Z" #inst "2016-03-18T14:41:00Z"
    "2016-03-18T14:41:00.23Z" #inst "2016-03-18T14:41:00.23Z"
    "2016-03-18T14:41:00.23+01:00" #inst "2016-03-18T14:41:00.23+01:00"))

(deftest xml-file-type-test
  (are [x c] (= c (s/conform :odm.xml/file-type x))
    "Snapshot" :snapshot
    "Transactional" :transactional
    "snapshot" :snapshot
    "transactional" :transactional
    "SNAPSHOT" :snapshot
    "TRANSACTIONAL" :transactional)
  (given-problems :odm.xml/file-type nil
    [0 :pred] := 'conform-file-type
    [0 :via] := [:odm.xml/file-type])
  (given-problems :odm.xml/file-type "foo"
    [0 :pred] := 'conform-file-type
    [0 :via] := [:odm.xml/file-type])
  (are [x u] (= u (s/unform :odm.xml/file-type x))
    :snapshot "Snapshot"
    :transactional "Transactional")
  (testing "generator is available"
    (is (s/exercise :odm.xml/file-type))))

(deftest xml-date-time-test
  (are [x c] (= c (s/conform :odm.xml/date-time x))
    "1969-12-31T23:59:59.999Z" #inst "1969-12-31T23:59:59.999Z")
  (given-problems :odm.xml/date-time nil
    [0 :pred] := 'conform-date-time
    [0 :via] := [:odm.xml/date-time])
  (given-problems :odm.xml/date-time "foo"
    [0 :pred] := 'conform-date-time
    [0 :via] := [:odm.xml/date-time])
  (are [x u] (= u (s/unform :odm.xml/date-time x))
    (t/date-time 1969 12 31 23 59 59 999) "1969-12-31T23:59:59.999Z")
  (testing "generator is available"
    (is (s/exercise :odm.xml/date-time))))

(deftest parse-item-group-test
  (are [sexp item-group] (= item-group (parse-item-group (element sexp)))
    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}]
    {:tx-type :insert}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :string :string-value "1"}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataInteger {:ItemOID "I1"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :integer :integer-value 1}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataFloat {:ItemOID "I1"} "1.1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :float :float-value 1.1M}}}

    [:ItemGroupData {:ItemGroupOID "IG1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "I1"} "1"]
     [:ItemDataInteger {:ItemOID "I2"} "1"]]
    {:tx-type :insert
     :items
     {"I1" {:data-type :string :string-value "1"}
      "I2" {:data-type :integer :integer-value 1}}}))

(defspec item-group-parse-unparse-check 10
  (prop/for-all [item-group (s/gen ::p/item-group)]
    (= item-group (parse-item-group (element (unparse-item-group "x" item-group))))))

(deftest parse-form-test
  (are [sexp form-data] (= form-data (parse-form (element sexp)))
    [:FormData {:FormOID "F1"}]
    {}

    [:FormData {:FormOID "F1" :TransactionType "Insert"}]
    {:tx-type :insert}

    [:FormData {:FormOID "F1"}
     [:ItemGroupData {:ItemGroupOID "IG1"}]]
    {:item-groups
     {"IG1" {}}}))

(defspec form-parse-unparse-check 10
  (prop/for-all [form (s/gen ::p/form)]
    (= form (parse-form (element (unparse-form "x" form))))))

(deftest parse-study-event-test
  (are [sexp study-event-data] (= study-event-data (parse-study-event (element sexp)))
    [:StudyEventData {:StudyEventOID "SE1"}]
    {}

    [:StudyEventData {:StudyEventOID "SE1" :TransactionType "Insert"}]
    {:tx-type :insert}

    [:StudyEventData {:StudyEventOID "SE1"}
     [:FormData {:FormOID "F1"}]]
    {:forms
     {"F1" {}}}))

(defspec study-event-parse-unparse-check 3
  (prop/for-all [study-event (s/gen ::p/study-event)]
    (= study-event (parse-study-event (element (unparse-study-event "x" study-event))))))

(deftest parse-subject-test
  (are [sexp subject-data] (= subject-data (parse-subject (element sexp)))
    [:SubjectData {:SubjectKey "SK1"}]
    {}

    [:SubjectData {:SubjectKey "SK1" :TransactionType "Insert"}]
    {:tx-type :insert}

    [:SubjectData {:SubjectKey "SK1"}
     [:StudyEventData {:StudyEventOID "SE1"}]]
    {:study-events
     {"SE1" {}}}))

(defspec subject-parse-unparse-check 3
  (prop/for-all [subject (s/gen ::p/subject)]
    (= subject (parse-subject (element (unparse-subject "x" subject))))))

(deftest parse-clinical-datum-test
  (are [sexp clinical-datum] (= clinical-datum (parse-clinical-datum (element sexp)))
    [:ClinicalData {:StudyOID "S1"}]
    {}

    [:ClinicalData
     {:StudyOID "S1"}
     [:SubjectData {:SubjectKey "SK1"}]]
    {:subjects
     {"SK1" {}}}))

(defspec clinical-datum-parse-unparse-check 3
  (prop/for-all [clinical-datum (s/gen ::p/clinical-datum)]
    (= clinical-datum (parse-clinical-datum (element (unparse-clinical-datum "x" clinical-datum))))))

(deftest parse-odm-file-test
  (are [sexp odm-file] (= odm-file (parse-odm-file (element sexp)))
    [:ODM
     {:FileType "Snapshot"
      :FileOID "F1"
      :CreationDateTime "2016-03-18T14:41:00Z"}]
    {:file-type :snapshot
     :file-oid "F1"
     :creation-date-time #inst "2016-03-18T14:41:00Z"}

    [:ODM
     {:FileType "Snapshot"
      :FileOID "F1"
      :CreationDateTime "2016-03-18T14:41:00Z"}
     [:ClinicalData {:StudyOID "S1"}]]
    {:file-type :snapshot
     :file-oid "F1"
     :creation-date-time #inst "2016-03-18T14:41:00Z"
     :clinical-data {"S1" {}}}))

(defspec odm-file-parse-unparse-check 1
  (prop/for-all [file (s/gen ::p/file)]
    (= file (parse-odm-file (element (unparse-odm-file file))))))
