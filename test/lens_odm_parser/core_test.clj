(ns lens-odm-parser.core-test
  (:require
    [clj-time.core :as t]
    [clojure.data.xml :as xml]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.test :refer :all]
    [com.gfredericks.test.chuck.clojure-test :refer [checking]]
    [juxt.iota :refer [given]]
    [lens-odm-parser.core :refer :all]
    [odm.clinical-data :as clinical-data]
    [odm.metadata-version :as metadata-version]
    [odm.file :as file]
    [odm.form-def :as form-def]
    [odm.form-data :as form-data]
    [odm.item-data :as item-data]
    [odm.item-def :as item-def]
    [odm.item-ref :as item-ref]
    [odm.item-group-data :as item-group-data]
    [odm.item-group-def :as item-group-def]
    [odm.item-group-ref :as item-group-ref]
    [odm.study :as study]
    [odm.study-event-data :as study-event-data]
    [odm.subject-data :as subject-data])
  (:import
    [java.math MathContext]))

(st/instrument)

(defmacro given-problems [spec val & body]
  `(given (::s/problems (s/explain-data ~spec ~val))
     ~@body))

(defn- element [sexp]
  (xml/sexp-as-element sexp))

(deftest tx-type-test
  (testing "valid transaction types"
    (are [transaction-type tx] (= tx (-> [:FormData {:TransactionType transaction-type}]
                                         (element) (parse-elem)
                                         (get-in [::study-event-data/form-data 0 :odm/tx-type])))
      "Insert" :insert
      "Update" :update
      "Remove" :remove
      "Context" :context))
  (testing "invalid transaction type"
    (is (thrown-with-msg? Exception #"Invalid value" (parse-elem (element [:FormData {:TransactionType "Foo"}]))))))

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
    "1.1" 1.1
    "1" 1)
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
    "1969-12-31T23:59:59.999Z" #inst "1969-12-31T23:59:59.999Z"
    "1969-12-31T23:59:59Z" #inst "1969-12-31T23:59:59.000Z"
    "1969-12-31T23:59:59" #inst "1969-12-31T23:59:59.000-00:00")
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

(deftest unparse-item-data-test
  (testing "Non-finite doubles are not unparsable"
    (are [x] (thrown-with-msg?
               Exception #"Unable to unparse non-finite double"
               (unparse-item-data
                 #::item-data{:item-oid "I01"
                              :data-type :float
                              :float-value x}))
      Double/POSITIVE_INFINITY
      Double/NEGATIVE_INFINITY
      Double/NaN)))

(defmacro given-parsed [sexp & body]
  `(given (-> ~sexp element parse-elem first second first)
     ~@body))

(deftest parse-item-data-test
  (testing "String item data"
    (given-parsed
      [:ItemDataString {:ItemOID "I01"} "1"]

      ::item-data/item-oid := "I01"
      ::item-data/data-type := :string
      ::item-data/string-value := "1"))

  (testing "Integer item data"
    (given-parsed
      [:ItemDataInteger {:ItemOID "I01"} "1"]

      ::item-data/item-oid := "I01"
      ::item-data/data-type := :integer
      ::item-data/integer-value := 1))

  (testing "Float item data"
    (given-parsed
      [:ItemDataFloat {:ItemOID "I01"} "1"]

      ::item-data/item-oid := "I01"
      ::item-data/data-type := :float
      ::item-data/float-value := 1))

  (testing "Date-time item data"
    (given-parsed
      [:ItemDataDatetime {:ItemOID "I01"} "2017-02-13T07:50:00.000Z"]

      ::item-data/item-oid := "I01"
      ::item-data/data-type := :date-time
      ::item-data/date-time-value := #inst "2017-02-13T07:50:00.000Z")))

(defn double= [x y]
  (= (.round (bigdec x) MathContext/DECIMAL64)
     (.round (bigdec y) MathContext/DECIMAL64)))

(defn- =double
  "Like = but compares :odm.item-data/float-value by rounding it to double
  precision."
  [x y]
  (cond
    (and (::item-data/float-value x) (::item-data/float-value y))
    (and (=double (dissoc x ::item-data/float-value) (dissoc y ::item-data/float-value))
         (double= (::item-data/float-value x) (::item-data/float-value y)))
    (and (map? x) (map? y))
    (and (every? #(contains? x %) (keys y))
         (reduce-kv
           (fn [r k v]
             (and r (=double v (get y k))))
           true
           x))
    (and (coll? x) (coll? y))
    (every? true? (map =double x y))
    :else (= x y)))

(deftest item-data-parse-unparse-test
  (checking "item-data-parse-unparse" 1000
    [item-data (s/gen :odm/item-data)]
    (=double item-data (parse-elem (element (unparse-item-data item-data))))))

(deftest parse-item-group-data-test
  (testing "Item group data without item data"
    (given-parsed
      [:ItemGroupData {:ItemGroupOID "IG01" :TransactionType "Insert"}]

      ::item-group-data/item-group-oid := "IG01"
      :odm/tx-type := :insert))

  (testing "Item group data with one item data element"
    (given-parsed
      [:ItemGroupData {:ItemGroupOID "IG01"}
       [:ItemDataString {:ItemOID "I01"} "1"]]

      [::item-group-data/item-data 0 ::item-data/item-oid] := "I01"
      [::item-group-data/item-data 0 ::item-data/data-type] := :string
      [::item-group-data/item-data 0 ::item-data/string-value] := "1"))

  (testing "Item group data with two item data elements"
    (given-parsed
      [:ItemGroupData {:ItemGroupOID "IG01"}
       [:ItemDataString {:ItemOID "I01"} "1"]
       [:ItemDataInteger {:ItemOID "I02"} "1"]]

      [::item-group-data/item-data 0 ::item-data/item-oid] := "I01"
      [::item-group-data/item-data 0 ::item-data/string-value] := "1"
      [::item-group-data/item-data 1 ::item-data/item-oid] := "I02"
      [::item-group-data/item-data 1 ::item-data/integer-value] := 1)))

(deftest item-group-data-parse-unparse-test
  (checking "item-group-data-parse-unparse" 100
    [item-group-data (s/gen :odm/item-group-data)]
    (=double item-group-data (parse-elem (element (unparse-item-group-data item-group-data))))))

(deftest parse-form-data-test
  (testing "Form data without item group data"
    (given-parsed
      [:FormData {:FormOID "F01"}]

      ::form-data/form-oid := "F01"))

  (testing "Form data with one item group data element"
    (given-parsed
      [:FormData {:FormOID "F01"}
       [:ItemGroupData {:ItemGroupOID "IG01"}]]

      [::form-data/item-group-data 0 ::item-group-data/item-group-oid] := "IG01")))

(deftest form-data-parse-unparse-test
  (checking "form-data-parse-unparse" 100
    [form-data (s/gen :odm/form-data)]
    (=double form-data (parse-elem (element (unparse-form-data form-data))))))

(deftest parse-study-event-data-test
  (testing "Study event data without form data"
    (given-parsed
      [:StudyEventData {:StudyEventOID "SE01"}]

      ::study-event-data/study-event-oid := "SE01"))

  (testing "Study event data with one form data element"
    (given-parsed
      [:StudyEventData {:StudyEventOID "SE01"}
       [:FormData {:FormOID "F01"}]]

      [::study-event-data/form-data 0 ::form-data/form-oid] := "F01")))

(deftest study-event-data-parse-unparse-test
  (checking "study-event-data-parse-unparse" 100
    [study-event-data (s/gen :odm/study-event-data)]
    (=double study-event-data (parse-elem (element (unparse-study-event-data study-event-data))))))

(deftest parse-subject-data-test
  (testing "Subject data without study event data"
    (given-parsed
      [:SubjectData {:SubjectKey "SK01"}]

      ::subject-data/subject-key := "SK01"))

  (testing "Subject data with one study event data element"
    (given-parsed
      [:SubjectData {:SubjectKey "SK01"}
       [:StudyEventData {:StudyEventOID "SE01"}]]

      [::subject-data/study-event-data 0 ::study-event-data/study-event-oid] := "SE01")))

(deftest subject-data-parse-unparse-test
  (checking "subject-data-parse-unparse" 10
    [subject-data (s/gen :odm/subject-data)]
    (=double subject-data (parse-elem (element (unparse-subject-data subject-data))))))

(deftest parse-clinical-data-test
  (testing "Clinical data without subject data"
    (given-parsed
      [:ClinicalData {:StudyOID "S01" :MetaDataVersionOID "V01"}]

      ::clinical-data/study-oid := "S01"
      ::clinical-data/metadata-version-oid := "V01"))

  (testing "Clinical data with one subject data element"
    (given-parsed
      [:ClinicalData {:StudyOID "S01" :MetaDataVersionOID "V01"}
       [:SubjectData {:SubjectKey "SK01"}]]

      [::clinical-data/subject-data 0 ::subject-data/subject-key] := "SK01")))

(deftest question-parse-test
  (testing "Question"
    (given-parsed [:Question [:TranslatedText "Some Translated Text"]]

      :text := "Some Translated Text")))

(deftest study-parse-test
  (testing "Study"
    (given-parsed
      [:Study {:OID "S01"}
       [:GlobalVariables {}
        [:StudyName "Study-Name"]
        [:StudyDescription "This is a study"]
        [:ProtocolName "Some Protocol"]]]

      ::study/oid := "S01"
      ::study/name := "Study-Name"
      ::study/description := "This is a study"
      ::study/protocol-name := "Some Protocol")))

(deftest metadata-parse-test
  (testing "Metadata"
    (given-parsed
      [:MetaDataVersion {:OID "LATEST" :Name "Some-Name" :Description "MV description"}]

      ::metadata-version/description := "MV description"
      ::metadata-version/name := "Some-Name"
      ::metadata-version/oid := "LATEST")))

(deftest metadata-form-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:MetaDataVersion {:OID "LATEST" :Name "Some-Name" :Description "MV description"}
       [:FormDef {:OID "T01" :Name "Form1" :Repeating "No"}]
       [:FormDef {:OID "T02" :Name "Form2" :Repeating "No"}]]

      [::metadata-version/form-defs 0 ::form-def/oid] := "T01"
      [::metadata-version/form-defs 1 ::form-def/oid] := "T02")))

(deftest metadata-item-group-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:MetaDataVersion {:OID "LATEST" :Name "Some-Name" :Description "MV description"}
       [:ItemGroupDef {:OID "T01" :Name "Form1" :Repeating "No"}]
       [:ItemGroupDef {:OID "T02" :Name "Form2" :Repeating "No"}]]

      [::metadata-version/item-group-defs 0 ::item-group-def/oid] := "T01"
      [::metadata-version/item-group-defs 1 ::item-group-def/oid] := "T02")))

(deftest metadata-item-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:MetaDataVersion {:OID "LATEST" :Name "Some-Name" :Description "MV description"}
       [:ItemDef {:OID "T01" :Name "Form1" :DataType "text"}]
       [:ItemDef {:OID "T02" :Name "Form2" :DataType "text"}]]

      [::metadata-version/item-defs 0 ::item-def/oid] := "T01"
      [::metadata-version/item-defs 1 ::item-def/oid] := "T02")))

(deftest form-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:FormDef {:OID "T01" :Name "Form1" :Repeating "No"}
       [:ItemGroupRef {:ItemGroupOID "IG01" :Mandatory "No"}]
       [:ItemGroupRef {:ItemGroupOID "IG02" :Mandatory "No"}]]

      [::form-def/item-group-refs 0 ::item-group-ref/item-group-oid] := "IG01"
      [::form-def/item-group-refs 1 ::item-group-ref/item-group-oid] := "IG02")))

(deftest item-group-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:ItemGroupDef {:OID "T01" :Name "ItemGroup1" :Repeating "No"}
       [:ItemRef {:ItemOID "I01" :Mandatory "No"}]
       [:ItemRef {:ItemOID "I02" :Mandatory "No"}]]

      [::item-group-def/item-refs 0 ::item-ref/item-oid] := "I01"
      [::item-group-def/item-refs 1 ::item-ref/item-oid] := "I02")))

(deftest item-def-parse-test
  (testing "Metadata"
    (given-parsed
      [:ItemDef {:OID "T01" :Name "Form1" :DataType "text"}
       [:CodeListRef {:CodeListOID "CL01"}]]

      [:odm/code-list-ref :odm.code-list-ref/code-list-oid] := "CL01")))

(deftest clinical-data-parse-unparse-test
  (checking "clinical-data-parse-unparse" 10
    [clinical-data (s/gen :odm/clinical-data)]
    (=double clinical-data (parse-elem (element (unparse-clinical-data clinical-data))))))

(deftest parse-file-test
  (testing "File without clinical data"
    (given (-> [:ODM
                {:FileType "Snapshot"
                 :FileOID "FI01"
                 :CreationDateTime "2016-03-18T14:41:00Z"}]
               (element) (parse-elem))

      ::file/oid := "FI01"
      ::file/type := :snapshot
      ::file/creation-date-time := #inst "2016-03-18T14:41:00.000-00:00"))

  (testing "File with clinical data"
    (given-parsed
      [:ODM
       {:FileType "Snapshot"
        :FileOID "FI01"
        :CreationDateTime "2016-03-18T14:41:00Z"}
       [:ClinicalData {:StudyOID "S01" :MetaDataVersionOID "V01"}]]

      ::clinical-data/study-oid := "S01")))

(deftest unparse-file-test
  (testing "Concrete example"
    (given
      (second
        (unparse-file
          {::file/oid "FI01"
           ::file/type :snapshot
           ::file/creation-date-time #inst "2016-03-18T14:41:00.000-00:00"}))
      :FileType := "Snapshot"
      :FileOID := "FI01"
      :CreationDateTime := "2016-03-18T14:41:00.000Z"))

  (testing "Spec check"
    (is
      (->> (st/check `unparse-file)
           (every? #(get-in % [:clojure.spec.test.check/ret :result]))))))
