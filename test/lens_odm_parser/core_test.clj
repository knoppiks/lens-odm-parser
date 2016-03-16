(ns lens-odm-parser.core-test
  (:require [clojure.data.xml :as xml]
            [clojure.data.zip.xml :refer [xml1->]]
            [clojure.test :refer :all]
            [clojure.zip :as zip]
            [lens-odm-parser.core :refer :all]))

(defn- zipper [sexp]
  (zip/xml-zip (xml/sexp-as-element sexp)))

(deftest tx-type-test
  (testing "valid transaction types"
    (are [transaction-type tx] (= tx (tx-type (zipper [:Foo {:TransactionType transaction-type}])))
      "Insert" :insert
      "Update" :update
      "Remove" :remove
      "Context" :context))
  (testing "takes trasnaction type from parent"
    (is (= :insert (tx-type (xml1-> (zipper [:Foo {:TransactionType "Insert"} [:Bar]]) :Bar)))))
  (testing "invalid transaction type"
    (is (thrown? Exception (tx-type (zipper [:Foo {:TransactionType "Foo"}])))))
  (testing "missing transaction type"
    (is (thrown? Exception (tx-type (zipper [:Foo]))))))

(deftest parse-subject-test
  (are [sexp subject] (= subject (parse-subject (zipper sexp)))
    [:SubjectData {:SubjectKey "1" :TransactionType "Insert"}]
    {:subject-key "1" :tx-type :insert :study-events []}))

(deftest parse-item-group-test
  (are [sexp item-group] (= item-group (parse-item-group (zipper sexp)))
    [:ItemGroupData {:ItemGroupOID "1" :TransactionType "Insert"}]
    {:item-group-oid "1" :tx-type :insert :items []}

    [:ItemGroupData {:ItemGroupOID "1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "2"} "3"]]
    {:item-group-oid "1" :tx-type :insert :items
     [{:item-oid "2" :tx-type :insert :data-type :string :value "3"}]}

    [:ItemGroupData {:ItemGroupOID "1" :TransactionType "Insert"}
     [:ItemDataInteger {:ItemOID "2"} "3"]]
    {:item-group-oid "1" :tx-type :insert :items
     [{:item-oid "2" :tx-type :insert :data-type :integer :value 3}]}

    [:ItemGroupData {:ItemGroupOID "1" :TransactionType "Insert"}
     [:ItemDataFloat {:ItemOID "2"} "3.1"]]
    {:item-group-oid "1" :tx-type :insert :items
     [{:item-oid "2" :tx-type :insert :data-type :float :value 3.1}]}

    [:ItemGroupData {:ItemGroupOID "1" :TransactionType "Insert"}
     [:ItemDataString {:ItemOID "2"} "3"]
     [:ItemDataInteger {:ItemOID "4"} "5"]]
    {:item-group-oid "1" :tx-type :insert :items
     [{:item-oid "2" :tx-type :insert :data-type :string :value "3"}
      {:item-oid "4" :tx-type :insert :data-type :integer :value 5}]}))

(deftest parse-string-item-test
  (are [sexp item] (= item (parse-string-item (zipper sexp)))
    [:ItemDataString {:ItemOID "1" :TransactionType "Insert"} "2"]
    {:item-oid "1" :tx-type :insert :data-type :string :value "2"}))

(deftest parse-integer-item-test
  (are [sexp item] (= item (parse-integer-item (zipper sexp)))
    [:ItemDataInteger {:ItemOID "1" :TransactionType "Insert"} "2"]
    {:item-oid "1" :tx-type :insert :data-type :integer :value 2})
  (let [sexp [:ItemDataInteger {:ItemOID "1" :TransactionType "Insert"} "a"]]
    (is (thrown? Exception (parse-integer-item (zipper sexp))))))

(deftest parse-float-item-test
  (are [sexp item] (= item (parse-float-item (zipper sexp)))
    [:ItemDataFloat {:ItemOID "1" :TransactionType "Insert"} "2.1"]
    {:item-oid "1" :tx-type :insert :data-type :float :value 2.1})
  (let [sexp [:ItemDataFloat {:ItemOID "1" :TransactionType "Insert"} "2"]]
    (is (float? (:value (parse-float-item (zipper sexp))))))
  (let [sexp [:ItemDataFloat {:ItemOID "1" :TransactionType "Insert"} "a"]]
    (is (thrown? Exception (parse-float-item (zipper sexp))))))
