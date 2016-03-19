(ns user
  (:use criterium.core)
  (:use plumbing.core)
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint pp]]
            [clojure.repl :refer :all]
            [clojure.zip :as zip]
            [lens-odm-parser.core :as parser]
            [schema.core :as s]))

;(s/set-fn-validation! true)

(defn parse-odm-file [filename]
  (->> (io/input-stream filename)
       (xml/parse)
       (zip/xml-zip)
       (parser/parse-clinical-data)))

(comment
  ;; 55 ms
  (quick-bench (parse-odm-file "/Users/akiel/z/export-1/S002_T00720-ODM.xml"))
  )
