(ns user
  (:use criterium.core)
  (:use plumbing.core)
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint pp]]
            [clojure.repl :refer :all]
            [lens-odm-parser.core :as p]
            [schema.core :as s]))

(s/set-fn-validation! true)

(defn parse-odm-file [filename]
  (->> (io/input-stream filename)
       (xml/parse)
       (p/parse-odm-file)))
