(ns user
  (:use criterium.core)
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint pp]]
            [clojure.repl :refer :all]
            [clojure.spec.test.alpha :as st]
            [lens-odm-parser.core :as p]))

(st/instrument)

(defn parse-odm-file [filename]
  (->> (io/input-stream filename)
       (xml/parse)
       (p/parse-odm-file)))
