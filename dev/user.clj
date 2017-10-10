(ns user
  (:use criterium.core)
  (:require
    [clojure.data.xml :as xml]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint pp]]
    [clojure.repl :refer :all]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [lens-odm-parser.core :as p]
    [odm.file]))

(st/instrument)

(defn parse-file [filename]
  (->> (io/input-stream filename)
       (xml/parse)
       (p/parse-file)))

(comment
  (s/valid? :odm/file (parse-file ""))
  )
