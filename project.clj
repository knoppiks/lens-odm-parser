(defproject lens-odm-parser "0.1-SNAPSHOT"
  :description "Parses ODM XML files into a Clojure data structure."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [clj-time "0.11.0"]
                 [prismatic/plumbing "0.5.2"]
                 [prismatic/schema "1.0.4"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/test.check "0.9.0"]
                             [criterium "0.4.4"]]
              :global-vars {*print-length* 20}}})
