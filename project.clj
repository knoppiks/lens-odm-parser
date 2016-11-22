(defproject org.clojars.akiel/lens-odm-parser "0.3-SNAPSHOT"
  :description "Parses ODM XML files into a Clojure data structure."
  :url "https://github.com/alexanderkiel/lens-odm-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[clj-time "0.11.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/data.xml "0.0.8"]
                 [prismatic/plumbing "0.5.2"]
                 [prismatic/schema "1.0.4"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/test.check "0.9.0"]
                             [criterium "0.4.4"]]
              :global-vars {*print-length* 20}}})
