(defproject org.clojars.akiel/lens-odm-parser "0.5-SNAPSHOT"
  :description "Parses ODM XML files into a Clojure data structure."
  :url "https://github.com/alexanderkiel/lens-odm-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.0.0"
  :pedantic? :abort

  :dependencies [[clj-time "0.12.2"]
                 [org.clojars.akiel/odm-spec "0.3-alpha14"]
                 [org.clojure/clojure "1.9.0-alpha14"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[criterium "0.4.4"]
                             [juxt/iota "0.2.2"]
                             [org.clojure/data.xml "0.0.8"]
                             [org.clojure/test.check "0.9.0"]]
              :global-vars {*print-length* 20}}})
