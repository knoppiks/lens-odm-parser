(defproject org.clojars.akiel/lens-odm-parser "0.7-SNAPSHOT"
  :description "Parses ODM XML files into a Clojure data structure."
  :url "https://github.com/alexanderkiel/lens-odm-parser"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.0.0"
  :pedantic? :abort

  :dependencies [[camel-snake-kebab "0.4.0"]
                 [clj-time "0.12.2"]
                 [org.clojars.akiel/odm-spec "0.5-alpha19"]
                 [org.clojure/clojure "1.9.0-RC2"]]

  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[com.gfredericks/test.chuck "0.2.7"]
                             [criterium "0.4.4"]
                             [org.clojars.akiel/iota "0.1"]
                             [org.clojure/data.xml "0.0.8"]
                             [org.clojure/test.check "0.9.0"]]
              :global-vars {*print-length* 20}}})
