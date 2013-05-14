(defproject al-jabr "0.1.0-SNAPSHOT"
  :description "A Clojure wrapper of twitter/algebird"
  :url "https://github.com/sorenmacbeth/al-jabr"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.0.0"
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.twitter/algebird-core_2.9.2 "0.1.12"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                   :plugins [[lein-midje "3.0.1"]]}})
