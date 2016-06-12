(defproject sparkle "0.2.0-SNAPSHOT"
  :description ""
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha5"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/algo.generic "0.1.2"]
                 [com.stuartsierra/component "0.3.1"]
                 [com.evocomputing/colors "1.0.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]
                                  [org.clojure/java.classpath "0.2.0"]]}})

