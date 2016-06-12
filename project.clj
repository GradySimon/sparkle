(defproject sparkle "0.2.0-SNAPSHOT"
  :description "An LED animation engine"
  :url "https://github.com/GradySimon/sparkle"
  :license {:name "MIT License"}
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
