(defproject sparkle "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.clojure/algo.generic "0.1.2"]
                 [org.clojure/tools.logging "0.3.1"]
                 [fipp "0.6.2"]
                 [gloss "0.2.5"]]
  :main ^:skip-aot sparkle.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
