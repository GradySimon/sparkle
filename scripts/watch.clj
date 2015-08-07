(require '[cljs.build.api :as b])

(b/watch "src"
  {:main '..core
   :output-to "out//.js"
   :output-dir "out"})
