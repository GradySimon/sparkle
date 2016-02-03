(ns sparkle.util)

(defn now []
  (System/currentTimeMillis))

(defn constrain
  ([start end n]
   (min end (max start n))))
