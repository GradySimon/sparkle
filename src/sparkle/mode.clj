(ns sparkle.mode
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.zip :as zip]))

; modes return either a seq or a map of these:
; {:mode mode-name
;  :params params-map}
; for each child. seq if their :model/children attribute is specified as a seq
; map if they use a map 
; eval-mode will also supply an env-map to each mode call, 
; which it gets from core/update

(defmulti plasma
  (fn [env model params]
    (:model/type model)))

; (defmethod plasma :model.type/strip
;   [{:keys [time]} model {:keys [period]}]
;   (let [count (:model/count model)]
;     (for [x (range count)]
;       {:r (* 0.609 (Math/sin (+ (* x 2) (/ time 1000))))
;        :g (* 0.164 (Math/sin (+ (* x 2) (/ time 1000))))
;        :b (* 0 (Math/sin (+ (* x 2) (/ time 1000))))})))

; (defmethod math-christmas :model.type/strip
;   [{:keys [time]} model {:keys [period]}]
;   (let [count (:model/count model)
;         scaled-time (/ time 10000)
;         v (fn [x]
;              (+ (Math/sin (+ (* 2 x) scaled-time))
;                 (Math/sin (+ (* 10 (+ (* x (Math/sin (/ scaled-time 2)))
;                                       (* 2 (Math/cos (/ scaled-time 3)))))
;                              scaled-time))))]
;     (for [x (range count)]
;       {:r (* 0.60 (Math/sin (* (v (* x 0.1)) Math/PI)))
;        :g (* 0.34 (Math/cos (* (v (* x 0.1)) Math/PI)))
;        :b 0})))

(def math-christmas
  {:rgb-multipliers
    {:r 0.6 :g 0.34 :b 0}})

(defmethod plasma :model.type/strip
  "See http://www.bidouille.org/prog/plasma"
  [{:keys [time]} model {:keys [time-scale-factor rgb-multipliers y-val]}]
  (let [count (:model/count model)
        scaled-time (/ time time-scale-factor)
        v (fn [x y]
             (+ (Math/sin (+ (* 2 x) scaled-time))
                (Math/sin (+ (* 10 (+ (* x (Math/sin (/ scaled-time 2)))
                                      (* y (Math/cos (/ scaled-time 3)))))
                             scaled-time))
                (let [cx (+ x (* 0.5 (Math/sin (/ scaled-time 5))))
                      cy (+ y (* 0.5 (Math/cos (/ scaled-time 3))))]
                      )))]
    (for [x (range count)
          :let [v-val (v (* x 0.1) y-val)]]
        {:r (* (:r rgb-multipliers) (Math/sin (* v-val Math/PI)))
         :g (* (:g rgb-multipliers) (Math/sin (+ (* v-val Math/PI) (* (/ 2 3) Math/PI))))
         :b (* (:b rgb-multipliers) (Math/sin (+ (* v-val Math/PI) (* (/ 4 3) Math/PI))))})))

(defmethod plasma :model.type/cylinder
  [env model params]
  (for [strip-num (range (count (:model/children model)))]
    {:mode plasma 
     :params (assoc-in params [:y-val] strip-num)}))

(defn px-blink [time period on-color off-color]
  (if (= 0 (mod (quot time period) 2))
    on-color
    off-color))

(defn strip-blink [{:keys [time] :as env} model {:keys [period on-color off-color] :as params}]
  (repeat (model :model/count)
    (px-blink time period on-color off-color)))

(defn cylinder-blink [{:keys [time] :as env} model params]
  (for [child-ring (:model/children model)]
    {:mode strip-blink
     :params params}))

; This proves that I need a higher-order solution...
(defn test-installation-blink [env model params]
  {:test-installation/sleeve
    {:mode cylinder-blink
     :params params}
   :test-installation/forehead-ring
    {:mode strip-blink
     :params params}})

(defn mode-frame? [value]
  (and (contains? value :mode)
       (contains? value :params)))

(defn mode-tree-node? [value]
  (and (contains? value :model-node)
       (contains? value :mode-frame)
       (and (not (contains? value :pixels)))))

(defn mode-tree-node-children [mode-tree-node]
  (cond
    (contains? mode-tree-node :pixels) (seq (:pixels mode-tree-node))
    (contains? mode-tree-node :children)
      (cond
        (map? (:children mode-tree-node)) (vals (:children mode-tree-node))
        (sequential? (:children mode-tree-node)) (seq (:children mode-tree-node)))))

(defn make-mode-tree-node [node children]
  (assoc node :children children))

(defn mode-tree-zip [mode-tree]
  "Returns a zipper for playing with mode-tree."
  (zip/zipper
    mode-tree-node?
    mode-tree-node-children
    make-mode-tree-node
    mode-tree))

; a pixel address map is a map of the form
; {:channel-num1 {order-key1 [pixel1 pixel2 ..]
;                 order-key2 [pixel3 pixel4 ..] ..}
;  :channel-num2 {order-key1 [..] ..}
;  ..}
;
; This needs to get flattened to
; {:channel-num1 [pixel1 pixel2 pixel3 pixel4]
;  :channel-num2 [..]
;  ..}

(defn scale-pixel [pixel]
  (let [scale-fn (fn [value]
                    (int (min 256 (max 0 (* 256 value)))))]
    (fmap scale-fn pixel)))

(defn flatten-pixel-map [pixel-address-map]
  (fmap
    (fn [channel-offset-map]
      (let [sorted-pixel-segments (vals (into (sorted-map) channel-offset-map))]
        (vec (reduce concat [] sorted-pixel-segments))))
    pixel-address-map))

(defn merge-pixel-node [pixel-address-map node]
  (let [{:keys [channel order-key]} (get-in node [:model-node :model.leaf/address])
        pixels (vec (map scale-pixel (:pixels node)))]
    (assoc-in pixel-address-map [channel order-key] pixels)))

(defn pixel-map [mode-tree]
  "Traverses the mode-tree, builds a vector of pixels, suitable for pushing
   to the device."
  (let [zipped-tree (mode-tree-zip mode-tree)
        leaf-nodes (map zip/node (filter (complement zip/branch?) ;filter only non-branch nodes
                                   (take-while (complement zip/end?) ;take until the :end
                                               (iterate zip/next zipped-tree))))]
    (flatten-pixel-map
      (reduce merge-pixel-node {} leaf-nodes))))
