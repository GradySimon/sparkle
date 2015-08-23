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


(defmulti blink 
  (fn [env model params]
    (:model/type model))

(defmethod blink :model.type/strip [{:keys [time] :as env} model {:keys [period on-color off-color] :as params}]
  (repeat (model :model/count)
    (px-blink time period on-color off-color))))

(defmethod blink :model.type/cylinder [{:keys [time] :as env} model params]
  (for [child-ring (:model/children model)]
    {:mode strip-blink
     :params params}))

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

(defrecord ModeFrame [mode params])

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
    (contains? mode-tree-node :children) (vals (:children mode-tree-node))))

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

(defn flatten-pixel-map [pixel-address-map]
  (fmap
    (fn [channel-offset-map]
      (let [sorted-pixel-segments (vals (into (sorted-map) channel-offset-map))]
        (vec (reduce concat [] sorted-pixel-segments))))
    pixel-address-map))

(defn pixel-map [mode-tree]
  "Traverses the mode-tree, builds a vector of pixels, suitable for pushing
   to the device."
  (let [zipped-tree (mode-tree-zip mode-tree)
        leaf-nodes (map zip/node (filter (complement zip/branch?) ;filter only non-branch nodes
                                   (take-while (complement zip/end?) ;take until the :end
                                               (iterate zip/next zipped-tree))))]
    (flatten-pixel-map
      (reduce (fn [pixel-address-map node]
                  (let [{:keys [channel order-key]} (get-in node [:model-node :model.leaf/address])
                        pixels (vec (:pixels node))]
                    (assoc-in pixel-address-map [channel order-key] pixels)))
                {}
                leaf-nodes))))
