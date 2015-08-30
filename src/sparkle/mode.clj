(ns sparkle.mode
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [clojure.zip :as zip]
            [fipp.edn :as fipp]))

; modes return either a seq or a map of these:
; {:mode mode-name
;  :params params-map}
; for each child. seq if their :model/children attribute is specified as a seq
; map if they use a map 
; eval-mode will also supply an env-map to each mode call, 
; which it gets from core/update

(defmulti dimensions
  (fn [model]
    (:model/type model)))

(defmethod dimensions :model.type/cylinder [model]
  [(count (:model/children model))
   (apply max (map :model/count (:model/children model)))])

(defmulti slave
  (fn [env model params state]
    (:model/type model)))

(defmethod slave :model.type/strip
  [env model {:keys [pixels]} state]
  (let [pixel-count (:model/count model)]
    {:pixels (take pixel-count pixels)}))

(defmulti plasma
  "See http://www.bidouille.org/prog/plasma"
  (fn [env model params]
    (:model/type model)))

(def math-christmas
  {:time-scale-factor 10000
   :rgb-multipliers
     {:r 0.74 :g 0.33 :b 0}})

(def full-spectrum
  {:time-scale-factor 10000
   :rgb-multipliers
     {:r 0.33 :g 0.33 :b 0.33}})

(defmethod plasma :model.type/strip
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
                      (Math/sin (+ 1 scaled-time (* 100 (+ (* cx cx) (* cy cy))))))))]
    (for [x (range count)
          :let [v-val (v (* x 0.025) (* y-val 0.025 ))]]
        {:r (* (:r rgb-multipliers) (Math/sin (* v-val Math/PI)))
         :g (* (:g rgb-multipliers) (Math/sin (+ (* v-val Math/PI) (* (/ 2 3) Math/PI))))
         :b (* (:b rgb-multipliers) (Math/sin (+ (* v-val Math/PI) (* (/ 4 3) Math/PI))))})))

(defmethod plasma :model.type/cylinder
  [env model params]
  (for [strip-num (range (count (:model/children model)))]
    {:mode plasma 
     :params (assoc-in params [:y-val] strip-num)}))

(defn get-point [grid x-coord y-coord]
  (nth (nth grid x-coord) y-coord))

(defn grid-dimensions [grid]
  [(count grid)
   (count (first grid))])

(defn print-grid [grid]
  (println "----------------------------------")
  (doseq [row grid]
    (println (map #(if % "#" " ") row))))

(defn relative [grid x-coord y-coord [x-offset y-offset]]
  (let [[rows columns] (grid-dimensions grid)
        target-x (mod (+ x-coord y-offset) rows)
        target-y (mod (+ y-coord y-offset) columns)]
      (get-point grid target-x target-y)))

(defn neighbor-count [grid x-coord y-coord]
  (let [neighbors
          (map #(relative grid x-coord y-coord %)
            (filter #(not= [0 0] %)
              (for [x-offset [-1 0 1]
                    y-offset [-1 0 1]]
                [x-offset y-offset])))]
  (count (filter identity neighbors))))

(defn update-conway-pixel [old-grid x-coord y-coord]
  (let [neighbor-count (neighbor-count old-grid x-coord y-coord)
        alive (get-point old-grid x-coord y-coord)]
    (if alive
      (cond
        (<= neighbor-count 1) false
        (<= neighbor-count 3) true
        (> neighbor-count 3) false)
      (if (= neighbor-count 3)
        true
        false))))

(defn new-conway-grid [rows columns]
  (vec (for [row (range rows)]
          (vec (for [column (range columns)]
                  (> 0.05 (rand)))))))

(defn update-conway-grid [old-grid]
  (let [[rows columns] (grid-dimensions old-grid)
        new-grid 
          (vec (for [row (range rows)]
                  (vec (map #(update-conway-pixel old-grid row %) (range columns)))))]
    (if (= new-grid old-grid)
      (new-conway-grid rows columns)
      new-grid)))

(defn conways [{:keys [time]} model {:keys [period on-color off-color]} {:keys [step-start-time grid]}]
  (let [non-null-start-time (if-not step-start-time
                              time
                              step-start-time)
        time-since-last (- time non-null-start-time)
        should-update (< period time-since-last)
        current-step-start-time (if should-update
                                  (+ non-null-start-time period)
                                  non-null-start-time)
        current-grid (if-not grid
                        (let [[row-count column-count] (dimensions model)]
                          (println "State grid is null. Making new grid")
                          (new-conway-grid row-count column-count))
                        (if should-update
                                (update-conway-grid grid)
                                grid))
        child-mode-frames (for [row current-grid]
                            {:mode slave
                             :params {:pixels (map #(if % on-color off-color) row)}})]
      ; (when should-update
      ;   (print-grid current-grid))
      {:child-mode-frames child-mode-frames
       :state
        {:step-start-time current-step-start-time
         :grid current-grid}}))

; Note: a conway grid is a rectangular vector of vectors.
;       each element in the top-level vector is a row, and corresponds to x
;       the inner vectors hold column values for their row and correspond to y


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
  (assoc node :children children))-0

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
