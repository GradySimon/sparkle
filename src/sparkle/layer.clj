(ns sparkle.layer
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [com.evocomputing.colors :as c :refer [create-color] :rename {create-color color}]
            [sparkle.util :refer [constrain]]))

(defn static-color [color]
  (fn [env frame]
    (mapv (constantly color) frame)))

(defn scale-brightness [factor]
  (fn [env frame]
    (mapv (fn [pixel]
           (fmap #(* factor %) pixel))
          frame)))

(defn brightness-gradient [start-factor end-factor]
  (fn [env frame]
    (let [step (/ (- end-factor start-factor)
                  (dec (count frame)))
          factors (iterate #(+ % step) start-factor)]
      (mapv (fn [pixel factor]
             (fmap #(* factor %) pixel))
            frame factors))))

(defn pulse-brightness [{:keys [time] :as env} frame]
  (let [scaled-time (* 2 Math/PI (/ time 1000))
        brightness-factor (+ 1/2
                             (/ (Math/sin scaled-time) 2))]
    (mapv (fn [pixel]
           (fmap #(* % brightness-factor) pixel))
          frame)))

(defn wrapping-march
  "Like range, but starts counting up from 0 when it reaches wrap-point"
  ([start n wrap-point]
   (wrapping-march start n wrap-point 1))
  ([start n wrap-point step]
   (mapv (fn [pos]
          (if (>= pos wrap-point)
            (mod pos wrap-point)
            pos))
    (range start (+ (* step n) start) step))))

(defn stepping-static-color [color interval width initial-offset]
  (fn
    ([env frame]
     ((stepping-static-color color interval width initial-offset)
      env {:offset initial-offset :last-time 0} frame))
    ([{:keys [time]} {prev-offset :offset last-time :last-time :as state} frame]
     (let [[offset last-time] (if (>= (- time last-time) interval)
                                [(mod (inc prev-offset) (count frame)) time]
                                [prev-offset last-time])]
        {:frame (apply assoc frame (interleave (wrapping-march offset width (count frame))
                                               (repeat color)))
         :state {:offset offset :last-time last-time}}))))

(defn lowest-factor
  "Returns the lowest number by which n is evenly divisible."
  [n]
  (let [sign (/ n (Math/abs n))
        check-candidates (fn [candidate]
                            (if (zero? (rem n candidate))
                              candidate
                              (recur (+ sign candidate))))]
   (if (> n 1)
     (check-candidates (* sign 2))
     1)))

(defn triangle-wave
  "Basic triangle wave function. Follows straight lines between 0 and 1 with a
  period of 2."
  [x]
  (- 1 (* 2 (Math/abs (- (Math/round (* x 0.5)) (* x 0.5))))))

(defn uniform-spaced-hues
  "Returns n uniformly spaced hues, centered at center degrees, covering
  breadth degrees of the color wheel."
  [n center breadth]
  (let [spacing (quot breadth (inc n))
        start (- center (/ breadth 2))]
    (range (+ start spacing) (+ start breadth) spacing)))

(defn -rainbow-recurse
  "Recursive part of recursive-rainbow. Returns a frame"
  [center breadth depth frame]
  (let [pixel-count (count frame)
        divisions (lowest-factor pixel-count)
        pixels-per-division (quot pixel-count divisions)]
    (if (or (= depth 0) (= pixel-count 1))
      (take pixel-count (repeat center))
      (mapcat -rainbow-recurse
        (uniform-spaced-hues divisions center breadth)
        (take divisions (repeat (/ breadth divisions)))
        (take divisions (repeat (dec depth)))
        (partition pixels-per-division frame)))))

(defn recursive-rainbow
  "Recursively splits the frame into equal-size regions. Each region is painted
  a color, sampled from even spacings around the color wheel. With a frame of size 6, starts all as one color"
  [interval center]
  (fn [{:keys [time]} frame]
    (let [depth (int (* 4 (triangle-wave (/ time (* 0.5 interval)))))]
      (mapv #(color {:h % :s 100 :l 12}) (-rainbow-recurse center 360 depth frame)))))

(defn hue-slide
  "A layer that rotates the hue of each pixel around the color wheel,
  completing one rotation each period milliseconds"
  [period]
  (fn [{:keys [time]} frame]
    (mapv #(c/adjust-hue % (* 360 (/ (mod time period) period))) frame)))

(defn sort-by-hue
  "A layer that sorts the pixels by hue"
  [_ frame]
  (vec (sort #(- (c/hue %1) (c/hue %2)) frame)))

(defn check-frame
  "Verify that a layer didn't make an invalid change to the frame."
  [old-frame new-frame layer]
  (when (not= (count old-frame) (count new-frame))
    (throw (Exception. (str "Error after " layer
                            ". New frame is a different size (" (count new-frame)
                            ") than the previous frame (" (count old-frame) ").")))))

;; Layers are either functions or {:layer-fn :state} maps. Layers
;; which wish to use state must have an arity-2 version that can be
;; called  without state that will return an {:frame :state} map
;; containing the state to be passed to their arity-3 version on the
;; next frame.

(defn iterate-layer
  "Calls the layer with provided env on frame. Returns a vector
  of [frame next-layer] where next-layer is the layer that should be
  passed to iterate-layer on the next frame."
 [layer env frame]
 (if (map? layer)
   (let [{:keys [layer-fn state]} layer
         {new-frame :frame next-state :state} (layer-fn env state frame)]
     (check-frame frame new-frame layer)
     [new-frame (assoc layer :state next-state)])
   (let [layer-result (layer env frame)]
     (if (map? layer-result)
       (let [{new-frame :frame next-state :state} layer-result]
         (check-frame frame new-frame layer)
         [new-frame {:layer-fn layer :state next-state}])
       (do
         (check-frame frame layer-result layer)
         [layer-result layer])))))

(defn apply-layers
  "Applies each layer in layers to the frame with env. Returns the
  resulting frame and the layer vector that should be used next frame."
  [layers env frame]
  (reduce (fn [[current-frame prev-layers] layer]
            (let [[next-frame next-layer] (iterate-layer layer env current-frame)]
              [next-frame (conj prev-layers next-layer)]))
          [frame []] layers))
