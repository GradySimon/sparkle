(ns sparkle.layer
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [sparkle.util :refer [constrain]]))

(defn static-color [color]
  (fn [env leds]
    (map (constantly color) leds)))

(defn scale-brightness [factor]
  (fn [env leds]
    (map (fn [led]
           (fmap #(* factor %) led))
         leds)))

(defn brightness-gradient [start-factor end-factor]
  (fn [env leds]
    (let [step (/ (- end-factor start-factor)
                  (dec (count leds)))
          factors (iterate #(+ % step) start-factor)]
      (map (fn [led factor]
             (fmap #(* factor %) led))
           leds factors))))

(defn pulse-brightness [{:keys [time] :as env} leds]
  (let [scaled-time (* 2 Math/PI (/ time 1000))
        brightness-factor (+ 1/2
                             (/ (Math/sin scaled-time) 2))]
    (map (fn [led]
           (fmap #(* % brightness-factor) led))
         leds)))

(defn swimming-static-color
  ([color length interval]
   (swimming-static-color color length interval 0))

  ([color length interval initial-offset]
   (fn [{:keys [time] :as env} leds]
     (let [leds (vec leds)
           size (count leds)
           last (dec size)
           offset (+ initial-offset
                     (int (Math/floor (* (inc size)
                                         (/ (mod time interval) interval)))))
           start (constrain 0 last (- offset length))]
       (vec (concat (subvec leds 0 start)
                    (take (- offset start) (repeat color))
                    (subvec leds (min last offset))))))))

(defn stepping-static-color [color interval]
  (fn
    ([{:keys [time]} leds]
     {:leds (assoc leds 0 color)
      :state {:offset 0 :last-time time}})
    ([{:keys [time]} {:keys [offset last-time] :as state} leds]
     (if (>= (- time last-time) interval)
       (let [new-offset (mod (inc offset) (count leds))]
         {:leds (assoc leds new-offset color)
          :state {:offset new-offset :last-time time}})
       {:leds (assoc leds offset color)
        :state  state}))))

;; Layers are either functions or {:layer-fn :state} maps. Layers
;; which wish to use state must have an arity-2 version that can be
;; called without state that will return an {:leds :state} map
;; containing the state to be passed to their arity-3 version on the
;; next frame.

(defn iterate-layer
  "Calls the layer with provided env on leds. Returns a vector
  of [leds next-layer] where next-layer is the layer that should be
  passed to iterate-layer on the next frame."
 [layer env leds]
 (if (map? layer)
   (let [{:keys [layer-fn state]} layer
         {new-leds :leds next-state :state} (layer-fn env state leds)]
     [new-leds (assoc layer :state next-state)])
   (let [layer-result (layer env leds)]
     (if (map? layer-result)
       (let [{new-leds :leds next-state :state} layer-result]
         [new-leds {:layer-fn layer :state next-state}])
       [layer-result layer]))))

(defn apply-layers
  "Applies each layer in layers to the leds with env. Returns the
  resulting leds and the layer vector that should be used next frame."
  [layers env leds]
  (reduce (fn [[current-leds prev-layers] layer]
            (let [[next-leds next-layer] (iterate-layer layer env current-leds)]
              [next-leds (conj prev-layers next-layer)]))
          [leds []] layers))
