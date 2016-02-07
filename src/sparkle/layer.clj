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

(defn apply-layers [layers env leds]
  (let [env-applied-layers (for [layer layers] (partial layer env))]
    ((apply comp env-applied-layers) leds)))
