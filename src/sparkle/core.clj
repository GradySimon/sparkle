(ns sparkle.core
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component])
  (:gen-class))

; Let's start by implementing rendering a vec of layers on one static set of LEDs

(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})

(def leds (take 5 (repeat black)))

(defn static-color [color]
  (fn [leds]
    (map (constantly color) leds)))

(defn scale-brightness [factor]
  (fn [leds]
    (map (fn [led]
           (fmap #(* factor %) led))
         leds)))

(defn brightness-gradient [start-factor end-factor]
  (fn [leds]
    (let [step (/ (- end-factor start-factor)
                  (- (count leds) 1))
          factors (iterate #(+ % step) start-factor)]
      (println (take 5 factors))
      (map (fn [led factor]
             (fmap #(* factor %) led))
           leds factors))))

(defn apply-layers [layers leds]
  ((apply comp layers) leds))

(apply-layers [(brightness-gradient 0.8 0)
               (scale-brightness 1)
               (static-color white)]
              leds)

(defmulti execute (fn [command layers status] (:type command)))

(defmethod execute :start [_ layers status]
  [layers :running])

(defmethod execute :pause [_ layers status]
  [layers :paused])

(defmethod execute :update [{:keys [new-layers]} layers status]
  [new-layers status])

(defmethod execute :stop [_ _ _]
  nil)

(defn start-rendering [command-chan frame-chan]
  (go-loop [layers []
            status :running]
    (let [[layers status :as loop-result]
          (if (= status :running)
            (alt! command-chan ([command] (execute command layers status)) 
                  :default (do
                             (render layers leds)
                             [layers :running]))
            (execute (<! command-chan) layers status))]
      (when loop-result
        (recur loop-result)))))

; make the if :running render logic the default of alt?

(defrecord Renderer [command-chan frame-chan]
  component/Lifecycle

  (start [renderer]
    (start-rendering command-chan frame-chan)
    (renderer))

  (stop [renderer]
    (>! command-chan {:type :stop})))
