(ns sparkle.core
  (:require [clojure.core.async :refer [thread chan <!! >!! alt!! close!]]
   [clojure.algo.generic.functor :refer [fmap]]
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
      (map (fn [led factor]
             (fmap #(* factor %) led))
           leds factors))))

(defn apply-layers [layers leds]
  ((apply comp layers) leds))


(defn display [{:keys [frame-chan] :as displayer} frame]
  (>!! frame-chan frame))

(defn start-displaying [{:keys [frame-chan] :as displayer}]
  (thread
    (loop [prev-frame nil]
      (let [frame (<!! frame-chan)]
        (if frame
          (do (when (not= frame prev-frame)
                (println frame))
              (recur frame))
          (println "Shutting down displayer"))))))

(defrecord Displayer [frame-chan]
  component/Lifecycle
  
  (start [displayer]
    (let [frame-chan (chan)
          displayer (assoc displayer :frame-chan frame-chan)]
      (start-displaying displayer)
      displayer))

  (stop [displayer]
    (close! frame-chan)
    displayer))

(defn render [layers displayer]
  (display displayer (apply-layers layers leds)))

(defn send-command [{:keys [command-chan] :as renderer} command]
  (>!! command-chan command))

(defmulti execute (fn [command layers status] (:type command)))

(defmethod execute :start [_ layers status]
  [layers :running])

(defmethod execute :pause [_ layers status]
  [layers :paused])

(defmethod execute :update [{:keys [new-layers]} layers status]
  [new-layers status])

(defmethod execute :stop [_ _ _]
  nil)

(defn start-rendering [{:keys [command-chan displayer] :as renderer}]
  (thread
    (loop [layers []
           status :running]
      (let [[layers status :as loop-result]
            (if (= status :running)
              (alt!! command-chan ([command] (execute command layers status)) 
                    :default (do
                               (render layers displayer)
                               [layers :running]))
              (execute (<!! command-chan) layers status))]
        (if loop-result
          (recur layers status)
          (println "Shutting down render loop"))))))

(defrecord Renderer [command-chan displayer]
  component/Lifecycle

  (start [renderer]
    (let [command-chan (chan)
          renderer (assoc renderer :command-chan command-chan)]
      (start-rendering renderer)
      renderer))

  (stop [renderer]
    (>!! command-chan {:type :stop})
    renderer))


(defn sparkle-system []
  (component/system-map
   :renderer (component/using (map->Renderer {})
                              [:displayer])
   :displayer (map->Displayer {})))

;;; Below: defs useful for interacting with the running renderer

(def started-system (component/start (sparkle-system)))

(def renderer (:renderer started-system))

(def displayer (:displayer started-system))

