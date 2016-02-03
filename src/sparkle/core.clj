(ns sparkle.core
  (:require [clojure.core.async :refer [thread chan <!! >!! alt!! close!]]
            [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component]
            [sparkle.util :refer [now constrain]]
            [sparkle.display :as d :refer [display]])
  (:gen-class))


(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})

(def leds (take 36 (repeat black)))


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


(defrecord RenderState [env model])


(defn get-env-updates [env]
  (-> env
      (assoc :time (now))))

(defn render-step [{:keys [env model] :as state}]
  (let [{:keys [layers]} model
        updated-env (get-env-updates env)]
    (->> leds
         (apply-layers layers updated-env))))


(defn send-command [{:keys [command-chan] :as renderer} command]
  (>!! command-chan command))

(defmulti execute (fn [command layers status] (:type command)))

(defmethod execute :start [_ state status]
  [state :running])

(defmethod execute :pause [_ state status]
  [state :paused])

(defmethod execute :update [{:keys [new-state]} state status]
  [new-state status])

(defmethod execute :stop [_ _ _]
  nil)

(defn start-rendering [{:keys [command-chan displayer] :as renderer}]
  (thread
    (loop [state (-> RenderState {} {})
           status :running]
      (let [[state status :as loop-result]
            (if (= status :running)
              (alt!! command-chan ([command] (execute command state status)) 
                     :default (let [next-frame (render-step state)]
                                (display displayer next-frame)
                                [state :running]))
              (execute (<!! command-chan) state status))]
        (if loop-result
          (recur state status)
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
   :displayer (d/new-fadecandy-displayer "localhost" 7890)))

;;; Below: defs useful for interacting with the running renderer

(def started-system (component/start (sparkle-system)))

(def renderer (:renderer started-system))

(def displayer (:displayer started-system))

