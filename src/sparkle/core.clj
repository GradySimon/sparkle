(ns sparkle.core
  (:require [clojure.core.async :refer [thread chan <!! >!! alt!! close!]]
            [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component]
            [sparkle.fadecandy-opc :as fc])
  (:gen-class))

(defn now []
  (System/currentTimeMillis))

(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})

(def leds (take 5 (repeat black)))

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
                  (- (count leds) 1))
          factors (iterate #(+ % step) start-factor)]
      (map (fn [led factor]
             (fmap #(* factor %) led))
           leds factors))))

(defn pulse-brightness [{:keys [time] :as env} leds]
  (let [scaled-time (/ time 10000)
        brightness-factor (Math/sin scaled-time)]
    (map (fn [led]
           (fmap #(* % brightness-factor) led))
         leds)))

(defn apply-layers [layers env leds]
  (let [env-applied-layers (for [layer layers] (partial layer env))]
    ((apply comp env-applied-layers) leds)))


(defrecord RenderState [env model])


(defprotocol Displayer
  "A protocol for things that are capable of displaying frames"
  (display [displayer frame] "Display a frame"))


;; The minimum time, in millis, between frames printed by ConsoleDisplayer
(def console-display-interval 500)

(defrecord ConsoleDisplayer [previous]
  component/Lifecycle
  (start [displayer]
    (assoc displayer
           :previous (atom {:frame nil
                            :time (- (now) console-display-interval)})))

  (stop [displayer]
    (dissoc displayer :previous))

  Displayer
  (display [{:keys [previous]} frame]
    (let [{prev-frame :frame prev-time :time} @previous]
      (when (and (>= (now) (+ prev-time console-display-interval))
                 (not= frame prev-frame))
        (reset! previous {:frame frame :time (now)})
        (println frame)))))

(defn scale-pixel [pixel]
  (let [scale-fn (fn [value]
                   (int (min 256 (max 0 (* 256 value)))))]
    (fmap scale-fn pixel)))

(defrecord FadecandyDisplayer []
  component/Lifecycle
  (start [displayer]
    displayer)

  (stop [displayer]
    displayer)

  Displayer
  (display [displayer frame]
    (fc/push-pixels {0 (map scale-pixel frame)})))


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
   :displayer (map->FadecandyDisplayer {})))

;;; Below: defs useful for interacting with the running renderer

(def started-system (component/start (sparkle-system)))

(def renderer (:renderer started-system))

(def displayer (:displayer started-system))
