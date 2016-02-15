(ns sparkle.core
  (:require [clojure.core.async :refer [thread chan <!! >!! alt!! close!]]
            [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component]
            [sparkle.util :refer [now constrain]]
            [sparkle.display :as d :refer [display]]
            [sparkle.layer :as l :refer [apply-layers]])
  (:gen-class))

(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})

(def leds (take 36 (repeat black)))

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

(defmethod execute :start [_ state _]
  [state :running])

(defmethod execute :pause [_ state _]
  [state :paused])

(defmethod execute :update [{:keys [new-state]} _ status]
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

