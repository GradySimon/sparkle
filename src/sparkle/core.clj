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

(defrecord RenderState [env model])

(defn get-env-updates [env]
  (-> env
      (assoc :time (now))))

(defmulti inflate (fn [shape] (:type shape)))

(defmethod inflate :strip [{:keys [pixel-count]}]
  (vec (take pixel-count (repeat black))))

(defn render-step
  "Renders the provided RenderState into a frame."
  [{:keys [env model] :as state}]
  (let [{:keys [shape layers]} model
        updated-env (get-env-updates env)
        [frame next-layers] (apply-layers layers updated-env (inflate shape))]
    {:frame frame
     :new-state {:env env
                 :model (assoc model :layers next-layers)}}))

(defn send-command [{:keys [command-chan] :as renderer} command]
  (>!! command-chan command))

(defmulti execute (fn [command layers status] (:type command)))

(defmethod execute :start [_ state _]
  [state :running])

(defmethod execute :stop [_ state _]
  [state :stopped])

(defmethod execute :update [{:keys [new-state]} _ status]
  [new-state status])

(defmethod execute :kill [_ _ _]
  nil)

(defn start-rendering [{:keys [command-chan displayer] :as renderer}]
  (thread
    (loop [state (->RenderState {} nil)
           status :stopped]
      (let [[state status :as loop-result]
            (if (= status :running)
              (alt!! command-chan ([command] (execute command state status)) 
                     :default (let [{:keys [frame new-state]} (render-step state)]
                                (display displayer frame)
                                [new-state :running]))
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
    (>!! command-chan {:type :kill})
    renderer))


(defn sparkle-system []
  (component/system-map
   :renderer (component/using (map->Renderer {})
                              [:displayer])
   ;:displayer (d/map->ConsoleDisplayer {})))
   :displayer (d/new-fadecandy-displayer "localhost" 7890)))

