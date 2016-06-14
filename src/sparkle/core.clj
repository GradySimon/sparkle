(ns sparkle.core
  (:require [clojure.spec :as s]
            [clojure.core.async :refer [thread chan <!! >!! alt!! close!]]
            [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component]
            [com.evocomputing.colors :as c :refer [create-color] :rename {create-color color}]
            [sparkle.spec]
            [sparkle.util :refer [now constrain]]
            [sparkle.display :as d :refer [display]]
            [sparkle.layer :as l :refer [apply-layers]])
  (:gen-class))

(def black (color [0 0 0]))

(defrecord RenderState [env model])

(defn get-env-updates [env]
  (-> env
      (assoc :time (now))))

(s/fdef inflate
  :args (s/cat :shape :model/shape)
  :ret :core/frame)

(defmulti inflate
  "Returns a frame of black (completely off) pixels in the specified `shape`"
  (fn [shape] (:type shape)))

(defmethod inflate :strip [{:keys [pixel-count]}]
  (vec (take pixel-count (repeat black))))

(s/fdef render-step
  :args (s/cat :state :render/state)
  :ret (s/keys :req-un [:core/frame :render/state]))

(defn render-step
  "Renders the provided RenderState into a frame."
  [{:keys [env model] :as state}]
  (let [{:keys [shape layers]} model
        updated-env (get-env-updates env)
        [frame next-layers] (apply-layers layers updated-env (inflate shape))]
    {:frame frame
     :state {:env env
             :model (assoc model :layers next-layers)}}))

(defn send-command
  "Shortcut function for sending `command` into the `command-chan` of a `renderer`"
  [{:keys [command-chan] :as renderer} command]
  (>!! command-chan command))

(s/fdef execute
  :args (s/cat :command :render/command
               :state :render/state
               :status :remder/status)
  :ret (s/cat :new-state :render/state 
              :new-status :render/status))

(defmulti execute
  "Executes the given `command` given `state` and `status`, returning a vector containing the
  updated state and status"
  (fn [command state status] (:type command)))

(defmethod execute :start [_ state _]
  [state :running])

(defmethod execute :stop [_ state _]
  [state :stopped])

(defmethod execute :update [{:keys [new-state]} _ status]
  [new-state status])

(defmethod execute :kill [_ _ _]
  nil)

(defn start-rendering
  "Starts a rendering loop that will listen for commands on `command-chan` and display frames using
  `displayer`"
  [{:keys [command-chan displayer] :as renderer}]
  (thread
    (loop [state (->RenderState {} nil)
           status :stopped]
      (let [[state status :as loop-result]
            (if (= status :running)
              (alt!! command-chan ([command] (execute command state status)) 
                     :default (let [{:keys [frame state]} (render-step state)]
                                (display displayer frame)
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
    (>!! command-chan {:type :kill})
    renderer))


(defn sparkle-system []
  (component/system-map
   :renderer (component/using (map->Renderer {})
                              [:displayer])
   ;:displayer (d/map->ConsoleDisplayer {})
   :displayer (d/new-fadecandy-displayer "localhost" 7890)))
