(ns sparkle.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go go-loop chan buffer close! thread
                     alt! alts! alts!! timeout]]
            [clojure.tools.logging :as log]
            [fipp.edn :refer [pprint]]
            [sparkle.mode :as mode]
            [sparkle.color :as color]
            [sparkle.fadecandy-opc :as fc])
  (:gen-class))

(def model-file-path "models.edn")

(def selected-model "test-strip")

(defrecord State [env model mode-frame])

(defn select-model [name model]
  (first (filter #(= name (% :model/name)) model)))

; eval-state returns a tree-structured map, that looks like this:
; {:model-node {...}
;  :mode-frame {:mode mode :params params}
;  :children [{:child-name {child}} | [{child}]]}

(defn eval-state 
  [{env :env model :model {:keys [mode params] :as mode-frame} :mode-frame}]
  (if (not (contains? model :model/children))
    {:model-node model
     :mode-frame mode-frame
     :pixels (mode env model params)}
    
    (let [child-mode-frames (mode env model params)
          child-models (:model/children model)]
      {:model-node (dissoc model :model/children)
       :mode-frame mode-frame
       :children (cond
                   (sequential? child-models)
                     (map #(eval-state (->State env %1 %2)) child-models child-mode-frames)
                   (map? child-models)
                     (into {}
                       (map (fn [child-name]
                              {child-name (eval-state
                                            (->State
                                              env
                                              (child-name child-models)
                                              (child-name child-mode-frames)))})
                            (keys child-models))))})))

(defn setup []
  (let [model (select-model selected-model (edn/read-string (slurp model-file-path)))]
    {:model model}))

; This is where any automatic updates to env should happen.
(defn next-state 
  [{:keys [env model mode-frame]}]
  (let [env (assoc-in env [:time] (System/currentTimeMillis))]
    (map->State 
      {:env env
       :model model
       :mode-frame 
         {:mode mode/strip-blink
          :params {:on-color {:r 156 :g 42 :b 0}
                   :off-color {:r 156 :g 42 :b 0}
                   :period 25}}})))

(defn log-framerate [period]
  (fn [step]
    (let [cycle-start-time (atom (System/currentTimeMillis))
          frame-count (atom 0)]
      (fn
        ([] (step))
        ([accum] (step accum))
        ([accum frame] 
          (let [current-time (System/currentTimeMillis)
                elapsed-cycle-time (- current-time @cycle-start-time)]
            (if (> elapsed-cycle-time period)
              (let [framerate (quot 1000 (/ elapsed-cycle-time @frame-count))]
                (log/info "Framerate:" framerate)
                (reset! cycle-start-time (System/currentTimeMillis))
                (reset! frame-count 0))
              (swap! frame-count inc))
            (step accum frame)))))))


(defmulti update-state 
  (fn [command state] (:type command)))

(defmethod update-state :stop [command state]
  state)

(defmethod update-state :render [command state]
  (next-state state))

(defn render-loop [command-chan render-chan]
  (go-loop [last-state (setup)]
    (let [command (alt!
                    command-chan ([command] command)
                    :default {:type :render})
          state (update-state command last-state)]
      (case (:type command)
        :stop nil
        :render (do
                  (>! render-chan state)
                  (recur state))
        (recur state)))))

(def render-pipeline
  (comp
    (map eval-state)
    (map mode/pixel-map)
    (log-framerate 1000)
    ))

(defn -main
  [& args]
  (let [command-chan (chan (buffer 10))
        render-chan (chan 1 render-pipeline)
        render-loop-chan (render-loop command-chan render-chan)]
    (go-loop []
      (fc/push-pixels (<! render-chan)))
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. (fn [] 
                  (log/info "Recieved shutdown signal. Halting...")
                  (>!! render-chan {:type :stop}))))
    (<!! render-loop-chan)))

(log/info "Alive and kicking!")
