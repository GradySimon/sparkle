(ns sparkle.core
  (:require [clojure.edn :as edn]
            [clojure.core.async :as a
             :refer [>! <! >!! <!! go go-loop chan buffer close! thread
                     alt! alts! alts!! timeout]]
            [clojure.tools.logging :as log]
            [fipp.edn :as fipp]
            [sparkle.mode :as mode]
            [sparkle.color :as color]
            [sparkle.fadecandy-opc :as fc])
  (:gen-class))

(def model-file-path "models.edn")

(def selected-model "right-sleeve")

; note: currently there are two concepts of state: this
; record, and mode state, which is part of a mode frame

(defrecord State [env model mode-frame])

(defn select-model [name model]
  (first (filter #(= name (% :model/name)) model)))

; eval-state returns a tree-structured map, that looks like this:
; {:model-node {...}
;  :mode-frame {:mode mode :params params}
;  :children [{:child-name {child}} | [{child}]]}

; To support stateful modes, there needs to be some means of persisting
; a mode's state accross invokations of eval-state.
; One way to do this is to pass each recursive call to eval-state a path
; at which it can find its state in the previous eval tree 
; 
; Alternatively, just pass the state? Top level call gets the whole previous
; tree. It traverses it in parallel with the model, passes it to each recursive
; call.


(defn eval-state 
  [{env :env 
    model :model 
    {:keys [mode params] :as mode-frame} :mode-frame}
    {last-state :state last-children :children}]
  (if (not (contains? model :model/children))
    (let [{pixels :pixels updated-state :state} (mode env model params last-state)]
      {:model-node model
       :mode-frame mode-frame
       :state updated-state
       :pixels pixels})
    
    (let [{child-mode-frames :child-mode-frames updated-state :state} 
            (mode env model params last-state)
          child-models 
            (:model/children model)]
      {:model-node (dissoc model :model/children)
       :mode-frame mode-frame
       :state updated-state
       :children (cond
                   (sequential? child-models)
                     (map #(eval-state (->State env %1 %2) %3) 
                      child-models 
                      child-mode-frames
                      (if last-children
                        last-children
                        (repeat (count child-models) nil)))
                   (map? child-models)
                     (into {}
                       (map (fn [child-name]
                              {child-name (eval-state
                                            (->State
                                              env
                                              (child-name child-models)
                                              (child-name child-mode-frames))
                                            (child-name (if last-children
                                                          last-children
                                                          (repeat (count child-models) nil))))})
                            (keys child-models))))})))

(defn setup []
  (let [model (select-model selected-model (edn/read-string (slurp model-file-path)))
        env {:time (System/currentTimeMillis)}]
    (map->State 
      {:env env
       :model model
       :mode-frame 
         {:mode mode/conways
          :params {:period 1000
                   :on-color {:r 0.3 :g 0 :b 0.3}
                   :off-color {:r 0 :g 0 :b 0.3}}}})))

; This is where any automatic updates to env should happen.
(defn next-state 
  [{:keys [env model mode-frame]}]
  (let [env (assoc-in env [:time] (System/currentTimeMillis))]
    (map->State 
      {:env env
       :model model
       :mode-frame mode-frame})))

(defn report-framerate [framerate]
  nil)

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
                (report-framerate framerate)
                (reset! cycle-start-time (System/currentTimeMillis))
                (reset! frame-count 0))
              (swap! frame-count inc))
            (step accum frame)))))))

(defn edit-state [{:keys [path value]} state]
  (assoc-in state path value))

(defn render-loop [command-chan render-chan]
  (go-loop [status :running
            last-state (setup)
            last-eval-tree {}]
    (let [state (next-state last-state)
          command
            (if (= status :running)
                  (alt!
                    command-chan ([command] command)
                    :default {:type :render})
                  (<! command-chan))]
      (case (:type command)
        :stop (recur :stopped state last-eval-tree)
        :start (recur :running state last-eval-tree)
        :edit (recur status (edit-state command state) last-eval-tree)
        :report (do
                  (fipp/pprint state)
                  (recur status state last-eval-tree))
        :render (let [eval-tree (eval-state state last-eval-tree)
                      pixels (mode/pixel-map eval-tree)]
                  ;(println "---------------------")
                  (>! render-chan pixels)
                  (recur status state eval-tree))
        (recur state status last-eval-tree)))))

(def render-pipeline
  (comp
    (map eval-state)
    (map mode/pixel-map)
    ;(log-framerate 1000)
    ))

(defonce command-chan (chan (buffer 10)))

(defonce render-chan (chan 1))

(defn init []
  (fc/start-pushing-pixels render-chan)
  (render-loop command-chan render-chan)
  :success)

(defn edit [path value]
  (>!! command-chan {:type :edit :path path :value value}))

(defn start []
  (>!! command-chan {:type :start}))

(defn stop []
  (>!! command-chan {:type :stop}))

(defn report []
  (>!! command-chan {:type :report}))

(println "Alive and kicking!")
