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
  (let [model (select-model selected-model (edn/read-string (slurp model-file-path)))
        env {:time (System/currentTimeMillis)}]
    (map->State 
      {:env env
       :model model
       :mode-frame 
         {:mode mode/plasma
          :params {}}})))

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
  (go-loop [last-state (setup)
            status :running]
    (let [state (next-state last-state)
          command 
            (if (= status :running)
                  (alt!
                    command-chan ([command] command)
                    :default {:type :render})
                  (<! command-chan))]
      (case (:type command)
        :stop (recur state :stopped)
        :start (recur state :running)
        :edit (recur (edit-state command state) status)
        :report (do
                  (fipp/pprint state)
                  (recur state status))
        :render (do
                  (>! render-chan state)
                  (recur state status))
        (recur state status)))))

(def render-pipeline
  (comp
    (map eval-state)
    (map mode/pixel-map)
    ;(log-framerate 1000)
    ))

(def command-chan (chan (buffer 10)))

(def render-chan (chan (buffer 1) render-pipeline))

(defn init []
  (fc/start-pushing-pixels render-chan)
  (render-loop command-chan render-chan)
  :success)

(defn -main
  [& args]
  (let [render-chan (chan (buffer 1) render-pipeline)
        pixel-pusher-chan (fc/start-pushing-pixels render-chan)
        render-loop-chan (render-loop command-chan render-chan)]
    (.addShutdownHook (Runtime/getRuntime)
      (Thread. (fn []
                  (println "\nRecieved shutdown signal. Halting...")
                  (>!! command-chan {:type :stop}))))
    (<!! render-loop-chan)
    (<!! pixel-pusher-chan)))

(defn edit [path value]
  (>!! command-chan {:type :edit :path path :value value}))

(defn start []
  (>!! command-chan {:type :start}))

(defn stop []
  (>!! command-chan {:type :stop}))

(defn report []
  (>!! command-chan {:type :report}))

(println "Alive and kicking!")
