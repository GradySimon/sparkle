(ns sparkle.core
  (:require [clojure.edn :as edn]
            [fipp.edn :refer [pprint]]
            [sparkle.mode :as mode]
            [sparkle.color :as color])
  (:gen-class))

(def model-file-path "models.edn")

(def selected-model "test-installation")

(println "Alive and kicking!")

(defn select-model [name model]
  (first (filter #(= name (% :model/name)) model)))

; eval-mode returns a tree-structured map, that looks like this:
; {:model-node {...}
;  :mode-frame {:mode mode :params params}
;  :children [{:child-name {child}} | [{child}]]}

(defn eval-mode [env model {:keys [mode params] :as mode-frame}]
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
                     (map #(eval-mode env %1 %2) child-models child-mode-frames)
                   (map? child-models)
                     (into {}
                       (map (fn [child-name]
                              {child-name (eval-mode
                                            env
                                            (child-name child-models)
                                            (child-name child-mode-frames))})
                            (keys child-models))))})))

(defn setup []
  (let [model (select-model selected-model (edn/read-string (slurp model-file-path)))]
    {:model model}))

(defn next-state [{:keys [model]}]
  ; specify initial mode and params
  ; provide env-map
  (let [env {:time (System/currentTimeMillis)}]
    {:model model
     :mode mode/test-installation-blink
     :params {:on-color color/full-white
              :off-color color/black
              :period 500}
     :env env}))

(defn render [{:keys [model mode params env]}]
  (let [mode-tree (eval-mode env model {:mode mode :params params})]
    (pprint [mode-tree])
    ))

(defn run []
  (loop [last-state (setup)
         cycle-start-time (System/currentTimeMillis)
         frame-number 0]
    (let [current-state (next-state last-state)
          current-time (System/currentTimeMillis)
          elapsed-cycle-time (- current-time cycle-start-time)]
      (render (next-state current-state))
      (if (> (- current-time cycle-start-time) 100)
        ; Report frame rate, restart cycle
        (let [frame-rate (quot 1000 (/ elapsed-cycle-time frame-number))]
          (println "Frame rate:" frame-rate "Frames this cycle:" frame-number)
          (recur current-state (System/currentTimeMillis) 0))
        ; Otherwise, continue
        (recur current-state cycle-start-time (inc frame-number))))))

(defn -main
  [& args]
  (run))