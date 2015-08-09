(ns sparkle.core
  (:require [clojure.browser.repl :as repl]
            [clojure.edn :as edn]
            [fipp.edn :as fipp]
            [sparkle.mode :as mode]
            [sparkle.color :as color]))

;; (defonce conn
;;   (repl/connect "http://localhost:9000/repl"))

(def model-file-path "models.edn")

(def selected-model "test-strip")

(enable-console-print!)

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
     :pixels (mode model params env)}
    
    (let [child-mode-frames (mode model params env)
          child-models (model :model/child-models)]
      {:model-node (dissoc model :model/children)
       :mode-frame mode-frame
       :children (cond
                   (seq? child-models) (map #(eval-mode env %1 %2) child-models child-mode-frames)
                   (map? child-models) (into {} (map (fn [{name model} mode-frame]
                                                         {name (eval-mode env model mode-frame)})
                                                     child-models child-mode-frames)))})))

(defn setup []
  (let [model (select-model selected-model (edn/read-string (slurp model-file-path)))]
    {:model model))

(defn update [{:keys [model]}]
  ; specify initial mode and params
  ; provide env-map
  (let [env {:time (.getTime (js/Date.))}]
    {:model model
     :mode mode/strip-blink
     :params {:on-color color/full-white
              :off-color color/black
              :period 500}
     :env env}))

(defn render [{:keys [model mode params env]}]
  (let [mode-tree (eval-mode model {:mode mode :params params} env)]
    (fipp/pprint [mode-tree])))

(defn run []
  (let [initial-state (setup)]
    (loop [state initial-state]
      (render (update state)))))

(run)