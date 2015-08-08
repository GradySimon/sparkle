(ns sparkle.core
  (:require [clojure.algo.generic.functor :only fmap]
            [clojure.browser.repl :as repl]
            [clojure.edn :as edn]
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
  (let [child-mode-frames (mode model params env)
        chold-models (model :model/chold-models)]
    {:model-node model
     :mode-frame mode-frame
     :children (cond
                 (seq? chold-models) (map #(eval-mode env %1 %2) chold-models child-mode-frames)
                 (map? chold-models) (into {} (map (fn [{name model} mode-frame]
                                                       {name (eval-mode env model mode-frame)})
                                                   chold-models child-mode-frames)))}))

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
  (let [evaluated-mode (eval-mode model {:mode mode :params params} env)]
    ))
