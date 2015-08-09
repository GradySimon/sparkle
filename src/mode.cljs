(ns sparkle.mode)

; modes return either a seq or a map of these:
; {:mode mode-name
;  :params params-map}
; for each child. seq if their :model/children attribute is specified as a seq
; map if they use a map 
; eval-mode will also supply an env-map to each mode call, 
; which it gets from core/update


(defn px-blink [period {:keys [on-color off-color]} time]
  (if (= 0 (mod (/ time period) 2))
    (on-color)
    (off-color)))

(defn strip-blink [model {:keys [period on-color off-color] :as params} {:keys [time] :as env}]
  (repeat (model :model/count)
    {:mode px-blink :params params}))

(defn mode-frame? [value]
  (and (contains? value :mode)
       (contains? value :params)))

