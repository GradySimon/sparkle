(ns sparkle.spec
  "Specs for important Sparkle components"
  (:require [clojure.spec :as s]))

(s/def :render/state (s/keys :req-un [:core/env :core/model]))
(s/def :render/command (s/keys :req-un [:render.command/type]))
(s/def :render.command/type #{:start :stop :update :kill})
(s/def :render/status #{:running :stopped})

(s/def :core/env (s/keys :opt-un [:env/time]))
(s/def :env/time (s/and long? #(> % 0)))

(s/def :core/model (s/keys :req-un [:model/shape :model/layers]))
(s/def :model/shape (s/keys :req-un [:model/type]))
(s/def :model/type keyword?)
(s/def :model/layers (s/coll-of :core/layer []))

(s/def :core/layer
  (s/or :stateless-layer :layer/layer-fn
        :stateful-layer (s/keys :req-un [:layer/layer-fn :layer/state])))
(s/def :layer/layer-fn
  (s/fspec :args (s/cat :env :core/env :frame :core/frame)
           :ret :layer/return))
           ;:fn #(= (count (-> % :args :frame)) (count (-> % :ret :frame)))))
; TODO: switch layers to always return a map. Get rid of s/or in layer return val. Reenable :fn spec in layer-fn fdef)). Simplify iterate-layer.
(s/def :layer/return (s/or :frame :core/frame
                           :frame-and-state (s/keys :req-un [:core/frame :layer/state])))
(s/def :layer/state (constantly true))

(s/def :core/frame (s/coll-of :core/color []))

(s/def :core/color associative?)
