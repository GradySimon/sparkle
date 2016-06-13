(ns sparkle.spec
  "Specs for important Sparkle components"
  (:require [clojure.spec :as s]))

(s/def :core/render-state (s/keys :req-un [:core/env :core/model]))

(s/def :core/env (s/keys :opt-un [:env/time]))

(s/def :env/time (s/and long? #(> % 0)))

(s/def :core/model (s/keys :req [:model/shape :model/layers]))

(s/def :model/shape (s/keys :req [:type]))

(s/def :model/layers (s/coll-of :model/layer []))

(s/def :core/layer
  (s/or :stateless-layer :layer/layer-fn
        :stateful-layer (s/keys :req-un [:layer/layer-fn :layer/state])))

(s/def :layer/layer-fn (s/fspec :args (s/cat :env :core/env :frame :core/frame)
                                :ret :layer/layer-return
                                :fn #(= (count (-> % :args :frame)) (count (:ret)))))
; TODO: Add constraint about stateful layer arity to layer-fn spec

(s/def :layer/layer-return (s/or :frame :core/frame
                                 :frame-and-state (s/keys :req-un [:core/frame :layer/state])))

(s/def :layer/state (constantly true))

(s/def :core/frame (constantly true)) ; TODO: spec for frame
