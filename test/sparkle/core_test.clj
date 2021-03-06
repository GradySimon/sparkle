(ns sparkle.core-test
  (:require [clojure.test :refer :all]
            [sparkle.core :refer :all]))

(deftest render-blank-state
  (testing "Rendering with an empty layer vector"
    (is (= (render-step (map->RenderState {:env {}
                                           :model {:shape {:type :strip :pixel-count 3}
                                                   :layers []}}))
           [black black black]))))
