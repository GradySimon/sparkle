(ns sparkle.layer-test
  (:require [clojure.test :refer :all]
            [sparkle.layer :refer :all]))

(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})

(def three-white (vec (take 3 (repeat white))))

(deftest test-scale-brightness
  (testing "scale-brigntess"
    (is (= (vec (take 3 (repeat {:r 0.5 :g 0.5 :b 0.5})))
           ((scale-brightness 0.5) {} three-white)))))

(deftest test-brightness-gradient
  (testing "brightness-gradient"
    (let [bg (brightness-gradient 0 1)]
      (is (= [black {:r 1/2 :g 1/2 :b 1/2} white]
             (bg {} three-white))))))


