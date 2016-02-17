(ns sparkle.layer-test
  (:require [clojure.test :refer :all]
            [sparkle.layer :refer :all]))

(def black {:r 0 :g 0 :b 0})
(def white {:r 1 :g 1 :b 1})
(def mid-magenta {:r 0.5 :g 0 :b 0.5})

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

(deftest test-stepping-static-color
  (let [ssc-layer (stepping-static-color mid-magenta 1000)]
    (testing "stepping-static-color"
      (testing "Call stepping-static-color without state"
        (is (= (ssc-layer {:time 42000} three-white)
               {:leds [mid-magenta white white]
                :state {:offset 0 :last-time 42000}})))
      (testing "Call stepping-static-color when it's time to transition"
        (is (= (ssc-layer {:time 42000} {:offset 1 :last-time 41000} three-white)
               {:leds [white white mid-magenta]
                :state {:offset 2 :last-time 42000}}))))))

(deftest test-apply-layer
  (testing "apply-layer"
    (testing "called with an empty layer vector"
      (is (= (apply-layers [] {:time 42000} three-white)
             [three-white []])))))
