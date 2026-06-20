(ns skylobby.fx-test
  (:require
    [clojure.test :refer [deftest is]]
    skylobby.fx))


(set! *warn-on-reflection* true)


(def screen-bounds
  {:min-x -2560.0
   :min-y 0.0
   :max-x 2560.0
   :max-y 1440.0
   :width 5120.0
   :height 1440.0})


(deftest fitx
  (is (= 0
         (with-redefs [skylobby.fx/get-screen-bounds (constantly nil)]
           (skylobby.fx/fitx nil))))
  (is (= -128.0
         (with-redefs [skylobby.fx/get-screen-bounds (constantly screen-bounds)]
           (skylobby.fx/fitx nil))))
  (is (= -128.0
         (skylobby.fx/fitx screen-bounds))))
(deftest fity
  (is (= 0
         (with-redefs [skylobby.fx/get-screen-bounds (constantly nil)]
           (skylobby.fx/fity nil))))
  (is (= 592.0
         (with-redefs [skylobby.fx/get-screen-bounds (constantly screen-bounds)]
           (skylobby.fx/fity nil))))
  (is (= 592.0
         (skylobby.fx/fity screen-bounds))))
(deftest fitwidth
  (is (= 256
         (with-redefs [skylobby.fx/get-screen-bounds (constantly nil)]
           (skylobby.fx/fitwidth nil -10000))))
  (is (= 256
         (with-redefs [skylobby.fx/get-screen-bounds (constantly screen-bounds)]
           (skylobby.fx/fitwidth nil -10000))))
  (is (= 256
         (skylobby.fx/fitwidth screen-bounds -10000))))
(deftest fitheight
  (is (= 256
         (with-redefs [skylobby.fx/get-screen-bounds (constantly nil)]
           (skylobby.fx/fitheight nil -10000))))
  (is (= 256
         (with-redefs [skylobby.fx/get-screen-bounds (constantly screen-bounds)]
           (skylobby.fx/fitheight nil -10000))))
  (is (= 256
         (skylobby.fx/fitheight screen-bounds -10000))))

(deftest type-scale-has-expected-steps
  (is (= 14 (:base skylobby.fx/type-scale)))
  (is (= 28 (:xxl skylobby.fx/type-scale)))
  (is (every? number? (vals skylobby.fx/type-scale))))

(deftest space-scale-is-a-numeric-ladder
  (is (= [4 8 12 16 24 32]
         (map skylobby.fx/space-scale [:1 :2 :3 :4 :5 :6]))))

(deftest ramps-define-all-surface-keys
  (doseq [ramp [skylobby.fx/black-ramp skylobby.fx/grey-ramp skylobby.fx/light-ramp]]
    (doseq [k [:surface-0 :surface-1 :surface-2 :surface-3 :border :focus
               :selection :selection-unfocused :text-1 :text-2
               :row-odd :row-even :thumb :thumb-hover :tab-highlight]]
      (is (contains? ramp k) (str "missing " k)))
    ;; focus must be visible, never transparent (a11y regression guard)
    (is (not= "transparent" (:focus ramp)))))
