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
               :row-odd :row-even :thumb :thumb-hover :tab-highlight :tab-selected-accent]]
      (is (contains? ramp k) (str "missing " k)))
    ;; focus must be visible, never transparent (a11y regression guard)
    (is (not= "transparent" (:focus ramp)))))

(deftest theme-data-wires-ramp-into-root
  (let [d (skylobby.fx/theme-data skylobby.fx/black-ramp)]
    (is (= "rgb(18,18,18)" (get-in d [".root" :-fx-background])))
    (is (= "rgb(28,28,28)" (get-in d [".root" :-fx-base])))
    (is (= "rgb(40,40,40)" (get-in d [".root" :-fx-control-inner-background])))
    ;; focus is visible, not transparent
    (is (= "rgb(120,120,130)" (get-in d [".root" :-fx-focus-color])))
    ;; primary text color is wired from the ramp token
    (is (= "rgb(228,228,228)" (get-in d [".root" :-fx-text-base-color])))))

(deftest theme-data-defines-structural-selectors
  (let [d (skylobby.fx/theme-data skylobby.fx/black-ramp)]
    (doseq [sel [".root" ".tab" ".tab:selected" ".tab-header-background"
                 ".table-view" ".table-row-cell:odd" ".table-row-cell:even"
                 ".scroll-bar:vertical .thumb"]]
      (is (contains? d sel) (str "missing selector " sel)))
    (is (some? (get-in d [".scroll-bar:vertical .thumb" :-fx-background-color])))))

(deftest presets-keep-public-api
  (is (= #{"black" "grey" "light"} (set (keys skylobby.fx/style-presets))))
  ;; each preset still a non-empty map with a themed root
  (doseq [k ["black" "grey" "light"]]
    (let [d (get skylobby.fx/style-presets k)]
      (is (map? d))
      (is (contains? (get d ".root") :-fx-background)))))

(deftest every-preset-has-visible-focus
  (doseq [k ["black" "grey" "light"]]
    (is (not= "transparent"
              (get-in (get skylobby.fx/style-presets k) [".root" :-fx-focus-color])))))

(deftest every-preset-has-card-class
  (doseq [k ["black" "grey" "light"]]
    (let [d (get skylobby.fx/style-presets k)]
      (is (contains? d ".skylobby-card") (str k " missing .skylobby-card"))
      (is (some? (get-in d [".skylobby-card" :-fx-background-color]))))))
