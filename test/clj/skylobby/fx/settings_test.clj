(ns skylobby.fx.settings-test
  (:require
    [cljfx.api :as fx]
    [clojure.test :refer [deftest is]]
    [skylobby.fx.settings :as fx.settings]))


(set! *warn-on-reflection* true)


(deftest battle-settings
  (is (map?
        (fx.settings/battle-settings
          {:fx/context (fx/create-context nil)}))))


(deftest settings-root
  (is (map?
        (fx.settings/settings-root
          {:fx/context (fx/create-context nil)}))))


(deftest settings-window
  (is (map?
        (fx.settings/settings-window-impl
          {:fx/context (fx/create-context nil)
           :screen-bounds {}}))))


(deftest filterable-section-is-carded
  (let [result (fx.settings/filterable-section {:title "Example" :search "" :children []})]
    (is (= :v-box (:fx/type result)))
    (is (= ["skylobby-card"] (:style-class result)))))

(deftest filterable-section-has-rhythm
  (let [r (fx.settings/filterable-section {:title "X" :search "" :children []})]
    (is (= 8 (:spacing r)))))

(deftest checkbox-setting-uses-body-class
  (let [r (fx.settings/filterable-checkbox-setting {:title "X" :search "" :check-box {:fx/type :check-box}})]
    (is (= ["skylobby-body"] (:style-class r)))))
