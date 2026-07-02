(ns skylobby.fx.sync-test
  (:require
    [cljfx.api :as fx]
    [clojure.test :refer [deftest is]]
    [skylobby.fx.sync :as fx.sync]))


(set! *warn-on-reflection* true)


(deftest sync-pane
  (is (map?
        (fx.sync/sync-pane
          {:fx/context (fx/create-context nil)})))
  (is (map?
        (fx.sync/sync-pane
          {:fx/context (fx/create-context nil)
           :issues [{}]}))))

(deftest sync-pane-has-padding
  (let [pane (fx.sync/sync-pane {:resource "Map" :issues []})]
    (is (= :v-box (:fx/type pane)))
    (is (= 8 (:padding pane)))
    (is (= 4 (:spacing pane)))))
