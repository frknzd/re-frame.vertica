(ns re-frame-inspector.graph-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [goog.object :as gobj]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [reagent.ratom :as ratom]
            [re-frame-inspector.graph :as graph]
            [re-frame-inspector.registry :as registry]
            [re-frame-inspector.state :as state]))

(defn- mock-element [render-reaction]
  (let [component #js {:tag 1
                       :stateNode #js {:cljsRatom render-reaction}
                       :type (fn SelectedCard [])
                       :memoizedProps nil
                       :return nil}
        host #js {:tag 5 :return component}
        element #js {:tagName "ARTICLE" :id "selected" :className "card"}]
    (gobj/set element "__reactFiber$inspector-test" host)
    element))

(defn- nodes-of-kind [snapshot kind]
  (filter #(= kind (:kind %)) (:nodes snapshot)))

(defn- capturing-reaction [f]
  (let [owner #js {}]
    (ratom/run-in-reaction f owner "cljsRatom" (fn [_]) {})
    (gobj/get owner "cljsRatom")))

(deftest live-dag-and-provenance
  (registry/install!)
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (reset! db/app-db {:profile {:name "Ada"}})
  (rf/reg-sub ::profile (fn [db _] (:profile db)))
  (rf/reg-sub ::name :<- [::profile] (fn [profile _] (:name profile)))
  (let [subscription (rf/subscribe [::name])
        render-reaction (capturing-reaction #(deref subscription))
        snapshot (graph/snapshot (mock-element render-reaction))]
    (is (= [subscription] (vec (array-seq (gobj/get render-reaction "watching")))))
    (testing "layer-2, layer-3, component and element nodes"
      (is (= 1 (count (nodes-of-kind snapshot :app-db-path))))
      (is (= 2 (count (nodes-of-kind snapshot :subscription))))
      (is (= 1 (count (nodes-of-kind snapshot :component))))
      (is (= 1 (count (nodes-of-kind snapshot :element))))
      (is (= "[:profile]" (:label (first (nodes-of-kind snapshot :app-db-path))))))
    (testing "shared nodes are de-duplicated"
      (is (= (count (:nodes snapshot)) (count (set (map :id (:nodes snapshot)))))))
    (ratom/dispose! render-reaction)
    (ratom/dispose! subscription))
  (rf/clear-sub ::profile)
  (rf/clear-sub ::name))

(deftest raw-subscriptions-are-partial
  (registry/install!)
  (gobj/set js/globalThis "React" #js {:version "17.0.2"})
  (rf/reg-sub-raw ::raw (fn [app-db _] (ratom/make-reaction #(deref app-db))))
  (let [subscription (rf/subscribe [::raw])
        render-reaction (capturing-reaction #(deref subscription))
        snapshot (graph/snapshot (mock-element render-reaction))
        node (first (nodes-of-kind snapshot :subscription))]
    (is (false? (:complete? node)))
    (is (re-find #"reg-sub-raw" (:reason node)))
    (ratom/dispose! render-reaction)
    (ratom/dispose! subscription))
  (rf/clear-sub ::raw))

(deftest clear-and-hot-registration-lifecycle
  (registry/install!)
  (rf/reg-sub ::hot (fn [db _] (:old db)))
  (let [generation (get-in @state/registrations [::hot :generation])]
    (rf/reg-sub ::hot (fn [db _] (:new db)))
    (is (= (inc generation)
           (get-in @state/registrations [::hot :generation]))))
  (rf/clear-sub ::hot)
  (is (nil? (get @state/registrations ::hot))))
