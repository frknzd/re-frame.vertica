(ns re-frame.vertica.graph-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [goog.object :as gobj]
            [re-frame.core :as rf]
            [re-frame.db :as db]
            [reagent.ratom :as ratom]
            [re-frame.vertica.graph :as graph]
            [re-frame.vertica.registry :as registry]
            [re-frame.vertica.state :as state]))

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
      (is (= "[:profile :name]" (:label (first (nodes-of-kind snapshot :app-db-path)))))
      (is (= [":profile" ":name"]
             (:association-path (first (nodes-of-kind snapshot :app-db-path)))))
      (is (= 2 (:specificity (first (nodes-of-kind snapshot :app-db-path)))))
      (is (map? (:app-db-tree snapshot)))
      (let [path-id (:id (first (nodes-of-kind snapshot :app-db-path)))
            profile-id (:id (first (filter #(= "[:re-frame.vertica.graph-test/profile]" (:label %))
                                           (nodes-of-kind snapshot :subscription))))
            name-id (:id (first (filter #(= "[:re-frame.vertica.graph-test/name]" (:label %))
                                        (nodes-of-kind snapshot :subscription))))]
        (is (some #(= {:from path-id :to profile-id :kind :data-input} %)
                  (:edges snapshot)))
        (is (not-any? #(= {:from path-id :to name-id :kind :data-input} %)
                      (:edges snapshot)))))
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

(deftest provenance-includes-reagent-owners-that-supply-child-props
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (let [parent-value {:name "parent"}]
    (reset! db/app-db {:parent parent-value :selected "selected" :unrelated "noise"})
  (rf/reg-sub ::parent-only (fn [db _] (:parent db)))
  (rf/reg-sub ::selected-only (fn [db _] (:selected db)))
  (rf/reg-sub ::unrelated (fn [db _] (:unrelated db)))
  (let [parent-sub (rf/subscribe [::parent-only])
        selected-sub (rf/subscribe [::selected-only])
        unrelated-sub (rf/subscribe [::unrelated])
        parent-reaction (capturing-reaction #(do (deref parent-sub) (deref unrelated-sub)))
        selected-reaction (capturing-reaction #(deref selected-sub))
        selected-component (fn SelectedComponent [])
        parent #js {:tag 1
                    :stateNode #js {:cljsRatom parent-reaction}
                    :type (fn ParentComponent [])
                    :memoizedProps nil
                    :return nil}
        selected #js {:tag 1
                      :stateNode #js {:cljsRatom selected-reaction}
                      :type selected-component
                      ;; The scalar equals the unrelated subscription output on
                      ;; purpose; scalar equality must not become provenance.
                      :memoizedProps #js {:argv [selected-component parent-value "noise"]}
                      :return parent}
        plain-wrapper #js {:tag 1
                           :stateNode #js {}
                           :type (fn PlainReactWrapper [])
                           :memoizedProps nil
                           :return selected}
        host #js {:tag 5 :return plain-wrapper}
        element #js {:tagName "DIV" :id "selected-only" :className ""}]
    (gobj/set element "__reactFiber$inspector-nearest-test" host)
    (let [snapshot (graph/snapshot element)]
      (is (= #{"[:parent]" "[:selected]"}
             (set (map :label (nodes-of-kind snapshot :app-db-path)))))
      (is (= #{"[:re-frame.vertica.graph-test/parent-only]"
               "[:re-frame.vertica.graph-test/selected-only]"}
             (set (map :label (nodes-of-kind snapshot :subscription)))))
      (is (= 2 (count (nodes-of-kind snapshot :prop))))
      (is (= #{"arg 1" "arg 2"}
             (set (map :label (nodes-of-kind snapshot :prop)))))
      (is (= #{"re-frame.vertica.graph-test/SelectedComponent"}
             (set (map :component-name (nodes-of-kind snapshot :prop)))))
      (is (= #{0 1}
             (set (map :argument-index (nodes-of-kind snapshot :prop)))))
      (is (= #{2}
             (set (map :argument-count (nodes-of-kind snapshot :prop)))))
      (is (= 1 (count (filter :complete? (nodes-of-kind snapshot :prop)))))
      (is (= 2 (count (nodes-of-kind snapshot :component))))
      (is (not-any? #(re-find #"PlainReactWrapper$" (:label %))
                    (nodes-of-kind snapshot :component))))
    (doseq [reaction [parent-reaction selected-reaction parent-sub selected-sub unrelated-sub]]
      (ratom/dispose! reaction)))
  (rf/clear-sub ::parent-only)
  (rf/clear-sub ::selected-only)
  (rf/clear-sub ::unrelated)))
