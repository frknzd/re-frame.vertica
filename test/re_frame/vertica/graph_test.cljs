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
  (reset! state/selection-generation 17)
  (let [subscription (rf/subscribe [::name])
        render-reaction (capturing-reaction #(let [name @subscription]
                                               [:article#selected.card name]))
        snapshot (graph/snapshot (mock-element render-reaction))]
    (is (= 17 (:selection-generation snapshot)))
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
        selected-component (fn SelectedComponent [])
        selected-state #js {}
        selected-props #js {:argv [selected-component parent-value "noise"]}
        _ (gobj/set selected-state "props" selected-props)
        selected-reaction
        (capturing-reaction
          #(let [selected-value @selected-sub
                 argv (gobj/get (gobj/get selected-state "props") "argv")
                 inherited (nth argv 1)]
             [:div#selected-only (:name inherited) selected-value]))
        _ (gobj/set selected-state "cljsRatom" selected-reaction)
        parent #js {:tag 1
                    :stateNode #js {:cljsRatom parent-reaction}
                    :type (fn ParentComponent [])
                    :memoizedProps nil
                    :return nil}
        selected #js {:tag 1
                      :stateNode selected-state
                      :type selected-component
                      ;; The scalar equals the unrelated subscription output on
                      ;; purpose; scalar equality must not become provenance.
                      :memoizedProps selected-props
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
                    (nodes-of-kind snapshot :component)))
      (is (identical? selected-props (gobj/get selected-state "props")))
      (is (= [selected-component parent-value "noise"]
             (gobj/get selected-props "argv"))))
    (doseq [reaction [parent-reaction selected-reaction parent-sub selected-sub unrelated-sub]]
      (ratom/dispose! reaction)))
  (rf/clear-sub ::parent-only)
  (rf/clear-sub ::selected-only)
  (rf/clear-sub ::unrelated)))

(deftest causal-provenance-removes-read-but-unused-and-sibling-only-paths
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (reset! db/app-db {:selected "selected" :sibling "sibling" :discarded "noise"})
  (rf/reg-sub ::selected-causal
    (fn [db _]
      (:discarded db)
      (:selected db)))
  (rf/reg-sub ::sibling-causal (fn [db _] (:sibling db)))
  (let [selected-sub (rf/subscribe [::selected-causal])
        sibling-sub (rf/subscribe [::sibling-causal])
        render-reaction
        (capturing-reaction
          #(let [selected @selected-sub sibling @sibling-sub]
             [:section#root
              [:span#selected-branch selected]
              [:span#sibling-branch sibling]]))
        component #js {:tag 1
                       :stateNode #js {:cljsRatom render-reaction}
                       :type (fn CausalCard [])
                       :memoizedProps nil
                       :return nil}
        root-fiber #js {:tag 5 :return component}
        selected-fiber #js {:tag 5 :return root-fiber}
        selected #js {:tagName "SPAN" :id "selected-branch" :className ""}
        root #js {:tagName "SECTION" :id "root" :className ""
                  :children #js [selected]}
        _ (gobj/set selected "parentElement" root)
        _ (gobj/set root "parentElement" nil)
        _ (gobj/set root "__reactFiber$causal-root" root-fiber)
        _ (gobj/set selected "__reactFiber$causal-selected" selected-fiber)
        snapshot (graph/snapshot selected)]
    (is (= #{"[:selected]"}
           (set (map :label (nodes-of-kind snapshot :app-db-path)))))
    (is (every? #(= :confirmed (:evidence %))
                (nodes-of-kind snapshot :app-db-path)))
    (doseq [reaction [render-reaction selected-sub sibling-sub]]
      (ratom/dispose! reaction)))
  (rf/clear-sub ::selected-causal)
  (rf/clear-sub ::sibling-causal))

(deftest callback-closures-are-ignored-but-visible-styles-are-causal
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (reset! db/app-db {:callback-value 1 :color "red"})
  (rf/reg-sub ::callback-value (fn [db _] (:callback-value db)))
  (rf/reg-sub ::color (fn [db _] (:color db)))
  (let [callback-sub (rf/subscribe [::callback-value])
        color-sub (rf/subscribe [::color])
        render-reaction
        (capturing-reaction
          #(let [callback-value @callback-sub color @color-sub]
             [:article#selected.card
              {:on-click (fn [] callback-value)
               :style {:color color}}
              "Go"]))
        snapshot (graph/snapshot (mock-element render-reaction))]
    (is (= #{"[:color]"}
           (set (map :label (nodes-of-kind snapshot :app-db-path)))))
    (is (every? #(= :confirmed (:evidence %))
                (nodes-of-kind snapshot :app-db-path)))
    (doseq [reaction [render-reaction callback-sub color-sub]] (ratom/dispose! reaction)))
  (rf/clear-sub ::callback-value)
  (rf/clear-sub ::color))

(deftest failed-counterfactual-renders-are-uncertain-and-restore-live-state
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (let [original-db {:value "stable"}]
    (reset! db/app-db original-db)
    (rf/reg-sub ::fragile (fn [db _] (:value db)))
    (let [subscription (rf/subscribe [::fragile])
          render-reaction
          (capturing-reaction
            #(let [value @subscription]
               (when-not (= "stable" value) (throw (js/Error. "counterfactual boom")))
               [:article#selected.card value]))
          live-value @subscription
          live-state (gobj/get subscription "state")
          snapshot (graph/snapshot (mock-element render-reaction))
          uncertain (first (nodes-of-kind snapshot :app-db-path))]
      (is (= :inconclusive (:evidence uncertain)))
      (is (= "[:value]" (:label uncertain)))
      (is (re-find #"counterfactual boom" (:reason uncertain)))
      (is (= original-db @db/app-db))
      (is (= live-value @subscription))
      (is (identical? live-state (gobj/get subscription "state")))
      (doseq [reaction [render-reaction subscription]] (ratom/dispose! reaction)))
    (rf/clear-sub ::fragile)))

(deftest confirmation-wins-when-the-same-path-also-has-inconclusive-evidence
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (reset! db/app-db {:shared "stable"})
  (rf/reg-sub ::shared-confirmed (fn [db _] (:shared db)))
  (rf/reg-sub ::shared-fragile
    (fn [db _]
      (let [value (:shared db)]
        (when-not (= "stable" value) (throw (js/Error. "fragile replay")))
        value)))
  (let [confirmed-sub (rf/subscribe [::shared-confirmed])
        fragile-sub (rf/subscribe [::shared-fragile])
        render-reaction
        (capturing-reaction
          #(let [confirmed @confirmed-sub
                 fragile @fragile-sub]
             [:article#selected.card confirmed fragile]))
        snapshot (graph/snapshot (mock-element render-reaction))
        paths (vec (nodes-of-kind snapshot :app-db-path))
        path-id (:id (first paths))
        subscription-ids (set (map :id (nodes-of-kind snapshot :subscription)))]
    (is (= 1 (count paths)))
    (is (= "[:shared]" (:label (first paths))))
    (is (= :confirmed (:evidence (first paths))))
    (is (= subscription-ids
           (set (map :to (filter #(= path-id (:from %)) (:edges snapshot))))))
    (doseq [reaction [render-reaction confirmed-sub fragile-sub]] (ratom/dispose! reaction)))
  (rf/clear-sub ::shared-confirmed)
  (rf/clear-sub ::shared-fragile))

(deftest deeply-nested-causal-paths-have-no-depth-limit
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (let [path (conj (vec (repeat 80 :child)) :value)]
    (reset! db/app-db (assoc-in {} path "deep"))
    (rf/reg-sub ::deep (fn [db _] (get-in db path)))
    (let [subscription (rf/subscribe [::deep])
          render-reaction (capturing-reaction #(let [value @subscription]
                                                 [:article#selected.card value]))
          snapshot (graph/snapshot (mock-element render-reaction))
          path-node (first (nodes-of-kind snapshot :app-db-path))]
      (is (= path (:path path-node)))
      (is (= 81 (:specificity path-node)))
      (doseq [reaction [render-reaction subscription]] (ratom/dispose! reaction)))
    (rf/clear-sub ::deep)))

(deftest causal-graph-has-no-former-300-node-cap
  (gobj/set js/globalThis "React" #js {:version "18.3.1"})
  (let [path-count 305]
    (reset! db/app-db {:values (vec (range path-count))})
    (rf/reg-sub ::many-values (fn [db _] (mapv identity (:values db))))
    (let [subscription (rf/subscribe [::many-values])
          render-reaction (capturing-reaction #(let [values @subscription]
                                                 [:article#selected.card (pr-str values)]))
          snapshot (graph/snapshot (mock-element render-reaction))]
      (is (= path-count (count (nodes-of-kind snapshot :app-db-path))))
      (is (> (count (:nodes snapshot)) 300))
      (is (false? (:truncated? snapshot)))
      (doseq [reaction [render-reaction subscription]] (ratom/dispose! reaction)))
    (rf/clear-sub ::many-values)))
