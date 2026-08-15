(ns re-frame.vertica.causal-test
  (:require [cljs.test :refer-macros [deftest is]]
            [react :as react]
            [re-frame.vertica.causal :as causal]))

(deftest normalizes-visible-react-output-without-callback-identities
  (let [output (react/createElement
                 "article"
                 #js {:id "selected"
                      :className "card"
                      :style #js {:color "red" :display "block"}
                      :onClick (fn [])}
                 "Hello"
                 (react/createElement "strong" nil "Ada"))
        node (first (causal/normalize-nodes output))]
    (is (= :element (:kind node)))
    (is (= "article" (:tag node)))
    (is (= "red" (get-in node [:attrs "style" "color"])))
    (is (not (contains? (:attrs node) "onClick")))
    (is (= [:text :element] (mapv :kind (:children node))))))

(deftest fragments-normalize-to-their-visible-host-roots
  (let [output (react/createElement
                 react/Fragment nil
                 (react/createElement "span" #js {:id "one"} "One")
                 (react/createElement "span" #js {:id "two"} "Two"))]
    (is (= ["one" "two"]
           (mapv #(get-in % [:attrs "id"]) (causal/normalize-nodes output))))))

(deftest root-counterfactuals-can-use-false
  (let [current (atom true)
        replay-count (atom 0)
        render-fn #(react/createElement "div" #js {:id "root"} (str @current))
        baseline (first (causal/normalize-nodes (render-fn)))
        classifications
        (causal/classify-paths
          {:app-db true
           :paths [[]]
           :root-reaction (atom true)
           :prop-sources []
           :oracle {:render-fn render-fn
                    :route {:root {:tag "div" :id "root" :classes ""}
                            :indices []}
                    :baseline baseline}
           :replay (fn [counterfactual-db]
                     (swap! replay-count inc)
                     (reset! current counterfactual-db)
                     {:value counterfactual-db
                      :values {}
                      :reactions {}
                      :complete? true})})]
    (is (= :confirmed (get-in classifications [[] :status])))
    (is (= 2 @replay-count))))
