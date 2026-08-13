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
