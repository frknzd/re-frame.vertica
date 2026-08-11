(ns re-frame-inspector.shared-test
  (:require [clojure.test :refer [deftest is testing]]
            [re-frame-inspector.shared :as shared]))

(deftest leaf-path-selection
  (is (= [[:people 1 :name]]
         (shared/leaf-paths [[:people] [:people 1] [:people 1 :name]])))
  (is (= [[:people shared/wildcard]]
         (shared/leaf-paths [[:people] [:people shared/wildcard]]))))

(deftest deterministic-ordering
  (let [{:keys [nodes edges]}
        (shared/deterministic-graph
          [{:id "c" :kind :component :label "C"}
           {:id "p" :kind :app-db-path :label "P"}
           {:id "s" :kind :subscription :label "S"}]
          [{:from "s" :to "c" :kind :render-input}
           {:from "p" :to "s" :kind :data-input}])]
    (is (= ["p" "s" "c"] (mapv :id nodes)))
    (is (= ["p" "s"] (mapv :from edges)))))

(deftest bounded-values
  (is (= "123" (shared/bounded-preview 123 10)))
  (is (= 10 (count (shared/bounded-preview (apply str (repeat 30 "x")) 10)))))

