(ns re-frame.vertica.ui.graph-layout-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.ui.graph-layout :as layout]))

(def nodes
  [{:id "component-child" :kind :component :label "Child"}
   {:id "path-b" :kind :app-db-path :label "[:profile :name]" :specificity 2 :preview "Ada"}
   {:id "element" :kind :element :label "article#selected"}
   {:id "subscription-child" :kind :subscription :label "[:name]"}
   {:id "path-a" :kind :app-db-path :label "[:profile]" :specificity 1 :preview "{:name Ada}"}
   {:id "subscription-parent" :kind :subscription :label "[:profile]"}
   {:id "component-parent" :kind :component :label "Parent"}])

(def edges
  [{:from "subscription-parent" :to "subscription-child" :kind :data-input}
   {:from "component-parent" :to "component-child" :kind :render-ownership}])

(deftest sections-stay-ordered
  (let [sections (layout/build-sections nodes edges)]
    (is (= layout/section-order (mapv :kind sections)))
    (is (= "APP-DB" (:title (first sections))))
    (is (= "VALUE" (:value (first sections))))
    (is (nil? (:value (first (filter #(= :component (:kind %)) sections)))))))

(deftest hierarchies-flow-from-results-to-inputs
  (let [sections (layout/build-sections nodes edges)
        subscriptions (first (filter #(= :subscription (:kind %)) sections))
        components (first (filter #(= :component (:kind %)) sections))]
    (is (= [["subscription-child" 0] ["subscription-parent" 1]]
           (mapv (juxt :id :depth) (:nodes subscriptions))))
    (is (= [["component-child" 0] ["component-parent" 1]]
           (mapv (juxt :id :depth) (:nodes components))))
    (is (= [[0 ["subscription-child"]] [1 ["subscription-parent"]]]
           (mapv (fn [{:keys [level nodes]}] [level (mapv :id nodes)])
                 (:levels subscriptions))))))

(deftest shared-input-uses-shortest-distance
  (let [section (->> (layout/build-sections
                       [{:id "result-a" :kind :subscription :label "a"}
                        {:id "result-b" :kind :subscription :label "b"}
                        {:id "middle" :kind :subscription :label "middle"}
                        {:id "shared" :kind :subscription :label "shared"}]
                       [{:from "shared" :to "result-a"}
                        {:from "shared" :to "middle"}
                        {:from "middle" :to "result-b"}])
                     (filter #(= :subscription (:kind %))) first)]
    (is (= 1 (:depth (first (filter #(= "shared" (:id %)) (:nodes section))))))))

(deftest paths-are-most-specific-first
  (is (= ["path-b" "path-a"]
         (->> (layout/build-sections nodes edges)
              (filter #(= :app-db-path (:kind %))) first :nodes (mapv :id)))))

(deftest empty-graph-has-all-sections
  (let [sections (layout/build-sections [])]
    (is (= (count layout/section-order) (count sections)))
    (is (every? #(empty? (:nodes %)) sections))))

(deftest vector-entry-layout-and-collection-collapse
  (let [entries [{:key "0" :node {:kind :scalar}}
                 {:key "1" :node {:kind :scalar}}
                 {:key "2" :node {:kind :map}}
                 {:key "3" :node {:kind :vector}}
                 {:key "4" :node {:kind :scalar}}
                 {:key "5" :node {:kind :summary}}]]
    (is (= [[:compact ["0" "1"]] [:tree ["2" "3"]]
            [:compact ["4"]] [:tree ["5"]]]
           (mapv (fn [{:keys [layout entries]}] [layout (mapv :key entries)])
                 (layout/group-db-vector-entries entries))))
    (is (layout/db-collection-starts-collapsed?
          {:kind :map :child-count 6 :all-children? true}))
    (is (not (layout/db-collection-starts-collapsed?
               {:kind :vector :child-count 5 :all-children? true})))
    (is (not (layout/db-collection-starts-collapsed?
               {:kind :set :child-count 100 :all-children? false})))))

(deftest component-associations-and-leaf-badges
  (let [association-nodes [{:id "path" :kind :app-db-path :label "path"}
                           {:id "shared" :kind :subscription :label "shared"}
                           {:id "name" :kind :subscription :label "name"}
                           {:id "card" :kind :component :label "Card"}
                           {:id "header" :kind :component :label "Header"}]
        association-edges [{:from "path" :to "shared" :kind :data-input}
                           {:from "shared" :to "name" :kind :data-input}
                           {:from "name" :to "card" :kind :render-input}
                           {:from "shared" :to "header" :kind :render-input}
                           {:from "header" :to "card" :kind :render-ownership}]
        associations (layout/build-component-associations association-nodes association-edges)]
    (is (= ["card" "header"] (mapv :id (get associations "path"))))
    (is (= ["card"] (mapv :id (get associations "name"))))
    (is (not (contains? associations "card")))
    (is (= ["card"]
           (mapv :id (layout/leaf-most-components
                       (filterv #(contains? #{"header" "card"} (:id %)) association-nodes)
                       association-edges))))))

(deftest wildcard-paths-match-concrete-paths
  (is (layout/app-db-path-matches?
        [":people" "42" ":name"]
        [":people" shared/association-wildcard ":name"]))
  (is (not (layout/app-db-path-matches? [":people" "42"] [":people" "7"])))
  (is (not (layout/app-db-path-matches?
             [":people" ":one/name"] [":people" ":two/name"]))))
