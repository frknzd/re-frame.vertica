(ns re-frame.vertica.shared-test
  (:require [clojure.test :refer [deftest is testing]]
            [re-frame.vertica.shared :as shared]))

(deftest leaf-path-selection
  (is (= [[:people 1 :name]]
         (shared/leaf-paths [[:people] [:people 1] [:people 1 :name]])))
  (is (= [[:people shared/wildcard]]
         (shared/leaf-paths [[:people] [:people shared/wildcard]]))))

(deftest association-paths-preserve-identities-and-wildcards
  (is (= [":one/name" ":two/name" "\"K660\"" "0"
          shared/association-wildcard]
         (shared/association-path
           [:one/name :two/name "K660" 0 shared/wildcard]))))

(deftest leaf-path-selection-scales-to-large-read-sets
  (let [leaves (mapv #(vector :rows % :value) (range 5000))
        paths (into [[:rows]] (mapcat (fn [leaf] [(pop leaf) leaf]) leaves))]
    (is (= 5000 (count (shared/leaf-paths paths))))
    (is (= [:rows 0 :value] (first (shared/leaf-paths paths))))))

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
  (is (= 10 (count (shared/bounded-preview (apply str (repeat 30 "x")) 10))))
  (is (= {:text "\"xxxxxxxx…" :truncated? true}
         (shared/value-preview (apply str (repeat 30 "x")) 10)))
  (is (= "\"complete\"" (shared/value-string "complete"))))

(deftest formats-nested-values
  (let [formatted (shared/value-string
                    {:patient {:identifiers (vec (range 20))
                               :active true}})]
    (is (re-find #"\n" formatted))
    (is (re-find #":identifiers" formatted))
    (is (re-find #":active true" formatted))))

(deftest preview-work-is-bounded
  (let [realized (atom 0)
        large-value (map (fn [value] (swap! realized inc) value) (range 100000))
        preview (shared/value-preview large-value 1200)]
    (is (:truncated? preview))
    (is (< @realized 100))
    (is (< (count (:text preview)) 1200))))

(deftest nested-preview-shows-values-instead-of-depth-markers
  (let [value {:ai.ibis.mzg2.shared.db/user-settings
               #{{:id 1 :settings {:theme :dark :density :compact}}
                 {:id 2 :settings {:theme :light :density :comfortable}}
                 {:id 3 :settings {:theme :system :density :compact}}}}
        preview (shared/value-preview value 1200)]
    (is (re-find #":theme" (:text preview)))
    (is (re-find #":density" (:text preview)))
    (is (not (re-find #"[\[{, ]#[\]}, ]" (:text preview))))))
