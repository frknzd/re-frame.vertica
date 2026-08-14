(ns re-frame.vertica.projection-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.projection :as projection]))

(defn- child [node key-label]
  (:node (first (filter #(= key-label (:key %)) (:children node)))))

(deftest touched-tree-keeps-only-contributing-path-and-ancestors
  (let [db {:screen {:panel {:mode :coding :large (vec (range 100))}
                     :neighbor-a 1 :neighbor-b 2 :neighbor-c 3}
            :untouched {:huge (range 1000)}}
        db (assoc-in db [:screen :panel :spare-a] 1)
        db (assoc-in db [:screen :panel :spare-b] 2)
        tree (projection/app-db-tree db [[:screen :panel :mode]])
        screen (child tree ":screen")
        panel (child screen ":panel")
        mode (child panel ":mode")
        untouched (child tree ":untouched")]
    (is (:touched? screen))
    (is (:touched? panel))
    (is (:exact? mode))
    (is (= ":coding" (:text mode)))
    (is (nil? untouched))
    (is (= [":mode"] (mapv :key (:children panel))))))

(deftest vector-projection-excludes-neighbors
  (let [tree (projection/app-db-tree {:items (vec (range 20))} [[:items 10]])
        items (child tree ":items")
        entries (remove #(nil? (:key %)) (:children items))]
    (is (= [10] (mapv #(js/parseInt (:key %) 10) entries)))
    (is (= "[:items 10]" (get-in (first entries) [:node :path-label])))
    (is (:exact? (:node (first entries))))))

(deftest collections-report-when-every-direct-child-is-involved
  (let [values (vec (range 6))
        all-paths (mapv #(vector :items %) (range 6))
        all-items (child (projection/app-db-tree {:items values} all-paths) ":items")
        partial-items (child (projection/app-db-tree {:items values} [[:items 0] [:items 1]])
                             ":items")]
    (is (= 6 (:child-count all-items)))
    (is (true? (:all-children? all-items)))
    (is (= 6 (count (:children all-items))))
    (is (false? (:all-children? partial-items)))
    (is (= 2 (count (:children partial-items))))))

(deftest wildcard-traversal-involves-every-direct-map-child
  (let [tree (projection/app-db-tree {:scores {"a" 1 "b" 2 "c" 3 "d" 4 "e" 5 "f" 6}}
                                     [[:scores :re-frame.vertica/all]])
        scores (child tree ":scores")]
    (is (= 6 (:child-count scores)))
    (is (true? (:all-children? scores)))
    (is (= 6 (count (:children scores))))))

(deftest exact-collections-are-leaves-not-context-expansions
  (let [tree (projection/app-db-tree {:items (vec (range 20))} [[:items]])
        items (child tree ":items")]
    (is (= :summary (:kind items)))
    (is (= "[…]" (:text items)))
    (is (:exact? items))))

(deftest exact-collections-load-in-pages-and-can-be-expanded-recursively
  (let [db {:items [{:rows (vec (range 25))}]}
        paths [[:items]]
        initial (child (projection/app-db-tree db paths) ":items")
        items (projection/app-db-branch db paths [:items] {[:items] 10})
        first-item (child items "0")
        item (projection/app-db-branch db paths [:items 0]
                                       {[:items] 10 [:items 0] 10})
        rows-summary (child item ":rows")
        rows (projection/app-db-branch db paths [:items 0 :rows]
                                      {[:items] 10 [:items 0] 10 [:items 0 :rows] 10})
        more (last (:children rows))]
    (is (= :summary (:kind initial)))
    (is (= :vector (:kind items)))
    (is (= :summary (:kind first-item)))
    (is (= :map (:kind item)))
    (is (= :summary (:kind rows-summary)))
    (is (= :vector (:kind rows)))
    (is (= (mapv str (range 10)) (mapv :key (butlast (:children rows)))))
    (is (= :ellipsis (get-in more [:node :kind])))
    (is (= 10 (get-in more [:node :visible-count])))
    (is (= "… 15 more" (get-in more [:node :text])))))

(deftest all-entry-traversals-page-without-losing-coverage-metadata
  (let [values (vec (range 25))
        paths (mapv #(vector :items %) (range 25))
        items (child (projection/app-db-tree {:items values} paths) ":items")
        first-page (:children items)
        expanded (projection/app-db-branch {:items values} paths [:items] {[:items] 20})]
    (is (true? (:all-children? items)))
    (is (= 25 (:child-count items)))
    (is (= 11 (count first-page)))
    (is (= :ellipsis (get-in (last first-page) [:node :kind])))
    (is (= 21 (count (:children expanded))))
    (is (= "… 5 more" (get-in (last (:children expanded)) [:node :text])))))

(deftest missing-causal-paths-remain-visible
  (let [tree (projection/app-db-tree {:items [] :flags #{}} [[:missing] [:items 4] [:flags :admin]])
        missing (child tree ":missing")
        item (child (child tree ":items") "4")
        flag (child (child tree ":flags") ":admin")]
    (is (:exact? missing))
    (is (= "nil" (:text missing)))
    (is (:exact? item))
    (is (:exact? flag))))

(deftest set-membership-keeps-the-touched-value
  (let [tree (projection/app-db-tree {:tags #{:a :b :c :d :e :f}} [[:tags :f]])
        tags (child tree ":tags")
        touched (some #(when (get-in % [:node :exact?]) (:node %)) (:children tags))]
    (is (= ":f" (:text touched)))
    (is (= "[:tags :f]" (:path-label touched)))))

(deftest projected-nodes-retain-raw-paths-for-component-association
  (let [tree (projection/app-db-tree {:items [{:name "Ada"}]} [[:items 0 :name]])
        items (child tree ":items")
        first-item (child items "0")
        name-node (child first-item ":name")]
    (is (= [] (:path tree)))
    (is (= [:items] (:path items)))
    (is (= [:items 0 :name] (:path name-node)))
    (is (= [":items" "0" ":name"] (:association-path name-node)))))

(deftest long-scalars-advertise-expandable-previews
  (let [long-value (apply str (repeat 400 "x"))
        tree (projection/app-db-tree {:long long-value} [[:long]])
        long-node (child tree ":long")]
    (is (:preview-truncated? long-node))
    (is (= 240 (count (:text long-node))))))
