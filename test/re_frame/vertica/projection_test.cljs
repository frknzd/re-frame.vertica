(ns re-frame.vertica.projection-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.projection :as projection]))

(defn- child [node key-label]
  (:node (first (filter #(= key-label (:key %)) (:children node)))))

(deftest touched-tree-keeps-path-and-bounds-context
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
    (is (= "{…}" (:text untouched)))
    (is (some #(= "ellipsis" (get-in % [:node :kind])) (:children panel)))))

(deftest vector-context-includes-neighbors-and-paths
  (let [tree (projection/app-db-tree {:items (vec (range 20))} [[:items 10]])
        items (child tree ":items")
        entries (remove #(nil? (:key %)) (:children items))]
    (is (= [8 9 10 11 12] (mapv #(js/parseInt (:key %) 10) entries)))
    (is (= "[:items 10]" (get-in (nth entries 2) [:node :path-label])))
    (is (:exact? (:node (nth entries 2))))))

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
