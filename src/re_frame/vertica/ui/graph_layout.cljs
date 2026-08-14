(ns re-frame.vertica.ui.graph-layout
  (:require [re-frame.vertica.shared :as shared]))

(def section-order [:app-db-path :subscription :prop :component :element])

(def section-meta
  {:app-db-path {:title "APP-DB" :identity "PATH" :value "VALUE"}
   :subscription {:title "SUBSCRIPTIONS" :identity "QUERY" :value "VALUE"}
   :prop {:title "PROPS" :identity "ARGUMENT" :value "VALUE"}
   :component {:title "REAGENT COMPONENTS" :identity "COMPONENT"}
   :element {:title "SELECTED ELEMENT" :identity "ELEMENT"}})

(defn compare-nodes [a b]
  (let [label-order (compare (str (:label a)) (str (:label b)))]
    (if (zero? label-order)
      (compare (str (:id a)) (str (:id b)))
      label-order)))

(defn- compare-paths [a b]
  (let [specificity-order (compare (or (:specificity b) 0)
                                   (or (:specificity a) 0))]
    (if (zero? specificity-order) (compare-nodes a b) specificity-order)))

(defn group-db-vector-entries [entries]
  (reduce (fn [groups entry]
            (let [layout (if (= :scalar (get-in entry [:node :kind])) :compact :tree)]
              (if (= layout (:layout (peek groups)))
                (update-in groups [(dec (count groups)) :entries] conj entry)
                (conj groups {:layout layout :entries [entry]}))))
          []
          (or entries [])))

(defn db-collection-starts-collapsed?
  ([node] (db-collection-starts-collapsed? node 5))
  ([node threshold]
   (and (contains? #{:map :vector :set} (:kind node))
        (true? (:all-children? node))
        (> (or (:child-count node) 0) threshold))))

(defn app-db-path-matches? [actual pattern]
  (and (vector? actual)
       (vector? pattern)
       (= (count actual) (count pattern))
       (every? true?
               (map (fn [actual-segment pattern-segment]
                      (or (= shared/association-wildcard pattern-segment)
                          (= (str actual-segment) (str pattern-segment))))
                    actual pattern))))

(defn- assign-depths [nodes children initial-queue]
  (let [by-id (into {} (map (juxt :id identity)) nodes)
        sorted-nodes (sort compare-nodes nodes)]
    (loop [queue (vec initial-queue)
           index 0
           depths {}]
      (if (< index (count queue))
        (let [[id depth] (nth queue index)
              current (get depths id)]
          (if (and (some? current) (<= current depth))
            (recur queue (inc index) depths)
            (let [next-nodes (->> (get children id [])
                                  (keep by-id)
                                  (sort compare-nodes))]
              (recur (into queue (map #(vector (:id %) (inc depth))) next-nodes)
                     (inc index)
                     (assoc depths id depth)))))
        (if-let [unvisited (first (remove #(contains? depths (:id %)) sorted-nodes))]
          (recur (conj queue [(:id unvisited) 0]) index depths)
          depths)))))

(defn- hierarchical-nodes [nodes edges reverse-flow?]
  (if (< (count nodes) 2)
    (mapv #(assoc % :depth 0) nodes)
    (let [ids (set (map :id nodes))
          initial (zipmap ids (repeat []))
          {:keys [children incoming]}
          (reduce (fn [{:keys [children incoming] :as state} edge]
                    (let [from (:from edge)
                          to (:to edge)]
                      (if (or (not (contains? ids from))
                              (not (contains? ids to))
                              (= from to))
                        state
                        (let [parent (if reverse-flow? to from)
                              child (if reverse-flow? from to)]
                          {:children (update children parent conj child)
                           :incoming (update incoming child inc)}))))
                  {:children initial :incoming (zipmap ids (repeat 0))}
                  edges)
          roots (sort compare-nodes (filter #(zero? (get incoming (:id %))) nodes))
          depths (assign-depths nodes children (map #(vector (:id %) 0) roots))]
      (->> nodes
           (map #(assoc % :depth (get depths (:id %) 0)))
           (sort (fn [a b]
                   (let [depth-order (compare (:depth a) (:depth b))]
                     (if (zero? depth-order) (compare-nodes a b) depth-order))))
           vec))))

(defn- group-levels [nodes]
  (->> nodes
       (group-by :depth)
       (sort-by key)
       (mapv (fn [[level level-nodes]] {:level level :nodes level-nodes}))))

(defn build-component-associations [nodes edges]
  (let [by-id (into {} (map (juxt :id identity)) nodes)
        upstream (atom (zipmap (keys by-id) (repeat [])))
        direct-inputs (atom {})
        component-children (atom (zipmap (map :id (filter #(= :component (:kind %)) nodes))
                                         (repeat [])))]
    (doseq [edge edges
            :when (and (contains? by-id (:from edge))
                       (contains? by-id (:to edge)))]
      (when (= :data-input (:kind edge))
        (swap! upstream update (:to edge) conj (:from edge)))
      (when (and (= :render-input (:kind edge))
                 (= :component (:kind (get by-id (:to edge)))))
        (swap! direct-inputs update (:to edge) (fnil conj []) (:from edge)))
      (when (and (= :render-ownership (:kind edge))
                 (= :component (:kind (get by-id (:from edge))))
                 (= :component (:kind (get by-id (:to edge)))))
        (swap! component-children update (:from edge) conj (:to edge))))
    (let [leaf-ranks (atom {})]
      (letfn [(leaf-rank [id visiting]
                (if-let [rank (get @leaf-ranks id)]
                  rank
                  (if (contains? visiting id)
                    0
                    (let [children (get @component-children id [])
                          rank (if (seq children)
                                 (inc (apply max (map #(leaf-rank % (conj visiting id)) children)))
                                 0)]
                      (swap! leaf-ranks assoc id rank)
                      rank))))]
        (doseq [id (keys @component-children)] (leaf-rank id #{}))
        (let [associations
              (reduce
                (fn [result component]
                  (loop [queue (vec (get @direct-inputs (:id component) []))
                         index 0
                         visited #{}
                         result result]
                    (if (< index (count queue))
                      (let [id (nth queue index)]
                        (if (contains? visited id)
                          (recur queue (inc index) visited result)
                          (recur (into queue (get @upstream id []))
                                 (inc index)
                                 (conj visited id)
                                 (update result id (fnil conj #{}) (:id component)))))
                      result)))
                {}
                (filter #(= :component (:kind %)) nodes))]
          (into {}
                (map (fn [[id component-ids]]
                       [id (->> component-ids
                                (keep by-id)
                                (sort (fn [a b]
                                        (let [rank-order (compare (get @leaf-ranks (:id a) 0)
                                                                  (get @leaf-ranks (:id b) 0))]
                                          (if (zero? rank-order) (compare-nodes a b) rank-order))))
                                vec)]))
                associations))))))

(defn leaf-most-components [components edges]
  (let [by-id (into {} (map (juxt :id identity)) components)
        children (reduce (fn [result edge]
                           (if (= :render-ownership (:kind edge))
                             (update result (:from edge) (fnil conj []) (:to edge))
                             result))
                         {}
                         edges)
        associated-descendant?
        (fn [id]
          (loop [queue (vec (get children id [])) index 0 visited #{}]
            (if (< index (count queue))
              (let [child-id (nth queue index)]
                (cond
                  (contains? visited child-id) (recur queue (inc index) visited)
                  (contains? by-id child-id) true
                  :else (recur (into queue (get children child-id []))
                               (inc index)
                               (conj visited child-id))))
              false)))]
    (->> (vals by-id)
         (remove #(associated-descendant? (:id %)))
         (sort compare-nodes)
         vec)))

(defn build-sections
  ([nodes] (build-sections nodes []))
  ([nodes edges]
   (let [layers (group-by :kind nodes)
         kind-by-id (into {} (map (juxt :id :kind)) nodes)]
     (mapv
       (fn [kind]
         (let [path-layer? (= :app-db-path kind)
               layer (vec (sort (if path-layer? compare-paths compare-nodes)
                                (get layers kind [])))
               same-kind-edges (filterv #(and (= kind (get kind-by-id (:from %)))
                                              (= kind (get kind-by-id (:to %))))
                                        edges)
               ordered (if path-layer?
                         (mapv #(assoc % :depth 0) layer)
                         (hierarchical-nodes layer same-kind-edges
                                             (contains? #{:subscription :component} kind)))]
           (merge {:kind kind
                   :nodes ordered
                   :levels (when (= :subscription kind) (group-levels ordered))}
                  (get section-meta kind))))
       section-order))))
