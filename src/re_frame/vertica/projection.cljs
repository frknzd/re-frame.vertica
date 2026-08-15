(ns re-frame.vertica.projection
  (:require [re-frame.vertica.shared :as shared]))

(def ^:private page-size 10)
(def ^:dynamic *expansions* {})

(defn- expansion-limit [path]
  (max 0 (get *expansions* path 0)))

(defn- expanded? [path]
  (pos? (expansion-limit path)))

(defn- path-prefix? [actual pattern]
  (and (<= (count actual) (count pattern))
       (every? true?
               (map (fn [actual-segment pattern-segment]
                      (or (= shared/wildcard pattern-segment)
                          (= actual-segment pattern-segment)))
                    actual pattern))))

(defn- path-matches? [actual pattern]
  (and (= (count actual) (count pattern))
       (path-prefix? actual pattern)))

(defn- relevant-paths [paths path]
  (filterv #(path-prefix? path %) paths))

(defn- collection-summary [value]
  (cond
    (map? value) "{…}"
    (vector? value) "[…]"
    (set? value) "#{…}"
    (sequential? value) "(…)"
    :else "…"))

(defn- base-node [path touched? exact?]
  {:path path
   :association-path (shared/association-path path)
   :path-label (shared/path-label path)
   :touched? touched?
   :exact? exact?})

(defn- scalar-node [value path touched? exact?]
  (let [preview (shared/value-preview value 240)]
    (assoc (base-node path touched? exact?)
           :kind :scalar
           :text (:text preview)
           :preview-truncated? (:truncated? preview))))

(defn- exact-collection-node [value path]
  (assoc (base-node path true true)
         :kind :summary
         :text (collection-summary value)))

(defn- context-collection-node [value path]
  (assoc (base-node path false false)
         :kind :summary
         :text (collection-summary value)))

(defn- ellipsis-node [path omitted visible-count]
  (assoc (base-node path false false)
         :kind :ellipsis
         :text (str "… " omitted " more")
         :visible-count visible-count))

(declare project-node)

(defn- next-segments [relevant path]
  (->> relevant
       (keep #(when (> (count %) (count path)) (nth % (count path))))
       distinct vec))

(defn- all-children-involved? [value segments]
  (boolean
    (if (some #{shared/wildcard} segments)
      true
      (let [segments (set segments)]
        (and (= (count value) (count segments))
             (every? #(contains? segments %)
                     (cond
                       (map? value) (keys value)
                       (vector? value) (range (count value))
                       (set? value) value
                       :else [])))))))

(defn- collection-node [base kind open close value children segments]
  (assoc base
         :kind kind
         :open open
         :close close
         :child-count (count value)
         :all-children? (all-children-involved? value segments)
         :children children))

(defn- paged-items [items total path]
  (let [limit (max page-size (expansion-limit path))
        visible (vec (take limit items))]
    {:visible visible :omitted (- total (count visible))}))

(defn- child-node [child child-path paths touched?]
  (cond
    (or touched? (expanded? child-path)) (project-node child child-path paths)
    (coll? child) (context-collection-node child child-path)
    :else (scalar-node child child-path false false)))

(defn- child-entry [key child path paths touched?]
  {:key (pr-str key)
   :node (child-node child (conj path key) paths touched?)})

(defn- append-ellipsis [entries path omitted]
  (cond-> entries
    (pos? omitted) (conj {:key nil :node (ellipsis-node path omitted (count entries))})))

(defn- map-children [value path relevant]
  (let [segments (next-segments relevant path)
        wildcard? (boolean (some #{shared/wildcard} segments))
        all? (all-children-involved? value segments)
        paged? (or all? (and (empty? segments) (expanded? path)))
        requested (if paged? (keys value) (remove #{shared/wildcard} segments))
        {:keys [visible omitted]} (if paged?
                                    (paged-items requested (count value) path)
                                    {:visible (vec requested) :omitted 0})
        touched (set (remove #{shared/wildcard} segments))
        entries (mapv #(child-entry % (get value %) path relevant
                                    (or wildcard? (contains? touched %)))
                      visible)]
    (append-ellipsis entries path omitted)))

(defn- vector-children [value path relevant]
  (let [segments (next-segments relevant path)
        wildcard? (boolean (some #{shared/wildcard} segments))
        all? (all-children-involved? value segments)
        paged? (or all? (and (empty? segments) (expanded? path)))
        requested (if paged? (range (count value)) (remove #{shared/wildcard} segments))
        {:keys [visible omitted]} (if paged?
                                    (paged-items requested (count value) path)
                                    {:visible (vec requested) :omitted 0})
        touched (set (remove #{shared/wildcard} segments))
        entries (mapv #(child-entry % (when (and (integer? %) (<= 0 %) (< % (count value)))
                                         (nth value %))
                                      path relevant (or wildcard? (contains? touched %)))
                      visible)]
    (append-ellipsis entries path omitted)))

(defn- set-children [value path relevant]
  (let [segments (next-segments relevant path)
        wildcard? (boolean (some #{shared/wildcard} segments))
        all? (all-children-involved? value segments)
        paged? (or all? (and (empty? segments) (expanded? path)))
        requested (if paged? value (remove #{shared/wildcard} segments))
        {:keys [visible omitted]} (if paged?
                                    (paged-items requested (count value) path)
                                    {:visible (vec requested) :omitted 0})
        touched (set (remove #{shared/wildcard} segments))
        entries
        (mapv (fn [item]
                (let [present? (contains? value item)
                      child (when present? item)
                      child-path (conj path item)]
                  {:key (when-not present? (pr-str item))
                   :node (child-node child child-path relevant
                                     (or wildcard? (contains? touched item)))}))
              visible)]
    (append-ellipsis entries path omitted)))

(defn project-node [value path paths]
  (let [relevant (relevant-paths paths path)
        exact? (boolean (some #(path-matches? path %) relevant))
        descendant? (boolean (some #(> (count %) (count path)) relevant))
        base (base-node path (boolean (seq relevant)) exact?)
        segments (next-segments relevant path)]
    (cond
      (and exact? (coll? value) (not descendant?) (not (expanded? path)))
      (exact-collection-node value path)
      (map? value) (collection-node base :map "{" "}" value
                                    (map-children value path relevant) segments)
      (vector? value) (collection-node base :vector "[" "]" value
                                       (vector-children value path relevant) segments)
      (set? value) (collection-node base :set "#{" "}" value
                                    (set-children value path relevant) segments)
      :else (scalar-node value path (boolean (seq relevant)) exact?))))

(defn app-db-tree
  ([app-db paths] (app-db-tree app-db paths {}))
  ([app-db paths expansions]
   (binding [*expansions* expansions]
     (project-node app-db [] (vec paths)))))

(defn app-db-branch [app-db paths path expansions]
  (binding [*expansions* expansions]
    (project-node (get-in app-db path) path (vec paths))))
