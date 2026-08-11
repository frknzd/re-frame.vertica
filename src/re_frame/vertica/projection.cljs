(ns re-frame.vertica.projection
  (:require [re-frame.vertica.shared :as shared]))

(def ^:private context-siblings 2)
(def ^:private exact-context-items 4)
(def ^:dynamic *context-limits* {})

(defn- context-limit [path default-limit]
  (max default-limit (get *context-limits* path 0)))

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
  (filter #(path-prefix? path %) paths))

(defn- collection-summary [value]
  (cond
    (map? value) "{…}"
    (vector? value) "[…]"
    (set? value) "#{…}"
    (sequential? value) "(…)"
    :else "…"))

(defn- scalar-node [value path touched? exact?]
  (let [preview (shared/value-preview value 240)]
    {:kind "scalar"
     :text (:text preview)
     :preview-truncated? (:truncated? preview)
     :path path
     :association-path (shared/association-path path)
     :path-label (shared/path-label path)
     :touched? touched?
     :exact? exact?}))

(defn- summary-node [value path]
  {:kind "summary"
   :text (collection-summary value)
   :path path
   :association-path (shared/association-path path)
   :path-label (shared/path-label path)
   :touched? false})

(defn- ellipsis-node [path omitted]
  {:kind "ellipsis"
   :text (if (pos? omitted) (str "… " omitted " more") "…")
   :path path
   :association-path (shared/association-path path)
   :path-label (shared/path-label path)
   :touched? false})

(declare project-node)

(defn- entry [key value child-path paths touched?]
  {:key (pr-str key)
   :node (if touched?
           (project-node value child-path paths)
           (if (coll? value)
             (summary-node value child-path)
             (scalar-node value child-path false false)))})

(defn- map-children [value path paths exact?]
  (let [relevant (vec (relevant-paths paths path))
        next-segments (->> relevant
                           (keep #(when (> (count %) (count path))
                                    (nth % (count path))))
                           distinct vec)
        wildcard? (some #{shared/wildcard} next-segments)
        touched-keys (if wildcard?
                       (vec (take exact-context-items (keys value)))
                       (->> next-segments (remove #{shared/wildcard}) (filter #(contains? value %)) vec))
        touched-set (set touched-keys)
        visible-context (context-limit path (if (or exact? wildcard?)
                                              exact-context-items
                                              context-siblings))
        context-entries (->> value (remove #(contains? touched-set (key %))) (take visible-context) vec)
        touched-entries (mapv (fn [key] [key (get value key)]) touched-keys)
        entries (concat touched-entries context-entries)
        rendered (mapv (fn [[key child]]
                         (entry key child (conj path key) paths
                                (or wildcard? (contains? touched-set key))))
                       entries)
        omitted (- (count value) (count entries))]
    (cond-> rendered
      (pos? omitted) (conj {:key nil :node (ellipsis-node path omitted)}))))

(defn- vector-indexes [value path paths exact?]
  (let [relevant (vec (relevant-paths paths path))
        next-segments (->> relevant
                           (keep #(when (> (count %) (count path))
                                    (nth % (count path))))
                           distinct vec)
        wildcard? (some #{shared/wildcard} next-segments)
        touched (if wildcard?
                  (range (min exact-context-items (count value)))
                  (filter #(and (integer? %) (< -1 % (count value))) next-segments))
        visible-context (context-limit path (if (or exact? wildcard?)
                                              exact-context-items
                                              context-siblings))
        context (if (or exact? wildcard?)
                  (range (min visible-context (count value)))
                  (mapcat #(range (max 0 (- % visible-context))
                                  (min (count value) (+ % visible-context 1))) touched))]
    {:indexes (vec (sort (distinct (concat touched context))))
     :touched (set touched)
     :wildcard? wildcard?}))

(defn- vector-children [value path paths exact?]
  (let [{:keys [indexes touched wildcard?]} (vector-indexes value path paths exact?)
        entries (mapv (fn [index]
                        (entry index (nth value index) (conj path index) paths
                               (or wildcard? (contains? touched index))))
                      indexes)
        omitted (- (count value) (count indexes))]
    (cond-> entries
      (pos? omitted) (conj {:key nil :node (ellipsis-node path omitted)}))))

(defn- set-children [value path paths exact?]
  (let [relevant (vec (relevant-paths paths path))
        next-segments (->> relevant
                           (keep #(when (> (count %) (count path))
                                    (nth % (count path))))
                           distinct vec)
        wildcard? (some #{shared/wildcard} next-segments)
        touched-items (if wildcard?
                        (vec (take exact-context-items value))
                        (vec (filter #(contains? value %) next-segments)))
        touched-set (set touched-items)
        visible-context (context-limit path (if (or exact? wildcard?)
                                              exact-context-items
                                              context-siblings))
        context-items (->> value (remove touched-set) (take visible-context) vec)
        items (concat touched-items context-items)
        entries (mapv (fn [item]
                        {:key nil
                         :node (if (or wildcard? (contains? touched-set item))
                                 (project-node item (conj path item) paths)
                                 (if (coll? item)
                                   (summary-node item (conj path item))
                                   (scalar-node item (conj path item) false false)))})
                      items)
        omitted (- (count value) (count items))]
    (cond-> entries
      (pos? omitted) (conj {:key nil :node (ellipsis-node path omitted)}))))

(defn project-node [value path paths]
  (let [relevant (vec (relevant-paths paths path))
        exact? (boolean (some #(path-matches? path %) relevant))
        touched? (boolean (seq relevant))
        base {:path-label (shared/path-label path)
              :path path
              :association-path (shared/association-path path)
              :touched? touched?
              :exact? exact?}]
    (cond
      (map? value) (assoc base :kind "map" :open "{" :close "}"
                          :children (map-children value path paths exact?))
      (vector? value) (assoc base :kind "vector" :open "[" :close "]"
                             :children (vector-children value path paths exact?))
      (set? value) (assoc base :kind "set" :open "#{" :close "}"
                          :children (set-children value path paths exact?))
      :else (scalar-node value path touched? exact?))))

(defn app-db-tree
  ([app-db paths] (app-db-tree app-db paths {}))
  ([app-db paths expansions]
   (binding [*context-limits* expansions]
     (project-node app-db [] (vec paths)))))

(defn app-db-branch [app-db paths path expansions]
  (binding [*context-limits* expansions]
    (project-node (get-in app-db path) path (vec paths))))
