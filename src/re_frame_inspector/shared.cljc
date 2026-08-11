(ns re-frame-inspector.shared
  (:require [clojure.string :as str]))

(def protocol-version 1)
(def wildcard :re-frame-inspector/all)

(defn path-prefix?
  [prefix path]
  (and (< (count prefix) (count path))
       (= prefix (subvec (vec path) 0 (count prefix)))))

(defn leaf-paths
  "Remove a path when a more specific proven path exists. Wildcard descendants
  remain concrete provenance and therefore also supersede their parent."
  [paths]
  (let [paths (set (map vec paths))]
    (->> paths
         (remove (fn [p] (some #(path-prefix? p %) paths)))
         (sort-by pr-str)
         vec)))

(defn path-label [path]
  (str "["
       (str/join " " (map #(if (= wildcard %) "*" (pr-str %)) path))
       "]"))

(defn bounded-preview
  ([value] (bounded-preview value 180))
  ([value limit]
   (let [s (try (pr-str value)
                (catch #?(:clj Throwable :cljs :default) _ "<unprintable>"))]
     (if (> (count s) limit)
       (str (subs s 0 (max 0 (- limit 1))) "…")
       s))))

(defn stable-id [kind value]
  (str (name kind) "-" (Math/abs (hash [kind value]))))

(def layer-order
  {:app-db-path 0 :subscription 1 :component 2 :element 3})

(defn deterministic-graph [nodes edges]
  {:nodes (->> nodes
               (sort-by (juxt #(get layer-order (:kind %) 99) :label :id))
               vec)
   :edges (->> edges (sort-by (juxt :from :to :kind)) distinct vec)})

