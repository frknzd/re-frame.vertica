(ns re-frame.vertica.shared
  (:require [clojure.string :as str]
            #?(:clj [clojure.pprint :as pprint]
               :cljs [cljs.pprint :as pprint])))

(def protocol-version 1)
(def wildcard :re-frame.vertica/all)
(def association-wildcard "__re-frame.vertica-wildcard__")
(def panel-z-index 2147483647)
(def selection-highlight-z-index (dec panel-z-index))
(def component-highlight-z-index (- panel-z-index 2))

(defn association-path
  "Encode a provenance path as stable strings for UI-side matching. Strings are
  printed as EDN too, so the wildcard sentinel cannot collide with an app-db
  string containing the same text."
  [path]
  (mapv #(if (= wildcard %)
           association-wildcard
           (pr-str %))
        path))

(defn path-prefix?
  [prefix path]
  (and (< (count prefix) (count path))
       (= prefix (subvec (vec path) 0 (count prefix)))))

(defn leaf-paths
  "Remove a path when a more specific proven path exists. Wildcard descendants
  remain concrete provenance and therefore also supersede their parent."
  [paths]
  (let [paths (set (map vec paths))
        ;; A path is non-leaf exactly when it is a strict prefix of another
        ;; path. Building that prefix set is linear in total path depth; the
        ;; previous all-pairs search became quadratic on collection-heavy
        ;; subscription replays.
        prefixes (into #{}
                       (mapcat (fn [path]
                                 (map #(subvec path 0 %) (range (count path)))))
                       paths)]
    (->> paths
         (remove prefixes)
         (sort-by pr-str)
         vec)))

(defn path-label [path]
  (str "["
       (str/join " " (map #(if (= wildcard %) "*" (pr-str %)) path))
       "]"))

(def ^:private preview-print-length 4)
(def ^:private preview-print-level 7)

(defn- formatted-value [value bounded?]
  (-> (binding [pprint/*print-right-margin* 72
                pprint/*print-miser-width* 48
                *print-length* (when bounded? preview-print-length)
                *print-level* (when bounded? preview-print-level)]
        (with-out-str (pprint/pprint value)))
      (str/replace #"\n$" "")))

(defn- structurally-truncated?
  ([value] (structurally-truncated? value 0))
  ([value depth]
   (if-not (coll? value)
     false
     (if (>= depth preview-print-level)
       (boolean (seq value))
       (let [sample (vec (take (inc preview-print-length) value))
             too-many? (> (count sample) preview-print-length)
             visible (take preview-print-length sample)
             children (if (map? value) (mapcat identity visible) visible)]
         (or too-many?
             (boolean (some #(structurally-truncated? % (inc depth)) children))))))))

(defn value-string [value]
  (try (formatted-value value false)
       (catch #?(:clj Throwable :cljs :default) _ "<unprintable>")))

(defn value-preview [value limit]
  (let [s (try (formatted-value value true)
               (catch #?(:clj Throwable :cljs :default) _ "<unprintable>"))
        character-truncated? (> (count s) limit)
        truncated? (or character-truncated? (structurally-truncated? value))]
    {:text (if character-truncated?
             (str (subs s 0 (max 0 (- limit 1))) "…")
             s)
     :truncated? truncated?}))

(defn bounded-preview
  ([value] (bounded-preview value 180))
  ([value limit] (:text (value-preview value limit))))

(defn stable-id [kind value]
  (str (name kind) "-" (Math/abs (hash [kind value]))))

(def layer-order
  {:app-db-path 0 :subscription 1 :prop 2 :component 3 :element 4})

(defn deterministic-graph [nodes edges]
  {:nodes (->> nodes
               (sort-by (juxt #(get layer-order (:kind %) 99) :label :id))
               vec)
   :edges (->> edges (sort-by (juxt :from :to :kind)) distinct vec)})
