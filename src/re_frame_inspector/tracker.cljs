(ns re-frame-inspector.tracker
  (:require [re-frame-inspector.shared :as shared]))

(declare tracked)

(defn- unsupported! [op path]
  (throw (ex-info (str "unsupported replay operation: " (name op))
                  {:type ::unsupported :operation op :path path})))

(defn- record! [reads path]
  (swap! reads conj (vec path)))

(defn- child [reads path key value]
  (let [next-path (conj path key)]
    (record! reads next-path)
    (tracked reads next-path value)))

(defn- collection-seq [reads path value]
  (record! reads (conj path shared/wildcard))
  (cond
    (map? value) (seq (map (fn [[k v]] [k (tracked reads (conj path k) v)]) value))
    (vector? value) (seq (map-indexed #(tracked reads (conj path %1) %2) value))
    (set? value) (seq (map #(tracked reads (conj path %) %) value))
    :else (seq value)))

(deftype TrackedMap [value reads path]
  ILookup
  (-lookup [_ k]
    (if (contains? value k) (child reads path k (get value k)) nil))
  (-lookup [_ k not-found]
    (if (contains? value k) (child reads path k (get value k)) not-found))
  IAssociative
  (-contains-key? [_ k] (record! reads (conj path k)) (contains? value k))
  (-assoc [_ _ _] (unsupported! :assoc path))
  IMap
  (-dissoc [_ _] (unsupported! :dissoc path))
  ICounted
  (-count [_] (record! reads (conj path shared/wildcard)) (count value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  IKVReduce
  (-kv-reduce [_ f init]
    (record! reads (conj path shared/wildcard))
    (reduce-kv (fn [acc k v] (f acc k (tracked reads (conj path k) v))) init value))
  ICollection
  (-conj [_ _] (unsupported! :conj path))
  IEmptyableCollection
  (-empty [_] {})
  IEquiv
  (-equiv [_ other] (= value other))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#<tracked " (pr-str value) ">"))))

(deftype TrackedVector [value reads path]
  ILookup
  (-lookup [_ k] (if (and (integer? k) (< -1 k (count value)))
                   (child reads path k (nth value k)) nil))
  (-lookup [_ k not-found] (if (and (integer? k) (< -1 k (count value)))
                             (child reads path k (nth value k)) not-found))
  IIndexed
  (-nth [_ n] (child reads path n (nth value n)))
  (-nth [_ n not-found] (if (< -1 n (count value))
                          (child reads path n (nth value n)) not-found))
  IAssociative
  (-contains-key? [_ k] (record! reads (conj path k)) (and (integer? k) (< -1 k (count value))))
  (-assoc [_ _ _] (unsupported! :assoc path))
  IVector
  (-assoc-n [_ _ _] (unsupported! :assoc-n path))
  ISequential
  ICounted
  (-count [_] (record! reads (conj path shared/wildcard)) (count value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  IKVReduce
  (-kv-reduce [_ f init]
    (record! reads (conj path shared/wildcard))
    (reduce-kv (fn [acc k v] (f acc k (tracked reads (conj path k) v))) init value))
  ICollection
  (-conj [_ _] (unsupported! :conj path))
  IEmptyableCollection
  (-empty [_] [])
  IStack
  (-peek [_] (when (seq value) (child reads path (dec (count value)) (peek value))))
  (-pop [_] (unsupported! :pop path))
  IEquiv
  (-equiv [_ other] (= value other))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#<tracked " (pr-str value) ">"))))

(deftype TrackedSet [value reads path]
  ILookup
  (-lookup [_ k] (record! reads (conj path k)) (get value k))
  (-lookup [_ k not-found] (record! reads (conj path k)) (get value k not-found))
  ISet
  (-disjoin [_ _] (unsupported! :disjoin path))
  IFn
  (-invoke [_ k] (record! reads (conj path k)) (get value k))
  (-invoke [_ k not-found] (record! reads (conj path k)) (get value k not-found))
  ICounted
  (-count [_] (record! reads (conj path shared/wildcard)) (count value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  ICollection
  (-conj [_ _] (unsupported! :conj path))
  IEmptyableCollection
  (-empty [_] #{})
  IEquiv
  (-equiv [_ other] (= value other))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#<tracked " (pr-str value) ">"))))

(defn tracked [reads path value]
  (cond
    (map? value) (TrackedMap. value reads path)
    (vector? value) (TrackedVector. value reads path)
    (set? value) (TrackedSet. value reads path)
    :else value))

(defn replay
  "Replay a pure layer-2 computation against read-tracking collections. Returns
  all proven paths even when the computation throws."
  [computation app-db query-v dyn-v]
  (let [reads (atom #{})]
    (try
      (if (some? dyn-v)
        (computation (tracked reads [] app-db) query-v dyn-v)
        (computation (tracked reads [] app-db) query-v))
      {:paths (shared/leaf-paths @reads) :complete? true}
      (catch :default error
        {:paths (shared/leaf-paths @reads)
         :complete? false
         :reason (or (ex-message error) (str error))
         :operation (:operation (ex-data error))}))))

