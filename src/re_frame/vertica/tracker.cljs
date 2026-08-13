(ns re-frame.vertica.tracker
  (:require [re-frame.vertica.shared :as shared]))

(declare tracked)

(defn read-log []
  (atom {:paths #{}
         :structural #{}
         :complete-iterations #{}}))

(defn recorded-paths [reads]
  (let [{:keys [paths structural complete-iterations]} @reads
        concrete (set (shared/leaf-paths paths))
        completed-with-children?
        (fn [path]
          (and (contains? complete-iterations path)
               (some #(shared/path-prefix? path %) concrete)))
        required-structural (remove completed-with-children? structural)]
    (->> (into concrete required-structural)
         (sort-by pr-str)
         vec)))

(defn- unsupported! [op path]
  (throw (ex-info (str "unsupported replay operation: " (name op))
                  {:type ::unsupported :operation op :path path})))

(defn- record! [reads path]
  (let [path (vec path)]
    (when-not (contains? (:paths @reads) path)
      (swap! reads update :paths conj path))))

(defn- record-structural! [reads path]
  (let [path (vec path)]
    (when-not (contains? (:structural @reads) path)
      (swap! reads update :structural conj path))))

(defn- complete-iteration! [reads path]
  (swap! reads update :complete-iterations conj (vec path)))

(defn- child [reads path key value]
  (let [next-path (conj path key)]
    (record! reads next-path)
    (tracked reads next-path value)))

(defn- unchunked-seq
  "Transform collection entries one at a time. Core map/map-indexed may realize
  a whole chunk, which would over-report paths after first/take/some stops."
  [items transform on-complete]
  (lazy-seq
    (if-let [items (seq items)]
      (cons (transform (first items))
            (unchunked-seq (next items) transform on-complete))
      (do (on-complete) nil))))

(defn- unchunked-indexed-seq [items index transform on-complete]
  (lazy-seq
    (if-let [items (seq items)]
      (cons (transform index (first items))
            (unchunked-indexed-seq (next items) (inc index) transform on-complete))
      (do (on-complete) nil))))

(defn- collection-seq [reads path value]
  (record-structural! reads path)
  (let [on-complete #(complete-iteration! reads path)]
    (cond
      (map? value)
      (seq (unchunked-seq value
              (fn [[k v]] [k (child reads path k v)]) on-complete))

      (vector? value)
      (seq (unchunked-seq (range (count value))
              (fn [index] (child reads path index (nth value index))) on-complete))

      (set? value)
      (seq (unchunked-seq value
              (fn [item] (child reads path item item)) on-complete))

      (sequential? value)
      (seq (unchunked-indexed-seq value 0
              (fn [index item] (child reads path index item)) on-complete))

      :else (seq value))))

(defn- collection-count [reads path value]
  ;; Count depends on collection structure, not on every current member.
  (record-structural! reads path)
  (count value))

(defn- tracked-kv-reduce [reads path value f init]
  (record-structural! reads path)
  (if (empty? value)
    (do (complete-iteration! reads path) init)
    (let [short-circuited? (atom false)
          result (reduce-kv (fn [acc k v]
                              (let [next (f acc k (child reads path k v))]
                                (when (reduced? next) (reset! short-circuited? true))
                                next))
                            init value)]
      (when-not @short-circuited? (complete-iteration! reads path))
      result)))

(deftype TrackedMap [value reads path]
  ILookup
  (-lookup [_ k]
    (if (contains? value k)
      (child reads path k (get value k))
      (do (record! reads (conj path k)) nil)))
  (-lookup [_ k not-found]
    (if (contains? value k)
      (child reads path k (get value k))
      (do (record! reads (conj path k)) not-found)))
  IAssociative
  (-contains-key? [_ k] (record! reads (conj path k)) (contains? value k))
  (-assoc [_ _ _] (unsupported! :assoc path))
  IMap
  (-dissoc [_ _] (unsupported! :dissoc path))
  ICounted
  (-count [_] (collection-count reads path value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  IKVReduce
  (-kv-reduce [_ f init]
    (tracked-kv-reduce reads path value f init))
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
                   (child reads path k (nth value k))
                   (do (record! reads (conj path k)) nil)))
  (-lookup [_ k not-found] (if (and (integer? k) (< -1 k (count value)))
                             (child reads path k (nth value k))
                             (do (record! reads (conj path k)) not-found)))
  IIndexed
  (-nth [_ n]
    (record! reads (conj path n))
    (tracked reads (conj path n) (nth value n)))
  (-nth [_ n not-found] (if (< -1 n (count value))
                          (child reads path n (nth value n))
                          (do (record! reads (conj path n)) not-found)))
  IAssociative
  (-contains-key? [_ k] (record! reads (conj path k)) (and (integer? k) (< -1 k (count value))))
  (-assoc [_ _ _] (unsupported! :assoc path))
  IVector
  (-assoc-n [_ _ _] (unsupported! :assoc-n path))
  ISequential
  ICounted
  (-count [_] (collection-count reads path value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  IKVReduce
  (-kv-reduce [_ f init]
    (tracked-kv-reduce reads path value f init))
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
  (-count [_] (collection-count reads path value))
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

(deftype TrackedSequential [value reads path]
  ISequential
  ICounted
  (-count [_] (collection-count reads path value))
  ISeqable
  (-seq [_] (collection-seq reads path value))
  IReduce
  (-reduce [this f] (reduce f (seq this)))
  (-reduce [this f start] (reduce f start (seq this)))
  ICollection
  (-conj [_ _] (unsupported! :conj path))
  IEmptyableCollection
  (-empty [_] '())
  IEquiv
  (-equiv [_ other] (= value other))
  IPrintWithWriter
  (-pr-writer [_ writer _] (-write writer (str "#<tracked " (pr-str value) ">"))))

(defn tracked [reads path value]
  (cond
    (map? value) (TrackedMap. value reads path)
    (vector? value) (TrackedVector. value reads path)
    (set? value) (TrackedSet. value reads path)
    (sequential? value) (TrackedSequential. value reads path)
    :else value))

(defn replay
  "Replay a pure layer-2 computation against read-tracking collections. Returns
  all proven paths even when the computation throws."
  [computation app-db query-v dyn-v]
  (let [reads (read-log)]
    (try
      (if (some? dyn-v)
        (computation (tracked reads [] app-db) query-v dyn-v)
        (computation (tracked reads [] app-db) query-v))
      {:paths (recorded-paths reads) :complete? true}
      (catch :default error
        {:paths (recorded-paths reads)
         :complete? false
         :reason (or (ex-message error) (str error))
         :operation (:operation (ex-data error))}))))
