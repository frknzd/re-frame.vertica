(ns re-frame.vertica.causal
  (:require [clojure.string :as str]
            [goog.object :as gobj]
            [re-frame.vertica.react :as react]))

(def ^:private sentinel ::perturbed)

(defn- reference-value? [value]
  (or (coll? value) (array? value)))

(defn- children [value]
  (cond
    (map? value) (map (fn [[key child]] [key child]) value)
    (vector? value) (map-indexed vector value)
    (array? value) (map-indexed vector (array-seq value))
    (set? value) (map (fn [item] [item item]) value)
    (sequential? value) (map-indexed vector value)
    :else []))

(defn- index-aligned! [replacements baseline counterfactual]
  (let [seen (js/WeakSet.)]
    (loop [stack [[baseline counterfactual]]]
      (when-let [[before after] (peek stack)]
        (let [stack (pop stack)]
          (if (or (not (reference-value? before)) (.has seen before))
            (recur stack)
            (let [after-by-key (into {} (children after))]
              (.add seen before)
              (.set replacements before after)
              (recur
                (reduce (fn [result [key child]]
                          (if (contains? after-by-key key)
                            (conj result [child (get after-by-key key)])
                            result))
                        stack (children before))))))))))

(defn- replacement-index [prop-sources replay]
  (reduce
    (fn [index {:keys [query reaction]}]
      (if (contains? (:values replay) query)
        (do
          (index-aligned! index
                          (try @reaction (catch :default _ nil))
                          (get (:values replay) query))
          index)
        index))
    (js/WeakMap.) prop-sources))

(declare replace-references)

(defn- replace-map-references [value replacements seen]
  (reduce-kv (fn [result key child]
               (assoc result key (replace-references child replacements seen)))
             (empty value) value))

(defn- replace-references [value replacements seen]
  (cond
    (and (reference-value? value) (.has replacements value)) (.get replacements value)
    (or (not (reference-value? value)) (.has seen value)) value
    :else
    (do
      (.add seen value)
      (cond
        (map? value) (replace-map-references value replacements seen)
        (vector? value) (mapv #(replace-references % replacements seen) value)
        (array? value) (to-array (map #(replace-references % replacements seen) (array-seq value)))
        (set? value) (into #{} (map #(replace-references % replacements seen)) value)
        (sequential? value) (doall (map #(replace-references % replacements seen) value))
        :else value))))

(defn- event-attribute? [key]
  (boolean (re-find #"^on(?:-|[A-Z])" key)))

(declare normalize-visible-value)

(defn- visible-attributes [attributes]
  (let [entries (cond
                  (map? attributes) attributes
                  (and attributes (object? attributes))
                  (map (fn [key] [key (gobj/get attributes key)])
                       (array-seq (js/Object.keys attributes)))
                  :else [])]
    (->> entries
         (keep (fn [[raw-key value]]
                 (let [key (if (keyword? raw-key) (name raw-key) (str raw-key))]
                   (when (and (not (contains? #{"children" "key" "ref"} key))
                              (not (event-attribute? key))
                              (not (fn? value)))
                     [key (normalize-visible-value value)]))))
         (into (sorted-map)))))

(defn- normalize-visible-value
  ([value] (normalize-visible-value value (js/WeakSet.)))
  ([value seen]
   (cond
     (fn? value) ::function
     (map? value) (into (sorted-map)
                        (map (fn [[key child]]
                               [(str key) (normalize-visible-value child seen)]))
                        value)
     (vector? value) (mapv #(normalize-visible-value % seen) value)
     (set? value) (->> value (map #(normalize-visible-value % seen)) (sort-by pr-str) vec)
     (array? value) (mapv #(normalize-visible-value % seen) (array-seq value))
     (sequential? value) (mapv #(normalize-visible-value % seen) value)
     (and value (object? value)
          (or (identical? (js/Object.getPrototypeOf value) js/Object.prototype)
              (nil? (js/Object.getPrototypeOf value))))
     (if (.has seen value)
       ::cycle
       (do
         (.add seen value)
         (into (sorted-map)
               (map (fn [key] [key (normalize-visible-value (gobj/get value key) seen)]))
               (array-seq (js/Object.keys value)))))
     (or (string? value) (number? value) (keyword? value) (symbol? value)
         (boolean? value) (nil? value)) value
     :else (str value))))

(defn- hiccup-tag [tag]
  (let [raw (name tag)
        [_ element-tag id suffix] (re-matches #"([^#.]+)(?:#([^\.]+))?(.*)" raw)
        classes (when (seq suffix) (remove str/blank? (str/split suffix #"\.")))]
    {:tag (or element-tag raw)
     :id id
     :classes (vec classes)}))

(declare normalize-nodes)

(defn- normalize-hiccup [form]
  (let [descriptor (hiccup-tag (first form))
        second-value (second form)
        attributes (if (map? second-value) second-value {})
        child-start (if (map? second-value) 2 1)
        attributes (cond-> attributes
                     (and (:id descriptor) (not (contains? attributes :id)))
                     (assoc :id (:id descriptor))
                     (and (seq (:classes descriptor))
                          (not (or (contains? attributes :class)
                                   (contains? attributes :className))))
                     (assoc :class (str/join " " (:classes descriptor))))]
    {:kind :element
     :tag (:tag descriptor)
     :attrs (visible-attributes attributes)
     :children (vec (mapcat normalize-nodes (drop child-start form)))}))

(defn- react-element? [value]
  (and value (object? value) (some? (gobj/get value "props"))
       (some? (gobj/get value "type"))))

(defn- normalize-react-element [element]
  (let [type (gobj/get element "type")
        props (gobj/get element "props")]
    (cond
      (string? type)
      [{:kind :element
        :tag type
        :attrs (visible-attributes props)
        :children (vec (normalize-nodes (gobj/get props "children")))}]

      ;; A Fragment has no host representation; its visible children occupy
      ;; the surrounding level directly.
      (= "symbol" (js* "typeof ~{}" type))
      (normalize-nodes (gobj/get props "children"))

      ;; Nested component elements are deliberately opaque. A selected node
      ;; inside one has a nearer Reagent owner and is analyzed there.
      :else [{:kind :opaque-component
              :name (or (gobj/get type "displayName") (gobj/get type "name") "component")}])))

(defn normalize-nodes [value]
  (cond
    (nil? value) []
    (react-element? value) (normalize-react-element value)
    (and (vector? value) (or (keyword? (first value)) (string? (first value))))
    [(normalize-hiccup value)]
    (array? value) (vec (mapcat normalize-nodes (array-seq value)))
    (and (sequential? value) (not (string? value))) (vec (mapcat normalize-nodes value))
    (or (string? value) (number? value) (keyword? value) (symbol? value))
    [{:kind :text :text (str value)}]
    :else []))

(defn- element-children [node]
  (filterv #(= :element (:kind %)) (:children node)))

(defn- root-matches? [node {:keys [tag id classes]}]
  (and (= (str/lower-case (str tag)) (str/lower-case (str (:tag node))))
       (or (str/blank? id) (= id (get-in node [:attrs "id"])))
       (or (str/blank? classes)
           (= (set (remove str/blank? (str/split classes #"\s+")))
              (set (remove str/blank?
                           (str/split (str (or (get-in node [:attrs "class"])
                                               (get-in node [:attrs "className"])
                                               "")) #"\s+")))))))

(defn- selected-branch [render-value route]
  (let [roots (filterv #(= :element (:kind %)) (normalize-nodes render-value))
        matches (filterv #(root-matches? % (:root route)) roots)
        root (cond
               (= 1 (count matches)) (first matches)
               :else nil)]
    (reduce (fn [node index]
              (when node (get (element-children node) index)))
            root (:indices route))))

(defn render-oracle [element leaf-component]
  (try
    (let [reaction (:reaction leaf-component)
          render-fn (and reaction (gobj/get reaction "f"))
          route (react/selected-element-route element leaf-component)]
      (cond
        (not (fn? render-fn)) {:error "The selected component render closure is unavailable."}
        (nil? route) {:error "The selected DOM branch could not be mapped to the component output."}
        :else
        (let [first-branch (selected-branch (render-fn) route)
              second-branch (selected-branch (render-fn) route)]
          (cond
            (nil? first-branch) {:error "The selected DOM branch was not found in the component output."}
            (not= first-branch second-branch) {:error "The selected component render output is nondeterministic."}
            :else {:render-fn render-fn :route route :baseline first-branch
                   :argv-slot (react/component-argv-slot leaf-component)}))))
    (catch :default error
      {:error (str "The selected component could not be rendered safely: "
                   (or (ex-message error) error))})))

(defn- with-counterfactual-state [replay prop-sources oracle f]
  (let [reaction-snapshots
        (reduce-kv (fn [result query reaction]
                     (if (contains? (:values replay) query)
                       (conj result [query reaction (gobj/get reaction "state")])
                       result))
                   [] (:reactions replay))
        slot (:argv-slot oracle)
        original-argv (:value slot)
        replacements (replacement-index prop-sources replay)
        next-argv (when slot (replace-references original-argv replacements (js/WeakSet.)))]
    (try
      (doseq [[query reaction _] reaction-snapshots]
        (gobj/set reaction "state" (get (:values replay) query)))
      (when slot (gobj/set (:target slot) "argv" next-argv))
      (f)
      (finally
        (when slot (gobj/set (:target slot) "argv" original-argv))
        (doseq [[_ reaction state] reaction-snapshots]
          (gobj/set reaction "state" state))))))

(defn- distinct-values [baseline values]
  (distinct (remove #(= baseline %) values)))

(defn- scalar-variants [value]
  (distinct-values
    value
    (cond
      (nil? value) [sentinel false 0 ""]
      (boolean? value) [(not value)]
      (number? value) [0 1 -1 (inc value) (dec value) (- value)
                       js/Number.MAX_SAFE_INTEGER js/Number.MIN_SAFE_INTEGER]
      (string? value) ["" (str value "__re_frame_vertica__")
                       (str "__re_frame_vertica__" value)]
      (keyword? value) [sentinel (keyword (namespace value) (str (name value) "-perturbed"))]
      (symbol? value) ['re-frame.vertica.causal/perturbed]
      :else [sentinel nil])))

(defn- unique-map-key [value]
  (loop [index 0]
    (let [candidate (keyword "re-frame.vertica.causal" (str "perturbed-" index))]
      (if (contains? value candidate)
        (recur (inc index))
        candidate))))

(defn- collection-variants [value]
  (distinct-values
    value
    (cond
      (map? value) (cond-> [(assoc value (unique-map-key value) sentinel)]
                     (seq value) (conj (dissoc value (ffirst value))))
      (vector? value) (cond-> [(conj value sentinel) []]
                        (seq value) (conj (pop value) (vec (reverse value))))
      (set? value) (cond-> [(conj value sentinel) #{}]
                     (seq value) (conj (disj value (first value))))
      (sequential? value) (cond-> [(cons sentinel value) '()]
                            (seq value) (conj (rest value) (reverse value)))
      :else [])))

(defn- value-variants [value]
  (if (coll? value) (collection-variants value) (scalar-variants value)))

(defn- replace-child [parent key replacement]
  (cond
    (map? parent) {:ok? true :value (assoc parent key replacement)}
    (vector? parent)
    (if (and (integer? key) (<= 0 key) (< key (count parent)))
      {:ok? true :value (assoc parent key replacement)}
      {:ok? false :reason "An out-of-range vector lookup cannot be safely materialized."})
    (set? parent)
    {:ok? true :value (if (contains? parent key) (disj parent key) (conj parent key))}
    :else {:ok? false :reason "The candidate path crosses a value that cannot be safely replaced."}))

(defn- rebuild-path [value frames]
  (reduce (fn [child [parent key]]
            (:value (replace-child parent key child)))
          value
          (reverse frames)))

(defn- perturbations [app-db path]
  (if (empty? path)
    {:values (value-variants app-db)}
    (loop [cursor app-db
           remaining path
           frames []]
      (if (= 1 (count remaining))
        (let [key (first remaining)]
          (if (set? cursor)
            (let [{:keys [ok? value reason]} (replace-child cursor key nil)]
              (if ok?
                {:values [(rebuild-path value frames)]}
                {:values [] :reason reason}))
            (let [present? (cond
                             (map? cursor) (contains? cursor key)
                             (vector? cursor) (and (integer? key) (<= 0 key) (< key (count cursor)))
                             :else false)
                  current (when present? (get cursor key))
                  variants (seq (if present? (value-variants current) (scalar-variants nil)))]
              (if-not variants
                {:values [] :reason "No safe counterfactual value exists for this path."}
                (let [first-leaf (replace-child cursor key (first variants))]
                  (if-not (:ok? first-leaf)
                    {:values [] :reason (:reason first-leaf)}
                    {:values
                     (cons (rebuild-path (:value first-leaf) frames)
                           (map (fn [variant]
                                  (-> (replace-child cursor key variant)
                                      :value
                                      (rebuild-path frames)))
                                (next variants)))}))))))
        (let [key (first remaining)
              child (cond
                      (map? cursor) (when (contains? cursor key) (get cursor key))
                      (vector? cursor) (when (and (integer? key) (<= 0 key) (< key (count cursor)))
                                         (get cursor key))
                      :else nil)]
          (if (nil? child)
            {:values [] :reason "The candidate path has no replaceable parent in the current app-db."}
            (recur child (rest remaining) (conj frames [cursor key]))))))))

(defn classify-paths
  [{:keys [app-db paths replay prop-sources oracle root-reaction]}]
  (let [oracle-error (:error oracle)
        live-root (try @root-reaction (catch :default error error))
        baseline-replay (when-not oracle-error
                          (try (replay app-db)
                               (catch :default error
                                 {:complete? false :reason (or (ex-message error) (str error))})))
        setup-error (or oracle-error
                        (when (and (seq prop-sources) (nil? (:argv-slot oracle)))
                          "The selected component arguments cannot be safely substituted.")
                        (when-not (:complete? baseline-replay)
                          (or (:reason baseline-replay) "Subscription replay was incomplete."))
                        (when (and baseline-replay (not= live-root (:value baseline-replay)))
                          "Subscription replay does not reproduce the live value."))]
    (into {}
          (map
            (fn [path]
              (if setup-error
                [path {:status :inconclusive :reason setup-error}]
                (let [{databases :values perturbation-error :reason} (perturbations app-db path)]
                  (if (empty? databases)
                    [path {:status :inconclusive :reason perturbation-error}]
                    (loop [remaining (seq databases) successful 0 failure nil]
                      (if remaining
                        (let [counterfactual-db (first remaining)
                              result
                              (try
                                (let [replayed (replay counterfactual-db)]
                                  (cond
                                    (not (:complete? replayed))
                                    {:error (or (:reason replayed) "Subscription replay was incomplete.")}
                                    :else
                                    (with-counterfactual-state
                                      replayed prop-sources oracle
                                      (fn []
                                        (let [branch (selected-branch ((:render-fn oracle)) (:route oracle))]
                                          (if (nil? branch)
                                            {:changed? true}
                                            {:changed? (not= (:baseline oracle) branch)}))))))
                                (catch :default error
                                  {:error (or (ex-message error) (str error))}))]
                          (cond
                            (:changed? result) [path {:status :confirmed}]
                            (:error result) (recur (next remaining) successful (or failure (:error result)))
                            :else (recur (next remaining) (inc successful) failure)))
                        [path (cond
                                failure {:status :inconclusive :reason failure}
                                (pos? successful) {:status :rejected}
                                :else {:status :inconclusive
                                       :reason "No counterfactual render completed."})]))))))
          paths))))
