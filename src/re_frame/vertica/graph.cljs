(ns re-frame.vertica.graph
  (:require [re-frame.core :as rf]
            [re-frame.db :as db]
            [re-frame.subs :as subs]
            [goog.object :as gobj]
            [re-frame.vertica.react :as react]
            [re-frame.vertica.registry :as registry]
            [re-frame.vertica.projection :as projection]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.state :as state]
            [re-frame.vertica.tracker :as tracker]))

(def max-nodes 300)
(def max-edges 600)

(defn- node-room? [graph]
  (< (count (:nodes graph)) max-nodes))

(defn- add-node [graph id node]
  (cond
    (contains? (:nodes graph) id) (update graph :nodes assoc id node)
    (node-room? graph) (update graph :nodes assoc id node)
    :else (assoc graph :truncated? true)))

(defn- add-edge [graph edge]
  (cond
    (contains? (:edges graph) edge) graph
    (< (count (:edges graph)) max-edges) (update graph :edges conj edge)
    :else (assoc graph :truncated? true)))

(defn- watched [reaction]
  (when-let [items (and reaction (gobj/get reaction "watching"))]
    (array-seq items)))

(defn- reaction-query [reaction]
  (try (subs/query-v-for-reaction reaction)
       (catch :default _ nil)))

(defn- query-id [query-v] (first query-v))

(defn- subscription-id [query-v]
  (shared/stable-id :subscription query-v))

(defn- value-token [kind identity]
  (state/new-token!))

(defonce ^:private preview-cache (js/WeakMap.))

(defn- value-preview [value limit]
  (if (coll? value)
    (let [cached (or (.get preview-cache value) {})]
      (if-let [preview (get cached limit)]
        preview
        (let [preview (shared/value-preview value limit)]
          (.set preview-cache value (assoc cached limit preview))
          preview)))
    (shared/value-preview value limit)))

(defn- path-value [app-db path]
  (let [wildcard-index (first (keep-indexed #(when (= shared/wildcard %2) %1) path))
        concrete (if (nil? wildcard-index) path (subvec path 0 wildcard-index))]
    (get-in app-db concrete)))

(declare replay-subscription-value add-paths)

(defn- fallback-value [reaction complete?]
  (reset! complete? false)
  (try @reaction (catch :default error error)))

(defn- resolve-signal [signal reads visiting memo traces complete?]
  (cond
    (identical? signal db/app-db) (tracker/tracked reads [] @db/app-db)
    (reaction-query signal) (replay-subscription-value signal (reaction-query signal)
                                                       visiting memo traces)
    (vector? signal) (mapv #(resolve-signal % reads visiting memo traces complete?) signal)
    (map? signal) (reduce-kv (fn [result key value]
                               (assoc result key (resolve-signal value reads visiting memo traces complete?)))
                             (empty signal) signal)
    (array? signal) (mapv #(resolve-signal % reads visiting memo traces complete?) (array-seq signal))
    (sequential? signal) (doall (map #(resolve-signal % reads visiting memo traces complete?) signal))
    (satisfies? IDeref signal) (fallback-value signal complete?)
    :else signal))

(defn- replay-subscription-value [reaction query-v visiting memo traces]
  (if (contains? @memo query-v)
    (get @memo query-v)
    (if (contains? visiting query-v)
      (let [complete? (or (some-> @traces (get query-v) :complete?) (atom true))]
        (fallback-value reaction complete?))
      (let [reads (tracker/read-log)
            complete? (atom true)]
        (swap! traces assoc query-v {:reads reads :complete? complete?})
        (if-let [registration (get @state/registrations (query-id query-v))]
          (let [dyn-v (when (= query-v (:latest-query registration)) (:latest-dyn registration))
                signals (registry/input-signals registration query-v dyn-v)]
            (if (= ::registry/unknown signals)
              (fallback-value reaction complete?)
              (try
                (let [inputs (resolve-signal signals reads (conj visiting query-v) memo traces complete?)
                      value (if (some? dyn-v)
                              ((:computation-fn registration) inputs query-v dyn-v)
                              ((:computation-fn registration) inputs query-v))]
                  (swap! memo assoc query-v value)
                  value)
                (catch :default _ (fallback-value reaction complete?)))))
          (fallback-value reaction complete?))))))

(defn- trace-subscription [reaction query-v]
  (let [traces (atom {})]
    (replay-subscription-value reaction query-v #{} (atom {}) traces)
    (into {}
          (map (fn [[query {:keys [reads complete?]}]]
                 [query {:paths (tracker/recorded-paths reads)
                         :complete? @complete?}]))
          @traces)))

(defn- add-traces [graph traces app-db]
  (reduce-kv
    (fn [g query-v analysis]
      (let [sub-id (subscription-id query-v)]
        (if (contains? (:nodes g) sub-id)
          (-> g
              (update-in [:nodes sub-id :complete?] #(and % (:complete? analysis)))
              (add-paths sub-id analysis app-db))
          g)))
    graph traces))

(defn- add-paths [graph sub-id analysis app-db]
  (reduce
    (fn [g path]
      (let [id (shared/stable-id :app-db-path path)]
        (if (and (not (contains? (:nodes g) id)) (not (node-room? g)))
          (reduced (assoc g :truncated? true))
          (let [existing? (contains? (:nodes g) id)
                value (path-value app-db path)
                token (when-not existing? (value-token :app-db-path path))
                preview (when-not existing? (value-preview value 1200))
                with-node (if existing?
                            g
                            (do
                              (state/remember-value! token value)
                              (add-node g id
                                        {:id id :kind :app-db-path :label (shared/path-label path)
                                         :path path
                                         :association-path (shared/association-path path)
                                         :specificity (count path)
                                         :preview (:text preview)
                                         :preview-truncated? (:truncated? preview)
                                         :token token})))]
            (add-edge with-node {:from id :to sub-id :kind :data-input})))))
    graph (:paths analysis)))

(declare add-subscription)

(defn- add-subscription [graph reaction query-v]
  (let [id (subscription-id query-v)]
    (if (contains? (:nodes graph) id)
      graph
      (if-not (node-room? graph)
        (assoc graph :truncated? true)
        (let [registration (get @state/registrations (query-id query-v))
              value (try @reaction (catch :default error error))
              token (value-token :subscription query-v)
              input-reactions (->> (watched reaction)
                                   (keep #(when-let [q (reaction-query %)] [% q])))
              raw-or-missing? (nil? registration)
              preview (value-preview value 600)
              node {:id id :kind :subscription :label (pr-str query-v)
                    :preview (:text preview) :preview-truncated? (:truncated? preview)
                    :token token
                    :complete? (not raw-or-missing?)
                    :reason (when raw-or-missing?
                              "reg-sub-raw, Subscription alpha, or registration before preload")}]
          (state/remember-value! token value)
          (reduce
            (fn [g [input input-query]]
              (if-not (node-room? g)
                (reduced (assoc g :truncated? true))
                (let [with-input (add-subscription g input input-query)
                      input-id (subscription-id input-query)]
                  (if (contains? (:nodes with-input) input-id)
                    (add-edge with-input {:from input-id :to id :kind :data-input})
                    with-input))))
            (add-node graph id node) input-reactions))))))

(def ^:private max-prop-reference-scan 2000)

(defn- reference-value? [value]
  (or (coll? value) (array? value)))

(defn- reference-children [value remaining]
  (take remaining
        (cond
          (map? value) (vals value)
          (or (vector? value) (set? value)) value
          (array? value) (array-seq value)
          :else [])))

(defn- reference-index [values]
  (let [references (js/WeakSet.)]
    (loop [stack (vec values)
           scanned 0]
      (if (or (empty? stack) (>= scanned max-prop-reference-scan))
        references
        (let [value (peek stack)
              stack (pop stack)]
          (if (or (not (reference-value? value)) (.has references value))
            (recur stack scanned)
            (do
              (.add references value)
              (recur (into stack (reference-children value (- max-prop-reference-scan scanned 1)))
                     (inc scanned)))))))))

(defn- contains-indexed-reference? [value references]
  (loop [stack [value]
         seen (js/WeakSet.)
         scanned 0]
    (if (or (empty? stack) (>= scanned max-prop-reference-scan))
      false
      (let [candidate (peek stack)
            stack (pop stack)]
        (cond
          (not (reference-value? candidate)) (recur stack seen scanned)
          (.has references candidate) true
          (.has seen candidate) (recur stack seen scanned)
          :else (do
                  (.add seen candidate)
                  (recur (into stack (reference-children candidate (- max-prop-reference-scan scanned 1)))
                         seen (inc scanned))))))))

(defn- component-subscriptions [{:keys [reaction]}]
  (->> (watched reaction)
       (keep (fn [watched-reaction]
               (when-let [query-v (reaction-query watched-reaction)]
                 {:reaction watched-reaction :query query-v})))
       vec))

(defn- prop-id [component-id index]
  (shared/stable-id :prop [component-id index]))

(defn- exact-reference-paths [paths references app-db]
  (->> paths
       (mapcat (fn [path]
                 (map #(subvec (vec path) 0 %) (range 1 (inc (count path))))))
       distinct
       (filter (fn [path]
                 (let [value (path-value app-db path)]
                   (and (reference-value? value) (.has references value)))))
       shared/leaf-paths))

(defn- refine-prop-traces [traces prop-values app-db]
  (let [references (reference-index prop-values)
        exact (into {}
                    (map (fn [[query analysis]]
                           [query (exact-reference-paths (:paths analysis) references app-db)]))
                    traces)]
    (if (some seq (vals exact))
      (reduce-kv (fn [result query analysis]
                   (assoc result query (assoc analysis :paths (get exact query))))
                 {} traces)
      traces)))

(defn snapshot [element]
  (state/reset-values!)
  ;; Direct leaf subscriptions are exact render dependencies. Ancestor
  ;; subscriptions are included only when their live output shares an object
  ;; identity with an actual leaf argument; scalar value equality is never used
  ;; as provenance evidence.
  (let [{owners :components :keys [warning]} (react/owning-components element max-nodes)
        components (filterv :reaction owners)
        leaf-component (peek components)
        leaf-id (:id leaf-component)
        leaf-arguments (vec (:arguments leaf-component))
        direct-inputs (component-subscriptions leaf-component)
        prop-descriptors (mapv (fn [index value]
                                 {:index index :value value
                                  :id (prop-id leaf-id index)
                                  :references (reference-index [value])})
                               (range) leaf-arguments)
        ancestor-inputs (mapcat component-subscriptions
                                (if (seq components) (pop components) []))
        prop-sources
        (vec (for [{:keys [id value references]} prop-descriptors
                   {:keys [reaction query]} ancestor-inputs
                   :let [subscription-value (try @reaction (catch :default _ nil))]
                   :when (and (reference-value? value)
                              (contains-indexed-reference? subscription-value references))]
               {:prop-id id :value value :reaction reaction :query query}))
        sourced-prop-ids (set (map :prop-id prop-sources))
        relevant-inputs
        (-> (reduce (fn [inputs {:keys [reaction query]}]
                      (assoc inputs query {:reaction reaction :direct? true :props []}))
                    {} direct-inputs)
            ((fn [inputs]
               (reduce (fn [result {:keys [reaction query] :as source}]
                         (update result query
                                 (fn [entry]
                                   (-> (or entry {:reaction reaction :direct? false :props []})
                                       (update :props conj source)))))
                       inputs prop-sources))))
        element-id (react/object-id "element" element)
        element-token (value-token :element element)
        initial {:nodes {element-id {:id element-id :kind :element
                                     :label (or (react/element-label element) "No element")
                                     :preview (shared/bounded-preview element)
                                     :token element-token}}
                 :edges #{}}
        _ (state/remember-value! element-token element)
        traces (atom {})
        with-components
        (reduce
          (fn [graph {:keys [id name reaction adapter arguments]}]
            (if-not (node-room? graph)
              (reduced (assoc graph :truncated? true))
              (let [component-token (value-token :component id)
                    graph (add-node graph id
                                    {:id id :kind :component :label name
                                     :adapter adapter :token component-token
                                     :preview (str (cljs.core/name adapter) " Reagent component")})
                    _ (state/remember-value! component-token reaction)]
                graph)))
          initial components)
        with-props
        (reduce
          (fn [graph {:keys [id index value]}]
            (if-not (node-room? graph)
              (reduced (assoc graph :truncated? true))
              (let [token (value-token :prop [leaf-id index])
                    preview (value-preview value 600)
                    graph (add-node graph id
                                    {:id id :kind :prop :label (str "arg " (inc index))
                                     :component-name (:name leaf-component)
                                     :argument-index index
                                     :argument-count (count leaf-arguments)
                                     :preview (:text preview)
                                     :preview-truncated? (:truncated? preview)
                                     :complete? (contains? sourced-prop-ids id)
                                     :reason (when-not (contains? sourced-prop-ids id)
                                               "Exact prop value; no identity-preserving subscription source was found.")
                                     :token token})]
                (state/remember-value! token value)
                (add-edge graph {:from id :to leaf-id :kind :render-input}))))
          with-components prop-descriptors)
        with-inputs
        (reduce-kv
          (fn [graph query-v {:keys [reaction direct? props]}]
            (let [sub-id (subscription-id query-v)
                  with-sub (add-subscription graph reaction query-v)]
              (if-not (contains? (:nodes with-sub) sub-id)
                (reduced (assoc with-sub :truncated? true))
                (let [trace-map (or (get @traces query-v)
                                    (let [result (trace-subscription reaction query-v)]
                                      (swap! traces assoc query-v result)
                                      result))
                      relevant-traces (if direct?
                                        trace-map
                                        (refine-prop-traces trace-map (map :value props) @db/app-db))
                      with-traces (add-traces with-sub relevant-traces @db/app-db)
                      with-render (if direct?
                                    (add-edge with-traces {:from sub-id :to leaf-id :kind :render-input})
                                    with-traces)]
                  (reduce (fn [g {:keys [prop-id]}]
                            (add-edge g {:from sub-id :to prop-id :kind :data-input}))
                          with-render props)))))
          with-props relevant-inputs)
        component-ids (->> components (map :id) (filter #(contains? (:nodes with-inputs) %)) vec)
        ownership (concat (map (fn [[parent child]]
                                 {:from parent :to child :kind :render-ownership})
                               (partition 2 1 component-ids))
                          (when-let [last-component (peek component-ids)]
                            [{:from last-component :to element-id :kind :render-ownership}]))
        graph (reduce add-edge with-inputs ownership)
        ordered (shared/deterministic-graph (vals (:nodes graph)) (:edges graph))
        app-db-paths (mapv :path (filter #(= :app-db-path (:kind %)) (:nodes ordered)))
        _ (reset! state/app-db-paths app-db-paths)
        warnings (vec (concat @state/runtime-warnings
                              (when warning [warning])
                              (when (some (fn [{:keys [reaction]}]
                                            (some #(nil? (reaction-query %)) (watched reaction)))
                                          components)
                                [{:code :local-ratom
                                  :message "A component watches local ratoms; only re-frame subscription inputs are graphed."}])
                              (when (and element (empty? components))
                                [{:code :no-reagent-owner
                                  :message "No supported owning Reagent component was found."}]))) ]
    (assoc ordered
           :protocol shared/protocol-version
           :revision @state/revision
           :selection {:label (react/element-label element)}
           :app-db-tree (projection/app-db-tree @db/app-db app-db-paths @state/app-db-expansions)
           :truncated? (boolean (:truncated? graph))
           :warnings warnings)))
