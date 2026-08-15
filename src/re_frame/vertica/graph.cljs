(ns re-frame.vertica.graph
  (:require [re-frame.db :as db]
            [re-frame.subs :as subs]
            [goog.object :as gobj]
            [re-frame.vertica.causal :as causal]
            [re-frame.vertica.react :as react]
            [re-frame.vertica.registry :as registry]
            [re-frame.vertica.projection :as projection]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.state :as state]
            [re-frame.vertica.tracker :as tracker]))

(defn- add-node [graph id node]
  (update graph :nodes assoc id node))

(defn- add-edge [graph edge]
  (update graph :edges conj edge))

(defn- watched [reaction]
  (when-let [items (and reaction (gobj/get reaction "watching"))]
    (array-seq items)))

(defn- reaction-query [reaction]
  (try (subs/query-v-for-reaction reaction)
       (catch :default _ nil)))

(defn- query-id [query-v] (first query-v))

(defn- subscription-id [query-v]
  (shared/stable-id :subscription query-v))

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

(defn- resolve-signal [signal reads visiting memo traces complete? app-db]
  (cond
    (identical? signal db/app-db) (tracker/tracked reads [] app-db)
    (reaction-query signal) (replay-subscription-value signal (reaction-query signal)
                                                       visiting memo traces app-db)
    (vector? signal) (mapv #(resolve-signal % reads visiting memo traces complete? app-db) signal)
    (map? signal) (reduce-kv (fn [result key value]
                               (assoc result key (resolve-signal value reads visiting memo traces complete? app-db)))
                             (empty signal) signal)
    (array? signal) (mapv #(resolve-signal % reads visiting memo traces complete? app-db) (array-seq signal))
    (sequential? signal) (doall (map #(resolve-signal % reads visiting memo traces complete? app-db) signal))
    (satisfies? IDeref signal) (fallback-value signal complete?)
    :else signal))

(defn- replay-subscription-value [reaction query-v visiting memo traces app-db]
  (if (contains? @memo query-v)
    (get @memo query-v)
    (if (contains? visiting query-v)
      (let [complete? (or (some-> @traces (get query-v) :complete?) (atom true))]
        (fallback-value reaction complete?))
      (let [reads (tracker/read-log)
            complete? (atom true)]
        (swap! traces assoc query-v {:reads reads :complete? complete? :reaction reaction})
        (if-let [registration (get @state/registrations (query-id query-v))]
          (let [dyn-v (when (= query-v (:latest-query registration)) (:latest-dyn registration))
                signals (registry/input-signals registration query-v dyn-v)]
            (if (= ::registry/unknown signals)
              (fallback-value reaction complete?)
              (try
                (let [inputs (resolve-signal signals reads (conj visiting query-v) memo traces complete? app-db)
                      value (if (some? dyn-v)
                              ((:computation-fn registration) inputs query-v dyn-v)
                              ((:computation-fn registration) inputs query-v))]
                  (swap! memo assoc query-v value)
                  value)
                (catch :default _ (fallback-value reaction complete?)))))
          (fallback-value reaction complete?))))))

(defn- trace-subscription [reaction query-v app-db]
  (let [traces (atom {})
        memo (atom {})
        value (replay-subscription-value reaction query-v #{} memo traces app-db)
        analyses (into {}
                       (map (fn [[query {:keys [reads complete?]}]]
                              [query {:paths (tracker/recorded-paths reads)
                                      :complete? @complete?}]))
                       @traces)]
    {:value value
     :values @memo
     :reactions (into {} (map (fn [[query analysis]] [query (:reaction analysis)])) @traces)
     :traces analyses
     :complete? (every? :complete? (vals analyses))
     :reason (when-not (every? :complete? (vals analyses))
               "Subscription replay was incomplete.")}))

(defn- add-traces [graph traces app-db evidence classifications]
  (reduce-kv
    (fn [g query-v analysis]
      (let [sub-id (subscription-id query-v)]
        (if (contains? (:nodes g) sub-id)
          (-> g
              (update-in [:nodes sub-id :complete?] #(and % (:complete? analysis)))
              (add-paths sub-id analysis app-db evidence classifications))
          g)))
    graph traces))

(defn- add-paths [graph sub-id analysis app-db evidence classifications]
  (reduce
    (fn [g path]
      (let [id (shared/stable-id :app-db-path path)
            existing (get-in g [:nodes id])
            value (path-value app-db path)
            token (when-not existing (state/new-token!))
            preview (when-not existing (value-preview value 1200))
            with-node
            (cond
              (nil? existing)
              (do
                (state/remember-value! token value)
                (add-node g id
                          {:id id :kind :app-db-path :label (shared/path-label path)
                           :path path
                           :evidence evidence
                           :reason (get-in classifications [path :reason])
                           :association-path (shared/association-path path)
                           :specificity (count path)
                           :preview (:text preview)
                           :preview-truncated? (:truncated? preview)
                           :token token}))

              (and (= evidence :confirmed) (not= :confirmed (:evidence existing)))
              (-> g
                  (assoc-in [:nodes id :evidence] :confirmed)
                  (assoc-in [:nodes id :reason] nil))

              :else g)]
        (add-edge with-node {:from id :to sub-id :kind :data-input})))
    graph (:paths analysis)))

(declare add-subscription)

(defn- add-subscription [graph reaction query-v]
  (let [id (subscription-id query-v)]
    (if (contains? (:nodes graph) id)
      graph
      (let [registration (get @state/registrations (query-id query-v))
              value (try @reaction (catch :default error error))
              token (state/new-token!)
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
              (let [with-input (add-subscription g input input-query)
                      input-id (subscription-id input-query)]
                  (if (contains? (:nodes with-input) input-id)
                    (add-edge with-input {:from input-id :to id :kind :data-input})
                    with-input)))
            (add-node graph id node) input-reactions)))))

(defn- reference-value? [value]
  (or (coll? value) (array? value)))

(defn- reference-children [value]
  (cond
          (map? value) (vals value)
          (or (vector? value) (set? value)) value
          (array? value) (array-seq value)
          :else []))

(defn- reference-index [values]
  (let [references (js/WeakSet.)]
    (loop [stack (vec values)]
      (if (empty? stack)
        references
        (let [value (peek stack)
              stack (pop stack)]
          (if (or (not (reference-value? value)) (.has references value))
            (recur stack)
            (do
              (.add references value)
              (recur (into stack (reference-children value))))))))))

(defn- contains-indexed-reference? [value references]
  (loop [stack [value]
         seen (js/WeakSet.)]
    (if (empty? stack)
      false
      (let [candidate (peek stack)
            stack (pop stack)]
        (cond
          (not (reference-value? candidate)) (recur stack seen)
          (.has references candidate) true
          (.has seen candidate) (recur stack seen)
          :else (do
                  (.add seen candidate)
                  (recur (into stack (reference-children candidate)) seen)))))))

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

(defn- traces-with-status [traces classifications status]
  (reduce-kv
    (fn [result query analysis]
      (assoc result query
             (update analysis :paths
                     (fn [paths]
                       (filterv #(= status (get-in classifications [% :status])) paths)))))
    {} traces))

(defn snapshot [element]
  (state/reset-values!)
  ;; Read tracking supplies candidates. Only counterfactual changes to the
  ;; selected render branch promote a candidate to confirmed provenance.
  (let [{owners :components :keys [warning]} (react/owning-components element)
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
        subscription-prop-sources
        (vec (for [{:keys [id value references]} prop-descriptors
                   {:keys [reaction query]} ancestor-inputs
                   :let [subscription-value (try @reaction (catch :default _ nil))]
                   :when (and (reference-value? value)
                              (contains-indexed-reference? subscription-value references))]
               {:prop-id id :value value :reaction reaction :query query}))
        sourced-prop-ids (set (map :prop-id subscription-prop-sources))
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
                       inputs subscription-prop-sources))))
        element-id (react/object-id "element" element)
        element-token (state/new-token!)
        initial {:nodes {element-id {:id element-id :kind :element
                                     :label (or (react/element-label element) "No element")
                                     :preview (shared/bounded-preview element)
                                     :token element-token}}
                 :edges #{}}
        _ (state/remember-value! element-token element)
        traces (atom {})
        render-oracle (causal/render-oracle element leaf-component)
        with-components
        (reduce
          (fn [graph {:keys [id name reaction adapter]}]
            (let [component-token (state/new-token!)
                  graph (add-node graph id
                                  {:id id :kind :component :label name
                                   :adapter adapter :token component-token
                                   :preview (str (cljs.core/name adapter) " Reagent component")})]
              (state/remember-value! component-token reaction)
              graph))
          initial components)
        with-props
        (reduce
          (fn [graph {:keys [id index value]}]
            (let [token (state/new-token!)
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
              (add-edge graph {:from id :to leaf-id :kind :render-input})))
          with-components prop-descriptors)
        with-inputs
        (reduce-kv
          (fn [graph query-v {:keys [reaction direct? props]}]
            (let [sub-id (subscription-id query-v)
                  with-sub (add-subscription graph reaction query-v)
                  analysis (or (get @traces query-v)
                               (let [result (trace-subscription reaction query-v @db/app-db)]
                                 (swap! traces assoc query-v result)
                                 result))
                  trace-map (:traces analysis)
                  relevant-traces (if direct?
                                    trace-map
                                    (refine-prop-traces trace-map (map :value props) @db/app-db))
                  candidate-paths (->> relevant-traces vals (mapcat :paths) distinct vec)
                  classifications
                  (causal/classify-paths
                    {:app-db @db/app-db
                     :paths candidate-paths
                     :replay #(trace-subscription reaction query-v %)
                     :prop-sources props
                     :oracle render-oracle
                     :root-reaction reaction})
                  confirmed (traces-with-status relevant-traces classifications :confirmed)
                  inconclusive (traces-with-status relevant-traces classifications :inconclusive)
                  with-confirmed (add-traces with-sub confirmed @db/app-db :confirmed classifications)
                  with-uncertain (add-traces with-confirmed inconclusive @db/app-db :inconclusive classifications)
                  with-render (if direct?
                                (add-edge with-uncertain {:from sub-id :to leaf-id :kind :render-input})
                                with-uncertain)]
              (reduce (fn [g {:keys [prop-id]}]
                        (add-edge g {:from sub-id :to prop-id :kind :data-input}))
                      with-render props)))
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
                              (when (and element (empty? components))
                                [{:code :no-reagent-owner
                                  :message "No supported owning Reagent component was found."}])))]
    (assoc ordered
           :protocol shared/protocol-version
           :revision @state/revision
           :selection-generation @state/selection-generation
           :selection {:label (react/element-label element)}
           :app-db-tree (projection/app-db-tree @db/app-db app-db-paths @state/app-db-expansions)
           :truncated? false
           :warnings warnings)))
