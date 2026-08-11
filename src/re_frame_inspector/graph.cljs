(ns re-frame-inspector.graph
  (:require [re-frame.core :as rf]
            [re-frame.db :as db]
            [re-frame.subs :as subs]
            [goog.object :as gobj]
            [re-frame-inspector.react :as react]
            [re-frame-inspector.registry :as registry]
            [re-frame-inspector.shared :as shared]
            [re-frame-inspector.state :as state]
            [re-frame-inspector.tracker :as tracker]))

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

(defn- path-value [app-db path]
  (let [wildcard-index (first (keep-indexed #(when (= shared/wildcard %2) %1) path))
        concrete (if (nil? wildcard-index) path (subvec path 0 wildcard-index))]
    (get-in app-db concrete)))

(defn- add-paths [graph sub-id analysis app-db]
  (reduce
    (fn [g path]
      (let [id (shared/stable-id :app-db-path path)
            token (value-token :app-db-path path)
            value (path-value app-db path)]
        (state/remember-value! token value)
        (-> g
            (update :nodes assoc id
                    {:id id :kind :app-db-path :label (shared/path-label path)
                     :preview (shared/bounded-preview value) :token token})
            (update :edges conj {:from id :to sub-id :kind :data-input}))))
    graph (:paths analysis)))

(declare add-subscription)

(defn- add-subscription [graph reaction query-v]
  (let [id (subscription-id query-v)]
    (if (contains? (:nodes graph) id)
      graph
      (let [registration (get @state/registrations (query-id query-v))
            value (try @reaction (catch :default error error))
            token (value-token :subscription query-v)
            input-reactions (->> (watched reaction)
                                 (keep #(when-let [q (reaction-query %)] [% q]))
                                 vec)
            raw-or-missing? (nil? registration)
            dyn-v (when (= query-v (:latest-query registration)) (:latest-dyn registration))
            layer-2? (and registration (registry/layer-2? (assoc registration
                                                              :latest-query query-v
                                                              :latest-dyn dyn-v)))
            analysis (when layer-2?
                       (tracker/replay (:computation-fn registration)
                                       @db/app-db query-v dyn-v))
            node {:id id :kind :subscription :label (pr-str query-v)
                  :preview (shared/bounded-preview value) :token token
                  :complete? (and (not raw-or-missing?)
                                  (or (nil? analysis) (:complete? analysis)))
                  :reason (cond
                            raw-or-missing? "reg-sub-raw, Subscription alpha, or registration before preload"
                            (and analysis (not (:complete? analysis))) (:reason analysis))}]
        (state/remember-value! token value)
        (let [with-node (update graph :nodes assoc id node)
              with-paths (if analysis (add-paths with-node id analysis @db/app-db) with-node)]
          (reduce
            (fn [g [input input-query]]
              (-> (add-subscription g input input-query)
                  (update :edges conj {:from (subscription-id input-query)
                                       :to id :kind :data-input})))
            with-paths input-reactions))))))

(defn snapshot [element]
  (state/reset-values!)
  (let [{:keys [components warning unsupported]} (react/owning-components element)
        element-id (react/object-id "element" element)
        element-token (value-token :element element)
        initial {:nodes {element-id {:id element-id :kind :element
                                     :label (or (react/element-label element) "No element")
                                     :preview (shared/bounded-preview element)
                                     :token element-token}}
                 :edges []}
        _ (state/remember-value! element-token element)
        with-components
        (reduce
          (fn [graph {:keys [id name reaction adapter]}]
            (let [component-token (value-token :component id)
                  graph (update graph :nodes assoc id
                                {:id id :kind :component :label name
                                 :adapter adapter :token component-token
                                 :preview (str (cljs.core/name adapter) " Reagent component")})
                  _ (state/remember-value! component-token reaction)]
              (reduce
                (fn [g watched-reaction]
                  (if-let [query-v (reaction-query watched-reaction)]
                    (-> (add-subscription g watched-reaction query-v)
                        (update :edges conj {:from (subscription-id query-v)
                                             :to id :kind :render-input}))
                    g))
                graph (watched reaction))))
          initial components)
        component-ids (mapv :id components)
        ownership (concat (map (fn [[parent child]]
                                 {:from parent :to child :kind :render-ownership})
                               (partition 2 1 component-ids))
                          (when-let [last-component (peek component-ids)]
                            [{:from last-component :to element-id :kind :render-ownership}]))
        graph (update with-components :edges into ownership)
        ordered (shared/deterministic-graph (vals (:nodes graph)) (:edges graph))
        warnings (vec (concat @state/runtime-warnings
                              (when warning [warning]) unsupported
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
           :warnings warnings)))
