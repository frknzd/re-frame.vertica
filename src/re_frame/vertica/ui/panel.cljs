(ns re-frame.vertica.ui.panel
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [re-frame.vertica.bridge :as bridge]
            [re-frame.vertica.ui.edn-tokenizer :as tokenizer]
            [re-frame.vertica.ui.graph-layout :as layout]
            [re-frame.vertica.ui.protocol :as protocol]
            [re-frame.vertica.ui.source-args :as source-args]))

(def ^:private poll-interval 150)
(def ^:private token-threshold 8000)
(def ^:private component-boxes-setting "re-frame.vertica.component-boxes")
(def ^:private select-element-message "Use Choose to select a Reagent element.")

(defn- call-bridge [method & arguments]
  (bridge/decode-response
    (case method
      :capabilities (bridge/capabilities)
      :status (bridge/status)
      :snapshot (bridge/snapshot)
      :navigate-element (bridge/navigate-element (name (first arguments)))
      :start-picker (bridge/start-picker)
      :stop-picker (bridge/stop-picker)
      :set-component-highlights (bridge/set-component-highlights (first arguments))
      :expand-node (bridge/expand-node (first arguments))
      :expand-app-db-path (apply bridge/expand-app-db-path arguments)
      (throw (js/Error. (str "Unknown panel bridge method: " (name method)))))))

(defn- token-hiccup [source]
  (map-indexed
    (fn [index {:keys [text type depth]}]
      (if (= :plain type)
        text
        ^{:key index}
        [:span {:class (str "edn-token edn-" (name type)
                            (when (= :bracket type)
                              (str " bracket-" (mod depth 6))))}
         text]))
    (tokenizer/edn-tokens source)))

(defn- deferred-edn-code [{:keys [inspected-window class-name source attrs]}]
  (r/with-let [tokens (r/atom nil)
               timer (.setTimeout inspected-window
                                  #(reset! tokens (token-hiccup source))
                                  0)]
    (into [:code (merge attrs
                        {:class (str class-name (when-not @tokens " tokenizing"))})]
          (or @tokens [source]))
    (finally (.clearTimeout inspected-window timer))))

(defn- edn-code
  ([context class-name value] (edn-code context class-name value nil))
  ([context class-name value attrs]
   (let [source (str (or value ""))]
     (if (>= (count source) token-threshold)
       ^{:key source}
       [deferred-edn-code {:inspected-window (:inspected-window context)
                           :class-name class-name
                           :source source
                           :attrs attrs}]
       (into [:code (merge attrs {:class class-name})]
             (token-hiccup source))))))

(defn- component-boxes-preference [{:keys [storage]}]
  (not= "false" (when storage (.getItem storage component-boxes-setting))))

(defn- update-component-boxes-button! [{:keys [state]} enabled?]
  (swap! state assoc :component-highlights? (boolean enabled?)))

(defn- show-error! [{:keys [state]} error]
  (swap! state assoc :status-message
         (or (.-message error) (ex-message error) (str error))))

(defn- source-basename [url]
  (let [path (first (str/split (str (or url "")) #"[?#]"))
        basename (subs path (inc (or (str/last-index-of path "/") -1)))]
    (try (js/decodeURIComponent basename) (catch :default _ basename))))

(defn- update-source-button! [{:keys [state]} location]
  (swap! state assoc :selected-source-location location))

(defn- apply-cached-argument-names [state graph]
  (update graph :nodes
          (fn [nodes]
            (mapv (fn [node]
                    (if (= :prop (:kind node))
                      (if-let [source-name
                               (get-in state [:argument-name-cache
                                              [(:component-name node) (:argument-count node)]
                                              (:argument-index node)])]
                        (assoc node :label source-name)
                        node)
                      node))
                  nodes))))

(defn- resolve-visible-argument-names! [{:keys [state] :as context}]
  (when (and (:source-resources-ready? @state) (:graph @state))
    (let [requests (->> (get-in @state [:graph :nodes])
                        (filter #(= :prop (:kind %)))
                        (keep (fn [node]
                                (let [key [(:component-name node) (:argument-count node)]]
                                  (when (and (first key)
                                             (int? (second key))
                                             (not (contains? (:argument-name-cache @state) key)))
                                    key))))
                        distinct)]
      (when (seq requests)
        (swap! state update :argument-name-cache
               (fn [cache]
                 (reduce (fn [result [component-name arity :as key]]
                           (assoc result key
                                  (source-args/resolve-argument-names
                                    (:source-index @state) component-name arity)))
                         cache requests)))
        (when-let [render! (:render! context)]
          (render! (:graph @state) {:preserve-scroll? true}))))))

(defn- resolve-selection-source-location! [{:keys [state] :as context}]
  (if (and (:source-resources-ready? @state) (:graph @state))
    (let [graph (:graph @state)
          components (layout/leaf-most-components
                       (filterv #(= :component (:kind %)) (:nodes graph))
                       (:edges graph))
          location (some #(source-args/resolve-source-location
                            (:source-index @state) (:label %))
                         components)]
      (update-source-button! context location))
    (update-source-button! context nil)))

(defn- source-resource? [url]
  (boolean (re-find #"\.(?:map|clj|cljs|cljc)(?:$|[?#])" (str url))))

(defn- resource-content [{:keys [inspected-window]} url]
  (-> (.fetch inspected-window url #js {:credentials "same-origin"})
      (.then (fn [response]
               (if (.-ok response)
                 (.text response)
                 (throw (js/Error. (str "Unable to read " url ": HTTP "
                                        (.-status response)))))))))

(defn- add-source-resource! [{:keys [state] :as context} url force?]
  (if (or (not (source-resource? url))
          (and (not force?) (contains? (:loaded-source-urls @state) url)))
    (js/Promise.resolve nil)
    (-> (resource-content context url)
        (.then (fn [content]
                 (swap! state update :source-index source-args/add-source-resource url content)
                 (swap! state update :loaded-source-urls conj url))))))

(defn- source-map-reference [source]
  (some->> (re-seq #"[#@]\s*sourceMappingURL\s*=\s*([^\s*]+)" (str source))
           last second))

(defn- decode-inline-source-map [{:keys [inspected-window]} url]
  (let [comma (.indexOf url ",")]
    (when (neg? comma) (throw (js/Error. "Invalid inline source map")))
    (let [metadata (subs url 5 comma)
          payload (subs url (inc comma))]
      (if (re-find #";base64(?:;|$)" metadata)
        (let [binary (.atob inspected-window payload)
              bytes (.from js/Uint8Array binary
                           (fn [character] (.charCodeAt character 0)))]
          (.decode (js/TextDecoder.) bytes))
        (js/decodeURIComponent payload)))))

(defn- add-script-source-map! [{:keys [state] :as context} script-url]
  (-> (resource-content context script-url)
      (.then
        (fn [script]
          (when-let [reference (source-map-reference script)]
            (if (str/starts-with? reference "data:")
              (let [url (str script-url ".inline.map")]
                (when-not (contains? (:loaded-source-urls @state) url)
                  (swap! state update :source-index source-args/add-source-resource
                         url (decode-inline-source-map context reference))
                  (swap! state update :loaded-source-urls conj url)))
              (add-source-resource! context (.-href (js/URL. reference script-url)) false)))))))

(defn- discover-resource-urls [{:keys [inspected-document inspected-window]}]
  (let [scripts (keep #(.-src %) (array-seq (.-scripts inspected-document)))
        resources (try (keep #(.-name %)
                             (array-seq (.getEntriesByType (.-performance inspected-window)
                                                           "resource")))
                       (catch :default _ []))]
    (vec (distinct (concat scripts resources)))))

(defn- settle-all [promises]
  (js/Promise.allSettled (clj->js promises)))

(defn- load-source-resources! [{:keys [state] :as context}]
  (if-let [loading (:source-resources-loading @state)]
    loading
    (let [urls (discover-resource-urls context)
          direct (filter source-resource? urls)
          scripts (filter #(re-find #"\.m?js(?:$|[?#])" %) urls)
          loading
          (-> (do
                (swap! state assoc
                       :source-resources-ready? false
                       :source-index {}
                       :loaded-source-urls #{})
                (settle-all (map #(add-source-resource! context % false) direct)))
              (.then (fn [_]
                       (settle-all (map #(add-script-source-map! context %) scripts))))
              (.then (fn [_]
                       (swap! state assoc
                              :argument-name-cache {}
                              :source-resources-ready? true)
                       (resolve-visible-argument-names! context)
                       (resolve-selection-source-location! context)))
              (.catch (fn [_]
                        (swap! state assoc :source-resources-ready? true)))
              (.finally (fn [] (swap! state assoc :source-resources-loading nil))))]
      (swap! state assoc :source-resources-loading loading)
      loading)))

(defn- component-badges
  ([context components] (component-badges context components false))
  ([{:keys [state]} components compact?]
   (let [leaves (layout/leaf-most-components components (get-in @state [:graph :edges]))]
     (when (seq leaves)
       (into
         [:span {:class (str "component-badges"
                             (when compact? " compact-component-badges"))
                 :aria-label (str "Leaf Reagent component: "
                                  (str/join "; " (map :label leaves)))}]
         (if compact?
           (let [labels (map #(or (:label %) "Anonymous component") leaves)]
             [[:span {:class "component-badge compact-component-badge"
                      :title (str "Reagent component"
                                  (when (not= 1 (count leaves)) "s")
                                  ": " (str/join "; " labels))}
               (if (= 1 (count leaves)) "C" (str "C×" (count leaves)))]])
           (map (fn [component]
                  ^{:key (:id component)}
                  [:span {:class "component-badge"
                          :title (str "Reagent component: " (:label component))}
                   (or (:label component) "Anonymous component")])
                leaves)))))))

(defn- components-for-db-node [{:keys [state]} node]
  (->> (:app-db-association-patterns @state)
       (filter #(layout/app-db-path-matches? (:association-path node) (:path %)))
       (mapcat :components)
       (reduce #(assoc %1 (:id %2) %2) {})
       vals
       (sort layout/compare-nodes)
       vec))

(declare render! db-node-view)

(defn- toggle-full-value! [{:keys [state] :as context} node showing?]
  (when-not (contains? (:loading-values @state) (:id node))
    (if showing?
      (swap! state update :expanded-values dissoc (:id node))
      (do
        (swap! state update :loading-values conj (:id node))
        (try
          (let [result (call-bridge :expand-node (:token node))]
            (when-not (:ok result)
              (throw (js/Error. (or (:error result) "Unable to expand value"))))
            (swap! state assoc-in [:expanded-values (:id node)]
                   {:token (:token node) :value (:value result)}))
          (catch :default error (show-error! context error))
          (finally (swap! state update :loading-values disj (:id node))))))))

(defn- node-view [{:keys [state] :as context} node section]
  (let [collapsed? (contains? (:collapsed-nodes @state) (:id node))
        expanded (get (:expanded-values @state) (:id node))
        showing? (= (:token expanded) (:token node))
        loading? (contains? (:loading-values @state) (:id node))
        value-title (if showing?
                      "Double-click to show the preview"
                      "Double-click to show the complete value")]
    [:article {:class (str "node-row " (name (:kind node))
                           (when (false? (:complete? node)) " partial")
                           (when collapsed? " collapsed"))
               :title (when (false? (:complete? node))
                        (or (:reason node)
                            "App-db provenance is incomplete for this subscription."))
               :data-node-id (:id node)
               :style {"--depth" (str (min (or (:depth node) 0) 8))}}
     [:div.node-content
      (into
        [:div.identity-cell
         [:div.tree-indent
          [:span.kind-dot]
          (edn-code context "node-identity" (:label node))]]
        (when (contains? #{:subscription :prop} (:kind node))
          (when-let [badges (component-badges
                              context (get (:component-associations @state) (:id node)))]
            [(update badges 1 update :class str " subscription-component-badges")])))
      (when (:value section)
        [:div {:class (str "value-cell"
                           (when (:preview-truncated? node) " expandable")
                           (when showing? " expanded")
                           (when loading? " loading"))
               :title (when (:preview-truncated? node) value-title)
               :tab-index (when (:preview-truncated? node) 0)
               :aria-busy (when loading? true)
               :on-double-click (when (:preview-truncated? node)
                                  (fn [event]
                                    (.preventDefault event)
                                    (toggle-full-value! context node showing?)))
               :on-key-down (when (:preview-truncated? node)
                              (fn [event]
                                (when (= "Enter" (.-key event))
                                  (.preventDefault event)
                                  (toggle-full-value! context node showing?))))}
         (edn-code context "node-value"
                   (if showing? (:value expanded) (or (:preview node) "—")))
         (when (:preview-truncated? node)
           [:button {:type "button"
                     :class "value-more"
                     :title (if showing?
                              "Show the shortened preview"
                              "Show the complete value")
                     :on-click (fn [event]
                                 (.preventDefault event)
                                 (.stopPropagation event)
                                 (toggle-full-value! context node showing?))}
            (if showing? "Show less" "… Show all")])])]
     [:button {:type "button"
               :class "row-toggle"
               :title (if collapsed? "Expand row" "Collapse row")
               :aria-label (if collapsed? "Expand row" "Collapse row")
               :on-click (fn [event]
                           (.stopPropagation event)
                           (swap! state update :collapsed-nodes
                                  (if collapsed? disj conj) (:id node)))}
      (if collapsed? "+" "−")]]))

(defn- db-node-state [node]
  (cond
    (= :ellipsis (:kind node)) :ellipsis
    (:exact? node) :exact
    (:touched? node) :ancestor
    :else :context))

(defn- db-collection? [node]
  (contains? #{:map :vector :set} (:kind node)))

(defn- db-node-collapsed? [state node path]
  (or (contains? (:collapsed-db-paths state) path)
      (and (layout/db-collection-starts-collapsed? node)
           (not (contains? (:expanded-default-db-paths state) path)))))

(defn- reset-app-db-view-state! [state]
  (swap! state assoc
         :collapsed-db-paths #{}
         :expanded-default-db-paths #{}
         :expanded-db-values {}))

(defn- toggle-db-node! [{:keys [state]} path collapsed?]
  (if collapsed?
    (swap! state #(-> %
                      (update :collapsed-db-paths disj path)
                      (update :expanded-default-db-paths conj path)))
    (swap! state #(-> %
                      (update :expanded-default-db-paths disj path)
                      (update :collapsed-db-paths conj path)))))

(defn- render-db-value [context text path class-name]
  [:span {:class (str "db-tree-value " class-name)}
   (edn-code context "db-tree-code" text {:data-path path})])

(defn- replace-db-branch [node path replacement]
  (cond
    (= path (:path-label node)) replacement
    (not (db-collection? node)) node
    :else (update node :children
                  (fn [children]
                    (mapv #(update % :node replace-db-branch path replacement) children)))))

(defn- load-more-db-context! [{:keys [state] :as context} path visible-count]
  (when-not (contains? (:loading-db-paths @state) path)
    (swap! state update :loading-db-paths conj path)
    (try
      (let [result (call-bridge :expand-app-db-path path (or visible-count 0))]
        (when-not (:ok result)
          (throw (js/Error. (or (:error result) "Unable to load more app-db context"))))
        (swap! state update-in [:graph :app-db-tree]
               replace-db-branch path (:node result)))
      (catch :default error (show-error! context error))
      (finally (swap! state update :loading-db-paths disj path)))))

(defn- toggle-full-db-value! [{:keys [state] :as context} path showing?]
  (when-not (contains? (:loading-db-paths @state) path)
    (if showing?
      (swap! state update :expanded-db-values dissoc path)
      (do
        (swap! state update :loading-db-paths conj path)
        (try
          (let [result (call-bridge :expand-app-db-path path)]
            (when (or (not (:ok result)) (nil? (:value result)))
              (throw (js/Error. (or (:error result) "Unable to expand app-db value"))))
            (swap! state assoc-in [:expanded-db-values path] (:value result)))
          (catch :default error (show-error! context error))
          (finally (swap! state update :loading-db-paths disj path)))))))

(defn- compact-db-vector-entry [{:keys [state] :as context} node key-label]
  (let [path (or (:path-label node) "[]")
        path-title (str "app-db " path)
        full-value (get (:expanded-db-values @state) path)
        showing? (some? full-value)
        loading? (contains? (:loading-db-paths @state) path)]
    (into
      [:div {:class (str "db-vector-entry " (name (db-node-state node)))
             :title path-title}
       (edn-code context "db-tree-code db-tree-key db-vector-key" key-label
                 {:data-path path :title path-title})
       (render-db-value context
                        (if showing? full-value (or (:text node) "nil"))
                        path
                        (str "leaf-value" (when showing? " expanded")))
       (when (:preview-truncated? node)
         [:button {:type "button"
                   :class (str "db-value-more" (when loading? " loading"))
                   :disabled loading?
                   :title (if showing?
                            (str "Collapse app-db value " path)
                            (str "Show complete app-db value " path))
                   :aria-label (if showing?
                                 (str "Collapse app-db value " path)
                                 (str "Show complete app-db value " path))
                   :on-click #(toggle-full-db-value! context path showing?)}
          (if showing? "Less" "All")])]
      (when-let [badges (component-badges
                          context (components-for-db-node context node) true)]
        [badges]))))

(defn- db-node-children [context node depth]
  (let [children
        (if (= :vector (:kind node))
          (mapcat
            (fn [{:keys [layout entries]}]
              (if (= :compact layout)
                [[:div {:class "db-vector-entries"
                        :style {"--db-depth" (str (inc depth))}
                        :key (str "compact-" (:path-label (:node (first entries))))}
                  (for [{:keys [node key]} entries]
                    ^{:key (:path-label node)}
                    [compact-db-vector-entry context node key])]]
                (for [{:keys [node key]} entries]
                  ^{:key (:path-label node)}
                  [db-node-view context node key (inc depth)])))
            (layout/group-db-vector-entries (:children node)))
          (for [{:keys [node key]} (:children node)]
            ^{:key (:path-label node)}
            [db-node-view context node key (inc depth)]))]
    (into [:div.db-tree-children] children)))

(defn- db-node-view
  ([context node] (db-node-view context node nil 0))
  ([{:keys [state] :as context} node key-label depth]
   (let [path (or (:path-label node) "[]")
         collection? (db-collection? node)
         collapsed? (and collection? (db-node-collapsed? @state node path))
         default-collapsed? (and collection? (layout/db-collection-starts-collapsed? node))
         full-value (get (:expanded-db-values @state) path)
         showing? (some? full-value)
         loading? (contains? (:loading-db-paths @state) path)
         toggle-title (str (if collapsed? "Expand " "Collapse ") path)
         line
         (if (= :ellipsis (:kind node))
           [:div {:class "db-tree-line" :style {"--db-depth" (str depth)}}
            [:span.db-tree-toggle-spacer]
            [:button {:type "button"
                      :class (str "db-tree-more" (when loading? " loading"))
                      :disabled loading?
                      :title (str "Load more entries from " path)
                      :on-click #(load-more-db-context!
                                   context path (:visible-count node))}
             (or (:text node) "… more")]]
           (into
             [:div {:class "db-tree-line" :style {"--db-depth" (str depth)}}
              (if collection?
                [:button {:type "button"
                          :class "db-tree-toggle"
                          :title toggle-title
                          :aria-label toggle-title
                          :on-click #(toggle-db-node! context path collapsed?)}
                 (if collapsed? "+" "−")]
                [:span.db-tree-toggle-spacer])
              (when (some? key-label)
                (edn-code context "db-tree-code db-tree-key" key-label
                          {:data-path path :title (str "app-db " path)}))
              (if (= :summary (:kind node))
                [:button {:type "button"
                          :class (str "db-tree-summary" (when loading? " loading"))
                          :disabled loading?
                          :title (str "Load entries from " path)
                          :aria-label (str "Load entries from " path)
                          :on-click #(load-more-db-context! context path 0)}
                 (or (:text node) "…")]
                (render-db-value
                  context
                  (if collection? (:open node)
                      (if showing? full-value (or (:text node) "nil")))
                  path
                  (if collection? "collection-value" "leaf-value")))
              (when (:preview-truncated? node)
                [:button {:type "button"
                          :class (str "db-value-more" (when loading? " loading"))
                          :disabled loading?
                          :title (if showing?
                                   (str "Collapse app-db value " path)
                                   (str "Show complete app-db value " path))
                          :on-click #(toggle-full-db-value! context path showing?)}
                 (if showing? "Show less" "… Show all")])
              (when (and collection? collapsed?)
                [:button {:type "button"
                          :class "db-tree-collapsed-mark"
                          :title (if default-collapsed?
                                   (str "Expand " path "; all " (:child-count node)
                                        " entries contribute")
                                   (str "Expand " path))
                          :on-click #(toggle-db-node! context path true)}
                 (if default-collapsed?
                   (str "… " (:child-count node) " involved")
                   "…")])]
             (when-let [badges (component-badges
                                 context (components-for-db-node context node))]
               [badges])))]
     (cond-> [:div {:class (str "db-tree-node " (name (db-node-state node))
                                (when collapsed? " collapsed"))}
              line]
       (and collection? (not collapsed?))
       (conj (db-node-children context node depth)
             [:div {:class "db-tree-line db-tree-close"
                    :style {"--db-depth" (str depth)}}
              [:span.db-tree-toggle-spacer]
              (render-db-value context (:close node) path "collection-value")])))))

(defn- app-db-section [context section app-db-tree]
  [:section {:class (str "graph-section " (name (:kind section)) " db-tree-section")}
   [:header.section-header
    [:h2.section-title
     (:title section)
     [:span.section-count (count (:nodes section))]]]
   (when (and (seq (:nodes section)) app-db-tree)
     [:div.db-tree [db-node-view context app-db-tree]])])

(defn- section-view [{:keys [state] :as context} section graph]
  (if (= :app-db-path (:kind section))
    [app-db-section context section (:app-db-tree graph)]
    [:section {:class (str "graph-section " (name (:kind section))
                           (if (:value section) " has-values" " single-column"))}
     [:header.section-header
      [:h2.section-title
       (:title section)
       [:span.section-count (count (:nodes section))]]]
     (when (seq (:nodes section))
       [:div.column-headings
        [:span.identity-heading (:identity section)]
        (when (:value section)
          [:span.value-heading (:value section)])])
     (when (seq (:nodes section))
       (if (:levels section)
         (for [{:keys [level nodes]} (:levels section)
               :let [collapsed? (contains? (:collapsed-subscription-levels @state) level)]]
           ^{:key level}
           [:div {:class (str "subscription-level" (when collapsed? " collapsed"))}
            [:button {:type "button"
                      :class "level-header"
                      :title (str (if collapsed? "Expand" "Collapse")
                                  " subscription level " level)
                      :aria-expanded (not collapsed?)
                      :on-click #(swap! state update :collapsed-subscription-levels
                                        (if collapsed? disj conj) level)}
             [:span.level-chevron (if collapsed? "▸" "▾")]
             [:span.level-title (str "LEVEL " level)]
             [:span.level-count (count nodes)]]
            (when-not collapsed?
              (into [:div.section-rows]
                    (map (fn [node]
                           ^{:key (:id node)} [node-view context node section])
                         nodes)))])
         (into [:div.section-rows]
               (map (fn [node]
                      ^{:key (:id node)} [node-view context node section])
                    (:nodes section)))))]))

(defn- sync-subscription-level-state! [state sections nodes]
  (let [selection-id (:id (first (filter #(= :element (:kind %)) nodes)))
        previous-selection (:subscription-selection-id @state)
        levels (set (map :level (:levels (first (filter #(= :subscription (:kind %)) sections)))))]
    (when (not= selection-id previous-selection)
      (swap! state assoc
             :subscription-selection-id selection-id
             :subscription-levels-initialized? false))
    (if-not (:subscription-levels-initialized? @state)
      (swap! state assoc
             :collapsed-subscription-levels (disj levels 0)
             :subscription-levels-initialized? true)
      (swap! state
             (fn [current]
               (let [known (:known-subscription-levels current)
                     retained (set (filter levels (:collapsed-subscription-levels current)))
                     added (set (filter #(and (not= 0 %) (not (contains? known %))) levels))]
                 (assoc current :collapsed-subscription-levels (into retained added))))))
    (swap! state assoc :known-subscription-levels levels)))

(defn- update-navigation! [{:keys [state]} navigation]
  (swap! state assoc :navigation (or navigation {})))

(defn- render!
  ([context graph] (render! context graph {}))
  ([{:keys [state canvas] :as context} next-graph {:keys [preserve-scroll?]}]
   (let [canvas-element @canvas
         next-graph (apply-cached-argument-names @state next-graph)
         nodes (or (:nodes next-graph) [])
         edges (or (:edges next-graph) [])
         sections (layout/build-sections nodes edges)
         selected (get-in next-graph [:selection :label])
         has-selection? (or selected
                            (some #(and (= :element (:kind %))
                                        (not= "No element" (:label %))) nodes))
         next-generation (:selection-generation next-graph)]
     (swap! state assoc
            :graph next-graph
            :sections sections
            :has-selection? (boolean has-selection?)
            :empty-message select-element-message
            :navigation (or (:navigation next-graph) {}))
     (sync-subscription-level-state! state sections nodes)
     (let [associations (layout/build-component-associations nodes edges)]
       (swap! state assoc
              :component-associations associations
              :app-db-association-patterns
              (->> nodes
                   (filter #(and (= :app-db-path (:kind %))
                                 (contains? associations (:id %))))
                   (mapv (fn [node]
                           {:path (:association-path node)
                            :components (get associations (:id node))})))))
     (when (and (number? next-generation)
                (not= next-generation (:db-selection-generation @state)))
       (reset-app-db-view-state! state)
       (swap! state assoc :db-selection-generation next-generation))
     (swap! state update :collapsed-nodes
            #(set (filter (set (map :id nodes)) %)))
     (swap! state update :expanded-values
            (fn [expanded]
              (let [by-id (into {} (map (juxt :id identity)) nodes)]
                (into {} (filter (fn [[id cached]]
                                   (= (:token cached) (:token (get by-id id))))) expanded))))
     (let [warnings (:warnings next-graph)]
       (swap! state assoc :status-message
              (if (seq warnings)
                (str/join " · " (map :message warnings))
                (str "Connected · " (count nodes) " nodes"))))
     (when (and (not preserve-scroll?)
                canvas-element
                (not= selected (:last-selection @state)))
       (.scrollTo canvas-element #js {:top 0 :left 0}))
     (swap! state assoc :last-selection selected)
     (resolve-visible-argument-names! context)
     (resolve-selection-source-location! context))))

(defn- render-snapshot!
  ([context graph] (render-snapshot! context graph {}))
  ([{:keys [state] :as context} graph options]
   (when graph
     (let [next-revision (:revision graph)]
       (when-not (and (number? next-revision)
                      (< next-revision (:revision @state)))
         (render! context graph options)
         (when (number? next-revision)
           (swap! state assoc :revision next-revision)))))))

(defn- update-bridge-state! [{:keys [state] :as context} ready? message]
  (swap! state assoc :bridge-ready? ready?)
  (if-not ready?
    (do
      (swap! state assoc
             :graph nil
             :sections []
             :revision -1
             :db-selection-generation nil
             :has-selection? false
             :empty-message
             "This app may be missing re-frame.vertica.preload. Add it to the development build, restart the build, and reload the page.")
      (reset-app-db-view-state! state)
      (update-source-button! context nil))
    (when-not (:graph @state)
      (swap! state assoc :has-selection? false :empty-message select-element-message)))
  (when (seq message) (swap! state assoc :status-message message))
  (update-navigation! context (get-in @state [:graph :navigation])))

(defn- navigate-selected! [{:keys [state] :as context} direction]
  (when-not (:navigation-running? @state)
    (swap! state assoc :navigation-running? true)
    (update-navigation! context (get-in @state [:graph :navigation]))
    (try
      (let [result (call-bridge :navigate-element direction)]
        (when (false? (:ok result))
          (update-navigation! context (:navigation result))
          (throw (js/Error. (or (:error result) "Unable to navigate the element tree"))))
        (render-snapshot! context result))
      (catch :default error (show-error! context error))
      (finally
        (swap! state assoc :navigation-running? false)
        (update-navigation! context (get-in @state [:graph :navigation]))))))

(defn- connect! [{:keys [state] :as context}]
  (when-not (:connection-running? @state)
    (swap! state assoc :connection-running? true)
    (try
      (let [capabilities (call-bridge :capabilities)
            message (protocol/compatibility-message capabilities)]
        (if (not= protocol/protocol-version (:protocol capabilities))
          (update-bridge-state! context false message)
          (do
            (update-bridge-state! context true message)
            (load-source-resources! context)
            (let [status (call-bridge :set-component-highlights
                                      (component-boxes-preference context))]
              (update-component-boxes-button!
                context (boolean (:component-highlights status)))))))
      (catch :default error
        (update-bridge-state! context false (or (.-message error) (str error))))
      (finally (swap! state assoc :connection-running? false)))))

(defn- refresh-inspector! [{:keys [state] :as context}]
  (when-not (:refresh-running? @state)
    (swap! state assoc
           :refresh-running? true
           :status-message "Refreshing provenance and source maps…")
    (-> (load-source-resources! context)
        (.then (fn [_]
                 (render-snapshot! context (call-bridge :snapshot)
                                   {:preserve-scroll? true})))
        (.catch #(show-error! context %))
        (.finally (fn []
                    (swap! state assoc :refresh-running? false))))))

(declare poll!)

(defn- schedule-poll! [{:keys [state inspected-window] :as context}]
  (when (:running? @state)
    (swap! state assoc :poll-timer
           (.setTimeout inspected-window #(poll! context) poll-interval))))

(defn- poll! [{:keys [state on-picking-change] :as context}]
  (try
    (if-not (:bridge-ready? @state)
      (connect! context)
      (let [status (call-bridge :status)]
        (if-not status
          (update-bridge-state! context false (protocol/compatibility-message nil))
          (do
            (swap! state assoc :picker-active? (boolean (:picker-active status)))
            (on-picking-change (boolean (:picker-active status)))
            (update-component-boxes-button!
              context (boolean (:component-highlights status)))
            (update-navigation! context (:navigation status))
            (when (not= (:revision status) (:revision @state))
              (render-snapshot! context (call-bridge :snapshot)))))))
    (catch :default _)
    (finally (schedule-poll! context))))

(defn start! [{:keys [state inspected-window] :as context}]
  (when-not (:running? @state)
    (swap! state assoc :running? true)
    (connect! context)
    (when-let [timer (:poll-timer @state)] (.clearTimeout inspected-window timer))
    (schedule-poll! context)))

(defn stop! [{:keys [state inspected-window on-picking-change]}]
  (swap! state assoc :running? false :picker-active? false)
  (when-let [timer (:poll-timer @state)] (.clearTimeout inspected-window timer))
  (swap! state assoc :poll-timer nil)
  (on-picking-change false)
  (try (call-bridge :stop-picker) (catch :default _))
  (try (call-bridge :set-component-highlights false) (catch :default _)))

(defn set-floating! [{:keys [state]} floating?]
  (swap! state assoc :floating? (boolean floating?)))

(defn set-status! [{:keys [state]} message]
  (swap! state assoc :status-message message))

(defn- open-selected-source! [{:keys [state open-source] :as context}]
  (when-let [location (:selected-source-location @state)]
    (try
      (open-source (:url location) location)
      (catch :default error (show-error! context error)))))

(defn- toggle-component-boxes! [{:keys [state storage] :as context}]
  (let [enabled? (not (:component-highlights? @state))]
    (when storage
      (try (.setItem storage component-boxes-setting (str enabled?))
           (catch :default _)))
    (update-component-boxes-button! context enabled?)
    (try
      (let [status (call-bridge :set-component-highlights enabled?)]
        (update-component-boxes-button!
          context (boolean (:component-highlights status))))
      (catch :default error (show-error! context error)))))

(defn- toggle-picker! [{:keys [state on-picking-change] :as context}]
  (try
    (let [status (call-bridge (if (:picker-active? @state)
                                :stop-picker
                                :start-picker))
          active? (boolean (:picker-active status))]
      (swap! state assoc :picker-active? active?)
      (update-component-boxes-button!
        context (boolean (:component-highlights status)))
      (update-navigation! context (:navigation status))
      (on-picking-change active?))
    (catch :default error (show-error! context error))))

(def ^:private navigation-controls
  [[:parent "↑" "Select parent element"]
   [:child "↓" "Select first child element"]
   [:previous "←" "Select previous sibling"]
   [:next "→" "Select next sibling"]])

(defn- panel-view
  [{:keys [state canvas on-close on-detach] :as context}]
  (let [{:keys [bridge-ready? component-highlights? empty-message floating? graph
                has-selection? navigation navigation-running? picker-active?
                refresh-running? sections selected-source-location status-message]}
        @state
        source-title (when selected-source-location
                       (str "Open " (:component-name selected-source-location)
                            " at " (:url selected-source-location)
                            ":" (:line selected-source-location)
                            ":" (:column selected-source-location)))
        detach-title (if floating?
                       "Attach the panel to the application"
                       "Detach the panel into a floating window")]
    [:div {:class (str "panel-shell" (when-not bridge-ready? " preload-missing"))}
     [:header
      [:strong "re-frame.vertica"]
      [:span#status status-message]
      [:div#tree-nav {:role "group" :aria-label "Navigate selected DOM element"}
       (for [[direction label title] navigation-controls]
         ^{:key direction}
         [:button {:type "button"
                   :data-direction (name direction)
                   :title title
                   :aria-label title
                   :disabled (or (not bridge-ready?)
                                 navigation-running?
                                 (not (get navigation direction)))
                   :on-click #(navigate-selected! context direction)}
          label])]
      [:button#open-source
       {:type "button"
        :hidden (nil? selected-source-location)
        :title source-title
        :aria-label source-title
        :on-click #(open-selected-source! context)}
       (when selected-source-location
         (str "↗ " (source-basename (:url selected-source-location))
              ":" (:line selected-source-location)))]
      [:button#refresh
       {:type "button"
        :class (when refresh-running? "loading")
        :disabled (or (not bridge-ready?) refresh-running?)
        :aria-busy refresh-running?
        :title "Refresh provenance and source-map argument names"
        :on-click #(refresh-inspector! context)}
       "↻ Refresh"]
      [:button#component-boxes
       {:type "button"
        :aria-pressed component-highlights?
        :disabled (not bridge-ready?)
        :title (if component-highlights?
                 "Hide Reagent component boxes"
                 "Show Reagent component boxes")
        :on-click #(toggle-component-boxes! context)}
       "◇ Boxes"]
      [:button#choose
       {:type "button"
        :data-active picker-active?
        :disabled (not bridge-ready?)
        :title "Choose an element in the page"
        :on-click #(toggle-picker! context)}
       (if picker-active? "× Cancel" "⌖ Choose")]
      [:button#detach
       {:type "button"
        :title detach-title
        :aria-label detach-title
        :on-click on-detach}
       (if floating? "↙ Attach" "↗ Detach")]
      [:button#close
       {:type "button"
        :title "Close panel (Ctrl+Shift+V)"
        :aria-label "Close panel"
        :on-click on-close}
       "×"]]
     [:main
      [:section#canvas-wrap
       {:ref #(reset! canvas %)}
       (into
         [:div#graph
          {:role "region"
           :aria-label "re-frame data flow from app-db paths to the selected element"}]
         (map (fn [section]
                ^{:key (:kind section)} [section-view context section graph])
              sections))
       [:div#empty {:hidden has-selection?} empty-message]]]]))

(defn mount!
  [{:keys [mount-node storage inspected-document inspected-window
           on-close on-detach on-picking-change open-source]
    :or {on-close (fn [])
         on-detach (fn [])
         on-picking-change (fn [_])}}]
  (let [state (r/atom {:graph nil
                       :sections []
                       :revision -1
                       :collapsed-nodes #{}
                       :collapsed-db-paths #{}
                       :expanded-default-db-paths #{}
                       :collapsed-subscription-levels #{}
                       :known-subscription-levels #{}
                       :subscription-levels-initialized? false
                       :subscription-selection-id nil
                       :db-selection-generation nil
                       :expanded-values {}
                       :expanded-db-values {}
                       :loading-values #{}
                       :loading-db-paths #{}
                       :last-selection nil
                       :source-index {}
                       :argument-name-cache {}
                       :loaded-source-urls #{}
                       :source-resources-ready? false
                       :source-resources-loading nil
                       :selected-source-location nil
                       :poll-timer nil
                       :running? false
                       :bridge-ready? false
                       :connection-running? false
                       :navigation-running? false
                       :refresh-running? false
                       :picker-active? false
                       :floating? false
                       :has-selection? false
                       :empty-message select-element-message
                       :status-message "Ready"
                       :navigation {}
                       :component-highlights?
                       (component-boxes-preference {:storage storage})
                       :component-associations {}
                       :app-db-association-patterns []})
        canvas (atom nil)
        context* (atom nil)
        context {:mount-node mount-node
                 :storage storage
                 :inspected-document inspected-document
                 :inspected-window inspected-window
                 :on-picking-change on-picking-change
                 :on-close on-close
                 :on-detach on-detach
                 :open-source open-source
                 :canvas canvas
                 :state state
                 :render! (fn [graph options] (render! @context* graph options))}]
    (reset! context* context)
    (rdom/render [panel-view context] mount-node)
    context))
