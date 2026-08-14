(ns re-frame.vertica.ui.panel
  (:require [clojure.string :as str]
            [re-frame.vertica.bridge :as bridge]
            [re-frame.vertica.ui.edn-tokenizer :as tokenizer]
            [re-frame.vertica.ui.graph-layout :as layout]
            [re-frame.vertica.ui.protocol :as protocol]
            [re-frame.vertica.ui.source-args :as source-args]))

(def ^:private poll-interval 150)
(def ^:private token-threshold 8000)
(def ^:private component-boxes-setting "re-frame.vertica.component-boxes")
(def ^:private select-element-message "Use Pick to select a Reagent element.")

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

(defn- query [root selector] (.querySelector root selector))

(defn- element
  ([context tag class-name] (element context tag class-name nil))
  ([{:keys [root]} tag class-name text]
   (let [node (.createElement (.-ownerDocument root) tag)]
     (when (seq class-name) (set! (.-className node) class-name))
     (when (some? text) (set! (.-textContent node) (str text)))
     node)))

(defn- append-tokenized! [{:keys [root] :as context} code text]
  (let [document (.-ownerDocument root)
        fragment (.createDocumentFragment document)]
    (doseq [{:keys [text type depth]} (tokenizer/edn-tokens text)]
      (if (= :plain type)
        (.append fragment (.createTextNode document text))
        (let [depth-class (if (= :bracket type) (str " bracket-" (mod depth 6)) "")]
          (.append fragment (element context "span"
                                     (str "edn-token edn-" (name type) depth-class)
                                     text)))))
    (.replaceChildren code fragment)))

(defn- edn-code [context class-name value]
  (let [code (element context "code" class-name)
        source (str (or value ""))]
    (if (>= (count source) token-threshold)
      (do
        (set! (.-textContent code) source)
        (.add (.-classList code) "tokenizing")
        (.setTimeout (:inspected-window context)
                     (fn []
                       (when (.-isConnected code)
                         (append-tokenized! context code source)
                         (.remove (.-classList code) "tokenizing")))
                     0))
      (append-tokenized! context code source))
    code))

(defn- component-boxes-preference [{:keys [storage]}]
  (not= "false" (when storage (.getItem storage component-boxes-setting))))

(defn- update-component-boxes-button! [{:keys [elements]} enabled?]
  (let [button (:component-boxes elements)]
    (.setAttribute button "aria-pressed" (str (boolean enabled?)))
    (set! (.-title button) (if enabled?
                            "Hide Reagent component boxes"
                            "Show Reagent component boxes"))))

(defn- show-error! [{:keys [elements]} error]
  (set! (.-textContent (:status elements))
        (or (.-message error) (ex-message error) (str error))))

(defn- source-basename [url]
  (let [path (first (str/split (str (or url "")) #"[?#]"))
        basename (subs path (inc (or (str/last-index-of path "/") -1)))]
    (try (js/decodeURIComponent basename) (catch :default _ basename))))

(defn- update-source-button! [{:keys [elements state]} location]
  (swap! state assoc :selected-source-location location)
  (let [button (:open-source elements)]
    (set! (.-hidden button) (nil? location))
    (when location
      (let [label (str (source-basename (:url location)) ":" (:line location))
            title (str "Open " (:component-name location) " at " (:url location)
                       ":" (:line location) ":" (:column location))]
        (set! (.-textContent button) (str "↗ " label))
        (set! (.-title button) title)
        (.setAttribute button "aria-label" title)))))

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

(defn- render-component-badges
  ([context components] (render-component-badges context components false))
  ([{:keys [state] :as context} components compact?]
   (let [leaves (layout/leaf-most-components components (get-in @state [:graph :edges]))]
     (when (seq leaves)
       (let [badges (element context "span"
                              (str "component-badges"
                                   (when compact? " compact-component-badges")))]
         (.setAttribute badges "aria-label"
                        (str "Leaf Reagent component: "
                             (str/join "; " (map :label leaves))))
         (if compact?
           (let [labels (map #(or (:label %) "Anonymous component") leaves)
                 badge (element context "span"
                                "component-badge compact-component-badge"
                                (if (= 1 (count leaves)) "C" (str "C×" (count leaves))))]
             (set! (.-title badge)
                   (str "Reagent component" (when (not= 1 (count leaves)) "s")
                        ": " (str/join "; " labels)))
             (.append badges badge))
           (doseq [component leaves]
             (let [badge (element context "span" "component-badge"
                                  (or (:label component) "Anonymous component"))]
               (set! (.-title badge) (str "Reagent component: " (:label component)))
               (.append badges badge))))
         badges)))))

(defn- components-for-db-node [{:keys [state]} node]
  (->> (:app-db-association-patterns @state)
       (filter #(layout/app-db-path-matches? (:association-path node) (:path %)))
       (mapcat :components)
       (reduce #(assoc %1 (:id %2) %2) {})
       vals
       (sort layout/compare-nodes)
       vec))

(declare render! render-db-node)

(defn- toggle-full-value! [{:keys [state] :as context} value node showing?]
  (when-not (.contains (.-classList value) "loading")
    (if showing?
      (do (swap! state update :expanded-values dissoc (:id node))
          (render! context (:graph @state) {:preserve-scroll? true}))
      (do
        (.add (.-classList value) "loading")
        (.setAttribute value "aria-busy" "true")
        (try
          (let [result (call-bridge :expand-node (:token node))]
            (when-not (:ok result)
              (throw (js/Error. (or (:error result) "Unable to expand value"))))
            (swap! state assoc-in [:expanded-values (:id node)]
                   {:token (:token node) :value (:value result)})
            (render! context (:graph @state) {:preserve-scroll? true}))
          (catch :default error
            (.remove (.-classList value) "loading")
            (.removeAttribute value "aria-busy")
            (show-error! context error)))))))

(defn- render-node [{:keys [state] :as context} node section]
  (let [collapsed? (contains? (:collapsed-nodes @state) (:id node))
        row (element context "article"
                     (str "node-row " (name (:kind node))
                          (when (false? (:complete? node)) " partial")
                          (when collapsed? " collapsed")))]
    (when (false? (:complete? node))
      (set! (.-title row) (or (:reason node)
                              "App-db provenance is incomplete for this subscription.")))
    (.setAttribute row "data-node-id" (:id node))
    (.setProperty (.-style row) "--depth" (str (min (or (:depth node) 0) 8)))
    (let [content (element context "div" "node-content")
          identity (element context "div" "identity-cell")
          tree (element context "div" "tree-indent")]
      (.append tree (element context "span" "kind-dot"))
      (.append tree (edn-code context "node-identity" (:label node)))
      (.append identity tree)
      (when (contains? #{:subscription :prop} (:kind node))
        (when-let [badges (render-component-badges
                           context (get (:component-associations @state) (:id node)))]
          (.add (.-classList badges) "subscription-component-badges")
          (.append identity badges)))
      (.append content identity)
      (when (:value section)
        (let [value (element context "div" "value-cell")
              expanded (get (:expanded-values @state) (:id node))
              showing? (= (:token expanded) (:token node))]
          (.append value (edn-code context "node-value"
                                   (if showing? (:value expanded) (or (:preview node) "—"))))
          (when (:preview-truncated? node)
            (.add (.-classList value) "expandable")
            (when showing? (.add (.-classList value) "expanded"))
            (set! (.-title value) (if showing?
                                   "Double-click to show the preview"
                                   "Double-click to show the complete value"))
            (let [more (element context "button" "value-more"
                                (if showing? "Show less" "… Show all"))]
              (set! (.-type more) "button")
              (set! (.-title more) (if showing?
                                    "Show the shortened preview"
                                    "Show the complete value"))
              (.addEventListener more "click"
                                 (fn [event]
                                   (.preventDefault event)
                                   (.stopPropagation event)
                                   (toggle-full-value! context value node showing?)))
              (.append value more))
            (set! (.-tabIndex value) 0)
            (.addEventListener value "dblclick"
                               (fn [event]
                                 (.preventDefault event)
                                 (toggle-full-value! context value node showing?)))
            (.addEventListener value "keydown"
                               (fn [event]
                                 (when (= "Enter" (.-key event))
                                   (.preventDefault event)
                                   (toggle-full-value! context value node showing?)))))
          (.append content value)))
      (let [toggle (element context "button" "row-toggle" (if collapsed? "+" "−"))]
        (set! (.-type toggle) "button")
        (set! (.-title toggle) (if collapsed? "Expand row" "Collapse row"))
        (.setAttribute toggle "aria-label" (.-title toggle))
        (.addEventListener toggle "click"
                           (fn [event]
                             (.stopPropagation event)
                             (swap! state update :collapsed-nodes
                                    (if collapsed? disj conj) (:id node))
                             (render! context (:graph @state) {:preserve-scroll? true})))
        (.append row content toggle)))
    row))

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

(defn- toggle-db-node! [{:keys [state] :as context} path collapsed?]
  (if collapsed?
    (swap! state #(-> %
                      (update :collapsed-db-paths disj path)
                      (update :expanded-default-db-paths conj path)))
    (swap! state #(-> %
                      (update :expanded-default-db-paths disj path)
                      (update :collapsed-db-paths conj path))))
  (render! context (:graph @state) {:preserve-scroll? true}))

(defn- render-db-value [context text path class-name]
  (let [value (element context "span" (str "db-tree-value " class-name))]
    (.append value (edn-code context "db-tree-code" text))
    value))

(defn- replace-db-branch [node path replacement]
  (cond
    (= path (:path-label node)) replacement
    (not (db-collection? node)) node
    :else (update node :children
                  (fn [children]
                    (mapv #(update % :node replace-db-branch path replacement) children)))))

(defn- load-more-db-context! [{:keys [state] :as context} button path visible-count]
  (when-not (.contains (.-classList button) "loading")
    (.add (.-classList button) "loading")
    (set! (.-disabled button) true)
    (try
      (let [result (call-bridge :expand-app-db-path path (or visible-count 0))]
        (when-not (:ok result)
          (throw (js/Error. (or (:error result) "Unable to load more app-db context"))))
        (swap! state update-in [:graph :app-db-tree]
               replace-db-branch path (:node result))
        (render! context (:graph @state) {:preserve-scroll? true}))
      (catch :default error
        (.remove (.-classList button) "loading")
        (set! (.-disabled button) false)
        (show-error! context error)))))

(defn- toggle-full-db-value! [{:keys [state] :as context} button node path showing?]
  (when-not (.contains (.-classList button) "loading")
    (if showing?
      (do (swap! state update :expanded-db-values dissoc path)
          (render! context (:graph @state) {:preserve-scroll? true}))
      (do
        (.add (.-classList button) "loading")
        (set! (.-disabled button) true)
        (try
          (let [result (call-bridge :expand-app-db-path path)]
            (when (or (not (:ok result)) (nil? (:value result)))
              (throw (js/Error. (or (:error result) "Unable to expand app-db value"))))
            (swap! state assoc-in [:expanded-db-values path] (:value result))
            (render! context (:graph @state) {:preserve-scroll? true}))
          (catch :default error
            (.remove (.-classList button) "loading")
            (set! (.-disabled button) false)
            (show-error! context error)))))))

(defn- render-compact-db-vector-entry [{:keys [state] :as context} node key-label]
  (let [path (or (:path-label node) "[]")
        entry (element context "div" (str "db-vector-entry " (name (db-node-state node))))
        path-title (str "app-db " path)
        key-code (edn-code context "db-tree-code db-tree-key db-vector-key" key-label)]
    (set! (.-title entry) path-title)
    (set! (.. key-code -dataset -path) path)
    (set! (.-title key-code) path-title)
    (.append entry key-code)
    (let [full-value (get (:expanded-db-values @state) path)
          showing? (some? full-value)]
      (.append entry (render-db-value context
                                      (if showing? full-value (or (:text node) "nil"))
                                      path
                                      (str "leaf-value" (when showing? " expanded"))))
      (when (:preview-truncated? node)
        (let [more (element context "button" "db-value-more"
                            (if showing? "Less" "All"))]
          (set! (.-type more) "button")
          (set! (.-title more) (if showing?
                                (str "Collapse app-db value " path)
                                (str "Show complete app-db value " path)))
          (.setAttribute more "aria-label" (.-title more))
          (.addEventListener more "click"
                             #(toggle-full-db-value! context more node path showing?))
          (.append entry more)))
      (when-let [badges (render-component-badges
                         context (components-for-db-node context node) true)]
        (.append entry badges)))
    entry))

(defn- render-db-node
  ([context node] (render-db-node context node nil 0))
  ([{:keys [state] :as context} node key-label depth]
   (let [path (or (:path-label node) "[]")
         collection? (db-collection? node)
         collapsed? (and collection? (db-node-collapsed? @state node path))
         default-collapsed? (and collection? (layout/db-collection-starts-collapsed? node))
         branch (element context "div"
                         (str "db-tree-node " (name (db-node-state node))
                              (when collapsed? " collapsed")))
         line (element context "div" "db-tree-line")]
     (.setProperty (.-style line) "--db-depth" (str depth))
     (if (= :ellipsis (:kind node))
       (do
         (.append line (element context "span" "db-tree-toggle-spacer"))
         (let [more (element context "button" "db-tree-more" (or (:text node) "… more"))]
           (set! (.-type more) "button")
           (set! (.-title more) (str "Load more entries from " path))
           (.addEventListener more "click"
                              #(load-more-db-context! context more path (:visible-count node)))
           (.append line more))
         (.append branch line))
       (do
         (if collection?
           (let [toggle (element context "button" "db-tree-toggle"
                                 (if collapsed? "+" "−"))]
             (set! (.-type toggle) "button")
             (set! (.-title toggle) (str (if collapsed? "Expand " "Collapse ") path))
             (.setAttribute toggle "aria-label" (.-title toggle))
             (.addEventListener toggle "click"
                                #(toggle-db-node! context path collapsed?))
             (.append line toggle))
           (.append line (element context "span" "db-tree-toggle-spacer")))
         (when (some? key-label)
           (let [key-code (edn-code context "db-tree-code db-tree-key" key-label)]
             (set! (.. key-code -dataset -path) path)
             (set! (.-title key-code) (str "app-db " path))
             (.append line key-code)))
         (if (= :summary (:kind node))
           (let [summary (element context "button" "db-tree-summary" (or (:text node) "…"))]
             (set! (.-type summary) "button")
             (set! (.-title summary) (str "Load entries from " path))
             (.setAttribute summary "aria-label" (.-title summary))
             (.addEventListener summary "click"
                                #(load-more-db-context! context summary path 0))
             (.append line summary))
           (let [full-value (get (:expanded-db-values @state) path)
                 showing? (some? full-value)]
             (.append line (render-db-value
                             context
                             (if collection? (:open node)
                                 (if showing? full-value (or (:text node) "nil")))
                             path
                             (if collection? "collection-value" "leaf-value")))
             (when (:preview-truncated? node)
               (let [more (element context "button" "db-value-more"
                                   (if showing? "Show less" "… Show all"))]
                 (set! (.-type more) "button")
                 (set! (.-title more) (if showing?
                                       (str "Collapse app-db value " path)
                                       (str "Show complete app-db value " path)))
                 (.addEventListener more "click"
                                    #(toggle-full-db-value! context more node path showing?))
                 (.append line more)))))
         (when (and collection? collapsed?)
           (let [text (if default-collapsed?
                        (str "… " (:child-count node) " involved") "…")
                 mark (element context "button" "db-tree-collapsed-mark" text)]
             (set! (.-type mark) "button")
             (set! (.-title mark)
                   (if default-collapsed?
                     (str "Expand " path "; all " (:child-count node) " entries contribute")
                     (str "Expand " path)))
             (.addEventListener mark "click" #(toggle-db-node! context path true))
             (.append line mark)))
         (when-let [badges (render-component-badges
                            context (components-for-db-node context node))]
           (.append line badges))
         (.append branch line)
         (when (and collection? (not collapsed?))
           (let [children (element context "div" "db-tree-children")]
             (if (= :vector (:kind node))
               (doseq [{:keys [layout entries] :as group}
                       (layout/group-db-vector-entries (:children node))]
                 (if (= :compact layout)
                   (let [compact (element context "div" "db-vector-entries")]
                     (.setProperty (.-style compact) "--db-depth" (str (inc depth)))
                     (doseq [entry entries]
                       (.append compact (render-compact-db-vector-entry
                                          context (:node entry) (:key entry))))
                     (.append children compact))
                   (doseq [entry (:entries group)]
                     (.append children (render-db-node
                                         context (:node entry) (:key entry) (inc depth))))))
               (doseq [entry (:children node)]
                 (.append children (render-db-node
                                     context (:node entry) (:key entry) (inc depth)))))
             (.append branch children)
             (let [close-line (element context "div" "db-tree-line db-tree-close")]
               (.setProperty (.-style close-line) "--db-depth" (str depth))
               (.append close-line (element context "span" "db-tree-toggle-spacer"))
               (.append close-line (render-db-value context (:close node) path "collection-value"))
               (.append branch close-line))))))
     branch)))

(defn- render-app-db-section [context section app-db-tree]
  (let [container (element context "section"
                           (str "graph-section " (name (:kind section)) " db-tree-section"))
        header (element context "header" "section-header")
        heading (element context "h2" "section-title" (:title section))]
    (.append heading (element context "span" "section-count" (count (:nodes section))))
    (.append header heading)
    (.append container header)
    (when (and (seq (:nodes section)) app-db-tree)
      (let [tree (element context "div" "db-tree")]
        (.append tree (render-db-node context app-db-tree))
        (.append container tree)))
    container))

(defn- render-section [{:keys [state] :as context} section graph]
  (if (= :app-db-path (:kind section))
    (render-app-db-section context section (:app-db-tree graph))
    (let [container (element context "section"
                             (str "graph-section " (name (:kind section))
                                  (if (:value section) " has-values" " single-column")))
          header (element context "header" "section-header")
          heading (element context "h2" "section-title" (:title section))]
      (.append heading (element context "span" "section-count" (count (:nodes section))))
      (.append header heading)
      (.append container header)
      (when (seq (:nodes section))
        (let [columns (element context "div" "column-headings")]
          (.append columns (element context "span" "identity-heading" (:identity section)))
          (when (:value section)
            (.append columns (element context "span" "value-heading" (:value section))))
          (.append container columns))
        (if (:levels section)
          (doseq [{:keys [level nodes]} (:levels section)]
            (let [collapsed? (contains? (:collapsed-subscription-levels @state) level)
                  group (element context "div"
                                 (str "subscription-level" (when collapsed? " collapsed")))
                  level-header (element context "button" "level-header")]
              (set! (.-type level-header) "button")
              (set! (.-title level-header)
                    (str (if collapsed? "Expand" "Collapse") " subscription level " level))
              (.setAttribute level-header "aria-expanded" (str (not collapsed?)))
              (.append level-header (element context "span" "level-chevron"
                                               (if collapsed? "▸" "▾")))
              (.append level-header (element context "span" "level-title" (str "LEVEL " level)))
              (.append level-header (element context "span" "level-count" (count nodes)))
              (.addEventListener level-header "click"
                                 (fn []
                                   (swap! state update :collapsed-subscription-levels
                                          (if collapsed? disj conj) level)
                                   (render! context (:graph @state) {:preserve-scroll? true})))
              (.append group level-header)
              (when-not collapsed?
                (let [rows (element context "div" "section-rows")]
                  (doseq [node nodes] (.append rows (render-node context node section)))
                  (.append group rows)))
              (.append container group)))
          (let [rows (element context "div" "section-rows")]
            (doseq [node (:nodes section)] (.append rows (render-node context node section)))
            (.append container rows))))
      container)))

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

(defn- update-navigation! [{:keys [state elements]} navigation]
  (doseq [button (:navigation elements)]
    (set! (.-disabled button)
          (or (not (:bridge-ready? @state))
              (:navigation-running? @state)
              (not (get navigation (keyword (.. button -dataset -direction))))))))

(defn- render!
  ([context graph] (render! context graph {}))
  ([{:keys [state elements] :as context} next-graph {:keys [preserve-scroll?]}]
   (let [canvas (:canvas elements)
         scroll-top (.-scrollTop canvas)
         next-graph (apply-cached-argument-names @state next-graph)
         nodes (or (:nodes next-graph) [])
         edges (or (:edges next-graph) [])
         sections (layout/build-sections nodes edges)
         selected (get-in next-graph [:selection :label])
         has-selection? (or selected
                            (some #(and (= :element (:kind %))
                                        (not= "No element" (:label %))) nodes))
         next-generation (:selection-generation next-graph)]
     (swap! state assoc :graph next-graph)
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
     (update-navigation! context (:navigation next-graph))
     (set! (.-hidden (:empty elements)) (boolean has-selection?))
     (swap! state update :collapsed-nodes
            #(set (filter (set (map :id nodes)) %)))
     (swap! state update :expanded-values
            (fn [expanded]
              (let [by-id (into {} (map (juxt :id identity)) nodes)]
                (into {} (filter (fn [[id cached]]
                                   (= (:token cached) (:token (get by-id id))))) expanded))))
     (.replaceChildren (:graph elements))
     (doseq [section sections]
       (.append (:graph elements) (render-section context section next-graph)))
     (let [warnings (:warnings next-graph)]
       (set! (.-textContent (:status elements))
             (if (seq warnings)
               (str/join " · " (map :message warnings))
               (str "Connected · " (count nodes) " nodes"))))
     (if preserve-scroll?
       (set! (.-scrollTop canvas) scroll-top)
       (when (not= selected (:last-selection @state))
         (.scrollTo canvas #js {:top 0 :left 0})))
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

(defn- update-bridge-state! [{:keys [state root elements] :as context} ready? message]
  (swap! state assoc :bridge-ready? ready?)
  (.toggle (.-classList root) "preload-missing" (not ready?))
  (set! (.-disabled (:pick elements)) (not ready?))
  (set! (.-disabled (:component-boxes elements)) (not ready?))
  (set! (.-disabled (:refresh elements))
        (or (not ready?) (:refresh-running? @state)))
  (if-not ready?
    (do
      (swap! state assoc :graph nil :revision -1 :db-selection-generation nil)
      (reset-app-db-view-state! state)
      (.replaceChildren (:graph elements))
      (set! (.-hidden (:empty elements)) false)
      (set! (.-textContent (:empty elements))
            "This app may be missing re-frame.vertica.preload. Add it to the development build, restart the build, and reload the page.")
      (update-source-button! context nil))
    (when-not (:graph @state)
      (set! (.-hidden (:empty elements)) false)
      (set! (.-textContent (:empty elements)) select-element-message)))
  (when (seq message) (set! (.-textContent (:status elements)) message))
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

(defn- refresh-inspector! [{:keys [state elements] :as context}]
  (when-not (:refresh-running? @state)
    (swap! state assoc :refresh-running? true)
    (set! (.-disabled (:refresh elements)) true)
    (.add (.-classList (:refresh elements)) "loading")
    (.setAttribute (:refresh elements) "aria-busy" "true")
    (set! (.-textContent (:status elements)) "Refreshing provenance and source maps…")
    (-> (load-source-resources! context)
        (.then (fn [_]
                 (render-snapshot! context (call-bridge :snapshot)
                                   {:preserve-scroll? true})))
        (.catch #(show-error! context %))
        (.finally (fn []
                    (swap! state assoc :refresh-running? false)
                    (set! (.-disabled (:refresh elements)) false)
                    (.remove (.-classList (:refresh elements)) "loading")
                    (.removeAttribute (:refresh elements) "aria-busy"))))))

(declare poll!)

(defn- schedule-poll! [{:keys [state inspected-window] :as context}]
  (when (:running? @state)
    (swap! state assoc :poll-timer
           (.setTimeout inspected-window #(poll! context) poll-interval))))

(defn- poll! [{:keys [state elements on-picking-change] :as context}]
  (try
    (if-not (:bridge-ready? @state)
      (connect! context)
      (let [status (call-bridge :status)]
        (if-not status
          (update-bridge-state! context false (protocol/compatibility-message nil))
          (do
            (set! (.. (:pick elements) -dataset -active) (str (boolean (:picker-active status))))
            (set! (.-textContent (:pick elements))
                  (if (:picker-active status) "× Cancel" "⌖ Pick"))
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
  (swap! state assoc :running? false)
  (when-let [timer (:poll-timer @state)] (.clearTimeout inspected-window timer))
  (swap! state assoc :poll-timer nil)
  (on-picking-change false)
  (try (call-bridge :stop-picker) (catch :default _))
  (try (call-bridge :set-component-highlights false) (catch :default _)))

(defn set-floating! [{:keys [elements]} floating?]
  (let [button (:detach elements)]
    (set! (.-textContent button) (if floating? "↙ Attach" "↗ Detach"))
    (set! (.-title button) (if floating?
                            "Attach the panel to the application"
                            "Detach the panel into a floating window"))
    (.setAttribute button "aria-label" (.-title button))))

(defn set-status! [{:keys [elements]} message]
  (set! (.-textContent (:status elements)) message))

(defn mount!
  [{:keys [root storage inspected-document inspected-window
           on-close on-detach on-picking-change open-source]
    :or {on-close (fn [])
         on-detach (fn [])
         on-picking-change (fn [_])}}]
  (let [elements {:status (query root "#status")
                  :empty (query root "#empty")
                  :graph (query root "#graph")
                  :canvas (query root "#canvas-wrap")
                  :pick (query root "#pick")
                  :refresh (query root "#refresh")
                  :component-boxes (query root "#component-boxes")
                  :open-source (query root "#open-source")
                  :detach (query root "#detach")
                  :close (query root "#close")
                  :navigation (array-seq (.querySelectorAll root "#tree-nav [data-direction]"))}
        state (atom {:graph nil
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
                     :component-associations {}
                     :app-db-association-patterns []})
        context* (atom nil)
        context {:root root
                 :storage storage
                 :inspected-document inspected-document
                 :inspected-window inspected-window
                 :on-picking-change on-picking-change
                 :open-source open-source
                 :elements elements
                 :state state
                 :render! (fn [graph options] (render! @context* graph options))}]
    (reset! context* context)
    (doseq [button (:navigation elements)]
      (.addEventListener button "click"
                         #(navigate-selected! context
                                              (keyword (.. button -dataset -direction)))))
    (update-navigation! context {})
    (update-component-boxes-button! context (component-boxes-preference context))
    (.addEventListener (:refresh elements) "click" #(refresh-inspector! context))
    (.addEventListener (:open-source elements) "click"
                       (fn []
                         (when-let [location (:selected-source-location @state)]
                           (try (open-source (:url location) location)
                                (catch :default error (show-error! context error))))))
    (.addEventListener (:component-boxes elements) "click"
                       (fn []
                         (let [enabled? (not= "true"
                                              (.getAttribute (:component-boxes elements)
                                                             "aria-pressed"))]
                           (when storage
                             (try (.setItem storage component-boxes-setting (str enabled?))
                                  (catch :default _)))
                           (update-component-boxes-button! context enabled?)
                           (try
                             (let [status (call-bridge :set-component-highlights enabled?)]
                               (update-component-boxes-button!
                                 context (boolean (:component-highlights status))))
                             (catch :default error (show-error! context error))))))
    (.addEventListener (:pick elements) "click"
                       (fn []
                         (try
                           (let [active? (= "true" (.. (:pick elements) -dataset -active))
                                 status (call-bridge (if active? :stop-picker :start-picker))]
                             (set! (.. (:pick elements) -dataset -active)
                                   (str (boolean (:picker-active status))))
                             (set! (.-textContent (:pick elements))
                                   (if (:picker-active status) "× Cancel" "⌖ Pick"))
                             (update-component-boxes-button!
                               context (boolean (:component-highlights status)))
                             (update-navigation! context (:navigation status))
                             (on-picking-change (boolean (:picker-active status))))
                           (catch :default error (show-error! context error)))))
    (.addEventListener (:detach elements) "click" on-detach)
    (.addEventListener (:close elements) "click" on-close)
    context))
