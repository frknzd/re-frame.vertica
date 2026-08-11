(ns re-frame.vertica.bridge
  (:require [cognitect.transit :as transit]
            [cljs.reader :as reader]
            [clojure.walk :as walk]
            [goog.object :as gobj]
            [re-frame.db :as db]
            [re-frame.vertica.graph :as graph]
            [re-frame.vertica.picker :as picker]
            [re-frame.vertica.projection :as projection]
            [re-frame.vertica.react :as react]
            [re-frame.vertica.registry :as registry]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.state :as state]))

(defonce writer (transit/writer :json))
(defonce reader (transit/reader :json))

(defn- encode [value]
  (transit/write writer (clj->js value)))

(defn- bounded-snapshot [element]
  (let [snapshot (graph/snapshot element)
        nodes (vec (take 300 (:nodes snapshot)))
        ids (set (map :id nodes))
        edges (->> (:edges snapshot)
                   (filter #(and (ids (:from %)) (ids (:to %))))
                   (take 600) vec)
        truncated? (or (:truncated? snapshot)
                       (< (count nodes) (count (:nodes snapshot)))
                       (< (count edges) (count (:edges snapshot))))]
    (cond-> (assoc snapshot :nodes nodes :edges edges)
      truncated? (update :warnings conj
                         {:code :snapshot-truncated
                          :message "Graph exceeded the 300 node / 600 edge transport limit."}))))

(defn capabilities []
  (encode {:protocol shared/protocol-version
           :name "re-frame.vertica"
           :preload true
           :registration-hook @registry/installed?
           :react-major (react/react-major)
           :react-supported (react/supported-react?)
           :features [:elements-selection :crosshair-picker :hover-preview
                      :reagent-only-picker :reagent-component-highlights
                      :element-navigation :persistent-highlight
                      :graph-snapshot :node-logging :node-expansion :transit-json]}))

(defn- selectable-element [element]
  (when (and element (not (picker/inspector-overlay? element))) element))

(defn relative-element [element direction]
  (selectable-element
    (case direction
      "parent" (some-> element .-parentElement)
      "child" (some-> element .-firstElementChild)
      "previous" (some-> element .-previousElementSibling)
      "next" (some-> element .-nextElementSibling)
      nil)))

(defn- navigation-state [element]
  {:parent (boolean (relative-element element "parent"))
   :child (boolean (relative-element element "child"))
   :previous (boolean (relative-element element "previous"))
   :next (boolean (relative-element element "next"))})

(defn status []
  (picker/heartbeat!)
  (let [element (or @state/hover-element @state/selected-element)]
    (encode {:protocol shared/protocol-version
             :revision @state/revision
             :picker-active (picker/active?)
             :component-highlights @state/component-highlights-enabled?
             :picker-outcome @state/picker-outcome
             :selection (some-> element react/element-label)
             :navigation (navigation-state @state/selected-element)})))

(defn select-element [element]
  (reset! state/selected-element element)
  (reset! state/hover-element nil)
  (reset! state/app-db-expansions {})
  (picker/highlight! element)
  (state/bump!)
  (encode (assoc (bounded-snapshot element) :navigation (navigation-state element))))

(defn snapshot []
  (let [element (or @state/hover-element @state/selected-element)]
    (encode (assoc (bounded-snapshot element)
                   :navigation (navigation-state @state/selected-element)))))

(defn navigate-element [direction]
  (if-let [element (relative-element @state/selected-element direction)]
    (do
      (reset! state/selected-element element)
      (reset! state/hover-element nil)
      (reset! state/app-db-expansions {})
      (picker/highlight! element)
      (state/bump!)
      (encode (assoc (bounded-snapshot element) :navigation (navigation-state element))))
    (encode {:protocol shared/protocol-version :ok false
             :error "There is no element in that direction."
             :navigation (navigation-state @state/selected-element)})))

(defn selected-element [] @state/selected-element)

(defn start-picker [] (picker/start!) (status))
(defn stop-picker [] (picker/stop!) (status))
(defn set-component-highlights [enabled?]
  (picker/set-component-highlights! enabled?)
  (status))

(defn log-node [token]
  (if (contains? @state/node-values token)
    (do (.log js/console "re-frame.vertica value" (get @state/node-values token))
        (encode {:protocol shared/protocol-version :ok true}))
    (encode {:protocol shared/protocol-version :ok false
             :error "Node token is unknown or belongs to an expired snapshot."})))

(defn expand-node [token]
  (if (contains? @state/node-values token)
    (encode {:protocol shared/protocol-version :ok true
             :value (shared/value-string (get @state/node-values token))})
    (encode {:protocol shared/protocol-version :ok false
             :error "Node token is unknown or belongs to an expired snapshot."})))

(defn expand-app-db-path [path-label]
  (try
    (let [path (reader/read-string path-label)]
      (if-not (vector? path)
        (encode {:protocol shared/protocol-version :ok false :error "Invalid app-db path."})
        (let [value (get-in @db/app-db path)
              expansions (swap! state/app-db-expansions update path (fnil + 2) 10)]
          (encode {:protocol shared/protocol-version
                   :ok true
                   :value (when-not (coll? value) (shared/value-string value))
                   :node (projection/app-db-branch @db/app-db @state/app-db-paths path expansions)}))))
    (catch :default error
      (encode {:protocol shared/protocol-version :ok false
               :error (or (ex-message error) (str error))}))))

(defn request [payload]
  (try
    (let [request (walk/keywordize-keys (transit/read reader payload))]
      (if (not= shared/protocol-version (:protocol request))
        (encode {:protocol shared/protocol-version :ok false
                 :error "Incompatible bridge protocol. Upgrade the preload and extension together."})
        (case (:action request)
          "capabilities" (capabilities)
          "status" (status)
          "snapshot" (snapshot)
          "navigate-element" (navigate-element (:direction request))
          "start-picker" (start-picker)
          "stop-picker" (stop-picker)
          "set-component-highlights" (set-component-highlights (:enabled request))
          "log-node" (log-node (:token request))
          "expand-node" (expand-node (:token request))
          "expand-app-db-path" (expand-app-db-path (:path request))
          (encode {:protocol shared/protocol-version :ok false :error "Unknown action."}))))
    (catch :default error
      (encode {:protocol shared/protocol-version :ok false
               :error (or (ex-message error) (str error))}))))

(defn install! []
  (let [api #js {:version shared/protocol-version
                 :capabilities capabilities
                 :status status
                 :selectElement select-element
                 :startPicker start-picker
                 :stopPicker stop-picker
                 :setComponentHighlights set-component-highlights
                 :snapshot snapshot
                 :navigateElement navigate-element
                 :selectedElement selected-element
                 :logNode log-node
                 :expandNode expand-node
                 :expandAppDbPath expand-app-db-path
                 :request request}]
    (gobj/set js/globalThis "__RE_FRAME_VERTICA__" api)
    api))
