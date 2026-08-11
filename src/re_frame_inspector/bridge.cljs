(ns re-frame-inspector.bridge
  (:require [cognitect.transit :as transit]
            [clojure.walk :as walk]
            [goog.object :as gobj]
            [re-frame-inspector.graph :as graph]
            [re-frame-inspector.picker :as picker]
            [re-frame-inspector.react :as react]
            [re-frame-inspector.registry :as registry]
            [re-frame-inspector.shared :as shared]
            [re-frame-inspector.state :as state]))

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
        truncated? (or (< (count nodes) (count (:nodes snapshot)))
                       (< (count edges) (count (:edges snapshot))))]
    (cond-> (assoc snapshot :nodes nodes :edges edges)
      truncated? (update :warnings conj
                         {:code :snapshot-truncated
                          :message "Graph exceeded the 300 node / 600 edge transport limit."}))))

(defn capabilities []
  (encode {:protocol shared/protocol-version
           :name "re-frame Inspector"
           :preload true
           :registration-hook @registry/installed?
           :react-major (react/react-major)
           :react-supported (react/supported-react?)
           :features [:elements-selection :crosshair-picker :hover-preview
                      :graph-snapshot :node-logging :transit-json]}))

(defn status []
  (encode {:protocol shared/protocol-version
           :revision @state/revision
           :picker-active (picker/active?)
           :picker-outcome @state/picker-outcome
           :selection (some-> (or @state/hover-element @state/selected-element)
                              react/element-label)}))

(defn select-element [element]
  (reset! state/selected-element element)
  (reset! state/hover-element nil)
  (state/bump!)
  (encode (bounded-snapshot element)))

(defn snapshot []
  (encode (bounded-snapshot (or @state/hover-element @state/selected-element))))

(defn selected-element [] @state/selected-element)

(defn start-picker [] (picker/start!) (status))
(defn stop-picker [] (picker/stop!) (status))

(defn log-node [token]
  (if (contains? @state/node-values token)
    (do (.log js/console "re-frame Inspector value" (get @state/node-values token))
        (encode {:protocol shared/protocol-version :ok true}))
    (encode {:protocol shared/protocol-version :ok false
             :error "Node token is unknown or belongs to an expired snapshot."})))

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
          "start-picker" (start-picker)
          "stop-picker" (stop-picker)
          "log-node" (log-node (:token request))
          (encode {:protocol shared/protocol-version :ok false :error "Unknown action."}))))
    (catch :default error
      (encode {:protocol shared/protocol-version :ok false
               :error (or (ex-message error) (str error))}))))

(defn install! []
  (let [api #js {:version shared/protocol-version
                 :capabilities capabilities
                 :status status
                 :selectElement select-element
                 :selectedElement selected-element
                 :startPicker start-picker
                 :stopPicker stop-picker
                 :snapshot snapshot
                 :logNode log-node
                 :request request}]
    (gobj/set js/globalThis "__RE_FRAME_INSPECTOR__" api)
    api))
