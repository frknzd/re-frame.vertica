(ns re-frame.vertica.bridge
  (:require [cognitect.transit :as transit]
            [cljs.reader :as reader]
            [clojure.walk :as walk]
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

(defn decode-response [encoded]
  (when (some? encoded)
    (walk/keywordize-keys (transit/read reader encoded))))

(defn- graph-snapshot [element]
  (graph/snapshot element))

(defn capabilities []
  (encode {:protocol shared/protocol-version
           :name "re-frame.vertica"
           :preload true
           :registration-hook @registry/installed?
           :react-major (react/react-major)
           :react-supported (react/supported-react?)
           :features [:in-page-panel :detachable-panel :keyboard-toggle
                      :crosshair-picker :hover-preview
                      :reagent-only-picker :reagent-component-highlights
                      :element-navigation :persistent-highlight
                      :graph-snapshot :node-logging :node-expansion :transit-json
                      :causal-render-provenance]}))

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
             :selection-generation @state/selection-generation
             :picker-active (picker/active?)
             :component-highlights @state/component-highlights-enabled?
             :picker-outcome @state/picker-outcome
             :selection (some-> element react/element-label)
             :navigation (navigation-state @state/selected-element)})))

(defn select-element [element]
  (reset! state/selected-element element)
  (reset! state/hover-element nil)
  (state/begin-selection!)
  (picker/highlight! element)
  (state/bump!)
  (encode (assoc (graph-snapshot element) :navigation (navigation-state element))))

(defn snapshot []
  (let [element (or @state/hover-element @state/selected-element)]
    (encode (assoc (graph-snapshot element)
                   :navigation (navigation-state @state/selected-element)))))

(defn navigate-element [direction]
  (if-let [element (relative-element @state/selected-element direction)]
    (do
      (reset! state/selected-element element)
      (reset! state/hover-element nil)
      (state/begin-selection!)
      (picker/highlight! element)
      (state/bump!)
      (encode (assoc (graph-snapshot element) :navigation (navigation-state element))))
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

(defn expand-app-db-path
  ([path-label] (expand-app-db-path path-label nil))
  ([path-label visible-count]
   (try
     (let [path (reader/read-string path-label)]
       (if-not (vector? path)
         (encode {:protocol shared/protocol-version :ok false :error "Invalid app-db path."})
         (let [value (get-in @db/app-db path)
               visible-count (if (number? visible-count) visible-count 0)
               current-limit (get @state/app-db-expansions path 0)
               next-limit (+ 10 (max visible-count current-limit))
               expansions (swap! state/app-db-expansions assoc path next-limit)]
           (encode {:protocol shared/protocol-version
                    :ok true
                    :value (when-not (coll? value) (shared/value-string value))
                    :node (projection/app-db-branch @db/app-db @state/app-db-paths path expansions)}))))
     (catch :default error
       (encode {:protocol shared/protocol-version :ok false
                :error (or (ex-message error) (str error))})))))

(defn request [payload]
  (try
    (let [request (walk/keywordize-keys (transit/read reader payload))]
      (if (not= shared/protocol-version (:protocol request))
        (encode {:protocol shared/protocol-version :ok false
                 :error "Incompatible bridge protocol. Rebuild the application with one re-frame.vertica version."})
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
