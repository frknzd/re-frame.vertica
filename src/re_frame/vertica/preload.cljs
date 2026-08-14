(ns re-frame.vertica.preload
  (:require [re-frame.vertica.bridge :as bridge]
            [re-frame.vertica.registry :as registry]
            [re-frame.vertica.ui.panel-host :as panel-host]
            [shadow.resource :as resource]))

(defonce initialized
  (do (registry/install!)
      (bridge/install!)
      (panel-host/install! (resource/inline "./ui/panel.css"))
      true))

(defn ^:export show-panel!
  ([] (show-panel! true))
  ([show?]
   (when-let [panel (.-__RE_FRAME_VERTICA_PANEL__ js/globalThis)]
     (if show? (.show panel) (.hide panel)))))

(defn ^:export toggle-panel! []
  (when-let [panel (.-__RE_FRAME_VERTICA_PANEL__ js/globalThis)]
    (.toggle panel)))

(defn ^:export detach-panel! []
  (when-let [panel (.-__RE_FRAME_VERTICA_PANEL__ js/globalThis)]
    (.detach panel)))
