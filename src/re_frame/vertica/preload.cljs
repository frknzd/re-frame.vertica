(ns re-frame.vertica.preload
  (:require [re-frame.vertica.registry :as registry]
            [re-frame.vertica.ui.panel-host :as panel-host]
            [shadow.resource :as resource]))

(defonce initialized
  (do (registry/install!)
      (panel-host/install! (resource/inline "/re_frame/vertica/ui/panel.css"))
      true))