(ns re-frame-inspector.preload
  (:require [re-frame-inspector.bridge :as bridge]
            [re-frame-inspector.registry :as registry]))

(defonce initialized
  (do (registry/install!)
      (bridge/install!)
      true))
