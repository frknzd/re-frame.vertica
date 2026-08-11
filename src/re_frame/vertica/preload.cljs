(ns re-frame.vertica.preload
  (:require [re-frame.vertica.bridge :as bridge]
            [re-frame.vertica.registry :as registry]))

(defonce initialized
  (do (registry/install!)
      (bridge/install!)
      true))
