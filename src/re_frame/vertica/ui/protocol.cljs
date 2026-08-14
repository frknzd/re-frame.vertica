(ns re-frame.vertica.ui.protocol
  (:require [re-frame.vertica.shared :as shared]))

(def protocol-version shared/protocol-version)

(defn compatibility-message [capabilities]
  (cond
    (nil? capabilities)
    "The in-application inspector bridge is unavailable. Rebuild the application with re-frame.vertica.preload."

    (not= protocol-version (:protocol capabilities))
    (str "Bridge protocol " (or (:protocol capabilities) "unknown")
         " is incompatible with panel protocol " protocol-version
         ". Rebuild the application with one re-frame.vertica version.")

    (not (:registration-hook capabilities))
    "Preload loaded too late: subscription registration could not be instrumented."

    (not (:react-supported capabilities))
    (str "React " (or (:react-major capabilities) "unknown")
         " is unsupported; React 17 or 18 is required.")

    :else "Connected"))
