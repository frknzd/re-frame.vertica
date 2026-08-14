(ns re-frame.vertica.ui.protocol
  (:require [re-frame.vertica.shared :as shared]))

(def protocol-version shared/protocol-version)

(defn compatibility-message [capabilities]
  (cond
    (nil? capabilities)
    "Preload missing or not loaded. This app may need re-frame.vertica.preload (globalThis.__RE_FRAME_VERTICA__ is unavailable)."

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
