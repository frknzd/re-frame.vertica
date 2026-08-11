(ns re-frame-inspector.state)

(defonce registrations (atom {}))
(defonce selected-element (atom nil))
(defonce hover-element (atom nil))
(defonce picker-outcome (atom nil))
(defonce revision (atom 0))
(defonce node-values (atom {}))
(defonce runtime-warnings (atom []))
(defonce token-prefix (str (random-uuid)))
(defonce token-counter (atom 0))

(defn bump! [] (swap! revision inc))

(defn warn! [warning]
  (swap! runtime-warnings
         (fn [warnings]
           (->> (conj warnings warning) distinct (take-last 30) vec)))
  (bump!))

(defn reset-values! [] (reset! node-values {}))

(defn new-token! []
  (str token-prefix "/" (swap! token-counter inc)))

(defn remember-value! [token value]
  (swap! node-values assoc token value)
  token)
