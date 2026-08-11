(ns re-frame-inspector.registry
  (:require [re-frame.db :as db]
            [re-frame.registrar :as registrar]
            [re-frame.subs :as subs]
            [re-frame-inspector.state :as state]))

(defonce installed? (atom false))
(defonce original-reg-sub (atom nil))

(defn- invoke-inputs [inputs-fn query-v dyn-v]
  (try
    (if (some? dyn-v)
      (inputs-fn query-v dyn-v)
      (inputs-fn query-v nil))
    (catch :default _ ::unknown)))

(defn layer-2?
  [{:keys [inputs-fn latest-query latest-dyn]}]
  (identical? db/app-db (invoke-inputs inputs-fn latest-query latest-dyn)))

(defn- instrumented-register [query-id args]
  (let [[inputs-fn computation-fn] (apply subs/sugar query-id subs/subscribe vector? args)
        generation (inc (get-in @state/registrations [query-id :generation] 0))]
    (apply @original-reg-sub query-id args)
    (let [original-handler (registrar/get-handler subs/kind query-id)
          wrapped-handler
          (with-meta
            (fn
              ([db query-v]
               (swap! state/registrations update query-id assoc
                      :latest-query query-v :latest-dyn nil)
               (original-handler db query-v))
              ([db query-v dyn-v]
               (swap! state/registrations update query-id assoc
                      :latest-query query-v :latest-dyn dyn-v)
               (original-handler db query-v dyn-v)))
            {::instrumented true})]
      (swap! state/registrations assoc query-id
             {:query-id query-id
              :inputs-fn inputs-fn
              :computation-fn computation-fn
              :handler wrapped-handler
              :generation generation
              :latest-query nil
              :latest-dyn nil})
      ;; The public registrar function would emit a duplicate-registration warning.
      ;; Replacing the handler atomically keeps re-frame's generated handler intact
      ;; while adding invocation capture around it.
      (swap! registrar/kind->id->handler assoc-in [subs/kind query-id] wrapped-handler)
      wrapped-handler)))

(defn reg-sub [query-id & args]
  (instrumented-register query-id args))

(defn- reconcile-registrations [_ _ old new]
  (let [before (get old subs/kind {})
        after (get new subs/kind {})]
    (doseq [[id registration] @state/registrations]
      (let [old-handler (get before id)
            new-handler (get after id)]
        (when (or (nil? new-handler)
                  (and (not (identical? old-handler new-handler))
                       (not (identical? (:handler registration) new-handler))
                       (not (::instrumented (meta new-handler)))))
          (swap! state/registrations dissoc id))))))

(defn install! []
  (when-not @installed?
    (let [original subs/reg-sub]
      (if (fn? original)
        (do
          (reset! original-reg-sub original)
          ;; Direct var assignment follows Closure renaming and therefore also
          ;; works when the host application uses advanced optimization.
          (set! subs/reg-sub reg-sub)
          (add-watch registrar/kind->id->handler ::reconcile reconcile-registrations)
          (reset! installed? true))
        (state/warn! {:code :registration-hook-unavailable
                      :message "Could not locate re-frame.subs/reg-sub; load the preload before application namespaces."}))))
  @installed?)
