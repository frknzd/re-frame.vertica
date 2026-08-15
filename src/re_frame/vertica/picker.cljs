(ns re-frame.vertica.picker
  (:require [re-frame.vertica.react :as react]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.state :as state]))

(defonce session (atom nil))
(defonce ^:private overlay (atom nil))
(defonce ^:private component-overlay (atom nil))
(defonce ^:private component-targets (atom []))
(defonce ^:private component-observer (atom nil))
(defonce ^:private component-dirty? (atom true))
(defonce ^:private last-component-refresh (atom 0))
(defonce ^:private highlighted-element (atom nil))
(defonce ^:private highlight-timer (atom nil))
(defonce ^:private last-heartbeat (atom 0))

(def ^:private highlight-refresh-ms 100)
(def ^:private component-refresh-ms 250)
(def ^:private heartbeat-timeout-ms 5000)
(def ^:private highlight-id "__re-frame.vertica-highlight")
(def ^:private component-highlight-id "__re-frame.vertica-component-highlights")
(def ^:private panel-id "__re-frame-vertica-panel")
(def ^:private svg-ns "http://www.w3.org/2000/svg")
(def ^:private blocked-interaction-events
  ["pointerdown" "pointerup" "pointercancel"
   "mousedown" "mouseup" "touchstart" "touchend"
   "auxclick" "dblclick" "contextmenu"])
(def ^:private blocked-listener-options #js {:capture true :passive false})
(def ^:private prevent-default-events #{"auxclick" "dblclick" "contextmenu"})
(def ^:private selection-end-events #{"pointerup" "mouseup" "touchend"})

(defn active? [] (some? @session))

(defn- make-overlay []
  (let [overlay (.createElement js/document "div")]
    (set! (.-id overlay) highlight-id)
    (set! (.-style.cssText overlay)
          (str "position:fixed;z-index:" shared/selection-highlight-z-index ";pointer-events:none;"
               "border:2px solid #38bdf8;background:rgba(56,189,248,.15);"
               "box-sizing:border-box;display:none"))
    (.appendChild (.-documentElement js/document) overlay)
    overlay))

(defn inspector-overlay? [element]
  (loop [candidate element]
    (if-not candidate
      false
      (if (contains? #{highlight-id component-highlight-id panel-id} (.-id candidate))
        true
        (recur (or (.-parentElement candidate)
                   (when (fn? (.-getRootNode candidate))
                     (some-> (.getRootNode candidate) .-host))))))))

(defn- make-component-overlay []
  (let [overlay (.createElementNS js/document svg-ns "svg")]
    (set! (.-id overlay) component-highlight-id)
    (.setAttribute overlay "aria-hidden" "true")
    (set! (.-style.cssText overlay)
          (str "position:fixed;inset:0;width:100vw;height:100vh;"
               "z-index:" shared/component-highlight-z-index
               ";pointer-events:none;display:none;overflow:hidden"))
    (.appendChild (.-documentElement js/document) overlay)
    overlay))

(defn- ensure-overlay! []
  (let [current @overlay]
    (if (and current (.-isConnected current))
      current
      (let [created (make-overlay)]
        (reset! overlay created)
        created))))

(defn- ensure-component-overlay! []
  (let [current @component-overlay]
    (if (and current (.-isConnected current))
      current
      (let [created (make-component-overlay)]
        (reset! component-overlay created)
        (reset! component-dirty? true)
        created))))

(defn- component-rect []
  (let [rect (.createElementNS js/document svg-ns "rect")]
    (.setAttribute rect "fill" "rgba(192,132,252,.08)")
    (.setAttribute rect "stroke" "#c084fc")
    (.setAttribute rect "stroke-width" "1.5")
    (.setAttribute rect "vector-effect" "non-scaling-stroke")
    rect))

(defn- inspectable-elements []
  (loop [roots [js/document]
         result []]
    (if-let [root (peek roots)]
      (let [elements (vec (array-seq (.querySelectorAll root "*")))
            shadow-roots (keep #(.-shadowRoot %) elements)]
        (recur (into (pop roots) shadow-roots) (into result elements)))
      result)))

(defn- scan-component-targets! [overlay]
  (let [targets (->> (inspectable-elements)
                     (remove inspector-overlay?)
                     (filter react/reagent-root-element?)
                     (mapv (fn [element] {:element element :rect (component-rect)})))]
    (.replaceChildren overlay)
    (doseq [{:keys [rect]} targets] (.appendChild overlay rect))
    (reset! component-targets targets)
    (reset! component-dirty? false)))

(defn- place-component-rect! [{:keys [element rect]}]
  (if (and (.-isConnected element) (.-isConnected rect))
    (let [bounds (.getBoundingClientRect element)
          visible? (and (pos? (.-width bounds))
                        (pos? (.-height bounds))
                        (> (.-right bounds) 0)
                        (> (.-bottom bounds) 0)
                        (< (.-left bounds) (.-innerWidth js/window))
                        (< (.-top bounds) (.-innerHeight js/window)))]
      (set! (.. rect -style -display) (if visible? "block" "none"))
      (when visible?
        (.setAttribute rect "x" (str (.-left bounds)))
        (.setAttribute rect "y" (str (.-top bounds)))
        (.setAttribute rect "width" (str (.-width bounds)))
        (.setAttribute rect "height" (str (.-height bounds)))))
    (reset! component-dirty? true)))

(defn- ensure-component-observer! []
  (when-not @component-observer
    (let [observer
          (js/MutationObserver.
            (fn [mutations]
              (when (some (fn [mutation]
                            (and (not (inspector-overlay? (.-target mutation)))
                                 (some #(= 1 (.-nodeType %))
                                       (concat (array-seq (.-addedNodes mutation))
                                               (array-seq (.-removedNodes mutation))))))
                          (array-seq mutations))
                (reset! component-dirty? true))))]
      (.observe observer (.-documentElement js/document)
                #js {:childList true :subtree true})
      (reset! component-observer observer))))

(defn- stop-component-highlights! []
  (when-let [observer @component-observer]
    (.disconnect observer)
    (reset! component-observer nil))
  (when-let [current @component-overlay]
    (set! (.. current -style -display) "none")
    (.replaceChildren current))
  (reset! component-targets [])
  (reset! component-dirty? true))

(defn- refresh-component-highlights! [now]
  (when (>= (- now @last-component-refresh) component-refresh-ms)
    (reset! last-component-refresh now)
    (let [overlay (ensure-component-overlay!)]
      (ensure-component-observer!)
      (when @component-dirty? (scan-component-targets! overlay))
      (doseq [target @component-targets] (place-component-rect! target))
      (set! (.. overlay -style -display) "block"))))

(defn- place-overlay! [overlay element]
  (if (and element (.-isConnected element) (not= element overlay))
    (let [rect (.getBoundingClientRect element)
          style (.-style overlay)]
      (set! (.-display style) "block")
      (set! (.-left style) (str (.-left rect) "px"))
      (set! (.-top style) (str (.-top rect) "px"))
      (set! (.-width style) (str (.-width rect) "px"))
      (set! (.-height style) (str (.-height rect) "px")))
    (set! (.. overlay -style -display) "none")))

(defn- stop-highlight-timer! []
  (when-let [timer @highlight-timer]
    (js/clearInterval timer)
    (reset! highlight-timer nil)))

(defn- refresh-highlight! []
  (let [now (.now js/Date)]
    (if (> (- now @last-heartbeat) heartbeat-timeout-ms)
      (do
        (when-let [current @overlay]
          (set! (.. current -style -display) "none"))
        (stop-component-highlights!)
        (stop-highlight-timer!))
      (do
        (if @highlighted-element
          (place-overlay! (ensure-overlay!) @highlighted-element)
          (when-let [current @overlay]
            (set! (.. current -style -display) "none")))
        (if @state/component-highlights-enabled?
          (refresh-component-highlights! now)
          (stop-component-highlights!))))))

(defn- start-highlight-timer! []
  (when-not @highlight-timer
    (reset! highlight-timer (js/setInterval refresh-highlight! highlight-refresh-ms))))

(defn heartbeat! []
  (reset! last-heartbeat (.now js/Date))
  (refresh-highlight!)
  (start-highlight-timer!)
  true)

(defn set-component-highlights! [enabled?]
  (let [enabled? (boolean enabled?)]
    (reset! state/component-highlights-enabled? enabled?)
    (if enabled?
      (do (reset! component-dirty? true) (heartbeat!))
      (stop-component-highlights!))
    enabled?))

(defn highlight! [element]
  (reset! highlighted-element element)
  (heartbeat!)
  (if element
    (place-overlay! (ensure-overlay!) element)
    (when-let [current @overlay]
      (set! (.. current -style -display) "none")))
  true)

(declare stop!)

(defn- event-point [event]
  (let [touches (or (.-changedTouches event) (.-touches event))
        touch (when (and touches (pos? (.-length touches))) (aget touches 0))
        x (if touch (.-clientX touch) (.-clientX event))
        y (if touch (.-clientY touch) (.-clientY event))]
    (when (and (number? x) (number? y)) [x y])))

(defn- deep-element-from-point [x y]
  (loop [element (.elementFromPoint js/document x y)]
    (if-let [shadow-root (some-> element .-shadowRoot)]
      (if-let [inner (.elementFromPoint shadow-root x y)]
        (if (identical? inner element) element (recur inner))
        element)
      element)))

(defn- reagent-element-at-point [x y]
  (some-> (deep-element-from-point x y)
          react/nearest-reagent-root-element))

(defn start! []
  (when-not @session
    (reset! state/picker-outcome nil)
    (heartbeat!)
    (let [frame (atom nil)
          point (atom nil)
          lock-timer (atom nil)
          cancel-frame (fn []
                         (when-let [pending @frame]
                           (js/cancelAnimationFrame pending)
                           (reset! frame nil)))
          cancel-lock-timer (fn []
                              (when-let [pending @lock-timer]
                                (js/clearTimeout pending)
                                (reset! lock-timer nil)))
          lock-selection (fn [x y]
                           (when-let [element (reagent-element-at-point x y)]
                             (cancel-frame)
                             (cancel-lock-timer)
                             (reset! state/selected-element element)
                             (reset! state/hover-element nil)
                             (state/begin-selection!)
                             (reset! state/picker-outcome :locked)
                             (state/bump!)
                             (stop!)))
          flush-move (fn []
                       (reset! frame nil)
                       (when-let [[x y] @point]
                         (let [element (reagent-element-at-point x y)]
                           (highlight! element)
                           (when-not (identical? element @state/hover-element)
                             (reset! state/hover-element element)
                             (state/bump!)))))
          move (fn [event]
                 (when-let [next-point (event-point event)]
                   (reset! point next-point))
                 (when-not @frame
                   (reset! frame (js/requestAnimationFrame flush-move))))
          block-interaction (fn [event]
                              ;; Stopping propagation blocks application handlers,
                              ;; while preserving pointer defaults allows Chrome to
                              ;; synthesize the final click that locks the picker.
                              (when (contains? prevent-default-events (.-type event))
                                (.preventDefault event))
                              (.stopImmediatePropagation event)
                              (when-let [next-point (event-point event)]
                                (reset! point next-point))
                              (when (and (contains? selection-end-events (.-type event))
                                         (nil? @lock-timer))
                                (when-let [[x y] @point]
                                  ;; Runs after the browser's immediate click. If
                                  ;; no click is emitted, selection still finalizes.
                                  (reset! lock-timer
                                          (js/setTimeout
                                            (fn []
                                              (reset! lock-timer nil)
                                              (when @session (lock-selection x y)))
                                            0)))))
          click (fn [event]
                  (.preventDefault event)
                  (.stopImmediatePropagation event)
                  (when-let [[x y] (event-point event)]
                    (lock-selection x y)))
          keydown (fn [event]
                    (.preventDefault event)
                    (.stopImmediatePropagation event)
                    (when (= "Escape" (.-key event))
                      (reset! state/hover-element nil)
                      (reset! state/picker-outcome :cancelled)
                      (state/bump!)
                      (stop!)))]
      (.addEventListener js/document "pointermove" move true)
      (doseq [event-type blocked-interaction-events]
        (.addEventListener js/document event-type block-interaction blocked-listener-options))
      (.addEventListener js/document "click" click true)
      (.addEventListener js/document "keydown" keydown true)
      (reset! session {:move move :block-interaction block-interaction
                       :click click :keydown keydown :frame frame
                       :lock-timer lock-timer})
      (state/bump!)))
  true)

(defn stop! []
  (when-let [{:keys [move block-interaction click keydown frame lock-timer]} @session]
    (when-let [pending @frame]
      (js/cancelAnimationFrame pending))
    (when-let [pending @lock-timer]
      (js/clearTimeout pending))
    (.removeEventListener js/document "pointermove" move true)
    (doseq [event-type blocked-interaction-events]
      (.removeEventListener js/document event-type block-interaction true))
    (.removeEventListener js/document "click" click true)
    (.removeEventListener js/document "keydown" keydown true)
    (reset! session nil)
    (highlight! @state/selected-element)
    (state/bump!))
  true)
