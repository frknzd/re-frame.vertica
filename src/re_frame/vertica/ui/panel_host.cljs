(ns re-frame.vertica.ui.panel-host
  (:require [goog.object :as gobj]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.ui.panel :as panel]))

(def panel-host-id "__re-frame-vertica-panel")
(def toggle-shortcut "Ctrl+Shift+V")
(def ^:private panel-width-setting "re-frame.vertica.panel-width")
(def ^:private minimum-panel-width 360)
(def ^:private maximum-default-panel-width 1000)
(def ^:private resize-key-step 40)

(defn default-panel-width [viewport-width]
  (min viewport-width
       maximum-default-panel-width
       (max 560 (* viewport-width 0.72))))

(defn constrain-panel-width [width viewport-width]
  (let [viewport-width (max 0 viewport-width)
        minimum-width (min minimum-panel-width viewport-width)]
    (-> width
        (max minimum-width)
        (min viewport-width))))

(defn- css-value [value]
  (if (keyword? value) (name value) (str value)))

(defn- set-styles! [element styles]
  (when-let [all (:all styles)]
    (.setProperty (.-style element) "all" (css-value all)))
  (doseq [[property value] (dissoc styles :all)]
    (.setProperty (.-style element) (name property) (css-value value))))

(defn- viewport-width [host inspected-window]
  (or (.-innerWidth inspected-window)
      (some-> host .-ownerDocument .-documentElement .-clientWidth)
      0))

(defn- stored-panel-width [storage]
  (when storage
    (try
      (let [width (js/parseFloat (.getItem storage panel-width-setting))]
        (when (js/Number.isFinite width) width))
      (catch :default _ nil))))

(defn- save-panel-width! [storage width]
  (when storage
    (try (.setItem storage panel-width-setting (str (js/Math.round width)))
         (catch :default _))))

(defn- set-panel-width! [host inspected-window width]
  (let [width (constrain-panel-width width (viewport-width host inspected-window))]
    (.setProperty (.-style host) "width" (str width "px"))
    width))

(defn- panel-host-styles [floating? width]
  (merge
    {:all :initial
     :position :fixed
     :box-sizing :border-box
     :z-index shared/panel-z-index
     :height "100vh"
     :display :none
     :overflow :hidden
     :background "#0b0e13"}
    (if floating?
      {:inset 0 :width "100%"}
      {:top 0
       :right 0
       :bottom 0
       :width (str width "px")
       :max-width "100vw"
       :box-shadow "-12px 0 36px rgba(0,0,0,.45)"
       :border-left "1px solid #303846"})))

(defn- panel-resize-handlers [host storage inspected-window]
  (let [drag (atom nil)
        document-element (.. host -ownerDocument -documentElement)]
    (letfn [(current-width [] (.. host getBoundingClientRect -width))

            (apply-width! [width]
              (set-panel-width! host inspected-window width))

            (finish-resize! [event]
              (when-let [{:keys [handle pointer-id cursor user-select]} @drag]
                (when (or (nil? event) (= pointer-id (.-pointerId event)))
                  (.removeEventListener inspected-window "pointermove" resize!)
                  (.removeEventListener inspected-window "pointerup" finish-resize!)
                  (.removeEventListener inspected-window "pointercancel" finish-resize!)
                  (try (.releasePointerCapture handle pointer-id)
                       (catch :default _))
                  (set! (.. document-element -style -cursor) cursor)
                  (set! (.. document-element -style -userSelect) user-select)
                  (save-panel-width! storage (current-width))
                  (reset! drag nil))))

            (resize! [event]
              (when-let [{:keys [pointer-id start-x start-width]} @drag]
                (when (= pointer-id (.-pointerId event))
                  (.preventDefault event)
                  (apply-width! (+ start-width (- start-x (.-clientX event)))))))

            (start-resize! [event]
              (when (and (nil? @drag) (zero? (.-button event)))
                (.preventDefault event)
                (let [handle (.-currentTarget event)
                      pointer-id (.-pointerId event)]
                  (reset! drag {:handle handle
                                :pointer-id pointer-id
                                :start-x (.-clientX event)
                                :start-width (current-width)
                                :cursor (.. document-element -style -cursor)
                                :user-select (.. document-element -style -userSelect)})
                  (try (.setPointerCapture handle pointer-id)
                       (catch :default _))
                  (set! (.. document-element -style -cursor) "ew-resize")
                  (set! (.. document-element -style -userSelect) "none")
                  (.addEventListener inspected-window "pointermove" resize!)
                  (.addEventListener inspected-window "pointerup" finish-resize!)
                  (.addEventListener inspected-window "pointercancel" finish-resize!))))

            (resize-from-keyboard! [event]
              (let [width (current-width)
                    next-width (case (.-key event)
                                 "ArrowLeft" (+ width resize-key-step)
                                 "ArrowRight" (- width resize-key-step)
                                 "Home" (default-panel-width
                                          (viewport-width host inspected-window))
                                 "End" (viewport-width host inspected-window)
                                 nil)]
                (when next-width
                  (.preventDefault event)
                  (save-panel-width! storage (apply-width! next-width)))))

            (toggle-full-width! [event]
              (.preventDefault event)
              (let [viewport (viewport-width host inspected-window)
                    width (current-width)
                    next-width (if (>= width (- viewport resize-key-step))
                                 (default-panel-width viewport)
                                 viewport)]
                (save-panel-width! storage (apply-width! next-width))))]
      {:on-resize-start start-resize!
       :on-resize-key-down resize-from-keyboard!
       :on-resize-double-click toggle-full-width!})))

(defn- create-instance!
  [{:keys [document css floating? storage inspected-document inspected-window
           on-close on-detach on-picking-change]}]
  (let [host (doto (.createElement document "div")
               (.setAttribute "data-re-frame-vertica-ui" "true")
               (.setAttribute "role" "complementary")
               (.setAttribute "aria-label" "re-frame.vertica inspector"))
        viewport (viewport-width host inspected-window)
        width (constrain-panel-width
                (or (stored-panel-width storage) (default-panel-width viewport))
                viewport)]
    (set! (.-id host) panel-host-id)
    (set-styles! host (panel-host-styles floating? width))
    (let [shadow (.attachShadow host #js {:mode "open"})
          style (.createElement document "style")
          mount-node (.createElement document "div")
          resize-handlers (when-not floating?
                            (panel-resize-handlers host storage inspected-window))]
      (set! (.-textContent style) css)
      (set! (.. mount-node -style -height) "100%")
      (.append shadow style mount-node)
      (.append (.-documentElement document) host)
      (let [context (panel/mount!
                      (merge
                        {:mount-node mount-node
                         :storage storage
                         :inspected-document inspected-document
                         :inspected-window inspected-window
                         :on-close on-close
                         :on-detach on-detach
                         :on-picking-change on-picking-change
                         :open-source (fn [url _]
                                        (.open inspected-window url "_blank" "noopener"))}
                        resize-handlers))]
        (panel/set-floating! context floating?)
        {:host host :context context}))))

(defn toggle-shortcut? [event]
  (boolean
    (and event
         (.-ctrlKey event)
         (.-shiftKey event)
         (not (.-altKey event))
         (not (.-metaKey event))
         (or (= "KeyV" (.-code event))
             (= "v" (.toLowerCase (str (.-key event))))))))

(defn install! [css]
  (let [inspected-window js/window
        inspected-document js/document]
    (when (and inspected-window (.-documentElement inspected-document))
      (or (gobj/get js/globalThis "__RE_FRAME_VERTICA_PANEL__")
          (let [state (atom {:open? false :detached? false :picking? false
                             :popup nil :popup-instance nil :popup-poll nil
                             :closing-popup? false})
                side-instance (atom nil)]
            (letfn [(show-instance! [instance visible?]
                      (when instance
                        (let [host (:host instance)]
                          (set! (.. host -style -display)
                                (if visible? "block" "none"))
                          (set! (.. host -style -visibility)
                                (if (and visible? (:picking? @state) (not (:detached? @state)))
                                  "hidden" "visible")))))

                    (on-picking-change [active?]
                      (swap! state assoc :picking? (boolean active?))
                      (when-not (:detached? @state)
                        (show-instance! @side-instance (:open? @state))))

                    (clear-popup-poll! []
                      (when-let [timer (:popup-poll @state)]
                        (.clearInterval inspected-window timer))
                      (swap! state assoc :popup-poll nil))

                    (close-popup! []
                      (clear-popup-poll!)
                      (when-let [popup (:popup @state)]
                        (when-not (.-closed popup)
                          (swap! state assoc :closing-popup? true)
                          (.close popup)
                          (swap! state assoc :closing-popup? false)))
                      (swap! state assoc :popup nil :popup-instance nil))

                    (attach! [& [close?]]
                      (when (:detached? @state)
                        (swap! state assoc :detached? false)
                        (clear-popup-poll!)
                        (when-let [instance (:popup-instance @state)]
                          (panel/stop! (:context instance)))
                        (if (not= false close?)
                          (close-popup!)
                          (swap! state assoc :popup nil :popup-instance nil))
                        (when (:open? @state)
                          (show-instance! @side-instance true)
                          (panel/start! (:context @side-instance)))))

                    (handle-popup-closed! []
                      (when (and (not (:closing-popup? @state)) (:detached? @state))
                        (attach! false)))

                    (detach! []
                      (when (and (:open? @state) (not (:detached? @state)))
                        (if-let [popup (.open inspected-window "" "_blank"
                                              "popup=yes,width=1100,height=820,resizable=yes,scrollbars=no")]
                          (let [popup-document (.-document popup)
                                popup-body (or (.-body popup-document)
                                               (.appendChild (.-documentElement popup-document)
                                                             (.createElement popup-document "body")))]
                            (set! (.-title popup-document)
                                  (str "re-frame.vertica | "
                                       (or (.-title inspected-document)
                                           (.. inspected-window -location -host))))
                            (set! (.. popup-document -documentElement -style -cssText)
                                  "margin:0;background:#0b0e13;overflow:hidden")
                            (set! (.. popup-body -style -cssText)
                                  "margin:0;background:#0b0e13;overflow:hidden")
                            (.replaceChildren popup-body)
                            (swap! state assoc :popup popup :detached? true :picking? false)
                            (panel/stop! (:context @side-instance))
                            (show-instance! @side-instance false)
                            (let [instance (create-instance!
                                             {:document popup-document
                                              :css css
                                              :floating? true
                                              :storage (.-localStorage inspected-window)
                                              :inspected-document inspected-document
                                              :inspected-window inspected-window
                                              :on-close #(set-open! false)
                                              :on-detach #(attach!)
                                              :on-picking-change on-picking-change})]
                              (swap! state assoc :popup-instance instance)
                              (show-instance! instance true)
                              (panel/start! (:context instance)))
                            (.addEventListener popup "keydown" shortcut-handler true)
                            (.addEventListener popup "beforeunload"
                                               #(.setTimeout inspected-window
                                                             handle-popup-closed! 0)
                                               #js {:once true})
                            (swap! state assoc :popup-poll
                                   (.setInterval inspected-window
                                                 #(when (.-closed popup)
                                                    (handle-popup-closed!))
                                                 500))
                            (.focus popup))
                          (panel/set-status!
                            (:context @side-instance)
                            "The browser blocked the floating window. Allow popups for this site and try again."))))

                    (set-open! [visible?]
                      (let [visible? (boolean visible?)]
                        (if (= visible? (:open? @state))
                          (when (and visible? (:detached? @state)
                                     (:popup @state) (not (.-closed (:popup @state))))
                            (.focus (:popup @state)))
                          (do
                            (swap! state assoc :open? visible?)
                            (if-not visible?
                              (do
                                (swap! state assoc :picking? false)
                                (if (:detached? @state)
                                  (do
                                    (swap! state assoc :detached? false)
                                    (when-let [instance (:popup-instance @state)]
                                      (panel/stop! (:context instance)))
                                    (close-popup!))
                                  (panel/stop! (:context @side-instance)))
                                (show-instance! @side-instance false))
                              (do
                                (show-instance! @side-instance true)
                                (panel/start! (:context @side-instance))))))
                        visible?))

                    (toggle! [] (set-open! (not (:open? @state))))

                    (shortcut-handler [event]
                      (when (toggle-shortcut? event)
                        (.preventDefault event)
                        (.stopPropagation event)
                        (toggle!)))]
              (reset! side-instance
                      (create-instance!
                        {:document inspected-document
                         :css css
                         :floating? false
                         :storage (.-localStorage inspected-window)
                         :inspected-document inspected-document
                         :inspected-window inspected-window
                         :on-close #(set-open! false)
                         :on-detach detach!
                         :on-picking-change on-picking-change}))
              (let [api #js {:shortcut toggle-shortcut
                             :show #(set-open! true)
                             :hide #(set-open! false)
                             :toggle toggle!
                             :detach detach!
                             :attach attach!
                             :isOpen #(boolean (:open? @state))
                             :isDetached #(boolean (:detached? @state))}]
                (.addEventListener inspected-window "keydown" shortcut-handler true)
                (.addEventListener inspected-window "beforeunload"
                                   #(when-let [popup (:popup @state)]
                                      (when-not (.-closed popup) (.close popup))))
                (gobj/set js/globalThis "__RE_FRAME_VERTICA_PANEL__" api)
                api)))))))
