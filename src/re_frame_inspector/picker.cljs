(ns re-frame-inspector.picker
  (:require [goog.object :as gobj]
            [re-frame-inspector.state :as state]))

(defonce session (atom nil))

(defn active? [] (some? @session))

(defn- make-overlay []
  (let [overlay (.createElement js/document "div")]
    (set! (.-id overlay) "__re-frame-inspector-highlight")
    (set! (.-style.cssText overlay)
          (str "position:fixed;z-index:2147483647;pointer-events:none;"
               "border:2px solid #38bdf8;background:rgba(56,189,248,.15);"
               "box-sizing:border-box;display:none"))
    (.appendChild (.-documentElement js/document) overlay)
    overlay))

(defn- place-overlay! [overlay element]
  (if (and element (not= element overlay))
    (let [rect (.getBoundingClientRect element)
          style (.-style overlay)]
      (set! (.-display style) "block")
      (set! (.-left style) (str (.-left rect) "px"))
      (set! (.-top style) (str (.-top rect) "px"))
      (set! (.-width style) (str (.-width rect) "px"))
      (set! (.-height style) (str (.-height rect) "px")))
    (set! (.. overlay -style -display) "none")))

(declare stop!)

(defn- sync-elements! [element]
  (when-let [inspect-fn (gobj/get js/globalThis "inspect")]
    (when (fn? inspect-fn) (inspect-fn element))))

(defn start! []
  (when-not @session
    (reset! state/picker-outcome nil)
    (let [overlay (make-overlay)
          move (fn [event]
                 (let [element (.elementFromPoint js/document (.-clientX event) (.-clientY event))]
                   (place-overlay! overlay element)
                   (when-not (identical? element @state/hover-element)
                     (reset! state/hover-element element)
                     (state/bump!))))
          click (fn [event]
                  (.preventDefault event)
                  (.stopImmediatePropagation event)
                  (let [element (.elementFromPoint js/document (.-clientX event) (.-clientY event))]
                    (reset! state/selected-element element)
                    (reset! state/hover-element nil)
                    (reset! state/picker-outcome :locked)
                    (state/bump!)
                    (sync-elements! element)
                    (stop!)))
          keydown (fn [event]
                    (when (= "Escape" (.-key event))
                      (.preventDefault event)
                      (reset! state/hover-element nil)
                      (reset! state/picker-outcome :cancelled)
                      (state/bump!)
                      (stop!)))]
      (.addEventListener js/document "pointermove" move true)
      (.addEventListener js/document "click" click true)
      (.addEventListener js/document "keydown" keydown true)
      (reset! session {:overlay overlay :move move :click click :keydown keydown})
      (state/bump!)))
  true)

(defn stop! []
  (when-let [{:keys [overlay move click keydown]} @session]
    (.removeEventListener js/document "pointermove" move true)
    (.removeEventListener js/document "click" click true)
    (.removeEventListener js/document "keydown" keydown true)
    (.remove overlay)
    (reset! session nil)
    (state/bump!))
  true)
