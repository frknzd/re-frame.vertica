(ns re-frame.vertica.ui.panel-host-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.ui.panel-host :as panel-host]))

(deftest panel-stays-above-inspection-overlays
  (is (> shared/panel-z-index shared/selection-highlight-z-index))
  (is (> shared/selection-highlight-z-index shared/component-highlight-z-index)))

(deftest uses-non-conflicting-shortcut
  (is (= "Ctrl+Shift+V" panel-host/toggle-shortcut))
  (is (panel-host/toggle-shortcut?
        #js {:ctrlKey true :shiftKey true :altKey false :metaKey false :code "KeyV"}))
  (is (not (panel-host/toggle-shortcut?
             #js {:ctrlKey true :shiftKey true :altKey false :metaKey false :code "KeyX"})))
  (is (not (panel-host/toggle-shortcut?
             #js {:ctrlKey false :shiftKey true :altKey false :metaKey false :code "KeyV"}))))

(deftest constrains-resizable-side-panel-to-the-viewport
  (is (= 720 (panel-host/default-panel-width 1000)))
  (is (= 1000 (panel-host/default-panel-width 1600)))
  (is (= 320 (panel-host/default-panel-width 320)))
  (is (= 360 (panel-host/constrain-panel-width 100 1200)))
  (is (= 1200 (panel-host/constrain-panel-width 1500 1200)))
  (is (= 320 (panel-host/constrain-panel-width 100 320))))
