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
