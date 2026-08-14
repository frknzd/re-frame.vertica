(ns re-frame.vertica.ui.panel-host-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.ui.panel-host :as panel-host]))

(deftest uses-non-conflicting-shortcut
  (is (= "Ctrl+Shift+V" panel-host/toggle-shortcut))
  (is (panel-host/toggle-shortcut?
        #js {:ctrlKey true :shiftKey true :altKey false :metaKey false :code "KeyV"}))
  (is (not (panel-host/toggle-shortcut?
             #js {:ctrlKey true :shiftKey true :altKey false :metaKey false :code "KeyX"})))
  (is (not (panel-host/toggle-shortcut?
             #js {:ctrlKey false :shiftKey true :altKey false :metaKey false :code "KeyV"}))))
