(ns re-frame.vertica.ui.protocol-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.ui.protocol :as protocol]))

(deftest bridge-errors-are-actionable
  (is (re-find #"Preload missing" (protocol/compatibility-message nil)))
  (is (re-find #"__RE_FRAME_VERTICA__" (protocol/compatibility-message nil)))
  (is (re-find #"Rebuild the application"
               (protocol/compatibility-message {:protocol 99})))
  (is (= "Connected"
         (protocol/compatibility-message
           {:protocol protocol/protocol-version
            :registration-hook true
            :react-supported true}))))

(deftest distinct-compatibility-states
  (is (re-find #"loaded too late"
               (protocol/compatibility-message
                 {:protocol 1 :registration-hook false})))
  (is (re-find #"React 19"
               (protocol/compatibility-message
                 {:protocol 1 :registration-hook true
                  :react-supported false :react-major 19}))))
