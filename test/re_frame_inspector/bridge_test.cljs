(ns re-frame-inspector.bridge-test
  (:require [cljs.test :refer-macros [deftest is]]
            [cognitect.transit :as transit]
            [clojure.walk :as walk]
            [goog.object :as gobj]
            [re-frame-inspector.bridge :as bridge]
            [re-frame-inspector.shared :as shared]))

(defn- decode [encoded]
  (walk/keywordize-keys (transit/read (transit/reader :json) encoded)))

(deftest versioned-transit-bridge
  (let [api (bridge/install!)
        capabilities (decode (bridge/capabilities))]
    (is (= shared/protocol-version (:protocol capabilities)))
    (is (some #{"transit-json"} (:features capabilities)))
    (is (fn? (gobj/get api "selectElement")))
    (is (fn? (gobj/get api "logNode")))))

(deftest incompatible-request
  (let [writer (transit/writer :json)
        request (transit/write writer #js {:protocol 99 :action "status"})
        response (decode (bridge/request request))]
    (is (false? (:ok response)))
    (is (re-find #"Upgrade" (:error response)))))
