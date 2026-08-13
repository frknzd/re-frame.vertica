(ns re-frame.vertica.bridge-test
  (:require [cljs.test :refer-macros [deftest is]]
            [cognitect.transit :as transit]
            [clojure.walk :as walk]
            [goog.object :as gobj]
            [re-frame.db :as db]
            [re-frame.vertica.bridge :as bridge]
            [re-frame.vertica.shared :as shared]
            [re-frame.vertica.state :as state]))

(defn- decode [encoded]
  (walk/keywordize-keys (transit/read (transit/reader :json) encoded)))

(deftest versioned-transit-bridge
  (let [api (bridge/install!)
        capabilities (decode (bridge/capabilities))]
    (is (= shared/protocol-version (:protocol capabilities)))
    (is (some #{"transit-json"} (:features capabilities)))
    (is (some #{"reagent-only-picker"} (:features capabilities)))
    (is (some #{"reagent-component-highlights"} (:features capabilities)))
    (is (some #{"causal-render-provenance"} (:features capabilities)))
    (is (fn? (gobj/get api "selectElement")))
    (is (fn? (gobj/get api "navigateElement")))
    (is (fn? (gobj/get api "selectedElement")))
    (is (fn? (gobj/get api "setComponentHighlights")))
    (is (fn? (gobj/get api "expandNode")))
    (is (fn? (gobj/get api "expandAppDbPath")))
    (is (fn? (gobj/get api "logNode")))))

(deftest navigates-relative-dom-elements
  (let [parent #js {:id "parent"}
        previous #js {:id "previous"}
        child #js {:id "child"}
        next #js {:id "next"}
        selected #js {:id "selected"
                      :parentElement parent
                      :firstElementChild child
                      :previousElementSibling previous
                      :nextElementSibling next}
        inspector-overlay #js {:id "__re-frame.vertica-highlight"}]
    (is (identical? parent (bridge/relative-element selected "parent")))
    (is (identical? child (bridge/relative-element selected "child")))
    (is (identical? previous (bridge/relative-element selected "previous")))
    (is (identical? next (bridge/relative-element selected "next")))
    (is (nil? (bridge/relative-element selected "unknown")))
    (is (nil? (bridge/relative-element #js {:nextElementSibling inspector-overlay} "next")))))

(deftest selection-boundaries-reset-app-db-expansions
  (reset! state/selection-generation 12)
  (reset! state/app-db-expansions {[:items] 20})
  (is (= 13 (state/begin-selection!)))
  (is (= 13 @state/selection-generation))
  (is (empty? @state/app-db-expansions)))

(deftest expands-a-token-to-its-complete-value
  (let [token (state/new-token!)
        value {:long (apply str (repeat 1500 "x"))}]
    (state/remember-value! token value)
    (let [response (decode (bridge/expand-node token))]
      (is (:ok response))
      (is (= (shared/value-string value) (:value response))))))

(deftest app-db-branches-remain-strictly-pruned
  (reset! db/app-db {:items (vec (range 100))})
  (reset! state/app-db-paths [[:items 50]])
  (reset! state/app-db-expansions {})
  (let [response (decode (bridge/expand-app-db-path "[:items]"))]
    (is (:ok response))
    (is (= ["50"] (mapv :key (get-in response [:node :children]))))))

(deftest app-db-collection-expansion-advances-from-the-visible-page
  (reset! db/app-db {:items (vec (range 25))})
  (reset! state/app-db-paths [[:items]])
  (reset! state/app-db-expansions {})
  (let [first-response (decode (bridge/expand-app-db-path "[:items]" 0))
        first-children (get-in first-response [:node :children])
        second-response (decode (bridge/expand-app-db-path "[:items]" 10))
        second-children (get-in second-response [:node :children])]
    (is (:ok first-response))
    (is (= 11 (count first-children)))
    (is (= "… 15 more" (get-in (last first-children) [:node :text])))
    (is (= 10 (get-in (last first-children) [:node :visible-count])))
    (is (:ok second-response))
    (is (= 21 (count second-children)))
    (is (= "… 5 more" (get-in (last second-children) [:node :text])))
    (is (= 20 (get @state/app-db-expansions [:items])))))

(deftest expands-an-app-db-scalar-to-its-complete-value
  (let [value (apply str (repeat 400 "x"))]
    (reset! db/app-db {:long value})
    (reset! state/app-db-paths [[:long]])
    (reset! state/app-db-expansions {})
    (let [response (decode (bridge/expand-app-db-path "[:long]"))]
      (is (:ok response))
      (is (= (pr-str value) (:value response))))))

(deftest incompatible-request
  (let [writer (transit/writer :json)
        request (transit/write writer #js {:protocol 99 :action "status"})
        response (decode (bridge/request request))]
    (is (false? (:ok response)))
    (is (re-find #"Upgrade" (:error response)))))
