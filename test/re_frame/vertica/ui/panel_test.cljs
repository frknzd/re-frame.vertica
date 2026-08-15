(ns re-frame.vertica.ui.panel-test
  (:require [cljs.test :refer-macros [deftest is]]
            [reagent.core :as r]
            [reagent.dom.server :as rdom-server]
            [re-frame.vertica.ui.panel :as panel]))

(deftest formats-panel-render-errors
  (is (= "Unable to render value"
         (panel/panel-error-message (js/Error. "Unable to render value"))))
  (is (= "Unknown inspector rendering error"
         (panel/panel-error-message nil))))

(deftest renders-recursive-app-db-nodes-through-reagent
  (let [db-node-view (deref #'panel/db-node-view)
        state (r/atom {:collapsed-db-paths #{}
                       :expanded-default-db-paths #{}
                       :expanded-db-values {}
                       :loading-db-paths #{}
                       :app-db-association-patterns []
                       :graph {:edges []}})
        context {:state state :inspected-window js/globalThis}
        tree {:kind :map
              :path-label "[]"
              :open "{"
              :close "}"
              :child-count 2
              :all-children? false
              :children [{:key ":patient"
                          :node {:kind :map
                                 :path-label "[:patient]"
                                 :open "{"
                                 :close "}"
                                 :child-count 1
                                 :all-children? false
                                 :children [{:key ":name"
                                             :node {:kind :scalar
                                                    :path-label "[:patient :name]"
                                                    :text "\"Ada\""}}]}}
                         {:key ":visits"
                          :node {:kind :vector
                                 :path-label "[:visits]"
                                 :open "["
                                 :close "]"
                                 :child-count 2
                                 :all-children? false
                                 :children [{:key "0"
                                             :node {:kind :scalar
                                                    :path-label "[:visits 0]"
                                                    :text "1"}}
                                            {:key "1"
                                             :node {:kind :scalar
                                                    :path-label "[:visits 1]"
                                                    :text "2"}}]}}]}
        html (rdom-server/render-to-static-markup [db-node-view context tree])]
    (is (re-find #"db-tree-node" html))
    (is (re-find #"Ada" html))
    (is (re-find #"db-vector-entry" html))))

(deftest renders-five-subscription-levels-through-reagent
  (let [section-view (deref #'panel/section-view)
        nodes (mapv (fn [level]
                      {:id (str "subscription-" level)
                       :kind :subscription
                       :label (str "[:level-" level "]")
                       :preview (str level)
                       :depth level
                       :complete? true})
                    (range 5))
        section {:kind :subscription
                 :title "SUBSCRIPTIONS"
                 :identity "QUERY"
                 :value "VALUE"
                 :nodes nodes
                 :levels (mapv (fn [node]
                                 {:level (:depth node) :nodes [node]})
                               nodes)}
        state (r/atom {:collapsed-subscription-levels #{}
                       :collapsed-nodes #{}
                       :expanded-values {}
                       :loading-values #{}
                       :component-associations {}
                       :graph {:edges []}})
        context {:state state :inspected-window js/globalThis}
        html (rdom-server/render-to-static-markup
               [section-view context section {:edges []}])]
    (is (= 5 (count (re-seq #"subscription-level" html))))
    (is (re-find #"LEVEL 4" html))))
