(ns re-frame.vertica.ui.source-args-test
  (:require [cljs.test :refer-macros [deftest is]]
            [re-frame.vertica.ui.source-args :as source-args]))

(deftest extracts-embedded-clojurescript
  (let [source-map {:version 3
                    :sections [{:map {:version 3
                                      :sources ["src/app/views.cljs" "vendor.js"]
                                      :sourcesContent ["(ns app.views)" "ignored"]}}
                               {:map {:version 3
                                      :sources ["src/app/panel.cljc"]
                                      :sourcesContent ["(ns app.panel)"]}}]}]
    (is (= [{:url "src/app/views.cljs" :content "(ns app.views)"}
            {:url "src/app/panel.cljc" :content "(ns app.panel)"}]
           (source-args/embedded-sources (js/JSON.stringify (clj->js source-map)))))))

(deftest resolves-source-location
  (let [source "(ns example.views)\n\n;; heading\n(defn card [item]\n  [:article item])"
        index (source-args/index-clojurescript-source
                source {} "file:///workspace/src/example/views.cljs")]
    (is (= {:component-name "example.views/card"
            :url "file:///workspace/src/example/views.cljs"
            :line 4 :column 1}
           (source-args/resolve-source-location index "example.views/card")))))

(deftest resolves-source-paths-relative-to-map
  (is (= [{:url "https://example.test/src/example/views.cljs"
           :content "(ns example.views) (defn card [] nil)"}]
         (source-args/embedded-sources
           {:version 3 :sourceRoot "../src"
            :sources ["example/views.cljs"]
            :sourcesContent ["(ns example.views) (defn card [] nil)"]}
           "https://example.test/js/app.js.map"))))

(deftest resolves-defn-arguments-and-destructuring
  (let [destructuring "{:keys [code version] :as selection}"
        source (str "(ns example.views)\n"
                    "(defn ^:private card \"doc\" [" destructuring " language] nil)")
        index (source-args/index-clojurescript-source source)]
    (is (= [destructuring "language"]
           (source-args/resolve-argument-names index "example.views/card" 2)))
    (is (nil? (source-args/resolve-argument-names index "example.views/card" 1)))))

(deftest selects-multi-arity-and-rest-signatures
  (let [index (source-args/index-clojurescript-source
                "(ns example.cards) (defn card ([item] item) ([item selected? & children] children))")]
    (is (= ["item"] (source-args/resolve-argument-names index "example.cards/card" 1)))
    (is (= ["item" "selected?" "children[0]" "children[1]"]
           (source-args/resolve-argument-names index "example.cards/card" 4)))))

(deftest supports-def-plus-fn-and-argument-metadata
  (let [index (source-args/add-source-resource
                {} "https://example.test/app.cljs"
                "(ns example.app) (def panel (fn named-panel [state dispatch] nil))")
        metadata-index (source-args/index-clojurescript-source
                         "(ns example.events) (defn row [^js event ^{:tag string} label] nil)")]
    (is (= ["state" "dispatch"]
           (source-args/resolve-argument-names index "example.app/panel" 2)))
    (is (= ["event" "label"]
           (source-args/resolve-argument-names metadata-index "example.events/row" 2)))))

(deftest indexes-defs-without-literal-functions
  (let [index (source-args/index-clojurescript-source
                "(ns example.views) (def card (reagent.core/create-class {}))"
                {} "https://example.test/src/example/views.cljs")]
    (is (= "https://example.test/src/example/views.cljs"
           (:url (source-args/resolve-source-location index "example.views/card"))))
    (is (nil? (source-args/resolve-argument-names index "example.views/card" 0)))))
