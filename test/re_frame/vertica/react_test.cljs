(ns re-frame.vertica.react-test
  (:require [cljs.test :refer-macros [deftest is]]
            [goog.object :as gobj]
            [re-frame.vertica.react :as react]))

(deftest detects-bundled-react-without-a-global
  (let [global-react (gobj/get js/globalThis "React")]
    (try
      (gobj/remove js/globalThis "React")
      (is (= 18 (react/react-major)))
      (is (react/supported-react?))
      (finally
        (when global-react
          (gobj/set js/globalThis "React" global-react))))))

(deftest ignores-missing-render-reactions-without-warning
  (let [component #js {:tag 1
                       :stateNode #js {}
                       :type (fn PlainComponent [])
                       :memoizedProps nil
                       :return nil}
        host #js {:tag 5 :return component}
        element #js {:tagName "DIV"}]
    (gobj/set element "__reactFiber$warning-test" host)
    (let [result (react/owning-components element)]
      (is (= 1 (count (:components result))))
      (is (nil? (:unsupported result))))))

(deftest normalizes-compiled-clojurescript-component-names
  (is (= "ai.ibis.mzg2.app.coding.preview/section-wrapper-tw"
         (react/normalize-component-name
           "ai$ibis$mzg2$app$coding$preview$section_wrapper_tw")))
  (is (= "ai.ibis.mzg2.app.coding.preview/doc-details-fc-tw"
         (react/normalize-component-name
           "ai.ibis.mzg2.app.coding.preview.doc_details_fc_tw")))
  (is (nil? (react/normalize-component-name "G__45009"))))

(deftest form-two-components-prefer-the-original-argv-name
  (let [outer (fn [])
        inner (fn [])
        _ (gobj/set outer "displayName" "example$views$patient_card")
        _ (gobj/set inner "displayName" "G__45009")
        fiber #js {:stateNode #js {:reagentRender inner
                                   :constructor #js {:displayName ""}}
                   :memoizedProps #js {:argv [outer]}
                   :type inner}]
    (is (= "example.views/patient-card" (react/component-name fiber)))))

(deftest reagent-root-elements-stop-at-other-host-elements
  (let [reaction #js {}
        component #js {:tag 1
                       :stateNode #js {:cljsRatom reaction}
                       :type (fn ReagentCard [])
                       :memoizedProps nil
                       :return nil}
        root-fiber #js {:tag 5 :return component}
        nested-fiber #js {:tag 5 :return root-fiber}
        root #js {:tagName "ARTICLE" :parentElement nil}
        nested #js {:tagName "SPAN" :parentElement root}]
    (gobj/set root "__reactFiber$root-test" root-fiber)
    (gobj/set nested "__reactFiber$nested-test" nested-fiber)
    (is (react/reagent-root-element? root))
    (is (not (react/reagent-root-element? nested)))
    (is (identical? root (react/nearest-reagent-root-element nested)))))
