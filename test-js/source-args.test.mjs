import assert from "node:assert/strict";
import test from "node:test";
import {
  addSourceResource,
  embeddedSources,
  indexClojureScriptSource,
  resolveArgumentNames,
  resolveSourceLocation
} from "../src-js/source-args.mjs";

test("extracts embedded ClojureScript from indexed source maps", () => {
  const map = {
    version: 3,
    sections: [
      { offset: { line: 0, column: 0 }, map: {
        version: 3,
        sources: ["src/app/views.cljs", "vendor.js"],
        sourcesContent: ["(ns app.views)", "ignored"]
      } },
      { offset: { line: 10, column: 0 }, map: {
        version: 3,
        sources: ["src/app/panel.cljc"],
        sourcesContent: ["(ns app.panel)"]
      } }
    ]
  };

  assert.deepEqual(embeddedSources(JSON.stringify(map)), [
    { url: "src/app/views.cljs", content: "(ns app.views)" },
    { url: "src/app/panel.cljc", content: "(ns app.panel)" }
  ]);
});

test("resolves a component definition to its source URL and position", () => {
  const source = `(ns example.views)\n\n;; heading\n(defn card [item]\n  [:article item])`;
  const index = indexClojureScriptSource(source, new Map(), "file:///workspace/src/example/views.cljs");

  assert.deepEqual(resolveSourceLocation(index, "example.views/card"), {
    componentName: "example.views/card",
    url: "file:///workspace/src/example/views.cljs",
    line: 4,
    column: 1
  });
});

test("indexes component vars even when their value is not a literal fn", () => {
  const source = `(ns example.views)\n(def card (reagent.core/create-class {}))`;
  const index = indexClojureScriptSource(source, new Map(), "https://example.test/src/example/views.cljs");

  assert.deepEqual(resolveSourceLocation(index, "example.views/card"), {
    componentName: "example.views/card",
    url: "https://example.test/src/example/views.cljs",
    line: 2,
    column: 1
  });
  assert.equal(resolveArgumentNames(index, "example.views/card", 0), null);
});

test("resolves embedded source paths relative to the source map", () => {
  const map = {
    version: 3,
    sourceRoot: "../src",
    sources: ["example/views.cljs"],
    sourcesContent: ["(ns example.views) (defn card [] nil)"]
  };

  assert.deepEqual(embeddedSources(map, "https://example.test/js/app.js.map"), [{
    url: "https://example.test/src/example/views.cljs",
    content: "(ns example.views) (defn card [] nil)"
  }]);
});

test("resolves defn argument names by namespace, component, and arity", () => {
  const source = `
    (ns ai.ibis.app.code-details.views)
    ;; (defn diag-details-panel [wrong])
    (defn ^:private diag-details-panel
      "Details panel"
      [{:keys [code version] :as selection} language]
      [:div code language])`;
  const index = indexClojureScriptSource(source);

  assert.deepEqual(
    resolveArgumentNames(index, "ai.ibis.app.code-details.views/diag-details-panel", 2),
    ["{:keys [code version] :as selection}", "language"]
  );
  assert.equal(resolveArgumentNames(index, "ai.ibis.app.code-details.views/missing", 2), null);
  assert.equal(resolveArgumentNames(index, "ai.ibis.app.code-details.views/diag-details-panel", 1), null);
});

test("selects a matching multi-arity signature and labels rest arguments", () => {
  const source = `
    (ns example.cards)
    (defn card
      ([item] item)
      ([item selected? & children] children))`;
  const index = indexClojureScriptSource(source);

  assert.deepEqual(resolveArgumentNames(index, "example.cards/card", 1), ["item"]);
  assert.deepEqual(resolveArgumentNames(index, "example.cards/card", 4), [
    "item", "selected?", "children[0]", "children[1]"
  ]);
});

test("supports components declared as def plus fn", () => {
  const index = new Map();
  addSourceResource(index, "https://example.test/app.cljs", `
    (ns example.app)
    (def panel (fn named-panel [state dispatch] [state dispatch]))`);

  assert.deepEqual(resolveArgumentNames(index, "example.app/panel", 2), ["state", "dispatch"]);
});

test("argument metadata does not become a runtime argument", () => {
  const index = indexClojureScriptSource(`
    (ns example.events)
    (defn row [^js event ^{:tag string} label] [event label])`);

  assert.deepEqual(resolveArgumentNames(index, "example.events/row", 2), ["event", "label"]);
});

test("never shortens a destructured prop name", () => {
  const destructuring = "{:keys [selected-diagnosis-code selected-diagnosis-description selected-diagnosis-ancestors selected-diagnosis-inclusion-terms] :as diagnostic-properties}";
  const index = indexClojureScriptSource(`
    (ns example.details)
    (defn panel [${destructuring}] nil)`);

  assert.deepEqual(resolveArgumentNames(index, "example.details/panel", 1), [destructuring]);
});
