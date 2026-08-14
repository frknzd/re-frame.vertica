# re-frame.vertica

`re-frame.vertica` is an in-app inspector that follows one selected UI element through a vertical slice of a re-frame application:

```text
app-db paths → subscriptions → props → Reagent components → DOM element
```

It shows the data and render dependencies that contributed to the selected element, rather than presenting the entire application as one global graph. The inspector is built into the development preload: users do not need to install a browser extension.

## Install

With `deps.edn`, add the released [Clojars artifact](https://clojars.org/net.clojars.frknzd/re-frame.vertica):

```clojure
{:deps
 {net.clojars.frknzd/re-frame.vertica {:mvn/version "0.3.4"}}}
```

For a shadow-cljs configuration that declares Maven dependencies directly:

```clojure
{:dependencies
 [[net.clojars.frknzd/re-frame.vertica "0.3.4"]]}
```

Then add the public preload namespace to the development build:

```clojure
{:builds
 {:app
  {:target :browser
   :devtools
   {:preloads [re-frame.vertica.preload]}}}}
```

Load the preload only in development. It must start before the application namespaces register their subscriptions, so restart the shadow-cljs build and reload the page after changing this setting. When it starts too late, the panel cannot recover the original subscription computation functions and reports the resulting partial trace.

## Use the panel

Focus the application and press `Ctrl+Shift+V` to open or close the panel. The shortcut intentionally differs from re-frame-10x's `Ctrl+Shift+X`, so both tools can be loaded without toggling each other. Click **Choose**, then click a Reagent element in the page. The panel locks onto the closest Reagent render owner and shows its app-db paths, subscriptions, live props, component ownership, and selected DOM element.

Use **Detach** to move the inspector into a floating browser window. **Attach** returns it to the application. If the floating window is closed directly, the inspector reattaches to the page. The application tab must remain open because the floating panel inspects that live runtime. Browsers can block a programmatically opened window; if that happens, allow popups for the development origin and click **Detach** again.

The panel can also be controlled programmatically:

```clojure
(re-frame.vertica.preload/show-panel! true)
(re-frame.vertica.preload/show-panel! false)
(re-frame.vertica.preload/toggle-panel!)
(re-frame.vertica.preload/detach-panel!)
```

The same controls are available to browser tooling on `globalThis.__RE_FRAME_VERTICA_PANEL__` as `show()`, `hide()`, `toggle()`, `detach()`, and `attach()`.

The top bar provides parent, child, and sibling navigation; a refresh action; and a persistent setting for purple Reagent component boxes. Choose mode accepts Reagent roots only, temporarily hides the attached panel so the full page is reachable, and suppresses application clicks while active. The selected element keeps its blue page highlight while the panel is open.

When an application script exposes an accessible ClojureScript source map with `sourcesContent`, the panel recovers original component argument names. It also shows a `↗ file.cljs:line` link for the selected component; in an in-page panel that link opens the source URL in a new tab. Cross-origin scripts must permit browser fetches for this optional source-map feature.

Value-preview truncation indicators are interactive; click **… Show all** to reveal the complete value. Subscription levels are collapsible; level 0 starts open and deeper levels start closed for each selection.

## What is traced

The selected leaf component's direct subscriptions are render candidates. An ancestor subscription is included only when its output shares an immutable collection identity with a leaf argument or one of its nested collections. Primitive value equality is not treated as provenance.

Relevant layer-2 subscriptions are first replayed against read-tracking wrappers to discover concrete keyword, map, vector, set, record, sequence, reduction, destructuring, `get`, and `get-in` candidates. Each candidate is then perturbed independently in a cloned app-db, the affected subscription DAG is replayed, and the selected component's render closure is evaluated without committing React work. A path is confirmed only when the normalized visible Hiccup/React branch corresponding to the selected DOM element changes. Callback and ref identities are ignored, so newly allocated closures do not create false positives.

The APP-DB tree contains contributing paths and only the structural ancestors needed to reach them—no neighboring entries or unrelated context is added automatically. Scalar vector entries use a compact, wrapping index/value view while nested collections remain expandable. Whole-collection and all-entry traversals are loaded ten entries at a time; collection summaries and **… more** controls can be used repeatedly to drill into nested values. When a map, vector, or set has more than five entries and every direct entry contributes, it starts collapsed with its contributing entry count and can be expanded normally. The tree has no inspector-imposed depth, path-count, node-count, or edge-count limit. Because counterfactual fuzzing is finite, it establishes evidence for the current execution path rather than a mathematical proof over every possible application state.

Prop names are never shortened. When accessible source maps contain ClojureScript `sourcesContent`, re-frame.vertica recovers the component's original argument names, including destructuring and matching multi-arity signatures. If source text is unavailable, the panel keeps complete fallback labels such as `arg 0` instead of guessing.

Subscription and app-db badges show the full leaf Reagent component name responsible for that dependency. Shared dependencies can therefore show more than one leaf badge without conflating their parent chains.

Some paths cannot be fully verified when data is transformed into a new object, was registered before the preload, or passes through unsupported `reg-sub-raw`, Subscription alpha, opaque custom derefables, disposed reactions, mutations, nondeterministic or throwing renders, unfamiliar fibers, or closed shadow roots. They remain in APP-DB with the same presentation as other contributing paths.

## Tested compatibility

The automated suite and production fixtures use these versions:

| Part | Tested version |
| --- | --- |
| Clojure | 1.12.0 |
| ClojureScript | 1.12.42 |
| shadow-cljs | 3.1.7 |
| re-frame | 1.4.7 |
| Reagent | 1.2.0 |
| React / ReactDOM | 17.0.2 and 18.3.1 |
| Transit CLJS | 0.8.280 |
| Browser | Chrome 120 or newer |
| Node.js used by the build | 20.x |
| Babashka used by the build | 1.12.209 |
| Leiningen used by CI | 2.12.0 |
| Java used by CI | Temurin 21 |

React 17 and React 18 each have a separately compiled fixture. Other dependency versions may work, but are not part of the verified compatibility matrix. React 19, iframes, closed shadow roots, and unresolved portals are currently outside scope.

## Privacy and runtime behavior

The panel and its bridge run locally in the inspected application's development build. The bridge is used directly by the attached or detached in-application panel and is not exposed as a page-global inspection API. No application data is sent to a service by this project.

Snapshots are not quantity-capped, so selecting a component backed by an exceptionally large dependency set can take significant time and memory. Values, reactions, fibers, and DOM objects stay in the application runtime and are accessed through snapshot-scoped opaque tokens. Counterfactual app-db values never replace the live app-db, and temporarily substituted subscription and argv fields are restored in `finally`.

## Develop and verify

Requirements are Node.js 20+, npm, Babashka 1.12.209+, Java, Leiningen, and Clojure CLI.

```bash
npm ci
bb verify
```

`bb verify` runs the JVM shared-model tests, the ClojureScript tracking and panel tests, the production preload build, and both React fixture builds. Run `bb tasks` to list the individual test, build, version-check, and release tasks. The shipped inspector runtime, including its Reagent/Hiccup panel, panel host, layout logic, tokenizer, and source-map parser, is implemented in ClojureScript. Build output is written under `dist/preload` and `dist/fixtures`.

For a manual fixture test, serve either `dist/fixtures/react17` or `dist/fixtures/react18` over HTTP, focus the page, and press `Ctrl+Shift+V`.

## License

[MIT](LICENSE)
