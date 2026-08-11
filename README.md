# re-frame.vertica

`re-frame.vertica` is a Chrome DevTools inspector that follows one selected UI element through a vertical slice of a re-frame application:

```text
app-db paths → subscriptions → props → Reagent components → DOM element
```

It shows the data and render dependencies that contributed to the selected element, rather than presenting the entire application as one global graph. The inspector is independent of re-frame-10x and can be used alongside it.

## Install

Two matching pieces are required: the preload library runs inside the application, while the unpacked Chrome extension provides the DevTools panel. Keep both on the same released version.

### 1. Add the preload to the application

With `deps.edn`, add the released [Clojars artifact](https://clojars.org/net.clojars.frknzd/re-frame.vertica):

```clojure
{:deps
 {net.clojars.frknzd/re-frame.vertica {:mvn/version "0.1.1"}}}
```

For a shadow-cljs configuration that declares Maven dependencies directly:

```clojure
{:dependencies
 [[net.clojars.frknzd/re-frame.vertica "0.1.1"]]}
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

### 2. Install the DevTools extension manually

1. Open the repository's [latest GitHub release](https://github.com/frknzd/re-frame.vertica/releases/latest).
2. Download `re-frame.vertica-devtools-vVERSION.zip`. `SHA256SUMS` in the same release can be used to verify the download.
3. Extract the ZIP to a permanent local directory. Do not delete that directory while the extension is installed.
4. Open `chrome://extensions` in Chrome and enable **Developer mode**.
5. Select **Load unpacked**, then choose the extracted directory containing `manifest.json`.
6. Open DevTools on an application with the preload enabled and select the **re-frame.vertica** tab.

To upgrade, download and extract the new ZIP over the directory (or select a new directory), then click **Reload** on the extension card in `chrome://extensions`. Upgrade the application's Clojars dependency to the same version and rebuild it.

### 3. Inspect a vertical slice

Select an element in Chrome's Elements panel, or use **Pick** in the re-frame.vertica top bar. The panel locks onto the closest Reagent render owner and shows its app-db paths, subscriptions, live props, component ownership, and selected DOM element.

The top bar also provides parent, child, and sibling navigation; a refresh action; and a persistent setting for purple Reagent component boxes. Pick mode accepts Reagent roots only and suppresses page clicks while active. The selected element keeps its blue page highlight while the DevTools panel is open.

All truncation indicators are interactive. Click `{…}`, `[…]`, `… N more`, or **… Show all** to reveal additional data. Subscription levels are collapsible; level 0 starts open and deeper levels start closed for each selection.

## What is traced

The selected leaf component's direct subscriptions are exact render dependencies. An ancestor subscription is included only when its output shares an immutable collection identity with a leaf argument or one of its nested collections. Primitive value equality is not treated as provenance.

Relevant layer-2 subscriptions are replayed against read-tracking wrappers. The tracker records concrete keyword, map, vector, set, record, sequence, reduction, destructuring, `get`, and `get-in` accesses made by the current invocation. Only proven app-db paths are highlighted; their structural ancestors remain visible so the paths can be navigated. New snapshots do not synthesize wildcard paths.

Prop names are never shortened. When an inspected script's source map contains ClojureScript `sourcesContent`, re-frame.vertica recovers the component's original argument names, including destructuring and matching multi-arity signatures. If source text is unavailable, the panel keeps complete fallback labels such as `arg 0` instead of guessing.

Subscription and app-db badges show the full leaf Reagent component name responsible for that dependency. Shared dependencies can therefore show more than one leaf badge without conflating their parent chains.

The trace can be partial when data is transformed into a new object, comes from a local ratom, was registered before the preload, or passes through unsupported `reg-sub-raw`, Subscription alpha, disposed reactions, mutations, unfamiliar fibers, or closed shadow roots. Proven paths survive an unsupported read or replay exception and are labeled partial rather than broadened by a guess.

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
| Chrome build target | Chrome 120 or newer |
| Node.js used by the build | 20.x |
| Leiningen used by CI | 2.12.0 |
| Java used by CI | Temurin 21 |

React 17 and React 18 each have a separately compiled fixture. Other dependency versions may work, but are not part of the verified compatibility matrix. React 19, iframes, closed shadow roots, unresolved portals, and Firefox packaging are currently outside scope.

## Privacy and protocol

The preload exposes `globalThis.__RE_FRAME_VERTICA__` using protocol version `1`. Communication stays between the inspected page and the local DevTools extension. No application data is sent to a service by this project.

Snapshots are bounded to 300 nodes and 600 edges. Values, reactions, fibers, and DOM objects remain inside the inspected page and are accessed through snapshot-scoped opaque tokens. A protocol mismatch is rejected with an upgrade message, which is why the preload and extension versions should stay aligned.

## Develop and verify

Requirements are Node.js 20+, npm, Java, Leiningen, Clojure CLI, and Chrome.

```bash
npm ci
npm run verify
```

`npm run verify` runs the JVM shared-model tests, ClojureScript tracking tests, extension protocol and parser tests, the production preload and extension build, and both React fixture builds. The unpacked output is written to `dist/extension`.

For a manual fixture test, serve either `dist/fixtures/react17` or `dist/fixtures/react18` over HTTP, load `dist/extension` as an unpacked extension, and open its re-frame.vertica panel. Native Elements selection and cross-panel `inspect(element)` are not reliably exposed to extension automation, so those interactions remain part of the manual smoke test.

## License

[MIT](LICENSE)
