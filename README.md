# re-frame Inspector

`re-frame Inspector` is a standalone Chrome DevTools extension for tracing the data used by a selected DOM element:

```text
app-db paths  →  subscriptions  →  owning Reagent components  →  DOM element
```

It is deliberately separate from re-frame-10x and can be loaded alongside it. Version 1 targets re-frame 1.4.6+, standard `reg-sub`, Reagent 1.2, React 17/18, the main frame, and open shadow roots.

## Add the preload to an application

Install this project as a local/root dependency, then load the preload before application namespaces register subscriptions. With shadow-cljs:

```clojure
{:deps {:local/root "/absolute/path/to/re-frame-inspector"}
 :builds
 {:app
  {:target :browser
   :devtools {:preloads [re-frame-inspector.preload]}}}}
```

For configurations that expose preloads directly, the public preload entry is:

```clojure
:preloads [re-frame-inspector.preload]
```

Reload the compiled application after changing preload configuration. A preload loaded after subscription namespaces cannot recover original computation functions; the panel reports that state explicitly.

## Build and load the extension

```bash
npm install
npm run build
```

Open `chrome://extensions`, enable **Developer mode**, choose **Load unpacked**, and select `dist/extension`. Open DevTools on the instrumented application and select **re-frame Inspector**.

Selecting a node in Chrome’s Elements panel updates the locked graph. **⌖ Pick** highlights the element under the pointer without modifying its layout, streams hover previews, locks on click, and cancels with Escape. Click a graph node to inspect its bounded preview or log the real in-page value to the inspected page’s console.

## Protocol and safety model

The preload publishes `globalThis.__RE_FRAME_INSPECTOR__` with protocol version `1` and these methods:

- `capabilities()`, `status()`, and `snapshot()`
- `selectElement(element)`
- `startPicker()` and `stopPicker()`
- `logNode(opaqueToken)`
- `request(transitJson)`

Return values are bounded Transit-JSON strings. Snapshots contain at most 300 nodes and 600 edges. Actual values, reactions, fibers, and DOM objects remain in the inspected page and are reachable from the extension only through snapshot-scoped opaque tokens. Mismatched bridge versions are rejected with an upgrade message.

Graph node kinds are `app-db-path`, `subscription`, `component`, and `element`. Edges are `data-input`, `render-input`, or `render-ownership`.

## Provenance semantics

Path inference replays only a relevant layer-2 subscription’s original pure computation function against read-tracking wrappers. It records accesses made by the current query/dynamic invocation, including keyword lookup, `get`, `get-in`, indexes, membership, destructuring, sequence/reduction, and map/vector/set/record traversal. Collection-wide reads use `*`.

Replay is isolated from the live reaction. An exception or unsupported mutation preserves every proven path and marks that subscription partial. Untaken branches are absent by design. The inspector never infers provenance from value equality or matching outputs.

`reg-sub-raw`, Subscription alpha, disposed reactions, local ratoms, registration before preload, and unfamiliar fibers are reported as partial/unsupported and do not prevent the rest of the graph from rendering. React 19, iframes, closed shadow roots, Firefox packaging, and unresolved portals are outside version 1.

## Verification

```bash
npm run verify
```

This runs JVM shared-model tests, ClojureScript tracking tests, mocked extension protocol tests, a production preload/extension build, and React 17/18 fixture builds.

Manual smoke test:

1. Serve `dist/fixtures/react17` or `dist/fixtures/react18` from a local HTTP server.
2. Open the fixture with both the unpacked Inspector and re-frame-10x installed.
3. In Elements, select `#selected-person`; confirm all owning components and shared subscription nodes appear.
4. Pick each person button, verify hover preview, click lock and Elements synchronization; start again and verify Escape cancels without changing the locked selection.
5. Inspect the `selected-id` and `people` path nodes, then log their values to the page console.
6. Change the selected person and confirm query/path previews update without duplicate shared nodes.

Chrome does not expose reliable automation for native Elements selection or cross-panel `inspect(element)`, so those interactions remain an explicit unpacked-extension smoke test.

