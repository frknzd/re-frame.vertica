import * as transit from "transit-js";
import { bridgeExpression, compatibilityMessage, PROTOCOL_VERSION, transitToPlain } from "./protocol.mjs";
import { ednTokens } from "./edn-tokenizer.mjs";
import { appDbPathMatches, buildComponentAssociations, buildSections, leafMostComponents } from "./graph-layout.mjs";

const reader = transit.reader("json");
const statusEl = document.querySelector("#status");
const emptyEl = document.querySelector("#empty");
const graphEl = document.querySelector("#graph");
const pickButton = document.querySelector("#pick");
const refreshButton = document.querySelector("#refresh");
const componentBoxesButton = document.querySelector("#component-boxes");
const navigationButtons = [...document.querySelectorAll("#tree-nav [data-direction]")];
let graph = null;
let revision = -1;
let collapsedNodes = new Set();
let collapsedDbPaths = new Set();
let collapsedSubscriptionLevels = new Set();
let knownSubscriptionLevels = new Set();
let subscriptionLevelsInitialized = false;
let subscriptionSelectionId = null;
let expandedValues = new Map();
let expandedDbValues = new Map();
let lastSelection = null;
let tokenizerWorker = null;
let nextTokenRequest = 0;
const tokenRequests = new Map();
let sourceMapWorker = null;
let nextSourceMapRequest = 0;
const sourceMapRequests = new Map();
const pendingArgumentLookups = new Set();
const argumentNameCache = new Map();
const loadedSourceUrls = new Set();
let sourceResourcesReady = false;
let sourceResourcesLoading = null;
const WORKER_TOKEN_THRESHOLD = 8000;
const POLL_INTERVAL = 150;
const COMPONENT_BOXES_SETTING = "re-frame.vertica.component-boxes";
let evalTail = Promise.resolve();
let pollTimer = null;
let selectionRefreshRunning = false;
let selectionRefreshQueued = false;
let navigationRunning = false;
let refreshRunning = false;
let componentAssociations = new Map();
let appDbAssociationPatterns = [];

function componentBoxesPreference() {
  return localStorage.getItem(COMPONENT_BOXES_SETTING) !== "false";
}

function updateComponentBoxesButton(enabled) {
  componentBoxesButton.setAttribute("aria-pressed", String(enabled));
  componentBoxesButton.title = enabled ? "Hide Reagent component boxes" : "Show Reagent component boxes";
}

function inspectedEval(expression) {
  const evaluate = () => new Promise((resolve, reject) => {
    chrome.devtools.inspectedWindow.eval(expression, (result, exception) => {
      if (exception) reject(new Error(exception.description || exception.value || "Evaluation failed"));
      else resolve(result);
    });
  });
  // DevTools eval calls execute synchronous bridge work in the inspected page.
  // Serializing them prevents slow snapshots from accumulating behind the
  // polling timer and freezing both the panel and the application.
  const result = evalTail.then(evaluate, evaluate);
  evalTail = result.catch(() => undefined);
  return result;
}

function decode(encoded) {
  return encoded == null ? null : transitToPlain(reader.read(encoded), transit);
}

async function call(method, argumentExpression = "") {
  return decode(await inspectedEval(bridgeExpression(method, argumentExpression)));
}

function element(name, className, text) {
  const node = document.createElement(name);
  if (className) node.className = className;
  if (text != null) node.textContent = text;
  return node;
}

function appendEdnTokens(code, tokens) {
  const fragment = document.createDocumentFragment();
  for (const token of tokens) {
    if (token.type === "plain") fragment.append(document.createTextNode(token.text));
    else {
      const depthClass = token.type === "bracket" ? ` bracket-${token.depth % 6}` : "";
      fragment.append(element("span", `edn-token edn-${token.type}${depthClass}`, token.text));
    }
  }
  code.replaceChildren(fragment);
}

function getTokenizerWorker() {
  if (tokenizerWorker) return tokenizerWorker;
  tokenizerWorker = new Worker(chrome.runtime.getURL("edn-worker.js"));
  tokenizerWorker.addEventListener("message", event => {
    const pending = tokenRequests.get(event.data.id);
    if (!pending) return;
    tokenRequests.delete(event.data.id);
    pending.resolve(event.data.tokens);
  });
  tokenizerWorker.addEventListener("error", error => {
    for (const pending of tokenRequests.values()) pending.reject(error);
    tokenRequests.clear();
    tokenizerWorker.terminate();
    tokenizerWorker = null;
  });
  return tokenizerWorker;
}

function tokenizeOffThread(text) {
  return new Promise((resolve, reject) => {
    const id = ++nextTokenRequest;
    tokenRequests.set(id, { resolve, reject });
    try {
      getTokenizerWorker().postMessage({ id, text });
    } catch (error) {
      tokenRequests.delete(id);
      reject(error);
    }
  });
}

function getSourceMapWorker() {
  if (sourceMapWorker) return sourceMapWorker;
  sourceMapWorker = new Worker(chrome.runtime.getURL("source-map-worker.js"));
  sourceMapWorker.addEventListener("message", event => {
    const pending = sourceMapRequests.get(event.data.id);
    if (!pending) return;
    sourceMapRequests.delete(event.data.id);
    pending.resolve(event.data);
  });
  sourceMapWorker.addEventListener("error", error => {
    for (const pending of sourceMapRequests.values()) pending.reject(error);
    sourceMapRequests.clear();
    sourceMapWorker.terminate();
    sourceMapWorker = null;
  });
  return sourceMapWorker;
}

function sourceWorkerRequest(message) {
  return new Promise((resolve, reject) => {
    const id = ++nextSourceMapRequest;
    sourceMapRequests.set(id, { resolve, reject });
    try {
      getSourceMapWorker().postMessage({ ...message, id });
    } catch (error) {
      sourceMapRequests.delete(id);
      reject(error);
    }
  });
}

function argumentCacheKey(componentName, arity) {
  return `${componentName}\u0000${arity}`;
}

function applyCachedArgumentNames(nextGraph) {
  let changed = false;
  for (const node of nextGraph?.nodes || []) {
    if (node.kind !== "prop") continue;
    const componentName = node["component-name"];
    const arity = Number(node["argument-count"]);
    const argumentIndex = Number(node["argument-index"]);
    const names = argumentNameCache.get(argumentCacheKey(componentName, arity));
    const sourceName = names?.[argumentIndex];
    if (sourceName && node.label !== sourceName) {
      node.label = sourceName;
      changed = true;
    }
  }
  return changed;
}

async function resolveVisibleArgumentNames() {
  if (!sourceResourcesReady || !graph) return;
  const requests = [];
  for (const node of graph.nodes || []) {
    if (node.kind !== "prop") continue;
    const componentName = node["component-name"];
    const arity = Number(node["argument-count"]);
    if (!componentName || !Number.isInteger(arity)) continue;
    const key = argumentCacheKey(componentName, arity);
    if (argumentNameCache.has(key) || pendingArgumentLookups.has(key)) continue;
    pendingArgumentLookups.add(key);
    requests.push({ componentName, arity });
  }
  if (!requests.length) return;
  try {
    const response = await sourceWorkerRequest({ type: "resolve", requests });
    for (const result of response.results || []) {
      const key = argumentCacheKey(result.componentName, result.arity);
      pendingArgumentLookups.delete(key);
      argumentNameCache.set(key, result.names || null);
    }
    if (applyCachedArgumentNames(graph)) render(graph, { preserveScroll: true });
  } catch (_) {
    for (const request of requests) {
      pendingArgumentLookups.delete(argumentCacheKey(request.componentName, request.arity));
    }
  }
}

function sourceResourceUrl(resource) {
  return resource?.url || resource?.request?.url || "";
}

function sourceResource(resource) {
  return /\.(?:map|clj|cljs|cljc)(?:$|[?#])/i.test(sourceResourceUrl(resource));
}

function decodeResourceContent(content, encoding) {
  if (encoding !== "base64") return content;
  const binary = atob(content);
  const bytes = Uint8Array.from(binary, character => character.charCodeAt(0));
  return new TextDecoder().decode(bytes);
}

function resourceContent(resource) {
  return new Promise((resolve, reject) => {
    resource.getContent((content, encoding) => {
      if (content == null) reject(new Error(`Unable to read ${sourceResourceUrl(resource)}`));
      else resolve(decodeResourceContent(content, encoding));
    });
  });
}

function inspectedResources() {
  return new Promise(resolve => chrome.devtools.inspectedWindow.getResources(resolve));
}

function inspectedNetworkRequests() {
  return new Promise(resolve => chrome.devtools.network.getHAR(log => resolve(log?.entries || [])));
}

async function addSourceResource(resource, force = false) {
  if (!sourceResource(resource)) return;
  const url = sourceResourceUrl(resource);
  if (!force && loadedSourceUrls.has(url)) return;
  const content = await resourceContent(resource);
  await sourceWorkerRequest({ type: "add", url, content });
  loadedSourceUrls.add(url);
}

function loadSourceResources() {
  if (sourceResourcesLoading) return sourceResourcesLoading;
  sourceResourcesLoading = (async () => {
    sourceResourcesReady = false;
    getSourceMapWorker().postMessage({ type: "reset" });
    loadedSourceUrls.clear();
    const [resources, requests] = await Promise.all([inspectedResources(), inspectedNetworkRequests()]);
    for (const resource of [...resources, ...requests].filter(sourceResource)) {
      try { await addSourceResource(resource); } catch (_) { /* Source names are optional. */ }
    }
    argumentNameCache.clear();
    pendingArgumentLookups.clear();
    sourceResourcesReady = true;
    await resolveVisibleArgumentNames();
  })().finally(() => { sourceResourcesLoading = null; });
  return sourceResourcesLoading;
}

function ednCode(className, text) {
  const code = element("code", className);
  const source = String(text ?? "");
  if (source.length >= WORKER_TOKEN_THRESHOLD) {
    code.textContent = source;
    code.classList.add("tokenizing");
    tokenizeOffThread(source).then(tokens => {
      if (!code.isConnected) return;
      appendEdnTokens(code, tokens);
      code.classList.remove("tokenizing");
    }).catch(() => code.classList.remove("tokenizing"));
  } else {
    appendEdnTokens(code, ednTokens(source));
  }
  return code;
}

function renderComponentBadges(components = []) {
  const leaves = leafMostComponents(components, graph?.edges || []);
  if (!leaves.length) return null;
  const badges = element("span", "component-badges");
  badges.setAttribute("aria-label", `Leaf Reagent component: ${leaves.map(component => component.label).join("; ")}`);
  leaves.forEach(component => {
    const badge = element("span", "component-badge", component.label || "Anonymous component");
    badge.title = `Reagent component: ${component.label}`;
    badges.append(badge);
  });
  return badges;
}

function componentsForDbNode(node) {
  const byId = new Map();
  for (const pattern of appDbAssociationPatterns) {
    if (!appDbPathMatches(node?.["association-path"], pattern.path)) continue;
    for (const component of pattern.components) byId.set(component.id, component);
  }
  return [...byId.values()].sort((a, b) => String(a.label).localeCompare(String(b.label)));
}

async function toggleFullValue(value, node, showingFullValue) {
  if (value.classList.contains("loading")) return;
  if (showingFullValue) {
    expandedValues.delete(node.id);
    render(graph, { preserveScroll: true });
    return;
  }
  value.classList.add("loading");
  value.setAttribute("aria-busy", "true");
  try {
    const result = await call("expandNode", JSON.stringify(node.token));
    if (!result?.ok) throw new Error(result?.error || "Unable to expand value");
    expandedValues.set(node.id, { token: node.token, value: result.value });
    render(graph, { preserveScroll: true });
  } catch (error) {
    value.classList.remove("loading");
    value.removeAttribute("aria-busy");
    showError(error);
  }
}

function renderNode(node, section) {
  const collapsed = collapsedNodes.has(node.id);
  const row = element("article", `node-row ${node.kind}${node["complete?"] === false ? " partial" : ""}${collapsed ? " collapsed" : ""}`);
  if (node["complete?"] === false) {
    row.title = node.reason || "App-db provenance is incomplete for this subscription.";
  }
  row.dataset.nodeId = node.id;
  row.style.setProperty("--depth", Math.min(node.depth || 0, 8));

  const content = element("div", "node-content");
  const identity = element("div", "identity-cell");
  const tree = element("div", "tree-indent");
  tree.append(element("span", "kind-dot"));
  const label = ednCode("node-identity", node.label);
  tree.append(label);
  identity.append(tree);
  if (node.kind === "subscription" || node.kind === "prop") {
    const badges = renderComponentBadges(componentAssociations.get(node.id));
    if (badges) {
      badges.classList.add("subscription-component-badges");
      identity.append(badges);
    }
  }
  content.append(identity);

  if (section.value) {
    const value = element("div", "value-cell");
    const expanded = expandedValues.get(node.id);
    const showingFullValue = expanded?.token === node.token;
    value.append(ednCode("node-value", showingFullValue ? expanded.value : (node.preview || "—")));
    if (node["preview-truncated?"]) {
      value.classList.add("expandable");
      if (showingFullValue) value.classList.add("expanded");
      value.title = showingFullValue ? "Double-click to show the preview" : "Double-click to show the complete value";
      const more = element("button", "value-more", showingFullValue ? "Show less" : "… Show all");
      more.type = "button";
      more.title = showingFullValue ? "Show the shortened preview" : "Show the complete value";
      more.addEventListener("click", event => {
        event.preventDefault();
        event.stopPropagation();
        toggleFullValue(value, node, showingFullValue);
      });
      value.append(more);
      value.tabIndex = 0;
      value.addEventListener("dblclick", event => {
        event.preventDefault();
        toggleFullValue(value, node, showingFullValue);
      });
      value.addEventListener("keydown", event => {
        if (event.key !== "Enter") return;
        event.preventDefault();
        toggleFullValue(value, node, showingFullValue);
      });
    }
    content.append(value);
  }

  const toggle = element("button", "row-toggle", collapsed ? "+" : "−");
  toggle.type = "button";
  toggle.title = collapsed ? "Expand row" : "Collapse row";
  toggle.setAttribute("aria-label", toggle.title);
  toggle.addEventListener("click", event => {
    event.stopPropagation();
    if (collapsed) collapsedNodes.delete(node.id); else collapsedNodes.add(node.id);
    render(graph, { preserveScroll: true });
  });
  row.append(content, toggle);
  return row;
}

function dbNodeState(node) {
  if (node.kind === "ellipsis") return "ellipsis";
  if (node["exact?"]) return "exact";
  if (node["touched?"]) return "ancestor";
  return "context";
}

function isDbCollection(node) {
  return node && ["map", "vector", "set"].includes(node.kind);
}

function renderDbValue(text, path, className = "") {
  const value = element("span", `db-tree-value ${className}`.trim());
  value.append(ednCode("db-tree-code", text));
  return value;
}

function replaceDbBranch(node, path, replacement) {
  if (node?.["path-label"] === path) return replacement;
  if (!isDbCollection(node)) return node;
  for (const entry of node.children || []) entry.node = replaceDbBranch(entry.node, path, replacement);
  return node;
}

async function loadMoreDbContext(button, path) {
  if (button.classList.contains("loading")) return;
  button.classList.add("loading");
  button.disabled = true;
  try {
    const result = await call("expandAppDbPath", JSON.stringify(path));
    if (!result?.ok) throw new Error(result?.error || "Unable to load more app-db context");
    graph["app-db-tree"] = replaceDbBranch(graph["app-db-tree"], path, result.node);
    render(graph, { preserveScroll: true });
  } catch (error) {
    button.classList.remove("loading");
    button.disabled = false;
    showError(error);
  }
}

async function toggleFullDbValue(button, node, path, showingFullValue) {
  if (button.classList.contains("loading")) return;
  if (showingFullValue) {
    expandedDbValues.delete(path);
    render(graph, { preserveScroll: true });
    return;
  }
  button.classList.add("loading");
  button.disabled = true;
  try {
    const result = await call("expandAppDbPath", JSON.stringify(path));
    if (!result?.ok || result.value == null) throw new Error(result?.error || "Unable to expand app-db value");
    expandedDbValues.set(path, result.value);
    render(graph, { preserveScroll: true });
  } catch (error) {
    button.classList.remove("loading");
    button.disabled = false;
    showError(error);
  }
}

function renderDbNode(node, key = null, depth = 0) {
  const path = node?.["path-label"] || "[]";
  const collection = isDbCollection(node);
  const collapsed = collection && collapsedDbPaths.has(path);
  const branch = element("div", `db-tree-node ${dbNodeState(node)}${collapsed ? " collapsed" : ""}`);
  const line = element("div", "db-tree-line");
  line.style.setProperty("--db-depth", depth);

  if (node.kind === "ellipsis") {
    line.append(element("span", "db-tree-toggle-spacer"));
    const more = element("button", "db-tree-more", node.text || "… more");
    more.type = "button";
    more.title = `Load more entries from ${path}`;
    more.addEventListener("click", () => loadMoreDbContext(more, path));
    line.append(more);
    branch.append(line);
    return branch;
  }

  if (collection) {
    const toggle = element("button", "db-tree-toggle", collapsed ? "+" : "−");
    toggle.type = "button";
    toggle.title = collapsed ? `Expand ${path}` : `Collapse ${path}`;
    toggle.setAttribute("aria-label", toggle.title);
    toggle.addEventListener("click", () => {
      if (collapsed) collapsedDbPaths.delete(path); else collapsedDbPaths.add(path);
      render(graph, { preserveScroll: true });
    });
    line.append(toggle);
  } else {
    line.append(element("span", "db-tree-toggle-spacer"));
  }

  if (key != null) {
    const keyCode = ednCode("db-tree-code db-tree-key", key);
    keyCode.dataset.path = path;
    keyCode.title = `app-db ${path}`;
    line.append(keyCode);
  }

  if (node.kind === "summary") {
    const summary = element("button", "db-tree-summary", node.text || "…");
    summary.type = "button";
    summary.title = `Expand app-db collection ${path}`;
    summary.addEventListener("click", () => loadMoreDbContext(summary, path));
    line.append(summary);
  } else {
    const fullDbValue = expandedDbValues.get(path);
    const showingFullDbValue = fullDbValue != null;
    line.append(renderDbValue(collection ? node.open : (showingFullDbValue ? fullDbValue : (node.text || "nil")), path,
                              collection ? "collection-value" : "leaf-value"));
    if (node["preview-truncated?"]) {
      const more = element("button", "db-value-more", showingFullDbValue ? "Show less" : "… Show all");
      more.type = "button";
      more.title = showingFullDbValue ? `Collapse app-db value ${path}` : `Show complete app-db value ${path}`;
      more.addEventListener("click", () => toggleFullDbValue(more, node, path, showingFullDbValue));
      line.append(more);
    }
  }
  if (collection && collapsed) {
    const collapsedMark = element("button", "db-tree-collapsed-mark", "…");
    collapsedMark.type = "button";
    collapsedMark.title = `Expand ${path}`;
    collapsedMark.addEventListener("click", () => {
      collapsedDbPaths.delete(path);
      render(graph, { preserveScroll: true });
    });
    line.append(collapsedMark);
  }
  const badges = renderComponentBadges(componentsForDbNode(node));
  if (badges) line.append(badges);
  branch.append(line);

  if (collection && !collapsed) {
    const children = element("div", "db-tree-children");
    for (const entry of node.children || []) children.append(renderDbNode(entry.node, entry.key, depth + 1));
    branch.append(children);
    const closeLine = element("div", "db-tree-line db-tree-close");
    closeLine.style.setProperty("--db-depth", depth);
    closeLine.append(element("span", "db-tree-toggle-spacer"));
    closeLine.append(renderDbValue(node.close, path, "collection-value"));
    branch.append(closeLine);
  }
  return branch;
}

function renderAppDbSection(section, appDbTree) {
  const container = element("section", "graph-section app-db-path db-tree-section");
  const header = element("header", "section-header");
  const heading = element("h2", "section-title", section.title);
  heading.append(element("span", "section-count", String(section.nodes.length)));
  header.append(heading);
  container.append(header);
  if (section.nodes.length && appDbTree) {
    const tree = element("div", "db-tree");
    tree.append(renderDbNode(appDbTree));
    container.append(tree);
  }
  return container;
}

function renderSection(section, appDbTree) {
  if (section.kind === "app-db-path") return renderAppDbSection(section, appDbTree);
  const container = element("section", `graph-section ${section.kind}${section.value ? " has-values" : " single-column"}`);
  const header = element("header", "section-header");
  const heading = element("h2", "section-title", section.title);
  heading.append(element("span", "section-count", String(section.nodes.length)));
  header.append(heading);
  container.append(header);

  if (section.nodes.length) {
    const columns = element("div", "column-headings");
    columns.append(element("span", "identity-heading", section.identity));
    if (section.value) columns.append(element("span", "value-heading", section.value));
    container.append(columns);
    if (section.levels) {
      for (const { level, nodes } of section.levels) {
        const collapsed = collapsedSubscriptionLevels.has(level);
        const group = element("div", `subscription-level${collapsed ? " collapsed" : ""}`);
        const levelHeader = element("button", "level-header");
        levelHeader.type = "button";
        levelHeader.title = collapsed ? `Expand subscription level ${level}` : `Collapse subscription level ${level}`;
        levelHeader.setAttribute("aria-expanded", String(!collapsed));
        levelHeader.append(element("span", "level-chevron", collapsed ? "▸" : "▾"));
        levelHeader.append(element("span", "level-title", `LEVEL ${level}`));
        levelHeader.append(element("span", "level-count", String(nodes.length)));
        levelHeader.addEventListener("click", () => {
          if (collapsed) collapsedSubscriptionLevels.delete(level);
          else collapsedSubscriptionLevels.add(level);
          render(graph, { preserveScroll: true });
        });
        group.append(levelHeader);
        if (!collapsed) {
          const rows = element("div", "section-rows");
          for (const node of nodes) rows.append(renderNode(node, section));
          group.append(rows);
        }
        container.append(group);
      }
    } else {
      const rows = element("div", "section-rows");
      for (const node of section.nodes) rows.append(renderNode(node, section));
      container.append(rows);
    }
  }
  return container;
}

function syncSubscriptionLevelState(sections, nodes) {
  const selectionId = nodes.find(node => node.kind === "element")?.id || null;
  if (selectionId !== subscriptionSelectionId) {
    subscriptionSelectionId = selectionId;
    subscriptionLevelsInitialized = false;
  }
  const levels = new Set(
    (sections.find(section => section.kind === "subscription")?.levels || []).map(group => group.level)
  );
  if (!subscriptionLevelsInitialized) {
    collapsedSubscriptionLevels = new Set([...levels].filter(level => level !== 0));
    subscriptionLevelsInitialized = true;
  } else {
    collapsedSubscriptionLevels = new Set(
      [...collapsedSubscriptionLevels].filter(level => levels.has(level))
    );
    for (const level of levels) {
      if (level !== 0 && !knownSubscriptionLevels.has(level)) collapsedSubscriptionLevels.add(level);
    }
  }
  knownSubscriptionLevels = levels;
}

function render(nextGraph, { preserveScroll = false } = {}) {
  const canvas = document.querySelector("#canvas-wrap");
  const scrollTop = canvas.scrollTop;
  applyCachedArgumentNames(nextGraph);
  graph = nextGraph;
  const nodes = nextGraph?.nodes || [];
  const edges = nextGraph?.edges || [];
  const sections = buildSections(nodes, edges);
  syncSubscriptionLevelState(sections, nodes);
  componentAssociations = buildComponentAssociations(nodes, edges);
  appDbAssociationPatterns = nodes
    .filter(node => node.kind === "app-db-path" && componentAssociations.has(node.id))
    .map(node => ({ path: node["association-path"], components: componentAssociations.get(node.id) }));
  const selected = nextGraph?.selection?.label;
  updateNavigation(nextGraph?.navigation);
  const hasSelection = Boolean(selected || nodes.some(node => node.kind === "element" && node.label !== "No element"));
  emptyEl.hidden = hasSelection;
  collapsedNodes = new Set([...collapsedNodes].filter(id => nodes.some(node => node.id === id)));
  const currentNodes = new Map(nodes.map(node => [node.id, node]));
  expandedValues = new Map([...expandedValues].filter(([id, cached]) => currentNodes.get(id)?.token === cached.token));

  graphEl.replaceChildren();
  for (const section of sections) {
    graphEl.append(renderSection(section, nextGraph?.["app-db-tree"]));
  }

  const warnings = nextGraph?.warnings || [];
  statusEl.textContent = warnings.length ? warnings.map(warning => warning.message).join(" · ") : `Connected · ${nodes.length} nodes`;
  if (preserveScroll) canvas.scrollTop = scrollTop;
  else if (selected !== lastSelection) canvas.scrollTo({ top: 0, left: 0 });
  lastSelection = selected;
  void resolveVisibleArgumentNames();
}

function renderSnapshot(nextGraph, options) {
  if (!nextGraph) return;
  const nextRevision = Number(nextGraph.revision);
  if (Number.isFinite(nextRevision) && nextRevision < revision) return;
  expandedDbValues.clear();
  render(nextGraph, options);
  if (Number.isFinite(nextRevision)) revision = nextRevision;
}

function showError(error) { statusEl.textContent = error.message; }

function updateNavigation(navigation = {}) {
  for (const button of navigationButtons) {
    button.disabled = navigationRunning || !navigation[button.dataset.direction];
  }
}

async function navigateSelected(direction) {
  if (navigationRunning) return;
  navigationRunning = true;
  updateNavigation(graph?.navigation);
  try {
    const result = await call("navigateElement", JSON.stringify(direction));
    if (result?.ok === false) {
      updateNavigation(result.navigation);
      throw new Error(result.error || "Unable to navigate the element tree");
    }
    renderSnapshot(result);
  } catch (error) {
    showError(error);
  } finally {
    navigationRunning = false;
    updateNavigation(graph?.navigation);
  }
}

async function connect() {
  try {
    const capabilities = await call("capabilities");
    statusEl.textContent = compatibilityMessage(capabilities);
    if (!capabilities || capabilities.protocol !== PROTOCOL_VERSION) return;
    void loadSourceResources();
    const boxesStatus = await call("setComponentHighlights", String(componentBoxesPreference()));
    updateComponentBoxesButton(Boolean(boxesStatus["component-highlights"]));
    await selectElementsNode();
  } catch (error) { showError(error); }
}

async function selectElementsNode() {
  selectionRefreshQueued = true;
  if (selectionRefreshRunning) return;
  selectionRefreshRunning = true;
  try {
    // Chrome may emit several selection events together. Keep at most one
    // follow-up refresh so they cannot form a backlog of full graph builds.
    while (selectionRefreshQueued) {
      selectionRefreshQueued = false;
      renderSnapshot(await call("selectElement", "$0"));
    }
  } catch (error) {
    showError(error);
  } finally {
    selectionRefreshRunning = false;
  }
}

async function refreshInspector() {
  if (refreshRunning) return;
  refreshRunning = true;
  refreshButton.disabled = true;
  refreshButton.classList.add("loading");
  refreshButton.setAttribute("aria-busy", "true");
  statusEl.textContent = "Refreshing provenance and source maps…";
  try {
    await loadSourceResources();
    renderSnapshot(await call("snapshot"), { preserveScroll: true });
  } catch (error) {
    showError(error);
  } finally {
    refreshRunning = false;
    refreshButton.disabled = false;
    refreshButton.classList.remove("loading");
    refreshButton.removeAttribute("aria-busy");
  }
}

chrome.devtools.panels.elements.onSelectionChanged.addListener(selectElementsNode);
chrome.devtools.inspectedWindow.onResourceAdded.addListener(resource => {
  if (!sourceResource(resource)) return;
  void addSourceResource(resource).then(() => {
    argumentNameCache.clear();
    pendingArgumentLookups.clear();
    sourceResourcesReady = true;
    return resolveVisibleArgumentNames();
  }).catch(() => undefined);
});
chrome.devtools.network.onRequestFinished.addListener(request => {
  if (!sourceResource(request)) return;
  void addSourceResource(request, true).then(() => {
    argumentNameCache.clear();
    pendingArgumentLookups.clear();
    sourceResourcesReady = true;
    return resolveVisibleArgumentNames();
  }).catch(() => undefined);
});
chrome.devtools.network.onNavigated.addListener(() => {
  argumentNameCache.clear();
  pendingArgumentLookups.clear();
});
for (const button of navigationButtons) {
  button.addEventListener("click", () => navigateSelected(button.dataset.direction));
}
updateNavigation();
updateComponentBoxesButton(componentBoxesPreference());
refreshButton.addEventListener("click", refreshInspector);
componentBoxesButton.addEventListener("click", async () => {
  const enabled = componentBoxesButton.getAttribute("aria-pressed") !== "true";
  localStorage.setItem(COMPONENT_BOXES_SETTING, String(enabled));
  updateComponentBoxesButton(enabled);
  try {
    const status = await call("setComponentHighlights", String(enabled));
    updateComponentBoxesButton(Boolean(status["component-highlights"]));
  } catch (error) { showError(error); }
});
pickButton.addEventListener("click", async () => {
  try {
    const status = await call(pickButton.dataset.active === "true" ? "stopPicker" : "startPicker");
    pickButton.dataset.active = String(status["picker-active"]);
    pickButton.textContent = status["picker-active"] ? "× Cancel" : "⌖ Pick";
    updateComponentBoxesButton(Boolean(status["component-highlights"]));
    updateNavigation(status.navigation);
  } catch (error) { showError(error); }
});

async function poll() {
  try {
    const status = await call("status");
    pickButton.dataset.active = String(status["picker-active"]);
    pickButton.textContent = status["picker-active"] ? "× Cancel" : "⌖ Pick";
    updateComponentBoxesButton(Boolean(status["component-highlights"]));
    updateNavigation(status.navigation);
    if (!selectionRefreshRunning && status.revision !== revision) {
      renderSnapshot(await call("snapshot"));
    }
  } catch (_) { /* DevTools navigation temporarily invalidates the context. */ }
  finally {
    pollTimer = setTimeout(poll, POLL_INTERVAL);
  }
}

connect().finally(() => {
  clearTimeout(pollTimer);
  pollTimer = setTimeout(poll, POLL_INTERVAL);
});
