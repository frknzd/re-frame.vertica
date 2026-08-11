import * as transit from "transit-js";
import { bridgeExpression, compatibilityMessage, PROTOCOL_VERSION, transitToPlain } from "./protocol.mjs";

const reader = transit.reader("json");
const statusEl = document.querySelector("#status");
const emptyEl = document.querySelector("#empty");
const svg = document.querySelector("#graph");
const viewport = document.querySelector("#viewport");
const edgesGroup = document.querySelector("#edges");
const nodesGroup = document.querySelector("#nodes");
const details = document.querySelector("#details");
const pickButton = document.querySelector("#pick");
let graph = null;
let revision = -1;
let transform = { x: 30, y: 30, scale: 1 };
let dragging = null;
let pickerWasActive = false;

function inspectedEval(expression) {
  return new Promise((resolve, reject) => {
    chrome.devtools.inspectedWindow.eval(expression, (result, exception) => {
      if (exception) reject(new Error(exception.description || exception.value || "Evaluation failed"));
      else resolve(result);
    });
  });
}

function decode(encoded) {
  return encoded == null ? null : transitToPlain(reader.read(encoded), transit);
}

async function call(method, argumentExpression = "") {
  return decode(await inspectedEval(bridgeExpression(method, argumentExpression)));
}

function svgElement(name, attributes = {}) {
  const element = document.createElementNS("http://www.w3.org/2000/svg", name);
  for (const [key, value] of Object.entries(attributes)) element.setAttribute(key, value);
  return element;
}

function layout(nodes) {
  const layers = { "app-db-path": [], subscription: [], component: [], element: [] };
  for (const node of nodes) (layers[node.kind] ||= []).push(node);
  const positions = new Map();
  const widths = { "app-db-path": 190, subscription: 220, component: 210, element: 190 };
  Object.entries(layers).forEach(([kind, layer], layerIndex) => {
    layer.sort((a, b) => a.label.localeCompare(b.label) || a.id.localeCompare(b.id));
    layer.forEach((node, row) => positions.set(node.id, {
      x: 40 + layerIndex * 290, y: 40 + row * 86, width: widths[kind] || 200, height: 48
    }));
  });
  return positions;
}

function applyTransform() {
  viewport.setAttribute("transform", `translate(${transform.x} ${transform.y}) scale(${transform.scale})`);
}

function render(nextGraph) {
  graph = nextGraph;
  edgesGroup.replaceChildren();
  nodesGroup.replaceChildren();
  const nodes = nextGraph?.nodes || [];
  emptyEl.hidden = nodes.length > 1;
  const positions = layout(nodes);
  for (const edge of nextGraph?.edges || []) {
    const from = positions.get(edge.from), to = positions.get(edge.to);
    if (!from || !to) continue;
    const x1 = from.x + from.width, y1 = from.y + from.height / 2;
    const x2 = to.x, y2 = to.y + to.height / 2;
    const bend = Math.max(35, (x2 - x1) / 2);
    edgesGroup.append(svgElement("path", { class: "edge", d: `M${x1},${y1} C${x1 + bend},${y1} ${x2 - bend},${y2} ${x2},${y2}` }));
  }
  for (const node of nodes) {
    const p = positions.get(node.id);
    const group = svgElement("g", { class: `node ${node.kind}${node["complete?"] === false ? " partial" : ""}`, transform: `translate(${p.x} ${p.y})` });
    group.append(svgElement("rect", { width: p.width, height: p.height }));
    const label = svgElement("text", { x: 10, y: 20 });
    label.textContent = node.label.length > 30 ? `${node.label.slice(0, 29)}…` : node.label;
    group.append(label);
    const preview = svgElement("text", { x: 10, y: 37, opacity: ".65" });
    preview.textContent = (node.preview || "").slice(0, 34);
    group.append(preview);
    group.addEventListener("click", () => showDetails(node));
    nodesGroup.append(group);
  }
  const warnings = nextGraph?.warnings || [];
  statusEl.textContent = warnings.length ? warnings.map(w => w.message).join(" · ") : `Connected · ${nodes.length} nodes`;
  applyTransform();
}

function showDetails(node) {
  details.replaceChildren();
  details.hidden = false;
  const heading = document.createElement("h2"); heading.textContent = node.label; details.append(heading);
  const dl = document.createElement("dl");
  for (const [label, value] of [["Kind", node.kind], ["Value", node.preview || "—"], ["Provenance", node["complete?"] === false ? "Partial" : "Complete"]]) {
    const dt = document.createElement("dt"), dd = document.createElement("dd"); dt.textContent = label; dd.textContent = String(value); dl.append(dt, dd);
  }
  details.append(dl);
  if (node.reason) { const warning = document.createElement("p"); warning.className = "warning"; warning.textContent = node.reason; details.append(warning); }
  if (node.token) {
    const button = document.createElement("button"); button.textContent = "Log real value to page console";
    button.addEventListener("click", () => call("logNode", JSON.stringify(node.token)).catch(showError)); details.append(button);
  }
}

function fit() {
  if (!nodesGroup.childElementCount) return;
  const bounds = viewport.getBBox(), box = svg.getBoundingClientRect();
  const scale = Math.min(1.5, Math.max(.15, Math.min((box.width - 50) / bounds.width, (box.height - 50) / bounds.height)));
  transform = { x: (box.width - bounds.width * scale) / 2 - bounds.x * scale, y: (box.height - bounds.height * scale) / 2 - bounds.y * scale, scale };
  applyTransform();
}

function showError(error) { statusEl.textContent = error.message; }

async function connect() {
  try {
    const capabilities = await call("capabilities");
    statusEl.textContent = compatibilityMessage(capabilities);
    if (!capabilities || capabilities.protocol !== PROTOCOL_VERSION) return;
    await selectElementsNode();
  } catch (error) { showError(error); }
}

async function selectElementsNode() {
  try { render(await call("selectElement", "$0")); }
  catch (error) { showError(error); }
}

chrome.devtools.panels.elements.onSelectionChanged.addListener(selectElementsNode);
pickButton.addEventListener("click", async () => {
  try {
    const status = await call(pickButton.dataset.active === "true" ? "stopPicker" : "startPicker");
    pickerWasActive = status["picker-active"];
    pickButton.dataset.active = String(status["picker-active"]);
    pickButton.textContent = status["picker-active"] ? "× Cancel" : "⌖ Pick";
  } catch (error) { showError(error); }
});
document.querySelector("#fit").addEventListener("click", fit);
svg.addEventListener("wheel", event => {
  event.preventDefault();
  const rect = svg.getBoundingClientRect(), factor = Math.exp(-event.deltaY * .001);
  const next = Math.min(3, Math.max(.1, transform.scale * factor));
  const px = event.clientX - rect.left, py = event.clientY - rect.top;
  transform.x = px - (px - transform.x) * next / transform.scale;
  transform.y = py - (py - transform.y) * next / transform.scale;
  transform.scale = next; applyTransform();
}, { passive: false });
svg.addEventListener("pointerdown", event => { dragging = { x: event.clientX, y: event.clientY, tx: transform.x, ty: transform.y }; svg.classList.add("dragging"); svg.setPointerCapture(event.pointerId); });
svg.addEventListener("pointermove", event => { if (dragging) { transform.x = dragging.tx + event.clientX - dragging.x; transform.y = dragging.ty + event.clientY - dragging.y; applyTransform(); } });
svg.addEventListener("pointerup", () => { dragging = null; svg.classList.remove("dragging"); });

setInterval(async () => {
  try {
    const status = await call("status");
    if (pickerWasActive && !status["picker-active"] && status["picker-outcome"] === "locked") {
      await inspectedEval("(()=>{const b=globalThis.__RE_FRAME_INSPECTOR__;const e=b&&b.selectedElement();if(e)inspect(e);return true;})()");
    }
    pickerWasActive = status["picker-active"];
    pickButton.dataset.active = String(status["picker-active"]);
    pickButton.textContent = status["picker-active"] ? "× Cancel" : "⌖ Pick";
    if (status.revision !== revision) { revision = status.revision; render(await call("snapshot")); }
  } catch (_) { /* DevTools navigation temporarily invalidates the context. */ }
}, 150);

connect();
