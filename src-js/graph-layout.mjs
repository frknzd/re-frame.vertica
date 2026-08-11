export const SECTION_ORDER = ["app-db-path", "subscription", "prop", "component", "element"];

export const SECTION_META = {
  "app-db-path": { title: "APP-DB", identity: "PATH", value: "VALUE" },
  subscription: { title: "SUBSCRIPTIONS", identity: "QUERY", value: "VALUE" },
  prop: { title: "PROPS", identity: "ARGUMENT", value: "VALUE" },
  component: { title: "REAGENT COMPONENTS", identity: "COMPONENT" },
  element: { title: "SELECTED ELEMENT", identity: "ELEMENT" }
};

function compareNodes(a, b) {
  return String(a.label).localeCompare(String(b.label)) || String(a.id).localeCompare(String(b.id));
}

function comparePaths(a, b) {
  return (Number(b.specificity) || 0) - (Number(a.specificity) || 0) || compareNodes(a, b);
}

export const APP_DB_ASSOCIATION_WILDCARD = "__re-frame.vertica-wildcard__";

function pathSegment(value) {
  return String(value);
}

function wildcardSegment(value) {
  return value === APP_DB_ASSOCIATION_WILDCARD;
}

export function appDbPathMatches(actual, pattern) {
  return Array.isArray(actual) && Array.isArray(pattern) && actual.length === pattern.length &&
    actual.every((segment, index) => wildcardSegment(pattern[index]) || pathSegment(segment) === pathSegment(pattern[index]));
}

function hierarchicalNodes(nodes, edges, reverseFlow = false) {
  if (nodes.length < 2) return nodes.map(node => ({ ...node, depth: 0 }));
  const ids = new Set(nodes.map(node => node.id));
  const children = new Map(nodes.map(node => [node.id, []]));
  const incoming = new Map(nodes.map(node => [node.id, 0]));

  for (const edge of edges) {
    if (!ids.has(edge.from) || !ids.has(edge.to) || edge.from === edge.to) continue;
    const parent = reverseFlow ? edge.to : edge.from;
    const child = reverseFlow ? edge.from : edge.to;
    children.get(parent).push(child);
    incoming.set(child, incoming.get(child) + 1);
  }
  for (const childIds of children.values()) childIds.sort();

  const byId = new Map(nodes.map(node => [node.id, node]));
  const roots = nodes.filter(node => incoming.get(node.id) === 0).sort(compareNodes);
  const depths = new Map();
  const queue = roots.map(node => [node.id, 0]);
  let queueIndex = 0;

  function assignQueuedDepths() {
    while (queueIndex < queue.length) {
      const [id, depth] = queue[queueIndex++];
      const current = depths.get(id);
      if (current != null && current <= depth) continue;
      depths.set(id, depth);
      const next = children.get(id).map(childId => byId.get(childId)).filter(Boolean).sort(compareNodes);
      for (const child of next) queue.push([child.id, depth + 1]);
    }
  }
  assignQueuedDepths();
  for (const node of [...nodes].sort(compareNodes)) {
    if (depths.has(node.id)) continue;
    queue.push([node.id, 0]);
    assignQueuedDepths();
  }
  return nodes
    .map(node => ({ ...node, depth: depths.get(node.id) || 0 }))
    .sort((a, b) => a.depth - b.depth || compareNodes(a, b));
}

function groupLevels(nodes) {
  const levels = new Map();
  for (const node of nodes) {
    if (!levels.has(node.depth)) levels.set(node.depth, []);
    levels.get(node.depth).push(node);
  }
  return [...levels.entries()]
    .sort(([a], [b]) => a - b)
    .map(([level, levelNodes]) => ({ level, nodes: levelNodes }));
}

export function buildComponentAssociations(nodes, edges = []) {
  const byId = new Map(nodes.map(node => [node.id, node]));
  const upstream = new Map(nodes.map(node => [node.id, []]));
  const directInputs = new Map();
  const componentChildren = new Map(
    nodes.filter(node => node.kind === "component").map(node => [node.id, []])
  );

  for (const edge of edges) {
    if (!byId.has(edge.from) || !byId.has(edge.to)) continue;
    if (edge.kind === "data-input") upstream.get(edge.to).push(edge.from);
    if (edge.kind === "render-input" && byId.get(edge.to)?.kind === "component") {
      if (!directInputs.has(edge.to)) directInputs.set(edge.to, []);
      directInputs.get(edge.to).push(edge.from);
    }
    if (edge.kind === "render-ownership" &&
        byId.get(edge.from)?.kind === "component" &&
        byId.get(edge.to)?.kind === "component") {
      componentChildren.get(edge.from).push(edge.to);
    }
  }

  const leafRanks = new Map();
  function leafRank(id, visiting = new Set()) {
    if (leafRanks.has(id)) return leafRanks.get(id);
    if (visiting.has(id)) return 0;
    const nextVisiting = new Set(visiting).add(id);
    const children = componentChildren.get(id) || [];
    const rank = children.length
      ? 1 + Math.max(...children.map(childId => leafRank(childId, nextVisiting)))
      : 0;
    leafRanks.set(id, rank);
    return rank;
  }
  for (const id of componentChildren.keys()) leafRank(id);

  const componentIdsByNode = new Map();
  for (const component of nodes.filter(node => node.kind === "component")) {
    const queue = [...(directInputs.get(component.id) || [])];
    const visited = new Set();
    for (let index = 0; index < queue.length; index += 1) {
      const id = queue[index];
      if (visited.has(id)) continue;
      visited.add(id);
      if (!componentIdsByNode.has(id)) componentIdsByNode.set(id, new Set());
      componentIdsByNode.get(id).add(component.id);
      for (const inputId of upstream.get(id) || []) queue.push(inputId);
    }
  }

  return new Map([...componentIdsByNode].map(([id, componentIds]) => [
    id,
    [...componentIds]
      .map(componentId => byId.get(componentId))
      .filter(Boolean)
      .sort((a, b) => leafRank(a.id) - leafRank(b.id) || compareNodes(a, b))
  ]));
}

export function leafMostComponents(components = [], edges = []) {
  const byId = new Map(components.map(component => [component.id, component]));
  const children = new Map();
  for (const edge of edges) {
    if (edge.kind !== "render-ownership") continue;
    if (!children.has(edge.from)) children.set(edge.from, []);
    children.get(edge.from).push(edge.to);
  }

  function hasAssociatedDescendant(id) {
    const queue = [...(children.get(id) || [])];
    const visited = new Set();
    for (let index = 0; index < queue.length; index += 1) {
      const childId = queue[index];
      if (visited.has(childId)) continue;
      visited.add(childId);
      if (byId.has(childId)) return true;
      queue.push(...(children.get(childId) || []));
    }
    return false;
  }

  return [...byId.values()]
    .filter(component => !hasAssociatedDescendant(component.id))
    .sort(compareNodes);
}

export function buildSections(nodes, edges = []) {
  const layers = Object.fromEntries(SECTION_ORDER.map(kind => [kind, []]));
  const kindById = new Map();
  for (const node of nodes) (layers[node.kind] ||= []).push(node);
  for (const node of nodes) kindById.set(node.id, node.kind);

  return SECTION_ORDER.map(kind => {
    const layer = layers[kind].sort(kind === "app-db-path" ? comparePaths : compareNodes);
    const sameKindEdges = edges.filter(edge => {
      return kindById.get(edge.from) === kind && kindById.get(edge.to) === kind;
    });
    const orderedNodes = kind === "app-db-path"
      ? layer.map(node => ({ ...node, depth: 0 }))
      : hierarchicalNodes(layer, sameKindEdges, kind === "subscription" || kind === "component");
    return {
      kind,
      ...SECTION_META[kind],
      nodes: orderedNodes,
      levels: kind === "subscription" ? groupLevels(orderedNodes) : null
    };
  });
}
