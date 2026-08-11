import test from "node:test";
import assert from "node:assert/strict";
import {
  APP_DB_ASSOCIATION_WILDCARD,
  appDbPathMatches,
  buildComponentAssociations,
  buildSections,
  leafMostComponents,
  SECTION_ORDER
} from "../src-js/graph-layout.mjs";

const nodes = [
  { id: "component-child", kind: "component", label: "Child" },
  { id: "path-b", kind: "app-db-path", label: "[:profile :name]", specificity: 2, preview: "Ada" },
  { id: "element", kind: "element", label: "article#selected" },
  { id: "subscription-child", kind: "subscription", label: "[:name]" },
  { id: "path-a", kind: "app-db-path", label: "[:profile]", specificity: 1, preview: "{:name Ada}" },
  { id: "subscription-parent", kind: "subscription", label: "[:profile]" },
  { id: "component-parent", kind: "component", label: "Parent" }
];

const edges = [
  { from: "subscription-parent", to: "subscription-child", kind: "data-input" },
  { from: "component-parent", to: "component-child", kind: "render-ownership" }
];

test("sections stay ordered from app-db to selected element", () => {
  const sections = buildSections(nodes, edges);
  assert.deepEqual(sections.map(section => section.kind), SECTION_ORDER);
  assert.equal(sections[0].title, "APP-DB");
  assert.equal(sections[0].value, "VALUE");
  assert.equal(sections.find(section => section.kind === "component").value, undefined);
});

test("hierarchies flow from result-nearest nodes back to their inputs", () => {
  const sections = buildSections(nodes, edges);
  const subscriptionSection = sections.find(section => section.kind === "subscription");
  const subscriptions = subscriptionSection.nodes;
  const components = sections.find(section => section.kind === "component").nodes;
  assert.deepEqual(subscriptions.map(node => [node.id, node.depth]), [
    ["subscription-child", 0], ["subscription-parent", 1]
  ]);
  assert.deepEqual(components.map(node => [node.id, node.depth]), [
    ["component-child", 0], ["component-parent", 1]
  ]);
  assert.deepEqual(subscriptionSection.levels.map(group => [group.level, group.nodes.map(node => node.id)]), [
    [0, ["subscription-child"]], [1, ["subscription-parent"]]
  ]);
});

test("shared subscription inputs use their shortest result distance", () => {
  const sharedNodes = [
    { id: "result-a", kind: "subscription", label: "[:result-a]" },
    { id: "result-b", kind: "subscription", label: "[:result-b]" },
    { id: "middle", kind: "subscription", label: "[:middle]" },
    { id: "shared", kind: "subscription", label: "[:shared]" }
  ];
  const sharedEdges = [
    { from: "shared", to: "result-a" },
    { from: "shared", to: "middle" },
    { from: "middle", to: "result-b" }
  ];
  const section = buildSections(sharedNodes, sharedEdges).find(item => item.kind === "subscription");
  assert.equal(section.nodes.find(node => node.id === "shared").depth, 1);
});

test("app-db paths show the most specific paths first", () => {
  const paths = buildSections(nodes, edges).find(section => section.kind === "app-db-path").nodes;
  assert.deepEqual(paths.map(node => node.id), ["path-b", "path-a"]);
});

test("empty graph still exposes all table sections", () => {
  const sections = buildSections([]);
  assert.equal(sections.length, 5);
  assert.ok(sections.every(section => section.nodes.length === 0));
});

test("subscriptions and paths inherit their downstream component associations", () => {
  const associationNodes = [
    { id: "path", kind: "app-db-path", label: "[:person :name]" },
    { id: "shared", kind: "subscription", label: "[:person]" },
    { id: "name", kind: "subscription", label: "[:person-name]" },
    { id: "card", kind: "component", label: "views/PersonCard" },
    { id: "header", kind: "component", label: "views/Header" }
  ];
  const associationEdges = [
    { from: "path", to: "shared", kind: "data-input" },
    { from: "shared", to: "name", kind: "data-input" },
    { from: "name", to: "card", kind: "render-input" },
    { from: "shared", to: "header", kind: "render-input" },
    { from: "header", to: "card", kind: "render-ownership" }
  ];
  const associations = buildComponentAssociations(associationNodes, associationEdges);
  assert.deepEqual(associations.get("path").map(node => node.id), ["card", "header"]);
  assert.deepEqual(associations.get("shared").map(node => node.id), ["card", "header"]);
  assert.deepEqual(associations.get("name").map(node => node.id), ["card"]);
  assert.equal(associations.has("card"), false);
});

test("badges keep only associated leaf components across ownership gaps", () => {
  const components = [
    { id: "root", kind: "component", label: "Root" },
    { id: "middle", kind: "component", label: "Middle" },
    { id: "leaf", kind: "component", label: "Leaf" },
    { id: "other", kind: "component", label: "OtherLeaf" }
  ];
  const ownership = [
    { from: "root", to: "middle", kind: "render-ownership" },
    { from: "middle", to: "leaf", kind: "render-ownership" }
  ];
  assert.deepEqual(
    leafMostComponents([components[0], components[2], components[3]], ownership).map(node => node.id),
    ["leaf", "other"]
  );
});

test("prop inputs carry app-db associations to their leaf component", () => {
  const propNodes = [
    { id: "path", kind: "app-db-path", label: "[:person]" },
    { id: "sub", kind: "subscription", label: "[:person]" },
    { id: "prop", kind: "prop", label: "arg 1" },
    { id: "card", kind: "component", label: "views/PersonCard" }
  ];
  const propEdges = [
    { from: "path", to: "sub", kind: "data-input" },
    { from: "sub", to: "prop", kind: "data-input" },
    { from: "prop", to: "card", kind: "render-input" }
  ];
  const associations = buildComponentAssociations(propNodes, propEdges);
  assert.deepEqual(associations.get("path").map(node => node.id), ["card"]);
  assert.deepEqual(associations.get("prop").map(node => node.id), ["card"]);
});

test("wildcard provenance associates with concrete projected app-db paths", () => {
  assert.equal(appDbPathMatches(
    [":people", "42", ":name"],
    [":people", APP_DB_ASSOCIATION_WILDCARD, ":name"]
  ), true);
  assert.equal(appDbPathMatches([":people", "42"], [":people", "7"]), false);
  assert.equal(appDbPathMatches([":people", ":one/name"], [":people", ":two/name"]), false);
  assert.equal(appDbPathMatches(
    [":versions", "\"2026\"", ":inclusion-terms", "0"],
    [":versions", "\"2026\"", ":inclusion-terms", APP_DB_ASSOCIATION_WILDCARD]
  ), true);
});
