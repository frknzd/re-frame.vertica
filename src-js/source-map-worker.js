import { addSourceResource, resolveArgumentNames, resolveSourceLocation } from "./source-args.mjs";

let sourceIndex = new Map();

self.addEventListener("message", event => {
  const { type, id, url, content, requests = [] } = event.data || {};
  if (type === "reset") {
    sourceIndex = new Map();
    return;
  }
  if (type === "add") {
    try {
      addSourceResource(sourceIndex, url, content);
      self.postMessage({ type: "added", id });
    } catch (error) {
      self.postMessage({ type: "added", id, error: error.message });
    }
    return;
  }
  if (type === "resolve") {
    const results = requests.map(({ componentName, arity }) => ({
      componentName,
      arity,
      names: resolveArgumentNames(sourceIndex, componentName, arity)
    }));
    self.postMessage({ type: "resolved", id, results });
    return;
  }
  if (type === "resolve-location") {
    const componentNames = requests.map(request => request.componentName).filter(Boolean);
    const location = componentNames
      .map(componentName => resolveSourceLocation(sourceIndex, componentName))
      .find(Boolean) || null;
    self.postMessage({ type: "resolved-location", id, location });
  }
});
