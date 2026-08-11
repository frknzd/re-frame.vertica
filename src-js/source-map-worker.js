import { addSourceResource, resolveArgumentNames } from "./source-args.mjs";

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
  }
});
