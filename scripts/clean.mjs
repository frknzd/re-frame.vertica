import { rm } from "node:fs/promises";

await rm("dist", { recursive: true, force: true });
await rm("extension/panel.js", { force: true });
await rm("extension/edn-worker.js", { force: true });
await rm("extension/source-map-worker.js", { force: true });
