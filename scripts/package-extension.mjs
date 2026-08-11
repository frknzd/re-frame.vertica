import { mkdir, readFile, rm } from "node:fs/promises";
import { spawnSync } from "node:child_process";
import { resolve } from "node:path";
import { assertVersions } from "./version.mjs";

const packageJson = JSON.parse(await readFile("package.json", "utf8"));
const version = process.argv[2] ?? packageJson.version;
await assertVersions(version);

const builtManifest = JSON.parse(await readFile("dist/extension/manifest.json", "utf8"));
if (builtManifest.version !== version) {
  throw new Error(
    `Built extension version ${builtManifest.version} does not match release version ${version}. Run npm run build first.`
  );
}

await mkdir("release", { recursive: true });
const archive = resolve(`release/re-frame.vertica-devtools-v${version}.zip`);
await rm(archive, { force: true });

const result = spawnSync("zip", ["-q", "-r", archive, "."], {
  cwd: "dist/extension",
  encoding: "utf8"
});
if (result.status !== 0) {
  throw new Error(`zip failed: ${result.stderr || result.stdout || `exit ${result.status}`}`);
}

console.log(archive);
