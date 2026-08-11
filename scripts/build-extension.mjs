import { build } from "esbuild";
import { copyFile, mkdir } from "node:fs/promises";

await mkdir("dist/extension", { recursive: true });
await build({
  entryPoints: ["src-js/panel.js"],
  outfile: "extension/panel.js",
  bundle: true,
  format: "iife",
  platform: "browser",
  target: "chrome120",
  minify: true,
  legalComments: "none"
});
for (const file of ["manifest.json", "devtools.html", "panel.html", "panel.css"]) {
  await copyFile(`extension/${file}`, `dist/extension/${file}`);
}
await copyFile("src-js/devtools.js", "dist/extension/devtools.js");
await copyFile("extension/panel.js", "dist/extension/panel.js");

