import { copyFile, mkdir } from "node:fs/promises";

for (const version of ["react17", "react18"]) {
  await mkdir(`dist/fixtures/${version}`, { recursive: true });
  await copyFile(`fixtures/${version}/index.html`, `dist/fixtures/${version}/index.html`);
}

