import { readFile } from "node:fs/promises";
import { assertVersions } from "./version.mjs";

const packageJson = JSON.parse(await readFile("package.json", "utf8"));
const expectedVersion = process.argv[2] ?? packageJson.version;
const versions = await assertVersions(expectedVersion);

console.log(`All ${Object.keys(versions).length} version declarations match ${expectedVersion}.`);
