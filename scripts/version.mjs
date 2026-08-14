import { readFile } from "node:fs/promises";

const SEMVER = /^\d+\.\d+\.\d+$/;

async function readJson(path) {
  return JSON.parse(await readFile(path, "utf8"));
}

function matchVersion(contents, pattern, source) {
  const match = contents.match(pattern);
  if (!match) {
    throw new Error(`Could not find the version in ${source}`);
  }
  return match[1];
}

export async function readVersions() {
  const [packageJson, packageLock, project, bumpConfig] = await Promise.all([
    readJson("package.json"),
    readJson("package-lock.json"),
    readFile("project.clj", "utf8"),
    readFile(".bumpversion.toml", "utf8")
  ]);

  return {
    "package.json": packageJson.version,
    "package-lock.json": packageLock.version,
    "package-lock.json root package": packageLock.packages?.[""]?.version,
    "project.clj": matchVersion(
      project,
      /\(defproject\s+net\.clojars\.frknzd\/re-frame\.vertica\s+"([^"]+)"/,
      "project.clj"
    ),
    ".bumpversion.toml": matchVersion(
      bumpConfig,
      /^current_version\s*=\s*"([^"]+)"/m,
      ".bumpversion.toml"
    )
  };
}

export async function assertVersions(expectedVersion) {
  if (!SEMVER.test(expectedVersion)) {
    throw new Error(`Expected a stable x.y.z version, received: ${expectedVersion}`);
  }

  const versions = await readVersions();
  const mismatches = Object.entries(versions).filter(([, version]) => version !== expectedVersion);
  if (mismatches.length > 0) {
    const details = mismatches
      .map(([source, version]) => `  ${source}: ${version ?? "missing"}`)
      .join("\n");
    throw new Error(`Version mismatch; expected ${expectedVersion}:\n${details}`);
  }

  return versions;
}
