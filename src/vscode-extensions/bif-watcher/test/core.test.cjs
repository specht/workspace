const assert = require("node:assert/strict");
const fs = require("node:fs/promises");
const os = require("node:os");
const path = require("node:path");
const { afterEach, test } = require("node:test");
const {
  BIF_MARKER_FILE,
  dependencyInstallRequired,
  NPM_INSTALL_STATE_FILE,
} = require("../dist/core.js");

const temporaryDirectories = [];

async function temporaryProject(packageJson = {}) {
  const root = await fs.mkdtemp(path.join(os.tmpdir(), "bif-runner-test-"));
  temporaryDirectories.push(root);
  await fs.writeFile(
    path.join(root, "package.json"),
    `${JSON.stringify(packageJson)}\n`,
    "utf8",
  );
  return root;
}

async function installDependency(root, packageName) {
  await fs.mkdir(path.join(root, "node_modules", ...packageName.split("/")), {
    recursive: true,
  });
}

async function createFileAt(filePath, time = Date.now()) {
  await fs.mkdir(path.dirname(filePath), { recursive: true });
  await fs.writeFile(filePath, "{}\n", "utf8");
  const date = new Date(time);
  await fs.utimes(filePath, date, date);
}

async function setTime(filePath, time) {
  const date = new Date(time);
  await fs.utimes(filePath, date, date);
}

afterEach(async () => {
  await Promise.all(
    temporaryDirectories.splice(0).map(root =>
      fs.rm(root, { recursive: true, force: true }),
    ),
  );
});

test("uses a repository-specific marker file", () => {
  assert.equal(BIF_MARKER_FILE, ".bif-project");
});

test("skips installation when the repository declares no dependencies", async () => {
  const root = await temporaryProject();
  assert.equal(await dependencyInstallRequired(root), false);
});

test("requires installation when a declared dependency is missing", async () => {
  const root = await temporaryProject({ dependencies: { chokidar: "^4.0.0" } });
  assert.equal(await dependencyInstallRequired(root), true);
});

test("requires npm's installation state for repositories with dependencies", async () => {
  const root = await temporaryProject({ dependencies: { chokidar: "^4.0.0" } });
  await installDependency(root, "chokidar");
  assert.equal(await dependencyInstallRequired(root), true);
});

test("skips installation when dependencies and metadata are current", async () => {
  const root = await temporaryProject({
    devDependencies: { "@example/package": "1.0.0" },
  });
  await installDependency(root, "@example/package");
  await createFileAt(path.join(root, "node_modules", NPM_INSTALL_STATE_FILE));
  assert.equal(await dependencyInstallRequired(root), false);
});

test("requires installation after package metadata changes", async () => {
  const root = await temporaryProject({ dependencies: { chokidar: "^4.0.0" } });
  await installDependency(root, "chokidar");
  const now = Date.now();
  await createFileAt(path.join(root, "node_modules", NPM_INSTALL_STATE_FILE), now - 2_000);
  await setTime(path.join(root, "package.json"), now);
  assert.equal(await dependencyInstallRequired(root), true);
});
