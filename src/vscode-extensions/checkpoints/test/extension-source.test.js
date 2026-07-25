const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

const root = path.resolve(__dirname, "..");

test("checkpoint tree groups entries by local day and shows only times on rows", () => {
  const source = fs.readFileSync(
    path.join(root, "src", "checkpointTree.ts"),
    "utf8",
  );

  assert.match(source, /weekday:\s*"long"/);
  assert.match(source, /CheckpointDayGroup/);
  assert.match(source, /CollapsibleState\.Expanded/);
  assert.match(source, /item\.description = `\$\{localTimeLabel/);
  assert.match(source, /statsLabel\(node\.byteStats\)/);
  assert.doesNotMatch(source, /item\.description = new Date\([^\n]+toLocaleString/);
});

test("checkpoint rows show added and removed bytes", () => {
  const treeSource = fs.readFileSync(
    path.join(root, "src", "checkpointTree.ts"),
    "utf8",
  );
  const gitSource = fs.readFileSync(
    path.join(root, "src", "git.ts"),
    "utf8",
  );

  assert.match(treeSource, /addedBytes/);
  assert.match(treeSource, /removedBytes/);
  assert.match(treeSource, /formatBytes/);
  assert.match(gitSource, /byteStatsBetweenTrees/);
  assert.match(gitSource, /cat-file.*--batch-check/s);
});

test("all checkpoints can be deleted only through hard confirmation", () => {
  const extensionSource = fs.readFileSync(
    path.join(root, "src", "extension.ts"),
    "utf8",
  );
  const packageJson = JSON.parse(
    fs.readFileSync(path.join(root, "package.json"), "utf8"),
  );

  assert.match(extensionSource, /deleteAllCheckpoints/);
  assert.match(extensionSource, /confirmation !== "LÖSCHEN"/);
  assert.ok(packageJson.contributes.commands.some(
    command => command.command === "hackschuleCheckpoints.deleteAll",
  ));
});

test("large checkpoint preflight is configurable", () => {
  const extensionSource = fs.readFileSync(
    path.join(root, "src", "extension.ts"),
    "utf8",
  );
  const packageJson = JSON.parse(
    fs.readFileSync(path.join(root, "package.json"), "utf8"),
  );

  assert.match(extensionSource, /confirmLargeCheckpoint/);
  assert.ok(packageJson.contributes.configuration.properties[
    "hackschuleCheckpoints.warnAddedBytes"
  ]);
  assert.ok(packageJson.contributes.configuration.properties[
    "hackschuleCheckpoints.warnChangedFiles"
  ]);
  assert.ok(packageJson.contributes.configuration.properties[
    "hackschuleCheckpoints.warnSingleFileBytes"
  ]);
});
