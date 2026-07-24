const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

const root = path.resolve(__dirname, "..");
const extension = fs.readFileSync(
  path.join(root, "src", "extension.ts"),
  "utf8",
);
const git = fs.readFileSync(
  path.join(root, "src", "git.ts"),
  "utf8",
);

test("restore checks selected tree equality before creating history", () => {
  assert.match(extension, /workspaceMatchesCheckpoint\(context, selected\.oid\)/);
  assert.match(extension, /befindet sich bereits auf dem Stand/);
  assert.match(git, /export async function workspaceMatchesCheckpoint/);
});

test("restore labels use the canonical original checkpoint", () => {
  assert.match(extension, /function canonicalCheckpoint/);
  assert.match(extension, /name: `Zurück zu „\$\{canonical\.name\}“`/);
  assert.match(extension, /restoredFrom: canonical\.oid/);
  assert.doesNotMatch(extension, /name: `Zurück zu „\$\{selected\.name\}“`/);
});

test("repository initialization returns context so create continues", () => {
  assert.match(extension, /await initializeRepository\(folder\.uri\.fsPath\)/);
  assert.match(extension, /const context = await discoverRepository\(folder\.uri\.fsPath\)/);
  assert.match(extension, /return context;/);
  assert.match(extension, /async function createCommand[\s\S]*await saveNamedCheckpoint\(context\)/);
});
