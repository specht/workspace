const test = require("node:test");
const assert = require("node:assert/strict");
const fs = require("node:fs");
const path = require("node:path");

const root = path.resolve(__dirname, "..");
const read = relative => fs.readFileSync(path.join(root, relative), "utf8");

test("extension is branded as Hackschule Checkpoints", () => {
  const pkg = JSON.parse(read("package.json"));
  const german = JSON.parse(read("package.nls.de.json"));

  assert.equal(pkg.name, "hackschule-checkpoints");
  assert.equal(german["extension.displayName"], "Hackschule Checkpoints");
  assert.equal(german["view.name"], "Checkpoints");
});

test("checkpoint rows require explicit compare or restore actions", () => {
  const source = read("src/checkpointTree.ts");
  assert.match(source, /contextValue = "hackschuleCheckpoint"/);
  assert.doesNotMatch(source, /item\.command\s*=/);

  const pkg = JSON.parse(read("package.json"));
  const inline = pkg.contributes.menus["view/item/context"]
    .filter(item => item.group.startsWith("inline"))
    .map(item => item.command);
  assert.deepEqual(inline, [
    "hackschuleCheckpoints.compareItem",
    "hackschuleCheckpoints.restoreItem"
  ]);
});

test("modal calls do not add their own cancel button", () => {
  const source = read("src/extension.ts");
  assert.doesNotMatch(source, /\{ modal: true \},\s*[^\n]+,\s*"Abbrechen"/);
});

test("automatic safety names contain no generated timestamp", () => {
  const source = read("src/extension.ts");
  assert.match(source, /Vor dem Wiederherstellen von/);
  assert.doesNotMatch(source, /automaticSafetyName[\s\S]*toLocaleString/);
});

test("checkpoint creation checks for an unchanged project before asking for a name", () => {
  const source = fs.readFileSync(
    path.join(root, "src", "extension.ts"),
    "utf8",
  );

  const dirtyCheck = source.indexOf("isDirtySinceLatestCheckpoint(context)");
  const namePrompt = source.indexOf("askCheckpointName()", dirtyCheck);
  assert.ok(dirtyCheck >= 0);
  assert.ok(namePrompt > dirtyCheck);
  assert.match(source, /Es wurde kein neuer Checkpoint erstellt/);
});

test("friendly comparison hides Git header noise", () => {
  const source = fs.readFileSync(
    path.join(root, "src", "diffProvider.ts"),
    "utf8",
  );

  assert.match(source, /Vergleich mit/);
  assert.match(source, /Grün \(\+\) ist neu/);
  assert.match(source, /Datei:/);
  assert.match(source, /formatFriendlyDiff/);
});
