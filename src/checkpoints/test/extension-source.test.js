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
  assert.match(source, /item\.description = localTimeLabel/);
  assert.doesNotMatch(source, /item\.description = new Date\([^\n]+toLocaleString/);
});
