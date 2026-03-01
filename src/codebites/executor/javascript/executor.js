#!/usr/bin/env node
"use strict";

const fs = require("fs");
const vm = require("vm");
const util = require("util");

// ---- Protocol output: keep stable handles to the real stdout/stderr ----
const REAL_STDOUT_WRITE = process.stdout.write.bind(process.stdout);
const REAL_STDERR_WRITE = process.stderr.write.bind(process.stderr);

function emit(obj) {
  REAL_STDOUT_WRITE(JSON.stringify(obj) + "\n");
}

function safeJsonValue(x) {
  try {
    JSON.stringify(x);
    return x;
  } catch {
    return { __repr__: util.inspect(x, { depth: 2 }) };
  }
}

function extractLocationFromStack(stack) {
  if (!stack) return null;
  const lines = String(stack).split("\n");
  // Prefer submission.js lines.
  for (let i = lines.length - 1; i >= 0; i--) {
    const l = lines[i];
    // e.g. at array_sum (submission.js:12:3)
    const m = l.match(/\(?([^()\s]+submission\.js):(\d+):(\d+)\)?/);
    if (m) {
      return { file: "submission.js", line: Number(m[2]) };
    }
  }
  return null;
}

class LiveCapture {
  constructor(index) {
    this.index = index;
    this._oldOut = null;
    this._oldErr = null;
  }

  start() {
    this._oldOut = process.stdout.write;
    this._oldErr = process.stderr.write;

    process.stdout.write = (chunk, encoding, cb) => {
      try {
        emit({ event: "output", stream: "stdout", text: Buffer.from(chunk).toString("utf-8"), index: this.index });
      } catch {}
      if (typeof encoding === "function") encoding();
      if (typeof cb === "function") cb();
      return true;
    };

    process.stderr.write = (chunk, encoding, cb) => {
      try {
        emit({ event: "output", stream: "stderr", text: Buffer.from(chunk).toString("utf-8"), index: this.index });
      } catch {}
      if (typeof encoding === "function") encoding();
      if (typeof cb === "function") cb();
      return true;
    };
  }

  stop() {
    if (this._oldOut) process.stdout.write = this._oldOut;
    if (this._oldErr) process.stderr.write = this._oldErr;
  }
}

function loadSubmission(code) {
  try { fs.mkdirSync("/workspace", { recursive: true }); } catch {}
  const path = "/workspace/submission.js";
  fs.writeFileSync(path, code, "utf8");

  // Use a VM context so `function foo(){}` becomes reachable via context.foo.
  // Provide a minimal, familiar environment.
  const ctx = {
    console,
    setTimeout,
    clearTimeout,
    setInterval,
    clearInterval,
    Buffer,
  };
  vm.createContext(ctx);

  const script = new vm.Script(code, { filename: path, displayErrors: true });
  const cap = new LiveCapture(-1);
  cap.start();
  try {
    script.runInContext(ctx, { timeout: 1000 });
  } finally {
    cap.stop();
  }
  return ctx;
}

function fatal(e) {
  emit({
    event: "fatal",
    error: {
      type: e && e.name ? e.name : "Error",
      message: e && e.message ? String(e.message) : String(e),
      location: extractLocationFromStack(e && e.stack),
    },
  });
}

let submissionCtx = null;
let fnName = null;

// Read NDJSON from stdin
let buf = "";
process.stdin.setEncoding("utf8");
process.stdin.on("data", (chunk) => {
  buf += chunk;
  let idx;
  while ((idx = buf.indexOf("\n")) >= 0) {
    const line = buf.slice(0, idx).trim();
    buf = buf.slice(idx + 1);
    if (!line) continue;

    let msg;
    try {
      msg = JSON.parse(line);
    } catch {
      continue;
    }

    const cmd = msg.cmd;

    if (cmd === "init") {
      try {
        const files = msg.files || {};
        const submission = files.submission;
        if (typeof submission !== "string") throw new Error("missing files.submission");

        const entry = msg.entry;
        fnName = typeof entry === "object" && entry ? entry.name : entry;
        if (!fnName) throw new Error("missing entry/name");

        submissionCtx = loadSubmission(submission);
        if (typeof submissionCtx[fnName] !== "function") {
          throw Object.assign(new Error(`Function ${fnName} is not defined in submission.js`), { name: "MissingFunction" });
        }

        emit({ event: "ready" });
      } catch (e) {
        fatal(e);
      }
    }

    else if (cmd === "case") {
      if (!submissionCtx || !fnName) {
        emit({ event: "fatal", error: { type: "NotInitialized", message: "init not received", location: null } });
        continue;
      }

      const caseIndex = msg.index;
      const args = Array.isArray(msg.args) ? msg.args : [];

      try {
        const cap = new LiveCapture(Number.isInteger(caseIndex) ? caseIndex : -1);
        cap.start();
        let result;
        try {
          result = submissionCtx[fnName](...args);
        } finally {
          cap.stop();
        }

        emit({ event: "case", index: caseIndex, ok: true, result: safeJsonValue(result), error: null });
      } catch (e) {
        emit({
          event: "case",
          index: caseIndex,
          ok: false,
          result: null,
          error: {
            type: e && e.name ? e.name : "Error",
            message: e && e.message ? String(e.message) : String(e),
            location: extractLocationFromStack(e && e.stack),
          },
        });
      }
    }

    else if (cmd === "stop") {
      process.exit(0);
    }
  }
});
