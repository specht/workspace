import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";
import { randomUUID } from "node:crypto";
import { run } from "./process";
import type { Checkpoint, CheckpointAction, RepositoryContext } from "./types";

export const CHECKPOINT_REF = "refs/hackschule-checkpoints/current";

interface CommitMetadata { name: string; action: CheckpointAction; restoredFrom?: string; }
interface TreeEntry { mode: string; type: string; oid: string; repositoryPath: string; }

function normalizePathspec(value: string): string {
  return value.split(path.sep).join("/") || ".";
}
function assertInside(parent: string, child: string): void {
  const relative = path.relative(parent, child);
  if (relative.startsWith("..") || path.isAbsolute(relative)) {
    throw new Error(`Path escapes workspace: ${child}`);
  }
}
export async function initializeRepository(workspaceRoot: string): Promise<void> {
  await run("git", ["init"], { cwd: workspaceRoot });
}
export async function discoverRepository(workspaceRoot: string): Promise<RepositoryContext> {
  const output = String(await run("git", ["rev-parse", "--show-toplevel"], { cwd: workspaceRoot })).trim();
  const repositoryRoot = path.resolve(output);
  const resolvedWorkspace = path.resolve(workspaceRoot);
  assertInside(repositoryRoot, resolvedWorkspace);
  return {
    repositoryRoot,
    workspaceRoot: resolvedWorkspace,
    scopePathspec: normalizePathspec(path.relative(repositoryRoot, resolvedWorkspace))
  };
}
export async function hasMergeConflicts(context: RepositoryContext): Promise<boolean> {
  const output = String(await run("git", ["diff", "--name-only", "--diff-filter=U", "--", context.scopePathspec], { cwd: context.repositoryRoot }));
  return output.trim().length > 0;
}
async function hasHead(context: RepositoryContext): Promise<boolean> {
  const output = String(await run("git", ["rev-parse", "--verify", "HEAD"], {
    cwd: context.repositoryRoot, allowFailure: true
  })).trim();
  return output.length > 0;
}
async function checkpointHead(context: RepositoryContext): Promise<string | undefined> {
  const output = String(await run("git", ["rev-parse", "--verify", CHECKPOINT_REF], {
    cwd: context.repositoryRoot, allowFailure: true
  })).trim();
  return output || undefined;
}
async function withTemporaryIndex<T>(
  context: RepositoryContext,
  callback: (env: NodeJS.ProcessEnv) => Promise<T>
): Promise<T> {
  const directory = await fs.mkdtemp(path.join(os.tmpdir(), "hackschule-checkpoint-"));
  const env = { GIT_INDEX_FILE: path.join(directory, `index-${randomUUID()}`) };
  try {
    if (await hasHead(context)) {
      await run("git", ["read-tree", "HEAD"], { cwd: context.repositoryRoot, env });
    } else {
      await run("git", ["read-tree", "--empty"], { cwd: context.repositoryRoot, env });
    }
    return await callback(env);
  } finally {
    await fs.rm(directory, { recursive: true, force: true });
  }
}
export async function snapshotTree(context: RepositoryContext): Promise<string> {
  return withTemporaryIndex(context, async env => {
    await run("git", ["add", "-A", "--", context.scopePathspec], { cwd: context.repositoryRoot, env });
    return String(await run("git", ["write-tree"], { cwd: context.repositoryRoot, env })).trim();
  });
}
function metadataMessage(metadata: CommitMetadata): string {
  return [
    `checkpoint: ${metadata.name}`,
    "",
    "Hackschule-Checkpoint: true",
    `Checkpoint-Name: ${metadata.name}`,
    `Checkpoint-Action: ${metadata.action}`,
    ...(metadata.restoredFrom ? [`Checkpoint-Restored-From: ${metadata.restoredFrom}`] : [])
  ].join("\n");
}
export async function createCheckpoint(context: RepositoryContext, metadata: CommitMetadata): Promise<Checkpoint> {
  const treeOid = await snapshotTree(context);
  const parentOid = await checkpointHead(context);
  const oid = String(await run("git", [
    "commit-tree", treeOid,
    ...(parentOid ? ["-p", parentOid] : []),
    "-m", metadataMessage(metadata)
  ], {
    cwd: context.repositoryRoot,
    env: {
      GIT_AUTHOR_NAME: "Hackschule Checkpoints",
      GIT_AUTHOR_EMAIL: "checkpoints@localhost",
      GIT_COMMITTER_NAME: "Hackschule Checkpoints",
      GIT_COMMITTER_EMAIL: "checkpoints@localhost"
    }
  })).trim();

  if (parentOid) {
    await run("git", ["update-ref", CHECKPOINT_REF, oid, parentOid], { cwd: context.repositoryRoot });
  } else {
    await run("git", ["update-ref", CHECKPOINT_REF, oid], { cwd: context.repositoryRoot });
  }

  return {
    oid,
    parentOid,
    timestamp: Math.floor(Date.now() / 1000),
    name: metadata.name,
    action: metadata.action,
    restoredFrom: metadata.restoredFrom
  };
}
function parseCheckpointMetadata(body: string): Pick<Checkpoint, "name" | "action" | "restoredFrom"> {
  const parsedAction = /^Checkpoint-Action:\s*(snapshot|restore)$/m.exec(body)?.[1];
  const action: CheckpointAction = parsedAction === "restore" ? "restore" : "snapshot";
  return {
    name:
      /^Checkpoint-Name:\s*(.+)$/m.exec(body)?.[1]?.trim() ??
      body.split("\n")[0]?.replace(/^checkpoint:\s*/, "").trim() ??
      "Projektstand",
    action,
    restoredFrom: /^Checkpoint-Restored-From:\s*(.+)$/m.exec(body)?.[1]?.trim()
  };
}
export async function listCheckpoints(context: RepositoryContext): Promise<Checkpoint[]> {
  if (!(await checkpointHead(context))) return [];
  const format = "%H%x1f%P%x1f%ct%x1f%B%x1e";
  const output = String(await run("git", ["log", "--first-parent", `--format=${format}`, CHECKPOINT_REF], {
    cwd: context.repositoryRoot
  }));
  return output.split("\x1e").map(record => record.trim()).filter(Boolean).map(record => {
    const [oid, parents, timestampText, ...bodyParts] = record.split("\x1f");
    const metadata = parseCheckpointMetadata(bodyParts.join("\x1f"));
    return {
      oid,
      parentOid: parents.split(" ")[0] || undefined,
      timestamp: Number.parseInt(timestampText, 10),
      ...metadata
    };
  });
}
export async function treeForCheckpoint(
  context: RepositoryContext,
  checkpointOid: string
): Promise<string> {
  return String(await run("git", ["rev-parse", `${checkpointOid}^{tree}`], {
    cwd: context.repositoryRoot
  })).trim();
}

export async function workspaceMatchesCheckpoint(
  context: RepositoryContext,
  checkpointOid: string
): Promise<boolean> {
  const [currentTree, selectedTree] = await Promise.all([
    snapshotTree(context),
    treeForCheckpoint(context, checkpointOid)
  ]);
  return currentTree === selectedTree;
}

export async function diffCheckpointAgainstCurrent(
  context: RepositoryContext,
  checkpointOid: string
): Promise<string> {
  const currentTree = await snapshotTree(context);
  const output = String(await run("git", [
    "diff",
    "--no-ext-diff",
    "--no-color",
    "--find-renames",
    checkpointOid,
    currentTree,
    "--",
    context.scopePathspec
  ], { cwd: context.repositoryRoot }));

  return output.trim().length > 0
    ? output
    : "Keine Unterschiede zwischen diesem Checkpoint und dem aktuellen Projekt.\n";
}

export async function isDirtySinceLatestCheckpoint(context: RepositoryContext): Promise<boolean> {
  const latest = await checkpointHead(context);
  if (!latest) return true;
  return (await snapshotTree(context)) !== (await treeForCheckpoint(context, latest));
}

async function checkpointEntries(
  context: RepositoryContext,
  checkpointOid: string
): Promise<Map<string, TreeEntry>> {
  const output = String(await run("git", [
    "ls-tree", "-r", "-z", checkpointOid, "--", context.scopePathspec
  ], { cwd: context.repositoryRoot }));

  const result = new Map<string, TreeEntry>();
  for (const record of output.split("\0").filter(Boolean)) {
    const match = /^(\d+)\s+(\w+)\s+([0-9a-f]+)\t(.+)$/.exec(record);
    if (!match) continue;

    const [, mode, type, oid, repositoryPath] = match;
    const absolutePath = path.resolve(context.repositoryRoot, repositoryPath);
    assertInside(context.workspaceRoot, absolutePath);
    result.set(absolutePath, { mode, type, oid, repositoryPath });
  }
  return result;
}
async function currentManagedFiles(context: RepositoryContext): Promise<Set<string>> {
  const output = String(await run("git", [
    "ls-files", "-z", "--cached", "--others", "--exclude-standard", "--", context.scopePathspec
  ], { cwd: context.repositoryRoot }));

  const result = new Set<string>();
  for (const repositoryPath of output.split("\0").filter(Boolean)) {
    const absolutePath = path.resolve(context.repositoryRoot, repositoryPath);
    assertInside(context.workspaceRoot, absolutePath);
    result.add(absolutePath);
  }
  return result;
}
async function removeEmptyParents(filePath: string, stopAt: string): Promise<void> {
  let current = path.dirname(filePath);
  while (current !== stopAt) {
    const relative = path.relative(stopAt, current);
    if (relative.startsWith("..") || path.isAbsolute(relative)) break;
    try {
      await fs.rmdir(current);
    } catch {
      break;
    }
    current = path.dirname(current);
  }
}
export async function restoreCheckpointFiles(
  context: RepositoryContext,
  checkpointOid: string
): Promise<void> {
  const entries = await checkpointEntries(context, checkpointOid);
  const currentFiles = await currentManagedFiles(context);

  for (const filePath of currentFiles) {
    if (entries.has(filePath)) continue;
    await fs.rm(filePath, { force: true });
    await removeEmptyParents(filePath, context.workspaceRoot);
  }

  for (const [absolutePath, entry] of entries) {
    if (entry.type !== "blob") {
      throw new Error(`Unsupported Git tree entry type: ${entry.type}`);
    }
    if (entry.mode === "120000") {
      throw new Error("Symbolic links are not supported by this checkpoint version.");
    }

    const contents = await run("git", ["cat-file", "blob", entry.oid], {
      cwd: context.repositoryRoot,
      binary: true
    });

    await fs.mkdir(path.dirname(absolutePath), { recursive: true });
    await fs.writeFile(absolutePath, contents);
    if (process.platform !== "win32") {
      await fs.chmod(absolutePath, entry.mode === "100755" ? 0o755 : 0o644);
    }
  }
}
