import * as path from "node:path";
import * as vscode from "vscode";
import { BRAND } from "./branding";
import { CheckpointTreeProvider } from "./checkpointTree";
import { CheckpointDiffProvider } from "./diffProvider";
import {
  createCheckpoint,
  deleteAllCheckpoints,
  diffCheckpointAgainstCurrent,
  discoverRepository,
  hasMergeConflicts,
  initializeRepository,
  isDirtySinceLatestCheckpoint,
  listCheckpoints,
  pendingCheckpointByteStats,
  restoreCheckpointFiles,
  workspaceMatchesCheckpoint
} from "./git";
import type { ByteStats, Checkpoint, RepositoryContext } from "./types";
import { askCheckpointName, chooseCheckpoint } from "./ui";

let treeProvider: CheckpointTreeProvider;
let diffProvider: CheckpointDiffProvider;

function activeWorkspaceFolder(): vscode.WorkspaceFolder | undefined {
  const activeDocument = vscode.window.activeTextEditor?.document.uri;
  if (activeDocument) {
    const folder = vscode.workspace.getWorkspaceFolder(activeDocument);
    if (folder) return folder;
  }

  const folders = vscode.workspace.workspaceFolders ?? [];
  return folders.length === 1 ? folders[0] : undefined;
}

async function existingRepositoryContext(): Promise<RepositoryContext | undefined> {
  const folder = activeWorkspaceFolder();
  if (!folder) return undefined;

  try {
    return await discoverRepository(folder.uri.fsPath);
  } catch {
    return undefined;
  }
}

async function repositoryContext(
  allowInitialize: boolean
): Promise<RepositoryContext | undefined> {
  const folder = activeWorkspaceFolder();
  if (!folder) {
    await vscode.window.showErrorMessage(
      "Öffne zuerst einen Projektordner oder eine Datei in dem Projekt."
    );
    return undefined;
  }

  try {
    return await discoverRepository(folder.uri.fsPath);
  } catch {
    if (!allowInitialize) return undefined;
  }

  const choice = await vscode.window.showInformationMessage(
    "Dieser Ordner ist noch kein Git-Projekt. Soll er für Checkpoints vorbereitet werden? Dabei wird nur lokal „git init“ ausgeführt.",
    { modal: true },
    "Projekt vorbereiten"
  );

  if (choice !== "Projekt vorbereiten") return undefined;

  try {
    await initializeRepository(folder.uri.fsPath);

    // Return the newly discovered context to the original command. The caller
    // must continue the requested create/restore action after initialization.
    const context = await discoverRepository(folder.uri.fsPath);
    treeProvider.refresh();
    return context;
  } catch (error: unknown) {
    const message = error instanceof Error ? error.message : String(error);
    await vscode.window.showErrorMessage(
      `Git konnte nicht vorbereitet werden: ${message}`
    );
    return undefined;
  }
}

function hasUnsavedWorkspaceDocuments(context: RepositoryContext): boolean {
  const root = path.resolve(context.workspaceRoot);
  return vscode.workspace.textDocuments.some(document => {
    if (!document.isDirty) return false;
    if (document.isUntitled) return true;
    if (document.uri.scheme !== "file") return false;

    const documentPath = path.resolve(document.uri.fsPath);
    const relative = path.relative(root, documentPath);
    return relative === "" ||
      (!relative.startsWith("..") && !path.isAbsolute(relative));
  });
}

async function ensureWorkspaceFilesSaved(
  context: RepositoryContext
): Promise<boolean> {
  if (!hasUnsavedWorkspaceDocuments(context)) return true;

  const choice = await vscode.window.showWarningMessage(
    "Alle Änderungen müssen gespeichert sein, bevor ein Checkpoint erstellt oder wiederhergestellt werden kann.",
    { modal: true },
    "Alle speichern und fortfahren"
  );

  if (choice !== "Alle speichern und fortfahren") return false;

  await vscode.workspace.saveAll(false);
  if (hasUnsavedWorkspaceDocuments(context)) {
    await vscode.window.showWarningMessage(
      "Einige Dateien konnten nicht gespeichert werden. Speichere oder benenne sie zuerst und versuche es dann erneut."
    );
    return false;
  }

  return true;
}

async function ensureRestoreIsSafe(context: RepositoryContext): Promise<boolean> {
  if (!(await ensureWorkspaceFilesSaved(context))) return false;

  if (await hasMergeConflicts(context)) {
    await vscode.window.showWarningMessage(
      "Das Projekt enthält ungelöste Git-Konflikte. Löse sie zuerst, bevor du einen Checkpoint wiederherstellst."
    );
    return false;
  }

  return true;
}

async function nextUnnamedCheckpointName(
  context: RepositoryContext
): Promise<string> {
  const checkpoints = await listCheckpoints(context);
  const used = new Set(
    checkpoints
      .map(checkpoint => /^Checkpoint (\d+)$/.exec(checkpoint.name)?.[1])
      .filter((value): value is string => value !== undefined)
      .map(Number)
  );

  let number = 1;
  while (used.has(number)) number += 1;
  return `Checkpoint ${number}`;
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  const units = ["KB", "MB", "GB", "TB"];
  let value = bytes / 1024;
  let unit = units[0];
  for (let index = 1; index < units.length && value >= 1024; index += 1) {
    value /= 1024;
    unit = units[index];
  }
  const digits = value >= 100 ? 0 : value >= 10 ? 1 : 2;
  return `${value.toLocaleString("de-DE", {
    minimumFractionDigits: 0,
    maximumFractionDigits: digits,
  })} ${unit}`;
}

async function confirmLargeCheckpoint(
  context: RepositoryContext,
  stats: ByteStats
): Promise<boolean> {
  const configuration = vscode.workspace.getConfiguration("hackschuleCheckpoints");
  const addedLimit = configuration.get<number>("warnAddedBytes", 262144000);
  const fileLimit = configuration.get<number>("warnChangedFiles", 2000);
  const singleFileLimit = configuration.get<number>("warnSingleFileBytes", 104857600);

  const reasons: string[] = [];
  if (addedLimit > 0 && stats.addedBytes >= addedLimit) {
    reasons.push(`${formatBytes(stats.addedBytes)} neue oder veränderte Daten`);
  }
  if (fileLimit > 0 && stats.changedFiles >= fileLimit) {
    reasons.push(`${stats.changedFiles} betroffene Dateien`);
  }
  if (singleFileLimit > 0 && stats.largestAddedBytes >= singleFileLimit) {
    reasons.push(`eine Datei mit ${formatBytes(stats.largestAddedBytes)}`);
  }

  if (reasons.length === 0) return true;

  const choice = await vscode.window.showWarningMessage(
    [
      "Dieser Checkpoint wäre ungewöhnlich groß.",
      reasons.join(" · "),
      "Prüfe, ob wirklich der richtige Projektordner geöffnet ist.",
    ].join(" "),
    { modal: true },
    "Trotzdem erstellen"
  );
  return choice === "Trotzdem erstellen";
}

async function createCheckpointWithName(
  context: RepositoryContext,
  name: string
): Promise<void> {
  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `Erstelle „${name}“…`,
      cancellable: false
    },
    async () => {
      await createCheckpoint(context, { name, action: "snapshot" });
    }
  );
  treeProvider.refresh(true);
}

async function saveNamedCheckpoint(context: RepositoryContext): Promise<boolean> {
  if (!(await ensureWorkspaceFilesSaved(context))) return false;

  if (!(await isDirtySinceLatestCheckpoint(context))) {
    await vscode.window.showInformationMessage(
      "Seit dem letzten Checkpoint hat sich nichts geändert. Es wurde kein neuer Checkpoint erstellt."
    );
    return false;
  }

  const pendingStats = await pendingCheckpointByteStats(context);
  if (!(await confirmLargeCheckpoint(context, pendingStats))) return false;

  const enteredName = await askCheckpointName();
  if (enteredName === undefined) return false;

  const name = enteredName || await nextUnnamedCheckpointName(context);
  await createCheckpointWithName(context, name);
  await vscode.window.showInformationMessage(
    `Checkpoint „${name}“ wurde erstellt.`
  );
  return true;
}

async function createCommand(): Promise<void> {
  const context = await repositoryContext(true);
  if (!context) return;
  await saveNamedCheckpoint(context);
}

function canonicalCheckpoint(
  selected: Checkpoint,
  checkpoints: Checkpoint[]
): Checkpoint {
  const byOid = new Map(
    checkpoints.map(checkpoint => [checkpoint.oid, checkpoint])
  );
  const visited = new Set<string>();
  let current = selected;

  while (
    current.action === "restore" &&
    current.restoredFrom &&
    !visited.has(current.oid)
  ) {
    visited.add(current.oid);
    const source = byOid.get(current.restoredFrom);
    if (!source) break;
    current = source;
  }

  return current;
}

async function restoreSelectedCheckpoint(
  context: RepositoryContext,
  selected: Checkpoint,
  checkpoints: Checkpoint[]
): Promise<void> {
  if (!(await ensureRestoreIsSafe(context))) return;

  const canonical = canonicalCheckpoint(selected, checkpoints);

  if (await workspaceMatchesCheckpoint(context, selected.oid)) {
    await vscode.window.showInformationMessage(
      `Das Projekt befindet sich bereits auf dem Stand „${canonical.name}“. Es wurde nichts verändert.`
    );
    return;
  }

  const dirty = await isDirtySinceLatestCheckpoint(context);

  if (dirty) {
    const choice = await vscode.window.showWarningMessage(
      "Das Projekt wurde seit dem letzten Checkpoint verändert. Der aktuelle Zustand wird zuerst als Sicherheits-Checkpoint gespeichert. Danach wird der gewählte Checkpoint wiederhergestellt.",
      { modal: true },
      "Aktuellen Stand speichern und zurückkehren"
    );

    if (choice !== "Aktuellen Stand speichern und zurückkehren") return;

    const safetyStats = await pendingCheckpointByteStats(context);
    if (!(await confirmLargeCheckpoint(context, safetyStats))) return;

    await createCheckpointWithName(
      context,
      `Vor dem Wiederherstellen von „${canonical.name}“`
    );
  } else {
    const choice = await vscode.window.showWarningMessage(
      `Zu „${canonical.name}“ zurückkehren? Neuere Checkpoints bleiben erhalten.`,
      { modal: true },
      "Wiederherstellen"
    );
    if (choice !== "Wiederherstellen") return;
  }

  await vscode.window.withProgress(
    {
      location: vscode.ProgressLocation.Notification,
      title: `Stelle „${canonical.name}“ wieder her…`,
      cancellable: false
    },
    async () => {
      await restoreCheckpointFiles(context, selected.oid);
      await createCheckpoint(context, {
        name: `Zurück zu „${canonical.name}“`,
        action: "restore",
        restoredFrom: canonical.oid
      });
    }
  );

  treeProvider.refresh(true);
  await vscode.commands.executeCommand(
    "workbench.files.action.refreshFilesExplorer"
  );
  await vscode.window.showInformationMessage(
    `„${canonical.name}“ wurde wiederhergestellt. Neuere Checkpoints sind weiterhin verfügbar.`
  );
}

async function restoreCommand(selectedCheckpoint?: Checkpoint): Promise<void> {
  const context = await repositoryContext(true);
  if (!context) return;

  const checkpoints = await listCheckpoints(context);
  if (checkpoints.length === 0) {
    const choice = await vscode.window.showInformationMessage(
      "Es gibt noch keine Checkpoints.",
      "Ersten Checkpoint erstellen"
    );
    if (choice === "Ersten Checkpoint erstellen") {
      await saveNamedCheckpoint(context);
    }
    return;
  }

  const selected = selectedCheckpoint ?? await chooseCheckpoint(checkpoints);
  if (!selected) return;

  await restoreSelectedCheckpoint(context, selected, checkpoints);
}

async function compareCommand(checkpoint: Checkpoint): Promise<void> {
  const context = await repositoryContext(false);
  if (!context) return;

  if (!(await ensureWorkspaceFilesSaved(context))) return;

  const diff = await diffCheckpointAgainstCurrent(context, checkpoint.oid);
  const uri = diffProvider.createDocumentUri(
    checkpoint.name,
    diff.rawDiff,
    diff.byteStats
  );
  const document = await vscode.workspace.openTextDocument(uri);
  await vscode.languages.setTextDocumentLanguage(document, "diff");
  await vscode.window.showTextDocument(document, { preview: true });
}

async function deleteAllCommand(): Promise<void> {
  const context = await repositoryContext(false);
  if (!context) return;

  const checkpoints = await listCheckpoints(context);
  if (checkpoints.length === 0) {
    await vscode.window.showInformationMessage("Es gibt keine Checkpoints zum Löschen.");
    return;
  }

  const firstChoice = await vscode.window.showWarningMessage(
    [
      `Alle ${checkpoints.length} Checkpoints dieses Projekts löschen?`,
      "Die aktuellen Projektdateien und der normale Git-Verlauf bleiben unverändert.",
      "Die Checkpoints können danach nicht mehr über die Erweiterung wiederhergestellt werden.",
    ].join(" "),
    { modal: true },
    "Löschen vorbereiten"
  );
  if (firstChoice !== "Löschen vorbereiten") return;

  const confirmation = await vscode.window.showInputBox({
    title: "Alle Checkpoints endgültig löschen",
    prompt: "Gib LÖSCHEN ein, um alle Checkpoints dieses Projekts zu entfernen.",
    placeHolder: "LÖSCHEN",
    ignoreFocusOut: true,
    validateInput(value) {
      return value === "LÖSCHEN" ? undefined : "Bitte gib genau LÖSCHEN ein.";
    },
  });
  if (confirmation !== "LÖSCHEN") return;

  await deleteAllCheckpoints(context);
  treeProvider.refresh(true);
  await vscode.window.showInformationMessage(
    "Alle Checkpoints wurden entfernt. Git gibt den belegten Speicherplatz möglicherweise erst bei einer späteren automatischen Bereinigung frei."
  );
}

export function activate(extensionContext: vscode.ExtensionContext): void {
  treeProvider = new CheckpointTreeProvider(existingRepositoryContext);
  diffProvider = new CheckpointDiffProvider();

  const treeView = vscode.window.createTreeView(
    "hackschuleCheckpoints.view",
    { treeDataProvider: treeProvider, showCollapseAll: false }
  );

  const statusBar = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left,
    80
  );
  statusBar.command = "hackschuleCheckpoints.create";
  statusBar.text = BRAND.statusBarText;
  statusBar.tooltip = BRAND.statusBarTooltip;

  const updateStatusBar = (): void => {
    const visible = vscode.workspace
      .getConfiguration("hackschuleCheckpoints")
      .get<boolean>("showStatusBar", true);
    visible ? statusBar.show() : statusBar.hide();
  };

  updateStatusBar();

  extensionContext.subscriptions.push(
    treeView,
    statusBar,
    vscode.workspace.registerTextDocumentContentProvider(
      CheckpointDiffProvider.scheme,
      diffProvider
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.create",
      createCommand
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.restore",
      () => restoreCommand()
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.restoreItem",
      (checkpoint: Checkpoint) => restoreCommand(checkpoint)
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.compareItem",
      (checkpoint: Checkpoint) => compareCommand(checkpoint)
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.refresh",
      () => treeProvider.refresh(true)
    ),
    vscode.commands.registerCommand(
      "hackschuleCheckpoints.deleteAll",
      deleteAllCommand
    ),
    vscode.workspace.onDidChangeWorkspaceFolders(() => treeProvider.refresh()),
    vscode.window.onDidChangeActiveTextEditor(() => treeProvider.refresh()),
    vscode.workspace.onDidChangeConfiguration(event => {
      if (event.affectsConfiguration("hackschuleCheckpoints.showStatusBar")) {
        updateStatusBar();
      }
    })
  );
}

export function deactivate(): void {}
