import * as vscode from "vscode";
import {
  BIF_MARKER_FILE,
  dependencyInstallCommand,
  dependencyInstallRequired,
} from "./core";

const TASK_TYPE = "bif-project-runner";
const TASK_SOURCE = "BIF";

interface BifTaskDefinition extends vscode.TaskDefinition {
  type: typeof TASK_TYPE;
  root: string;
}

function projectKey(folder: vscode.WorkspaceFolder): string {
  return folder.uri.toString();
}

async function rootFileExists(
  folder: vscode.WorkspaceFolder,
  fileName: string,
): Promise<boolean> {
  try {
    const entry = await vscode.workspace.fs.stat(
      vscode.Uri.joinPath(folder.uri, fileName),
    );
    return (entry.type & vscode.FileType.File) !== 0;
  } catch {
    return false;
  }
}

function isBifTask(
  execution: vscode.TaskExecution,
  folder: vscode.WorkspaceFolder,
): boolean {
  const definition = execution.task.definition;
  return (
    definition.type === TASK_TYPE &&
    definition.root === projectKey(folder)
  );
}

export class BifProjectRunner implements vscode.Disposable {
  private readonly executions = new Map<string, vscode.TaskExecution>();
  private readonly starting = new Set<string>();
  private readonly disposables: vscode.Disposable[] = [];
  private disposed = false;

  constructor() {
    this.disposables.push(
      vscode.workspace.onDidChangeWorkspaceFolders(event => {
        for (const folder of event.removed) {
          this.stop(folder);
        }
        for (const folder of event.added) {
          void this.start(folder);
        }
      }),
      vscode.tasks.onDidEndTaskProcess(event => {
        this.handleTaskEnd(event);
      }),
    );
  }

  async startAll(): Promise<void> {
    for (const folder of vscode.workspace.workspaceFolders ?? []) {
      await this.start(folder);
    }
  }

  async start(folder: vscode.WorkspaceFolder): Promise<void> {
    const key = projectKey(folder);
    if (this.disposed || this.starting.has(key) || this.executions.has(key)) {
      return;
    }
    if (!(await rootFileExists(folder, BIF_MARKER_FILE))) {
      return;
    }
    if (!(await rootFileExists(folder, "package.json"))) {
      void vscode.window.showErrorMessage(
        `BIF project “${folder.name}” has ${BIF_MARKER_FILE}, but no package.json at its root.`,
      );
      return;
    }

    this.starting.add(key);
    try {
      const existing = vscode.tasks.taskExecutions.find(execution =>
        isBifTask(execution, folder),
      );
      if (existing) {
        this.executions.set(key, existing);
        return;
      }

      const install = await dependencyInstallRequired(folder.uri.fsPath);
      const installCommand = install
        ? await dependencyInstallCommand(folder.uri.fsPath)
        : undefined;
      if (this.disposed || !(vscode.workspace.workspaceFolders ?? []).includes(folder)) {
        return;
      }

      const definition: BifTaskDefinition = { type: TASK_TYPE, root: key };
      const command = installCommand
        ? `${installCommand} && npm run dev`
        : "npm run dev";
      const task = new vscode.Task(
        definition,
        folder,
        "Development watcher",
        TASK_SOURCE,
        new vscode.ShellExecution(command, { cwd: folder.uri.fsPath }),
        [],
      );

      task.isBackground = true;
      task.detail = install
        ? `Install dependencies and run npm run dev in ${folder.name}`
        : `Run npm run dev in ${folder.name}`;
      task.presentationOptions = {
        reveal: vscode.TaskRevealKind.Silent,
        focus: false,
        panel: vscode.TaskPanelKind.Dedicated,
        showReuseMessage: false,
      };

      const execution = await vscode.tasks.executeTask(task);
      if (this.disposed || !(vscode.workspace.workspaceFolders ?? []).includes(folder)) {
        execution.terminate();
        return;
      }
      this.executions.set(key, execution);
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      void vscode.window.showErrorMessage(
        `BIF could not start in “${folder.name}”: ${message}`,
      );
    } finally {
      this.starting.delete(key);
    }
  }

  private handleTaskEnd(event: vscode.TaskProcessEndEvent): void {
    for (const [key, execution] of this.executions) {
      if (event.execution !== execution) {
        continue;
      }

      this.executions.delete(key);
      if (!this.disposed && event.exitCode !== undefined && event.exitCode !== 0) {
        const folder = (vscode.workspace.workspaceFolders ?? []).find(
          candidate => projectKey(candidate) === key,
        );
        if (folder) {
          void vscode.window
            .showErrorMessage(
              `BIF setup or development watcher stopped in “${folder.name}” (exit code ${event.exitCode}).`,
              "Restart",
            )
            .then(selection => {
              if (selection === "Restart") {
                void this.start(folder);
              }
            });
        }
      }
      return;
    }
  }

  private stop(folder: vscode.WorkspaceFolder): void {
    const key = projectKey(folder);
    this.executions.get(key)?.terminate();
    this.executions.delete(key);
    this.starting.delete(key);
  }

  dispose(): void {
    this.disposed = true;
    for (const execution of this.executions.values()) {
      execution.terminate();
    }
    this.executions.clear();
    this.starting.clear();
    for (const disposable of this.disposables) {
      disposable.dispose();
    }
  }
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const runner = new BifProjectRunner();
  context.subscriptions.push(runner);
  await runner.startAll();
}

export function deactivate(): void {}
