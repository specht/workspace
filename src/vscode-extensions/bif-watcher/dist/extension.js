"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.BifProjectRunner = void 0;
exports.activate = activate;
exports.deactivate = deactivate;
const vscode = __importStar(require("vscode"));
const core_1 = require("./core");
const TASK_TYPE = "bif-project-runner";
const TASK_SOURCE = "BIF";
function projectKey(folder) {
    return folder.uri.toString();
}
async function rootFileExists(folder, fileName) {
    try {
        const entry = await vscode.workspace.fs.stat(vscode.Uri.joinPath(folder.uri, fileName));
        return (entry.type & vscode.FileType.File) !== 0;
    }
    catch {
        return false;
    }
}
function isBifTask(execution, folder) {
    const definition = execution.task.definition;
    return (definition.type === TASK_TYPE &&
        definition.root === projectKey(folder));
}
class BifProjectRunner {
    executions = new Map();
    starting = new Set();
    disposables = [];
    disposed = false;
    constructor() {
        this.disposables.push(vscode.workspace.onDidChangeWorkspaceFolders(event => {
            for (const folder of event.removed) {
                this.stop(folder);
            }
            for (const folder of event.added) {
                void this.start(folder);
            }
        }), vscode.tasks.onDidEndTaskProcess(event => {
            this.handleTaskEnd(event);
        }));
    }
    async startAll() {
        for (const folder of vscode.workspace.workspaceFolders ?? []) {
            await this.start(folder);
        }
    }
    async start(folder) {
        const key = projectKey(folder);
        if (this.disposed || this.starting.has(key) || this.executions.has(key)) {
            return;
        }
        if (!(await rootFileExists(folder, core_1.BIF_MARKER_FILE))) {
            return;
        }
        if (!(await rootFileExists(folder, "package.json"))) {
            void vscode.window.showErrorMessage(`BIF project “${folder.name}” has ${core_1.BIF_MARKER_FILE}, but no package.json at its root.`);
            return;
        }
        this.starting.add(key);
        try {
            const existing = vscode.tasks.taskExecutions.find(execution => isBifTask(execution, folder));
            if (existing) {
                this.executions.set(key, existing);
                return;
            }
            const install = await (0, core_1.dependencyInstallRequired)(folder.uri.fsPath);
            if (this.disposed || !(vscode.workspace.workspaceFolders ?? []).includes(folder)) {
                return;
            }
            const definition = { type: TASK_TYPE, root: key };
            const command = install
                ? "npm install --prefer-offline --no-audit --no-fund && npm run dev"
                : "npm run dev";
            const task = new vscode.Task(definition, folder, "Development watcher", TASK_SOURCE, new vscode.ShellExecution(command, { cwd: folder.uri.fsPath }), []);
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
        }
        catch (error) {
            const message = error instanceof Error ? error.message : String(error);
            void vscode.window.showErrorMessage(`BIF could not start in “${folder.name}”: ${message}`);
        }
        finally {
            this.starting.delete(key);
        }
    }
    handleTaskEnd(event) {
        for (const [key, execution] of this.executions) {
            if (event.execution !== execution) {
                continue;
            }
            this.executions.delete(key);
            if (!this.disposed && event.exitCode !== undefined && event.exitCode !== 0) {
                const folder = (vscode.workspace.workspaceFolders ?? []).find(candidate => projectKey(candidate) === key);
                if (folder) {
                    void vscode.window
                        .showErrorMessage(`BIF setup or development watcher stopped in “${folder.name}” (exit code ${event.exitCode}).`, "Restart")
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
    stop(folder) {
        const key = projectKey(folder);
        this.executions.get(key)?.terminate();
        this.executions.delete(key);
        this.starting.delete(key);
    }
    dispose() {
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
exports.BifProjectRunner = BifProjectRunner;
async function activate(context) {
    const runner = new BifProjectRunner();
    context.subscriptions.push(runner);
    await runner.startAll();
}
function deactivate() { }
//# sourceMappingURL=extension.js.map