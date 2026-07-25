const vscode = require("vscode");
const fs = require("fs");
const yaml = require("yaml");
const os = require("os");
const path = require("path");

const tutorialDir = path.join(os.homedir(), ".hs-kbd-tutorial");
const stateFilePath = path.join(tutorialDir, ".state.json");
const managedWorkspacePath = path.join(
    tutorialDir,
    "Tastatur-Tutorial",
);
const workspaceStepStatePath = path.join(
    tutorialDir,
    "workspace-step-states",
);
const pendingWorkspaceStepStateKey =
    "hackschuleKeyboardTutorial.pendingWorkspaceStep";

function serializePosition(position) {
    return {
        line: position.line,
        character: position.character,
    };
}

function serializeSelection(selection) {
    return {
        anchor: serializePosition(selection.anchor),
        active: serializePosition(selection.active),
        start: serializePosition(selection.start),
        end: serializePosition(selection.end),
        isEmpty: selection.isEmpty,
    };
}

function serializeRange(range) {
    // Existing tutorial steps use visibleRanges[0][0] and [0][1].
    return [serializePosition(range.start), serializePosition(range.end)];
}

function serializeDocument(document) {
    return {
        uri: document.uri.toString(),
        fileName: document.fileName,
        languageId: document.languageId,
        lineCount: document.lineCount,
        isDirty: document.isDirty,
    };
}

function tabUri(tab) {
    return tab.input instanceof vscode.TabInputText
        ? tab.input.uri
        : undefined;
}

class TutorialViewProvider {
    constructor(context, sections) {
        this.context = context;
        this.sections = sections;
        this.webviewView = undefined;
        this.activeTutorialDocumentUri = undefined;
        this.activeTutorialOriginalContents = undefined;
        this.activeTutorialRootUri = undefined;
        this.activeWorkspaceFixturePath = undefined;
        this.activeWorkspaceStepKey = undefined;
        this.activeWorkspaceSharedKey = undefined;
        this.activeEventTypes = new Set();
        this.currentStepKey = undefined;
        this.loadQueue = Promise.resolve();
    }

    readCompletionState() {
        const state = {};
        for (const section of this.sections.sections) {
            for (const step of section.steps) {
                state[step.key] = false;
            }
        }

        fs.mkdirSync(tutorialDir, { recursive: true });
        if (fs.existsSync(stateFilePath)) {
            try {
                const data = JSON.parse(fs.readFileSync(stateFilePath, "utf8"));
                for (const key of Object.keys(data)) {
                    if (key in state) {
                        state[key] = data[key] === true;
                    }
                }
            } catch (error) {
                console.warn("Could not read keyboard tutorial state:", error);
            }
        }
        return state;
    }

    writeCompletionState(state) {
        fs.mkdirSync(tutorialDir, { recursive: true });
        fs.writeFileSync(stateFilePath, `${JSON.stringify(state, null, 2)}\n`, "utf8");
    }

    markStepComplete(step) {
        const state = this.readCompletionState();
        if (!(step in state)) {
            return;
        }
        state[step] = true;
        this.writeCompletionState(state);
    }

    postMessage(message) {
        if (!this.webviewView) {
            return false;
        }
        void this.webviewView.webview.postMessage(message);
        return true;
    }

    watches(eventType) {
        return this.activeEventTypes.has(eventType);
    }

    isUriInside(rootUri, candidateUri) {
        if (!rootUri || !candidateUri ||
            rootUri.scheme !== candidateUri.scheme ||
            rootUri.authority !== candidateUri.authority) {
            return false;
        }

        const rootPath = rootUri.path.endsWith("/")
            ? rootUri.path
            : `${rootUri.path}/`;

        return candidateUri.path === rootUri.path ||
            candidateUri.path.startsWith(rootPath);
    }

    isActiveTutorialUri(uri) {
        if (!uri) {
            return false;
        }

        if (this.activeTutorialRootUri) {
            return this.isUriInside(this.activeTutorialRootUri, uri);
        }

        return this.activeTutorialDocumentUri !== undefined &&
            uri.toString() === this.activeTutorialDocumentUri;
    }

    isActiveTutorialDocument(document) {
        return Boolean(document) && this.isActiveTutorialUri(document.uri);
    }

    relativeTutorialPath(uri) {
        if (this.activeTutorialRootUri &&
            this.isUriInside(this.activeTutorialRootUri, uri)) {
            return path.posix.relative(
                this.activeTutorialRootUri.path,
                uri.path,
            );
        }

        return path.posix.basename(uri.path);
    }

    serializeTutorialUri(uri) {
        return {
            uri: uri.toString(),
            path: uri.path,
            fsPath: uri.fsPath,
            relativePath: this.relativeTutorialPath(uri),
        };
    }

    parseStep(key, webview) {
        const htmlPath = vscode.Uri.joinPath(
            this.context.extensionUri,
            "tutorial",
            `${key}.html`,
        ).fsPath;

        if (!fs.existsSync(htmlPath)) {
            throw new Error(`Tutorial step not found: ${key}`);
        }

        const step = {};
        let htmlContent = fs.readFileSync(htmlPath, "utf8");
        htmlContent = htmlContent.replaceAll(
            "tutorial/keyboard.jpg",
            webview.asWebviewUri(
                vscode.Uri.joinPath(
                    this.context.extensionUri,
                    "tutorial",
                    "keyboard.jpg",
                ),
            ),
        );

        const yamlMatch = htmlContent.match(/<yaml>([\s\S]*?)<\/yaml>/i);
        if (yamlMatch) {
            Object.assign(step, yaml.parse(yamlMatch[1].trim()) ?? {});
            htmlContent = htmlContent.replace(yamlMatch[0], "");
        }

        const scriptMatch = htmlContent.match(/<script>([\s\S]*?)<\/script>/i);
        if (scriptMatch) {
            step.script = scriptMatch[1].trim();
            htmlContent = htmlContent.replace(scriptMatch[0], "");
        }

        step.instruction = htmlContent.trim();
        return step;
    }

    updateActiveEventTypes(script = "") {
        const handlers = {
            onDidChangeTextDocument: "handleOnDidChangeTextDocument",
            onDidSaveTextDocument: "handleOnDidSaveTextDocument",
            onDidChangeTextEditorSelection:
                "handleOnDidChangeTextEditorSelection",
            onDidChangeActiveTextEditor:
                "handleOnDidChangeActiveTextEditor",
            onDidChangeTextEditorOptions:
                "handleOnDidChangeTextEditorOptions",
            onDidChangeTextEditorVisibleRanges:
                "handleOnDidChangeTextEditorVisibleRanges",
            onDidCreateFiles: "handleOnDidCreateFiles",
            onDidRenameFiles: "handleOnDidRenameFiles",
            onDidDeleteFiles: "handleOnDidDeleteFiles",
            onDidChangeTabs: "handleOnDidChangeTabs",
        };

        this.activeEventTypes = new Set(
            Object.entries(handlers)
                .filter(([, handlerName]) => script.includes(handlerName))
                .map(([eventType]) => eventType),
        );
    }

    async resetDocument(document, contents) {
        const fullRange = new vscode.Range(
            document.positionAt(0),
            document.positionAt(document.getText().length),
        );
        const edit = new vscode.WorkspaceEdit();
        edit.replace(document.uri, fullRange, contents);
        await vscode.workspace.applyEdit(edit);
        await document.save();
    }

    findTabsForUri(uriString) {
        const tabs = [];

        for (const group of vscode.window.tabGroups.all) {
            for (const tab of group.tabs) {
                const uri = tabUri(tab);
                if (uri?.toString() === uriString) {
                    tabs.push(tab);
                }
            }
        }

        return tabs;
    }

    findTabsInsideRoot(rootUri) {
        const tabs = [];

        for (const group of vscode.window.tabGroups.all) {
            for (const tab of group.tabs) {
                const uri = tabUri(tab);
                if (uri && this.isUriInside(rootUri, uri)) {
                    tabs.push(tab);
                }
            }
        }

        return tabs;
    }

    async closeTabs(tabs, includeDirty = false) {
        const closable = tabs.filter(tab => includeDirty || !tab.isDirty);
        if (closable.length === 0) {
            return;
        }

        await vscode.window.tabGroups.close(closable, true);
    }

    async openStepDocument(key, step, restart) {
        this.activeTutorialRootUri = undefined;
        this.activeWorkspaceFixturePath = undefined;
        this.activeWorkspaceSharedKey = undefined;

        if (!step.file) {
            this.activeTutorialDocumentUri = undefined;
            this.activeTutorialOriginalContents = undefined;
            return { document: undefined, editor: undefined };
        }

        const sourcePath = vscode.Uri.joinPath(
            this.context.extensionUri,
            step.file,
        ).fsPath;

        if (!fs.existsSync(sourcePath)) {
            throw new Error(`Tutorial source file not found: ${step.file}`);
        }

        const contents = fs.readFileSync(sourcePath, "utf8");
        const stepDir = path.join(tutorialDir, "steps", key);
        const workingPath = path.join(stepDir, path.basename(sourcePath));
        fs.mkdirSync(stepDir, { recursive: true });

        if (!fs.existsSync(workingPath)) {
            fs.writeFileSync(workingPath, contents, "utf8");
        }

        let document = await vscode.workspace.openTextDocument(workingPath);
        this.activeTutorialDocumentUri = document.uri.toString();
        this.activeTutorialOriginalContents = contents;

        if (restart) {
            await this.resetDocument(document, contents);
            document = await vscode.workspace.openTextDocument(workingPath);
        }

        const editor = await vscode.window.showTextDocument(document, {
            preview: false,
        });

        if (step.cursor) {
            const position = new vscode.Position(
                step.cursor[0] - 1,
                step.cursor[1] - 1,
            );
            editor.selection = new vscode.Selection(position, position);
            editor.revealRange(
                new vscode.Range(position, position),
                step.cursor[2] === "top"
                    ? vscode.TextEditorRevealType.AtTop
                    : vscode.TextEditorRevealType.InCenter,
            );
        }

        if (step.scrollY) {
            const position = new vscode.Position(step.scrollY - 1, 0);
            editor.revealRange(
                new vscode.Range(position, position),
                vscode.TextEditorRevealType.AtTop,
            );
        }

        return { document, editor };
    }

    async uriExists(uri) {
        try {
            await vscode.workspace.fs.stat(uri);
            return true;
        } catch (error) {
            if (error instanceof vscode.FileSystemError &&
                error.code === "FileNotFound") {
                return false;
            }
            throw error;
        }
    }

    async copyFixtureDirectory(sourcePath, targetUri) {
        await vscode.workspace.fs.createDirectory(targetUri);

        for (const entry of fs.readdirSync(sourcePath, {
            withFileTypes: true,
        })) {
            const sourceEntry = path.join(sourcePath, entry.name);
            const targetEntry = vscode.Uri.joinPath(targetUri, entry.name);

            if (entry.isDirectory()) {
                await this.copyFixtureDirectory(sourceEntry, targetEntry);
            } else if (entry.isFile()) {
                await vscode.workspace.fs.writeFile(
                    targetEntry,
                    fs.readFileSync(sourceEntry),
                );
            }
        }
    }

    async clearDirectoryContents(rootUri) {
        await vscode.workspace.fs.createDirectory(rootUri);

        for (const [name] of await vscode.workspace.fs.readDirectory(
            rootUri,
        )) {
            await vscode.workspace.fs.delete(
                vscode.Uri.joinPath(rootUri, name),
                {
                    recursive: true,
                    useTrash: false,
                },
            );
        }
    }

    async copyUriDirectory(sourceUri, targetUri) {
        await vscode.workspace.fs.createDirectory(targetUri);

        for (const [name, type] of
            await vscode.workspace.fs.readDirectory(sourceUri)) {
            const sourceEntry = vscode.Uri.joinPath(sourceUri, name);
            const targetEntry = vscode.Uri.joinPath(targetUri, name);

            if (type === vscode.FileType.Directory) {
                await this.copyUriDirectory(sourceEntry, targetEntry);
            } else if (type === vscode.FileType.File) {
                await vscode.workspace.fs.writeFile(
                    targetEntry,
                    await vscode.workspace.fs.readFile(sourceEntry),
                );
            }
        }
    }

    workspaceStepStateUri(key) {
        return vscode.Uri.file(
            path.join(workspaceStepStatePath, key),
        );
    }

    sharedWorkspaceStateKey(sharedKey, version = 1) {
        return [
            "hackschuleKeyboardTutorial.sharedWorkspace",
            sharedKey,
            version,
        ].join(".");
    }

    async copyFixtureEntry(sourcePath, targetUri) {
        if (!fs.existsSync(sourcePath)) {
            return;
        }

        const stat = fs.statSync(sourcePath);
        if (stat.isDirectory()) {
            await this.copyFixtureDirectory(sourcePath, targetUri);
        } else if (stat.isFile()) {
            await vscode.workspace.fs.writeFile(
                targetUri,
                fs.readFileSync(sourcePath),
            );
        }
    }

    async resetSharedWorkspacePaths(rootUri, fixturePath, paths) {
        for (const relativePath of paths) {
            const targetUri = vscode.Uri.joinPath(
                rootUri,
                ...relativePath.split("/"),
            );

            if (await this.uriExists(targetUri)) {
                await vscode.workspace.fs.delete(targetUri, {
                    recursive: true,
                    useTrash: false,
                });
            }

            const sourcePath = path.join(
                fixturePath,
                ...relativePath.split("/"),
            );
            await this.copyFixtureEntry(sourcePath, targetUri);
        }
    }

    async ensureSharedWorkspaceContents(
        rootUri,
        fixturePath,
        sharedKey,
        version,
    ) {
        const stateKey = this.sharedWorkspaceStateKey(
            sharedKey,
            version,
        );
        const initialized = this.context.globalState.get(
            stateKey,
            false,
        );
        const sentinelExists = await this.uriExists(
            vscode.Uri.joinPath(rootUri, "willkommen.txt"),
        );

        if (initialized && sentinelExists) {
            return;
        }

        await this.closeTabs(
            this.findTabsInsideRoot(rootUri),
            true,
        );
        await this.clearDirectoryContents(rootUri);
        await this.copyFixtureDirectory(fixturePath, rootUri);

        const obsoleteStepStates = vscode.Uri.file(
            workspaceStepStatePath,
        );
        if (await this.uriExists(obsoleteStepStates)) {
            await vscode.workspace.fs.delete(obsoleteStepStates, {
                recursive: true,
                useTrash: false,
            });
        }

        await this.context.globalState.update(stateKey, true);
    }

    async removeWorkspaceStepState(key) {
        const stateUri = this.workspaceStepStateUri(key);
        if (await this.uriExists(stateUri)) {
            await vscode.workspace.fs.delete(stateUri, {
                recursive: true,
                useTrash: false,
            });
        }
    }

    async saveDirtyDocumentsInside(rootUri) {
        for (const document of vscode.workspace.textDocuments) {
            if (document.isDirty &&
                this.isUriInside(rootUri, document.uri)) {
                await document.save();
            }
        }
    }

    async storeWorkspaceStepState(key, rootUri) {
        await this.saveDirtyDocumentsInside(rootUri);

        const stateUri = this.workspaceStepStateUri(key);
        if (await this.uriExists(stateUri)) {
            await vscode.workspace.fs.delete(stateUri, {
                recursive: true,
                useTrash: false,
            });
        }

        await this.copyUriDirectory(rootUri, stateUri);
    }

    async resetWorkspaceStep(rootUri, fixturePath, key, restart) {
        await this.closeTabs(
            this.findTabsInsideRoot(rootUri),
            true,
        );

        await this.clearDirectoryContents(rootUri);

        const stateUri = this.workspaceStepStateUri(key);
        const useSavedState =
            !restart && await this.uriExists(stateUri);

        if (useSavedState) {
            await this.copyUriDirectory(stateUri, rootUri);
        } else {
            if (restart) {
                await this.removeWorkspaceStepState(key);
            }
            await this.copyFixtureDirectory(fixturePath, rootUri);
        }
    }

    async ensureManagedWorkspace(key) {
        const managedWorkspaceUri =
            vscode.Uri.file(managedWorkspacePath);

        const workspaceFolders =
            vscode.workspace.workspaceFolders ?? [];

        const managedWorkspaceIsOpen =
            workspaceFolders.length === 1 &&
            workspaceFolders[0].uri.toString() ===
                managedWorkspaceUri.toString();

        if (managedWorkspaceIsOpen) {
            return {
                workspaceFolder: workspaceFolders[0],
                reopeningWorkspace: false,
            };
        }

        /*
         * File-management exercises should never modify a student's
         * currently open project. Use one isolated tutorial workspace for
         * the whole chapter. Opening the folder restarts the extension host,
         * so remember which step should resume afterwards.
         */
        await vscode.workspace.fs.createDirectory(
            managedWorkspaceUri,
        );

        await this.context.globalState.update(
            pendingWorkspaceStepStateKey,
            {
                key,
                workspaceUri: managedWorkspaceUri.toString(),
            },
        );

        try {
            await vscode.commands.executeCommand(
                "vscode.openFolder",
                managedWorkspaceUri,
                {
                    forceReuseWindow: true,
                    noRecentEntry: true,
                },
            );
        } catch (error) {
            await this.context.globalState.update(
                pendingWorkspaceStepStateKey,
                undefined,
            );
            throw error;
        }

        return {
            workspaceFolder: undefined,
            reopeningWorkspace: true,
        };
    }

    async openWorkspaceTransitionStep(key) {
        const workspaceResult =
            await this.ensureManagedWorkspace(key);

        if (workspaceResult.reopeningWorkspace) {
            return {
                document: undefined,
                editor: undefined,
                reopeningWorkspace: true,
            };
        }

        this.activeTutorialDocumentUri = undefined;
        this.activeTutorialOriginalContents = undefined;
        this.activeTutorialRootUri = undefined;
        this.activeWorkspaceFixturePath = undefined;
        this.activeWorkspaceStepKey = undefined;
        this.activeWorkspaceSharedKey = undefined;

        return {
            document: undefined,
            editor: undefined,
        };
    }

    async openWorkspaceStep(key, step, restart) {
        const workspaceResult =
            await this.ensureManagedWorkspace(key);

        if (workspaceResult.reopeningWorkspace) {
            return {
                document: undefined,
                editor: undefined,
                reopeningWorkspace: true,
            };
        }

        const workspaceFolder =
            workspaceResult.workspaceFolder;

        const fixturePath = vscode.Uri.joinPath(
            this.context.extensionUri,
            step.workspace,
        ).fsPath;

        if (!fs.existsSync(fixturePath) ||
            !fs.statSync(fixturePath).isDirectory()) {
            throw new Error(
                `Tutorial workspace fixture not found: ${step.workspace}`,
            );
        }

        const rootUri = workspaceFolder.uri;
        const sharedKey = step.sharedWorkspace;
        const sharedVersion = step.sharedWorkspaceVersion ?? 1;

        const sameStepStillLoaded =
            this.activeWorkspaceStepKey === key &&
            this.activeTutorialRootUri?.toString() ===
                rootUri.toString() &&
            this.activeWorkspaceFixturePath === fixturePath;

        this.activeTutorialDocumentUri = undefined;
        this.activeTutorialOriginalContents = undefined;
        this.activeTutorialRootUri = rootUri;
        this.activeWorkspaceFixturePath = fixturePath;
        this.activeWorkspaceStepKey = key;
        this.activeWorkspaceSharedKey = sharedKey;

        if (sharedKey) {
            await this.ensureSharedWorkspaceContents(
                rootUri,
                fixturePath,
                sharedKey,
                sharedVersion,
            );

            if (restart) {
                await this.closeTabs(
                    this.findTabsInsideRoot(rootUri),
                    true,
                );

                if (step.resetSharedWorkspace === true) {
                    await this.clearDirectoryContents(rootUri);
                    await this.copyFixtureDirectory(
                        fixturePath,
                        rootUri,
                    );
                } else {
                    const resetPaths = Array.isArray(step.resetPaths)
                        ? step.resetPaths
                        : [];
                    await this.resetSharedWorkspacePaths(
                        rootUri,
                        fixturePath,
                        resetPaths,
                    );
                }
            }
        } else if (restart || !sameStepStillLoaded) {
            await this.resetWorkspaceStep(
                rootUri,
                fixturePath,
                key,
                restart,
            );
        }

        let document;
        let editor;

        const openFiles = Array.isArray(step.openFiles)
            ? step.openFiles
            : [];

        for (const relativePath of openFiles) {
            const uri = vscode.Uri.joinPath(rootUri, relativePath);
            document = await vscode.workspace.openTextDocument(uri);
            editor = await vscode.window.showTextDocument(document, {
                preview: false,
            });
        }

        if (step.activeFile) {
            const activeUri = vscode.Uri.joinPath(rootUri, step.activeFile);
            document = await vscode.workspace.openTextDocument(activeUri);
            editor = await vscode.window.showTextDocument(document, {
                preview: false,
            });
        }

        if (document) {
            this.activeTutorialDocumentUri = document.uri.toString();
        }

        return { document, editor };
    }

    async cleanUpStepBeforeLeaving(nextStepKey) {
        if (!this.currentStepKey || this.currentStepKey === nextStepKey) {
            return;
        }

        const state = this.readCompletionState();
        const completed = state[this.currentStepKey] === true;

        if (this.activeTutorialRootUri) {
            const previousStepKey = this.currentStepKey;

            if (this.activeWorkspaceSharedKey) {
                /*
                 * Shared chapter workspaces stay in place while the learner
                 * moves between exercises. Close finished exercise tabs, but
                 * do not replace the Explorer contents with another fixture.
                 */
                await this.closeTabs(
                    this.findTabsInsideRoot(this.activeTutorialRootUri),
                    completed,
                );
            } else {
                if (completed) {
                    await this.removeWorkspaceStepState(previousStepKey);
                } else {
                    await this.storeWorkspaceStepState(
                        previousStepKey,
                        this.activeTutorialRootUri,
                    );
                }

                await this.closeTabs(
                    this.findTabsInsideRoot(this.activeTutorialRootUri),
                    true,
                );
            }

            this.activeTutorialRootUri = undefined;
            this.activeWorkspaceFixturePath = undefined;
            this.activeWorkspaceStepKey = undefined;
            this.activeWorkspaceSharedKey = undefined;
            this.activeTutorialDocumentUri = undefined;
            this.activeTutorialOriginalContents = undefined;
            return;
        }

        if (!this.activeTutorialDocumentUri) {
            return;
        }

        const previousUri = this.activeTutorialDocumentUri;
        const document = vscode.workspace.textDocuments.find(
            candidate => candidate.uri.toString() === previousUri,
        );

        if (completed && document &&
            this.activeTutorialOriginalContents !== undefined) {
            await this.resetDocument(
                document,
                this.activeTutorialOriginalContents,
            );
            await this.closeTabs(this.findTabsForUri(previousUri), true);
        } else if (document && !document.isDirty) {
            await this.closeTabs(this.findTabsForUri(previousUri), false);
        }

        this.activeTutorialDocumentUri = undefined;
        this.activeTutorialOriginalContents = undefined;
    }

    createSnapshot(document, editor) {
        if (!document || !editor) {
            return undefined;
        }

        return {
            document: serializeDocument(document),
            contents: document.getText(),
            selections: editor.selections.map(serializeSelection),
            visibleRanges: editor.visibleRanges.map(serializeRange),
        };
    }

    async runTutorialAction(action) {
        if (action === "moveTutorialView") {
            /*
             * A click inside a webview does not necessarily set VS Code's
             * focusedView context key. Pass the view id directly instead of
             * relying on focus, otherwise Move Focused View can report that
             * no view is focused.
             */
            await vscode.commands.executeCommand(
                "workbench.action.moveFocusedView",
                "typingSteps",
            );
            return;
        }

        if (action === "resetTutorialView") {
            /*
             * Use the same explicit view id on the way back. The learner
             * chooses the existing Hackschule container in the primary Side
             * Bar. This avoids relying on the fragile focusedView context.
             */
            await vscode.commands.executeCommand(
                "workbench.action.moveFocusedView",
                "typingSteps",
            );
            return;
        }

        if (action === "showExplorer") {
            await vscode.commands.executeCommand(
                "workbench.view.explorer",
            );
            return;
        }

        throw new Error(
            `Unknown tutorial action: ${action}`,
        );
    }

    async loadStep(message, webview) {
        const key = message.key;

        // Do not forward setup and cleanup events to the new exercise.
        this.activeEventTypes = new Set();
        await this.cleanUpStepBeforeLeaving(key);

        const step = this.parseStep(key, webview);
        const state = this.readCompletionState();
        this.currentStepKey = key;

        const shouldReset = step.sharedWorkspace
            ? message.restart === true
            : message.restart === true || state[key] === true;

        const openResult = step.requiresWorkspace
            ? await this.openWorkspaceTransitionStep(key)
            : step.workspace
                ? await this.openWorkspaceStep(
                    key,
                    step,
                    shouldReset,
                )
                : await this.openStepDocument(
                    key,
                    step,
                    shouldReset,
                );

        /*
         * vscode.openFolder() restarts the extension host. Do not render an
         * error or an empty exercise while the window is switching.
         */
        if (openResult.reopeningWorkspace) {
            return;
        }

        const { document, editor } = openResult;

        this.updateActiveEventTypes(step.script);
        const snapshot = this.createSnapshot(document, editor);

        this.postMessage({
            command: "load_step_return",
            key,
            step,
            state,
            snapshot,
            workspaceRoot: this.activeTutorialRootUri
                ? this.serializeTutorialUri(this.activeTutorialRootUri)
                : undefined,
        });
    }

    resolveWebviewView(webviewView) {
        this.webviewView = webviewView;

        webviewView.webview.options = {
            enableScripts: true,
            localResourceRoots: [this.context.extensionUri],
        };

        const htmlPath = vscode.Uri.joinPath(
            this.context.extensionUri,
            "media",
            "index.html",
        ).fsPath;
        let html = fs.readFileSync(htmlPath, "utf8");

        html = html.replace(
            "styles.css",
            webviewView.webview.asWebviewUri(
                vscode.Uri.joinPath(
                    this.context.extensionUri,
                    "media",
                    "styles.css",
                ),
            ),
        );
        html = html.replace(
            "script.js",
            webviewView.webview.asWebviewUri(
                vscode.Uri.joinPath(
                    this.context.extensionUri,
                    "media",
                    "script.js",
                ),
            ),
        );
        html = html.replace('"__SECTIONS__"', JSON.stringify(this.sections));
        webviewView.webview.html = html;

        webviewView.onDidDispose(() => {
            if (this.webviewView === webviewView) {
                this.webviewView = undefined;
            }
        });

        webviewView.webview.onDidReceiveMessage(async message => {
            try {
                if (message.command === "load_step") {
                    this.loadQueue = this.loadQueue.then(
                        () => this.loadStep(message, webviewView.webview),
                        () => this.loadStep(message, webviewView.webview),
                    );
                    await this.loadQueue;
                } else if (message.command === "mark_step_complete") {
                    this.markStepComplete(message.step);
                    this.postMessage({
                        command: "update_state",
                        state: this.readCompletionState(),
                    });
                } else if (message.command === "run_tutorial_action") {
                    if (message.completeStep) {
                        this.markStepComplete(message.completeStep);
                        this.postMessage({
                            command: "update_state",
                            state: this.readCompletionState(),
                        });
                    }
                    await this.runTutorialAction(message.action);
                } else if (message.command === "ready") {
                    const state = this.readCompletionState();
                    this.postMessage({ command: "update_state", state });

                    const pending =
                        this.context.globalState.get(
                            pendingWorkspaceStepStateKey,
                        );

                    const currentWorkspaceUri =
                        vscode.workspace.workspaceFolders?.[0]
                            ?.uri.toString();

                    let requestedStepKey;

                    if (
                        pending &&
                        pending.workspaceUri === currentWorkspaceUri
                    ) {
                        requestedStepKey = pending.key;
                        await this.context.globalState.update(
                            pendingWorkspaceStepStateKey,
                            undefined,
                        );
                    } else if (this.currentStepKey) {
                        /*
                         * Moving a view between sidebars can recreate its
                         * webview. Resume the step that was already open
                         * instead of jumping to the first incomplete step.
                         */
                        requestedStepKey = this.currentStepKey;
                    }

                    let firstStep = 0;
                    let index = 0;
                    let foundStep = false;

                    for (const section of this.sections.sections) {
                        for (const step of section.steps) {
                            const isRequestedStep =
                                requestedStepKey &&
                                step.key === requestedStepKey;

                            const isFirstIncompleteStep =
                                !requestedStepKey &&
                                !state[step.key];

                            if (
                                isRequestedStep ||
                                isFirstIncompleteStep
                            ) {
                                firstStep = index;
                                foundStep = true;
                                break;
                            }

                            index += 1;
                        }

                        if (foundStep) {
                            break;
                        }
                    }

                    this.postMessage({
                        command: "click_step",
                        step: firstStep,
                    });
                }
            } catch (error) {
                console.error(error);
                this.postMessage({
                    command: "show_error",
                    message: error instanceof Error
                        ? error.message
                        : String(error),
                });
            }
        });
    }
}

TutorialViewProvider.serializeDocument = serializeDocument;
TutorialViewProvider.serializePosition = serializePosition;
TutorialViewProvider.serializeRange = serializeRange;
TutorialViewProvider.serializeSelection = serializeSelection;
TutorialViewProvider.tabUri = tabUri;

module.exports = TutorialViewProvider;
