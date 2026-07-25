const vscode = require("vscode");
const fs = require("fs/promises");
const yaml = require("yaml");
const TutorialViewProvider = require("./TutorialViewProvider");

let provider = null;

function serializeChange(change) {
    return {
        range: [
            TutorialViewProvider.serializePosition(change.range.start),
            TutorialViewProvider.serializePosition(change.range.end),
        ],
        rangeOffset: change.rangeOffset,
        rangeLength: change.rangeLength,
        text: change.text,
    };
}

function serializeTab(tab) {
    const uri = TutorialViewProvider.tabUri(tab);

    return {
        label: tab.label,
        isActive: tab.isActive,
        isDirty: tab.isDirty,
        isPinned: tab.isPinned,
        isPreview: tab.isPreview,
        resource: uri && provider
            ? provider.serializeTutorialUri(uri)
            : undefined,
    };
}

function tutorialTabs() {
    if (!provider) {
        return [];
    }

    const tabs = [];
    for (const group of vscode.window.tabGroups.all) {
        for (const tab of group.tabs) {
            const uri = TutorialViewProvider.tabUri(tab);
            if (uri && provider.isActiveTutorialUri(uri)) {
                tabs.push(serializeTab(tab));
            }
        }
    }
    return tabs;
}

function watchUserActivity(context) {
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument(event => {
            if (!provider?.watches("onDidChangeTextDocument") ||
                !provider.isActiveTutorialDocument(event.document)) {
                return;
            }
            provider.postMessage({
                command: "onDidChangeTextDocument",
                event: {
                    document: TutorialViewProvider.serializeDocument(
                        event.document,
                    ),
                    contentChanges: event.contentChanges.map(serializeChange),
                    reason: event.reason,
                },
                contents: event.document.getText(),
            });
        }),

        vscode.workspace.onDidSaveTextDocument(document => {
            if (!provider?.watches("onDidSaveTextDocument") ||
                !provider.isActiveTutorialDocument(document)) {
                return;
            }
            provider.postMessage({
                command: "onDidSaveTextDocument",
                event: {
                    document:
                        TutorialViewProvider.serializeDocument(document),
                },
                contents: document.getText(),
            });
        }),

        vscode.window.onDidChangeTextEditorSelection(event => {
            if (!provider?.watches("onDidChangeTextEditorSelection") ||
                !provider.isActiveTutorialDocument(
                    event.textEditor.document,
                )) {
                return;
            }
            provider.postMessage({
                command: "onDidChangeTextEditorSelection",
                event: {
                    document: TutorialViewProvider.serializeDocument(
                        event.textEditor.document,
                    ),
                    selections: event.selections.map(
                        TutorialViewProvider.serializeSelection,
                    ),
                    kind: event.kind,
                },
            });
        }),

        vscode.window.onDidChangeActiveTextEditor(editor => {
            if (!editor ||
                !provider?.watches("onDidChangeActiveTextEditor") ||
                !provider.isActiveTutorialDocument(editor.document)) {
                return;
            }
            provider.postMessage({
                command: "onDidChangeActiveTextEditor",
                event: {
                    document: TutorialViewProvider.serializeDocument(
                        editor.document,
                    ),
                    resource: provider.serializeTutorialUri(
                        editor.document.uri,
                    ),
                    selections: editor.selections.map(
                        TutorialViewProvider.serializeSelection,
                    ),
                    visibleRanges: editor.visibleRanges.map(
                        TutorialViewProvider.serializeRange,
                    ),
                },
            });
        }),

        vscode.window.onDidChangeTextEditorOptions(event => {
            if (!provider?.watches("onDidChangeTextEditorOptions") ||
                !provider.isActiveTutorialDocument(
                    event.textEditor.document,
                )) {
                return;
            }
            provider.postMessage({
                command: "onDidChangeTextEditorOptions",
                event: {
                    document: TutorialViewProvider.serializeDocument(
                        event.textEditor.document,
                    ),
                    options: event.options,
                },
            });
        }),

        vscode.window.onDidChangeTextEditorVisibleRanges(event => {
            if (!provider?.watches(
                "onDidChangeTextEditorVisibleRanges",
            ) || !provider.isActiveTutorialDocument(
                event.textEditor.document,
            )) {
                return;
            }
            provider.postMessage({
                command: "onDidChangeTextEditorVisibleRanges",
                event: {
                    document: TutorialViewProvider.serializeDocument(
                        event.textEditor.document,
                    ),
                    visibleRanges: event.visibleRanges.map(
                        TutorialViewProvider.serializeRange,
                    ),
                },
            });
        }),

        vscode.workspace.onDidCreateFiles(event => {
            if (!provider?.watches("onDidCreateFiles")) {
                return;
            }

            const files = event.files
                .filter(uri => provider.isActiveTutorialUri(uri))
                .map(uri => provider.serializeTutorialUri(uri));

            if (files.length > 0) {
                provider.postMessage({
                    command: "onDidCreateFiles",
                    event: { files },
                });
            }
        }),

        vscode.workspace.onDidRenameFiles(event => {
            if (!provider?.watches("onDidRenameFiles")) {
                return;
            }

            const files = event.files
                .filter(({ oldUri, newUri }) =>
                    provider.isActiveTutorialUri(oldUri) ||
                    provider.isActiveTutorialUri(newUri),
                )
                .map(({ oldUri, newUri }) => ({
                    oldUri: provider.serializeTutorialUri(oldUri),
                    newUri: provider.serializeTutorialUri(newUri),
                }));

            if (files.length > 0) {
                provider.postMessage({
                    command: "onDidRenameFiles",
                    event: { files },
                });
            }
        }),

        vscode.workspace.onDidDeleteFiles(event => {
            if (!provider?.watches("onDidDeleteFiles")) {
                return;
            }

            const files = event.files
                .filter(uri => provider.isActiveTutorialUri(uri))
                .map(uri => provider.serializeTutorialUri(uri));

            if (files.length > 0) {
                provider.postMessage({
                    command: "onDidDeleteFiles",
                    event: { files },
                });
            }
        }),

        vscode.window.tabGroups.onDidChangeTabs(event => {
            if (!provider?.watches("onDidChangeTabs")) {
                return;
            }

            const related = tab => {
                const uri = TutorialViewProvider.tabUri(tab);
                return uri && provider.isActiveTutorialUri(uri);
            };

            const opened = event.opened.filter(related).map(serializeTab);
            const closed = event.closed.filter(related).map(serializeTab);
            const changed = event.changed.filter(related).map(serializeTab);

            if (opened.length || closed.length || changed.length) {
                provider.postMessage({
                    command: "onDidChangeTabs",
                    event: {
                        opened,
                        closed,
                        changed,
                        tabs: tutorialTabs(),
                    },
                });
            }
        }),
    );
}

async function activate(context) {
    const file = await fs.readFile(
        vscode.Uri.joinPath(
            context.extensionUri,
            "tutorial",
            "sections.yaml",
        ).fsPath,
        "utf8",
    );
    const sections = yaml.parse(file);
    provider = new TutorialViewProvider(context, sections);

    context.subscriptions.push(
        vscode.window.registerWebviewViewProvider(
            "typingSteps",
            provider,
        ),
    );

    watchUserActivity(context);
    console.log("Hackschule Keyboard Tutorial activated");
}

function deactivate() { }

module.exports = { activate, deactivate };
