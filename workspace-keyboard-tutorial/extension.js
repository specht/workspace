const vscode = require("vscode");
const path = require("path");
const fs = require("fs");
const TutorialViewProvider = require("./src/TutorialViewProvider");

class StepContentProvider {
    provideTextDocumentContent(uri) {
        return "Type this and comment it using the keyboard shortcut.";
    }
}

async function openStepAsUntitled(content, language = "plaintext") {
    const doc = await vscode.workspace.openTextDocument({
        content,
        language, // e.g. 'plaintext', 'javascript', 'markdown'
    });

    await vscode.window.showTextDocument(doc, vscode.ViewColumn.One);
    const uri = doc.uri;
    console.log('URI:', uri);
    return uri;
}

async function openFirstStepFile(context) {
    const fileUri = vscode.Uri.joinPath(context.extensionUri, "tutorial", "step-01.js");
    const content = fs.readFileSync(fileUri.fsPath, "utf-8");

    return openStepAsUntitled(content, 'javascript');
}

function watchUserActivity(context) {
    context.subscriptions.push(
        vscode.workspace.onDidChangeTextDocument((event) => {
            console.log("User edited the text:", event.contentChanges);
        }),

        vscode.window.onDidChangeTextEditorSelection((event) => {
            const pos = event.selections[0].active;
            console.log(`Cursor moved to line ${pos.line}, char ${pos.character}`);
        })
    );
}


async function activate(context) {
    const provider = new TutorialViewProvider(context);

    context.subscriptions.push(
        vscode.window.registerWebviewViewProvider("typingSteps", provider)
    );

    console.log("Extension activated");
    // let uri = await openFirstStepFile(context);

    const doc = await vscode.workspace.openTextDocument({
        language: "text",
        content: "Alice was beginning to get very tired of sitting by her sister on the bank, and of having nothing to do: once or twice she had peeped into the book her sister was reading, but it had no pictures or conversations in it, “and what is the use of a book,” thought Alice “without pictures or conversations?\n\nSo she was considering in her own mind (as well as she could, for the hot day made her feel very sleepy and stupid), whether the pleasure of making a daisy-chain would be worth the trouble of getting up and picking the daisies, when suddenly a White Rabbit with pink eyes ran close by her.\n\nThere was nothing so very remarkable in that; nor did Alice think it so very much out of the way to hear the Rabbit say to itself, “Oh dear! Oh dear! I shall be late!” (when she thought it over afterwards, it occurred to her that she ought to have wondered at this, but at the time it all seemed quite natural); but when the Rabbit actually took a watch out of its waistcoat-pocket, and looked at it, and then hurried on, Alice started to her feet, for it flashed across her mind that she had never before seen a rabbit with either a waistcoat-pocket, or a watch to take out of it, and burning with curiosity, she ran across the field after it, and fortunately was just in time to see it pop down a large rabbit-hole under the hedge.\n\nIn another moment down went Alice after it, never once considering how in the world she was to get out again.\n\nThe rabbit-hole went straight on like a tunnel for some way, and then dipped suddenly down, so suddenly that Alice had not a moment to think about stopping herself before she found herself falling down a very deep well.\n\n"
    });
    await vscode.window.showTextDocument(doc, { preview: false });

    watchUserActivity(context);

// const editor = vscode.window.activeTextEditor;
// if (editor && editor.document.isUntitled) {
//   // Revert the document silently
//   await vscode.commands.executeCommand("workbench.action.revertAndCloseActiveEditor");
// }    

    // for (const group of vscode.window.tabGroups.all) {
    //     console.log('Group', group);
    //     for (const tab of group.tabs) {
    //         console.log('Tab', tab);
    //         if (tab.input && tab.input.uri.toString() === uri.toString()) {
    //             await vscode.window.tabGroups.close(tab);
    //         }
    //     }
    // }
}

function deactivate() { }

module.exports = { activate, deactivate };
