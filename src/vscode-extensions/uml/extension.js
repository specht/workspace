// UML-Werkstatt for VS Code: .uml files get highlighting, problems while
// typing, the class diagram in a preview beside the editor and a command
// that saves the diagram as SVG next to the file.
'use strict';

const vscode = require('vscode');
const path = require('path');
const core = require('./core/uml-core.js');
const lib = require('./lib.js');

function activate(context) {
    const diagnostics = vscode.languages.createDiagnosticCollection('uml');
    context.subscriptions.push(diagnostics);
    const previews = new Map();

    const lint = (doc) => {
        if (doc.languageId !== 'uml') return;
        const model = core.compile(doc.getText());
        const toDiagnostic = (p, severity) => {
            const r = lib.problemRange(p, doc.lineCount, (i) => doc.lineAt(i).text.length);
            const d = new vscode.Diagnostic(new vscode.Range(r.line, r.from, r.line, r.to), p.msg, severity);
            d.source = 'UML-Werkstatt';
            return d;
        };
        diagnostics.set(doc.uri, [
            ...model.errors.map((p) => toDiagnostic(p, vscode.DiagnosticSeverity.Error)),
            ...model.warnings.map((p) => toDiagnostic(p, vscode.DiagnosticSeverity.Warning)),
        ]);
    };

    vscode.workspace.textDocuments.forEach(lint);
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(lint),
        vscode.workspace.onDidChangeTextDocument((e) => {
            lint(e.document);
            const preview = previews.get(e.document.uri.toString());
            if (preview) preview.scheduleUpdate();
        }),
        vscode.workspace.onDidCloseTextDocument((doc) => diagnostics.delete(doc.uri)),
    );

    const targetUri = async (uri) => {
        if (uri instanceof vscode.Uri) return uri;
        const editor = vscode.window.activeTextEditor;
        if (editor && editor.document.languageId === 'uml') return editor.document.uri;
        vscode.window.showInformationMessage('Öffne zuerst eine .uml-Datei.');
        return null;
    };

    context.subscriptions.push(
        vscode.commands.registerCommand('hackschuleUml.showPreview', async (uri) => {
            const target = await targetUri(uri);
            if (!target) return;
            const key = target.toString();
            if (previews.has(key)) {
                previews.get(key).panel.reveal(vscode.ViewColumn.Beside, true);
                return;
            }
            const doc = await vscode.workspace.openTextDocument(target);
            const preview = new Preview(context, doc, () => previews.delete(key));
            previews.set(key, preview);
        }),
        vscode.commands.registerCommand('hackschuleUml.saveDiagram', async (uri) => {
            const target = await targetUri(uri);
            if (target) await saveDiagram(target);
        }),
    );
}

async function readText(uri) {
    try {
        return Buffer.from(await vscode.workspace.fs.readFile(uri)).toString('utf8');
    } catch (e) {
        return null;
    }
}

async function writeText(uri, text) {
    await vscode.workspace.fs.writeFile(uri, Buffer.from(text, 'utf8'));
}

async function modelText(uri) {
    const open = vscode.workspace.textDocuments.find((d) => d.uri.toString() === uri.toString());
    if (open) return open.getText();
    return (await readText(uri)) || '';
}

function showProblems() {
    vscode.window.showErrorMessage('Das Diagramm hat noch Fehler. Sie stehen im Bereich »Probleme« und sind im Text rot unterstrichen.', 'Probleme anzeigen')
        .then((choice) => { if (choice) vscode.commands.executeCommand('workbench.actions.view.problems'); });
}

async function saveDiagram(uri) {
    const text = await modelText(uri);
    const model = core.compile(text);
    if (model.errors.length) return showProblems();
    if (model.classes.length === 0) {
        vscode.window.showInformationMessage('Im Diagramm steht noch keine Klasse.');
        return;
    }
    const target = vscode.Uri.file(lib.svgPath(uri.fsPath));
    const name = path.basename(target.fsPath);
    const existing = await readText(target);
    if (existing !== null && !lib.isGenerated(existing)) {
        const choice = await vscode.window.showWarningMessage(
            `${name} gibt es schon und wurde nicht von der UML-Werkstatt erzeugt. Überschreiben?`, { modal: true }, 'Überschreiben');
        if (choice !== 'Überschreiben') return;
    }
    const { svg } = core.diagram(text, { background: '#ffffff' });
    await writeText(target, svg + '\n');
    const choice = await vscode.window.showInformationMessage(`${name} gespeichert.`, 'Im Explorer zeigen');
    if (choice) vscode.commands.executeCommand('revealInExplorer', target);
}

class Preview {
    constructor(context, document, onDispose) {
        this.document = document;
        this.uri = document.uri;
        this.timer = null;
        const root = context.extensionUri;
        this.panel = vscode.window.createWebviewPanel('hackschuleUml.preview', `UML: ${path.basename(this.uri.fsPath)}`,
            { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
            { enableScripts: true, retainContextWhenHidden: true,
              localResourceRoots: [vscode.Uri.joinPath(root, 'media'), vscode.Uri.joinPath(root, 'core')] });
        this.panel.iconPath = vscode.Uri.joinPath(root, 'media', 'icon.svg');
        const webview = this.panel.webview;
        const nonce = [...Array(24)].map(() => Math.random().toString(36)[2]).join('');
        const src = (...p) => webview.asWebviewUri(vscode.Uri.joinPath(root, ...p));
        webview.html = `<!doctype html>
<html lang="de"><head><meta charset="utf-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; img-src ${webview.cspSource} data:; style-src ${webview.cspSource} 'unsafe-inline'; script-src 'nonce-${nonce}';">
<meta name="viewport" content="width=device-width, initial-scale=1">
<link rel="stylesheet" href="${src('media', 'preview.css')}">
<title>Klassendiagramm</title></head>
<body>
<div class="bar">
  <div class="seg zoom" role="group" aria-label="Zoom">
    <button type="button" data-zoom="out" title="Verkleinern (Strg + Mausrad)" aria-label="Verkleinern">−</button><button type="button" data-zoom="fit" title="Das ganze Diagramm zeigen">Einpassen</button><button type="button" data-zoom="in" title="Vergrößern (Strg + Mausrad)" aria-label="Vergrößern">+</button>
  </div>
  <span id="zoom-level" class="zoom-level" aria-live="polite"></span>
  <span class="spacer"></span>
  <button type="button" class="act" data-act="svg" title="Das Diagramm als SVG-Datei neben der .uml-Datei speichern">Als SVG speichern</button>
</div>
<div id="problems" class="problems" hidden></div>
<div id="board" class="board"></div>
<p class="hint" id="hint">Oben der Name der Klasse (kursiv mit «abstract»: abstrakt), darunter die Attribute, dann Konstruktoren und Methoden; + public, - private. Hohles Dreieck: erbt von. Linie: Assoziation, mit Pfeil nur in diese Richtung navigierbar; an den Enden die Multiplizitäten.</p>
<script nonce="${nonce}" src="${src('core', 'uml-core.js')}"></script>
<script nonce="${nonce}" src="${src('media', 'preview.js')}"></script>
</body></html>`;

        webview.onDidReceiveMessage((msg) => this.receive(msg));
        this.panel.onDidDispose(() => {
            clearTimeout(this.timer);
            onDispose();
        });
        this.update();
    }

    scheduleUpdate() {
        clearTimeout(this.timer);
        this.timer = setTimeout(() => this.update(), 200);
    }

    update() {
        this.panel.webview.postMessage({ type: 'model', text: this.document.getText(), file: path.basename(this.uri.fsPath) });
    }

    receive(msg) {
        if (!msg || typeof msg !== 'object') return;
        if (msg.type === 'ready') this.update();
        else if (msg.type === 'svg') saveDiagram(this.uri);
        else if (msg.type === 'reveal' && Number.isInteger(msg.line)) {
            vscode.window.showTextDocument(this.document, { viewColumn: vscode.ViewColumn.One }).then((editor) => {
                const line = Math.max(0, Math.min(this.document.lineCount - 1, msg.line - 1));
                const range = this.document.lineAt(line).range;
                editor.selection = new vscode.Selection(range.start, range.end);
                editor.revealRange(range, vscode.TextEditorRevealType.InCenterIfOutsideViewport);
            });
        }
    }
}

function deactivate() {}

module.exports = { activate, deactivate };
