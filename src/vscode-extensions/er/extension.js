// ER-Werkstatt for VS Code: .er files get highlighting, problems while
// typing, a diagram preview beside the editor (Chen, crow's foot, tables)
// and two commands that write files next to the model: the MySQL schema
// and the diagram as SVG. Nothing is run against a database: students run
// `mysql < film.sql` themselves.
'use strict';

const vscode = require('vscode');
const path = require('path');
const core = require('./core/er-core.js');
const lib = require('./lib.js');

const KIND_LABELS = { chen: 'Chen', crowfoot: 'Krähenfuß', tables: 'Tabellen' };

function activate(context) {
    const diagnostics = vscode.languages.createDiagnosticCollection('er');
    context.subscriptions.push(diagnostics);
    const previews = new Map();

    const lint = (doc) => {
        if (doc.languageId !== 'er') return;
        const { model, result } = core.compile(doc.getText());
        const toDiagnostic = (p, severity) => {
            const line = Math.max(0, Math.min(doc.lineCount - 1, (p.line || 1) - 1));
            const length = doc.lineAt(line).text.length;
            const from = Math.min(p.from || 0, length);
            const to = Math.max(from + 1, Math.min(p.to || length, length));
            const d = new vscode.Diagnostic(new vscode.Range(line, from, line, to), p.msg, severity);
            d.source = 'ER-Werkstatt';
            return d;
        };
        diagnostics.set(doc.uri, [
            ...model.errors.map((p) => toDiagnostic(p, vscode.DiagnosticSeverity.Error)),
            ...model.warnings.concat(result.warnings).map((p) => toDiagnostic(p, vscode.DiagnosticSeverity.Warning)),
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
        if (editor && editor.document.languageId === 'er') return editor.document.uri;
        vscode.window.showInformationMessage('Öffne zuerst eine .er-Datei.');
        return null;
    };

    context.subscriptions.push(
        vscode.commands.registerCommand('hackschuleEr.showPreview', async (uri) => {
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
        vscode.commands.registerCommand('hackschuleEr.generateSql', async (uri) => {
            const target = await targetUri(uri);
            if (target) await generateSql(target);
        }),
        vscode.commands.registerCommand('hackschuleEr.saveDiagram', async (uri, kind) => {
            const target = await targetUri(uri);
            if (target) await saveDiagram(target, kind);
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

async function readLayout(uri) {
    return lib.readLayout(await readText(vscode.Uri.file(lib.layoutPath(uri.fsPath))));
}

function showProblems() {
    vscode.window.showErrorMessage('Das Modell hat noch Fehler. Sie stehen im Bereich »Probleme« und sind im Text rot unterstrichen.', 'Probleme anzeigen')
        .then((choice) => { if (choice) vscode.commands.executeCommand('workbench.actions.view.problems'); });
}

async function generateSql(uri) {
    const { model, result } = core.compile(await modelText(uri));
    if (model.errors.length) return showProblems();

    const target = vscode.Uri.file(lib.sqlPath(uri.fsPath));
    const name = path.basename(target.fsPath);
    const existing = await readText(target);
    if (existing !== null && !lib.isGenerated(existing)) {
        const choice = await vscode.window.showWarningMessage(
            `${name} gibt es schon und wurde nicht von der ER-Werkstatt erzeugt. Überschreiben?`, { modal: true }, 'Überschreiben');
        if (choice !== 'Überschreiben') return;
    }
    await writeText(target, core.toSQL(result.tables, { source: path.basename(uri.fsPath) }));
    const choice = await vscode.window.showInformationMessage(
        `${name} erzeugt: ${result.tables.length} Tabellen. Im Terminal anlegen mit: mysql < ${name}`, 'Öffnen');
    if (choice) vscode.window.showTextDocument(target, { viewColumn: vscode.ViewColumn.Beside });
}

async function saveDiagram(uri, kind) {
    let which = kind;
    if (!lib.KINDS.includes(which)) {
        const pick = await vscode.window.showQuickPick(lib.KINDS.map((k) => ({ label: KIND_LABELS[k], kind: k })),
            { placeHolder: 'Welches Diagramm?' });
        if (!pick) return;
        which = pick.kind;
    }
    const text = await modelText(uri);
    const { model } = core.compile(text);
    if (model.errors.length) return showProblems();

    const layout = await readLayout(uri);
    const { svg } = core.diagram(text, which, { positions: layout[which], background: '#ffffff' });
    const target = vscode.Uri.file(lib.svgPath(uri.fsPath, which));
    await writeText(target, svg + '\n');
    const choice = await vscode.window.showInformationMessage(`${path.basename(target.fsPath)} gespeichert.`, 'Im Explorer zeigen');
    if (choice) vscode.commands.executeCommand('revealInExplorer', target);
}

class Preview {
    constructor(context, document, onDispose) {
        this.document = document;
        this.uri = document.uri;
        this.timer = null;
        this.saveTimer = null;
        this.layout = null;
        const root = context.extensionUri;
        this.panel = vscode.window.createWebviewPanel('hackschuleEr.preview', `ER: ${path.basename(this.uri.fsPath)}`,
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
<title>ER-Diagramm</title></head>
<body>
<div class="bar">
  <div class="seg" role="group" aria-label="Darstellung">
    <button type="button" data-kind="chen">Chen</button><button type="button" data-kind="crowfoot">Krähenfuß</button><button type="button" data-kind="tables">Tabellen</button>
  </div>
  <span class="spacer"></span>
  <button type="button" class="act" data-act="reset" title="Alle Kästen neu anordnen">Neu anordnen</button>
  <button type="button" class="act" data-act="svg" title="Dieses Diagramm als SVG-Datei neben dem Modell speichern">Als SVG speichern</button>
  <button type="button" class="act primary" data-act="sql" title="Das MySQL-Schema als .sql-Datei neben dem Modell speichern">SQL erzeugen</button>
</div>
<div id="problems" class="problems" hidden></div>
<div id="board" class="board"></div>
<p class="hint" id="hint"></p>
<details id="schema-box" open><summary>Tabellen</summary><div id="schema" class="schema"></div></details>
<details id="steps-box"><summary>So wurde übersetzt</summary><ol id="steps" class="steps"></ol></details>
<script nonce="${nonce}" src="${src('core', 'er-core.js')}"></script>
<script nonce="${nonce}" src="${src('media', 'preview.js')}"></script>
</body></html>`;

        webview.onDidReceiveMessage((msg) => this.receive(msg));
        this.panel.onDidDispose(() => {
            clearTimeout(this.timer);
            clearTimeout(this.saveTimer);
            onDispose();
        });
        readLayout(this.uri).then((layout) => {
            this.layout = layout;
            this.update();
        });
    }

    scheduleUpdate() {
        clearTimeout(this.timer);
        this.timer = setTimeout(() => this.update(), 200);
    }

    update() {
        if (!this.layout) return;
        this.panel.webview.postMessage({ type: 'model', text: this.document.getText(), layout: this.layout,
            file: path.basename(this.uri.fsPath) });
    }

    saveLayout() {
        clearTimeout(this.saveTimer);
        this.saveTimer = setTimeout(() => {
            writeText(vscode.Uri.file(lib.layoutPath(this.uri.fsPath)), lib.writeLayout(this.layout))
                .catch((e) => vscode.window.showWarningMessage(`Die Anordnung ließ sich nicht speichern: ${e.message}`));
        }, 300);
    }

    receive(msg) {
        if (!msg || typeof msg !== 'object') return;
        if (msg.type === 'ready') this.update();
        else if (msg.type === 'positions' && lib.KINDS.includes(msg.kind) && msg.positions && typeof msg.positions === 'object') {
            this.layout[msg.kind] = lib.readLayout(JSON.stringify({ [msg.kind]: msg.positions }))[msg.kind];
            this.saveLayout();
        } else if (msg.type === 'reset' && lib.KINDS.includes(msg.kind)) {
            this.layout[msg.kind] = {};
            this.saveLayout();
            this.update();
        } else if (msg.type === 'sql') {
            generateSql(this.uri);
        } else if (msg.type === 'svg' && lib.KINDS.includes(msg.kind)) {
            saveDiagram(this.uri, msg.kind);
        } else if (msg.type === 'reveal' && Number.isInteger(msg.line)) {
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
