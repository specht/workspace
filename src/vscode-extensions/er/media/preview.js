// The preview beside the .er editor. It draws with the same core as the
// command line (window.ErCore), so the picture here and the exported SVG are
// the same. Boxes can be dragged; their positions go back to the extension,
// which keeps them in the hidden .<model>.layout.json.
(function () {
    'use strict';
    const vscode = acquireVsCodeApi();
    const core = window.ErCore;
    const saved = vscode.getState() || {};
    let kind = ['chen', 'crowfoot', 'tables'].includes(saved.kind) ? saved.kind : 'chen';
    let text = '';
    let layout = { chen: {}, crowfoot: {}, tables: {} };
    let graph = null;
    let viewBox = null;
    let lastGood = null;

    const board = document.getElementById('board');
    const problems = document.getElementById('problems');
    const hint = document.getElementById('hint');
    const schemaBox = document.getElementById('schema');
    const stepsBox = document.getElementById('steps');

    const HINTS = {
        chen: 'Chen: Rechteck = Entität, Raute = Beziehung, Ellipse = Attribut, unterstrichen = Schlüssel. Kästen lassen sich verschieben.',
        crowfoot: 'Krähenfuß: | = eins, Krähenfuß = viele. n:m-Beziehungen sind hier noch eine einzige Linie.',
        tables: 'Tabellen: So entstehen die Tabellen in der Datenbank. Jede n:m-Beziehung ist jetzt eine eigene Tabelle. PK = Primärschlüssel, FK = Fremdschlüssel.',
    };

    function esc(s) {
        return core.esc(s);
    }

    // `code` in the step texts
    function withCode(s) {
        return esc(s).replace(/`([^`]+)`/g, '<code>$1</code>');
    }

    function setKind(k) {
        kind = k;
        vscode.setState({ kind });
        document.querySelectorAll('.seg button').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.kind === kind)));
        viewBox = null;
        render();
    }

    function render() {
        const compiled = core.compile(text);
        const ok = compiled.model.errors.length === 0;
        if (ok) lastGood = compiled;
        showProblems(compiled.model.errors);
        const use = ok ? compiled : lastGood;
        board.classList.toggle('stale', !ok && !!lastGood);
        hint.textContent = HINTS[kind];
        if (!use) {
            board.innerHTML = '<p class="empty">Schreib links ein Modell, zum Beispiel:<br><code>entity movie</code><br><code>&nbsp;&nbsp;id: int, key</code></p>';
            schemaBox.innerHTML = '';
            stepsBox.innerHTML = '';
            return;
        }
        graph = kind === 'chen' ? core.chenGraph(use.model) : core.crowfootGraph(use.model, use.result, kind === 'tables');
        core.layout(graph, layout[kind]);
        draw();
        showSchema(use.result);
    }

    function draw() {
        const vb = viewBox || null;
        board.innerHTML = core.renderSVG(graph, { viewBox: vb || undefined, title: `ER-Diagramm (${kind})` });
        const svg = board.querySelector('svg');
        svg.removeAttribute('width');
        svg.removeAttribute('height');
    }

    function showProblems(list) {
        if (!list.length) {
            problems.hidden = true;
            problems.innerHTML = '';
            return;
        }
        problems.hidden = false;
        problems.innerHTML = `<strong>${list.length === 1 ? '1 Fehler' : `${list.length} Fehler`}</strong>` +
            (lastGood ? ' – das Diagramm zeigt den letzten fehlerfreien Stand.' : '') +
            '<ul>' + list.slice(0, 8).map((p) => `<li><a href="#" data-line="${p.line}">Zeile ${p.line}</a>: ${esc(p.msg)}</li>`).join('') + '</ul>';
    }

    function showSchema(result) {
        schemaBox.innerHTML = result.tables.map((t) => {
            const fk = new Set(t.fks.flatMap((f) => f.cols));
            const cols = t.columns.map((c) => {
                let s = esc(c.name);
                if (t.pk.includes(c.name)) s = `<u>${s}</u>`;
                if (fk.has(c.name)) s = `<span class="fk">↑${s}</span>`;
                return s;
            });
            return `<div><b>${esc(t.name)}</b>(${cols.join(', ')})</div>`;
        }).join('') + '<p class="legend"><u>unterstrichen</u> = Primärschlüssel · <span class="fk">↑</span> = Fremdschlüssel</p>';
        stepsBox.innerHTML = result.steps.map((s) => `<li><span class="tag">${esc(s.tag)}</span> ${withCode(s.text)}</li>`).join('');
    }

    // ---- dragging: an entity or a relationship takes its attributes along
    let drag = null;
    function toSvg(ev) {
        const svg = board.querySelector('svg');
        const p = svg.createSVGPoint();
        p.x = ev.clientX;
        p.y = ev.clientY;
        return p.matrixTransform(svg.getScreenCTM().inverse());
    }
    board.addEventListener('pointerdown', (ev) => {
        const g = ev.target.closest && ev.target.closest('.node');
        if (!g || !graph) return;
        const id = g.getAttribute('data-id');
        const svg = board.querySelector('svg');
        viewBox = svg.getAttribute('viewBox').split(' ').map(Number);
        const movers = graph.nodes.filter((n) => n.id === id || (n.kind === 'A' && n.owner === id));
        drag = { start: toSvg(ev), orig: movers.map((n) => ({ n, x: n.x, y: n.y })) };
        board.setPointerCapture(ev.pointerId);
        ev.preventDefault();
    });
    board.addEventListener('pointermove', (ev) => {
        if (!drag) return;
        const pt = toSvg(ev);
        const dx = pt.x - drag.start.x;
        const dy = pt.y - drag.start.y;
        for (const o of drag.orig) {
            o.n.x = Math.round((o.x + dx) * 10) / 10;
            o.n.y = Math.round((o.y + dy) * 10) / 10;
        }
        draw();
    });
    const endDrag = () => {
        if (!drag) return;
        drag = null;
        const positions = {};
        for (const n of graph.nodes) positions[n.id] = { x: n.x, y: n.y };
        layout[kind] = positions;
        vscode.postMessage({ type: 'positions', kind, positions });
        viewBox = null;
        draw();
    };
    board.addEventListener('pointerup', endDrag);
    board.addEventListener('pointercancel', endDrag);

    document.querySelectorAll('.seg button').forEach((b) => b.addEventListener('click', () => setKind(b.dataset.kind)));
    document.querySelector('[data-act="reset"]').addEventListener('click', () => {
        layout[kind] = {};
        viewBox = null;
        vscode.postMessage({ type: 'reset', kind });
        render();
    });
    document.querySelector('[data-act="sql"]').addEventListener('click', () => vscode.postMessage({ type: 'sql' }));
    document.querySelector('[data-act="svg"]').addEventListener('click', () => vscode.postMessage({ type: 'svg', kind }));
    problems.addEventListener('click', (ev) => {
        const a = ev.target.closest('a[data-line]');
        if (!a) return;
        ev.preventDefault();
        vscode.postMessage({ type: 'reveal', line: Number(a.dataset.line) });
    });

    window.addEventListener('message', (ev) => {
        const msg = ev.data;
        if (!msg || msg.type !== 'model') return;
        text = String(msg.text || '');
        layout = Object.assign({ chen: {}, crowfoot: {}, tables: {} }, msg.layout || {});
        if (!drag) render();
    });

    setKind(kind);
    vscode.postMessage({ type: 'ready' });
})();
