// The preview beside the .er editor. It draws with the same core as the
// command line (window.ErCore), so the picture here and the exported SVG are
// the same. Boxes can be dragged; their positions go back to the extension,
// which keeps them in the hidden .<model>.layout.json.
//
// The diagram is drawn at a fixed scale (zoom) inside a scrolling board.
// Typing or dragging never rescales it: when the drawing grows or shrinks,
// the scroll position moves with it, so what you look at stays put. Only
// »Einpassen«, »Neu anordnen« and opening the preview without a saved zoom
// fit the whole diagram into the board.
(function () {
    'use strict';
    const vscode = acquireVsCodeApi();
    const core = window.ErCore;
    const saved = vscode.getState() || {};
    let kind = ['chen', 'crowfoot', 'tables'].includes(saved.kind) ? saved.kind : 'chen';
    let text = '';
    let layout = { chen: {}, crowfoot: {}, tables: {} };
    let graph = null;
    let lastGood = null;
    // Pixels per diagram unit, per kind; null: fit on the next draw.
    const zooms = Object.assign({ chen: null, crowfoot: null, tables: null }, saved.zooms || {});
    // The bounds the board shows now, so the next draw can keep its place.
    let shown = null;
    // Positions from the last drawing, per kind: while you type, boxes stay
    // where they are and only new ones are placed. Not saved; »Neu anordnen«
    // forgets them.
    const sticky = { chen: null, crowfoot: null, tables: null };
    const ZOOM_MIN = 0.2;
    const ZOOM_MAX = 3;

    const board = document.getElementById('board');
    const problems = document.getElementById('problems');
    const hint = document.getElementById('hint');
    const schemaBox = document.getElementById('schema');
    const stepsBox = document.getElementById('steps');
    const zoomLevel = document.getElementById('zoom-level');

    function saveState() {
        vscode.setState({ kind, zooms });
    }

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
        saveState();
        document.querySelectorAll('.seg button[data-kind]').forEach((b) => b.setAttribute('aria-pressed', String(b.dataset.kind === kind)));
        shown = null;
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
        if (!use || use.model.entities.length === 0) {
            shown = null;
            zoomLevel.textContent = '';
            board.innerHTML = '<p class="empty">Schreib links ein Modell, zum Beispiel:<br><code>entity movie</code><br><code>&nbsp;&nbsp;id: int, primary key</code></p>';
            schemaBox.innerHTML = '';
            stepsBox.innerHTML = '';
            return;
        }
        graph = kind === 'chen' ? core.chenGraph(use.model) : core.crowfootGraph(use.model, use.result, kind === 'tables');
        core.layout(graph, Object.assign(keepFromLastDrawing(graph), layout[kind]));
        sticky[kind] = {
            positions: Object.fromEntries(graph.nodes.map((n) => [n.id, { x: n.x, y: n.y }])),
            attrs: attributeSets(graph),
        };
        draw();
        showSchema(use.result);
    }

    // owner id -> the ids of its attributes, as one string to compare.
    function attributeSets(g) {
        const sets = {};
        for (const n of g.nodes) {
            if (n.kind !== 'A') continue;
            sets[n.owner] = (sets[n.owner] ? `${sets[n.owner]}|` : '') + n.id;
        }
        return sets;
    }

    // The positions of the last drawing that still fit: every node that is
    // still there, except the attributes of an owner whose attributes changed
    // (they are spread around it again).
    function keepFromLastDrawing(g) {
        const last = sticky[kind];
        if (!last) return {};
        const now = attributeSets(g);
        const keep = {};
        for (const n of g.nodes) {
            const p = last.positions[n.id];
            if (!p) continue;
            if (n.kind === 'A' && now[n.owner] !== last.attrs[n.owner]) continue;
            keep[n.id] = p;
        }
        return keep;
    }

    function fitZoom(vb) {
        const w = Math.max(1, board.clientWidth - 2);
        const h = Math.max(1, board.clientHeight - 2);
        return Math.min(ZOOM_MAX, Math.max(ZOOM_MIN, Math.min(w / vb[2], h / vb[3], 1.5)));
    }

    // The drawing is the diagram plus one board of empty space on every
    // side, so there is always a scroll position that keeps a point in place.
    // anchor: { mx, my, px, py }: diagram point (mx, my) goes to board pixel
    // (px, py). Without anchor, the diagram is centred.
    function draw(anchor) {
        const vb = core.viewBox(graph.nodes);
        if (zooms[kind] === null) {
            zooms[kind] = fitZoom(vb);
            anchor = null;
            saveState();
        }
        const z = zooms[kind];
        const cw = board.clientWidth;
        const ch = board.clientHeight;
        if (anchor === undefined && shown) {
            // Typing or dragging: the point in the middle of the board stays.
            anchor = { mx: shown[0] + (board.scrollLeft + cw / 2) / z, my: shown[1] + (board.scrollTop + ch / 2) / z, px: cw / 2, py: ch / 2 };
        }
        const canvas = [vb[0] - cw / z, vb[1] - ch / z, vb[2] + 2 * cw / z, vb[3] + 2 * ch / z];
        board.innerHTML = core.renderSVG(graph, { viewBox: canvas, title: `ER-Diagramm (${kind})` });
        const svg = board.querySelector('svg');
        svg.setAttribute('width', String(Math.round(canvas[2] * z)));
        svg.setAttribute('height', String(Math.round(canvas[3] * z)));
        const a = anchor || { mx: vb[0] + vb[2] / 2, my: vb[1] + vb[3] / 2, px: cw / 2, py: ch / 2 };
        board.scrollLeft = (a.mx - canvas[0]) * z - a.px;
        board.scrollTop = (a.my - canvas[1]) * z - a.py;
        shown = canvas;
        zoomLevel.textContent = `${Math.round(z * 100)} %`;
    }

    // Zoom to z, keeping the diagram point under (px, py) in place; px, py
    // are pixels from the board's top left, default its centre.
    function zoomTo(z, px, py) {
        if (!graph || !shown) return;
        const old = zooms[kind];
        const x = px === undefined ? board.clientWidth / 2 : px;
        const y = py === undefined ? board.clientHeight / 2 : py;
        const anchor = { mx: shown[0] + (board.scrollLeft + x) / old, my: shown[1] + (board.scrollTop + y) / old, px: x, py: y };
        zooms[kind] = Math.min(ZOOM_MAX, Math.max(ZOOM_MIN, z));
        saveState();
        draw(anchor);
    }

    function fit() {
        zooms[kind] = null;
        draw(null);
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
        draw();
    };
    board.addEventListener('pointerup', endDrag);
    board.addEventListener('pointercancel', endDrag);

    document.querySelectorAll('.seg button[data-kind]').forEach((b) => b.addEventListener('click', () => setKind(b.dataset.kind)));
    document.querySelector('[data-zoom="in"]').addEventListener('click', () => zoomTo(zooms[kind] * 1.25));
    document.querySelector('[data-zoom="out"]').addEventListener('click', () => zoomTo(zooms[kind] / 1.25));
    document.querySelector('[data-zoom="fit"]').addEventListener('click', fit);
    // Ctrl + wheel (and pinch on a touchpad) zooms around the pointer;
    // the plain wheel scrolls as usual.
    board.addEventListener('wheel', (ev) => {
        if (!ev.ctrlKey || !graph) return;
        ev.preventDefault();
        const r = board.getBoundingClientRect();
        zoomTo(zooms[kind] * Math.exp(-ev.deltaY * 0.0015), ev.clientX - r.left, ev.clientY - r.top);
    }, { passive: false });
    document.querySelector('[data-act="reset"]').addEventListener('click', () => {
        layout[kind] = {};
        sticky[kind] = null;
        zooms[kind] = null;
        shown = null;
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
