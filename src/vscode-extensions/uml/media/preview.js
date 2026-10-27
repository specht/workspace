// The preview beside the .uml editor. It draws with the same core as the
// command line (window.UmlCore), so the picture here and the saved SVG are
// the same.
//
// The diagram is drawn at a fixed scale (zoom) inside a scrolling board.
// Typing never rescales it: when the drawing grows or shrinks, the scroll
// position moves with it, so what you look at stays put. Only »Einpassen«
// and opening the preview without a saved zoom fit the whole diagram into
// the board.
(function () {
    'use strict';
    const vscode = acquireVsCodeApi();
    const core = window.UmlCore;
    const saved = vscode.getState() || {};
    let text = '';
    let graph = null;
    let lastGood = null;
    // Pixels per diagram unit; null: fit on the next draw.
    let zoom = Number.isFinite(saved.zoom) ? saved.zoom : null;
    // The bounds the board shows now, so the next draw can keep its place.
    let shown = null;
    const ZOOM_MIN = 0.2;
    const ZOOM_MAX = 3;

    const board = document.getElementById('board');
    const problems = document.getElementById('problems');
    const zoomLevel = document.getElementById('zoom-level');

    function saveState() {
        vscode.setState({ zoom });
    }

    function render() {
        const model = core.compile(text);
        const ok = model.errors.length === 0;
        if (ok) lastGood = model;
        showProblems(model.errors);
        const use = ok ? model : lastGood;
        board.classList.toggle('stale', !ok && !!lastGood);
        if (!use || use.classes.length === 0) {
            graph = null;
            shown = null;
            zoomLevel.textContent = '';
            board.innerHTML = '<p class="empty">Schreib links eine Klasse, zum Beispiel:<br><code>class Book</code><br><code>&nbsp;&nbsp;- title: String</code><br><code>&nbsp;&nbsp;+ lend(): void</code></p>';
            return;
        }
        graph = core.classGraph(use);
        core.layout(graph);
        draw();
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
        const vb = core.viewBox(graph);
        if (zoom === null) {
            zoom = fitZoom(vb);
            anchor = null;
            saveState();
        }
        const z = zoom;
        const cw = board.clientWidth;
        const ch = board.clientHeight;
        if (anchor === undefined && shown) {
            // Typing: the point in the middle of the board stays.
            anchor = { mx: shown[0] + (board.scrollLeft + cw / 2) / z, my: shown[1] + (board.scrollTop + ch / 2) / z, px: cw / 2, py: ch / 2 };
        }
        const canvas = [vb[0] - cw / z, vb[1] - ch / z, vb[2] + 2 * cw / z, vb[3] + 2 * ch / z];
        board.innerHTML = core.renderSVG(graph, { viewBox: canvas, title: 'Klassendiagramm' });
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
        const old = zoom;
        const x = px === undefined ? board.clientWidth / 2 : px;
        const y = py === undefined ? board.clientHeight / 2 : py;
        const anchor = { mx: shown[0] + (board.scrollLeft + x) / old, my: shown[1] + (board.scrollTop + y) / old, px: x, py: y };
        zoom = Math.min(ZOOM_MAX, Math.max(ZOOM_MIN, z));
        saveState();
        draw(anchor);
    }

    function fit() {
        if (!graph) return;
        zoom = null;
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
            '<ul>' + list.slice(0, 8).map((p) => `<li><a href="#" data-line="${p.line}">Zeile ${p.line}</a>: ${core.esc(p.msg)}</li>`).join('') + '</ul>';
    }

    document.querySelector('[data-zoom="in"]').addEventListener('click', () => zoomTo(zoom * 1.25));
    document.querySelector('[data-zoom="out"]').addEventListener('click', () => zoomTo(zoom / 1.25));
    document.querySelector('[data-zoom="fit"]').addEventListener('click', fit);
    // Ctrl + wheel (and pinch on a touchpad) zooms around the pointer;
    // the plain wheel scrolls as usual.
    board.addEventListener('wheel', (ev) => {
        if (!ev.ctrlKey || !graph) return;
        ev.preventDefault();
        const r = board.getBoundingClientRect();
        zoomTo(zoom * Math.exp(-ev.deltaY * 0.0015), ev.clientX - r.left, ev.clientY - r.top);
    }, { passive: false });
    document.querySelector('[data-act="svg"]').addEventListener('click', () => vscode.postMessage({ type: 'svg' }));
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
        render();
    });

    vscode.postMessage({ type: 'ready' });
})();
