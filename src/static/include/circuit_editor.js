// The circuit editor of the Logik-Werkstatt (src/content/logikwerkstatt):
// students place gates, switches and lamps, draw wires from pin to pin and
// watch the signals settle round by round, with the drawing and the engine
// of logic_circuit.js (load that first). A finished circuit can be saved as
// a block with its own pins and placed in the next circuit like a gate.
//
//   <div class='circuit-editor' data-free='true'></div>          a sandbox with switches and lamps to place
//   <div class='circuit-editor' data-task-editor='true' data-inputs='a b' data-outputs='y'
//        data-gates='nand' data-max-gates='4'></div>              in a task card (task_card.rb, check circuit)
//   <div class='circuit-view' data-netlist='in a; in b; nand g a b; out y g'></div>   a circuit to look at and switch
//                                         (data-on='a': switches that start at 1)
//
// A circuit is a netlist (LogicCircuit.Netlist): one part per line, each
// input naming its source. In a task card the editor keeps that text in the
// card's hidden field, which is what »Prüfen« sends; the server simulates it
// (circuit_check.rb).
//
// Kept in this browser only (localStorage, per account as the tips:
// data-hint-owner): the draft of every editor, so that a reload loses
// nothing, and the blocks a person has saved. A checked circuit is also the
// task's stored answer on the server (TaskState), which brings it, and the
// blocks inside it, to another computer.
//
// Mouse and touch: a palette button adds its part (a click puts it on a free
// place, dragging puts it where it is dropped); a part is moved by dragging
// it; a wire runs from an output pin (right) to an input pin (left), dragged
// or tapped one after the other; a click on a switch toggles it. Keyboard:
// Tab goes from part to part, the arrow keys move it, Enter or the space bar
// toggles a switch, Entf deletes, V starts a wire from the part's output and
// a digit ends it in that input of another part, a digit alone selects the
// wire into that input; Strg+Z and Strg+Y undo and redo. Several parts: a
// frame dragged on the empty paper or Shift+click selects them, dragging
// one of them moves them all, Entf deletes them, Strg+A selects all, Strg+D
// doubles; a right click (or the menu key) opens a menu of what fits the
// spot. The keys work wherever the focus is inside the editor. On a phone
// the circuit can only be looked at and switched.
(function () {
    'use strict';

    // in node (test/static/circuit_editor.test.cjs) only the geometry
    const isNode = typeof module !== 'undefined' && module.exports && typeof window === 'undefined';
    const LC = isNode ? require('./logic_circuit.js') : window.LogicCircuit;
    if (!LC || !LC.Netlist) return;
    const N = LC.Netlist;
    const D = LC.draw;
    const { GW, BUBBLE, SWITCH_W, SWITCH_H, LAMP, GATES } = LC;

    const SNAP = 8;
    const BLOCK_HEAD = 20;
    const PIN_STEP = 24;
    const MAX_HISTORY = 100;
    const MAX_TEXT = 4000;      // what the server takes as an answer (TaskStore::MAX_ANSWER)
    const TABLE_INPUTS = 5;     // a truth table beside the editor up to 32 rows
    const phone = () => window.matchMedia && window.matchMedia('(max-width: 575.98px)').matches;
    const coarse = () => window.matchMedia && window.matchMedia('(pointer: coarse)').matches;
    const reduced = () => window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    // ------------------------------------------------------------------
    // Storage in this browser

    const owner = () => (document.body && document.body.dataset.hintOwner) || 'gast';
    const storage = {
        get(key) {
            try { return JSON.parse(window.localStorage.getItem(key)); } catch (e) { return null; }
        },
        set(key, value) {
            try { window.localStorage.setItem(key, JSON.stringify(value)); return true; } catch (e) { return false; }
        },
        remove(key) {
            try { window.localStorage.removeItem(key); } catch (e) { /* nothing stored */ }
        },
    };

    // The person's blocks: { NAME: { parts } }, shared by every editor of
    // every page.
    const Library = {
        key: () => `circuit-blocks:${owner()}`,
        load() {
            const value = storage.get(this.key());
            return value && typeof value === 'object' ? value : {};
        },
        save(blocks) {
            storage.set(this.key(), blocks);
            document.dispatchEvent(new CustomEvent('circuit-library'));
        },
    };

    const clone = (x) => JSON.parse(JSON.stringify(x));
    const snap = (v, step = SNAP) => Math.round(v / step) * step;

    // ------------------------------------------------------------------
    // Geometry: the box of a part, its pins

    function blockDef(circuit, name) {
        return circuit.blocks[name] || { parts: [] };
    }

    function geom(circuit, p) {
        if (p.kind === 'in') {
            return { box: [p.x - SWITCH_W / 2, p.y - SWITCH_H / 2, SWITCH_W, SWITCH_H], ins: [], outs: { '': [p.x + SWITCH_W / 2, p.y] } };
        }
        if (p.kind === 'out') {
            return { box: [p.x - LAMP, p.y - LAMP, 2 * LAMP, 2 * LAMP], ins: [[p.x - LAMP, p.y]], outs: {} };
        }
        if (p.kind === 'block') {
            const def = blockDef(circuit, p.block);
            const ins = N.pinsIn(def);
            const outs = N.pinsOut(def);
            const rows = Math.max(ins.length, outs.length, 1);
            const longest = (list) => list.reduce((m, s) => Math.max(m, s.length), 0);
            const w = Math.max(64, snap(30 + 7.5 * (longest(ins) + longest(outs)), SNAP), snap(24 + 9 * p.block.length, SNAP));
            const h = BLOCK_HEAD + PIN_STEP * rows;
            const at = (i) => p.y + BLOCK_HEAD + PIN_STEP / 2 + PIN_STEP * i;
            return {
                box: [p.x, p.y, w, h],
                ins: ins.map((_, i) => [p.x, at(i)]),
                outs: Object.fromEntries(outs.map((name, i) => [name, [p.x + w, at(i)]])),
                inNames: ins, outNames: outs,
            };
        }
        const n = p.src.length;
        const h = LC.gateHeightFor(n);
        const neg = GATES[p.kind].neg;
        const w = GW + (neg ? 2 * BUBBLE : 0);
        return {
            box: [p.x, p.y, w, h],
            ins: p.src.map((_, i) => [p.x, n === 1 ? p.y + h / 2 : p.y + PIN_STEP / 2 + PIN_STEP * i]),
            outs: { '': [p.x + w, p.y + h / 2] },
        };
    }

    // where a part's drawing begins on the left: a switch's name stands left of it
    // a name in the mono font at 15 px: about 9 px a letter
    const LABEL_CHAR = 9;
    const labelLeft = (p, boxLeft) => (p.kind === 'in' ? boxLeft - 8 - LABEL_CHAR * p.id.length : boxLeft);

    // the source text of an output pin: g1, or h1.s for a block
    const sourceOf = (p, port) => (p.kind === 'block' ? `${p.id}.${port}` : p.id);

    // The ways a wire can run from one pin to another, at right angles, the
    // plain one first: straight, or across at a column between the two pins
    // (the middle, then lanes 8 px to either side), or, back to the left,
    // round below both parts (lanes further out).
    function routes(a, b, boxA, boxB) {
        const [x0, y0] = a;
        const [x1, y1] = b;
        if (y0 === y1 && x1 > x0) return [[[x0, y0], [x1, y1]]];
        if (x1 - x0 >= 16) {
            const middle = Math.round((x0 + x1) / 2 / 4) * 4;
            const list = [];
            for (let k = 0; k < 40; k++) {
                const xm = middle + (k % 2 ? 1 : -1) * 8 * Math.ceil(k / 2);
                if (xm >= x0 + 6 && xm <= x1 - 6) list.push([[x0, y0], [xm, y0], [xm, y1], [x1, y1]]);
            }
            return list;
        }
        const bottom = Math.max(boxA[1] + boxA[3], boxB[1] + boxB[3]) + 14;
        const list = [];
        for (let k = 0; k < 5; k++) {
            for (let j = 0; j < 5; j++) {
                const xa = x0 + 12 + 8 * j;
                const xb = x1 - 12 - 8 * j;
                const yb = bottom + 8 * k;
                list.push([[x0, y0], [xa, y0], [xa, yb], [xb, yb], [xb, y1], [x1, y1]]);
            }
        }
        return list;
    }

    // the plain way, for one wire alone
    const route = (a, b, boxA, boxB) => routes(a, b, boxA, boxB)[0];

    // whether two pieces of wire lie on top of each other for more than a point
    function shared([p, q], [r, t]) {
        if (p[0] === q[0] && r[0] === t[0] && p[0] === r[0]) {
            return Math.min(Math.max(p[1], q[1]), Math.max(r[1], t[1])) - Math.max(Math.min(p[1], q[1]), Math.min(r[1], t[1])) > 0;
        }
        if (p[1] === q[1] && r[1] === t[1] && p[1] === r[1]) {
            return Math.min(Math.max(p[0], q[0]), Math.max(r[0], t[0])) - Math.max(Math.min(p[0], q[0]), Math.min(r[0], t[0])) > 0;
        }
        return false;
    }

    const pieces = (points) => points.slice(1).map((q, i) => [points[i], q]);

    // Every wire of a circuit ({ src, a, b, boxA, boxB }) gets its points:
    // the first way on which it shares no piece with a wire from another
    // source (one source may share: that is where it branches, with a dot).
    // Straight wires go first, they cannot move.
    function routeAll(wires) {
        const placed = [];
        const order = wires.map((w) => ({ w, ways: routes(w.a, w.b, w.boxA, w.boxB) }))
            .sort((x, y) => (y.ways.length === 1) - (x.ways.length === 1));
        for (const { w, ways } of order) {
            const free = ways.find((way) => pieces(way).every((piece) => placed.every((o) => o.src === w.src || !shared(piece, o.piece))));
            w.points = free || ways[0];
            for (const piece of pieces(w.points)) placed.push({ src: w.src, piece });
        }
        return wires;
    }

    // the wires of a circuit's top level, routed
    function wiresOf(circuit) {
        const geoms = new Map(circuit.parts.map((p) => [p.id, geom(circuit, p)]));
        const list = [];
        for (const p of circuit.parts) {
            p.src.forEach((src, i) => {
                if (!src) return;
                const [id, port] = src.split('.');
                const from = geoms.get(id);
                if (!from) return;
                const a = from.outs[port || ''];
                const b = geoms.get(p.id).ins[i];
                if (!a || !b) return;
                list.push({ src, a, b, boxA: from.box, boxB: geoms.get(p.id).box, to: [p.id, i] });
            });
        }
        return routeAll(list);
    }

    // [left, top, right, bottom] of everything drawn: parts, the names of
    // switches (left of them) and lamps (right of them), the wires
    function extentOf(circuit, wires = wiresOf(circuit)) {
        let [x0, y0, x1, y1] = [Infinity, Infinity, -Infinity, -Infinity];
        for (const p of circuit.parts) {
            const [x, y, bw, bh] = geom(circuit, p).box;
            x0 = Math.min(x0, labelLeft(p, x));
            y0 = Math.min(y0, y);
            x1 = Math.max(x1, x + bw + (p.kind === 'out' ? 7 + LABEL_CHAR * p.id.length : 0));
            y1 = Math.max(y1, y + bh + (GATES[p.kind] ? 14 : 0));
        }
        for (const w of wires) {
            for (const [x, y] of w.points) {
                x0 = Math.min(x0, x);
                x1 = Math.max(x1, x);
                y1 = Math.max(y1, y);
            }
        }
        return x0 === Infinity ? [0, 0, 0, 0] : [x0, y0, x1, y1];
    }

    // the viewBox: everything drawn and a margin of 12 around it, from 0, 0
    // unless something lies further left or up
    function viewBoxOf(circuit, min = [0, 0]) {
        const [x0, y0, x1, y1] = extentOf(circuit);
        const left = Math.min(0, x0 - 12);
        const top = Math.min(0, y0 - 12);
        return [left, top, Math.max(x1 + 12 - left, min[0]), Math.max(y1 + 12 - top, min[1])];
    }

    // ------------------------------------------------------------------
    // Laying out a circuit that comes without positions (an example, a
    // circuit typed by hand): columns by the longest way from a switch

    function layout(circuit, scope = circuit) {
        const parts = scope.parts;
        if (parts.every((p) => Number.isFinite(p.x) && Number.isFinite(p.y))) return;
        const byId = new Map(parts.map((p) => [p.id, p]));
        const depth = new Map();
        const visiting = new Set();
        const depthOf = (p) => {
            if (depth.has(p.id)) return depth.get(p.id);
            if (p.kind === 'in' || visiting.has(p.id)) return 0;
            visiting.add(p.id);
            let d = 0;
            for (const s of p.src) {
                const from = s && byId.get(s.split('.')[0]);
                if (from) d = Math.max(d, depthOf(from) + 1);
            }
            visiting.delete(p.id);
            depth.set(p.id, d);
            return d;
        };
        parts.forEach(depthOf);
        const last = Math.max(1, ...parts.filter((p) => p.kind !== 'out').map((p) => depth.get(p.id) || 0)) + 1;
        const columns = new Map();
        for (const p of parts) {
            const col = p.kind === 'in' ? 0 : p.kind === 'out' ? last : depth.get(p.id);
            if (!columns.has(col)) columns.set(col, []);
            columns.get(col).push(p);
        }
        for (const [col, list] of columns) {
            let y = 24;
            for (const p of list) {
                const tall = p.kind === 'in' || p.kind === 'out' ? 0 : geom(circuit, Object.assign({}, p, { x: 0, y: 0 })).box[3];
                if (p.kind === 'in' || p.kind === 'out') {
                    p.x = col === 0 ? 48 : 64 + col * 112;
                    p.y = y + 16;
                    y += 56;
                } else {
                    p.x = 64 + col * 112 - 20;
                    p.y = y;
                    y += tall + 32;
                }
            }
        }
    }

    if (isNode) {
        module.exports = { geom, route, routes, routeAll, wiresOf, layout, labelLeft, extentOf, viewBoxOf, shared, pieces };
        return;
    }

    // ------------------------------------------------------------------
    // Styles (the gates, wires, switches and lamps are logic_circuit.js's)

    const STYLE = `
.circuit-editor, .circuit-view { --ce-select: var(--stepper-current); position: relative; }
.circuit-editor[data-free], .circuit-view { --widget: #5c677d; --widget-ink: #4a5264; --widget-chip: #ebecef; --widget-line: #dbdee3; --widget-tint: #fafafb;
    margin: 1.75em 0; padding: 0.9em 1.25em; border: 1px solid var(--widget-line); border-left: 5px solid var(--widget);
    border-radius: 0.6em; background: var(--widget-tint); box-shadow: 0 1px 3px rgba(92, 103, 125, 0.08); }
html[data-bs-theme="dark"] .circuit-editor[data-free], html[data-bs-theme="dark"] .circuit-view {
    --widget: #858d9e; --widget-ink: #a5abb7; --widget-chip: #25272b; --widget-line: #2a2e34; --widget-tint: #1a1b1b; }
.circuit-editor[data-free]::before, .circuit-view::before { display: inline-block; padding: 0.15em 0.75em; border-radius: 1em;
    background-color: var(--widget-chip); color: var(--widget-ink); font-size: 80%; font-weight: 700; letter-spacing: 0.02em; margin-bottom: 0.7em; }
.circuit-editor[data-free]::before { content: 'Zum Ausprobieren: Schaltungen bauen'; }
.circuit-view::before { content: 'Zum Ausprobieren: Eingänge anklicken'; }
@media (pointer: coarse) { .circuit-view::before { content: 'Zum Ausprobieren: Eingänge antippen'; } }
.circuit-editor[data-task-editor] { margin: 0.75em 0 0.5em; }
.circuit-editor .ce-palette, .circuit-editor .ce-actions { display: flex; flex-wrap: wrap; gap: 0.35em; align-items: center; margin: 0 0 0.45em; }
.circuit-editor .ce-palette-label { font-size: 85%; color: var(--bs-secondary-color); margin-right: 0.2em; }
.circuit-editor .ce-tool { display: inline-flex; align-items: center; gap: 0.35em; padding: 0.2em 0.55em; min-height: 2.3em; touch-action: none;
    font-size: 0.9em; user-select: none; -webkit-user-select: none; }
.circuit-editor .ce-tool svg { flex: none; overflow: visible; }
.circuit-editor .ce-gate-icon rect, .circuit-editor .ce-gate-icon circle { fill: none; stroke: currentColor; stroke-width: 1.4; }
.circuit-editor .ce-gate-icon text { fill: currentColor; font-size: 10px; text-anchor: middle; dominant-baseline: central; font-family: '0xProto Nerd Font Mono', monospace; }
.circuit-editor .ce-tool .ce-block-icon { font-family: '0xProto Nerd Font Mono', monospace; font-weight: 700; border: 1.6px solid currentColor;
    border-radius: 2px; padding: 0 0.3em; font-size: 0.85em; }
.circuit-editor .ce-tool .bi::before { vertical-align: 0; }
.circuit-editor .ce-actions .btn { display: inline-flex; align-items: center; gap: 0.35em; }
.circuit-editor .ce-actions .btn[aria-disabled='true'] { opacity: 0.45; pointer-events: none; }
.circuit-editor .ce-actions .ce-gap { flex: 1 1 auto; }
@media (pointer: coarse) {
    .circuit-editor .ce-tool, .circuit-editor .ce-actions .btn { min-height: 44px; min-width: 44px; justify-content: center; }
}
.circuit-editor .ce-body, .circuit-view .ce-body { display: flex; flex-wrap: wrap; gap: 0.75em 1.25em; align-items: flex-start; }
.circuit-editor .ce-canvas, .circuit-view .ce-canvas { flex: 1 1 auto; min-width: 0; max-width: 100%; overflow: auto; margin: 0; padding: 0; border: 0;
    box-shadow: none; border-radius: 0.4em; background: var(--bs-body-bg); }
.circuit-editor .ce-canvas { border: 1px solid var(--bs-border-color); }
.circuit-view .ce-canvas { background: transparent; flex: 0 1 auto; max-width: 100%; }
.ce-canvas.logic-circuit::before { content: none; display: none; }
.ce-canvas svg.ce-svg { display: block; overflow: visible; font-family: '0xProto Nerd Font Mono', monospace; touch-action: manipulation;
    user-select: none; -webkit-user-select: none; }
.circuit-editor .ce-canvas.ce-grid { background-image: radial-gradient(circle, color-mix(in srgb, var(--bs-body-color) 16%, transparent) 1px, transparent 1.2px);
    background-size: 16px 16px; background-position: -8px -8px; }
.ce-svg .ce-part { outline: none; }
.ce-svg .ce-part.ce-movable { cursor: grab; touch-action: none; }
.ce-svg .ce-part.ce-dragging { cursor: grabbing; }
.ce-svg .ce-part .ce-hit { fill: transparent; }
.ce-svg .ce-focus { fill: none; stroke: var(--ce-select); stroke-width: 2.5; stroke-dasharray: 5 3; opacity: 0; pointer-events: none; }
.ce-svg .ce-part.ce-selected .ce-focus, .ce-svg .ce-part:focus-visible .ce-focus { opacity: 1; }
.ce-svg .ce-wire-hit { fill: none; stroke: transparent; stroke-width: 12; cursor: pointer; }
.ce-svg .lc-wire.ce-selected { stroke: var(--ce-select); stroke-width: 3.6; }
.ce-svg:focus { outline: none; }
.ce-svg .ce-frame { fill: color-mix(in srgb, var(--ce-select) 10%, transparent); stroke: var(--ce-select); stroke-width: 1.2; stroke-dasharray: 4 3; pointer-events: none; }
.ce-menu { position: fixed; z-index: 1085; min-width: 14rem; display: block; }
.ce-menu .dropdown-item { display: flex; align-items: center; gap: 0.6em; }
.ce-menu .dropdown-item .bi { width: 1.1em; }
.ce-menu .ce-menu-key { margin-left: auto; padding-left: 1.5em; font-size: 85%; color: var(--bs-secondary-color); }
.ce-svg .ce-pin { fill: var(--lc-zero); pointer-events: none; }
.ce-svg .ce-pin.lc-one { fill: var(--lc-one); }
.ce-svg .ce-pin.ce-open { fill: var(--bs-body-bg); stroke: var(--lc-zero); stroke-width: 1.5; }
.ce-svg .ce-pin.ce-target { fill: var(--bs-body-bg); stroke: var(--ce-select); stroke-width: 2.2; }
.ce-svg .ce-pin.ce-source { fill: var(--ce-select); }
.ce-svg .ce-pin-hit { fill: transparent; cursor: crosshair; touch-action: none; }
.ce-svg .ce-rubber { fill: none; stroke: var(--ce-select); stroke-width: 2; stroke-dasharray: 6 4; pointer-events: none; }
.ce-svg .ce-block-name { fill: var(--lc-ink); font-size: 13px; font-weight: 700; text-anchor: middle; dominant-baseline: central; }
.ce-svg .ce-id { fill: var(--bs-secondary-color); font-size: 10px; text-anchor: middle; pointer-events: none; }
.circuit-editor .ce-status, .circuit-view .ce-status { margin: 0.5em 0 0; min-height: 1.5em; font-size: 0.95em; }
.circuit-editor .ce-count { color: var(--bs-secondary-color); margin-right: 0.6em; }
.circuit-editor .ce-count.ce-over { color: var(--bs-danger-text-emphasis, #b02a37); font-weight: 600; }
.circuit-editor .ce-message.ce-warn, .circuit-view .ce-message.ce-warn { color: var(--bs-danger-text-emphasis, #b02a37); }
.circuit-editor .ce-keys { font-size: 85%; color: var(--bs-secondary-color); margin: 0.35em 0 0; }
.circuit-editor .ce-phone-note { display: none; font-size: 90%; color: var(--bs-secondary-color); margin: 0 0 0.5em; }
.circuit-editor .ce-form { display: flex; flex-wrap: wrap; gap: 0.4em; align-items: center; margin: 0 0 0.5em; }
.circuit-editor .ce-form input { width: 9em; font-family: '0xProto Nerd Font Mono', monospace; }
.circuit-editor .ce-form .ce-form-error { color: var(--bs-danger-text-emphasis, #b02a37); font-size: 90%; flex-basis: 100%; }
.circuit-editor .ce-rename input { width: 7em; }
.ce-table { flex: 0 0 auto; font-family: '0xProto Nerd Font Mono', monospace; font-size: 0.9em; }
.ce-table .ce-table-title { font-family: var(--bs-body-font-family); font-size: 85%; color: var(--bs-secondary-color); margin: 0 0 0.25em; }
.ce-table table { border-collapse: collapse; margin: 0; }
.ce-table th, .ce-table td { min-width: 2.4em; padding: 0.1em 0.55em; text-align: center; border-bottom: 1px solid var(--bs-border-color); }
.ce-table th { font-weight: 600; }
.ce-table .lc-gap { border-left: 2px solid var(--bs-border-color); }
.ce-table tbody tr { cursor: pointer; }
.ce-table tbody tr:hover { background: color-mix(in srgb, var(--stepper-current) 6%, transparent); }
.ce-table tbody tr.lc-now { background: color-mix(in srgb, var(--stepper-current) 16%, transparent); box-shadow: inset 3px 0 0 var(--stepper-current); }
.ce-table tbody tr:focus-visible { outline: 2px solid var(--stepper-current); outline-offset: -2px; }
.ce-table .ce-table-note { font-family: var(--bs-body-font-family); font-size: 85%; color: var(--bs-secondary-color); max-width: 16em; }
@media (max-width: 575.98px) {
    .circuit-editor .ce-palette, .circuit-editor .ce-actions, .circuit-editor .ce-keys, .circuit-editor .ce-form { display: none; }
    .circuit-editor .ce-phone-note { display: block; }
}
`;

    function addStyle() {
        D.addStyle();
        if (document.getElementById('circuit-editor-style')) return;
        const style = D.html('style', { id: 'circuit-editor-style' });
        style.textContent = STYLE;
        document.head.appendChild(style);
    }

    const icon = (name) => D.html('i', { class: `bi ${name}`, 'aria-hidden': 'true' });
    const button = (label, iconName, more = {}) => {
        const b = D.html('button', Object.assign({ type: 'button', class: 'btn btn-sm btn-outline-secondary' }, more));
        if (iconName) b.appendChild(icon(iconName));
        if (label) b.appendChild(D.html('span', { text: label }));
        return b;
    };

    // a gate's symbol for a palette button: the rectangle with its sign,
    // the circle of a negated output, small enough for a button
    function gateIcon(kind) {
        const neg = GATES[kind].neg;
        const s = D.svg('svg', { width: neg ? 26 : 22, height: 24, viewBox: `0 0 ${neg ? 26 : 22} 24`, 'aria-hidden': 'true', class: 'ce-gate-icon' });
        D.svg('rect', { x: 1, y: 1, width: 18, height: 22, rx: 1 }, s);
        D.text(s, 10, 12.5, GATES[kind].sign, {});
        if (neg) D.svg('circle', { cx: 22, cy: 12, r: 3 }, s);
        return s;
    }

    // ------------------------------------------------------------------
    // The editor

    class Editor {
        // one part or wire selected: never several parts as well
        get sel() { return this._sel; }
        set sel(value) {
            this._sel = value;
            if (this.many) this.many.clear();
        }

        constructor(el) {
            this.el = el;
            this.mode = el.classList.contains('circuit-view') ? 'view' : el.dataset.free ? 'free' : 'task';
            const words = (name) => (el.dataset[name] || '').trim().split(/\s+/).filter(Boolean);
            this.fixedIn = words('inputs');
            this.fixedOut = words('outputs');
            this.allowed = words('gates').length ? words('gates') : N.GATE_KINDS.slice();
            this.maxGates = Number(el.dataset.maxGates) || null;
            this.minBlocks = Number(el.dataset.minBlocks) || null;
            this.blocksAllowed = el.dataset.blocks !== 'false';
            this.card = el.closest('.task-card');
            this.field = this.card ? this.card.querySelector('.circuit-answer') : null;
            this.draftKey = this.mode === 'free' ? 'sandbox' : this.card ? this.card.dataset.task : null;
            this.base = 0;            // the server's answer the draft builds on (its time)
            this.history = [];
            this.future = [];
            this.inputs = {};         // the switches' bits by name
            this.values = {};
            this.many = new Set();    // several parts selected (a frame, Shift+click)
            this.sel = null;          // { part: id } or { wire: [id, index] }
            this.pending = null;      // a wire begun: { from: [id, port] } or { to: [id, index] }
            this.timer = null;
            this.message = '';
            this.warn = false;
            this.build();
            this.c = this.startCircuit();
            if (this.mode === 'view') {
                this.c = this.parsed(el.dataset.netlist || '') || this.c;
                // switches that start at 1 (data-on='takt'): a flip-flop
                // comes to rest at once
                for (const name of words('on')) this.inputs[name] = 1;
            } else {
                const draft = this.draftKey ? storage.get(this.draftKey && this.storageKey()) : null;
                if (draft && typeof draft.text === 'string') {
                    const c = this.parsed(draft.text);
                    if (c) {
                        this.c = c;
                        this.base = Number(draft.base) || 0;
                    }
                }
                if (this.field) {
                    if (this.field.dataset.loaded) this.serverAnswer(this.field.value, Number(this.field.dataset.updated) || 0);
                    this.field.addEventListener('task-answer', (e) => this.serverAnswer(e.detail.answer, Number(e.detail.updated) || 0));
                    this.field.addEventListener('task-checked', (e) => {
                        this.base = Number(e.detail.updated) || this.base;
                        this.saveDraft();
                    });
                }
                document.addEventListener('circuit-library', () => this.renderPalette());
            }
            this.adoptLibrary();
            this.rebuild(false);
            this.render();
            this.syncField();
            if (window.ResizeObserver) new ResizeObserver(() => this.fitCanvas()).observe(this.canvas);
        }

        storageKey() {
            return `circuit-draft:${owner()}:${this.draftKey}`;
        }

        canEdit() {
            if (this.mode === 'view' || phone()) return false;
            if (this.card && window.TASK_PAGE && !window.TASK_PAGE.loggedIn) return false;
            return true;
        }

        // the task's switches and lamps, nothing between them
        startCircuit() {
            const parts = [];
            const tall = Math.max(this.fixedIn.length, this.fixedOut.length);
            const step = tall > 5 ? 48 : 64;
            // the switches far enough right for their names
            const x = snap(Math.max(48, 12 + 9 * Math.max(0, ...this.fixedIn.map((n) => n.length)) + SWITCH_W / 2 + 8));
            this.fixedIn.forEach((name, i) => parts.push({ id: name, kind: 'in', src: [], x, y: 40 + step * i }));
            this.fixedOut.forEach((name, i) => parts.push({ id: name, kind: 'out', src: [null], x: 560, y: 40 + step * i }));
            return { parts, blocks: {} };
        }

        // a netlist's circuit, laid out, or null (with a message)
        parsed(text) {
            const { circuit, errors } = N.parse(text);
            if (errors.length) {
                this.say(`Diese Schaltung lässt sich nicht lesen: ${errors[0].message}`, true);
                return null;
            }
            layout(circuit);
            for (const def of Object.values(circuit.blocks)) layout(circuit, def);
            if (this.mode === 'task') {
                // the task's switches and lamps are always there
                for (const name of this.fixedIn) if (!circuit.parts.some((p) => p.id === name)) circuit.parts.push({ id: name, kind: 'in', src: [], x: 48, y: 40 });
                for (const name of this.fixedOut) if (!circuit.parts.some((p) => p.id === name)) circuit.parts.push({ id: name, kind: 'out', src: [null], x: 560, y: 40 });
            }
            return circuit;
        }

        // the answer stored on the server: it wins over a draft that does not
        // know it (a check on another computer); an empty one is a deleted answer
        serverAnswer(text, updated) {
            if (!text) {
                if (this.field && this.field.dataset.loaded) {
                    storage.remove(this.storageKey());
                    this.c = this.startCircuit();
                    this.base = 0;
                    this.history = [];
                    this.future = [];
                    this.after(false);
                }
                return;
            }
            const draft = storage.get(this.storageKey());
            if (draft && typeof draft.text === 'string' && updated <= (Number(draft.base) || 0)) {
                this.base = updated;
                return;
            }
            const c = this.parsed(text);
            if (!c) return;
            this.c = c;
            this.base = updated;
            this.adoptLibrary();
            this.after(false, false);
            this.saveDraft();
        }

        // blocks inside a circuit that the library does not know yet join it
        adoptLibrary() {
            if (this.mode === 'view') return;
            const lib = Library.load();
            let added = false;
            for (const [name, def] of Object.entries(this.c.blocks)) {
                if (!lib[name]) {
                    lib[name] = clone(def);
                    added = true;
                }
            }
            if (added) Library.save(lib);
        }

        // ------------------------------------------------------------------
        // The frame: palette, actions, canvas, table, status

        build() {
            addStyle();
            const el = this.el;
            el.replaceChildren();
            this.palette = D.html('div', { class: 'ce-palette', role: 'toolbar', 'aria-label': 'Bauteile' });
            this.actions = D.html('div', { class: 'ce-actions', role: 'toolbar', 'aria-label': 'Bearbeiten' });
            this.form = D.html('div', { class: 'ce-form', hidden: '' });
            this.canvas = D.html('div', { class: 'ce-canvas logic-circuit' });
            this.svg = D.svg('svg', { class: 'ce-svg', role: 'group', 'aria-label': 'Schaltung' });
            this.canvas.appendChild(this.svg);
            this.table = D.html('div', { class: 'ce-table' });
            const body = D.html('div', { class: 'ce-body' }, [this.canvas, this.table]);
            this.status = D.html('div', { class: 'ce-status', 'aria-live': 'polite' });
            this.count = D.html('span', { class: 'ce-count' });
            this.msg = D.html('span', { class: 'ce-message' });
            this.status.append(this.count, this.msg);
            if (this.mode === 'view') {
                el.append(body, this.status);
            } else {
                const note = D.html('p', { class: 'ce-phone-note', text: 'Zum Bauen brauchst du einen größeren Bildschirm, ein Tablet oder einen Computer. Hier kannst du die Schaltung ansehen und die Schalter umlegen.' });
                const keys = D.html('p', { class: 'ce-keys', html: 'Tastatur: <kbd>Tab</kbd> zum nächsten Teil, Pfeiltasten schieben, <kbd>Entf</kbd> löscht, <kbd>V</kbd> und dann eine Ziffer am Ziel ziehen eine Leitung in diesen Eingang, <kbd>Strg</kbd>+<kbd>Z</kbd> nimmt zurück. Mehrere Teile: einen Rahmen um sie ziehen oder mit <kbd>Umschalt</kbd> anklicken, <kbd>Strg</kbd>+<kbd>A</kbd> wählt alle, <kbd>Strg</kbd>+<kbd>D</kbd> verdoppelt. Ein Rechtsklick öffnet ein Menü.' });
                el.append(note, this.palette, this.actions, this.form, body, this.status, keys);
                this.buildActions();
                this.renderPalette();
            }
            this.canvas.addEventListener('pointerdown', (e) => this.pointerDown(e));
            this.svg.addEventListener('keydown', (e) => this.keyDown(e));
            // a finger on a part or a pin moves it, it does not scroll the page
            this.canvas.addEventListener('touchstart', (e) => {
                if (this.canEdit() && e.target.closest && e.target.closest('.ce-movable, .ce-pin-hit')) e.preventDefault();
            }, { passive: false });
            this.svg.setAttribute('tabindex', '-1');
            this.canvas.addEventListener('contextmenu', (e) => this.contextMenu(e));
            // the keys wherever the focus is inside the editor: a part, a
            // button, the paper after a click on a wire or on nothing
            el.addEventListener('keydown', (e) => this.editorKey(e));
        }

        buildActions() {
            const a = this.actions;
            this.undoButton = button('Rückgängig', 'bi-arrow-counterclockwise', { title: 'Rückgängig (Strg+Z)' });
            this.redoButton = button('Wiederholen', 'bi-arrow-clockwise', { title: 'Wiederholen (Strg+Y)' });
            this.deleteButton = button('Löschen', 'bi-trash', { title: 'Das Ausgewählte löschen (Entf)' });
            this.moreButton = button('', 'bi-plus-lg', { title: 'Ein Eingang mehr', 'aria-label': 'Ein Eingang mehr' });
            this.lessButton = button('', 'bi-dash-lg', { title: 'Ein Eingang weniger', 'aria-label': 'Ein Eingang weniger' });
            this.renameButton = button('Umbenennen', 'bi-input-cursor-text');
            this.saveButton = button('Als Baustein speichern', 'bi-box-arrow-in-down');
            this.clearButton = button('Neu anfangen', 'bi-x-circle');
            this.undoButton.addEventListener('click', () => this.undo());
            this.redoButton.addEventListener('click', () => this.redo());
            this.deleteButton.addEventListener('click', () => this.deleteSelection());
            this.moreButton.addEventListener('click', () => this.changeInputs(1));
            this.lessButton.addEventListener('click', () => this.changeInputs(-1));
            this.renameButton.addEventListener('click', () => this.openRename());
            this.saveButton.addEventListener('click', () => this.openSaveBlock());
            this.clearButton.addEventListener('click', () => this.clearAll());
            a.append(this.undoButton, this.redoButton, this.deleteButton, this.lessButton, this.moreButton);
            if (this.mode === 'free') a.append(this.renameButton);
            a.append(D.html('span', { class: 'ce-gap' }));
            a.append(this.saveButton);
            a.append(this.clearButton);
        }

        renderPalette() {
            if (this.mode === 'view') return;
            const p = this.palette;
            const editing = p.classList.contains('ce-editing');
            p.replaceChildren();
            const tool = (label, symbol, make, title) => {
                const spec = make();
                const b = D.html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary ce-tool', title: title || `${label} einsetzen`,
                    'data-tool': spec.kind === 'block' ? `block:${spec.block}` : spec.kind });
                b.append(symbol, D.html('span', { text: label }));
                this.paletteDrag(b, make);
                p.appendChild(b);
                return b;
            };
            if (this.mode === 'free') {
                const sw = D.html('span', { class: 'ce-block-icon', text: '0' });
                tool('Schalter', sw, () => ({ kind: 'in' }), 'Einen Schalter einsetzen, einen Eingang der Schaltung');
                tool('Lampe', icon('bi-lightbulb'), () => ({ kind: 'out' }), 'Eine Lampe einsetzen, einen Ausgang der Schaltung');
            }
            for (const kind of N.GATE_KINDS) {
                if (!this.allowed.includes(kind)) continue;
                tool(N.GATE_NAMES[kind], gateIcon(kind), () => ({ kind }), `Ein ${N.GATE_NAMES[kind]} einsetzen`);
            }
            if (this.blocksAllowed) {
                const lib = Library.load();
                const names = Object.keys(lib).sort();
                if (names.length) {
                    p.appendChild(D.html('span', { class: 'ce-palette-label', text: editing ? 'Bausteine löschen:' : 'Bausteine:' }));
                    for (const name of names) {
                        if (editing) {
                            const del = button(name, 'bi-trash', { class: 'btn btn-sm btn-outline-danger ce-tool', title: `${name} aus deinen Bausteinen löschen` });
                            del.addEventListener('click', () => this.deleteBlock(name));
                            p.appendChild(del);
                        } else {
                            tool(name, icon('bi-box'), () => ({ kind: 'block', block: name }), `Den Baustein ${name} einsetzen`);
                        }
                    }
                    const edit = button('', editing ? 'bi-check-lg' : 'bi-pencil', { title: editing ? 'Fertig' : 'Bausteine löschen', 'aria-label': editing ? 'Fertig' : 'Bausteine löschen', 'aria-pressed': editing ? 'true' : 'false' });
                    edit.addEventListener('click', () => { p.classList.toggle('ce-editing'); this.renderPalette(); });
                    p.appendChild(edit);
                }
            }
        }

        // a palette button: a click puts its part on a free place, dragging
        // puts it where it is dropped on the canvas
        paletteDrag(b, make) {
            let start = null;
            let ghost = null;
            let dragged = false;
            b.addEventListener('pointerdown', (e) => {
                if (!this.canEdit() || e.button > 0) return;
                start = [e.clientX, e.clientY];
                dragged = false;
                b.setPointerCapture(e.pointerId);
            });
            b.addEventListener('pointermove', (e) => {
                if (!start) return;
                if (!ghost && Math.hypot(e.clientX - start[0], e.clientY - start[1]) > 6) {
                    dragged = true;
                    this.snapshot();
                    ghost = this.addPart(make(), this.toModel(e));
                    this.select({ part: ghost.id });
                }
                if (ghost) {
                    const at = this.toModel(e);
                    const g = geom(this.c, ghost);
                    ghost.x = snap(at[0] - g.box[2] / 2 + (ghost.x - g.box[0]));
                    ghost.y = snap(at[1] - g.box[3] / 2 + (ghost.y - g.box[1]));
                    this.render();
                }
            });
            const end = (e) => {
                if (!start) return;
                start = null;
                if (ghost) {
                    const r = this.canvas.getBoundingClientRect();
                    const inside = e && e.clientX >= r.left && e.clientX <= r.right && e.clientY >= r.top && e.clientY <= r.bottom;
                    if (!inside) this.placeFree(ghost);
                    this.keepInside(ghost);
                    ghost = null;
                    this.after(true);
                    this.focusPart(this.sel && this.sel.part);
                }
            };
            b.addEventListener('pointerup', end);
            b.addEventListener('pointercancel', () => { if (ghost) { this.placeFree(ghost); ghost = null; this.after(true); } start = null; });
            b.addEventListener('click', () => {
                if (dragged) { dragged = false; return; }
                if (!this.canEdit()) return;
                this.snapshot();
                const p = this.addPart(make(), null);
                this.placeFree(p);
                this.select({ part: p.id });
                this.after(true);
                this.focusPart(p.id);
            });
        }

        // ------------------------------------------------------------------
        // Changing the circuit

        snapshot() {
            this.history.push(JSON.stringify(this.c));
            if (this.history.length > MAX_HISTORY) this.history.shift();
            this.future = [];
        }

        undo() {
            if (!this.history.length || !this.canEdit()) return;
            this.future.push(JSON.stringify(this.c));
            this.c = JSON.parse(this.history.pop());
            this.sel = null;
            this.pending = null;
            this.after(false);
            this.say('Zurückgenommen.');
        }

        redo() {
            if (!this.future.length || !this.canEdit()) return;
            this.history.push(JSON.stringify(this.c));
            this.c = JSON.parse(this.future.pop());
            this.sel = null;
            this.pending = null;
            this.after(false);
            this.say('Wiederholt.');
        }

        // after a change: simulate, draw, store
        after(animate, store = true) {
            this.rebuild(animate);
            this.render();
            this.syncField();
            if (store) this.saveDraft();
        }

        nextId(prefix) {
            const used = new Set(this.c.parts.map((p) => p.id));
            for (let i = 1; ; i++) if (!used.has(`${prefix}${i}`)) return `${prefix}${i}`;
        }

        freeName(candidates, fallback) {
            const used = new Set(this.c.parts.map((p) => p.id));
            return candidates.find((n) => !used.has(n)) || this.nextId(fallback);
        }

        addPart(spec, at) {
            const p = { kind: spec.kind, src: [], x: 0, y: 0 };
            if (spec.kind === 'in') p.id = this.freeName('abcdefgh'.split(''), 'e');
            else if (spec.kind === 'out') {
                p.id = this.freeName(['y', 'z', 'q', 'r', 's', 't'], 'y');
                p.src = [null];
            } else if (spec.kind === 'block') {
                this.adoptBlock(spec.block);
                p.block = spec.block;
                p.id = this.nextId(spec.block.toLowerCase().replace(/[^a-z]/g, '').slice(0, 3) || 'b');
                p.src = N.pinsIn(this.c.blocks[spec.block]).map(() => null);
            } else {
                p.id = this.nextId('g');
                p.src = spec.kind === 'not' ? [null] : [null, null];
            }
            if (at) {
                p.x = snap(at[0]);
                p.y = snap(at[1]);
            }
            this.c.parts.push(p);
            return p;
        }

        // a block from the library into this circuit, with the blocks inside
        // it; instances of an older copy follow the library's
        adoptBlock(name) {
            const lib = Library.load();
            const take = (n, seen = new Set()) => {
                if (seen.has(n) || !lib[n]) return;
                seen.add(n);
                this.c.blocks[n] = clone(lib[n]);
                for (const p of lib[n].parts) if (p.kind === 'block') take(p.block, seen);
            };
            take(name);
            this.fixBlockPins();
        }

        // instances whose block changed: as many inputs as it has, no
        // source from an output it no longer has
        fixBlockPins(scope = this.c) {
            const byId = new Map(scope.parts.map((p) => [p.id, p]));
            for (const p of scope.parts) {
                if (p.kind === 'block' && this.c.blocks[p.block]) {
                    const n = N.pinsIn(this.c.blocks[p.block]).length;
                    p.src = Array.from({ length: n }, (_, i) => (p.src[i] === undefined ? null : p.src[i]));
                }
                p.src = p.src.map((s) => {
                    if (!s) return s;
                    const [id, port] = s.split('.');
                    const from = byId.get(id);
                    if (!from || from.kind === 'out') return null;
                    if (from.kind === 'block') return this.c.blocks[from.block] && N.pinsOut(this.c.blocks[from.block]).includes(port) ? s : null;
                    return port ? null : s;
                });
            }
        }

        boxes(except) {
            return this.c.parts.filter((p) => p !== except).map((p) => geom(this.c, p).box);
        }

        // the first free place on the grid: switches at the left edge, lamps
        // at the right, gates and blocks column by column from the left
        placeFree(p) {
            const width = Math.max(this.canvas.clientWidth || 640, 400);
            const height = Math.max(this.extent()[1], 260);
            const others = this.boxes(p);
            const g0 = geom(this.c, Object.assign({}, p, { x: 0, y: 0 }));
            const [ox, oy, w, h] = g0.box;
            const free = (x, y) => others.every(([bx, by, bw, bh]) => x + w + 16 < bx || bx + bw + 16 < x || y + h + 16 < by || by + bh + 16 < y);
            // lamps right of everything else
            const right = this.c.parts.filter((q) => q !== p && q.kind !== 'out').reduce((m, q) => Math.max(m, geom(this.c, q).box[0] + geom(this.c, q).box[2]), 120);
            const columns = p.kind === 'in' ? [snap(Math.max(32, 20 + 9 * p.id.length))]
                : p.kind === 'out' ? [snap(Math.min(Math.max(200, right + 72), Math.max(200, width - 80)))]
                    : Array.from({ length: Math.max(1, Math.floor((width - w - 160) / 96)) }, (_, i) => 128 + 96 * i);
            for (const x of columns) {
                for (let y = 24; y + h < height - 8 || (columns.length === 1 && y < 4000); y += 24) {
                    if (free(x, y)) {
                        p.x = snap(x - ox);
                        p.y = snap(y - oy);
                        return;
                    }
                }
            }
            // everything full: below the rest
            p.x = snap(columns[0] - ox);
            p.y = snap(height - oy);
        }

        keepInside(p) {
            const [bx, by] = geom(this.c, p).box;
            const left = labelLeft(p, bx);
            if (left < 4) p.x += snap(4 - left) + SNAP;
            if (by < 8) p.y += snap(8 - by) + SNAP;
        }

        fixed(p) {
            return this.mode === 'task' && (p.kind === 'in' || p.kind === 'out');
        }

        deleteSelection() {
            if (!this.canEdit()) return;
            if (this.many.size) {
                const parts = this.selectedParts();
                const gone = new Set(parts.filter((p) => !this.fixed(p)).map((p) => p.id));
                if (!gone.size) {
                    this.say('Die Schalter und Lampen der Aufgabe bleiben stehen; schieben kannst du sie.', true);
                    return;
                }
                this.snapshot();
                this.c.parts = this.c.parts.filter((q) => !gone.has(q.id));
                for (const q of this.c.parts) q.src = q.src.map((s) => (s && gone.has(s.split('.')[0]) ? null : s));
                this.sel = null;
                this.after(true);
                this.say(`${gone.size} Teile gelöscht.${gone.size < parts.length ? ' Die Schalter und Lampen der Aufgabe bleiben stehen.' : ''}`);
                this.svg.focus({ preventScroll: true });
                return;
            }
            if (!this.sel) return;
            if (this.sel.wire) {
                const [id, i] = this.sel.wire;
                const p = this.part(id);
                if (!p) return;
                this.snapshot();
                p.src[i] = null;
                this.sel = null;
                this.after(true);
                this.say('Leitung gelöscht.');
                this.focusPart(id);
                return;
            }
            const p = this.part(this.sel.part);
            if (!p) return;
            if (this.fixed(p)) {
                this.say('Die Schalter und Lampen der Aufgabe bleiben stehen; schieben kannst du sie.', true);
                return;
            }
            this.snapshot();
            this.c.parts = this.c.parts.filter((q) => q !== p);
            for (const q of this.c.parts) q.src = q.src.map((s) => (s && s.split('.')[0] === p.id ? null : s));
            this.sel = null;
            this.after(true);
            this.say(`${this.describe(p)} gelöscht.`);
            this.svg.focus();
        }

        // the parts selected: several, or the one
        selectedParts() {
            if (this.many.size) return [...this.many].map((id) => this.part(id)).filter(Boolean);
            const p = this.sel && this.sel.part && this.part(this.sel.part);
            return p ? [p] : [];
        }

        selectParts(ids) {
            this.pending = null;
            this.sel = ids.length === 1 ? { part: ids[0] } : null;
            if (ids.length > 1) this.many = new Set(ids);
        }

        sayChosen() {
            const parts = this.selectedParts();
            if (parts.length > 1) this.say(`${parts.length} Teile ausgewählt: Ziehen verschiebt sie zusammen, Entf löscht sie, Strg+D verdoppelt sie.`);
            else if (parts.length === 1) this.say(`${this.describe(parts[0])} ausgewählt.`);
            else this.say('');
        }

        selectAll() {
            if (!this.canEdit()) return;
            this.selectParts(this.c.parts.map((p) => p.id));
            this.render();
            this.sayChosen();
        }

        // the parts selected once more, a little lower right, with the
        // wires between them; the switches and lamps of a task stay single
        duplicate() {
            if (!this.canEdit()) return;
            const parts = this.selectedParts().filter((p) => !this.fixed(p));
            if (!parts.length) {
                this.say('Wähle zuerst Gatter oder Bausteine aus, die du verdoppeln willst.', true);
                return;
            }
            this.snapshot();
            const ids = new Map();
            const copies = parts.map((p) => {
                const q = this.addPart(p.kind === 'block' ? { kind: 'block', block: p.block } : { kind: p.kind });
                q.x = p.x + 6 * SNAP;
                q.y = p.y + 6 * SNAP;
                ids.set(p.id, q.id);
                return [p, q];
            });
            for (const [p, q] of copies) {
                q.src = p.src.map((src) => {
                    if (!src) return null;
                    const [id, port] = src.split('.');
                    return ids.has(id) ? ids.get(id) + (port ? `.${port}` : '') : null;
                });
                this.keepInside(q);
            }
            this.selectParts(copies.map(([, q]) => q.id));
            this.after(true);
            this.say(copies.length === 1 ? `${this.describe(copies[0][1])} verdoppelt.` : `${copies.length} Teile verdoppelt, mit den Leitungen zwischen ihnen.`);
        }

        // Entf, Strg+Z and the others wherever the focus is in the editor;
        // what a focused part does with a key (keyDown) comes first
        editorKey(e) {
            if (e.defaultPrevented || e.target.closest('input, textarea, select, .ce-form')) return;
            if (this.menu && e.key === 'Escape') {
                e.preventDefault();
                this.closeMenu();
                this.svg.focus({ preventScroll: true });
                return;
            }
            if (!this.canEdit()) return;
            const key = e.key.toLowerCase();
            if (e.key === 'ContextMenu' || (e.shiftKey && e.key === 'F10')) {
                e.preventDefault();
                const target = document.activeElement && this.svg.contains(document.activeElement) ? document.activeElement : this.svg;
                const r = target.getBoundingClientRect();
                this.contextMenu({ target, clientX: r.left + r.width / 2, clientY: r.top + r.height / 2, preventDefault() {} });
                return;
            }
            if (e.ctrlKey || e.metaKey) {
                if (key === 'z' && !e.shiftKey) { e.preventDefault(); this.undo(); }
                else if (key === 'y' || (key === 'z' && e.shiftKey)) { e.preventDefault(); this.redo(); }
                else if (key === 'a' && !e.target.closest('button')) { e.preventDefault(); this.selectAll(); }
                else if (key === 'd') { e.preventDefault(); this.duplicate(); }
                return;
            }
            if (e.altKey || e.target.closest('button')) return;
            if ((key === 'delete' || key === 'backspace') && (this.sel || this.many.size)) {
                e.preventDefault();
                this.deleteSelection();
            } else if (key === 'escape' && (this.sel || this.many.size || this.pending)) {
                this.pending = null;
                this.sel = null;
                this.render();
                this.say('');
            } else if (key.startsWith('arrow') && this.many.size) {
                e.preventDefault();
                this.moveParts(this.selectedParts(), key, e.shiftKey);
            }
        }

        moveParts(parts, key, far) {
            const step = far ? 4 * SNAP : SNAP;
            this.snapshot();
            for (const p of parts) {
                if (key === 'arrowleft') p.x -= step;
                if (key === 'arrowright') p.x += step;
                if (key === 'arrowup') p.y -= step;
                if (key === 'arrowdown') p.y += step;
                this.keepInside(p);
            }
            this.after(false);
        }

        // ------------------------------------------------------------------
        // The menu on a right click: what fits the part, the wire or the
        // empty paper under the pointer

        contextMenu(e) {
            if (!this.canEdit()) return;
            e.preventDefault();
            const partEl = e.target.closest && e.target.closest('[data-part]');
            const wire = e.target.closest && e.target.closest('[data-wire]');
            const items = [];
            if (partEl) {
                const id = partEl.dataset.part;
                if (!this.many.has(id) && !(this.sel && this.sel.part === id)) this.sel = { part: id };
                this.render();
                const parts = this.selectedParts();
                const editable = parts.some((p) => !this.fixed(p));
                items.push(['Verdoppeln', 'bi-copy', 'Strg+D', () => this.duplicate(), editable]);
                if (parts.length === 1) {
                    const p = parts[0];
                    if (GATES[p.kind] && p.kind !== 'not') {
                        items.push(['Ein Eingang mehr', 'bi-plus-lg', '+', () => this.changeInputs(1), p.src.length < N.LIMITS.maxInputs]);
                        items.push(['Ein Eingang weniger', 'bi-dash-lg', '−', () => this.changeInputs(-1), p.src.length > 2]);
                    }
                    if (this.mode === 'free' && (p.kind === 'in' || p.kind === 'out')) items.push(['Umbenennen', 'bi-input-cursor-text', '', () => this.openRename(), true]);
                }
                items.push(null);
                items.push([parts.length > 1 ? `${parts.length} Teile löschen` : 'Löschen', 'bi-trash', 'Entf', () => this.deleteSelection(), editable]);
                this.sayChosen();
            } else if (wire) {
                const [wid, wi] = wire.dataset.wire.split(':');
                this.sel = { wire: [wid, Number(wi)] };
                this.render();
                items.push(['Leitung löschen', 'bi-trash', 'Entf', () => this.deleteSelection(), true]);
            } else {
                items.push(['Rückgängig', 'bi-arrow-counterclockwise', 'Strg+Z', () => this.undo(), this.history.length > 0]);
                items.push(['Wiederholen', 'bi-arrow-clockwise', 'Strg+Y', () => this.redo(), this.future.length > 0]);
                items.push(['Alles auswählen', 'bi-bounding-box', 'Strg+A', () => this.selectAll(), this.c.parts.length > 0]);
                items.push(null);
                items.push(['Neu anfangen', 'bi-x-circle', '', () => this.clearAll(), this.c.parts.length > 0]);
            }
            this.openMenu(e.clientX, e.clientY, items);
        }

        openMenu(x, y, items) {
            this.closeMenu();
            const menu = D.html('div', { class: 'dropdown-menu ce-menu', role: 'menu' });
            for (const item of items) {
                if (!item) {
                    if (menu.lastChild) menu.appendChild(D.html('hr', { class: 'dropdown-divider' }));
                    continue;
                }
                const [label, iconName, keys, action, enabled] = item;
                const b = D.html('button', { type: 'button', class: `dropdown-item${enabled ? '' : ' disabled'}`, role: 'menuitem' });
                if (!enabled) b.setAttribute('aria-disabled', 'true');
                b.append(icon(iconName), D.html('span', { text: label }));
                if (keys) b.appendChild(D.html('span', { class: 'ce-menu-key', text: keys }));
                b.addEventListener('click', () => {
                    if (!enabled) return;
                    this.closeMenu();
                    action();
                });
                menu.appendChild(b);
            }
            document.body.appendChild(menu);
            const r = menu.getBoundingClientRect();
            menu.style.left = `${Math.max(4, Math.min(x, window.innerWidth - r.width - 4))}px`;
            menu.style.top = `${Math.max(4, Math.min(y, window.innerHeight - r.height - 4))}px`;
            const buttons = () => [...menu.querySelectorAll('.dropdown-item:not(.disabled)')];
            menu.addEventListener('keydown', (e) => {
                const list = buttons();
                const i = list.indexOf(document.activeElement);
                if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
                    e.preventDefault();
                    const next = list[(i + (e.key === 'ArrowDown' ? 1 : list.length - 1)) % list.length];
                    if (next) next.focus();
                } else if (e.key === 'Escape' || e.key === 'Tab') {
                    e.preventDefault();
                    this.closeMenu();
                    this.svg.focus({ preventScroll: true });
                }
            });
            const outside = (e) => { if (!menu.contains(e.target)) this.closeMenu(); };
            const away = () => this.closeMenu();
            document.addEventListener('pointerdown', outside, true);
            window.addEventListener('scroll', away, { passive: true, once: true });
            window.addEventListener('resize', away, { once: true });
            this.menu = { menu, outside, away };
            const first = buttons()[0];
            if (first) first.focus({ preventScroll: true });
        }

        closeMenu() {
            if (!this.menu) return;
            const { menu, outside, away } = this.menu;
            this.menu = null;
            document.removeEventListener('pointerdown', outside, true);
            window.removeEventListener('scroll', away);
            window.removeEventListener('resize', away);
            menu.remove();
        }

        changeInputs(delta) {
            const p = this.sel && this.sel.part && this.part(this.sel.part);
            if (!this.canEdit() || !p || !GATES[p.kind] || p.kind === 'not') return;
            const n = p.src.length + delta;
            if (n < 2 || n > N.LIMITS.maxInputs) return;
            this.snapshot();
            if (delta > 0) p.src.push(null);
            else p.src.pop();
            this.after(true);
            this.focusPart(p.id);
        }

        clearAll() {
            if (!this.canEdit()) return;
            const go = () => {
                this.snapshot();
                const keep = this.mode === 'task' ? this.startCircuit() : { parts: [], blocks: {} };
                // the task's switches and lamps stay where they were
                for (const p of keep.parts) {
                    const old = this.part(p.id);
                    if (old) { p.x = old.x; p.y = old.y; }
                }
                this.c = keep;
                this.sel = null;
                this.after(true);
                this.say('Neu angefangen. Rückgängig holt die Schaltung zurück.');
            };
            if (typeof window.confirm_dialog === 'function') {
                window.confirm_dialog({ title: 'Neu anfangen?', text: 'Alle Gatter und Leitungen dieser Schaltung werden gelöscht. Mit »Rückgängig« holst du sie zurück, solange die Seite offen ist.', confirm: 'Neu anfangen', icon: 'bi-x-circle', danger: true }, go);
            } else go();
        }

        connect(fromId, port, toId, index) {
            const to = this.part(toId);
            const from = this.part(fromId);
            if (!to || !from) return false;
            this.snapshot();
            to.src[index] = sourceOf(from, port);
            this.pending = null;
            this.sel = null;
            this.after(true);
            this.say(`Leitung von ${this.pinName(from, port, true)} nach ${this.pinName(to, index, false)}.`);
            return true;
        }

        part(id) {
            return this.c.parts.find((p) => p.id === id);
        }

        describe(p) {
            if (p.kind === 'in') return `Schalter ${p.id}`;
            if (p.kind === 'out') return `Lampe ${p.id}`;
            if (p.kind === 'block') return `Baustein ${p.block} (${p.id})`;
            return `${N.GATE_NAMES[p.kind]} ${p.id}`;
        }

        pinName(p, pin, out) {
            if (out) return p.kind === 'block' ? `${p.id}.${pin}` : p.id;
            if (p.kind === 'out') return `Lampe ${p.id}`;
            if (p.kind === 'block') return `${p.id}, Eingang ${geom(this.c, p).inNames[pin]}`;
            return `${p.id}, Eingang ${pin + 1}`;
        }

        // ------------------------------------------------------------------
        // Blocks

        openSaveBlock() {
            if (!this.canEdit()) return;
            const ins = this.c.parts.filter((p) => p.kind === 'in');
            const outs = this.c.parts.filter((p) => p.kind === 'out');
            if (!ins.length || !outs.length) {
                this.say('Ein Baustein braucht mindestens einen Schalter und eine Lampe: Sie werden seine Ein- und Ausgänge.', true);
                return;
            }
            this.form.replaceChildren();
            const id = `ce-name-${Math.random().toString(36).slice(2, 8)}`;
            const label = D.html('label', { for: id, text: 'Name des Bausteins:' });
            const input = D.html('input', { id, class: 'form-control form-control-sm', maxlength: '12', placeholder: 'etwa HA', autocomplete: 'off', spellcheck: 'false' });
            const ok = button('Speichern', 'bi-check-lg', { class: 'btn btn-sm btn-success' });
            const cancel = button('Abbrechen', 'bi-x-lg');
            const error = D.html('div', { class: 'ce-form-error', 'aria-live': 'polite' });
            const pins = D.html('div', { class: 'ce-form-hint text-body-secondary small', text: `Eingänge ${ins.sort((a, b) => a.y - b.y).map((p) => p.id).join(', ')}, Ausgänge ${outs.sort((a, b) => a.y - b.y).map((p) => p.id).join(', ')}, von oben nach unten.` });
            pins.style.flexBasis = '100%';
            const submit = () => {
                const message = this.saveBlock(input.value.trim());
                if (message) error.textContent = message;
                else this.form.hidden = true;
            };
            ok.addEventListener('click', submit);
            cancel.addEventListener('click', () => { this.form.hidden = true; });
            input.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') { e.preventDefault(); submit(); }
                if (e.key === 'Escape') { e.preventDefault(); this.form.hidden = true; }
            });
            this.form.append(label, input, ok, cancel, pins, error);
            this.form.hidden = false;
            input.focus();
        }

        // stores the circuit as a block; a message when it cannot be
        saveBlock(name) {
            if (!N.BLOCK_NAME.test(name) || N.KEYWORDS.includes(name.toLowerCase())) {
                return 'Ein Name aus Buchstaben und Ziffern, vorn ein Buchstabe, höchstens 12 Zeichen, etwa HA oder VA.';
            }
            const inside = N.usedBlocks(this.c);
            if (inside.has(name)) return `Deine Schaltung benutzt den Baustein ${name} schon. Ein Baustein kann sich nicht selbst enthalten: Nimm einen anderen Namen.`;
            const lib = Library.load();
            const save = () => {
                const order = (a, b) => a.y - b.y || a.x - b.x;
                const ins = this.c.parts.filter((p) => p.kind === 'in').sort(order);
                const outs = this.c.parts.filter((p) => p.kind === 'out').sort(order);
                const rest = this.c.parts.filter((p) => p.kind !== 'in' && p.kind !== 'out');
                for (const n of inside) lib[n] = clone(this.c.blocks[n]);
                lib[name] = { parts: clone([...ins, ...rest, ...outs]) };
                // other blocks in the library that used an older one keep
                // working by name; the circuit's own copies follow
                if (this.c.blocks[name]) this.c.blocks[name] = clone(lib[name]);
                Library.save(lib);
                this.say(`Gespeichert: Der Baustein ${name} steht jetzt oben bei den Bausteinen, auch in den anderen Editoren der Seite.`);
            };
            if (lib[name]) {
                if (typeof window.confirm_dialog === 'function') {
                    window.confirm_dialog({ title: `${name} ersetzen?`, text: `Es gibt schon einen Baustein ${name}. Der neue ersetzt ihn, auch dort, wo du ihn das nächste Mal einsetzt.`, confirm: 'Ersetzen', icon: 'bi-box-arrow-in-down' }, () => { save(); this.form.hidden = true; });
                    return 'Es gibt schon einen Baustein mit diesem Namen.';
                }
            }
            save();
            return null;
        }

        deleteBlock(name) {
            const lib = Library.load();
            const users = Object.keys(lib).filter((n) => n !== name && lib[n].parts.some((p) => p.kind === 'block' && p.block === name));
            if (users.length) {
                this.say(`${name} steckt in ${users.join(', ')}; lösch zuerst ${users.length === 1 ? 'diesen Baustein' : 'diese Bausteine'}.`, true);
                return;
            }
            const go = () => {
                delete lib[name];
                Library.save(lib);
                this.say(`Den Baustein ${name} gibt es nicht mehr bei deinen Bausteinen. Schaltungen, in denen er steckt, behalten ihn.`);
            };
            if (typeof window.confirm_dialog === 'function') {
                window.confirm_dialog({ title: `${name} löschen?`, text: `Der Baustein ${name} verschwindet aus der Leiste. Schaltungen, in denen er schon steckt, behalten ihre Kopie.`, confirm: 'Löschen', icon: 'bi-trash', danger: true }, go);
            } else go();
        }

        openRename() {
            const p = this.sel && this.sel.part && this.part(this.sel.part);
            if (!this.canEdit() || !p || (p.kind !== 'in' && p.kind !== 'out') || this.fixed(p)) {
                this.say('Wähle zuerst einen Schalter oder eine Lampe aus.', true);
                return;
            }
            this.form.replaceChildren();
            const id = `ce-rename-${Math.random().toString(36).slice(2, 8)}`;
            const input = D.html('input', { id, class: 'form-control form-control-sm', maxlength: '16', value: p.id, autocomplete: 'off', spellcheck: 'false' });
            const error = D.html('div', { class: 'ce-form-error', 'aria-live': 'polite' });
            const ok = button('Umbenennen', 'bi-check-lg', { class: 'btn btn-sm btn-success' });
            const cancel = button('Abbrechen', 'bi-x-lg');
            const submit = () => {
                const name = input.value.trim();
                if (name === p.id) { this.form.hidden = true; return; }
                if (!N.NAME.test(name)) { error.textContent = 'Buchstaben, Ziffern und _, vorn ein Buchstabe, etwa a, c_in oder s0.'; return; }
                if (this.part(name)) { error.textContent = `Den Namen ${name} gibt es in dieser Schaltung schon.`; return; }
                this.snapshot();
                const old = p.id;
                p.id = name;
                for (const q of this.c.parts) q.src = q.src.map((s) => (s === old ? name : s));
                if (this.inputs[old] !== undefined) { this.inputs[name] = this.inputs[old]; delete this.inputs[old]; }
                this.sel = { part: name };
                this.form.hidden = true;
                this.after(false);
                this.focusPart(name);
            };
            ok.addEventListener('click', submit);
            cancel.addEventListener('click', () => { this.form.hidden = true; });
            input.addEventListener('keydown', (e) => {
                if (e.key === 'Enter') { e.preventDefault(); submit(); }
                if (e.key === 'Escape') { e.preventDefault(); this.form.hidden = true; }
            });
            this.form.append(D.html('label', { for: id, text: `Neuer Name für ${this.describe(p)}:` }), input, ok, cancel, error);
            this.form.hidden = false;
            input.focus();
            input.select();
        }

        // ------------------------------------------------------------------
        // Simulation

        rebuild(animate) {
            if (this.timer) { clearTimeout(this.timer); this.timer = null; }
            this.error = null;
            try {
                this.flat = N.flatten(this.c);
            } catch (e) {
                this.flat = null;
                this.error = e.message;
                this.values = {};
                return;
            }
            const start = LC.initial(this.flat.circuit);
            for (const k of Object.keys(start)) if (this.values[k] !== undefined) start[k] = this.values[k];
            for (const p of this.c.parts) if (p.kind === 'in') start[p.id] = this.inputs[p.id] || 0;
            this.run(start, animate);
            this.feedback = N.hasFeedback(this.flat);
            this.renderTable();
        }

        run(start, animate) {
            if (this.timer) { clearTimeout(this.timer); this.timer = null; }
            if (!this.flat) return;
            const result = LC.settle(this.flat.circuit, start);
            this.stable = result.stable;
            const rounds = result.stable ? result.rounds : result.rounds.slice(0, LC.SHOWN_ROUNDS + 1);
            if (!animate || reduced() || rounds.length <= 1) {
                this.values = rounds[rounds.length - 1];
                this.paint();
                return;
            }
            let k = 0;
            const next = () => {
                this.values = rounds[k];
                this.paint(k < rounds.length - 1);
                if (k < rounds.length - 1) {
                    k += 1;
                    this.timer = setTimeout(next, LC.ROUND_MS);
                } else this.timer = null;
            };
            next();
        }

        toggle(name) {
            const p = this.part(name);
            if (!p || p.kind !== 'in' || !this.flat) return;
            this.inputs[name] = this.inputs[name] ? 0 : 1;
            const start = Object.assign({}, this.values, { [name]: this.inputs[name] });
            this.run(start, true);
        }

        setInputs(bits) {
            if (!this.flat) return;
            const start = Object.assign({}, this.values);
            for (const [name, bit] of Object.entries(bits)) {
                this.inputs[name] = bit;
                start[name] = bit;
            }
            this.run(start, true);
        }

        valueOf(src) {
            if (!src || !this.flat) return 0;
            return this.values[this.flat.resolve(src)] || 0;
        }

        // ------------------------------------------------------------------
        // Drawing

        // While building, the drawing has its own size, everything drawn and
        // a margin (viewBoxOf), and the canvas around it with the grid fills
        // the rest of the line, or scrolls; the table goes below when both
        // do not fit side by side. Only to look at (a phone, a circuit on
        // the page), it is as wide as there is room for.
        fitCanvas() {
            const [left, top, w, h] = viewBoxOf(this.c, this.mode === 'view' ? [200, 0] : [320, 260]);
            this.svg.setAttribute('viewBox', `${left} ${top} ${w} ${h}`);
            if (this.canEdit()) {
                this.svg.setAttribute('width', w);
                this.svg.setAttribute('height', h);
                this.svg.style.width = '';
                this.canvas.style.width = '';
            } else {
                this.svg.removeAttribute('width');
                this.svg.removeAttribute('height');
                this.svg.style.width = '100%';
                this.svg.style.height = 'auto';
                this.canvas.style.width = `${w}px`;
            }
        }

        extent() {
            return extentOf(this.c);
        }

        render() {
            const s = this.svg;
            s.replaceChildren();
            this.canvas.classList.toggle('ce-grid', this.canEdit());
            this.fitCanvas();
            const layers = { wires: D.svg('g', {}, s), parts: D.svg('g', {}, s), pins: D.svg('g', {}, s), over: D.svg('g', {}, s) };
            const c = this.c;
            const geoms = new Map(c.parts.map((p) => [p.id, geom(c, p)]));
            this.geoms = geoms;
            this.wireEls = [];
            this.dotEls = [];
            this.lampEls = [];
            this.switchEls = [];
            this.pinEls = [];
            // wires, each in a lane of its own where it would lie on another
            const list = wiresOf(c);
            for (const w of list) {
                const d = D.pathOf(w.points);
                const selected = this.sel && this.sel.wire && this.sel.wire[0] === w.to[0] && this.sel.wire[1] === w.to[1];
                const el = D.svg('path', { class: `lc-wire${selected ? ' ce-selected' : ''}`, d }, layers.wires);
                this.wireEls.push({ src: w.src, el });
                if (this.canEdit()) {
                    const hit = D.svg('path', { class: 'ce-wire-hit', d, 'data-wire': `${w.to[0]}:${w.to[1]}` }, layers.wires);
                    hit.appendChild(D.svg('title', {})).textContent = 'Leitung auswählen';
                }
            }
            for (const { p, src } of LC.junctions(list)) {
                this.dotEls.push({ src, el: D.svg('circle', { class: 'lc-dot', cx: p[0], cy: p[1], r: 3.5 }, layers.wires) });
            }
            // parts
            for (const p of c.parts) {
                const g = geoms.get(p.id);
                const movable = this.canEdit();
                const selected = (this.sel && this.sel.part === p.id) || this.many.has(p.id);
                const el = D.svg('g', { class: `ce-part${movable ? ' ce-movable' : ''}${selected ? ' ce-selected' : ''}`, 'data-part': p.id, tabindex: '0' }, layers.parts);
                const [bx, by, bw, bh] = g.box;
                D.svg('rect', { class: 'ce-focus', x: bx - 5, y: by - 5, width: bw + 10, height: bh + 10, rx: 6 }, el);
                if (p.kind === 'in') {
                    const sw = D.svg('g', { class: 'lc-switch' }, el);
                    D.svg('rect', { class: 'ce-hit', x: p.x - 22, y: p.y - 22, width: 44, height: 44 }, sw);
                    D.svg('rect', { class: 'lc-key', x: p.x - SWITCH_W / 2, y: p.y - SWITCH_H / 2, width: SWITCH_W, height: SWITCH_H, rx: 5 }, sw);
                    const value = D.text(sw, p.x, p.y + 1, '0');
                    D.text(el, p.x - SWITCH_W / 2 - 8, p.y, p.id, { class: 'lc-label', 'text-anchor': 'end' });
                    el.setAttribute('role', 'button');
                    this.switchEls.push({ id: p.id, g: sw, value, el });
                } else if (p.kind === 'out') {
                    const lamp = D.svg('g', { class: 'lc-lamp' }, el);
                    D.svg('circle', { cx: p.x, cy: p.y, r: LAMP }, lamp);
                    const value = D.text(lamp, p.x, p.y + 1, '0');
                    D.text(el, p.x + LAMP + 7, p.y, p.id, { class: 'lc-label' });
                    this.lampEls.push({ src: p.src[0], g: lamp, value, el, id: p.id });
                } else if (p.kind === 'block') {
                    const b = D.svg('g', { class: 'lc-block' }, el);
                    D.svg('rect', { class: 'lc-box', x: bx, y: by, width: bw, height: bh }, b);
                    D.text(b, bx + bw / 2, by + BLOCK_HEAD / 2 + 1, p.block, { class: 'ce-block-name' });
                    g.inNames.forEach((name, i) => D.text(b, bx + 5, g.ins[i][1], name, { class: 'lc-pin-label' }));
                    g.outNames.forEach((name) => D.text(b, bx + bw - 5, g.outs[name][1], name, { class: 'lc-pin-label', 'text-anchor': 'end' }));
                    D.svg('rect', { class: 'ce-hit', x: bx, y: by, width: bw, height: bh }, el);
                } else {
                    D.gate(el, p.x, p.y, p.kind, p.src.length);
                    D.svg('rect', { class: 'ce-hit', x: bx, y: by, width: GW, height: bh }, el);
                    if (this.canEdit()) D.text(el, p.x + GW / 2, by + bh + 11, p.id, { class: 'ce-id' });
                }
                el.setAttribute('aria-label', this.ariaLabel(p));
            }
            // pins: open inputs as rings; while wiring, the pins a wire can go to
            const pendingFrom = this.pending && this.pending.from;
            const pendingTo = this.pending && this.pending.to;
            for (const p of c.parts) {
                const g = geoms.get(p.id);
                g.ins.forEach(([x, y], i) => {
                    const open = !p.src[i];
                    const target = !!pendingFrom;
                    const source = pendingTo && pendingTo[0] === p.id && pendingTo[1] === i;
                    const el = D.svg('circle', { class: `ce-pin${open ? ' ce-open' : ''}${target ? ' ce-target' : ''}${source ? ' ce-source' : ''}`, cx: x, cy: y, r: target ? 4.5 : 3.2 }, layers.pins);
                    if (!open) this.pinEls.push({ src: p.src[i], el });
                    if (this.canEdit()) {
                        D.svg('circle', { class: 'ce-pin-hit', cx: x, cy: y, r: coarse() ? 15 : 10, 'data-pin': 'in', 'data-part': p.id, 'data-index': String(i) }, layers.pins)
                            .appendChild(D.svg('title', {})).textContent = `${this.pinName(p, i, false)}: hier eine Leitung anschließen`;
                    }
                });
                for (const [port, [x, y]] of Object.entries(g.outs)) {
                    const target = !!pendingTo;
                    const source = pendingFrom && pendingFrom[0] === p.id && pendingFrom[1] === port;
                    const el = D.svg('circle', { class: `ce-pin${target ? ' ce-target' : ''}${source ? ' ce-source' : ''}`, cx: x, cy: y, r: target ? 4.5 : 3.2 }, layers.pins);
                    this.pinEls.push({ src: sourceOf(p, port), el });
                    if (this.canEdit()) {
                        D.svg('circle', { class: 'ce-pin-hit', cx: x, cy: y, r: coarse() ? 15 : 10, 'data-pin': 'out', 'data-part': p.id, 'data-port': port }, layers.pins)
                            .appendChild(D.svg('title', {})).textContent = `Ausgang ${this.pinName(p, port, true)}: von hier eine Leitung ziehen`;
                    }
                }
            }
            this.overLayer = layers.over;
            this.paint();
            this.renderCount();
            this.renderActions();
        }

        ariaLabel(p) {
            if (p.kind === 'in') return `Schalter ${p.id}: ${this.inputs[p.id] ? 1 : 0}${this.canEdit() ? ', Leertaste schaltet um' : ', umschalten'}`;
            const from = p.src.map((s) => s || 'keine Leitung').join(', ');
            if (p.kind === 'out') return `Lampe ${p.id} an ${from}`;
            return `${this.describe(p)}, Eingänge: ${from}`;
        }

        paint(running) {
            if (!this.wireEls) return;
            for (const { src, el } of this.wireEls) el.classList.toggle('lc-one', this.valueOf(src) !== 0);
            for (const { src, el } of this.dotEls) el.classList.toggle('lc-one', this.valueOf(src) !== 0);
            for (const { src, el } of this.pinEls) el.classList.toggle('lc-one', this.valueOf(src) !== 0);
            for (const s of this.switchEls) {
                const on = !!this.inputs[s.id];
                s.g.classList.toggle('lc-one', on);
                s.value.textContent = on ? '1' : '0';
                s.el.setAttribute('aria-pressed', on ? 'true' : 'false');
                s.el.setAttribute('aria-label', this.ariaLabel(this.part(s.id)));
            }
            for (const l of this.lampEls) {
                const on = this.valueOf(l.src) !== 0;
                l.g.classList.toggle('lc-one', on);
                l.value.textContent = on ? '1' : '0';
            }
            if (this.tableRows) {
                const names = this.tableInputs;
                for (const { tr, row } of this.tableRows) tr.classList.toggle('lc-now', row.inputs.every((x, i) => x === (this.inputs[names[i]] || 0)));
            }
            if (this.error) this.say(this.error, true);
            else if (!running && this.stable === false) this.say('Die Schaltung kommt nicht zur Ruhe: Gatter, die sich im Kreis gegenseitig umschalten, ändern sich in jeder Runde. Leg einen Schalter um, dann sieh nach.', true);
            else if (!running && this.unstableSaid) this.say('');
            this.unstableSaid = !running && this.stable === false;
        }

        renderCount() {
            if (this.mode === 'view' || !this.flat) {
                this.count.textContent = '';
                return;
            }
            const n = Object.values(this.flat.gates).reduce((a, b) => a + b, 0);
            let text = n === 1 ? '1 Gatter' : `${n} Gatter`;
            if (this.flat.inner) text += `, ${this.flat.inner} davon in Bausteinen`;
            if (this.maxGates) text += `, erlaubt sind höchstens ${this.maxGates}`;
            if (this.minBlocks) text += `; Bausteine: ${this.flat.blocks} von mindestens ${this.minBlocks}`;
            const size = this.field ? N.format(this.pruned()).length : 0;
            if (size > MAX_TEXT) text += '. Zu groß zum Prüfen: Entferne Teile, die du nicht brauchst.';
            this.count.textContent = `${text}.`;
            this.count.classList.toggle('ce-over', (this.maxGates && n > this.maxGates) || size > MAX_TEXT);
        }

        renderActions() {
            if (this.mode === 'view') return;
            const can = this.canEdit();
            const p = this.sel && this.sel.part && this.part(this.sel.part);
            const set = (b, on) => { if (b) b.setAttribute('aria-disabled', on && can ? 'false' : 'true'); };
            set(this.undoButton, this.history.length > 0);
            set(this.redoButton, this.future.length > 0);
            set(this.deleteButton, !!(this.sel && (this.sel.wire || (p && !this.fixed(p)))) || this.selectedParts().some((q) => this.many.size && !this.fixed(q)));
            const gate = p && GATES[p.kind] && p.kind !== 'not';
            set(this.moreButton, gate && p.src.length < N.LIMITS.maxInputs);
            set(this.lessButton, gate && p.src.length > 2);
            this.moreButton.hidden = !gate;
            this.lessButton.hidden = !gate;
            set(this.renameButton, p && (p.kind === 'in' || p.kind === 'out'));
            set(this.saveButton, true);
            set(this.clearButton, true);
            this.palette.querySelectorAll('.ce-tool').forEach((b) => b.setAttribute('aria-disabled', can ? 'false' : 'true'));
        }

        renderTable() {
            const t = this.table;
            t.replaceChildren();
            this.tableRows = null;
            if (!this.flat) return;
            const ins = this.c.parts.filter((p) => p.kind === 'in');
            const outs = this.c.parts.filter((p) => p.kind === 'out');
            if (!ins.length || !outs.length) return;
            if (this.feedback) {
                if (this.mode !== 'view') t.appendChild(D.html('p', { class: 'ce-table-note', text: 'Eine Leitung führt zurück: Die Schaltung merkt sich etwas, eine Wahrheitstabelle hat sie nicht. Probier die Schalter nacheinander aus.' }));
                return;
            }
            if (ins.length > TABLE_INPUTS) {
                if (this.mode !== 'view') t.appendChild(D.html('p', { class: 'ce-table-note', text: `Mit ${ins.length} Schaltern hätte die Wahrheitstabelle ${1 << ins.length} Zeilen; hier steht sie nur bis ${TABLE_INPUTS} Schalter.` }));
                return;
            }
            const table = LC.truthTable(this.flat.circuit);
            this.tableInputs = table.inputs;
            const head = D.html('tr', {}, [...table.inputs.map((n) => D.html('th', { text: n })), ...table.outputs.map((n, i) => D.html('th', { class: i === 0 ? 'lc-gap' : null, text: n }))]);
            this.tableRows = table.rows.map((row) => {
                const tr = D.html('tr', { tabindex: '0', title: 'Diese Schalter einstellen' },
                    [...row.inputs.map((x) => D.html('td', { text: String(x) })), ...row.outputs.map((x, i) => D.html('td', { class: i === 0 ? 'lc-gap' : null, text: String(x) }))]);
                const pick = () => this.setInputs(Object.fromEntries(table.inputs.map((n, i) => [n, row.inputs[i]])));
                tr.addEventListener('click', pick);
                tr.addEventListener('keydown', (e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); pick(); } });
                return { tr, row };
            });
            t.append(D.html('p', { class: 'ce-table-title', text: this.mode === 'view' ? 'Wahrheitstabelle' : 'Deine Wahrheitstabelle' }),
                D.html('table', { 'aria-label': 'Wahrheitstabelle der Schaltung' }, [D.html('thead', {}, [head]), D.html('tbody', {}, this.tableRows.map((r) => r.tr))]));
        }

        say(text, warn = false) {
            if (!this.msg) return;
            this.msg.textContent = text;
            this.msg.classList.toggle('ce-warn', !!warn);
        }

        select(sel) {
            this.sel = sel;
        }

        focusPart(id) {
            if (!id) return;
            const el = this.svg.querySelector(`[data-part="${CSS.escape(id)}"]`);
            if (el) el.focus({ preventScroll: true });
        }

        // ------------------------------------------------------------------
        // The text: the hidden field of the card, and the draft

        // the circuit with only the blocks it uses
        pruned() {
            const used = N.usedBlocks(this.c);
            const blocks = {};
            for (const n of used) blocks[n] = this.c.blocks[n];
            return { parts: this.c.parts, blocks };
        }

        text() {
            return N.format(this.pruned());
        }

        syncField() {
            if (this.field) this.field.value = this.text();
        }

        saveDraft() {
            if (!this.draftKey || this.mode === 'view') return;
            storage.set(this.storageKey(), { text: this.text(), base: this.base, at: Date.now() });
        }

        // ------------------------------------------------------------------
        // Pointer and keys

        toModel(e) {
            const m = this.svg.getScreenCTM();
            if (!m) return [0, 0];
            const pt = this.svg.createSVGPoint();
            pt.x = e.clientX;
            pt.y = e.clientY;
            const q = pt.matrixTransform(m.inverse());
            return [q.x, q.y];
        }

        // the pin nearest to a point that a wire begun at pending can end in
        nearestPin(at, pending) {
            const radius = coarse() ? 26 : 16;
            let best = null;
            for (const p of this.c.parts) {
                const g = this.geoms.get(p.id) || geom(this.c, p);
                if (pending.from) {
                    g.ins.forEach(([x, y], i) => {
                        const d = Math.hypot(x - at[0], y - at[1]);
                        if (d <= radius && (!best || d < best.d)) best = { d, id: p.id, index: i };
                    });
                } else {
                    for (const [port, [x, y]] of Object.entries(g.outs)) {
                        const d = Math.hypot(x - at[0], y - at[1]);
                        if (d <= radius && (!best || d < best.d)) best = { d, id: p.id, port };
                    }
                }
            }
            return best;
        }

        finishWire(pending, hit) {
            if (pending.from) return this.connect(pending.from[0], pending.from[1], hit.id, hit.index);
            return this.connect(hit.id, hit.port, pending.to[0], pending.to[1]);
        }

        pointerDown(e) {
            if (e.button > 0) return;
            this.closeMenu();
            const at = this.toModel(e);
            const pin = e.target.closest('[data-pin]');
            const wire = e.target.closest('[data-wire]');
            const partEl = e.target.closest('[data-part]:not([data-pin])');
            const id = partEl && partEl.dataset.part;
            // view only (a phone, a page to look at): switches toggle
            if (!this.canEdit()) {
                const p = id && this.part(id);
                if (p && p.kind === 'in') this.toggle(p.id);
                return;
            }
            // a wire begun by tapping a pin: the next pin ends it
            if (this.pending && !this.drag) {
                const hit = this.nearestPin(at, this.pending);
                const pending = this.pending;
                this.pending = null;
                if (hit && this.finishWire(pending, hit)) { e.preventDefault(); return; }
                this.say('');
                this.render();
                if (!pin) return;
            }
            if (pin) {
                e.preventDefault();
                const p = this.part(pin.dataset.part);
                const pending = pin.dataset.pin === 'out' ? { from: [p.id, pin.dataset.port] } : { to: [p.id, Number(pin.dataset.index)] };
                const g = this.geoms.get(p.id);
                const origin = pending.from ? g.outs[pending.from[1]] : g.ins[pending.to[1]];
                this.drag = { kind: 'wire', pending, origin, start: [e.clientX, e.clientY], moved: false, pointer: e.pointerId };
                this.canvas.setPointerCapture(e.pointerId);
                this.bindDrag();
                return;
            }
            // the paper takes the keys (Entf, Strg+Z) when no part has them
            if (!partEl) this.svg.focus({ preventScroll: true });
            if (wire) {
                const [wid, wi] = wire.dataset.wire.split(':');
                this.sel = { wire: [wid, Number(wi)] };
                this.render();
                this.say(`Leitung in ${this.pinName(this.part(wid), Number(wi), false)} ausgewählt: Entf oder »Löschen« entfernt sie.`);
                return;
            }
            if (id) {
                e.preventDefault();
                // Shift (or Strg) and a click: one part more or less
                if (e.shiftKey || e.ctrlKey || e.metaKey) {
                    const ids = this.selectedParts().map((q) => q.id);
                    this.selectParts(ids.includes(id) ? ids.filter((x) => x !== id) : ids.concat([id]));
                    this.render();
                    this.sayChosen();
                    this.focusPart(id);
                    return;
                }
                // one of several selected: they move together
                if (this.many.has(id)) {
                    const from = new Map(this.selectedParts().map((q) => [q.id, [q.x, q.y]]));
                    this.drag = { kind: 'group', id, from, at, start: [e.clientX, e.clientY], moved: false, pointer: e.pointerId, before: JSON.stringify(this.c) };
                    this.canvas.setPointerCapture(e.pointerId);
                    this.bindDrag();
                    return;
                }
                const p = this.part(id);
                this.drag = { kind: 'part', id, start: [e.clientX, e.clientY], from: [p.x, p.y], at, moved: false, pointer: e.pointerId, before: JSON.stringify(this.c) };
                this.canvas.setPointerCapture(e.pointerId);
                this.bindDrag();
                return;
            }
            // the empty paper: a frame selects what it touches, a click alone
            // nothing (with Shift, the frame adds to what is selected)
            this.drag = { kind: 'frame', at, start: [e.clientX, e.clientY], moved: false, pointer: e.pointerId, add: e.shiftKey ? this.selectedParts().map((q) => q.id) : [] };
            this.canvas.setPointerCapture(e.pointerId);
            this.bindDrag();
        }

        bindDrag() {
            const move = (e) => this.pointerMove(e);
            const up = (e) => {
                this.canvas.removeEventListener('pointermove', move);
                this.canvas.removeEventListener('pointerup', up);
                this.canvas.removeEventListener('pointercancel', up);
                this.pointerUp(e);
            };
            this.canvas.addEventListener('pointermove', move);
            this.canvas.addEventListener('pointerup', up);
            this.canvas.addEventListener('pointercancel', up);
        }

        pointerMove(e) {
            const d = this.drag;
            if (!d) return;
            if (!d.moved && Math.hypot(e.clientX - d.start[0], e.clientY - d.start[1]) < 5) return;
            d.moved = true;
            const at = this.toModel(e);
            if (d.kind === 'frame') {
                this.overLayer.replaceChildren();
                D.svg('rect', { class: 'ce-frame', x: Math.min(d.at[0], at[0]), y: Math.min(d.at[1], at[1]), width: Math.abs(at[0] - d.at[0]), height: Math.abs(at[1] - d.at[1]) }, this.overLayer);
                return;
            }
            if (d.kind === 'group') {
                const dx = snap(at[0] - d.at[0]);
                const dy = snap(at[1] - d.at[1]);
                for (const [id, [x, y]] of d.from) {
                    const p = this.part(id);
                    if (!p) continue;
                    p.x = x + dx;
                    p.y = y + dy;
                    this.keepInside(p);
                }
                this.render();
                return;
            }
            if (d.kind === 'part') {
                const p = this.part(d.id);
                let x = snap(d.from[0] + at[0] - d.at[0]);
                let y = snap(d.from[1] + at[1] - d.at[1]);
                [x, y] = this.magnet(p, x, y);
                if (x !== p.x || y !== p.y) {
                    p.x = x;
                    p.y = y;
                    this.keepInside(p);
                    this.render();
                    const el = this.svg.querySelector(`[data-part="${CSS.escape(p.id)}"]`);
                    if (el) el.classList.add('ce-dragging');
                }
            } else {
                // the rubber band from the pin to the pointer
                this.overLayer.replaceChildren();
                D.svg('path', { class: 'ce-rubber', d: D.pathOf([d.origin, at]) }, this.overLayer);
                const hit = this.nearestPin(at, d.pending);
                if (hit) {
                    const g = this.geoms.get(hit.id);
                    const [x, y] = d.pending.from ? g.ins[hit.index] : g.outs[hit.port];
                    D.svg('circle', { class: 'ce-pin ce-target', cx: x, cy: y, r: 7 }, this.overLayer);
                }
            }
        }

        // moved near a straight wire, a part snaps to it
        magnet(p, x, y) {
            const g = geom(this.c, Object.assign({}, p, { x, y }));
            let best = null;
            const consider = (mine, other) => {
                const dy = other[1] - mine[1];
                if (Math.abs(dy) <= 6 && (!best || Math.abs(dy) < Math.abs(best))) best = dy;
            };
            // my inputs against their sources, my outputs against their targets
            p.src.forEach((s, i) => {
                if (!s) return;
                const [id, port] = s.split('.');
                const from = this.part(id);
                if (from && from !== p) consider(g.ins[i], geom(this.c, from).outs[port || '']);
            });
            for (const q of this.c.parts) {
                if (q === p) continue;
                q.src.forEach((s, i) => {
                    if (!s) return;
                    const [id, port] = s.split('.');
                    if (id === p.id && g.outs[port || '']) consider(g.outs[port || ''], geom(this.c, q).ins[i]);
                });
            }
            return best === null ? [x, y] : [x, y + best];
        }

        pointerUp(e) {
            const d = this.drag;
            this.drag = null;
            if (!d) return;
            if (d.kind === 'frame') {
                if (this.overLayer) this.overLayer.replaceChildren();
                if (!d.moved) {
                    if (this.sel || this.many.size) {
                        this.sel = null;
                        this.render();
                        this.say('');
                    }
                    return;
                }
                const at = this.toModel(e);
                const [lx, hx] = [Math.min(d.at[0], at[0]), Math.max(d.at[0], at[0])];
                const [ly, hy] = [Math.min(d.at[1], at[1]), Math.max(d.at[1], at[1])];
                const inside = this.c.parts.filter((p) => {
                    const [bx, by, bw, bh] = (this.geoms.get(p.id) || geom(this.c, p)).box;
                    return bx < hx && bx + bw > lx && by < hy && by + bh > ly;
                }).map((p) => p.id);
                this.selectParts([...new Set(d.add.concat(inside))]);
                this.render();
                this.sayChosen();
                this.svg.focus({ preventScroll: true });
                return;
            }
            if (d.kind === 'group') {
                if (d.moved) {
                    this.history.push(d.before);
                    if (this.history.length > MAX_HISTORY) this.history.shift();
                    this.future = [];
                    this.after(false);
                } else {
                    // a click on one of them: that one alone
                    this.sel = { part: d.id };
                    this.render();
                    this.sayChosen();
                }
                this.focusPart(d.id);
                return;
            }
            if (d.kind === 'part') {
                const p = this.part(d.id);
                if (d.moved) {
                    this.history.push(d.before);
                    if (this.history.length > MAX_HISTORY) this.history.shift();
                    this.future = [];
                    this.sel = { part: d.id };
                    this.after(false);
                } else {
                    this.sel = { part: d.id };
                    if (p.kind === 'in') {
                        this.render();
                        this.toggle(p.id);
                    } else {
                        this.render();
                        this.say(`${this.describe(p)} ausgewählt.`);
                    }
                }
                this.focusPart(d.id);
                return;
            }
            // a wire: dropped on a pin, or tapped (then the next pin ends it)
            const at = this.toModel(e);
            const hit = d.moved ? this.nearestPin(at, d.pending) : null;
            if (hit) {
                this.finishWire(d.pending, hit);
                return;
            }
            if (!d.moved) {
                this.pending = d.pending;
                this.render();
                this.say(d.pending.from ? 'Tippe jetzt auf einen Eingang (links an einem Gatter), oder daneben, um abzubrechen.' : 'Tippe jetzt auf einen Ausgang (rechts an einem Gatter oder Schalter), oder daneben, um abzubrechen.');
                return;
            }
            this.render();
            this.say('Die Leitung endet an keinem Anschluss: Lass sie über einem Eingang los.', true);
        }

        keyDown(e) {
            const el = e.target.closest('[data-part]');
            if (!el || e.ctrlKey || e.metaKey || e.altKey) return;
            const p = this.part(el.dataset.part);
            if (!p) return;
            const key = e.key;
            if ((key === 'Enter' || key === ' ') && p.kind === 'in') {
                e.preventDefault();
                this.sel = { part: p.id };
                this.toggle(p.id);
                return;
            }
            if (!this.canEdit()) return;
            if (key === 'Escape') {
                this.pending = null;
                this.sel = null;
                this.render();
                this.say('');
                this.focusPart(p.id);
                return;
            }
            if (key.startsWith('Arrow') && this.many.has(p.id)) {
                e.preventDefault();
                this.moveParts(this.selectedParts(), key.toLowerCase(), e.shiftKey);
                this.focusPart(p.id);
                return;
            }
            if (key.startsWith('Arrow')) {
                e.preventDefault();
                const step = e.shiftKey ? 4 * SNAP : SNAP;
                this.snapshot();
                if (key === 'ArrowLeft') p.x -= step;
                if (key === 'ArrowRight') p.x += step;
                if (key === 'ArrowUp') p.y -= step;
                if (key === 'ArrowDown') p.y += step;
                this.keepInside(p);
                this.sel = { part: p.id };
                this.after(false);
                this.focusPart(p.id);
                return;
            }
            if (key === 'Delete' || key === 'Backspace') {
                e.preventDefault();
                if (!this.many.has(p.id) && (!this.sel || (this.sel.part !== p.id && !this.sel.wire))) this.sel = { part: p.id };
                this.deleteSelection();
                return;
            }
            if (key === 'v' || key === 'V') {
                e.preventDefault();
                const ports = Object.keys(geom(this.c, p).outs);
                if (!ports.length) {
                    this.say('Eine Lampe hat keinen Ausgang; eine Leitung beginnt an einem Schalter, Gatter oder Baustein.', true);
                    return;
                }
                const current = this.pending && this.pending.from && this.pending.from[0] === p.id ? ports.indexOf(this.pending.from[1]) : -1;
                const port = ports[(current + 1) % ports.length];
                this.pending = { from: [p.id, port] };
                this.render();
                this.focusPart(p.id);
                this.say(`Leitung von ${this.pinName(p, port, true)}: Geh mit Tab zum Ziel und drück die Ziffer des Eingangs (1 ist der oberste); Esc bricht ab.`);
                return;
            }
            if (/^[1-9]$/.test(key)) {
                e.preventDefault();
                const index = Number(key) - 1;
                if (index >= geom(this.c, p).ins.length) {
                    this.say(`${this.describe(p)} hat keinen Eingang ${key}.`, true);
                    return;
                }
                if (this.pending && this.pending.from) {
                    const [fromId, port] = this.pending.from;
                    this.connect(fromId, port, p.id, index);
                    this.focusPart(p.id);
                    return;
                }
                if (p.src[index]) {
                    this.sel = { wire: [p.id, index] };
                    this.render();
                    this.focusPart(p.id);
                    this.say(`Leitung in ${this.pinName(p, index, false)} ausgewählt: Entf löscht sie.`);
                }
                return;
            }
            if (key === '+' || key === '-') {
                this.sel = { part: p.id };
                this.changeInputs(key === '+' ? 1 : -1);
            }
        }
    }

    // ------------------------------------------------------------------

    function start() {
        document.querySelectorAll('.circuit-editor, .circuit-view').forEach((el) => {
            if (el.dataset.mounted) return;
            el.dataset.mounted = '1';
            el.circuitEditor = new Editor(el);
        });
    }

    window.CircuitEditor = { start, Library, layout, route, geom, Editor };
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
