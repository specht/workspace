// Circuits to click (»Vom Gatter zum Rechenwerk«, src/content/logikgatter):
// a circuit drawn as SVG, its inputs switches that a click, a tap, Enter or
// the space bar toggles, every wire coloured by the bit on it, the outputs
// lamps. The gates are drawn as in DIN EN 60617 (IEC 60617): a rectangle
// with &, ≥1, =1 or 1, a small circle at the output where it is negated.
//
//   <div class='logic-circuit' data-circuit='half-adder' data-title='Halbaddierer'></div>
//   <div class='logic-circuit' data-circuit='gate-and gate-or gate-not'></div>
//
// Several names make a choice above the drawing (presets, docs/widgets.md).
// The circuits are the library below (CIRCUITS), laid out by hand.
//
// The engine is apart from the drawing and runs without a page
// (test/static/logic_circuit.test.cjs): round() lets every gate compute
// once from the values of the round before, all at the same time, as if
// each gate took the same time to switch; settle() repeats that until
// nothing changes, at most MAX_ROUNDS times. A circuit with feedback (the
// flip-flop) holds its bit that way, and one that never comes to rest
// (both gates of an RS flip-flop released at once) says so instead of
// running for ever. On a page the rounds are played one after another, so
// that a carry can be seen running through the adder.
(function () {
    'use strict';

    const GW = 40;          // a gate's width
    const PITCH = 24;       // between two inputs of a gate
    const BUBBLE = 4;       // the radius of the circle of a negated output
    const SWITCH_W = 30;    // an input's switch
    const SWITCH_H = 26;
    const LAMP = 13;        // an output's lamp, its radius
    const MAX_ROUNDS = 64;
    const SHOWN_ROUNDS = 12; // of a circuit that never comes to rest
    const ROUND_MS = 110;

    // ------------------------------------------------------------------
    // The engine

    const mask = (width) => (width >= 31 ? 0x7fffffff : (1 << width) - 1);

    // the gates: their sign (DIN EN 60617), a negated output, and what they
    // compute on w bits at once (a bus: every bit on its own)
    const GATES = {
        and: { sign: '&', neg: false, f: (xs, m) => xs.reduce((p, x) => p & x, m) },
        or: { sign: '≥1', neg: false, f: (xs) => xs.reduce((p, x) => p | x, 0) },
        xor: { sign: '=1', neg: false, f: (xs) => xs.reduce((p, x) => p ^ x, 0) },
        not: { sign: '1', neg: true, f: (xs, m) => ~xs[0] & m },
        nand: { sign: '&', neg: true, f: (xs, m) => ~xs.reduce((p, x) => p & x, m) & m },
        nor: { sign: '≥1', neg: true, f: (xs, m) => ~xs.reduce((p, x) => p | x, 0) & m },
    };

    // blocks: what they compute from their inputs, by port
    const BLOCKS = {
        // a full adder of width bits: a, b and the carry in
        add: (n, [a, b, c]) => {
            const m = mask(n.width || 1);
            const sum = a + b + c;
            return { s: sum & m, c_out: sum > m ? 1 : 0 };
        },
        // every bit of x through =1 with k: x itself when k is 0, every bit
        // turned round when k is 1
        inv: (n, [x, k]) => ({ out: k ? ~x & mask(n.width) : x }),
        // the choice of the ALU: the sum with its carry flag, or one of the
        // three logic results with the carry flag 0
        mux: (n, [sum, cf, and, or, xor, sel]) => ({ r: [sum, and, or, xor][sel] || 0, cf: sel === 0 ? cf : 0 }),
    };

    const keyOf = (src) => (typeof src === 'string' ? src : src.from);

    function isInput(n) {
        return n.kind === 'in' || n.kind === 'bus-in' || n.kind === 'choice';
    }

    function isOutput(n) {
        return n.kind === 'out' || n.kind === 'bus-out';
    }

    // The values at the start: every input and gate as the circuit says
    // (value:, init:), else 0.
    function initial(circuit) {
        const v = {};
        for (const n of circuit.nodes) {
            if (n.kind === 'choice') setChoice(circuit, v, n.id, n.value || 0);
            else if (isInput(n)) v[n.id] = n.value || 0;
            else if (!isOutput(n)) {
                if (n.init && typeof n.init === 'object') for (const [p, x] of Object.entries(n.init)) v[`${n.id}:${p}`] = x;
                else v[n.id] = n.init || 0;
            }
        }
        return v;
    }

    // a choice (the ALU's operation) sets its value and the control lines
    // that come from it at once: they are wires, not gates
    function setChoice(circuit, v, id, index) {
        const n = circuit.nodes.find((x) => x.id === id);
        v[id] = index;
        for (const [port, value] of Object.entries(n.options[index].lines)) v[`${id}:${port}`] = value;
    }

    // One round: every gate and block computes from the values before.
    function round(circuit, values) {
        const next = Object.assign({}, values);
        for (const n of circuit.nodes) {
            if (isInput(n) || isOutput(n)) continue;
            const xs = n.inputs.map((s) => values[keyOf(s)] || 0);
            if (GATES[n.kind]) {
                const m = mask(n.width || 1);
                // one bus into a gate: its bits are the inputs (zero test)
                const ins = n.reduce ? Array.from({ length: n.reduce }, (_, i) => (xs[0] >> i) & 1) : xs;
                next[n.id] = GATES[n.kind].f(ins, n.reduce ? 1 : m);
            } else {
                for (const [port, value] of Object.entries(BLOCKS[n.kind](n, xs))) next[`${n.id}:${port}`] = value;
            }
        }
        return next;
    }

    // a port a block has not computed yet counts as a change
    const same = (a, b) => Object.keys(a).length === Object.keys(b).length && Object.keys(a).every((k) => a[k] === b[k]);

    // From the values start (an input just changed), round after round
    // until nothing changes: { rounds: [start, …], stable }.
    function settle(circuit, start, max = MAX_ROUNDS) {
        const rounds = [start];
        let v = start;
        for (let i = 0; i < max; i++) {
            const next = round(circuit, v);
            if (same(next, v)) return { rounds, stable: true };
            rounds.push(next);
            v = next;
        }
        return { rounds, stable: false };
    }

    // The value at a source: a node ('a') or a port of a block ('sum:s').
    const valueAt = (values, src) => values[keyOf(src)] || 0;

    // The truth table of a circuit without memory: a row for every
    // combination of its single-bit inputs, the first input the highest
    // bit, as tables are written; each row the inputs and the outputs.
    function truthTable(circuit) {
        const ins = circuit.nodes.filter((n) => n.kind === 'in');
        const outs = circuit.nodes.filter((n) => n.kind === 'out');
        const rows = [];
        for (let row = 0; row < 1 << ins.length; row++) {
            const start = initial(circuit);
            ins.forEach((n, i) => { start[n.id] = (row >> (ins.length - 1 - i)) & 1; });
            const { rounds } = settle(circuit, start);
            const end = rounds[rounds.length - 1];
            rows.push({ inputs: ins.map((n) => start[n.id]), outputs: outs.map((n) => valueAt(end, n.from)) });
        }
        return { inputs: ins.map((n) => n.label || n.id), outputs: outs.map((n) => n.label || n.id), rows };
    }

    // ------------------------------------------------------------------
    // The circuits

    // a switch, its pin on the right (or below: exit 'down')
    const input = (id, x, y, label, more = {}) => Object.assign({ kind: 'in', id, x, y, label: label === undefined ? id : label }, more);
    // a gate, its top left corner at x, y; inputs from top to bottom
    const gate = (id, kind, x, y, inputs, more = {}) => Object.assign({ kind, id, x, y, inputs }, more);
    // a lamp, its pin on the left (or above: enter 'up')
    const output = (id, x, y, from, label, more = {}) => Object.assign({ kind: 'out', id, x, y, from, label: label === undefined ? id : label }, more);
    // a wire that bends at x (from left to right), or goes along points
    const at = (from, x) => ({ from, x });
    const via = (from, ...points) => ({ from, via: points });

    function oneGate(kind) {
        if (kind === 'not') {
            return { w: 280, h: 104, table: true, nodes: [input('a', 50, 52), gate('y', kind, 140, 36, ['a']), output('Y', 240, 52, 'y', 'y')] };
        }
        return {
            w: 280, h: 116, table: true,
            nodes: [input('a', 50, 30), input('b', 50, 86), gate('y', kind, 140, 34, [at('a', 100), at('b', 100)]), output('Y', 240, 58, 'y', 'y')],
        };
    }

    const CIRCUITS = {
        'gate-and': () => Object.assign(oneGate('and'), { label: 'UND (&)' }),
        'gate-or': () => Object.assign(oneGate('or'), { label: 'ODER (≥1)' }),
        'gate-not': () => Object.assign(oneGate('not'), { label: 'NICHT (1 mit Kreis)' }),
        'gate-xor': () => Object.assign(oneGate('xor'), { label: 'XOR (=1)' }),
        'gate-nand': () => Object.assign(oneGate('nand'), { label: 'NAND (& mit Kreis)' }),
        'gate-nor': () => Object.assign(oneGate('nor'), { label: 'NOR (≥1 mit Kreis)' }),

        // De Morgan: nicht (a oder b) and (nicht a) und (nicht b), side by side
        'de-morgan': () => ({
            label: 'nicht (a oder b) und (nicht a) und (nicht b)', w: 470, h: 196, table: true,
            nodes: [
                input('a', 40, 60), input('b', 40, 150),
                gate('o', 'or', 150, 18, [at('a', 100), at('b', 115)]),
                gate('y1', 'not', 240, 26, ['o']),
                gate('na', 'not', 150, 112, [at('a', 100)]),
                gate('nb', 'not', 150, 156, [at('b', 115)]),
                gate('y2', 'and', 260, 128, [at('na', 225), at('nb', 225)]),
                output('Y1', 400, 42, 'y1', 'y₁'),
                output('Y2', 400, 152, 'y2', 'y₂'),
            ],
        }),

        // NOT, AND, OR and XOR built from NAND only
        'nand-not': () => ({
            label: 'NICHT aus NAND', w: 330, h: 116, table: true,
            nodes: [input('a', 50, 58), gate('y', 'nand', 150, 34, [at('a', 105), at('a', 105)]), output('Y', 280, 58, 'y', 'y')],
        }),
        'nand-and': () => ({
            label: 'UND aus NAND', w: 420, h: 116, table: true,
            nodes: [
                input('a', 50, 30), input('b', 50, 86),
                gate('x', 'nand', 130, 34, [at('a', 95), at('b', 95)]),
                gate('y', 'nand', 240, 34, [at('x', 205), at('x', 205)]),
                output('Y', 370, 58, 'y', 'y'),
            ],
        }),
        'nand-or': () => ({
            label: 'ODER aus NAND', w: 420, h: 170, table: true,
            nodes: [
                input('a', 50, 40), input('b', 50, 128),
                gate('na', 'nand', 130, 16, [at('a', 100), at('a', 100)]),
                gate('nb', 'nand', 130, 104, [at('b', 100), at('b', 100)]),
                gate('y', 'nand', 250, 60, [at('na', 215), at('nb', 215)]),
                output('Y', 370, 84, 'y', 'y'),
            ],
        }),
        'nand-xor': () => ({
            label: 'XOR aus NAND', w: 500, h: 186, table: true,
            nodes: [
                input('a', 50, 40), input('b', 50, 146),
                gate('n1', 'nand', 130, 69, [at('a', 95), at('b', 95)]),
                gate('n2', 'nand', 250, 16, [at('a', 222), at('n1', 205)]),
                gate('n3', 'nand', 250, 122, [at('n1', 205), at('b', 222)]),
                gate('y', 'nand', 360, 69, [at('n2', 330), at('n3', 330)]),
                output('Y', 460, 93, 'y', 'y'),
            ],
        }),

        'half-adder': () => ({
            label: 'Halbaddierer', w: 330, h: 150, table: true,
            nodes: [
                input('a', 50, 46), input('b', 50, 124),
                gate('s', 'xor', 160, 34, ['a', at('b', 125)]),
                gate('c', 'and', 160, 88, [at('a', 105), 'b']),
                output('S', 280, 58, 's', 's'),
                output('C', 280, 112, 'c', 'c'),
            ],
        }),

        // two half adders and an OR; the half adders framed
        'full-adder': () => ({
            label: 'Volladdierer', w: 560, h: 252, table: true,
            frames: [
                { x: 134, y: 22, w: 62, h: 140, label: 'Halbaddierer' },
                { x: 276, y: 36, w: 64, h: 190, label: 'Halbaddierer' },
            ],
            nodes: [
                input('a', 66, 30), input('b', 66, 86), input('c', 66, 204, 'c_in'),
                gate('x', 'xor', 146, 34, [at('a', 96), at('b', 112)]),
                gate('c1', 'and', 146, 102, [at('a', 96), at('b', 112)]),
                gate('s', 'xor', 288, 46, ['x', at('c', 252)]),
                gate('c2', 'and', 288, 168, [at('x', 236), 'c']),
                gate('co', 'or', 400, 132, [at('c1', 372), at('c2', 384)]),
                output('S', 490, 70, 's', 's'),
                output('CO', 490, 156, 'co', 'c_out'),
            ],
        }),

        // four full adders, bit 3 on the left as in a written number; the
        // carry runs from right to left
        'adder-4': () => {
            const nodes = [];
            const left = 74;
            const step = 104;
            for (let i = 3; i >= 0; i--) {
                const x = left + (3 - i) * step;
                nodes.push(input(`a${i}`, x + 16, 34, `a${i}`, { exit: 'down', labelAbove: true }));
                nodes.push(input(`b${i}`, x + 58, 34, `b${i}`, { exit: 'down', labelAbove: true }));
                nodes.push({
                    kind: 'add', id: `fa${i}`, x, y: 92, w: 74, h: 58, sign: 'Σ', width: 1,
                    inputs: [`a${i}`, `b${i}`, i === 0 ? 'zero' : `fa${i - 1}:c_out`],
                    pins: { in: [[16, 0, 'down'], [58, 0, 'down'], [74, 40, 'left']], out: { s: [37, 58, 'down'], c_out: [0, 40, 'left'] } },
                    pinLabels: { in: ['a', 'b', 'c'], out: { s: 's', c_out: 'c' } },
                });
                nodes.push(output(`s${i}`, x + 37, 200, `fa${i}:s`, `s${i}`, { enter: 'up' }));
            }
            nodes.push({ kind: 'in', id: 'zero', x: left + 3 * step + 100, y: 132, fixed: true, label: '' });
            nodes.push(output('c4', 34, 132, 'fa3:c_out', 'c_out', { enter: 'right', labelBelow: true }));
            return {
                label: 'Addierer für 4 Bit', w: left + 3 * step + 124, h: 236, nodes,
                readout: (v) => {
                    const num = (p) => [3, 2, 1, 0].map((i) => v[`${p}${i}`] || 0);
                    const sum = [3, 2, 1, 0].map((i) => v[`fa${i}:s`] || 0);
                    const dec = (bits) => bits.reduce((n, b) => 2 * n + b, 0);
                    const a = num('a');
                    const b = num('b');
                    const c = v['fa3:c_out'] || 0;
                    return `<code>${a.join('')}</code> + <code>${b.join('')}</code> = <code>${sum.join('')}</code>, Übertrag <code>${c}</code>` +
                        `<span class='lc-dec'>; im Zehnersystem ${dec(a)} + ${dec(b)} = ${dec(sum) + 16 * c}${c ? `, in 4 Bit bleibt ${dec(sum)}` : ''}</span>`;
                },
            };
        },

        // the RS flip-flop of two NOR gates, Q above
        'rs-latch': () => ({
            label: 'RS-Flipflop', w: 400, h: 196, release: ['s', 'r'],
            nodes: [
                input('r', 50, 50, 'R'), input('s', 50, 160, 'S'),
                gate('q', 'nor', 150, 38, ['r', via('qn', [262, 148], [262, 108], [118, 108], [118, 74])], { init: 0 }),
                gate('qn', 'nor', 150, 124, [via('q', [246, 62], [246, 96], [128, 96], [128, 136]), 's'], { init: 1 }),
                output('Q', 340, 62, 'q', 'Q'),
                output('QN', 340, 148, 'qn', 'Q', { over: true }),
            ],
            say: latchSay,
        }),

        // the D flip-flop controlled by its clock (a D latch): the RS
        // flip-flop with two AND gates in front
        'd-latch': () => ({
            label: 'D-Flipflop, taktzustandsgesteuert', w: 560, h: 206,
            nodes: [
                input('e', 70, 104, 'Takt'), input('d', 70, 160, 'D'),
                gate('nd', 'not', 120, 34, [at('d', 100)]),
                gate('r', 'and', 210, 38, ['nd', at('e', 186)]),
                gate('s', 'and', 210, 124, [at('e', 186), 'd']),
                gate('q', 'nor', 340, 38, [at('r', 300), via('qn', [452, 148], [452, 108], [308, 108], [308, 74])], { init: 0 }),
                gate('qn', 'nor', 340, 124, [via('q', [436, 62], [436, 96], [318, 96], [318, 136]), at('s', 300)], { init: 1 }),
                output('Q', 510, 62, 'q', 'Q'),
                output('QN', 510, 148, 'qn', 'Q', { over: true }),
            ],
            wireLabels: [{ x: 272, y: 52, text: 'R' }, { x: 272, y: 138, text: 'S' }],
            say: (v) => (v.e ? `Der Takt ist 1: Q folgt D und ist ${v.q}.` : `Der Takt ist 0: Q behält ${v.q}, was immer D ist.`),
        }),

        // a 4-bit ALU with the operations and flags of the CPU Sandbox
        alu: () => ({
            label: 'Rechenwerk (ALU) für 4 Bit', w: 600, h: 386,
            nodes: [
                { kind: 'bus-in', id: 'a', x: 66, y: 26, bits: 4, label: 'a', pin: [120, 42, 'down'] },
                { kind: 'bus-in', id: 'b', x: 286, y: 26, bits: 4, label: 'b', pin: [340, 42, 'down'] },
                {
                    kind: 'choice', id: 'op', x: 470, y: 48, w: 96, h: 58, label: 'Befehl', value: 0,
                    options: [
                        { label: 'ADD', lines: { sub: 0, sel: 0 } },
                        { label: 'SUB', lines: { sub: 1, sel: 0 } },
                        { label: 'AND', lines: { sub: 0, sel: 1 } },
                        { label: 'OR', lines: { sub: 0, sel: 2 } },
                        { label: 'XOR', lines: { sub: 0, sel: 3 } },
                    ],
                    pins: { out: { sel: [0, 20, 'left'], sub: [0, 44, 'left'] } },
                },
                {
                    kind: 'inv', id: 'nb', x: 80, y: 128, w: 40, h: 26, sign: '=1', width: 4,
                    inputs: [via('b', [340, 84], [50, 84], [50, 141]), via('op:sub', [100, 92])],
                    pins: { in: [[0, 13, 'left'], [20, 0, 'down']], out: { out: [40, 13, 'right'] } },
                },
                {
                    kind: 'add', id: 'sum', x: 150, y: 110, w: 80, h: 64, sign: 'Σ', width: 4,
                    inputs: [via('a', [120, 70], [30, 70], [30, 122]), 'nb:out', via('op:sub', [136, 92], [136, 162])],
                    pins: { in: [[0, 12, 'left'], [0, 31, 'left'], [0, 52, 'left']], out: { s: [80, 20, 'right'], c_out: [80, 52, 'right'] } },
                    pinLabels: { in: ['a', 'b', 'c'], out: { s: 's', c_out: 'c' } },
                },
                gate('cf', 'xor', 272, 150, ['sum:c_out', via('op:sub', [252, 92], [252, 186])]),
                gate('and', 'and', 150, 200, [via('a', [120, 70], [30, 70], [30, 212]), via('b', [340, 84], [50, 84], [50, 236])], { width: 4 }),
                gate('or', 'or', 150, 260, [via('a', [120, 70], [30, 70], [30, 272]), via('b', [340, 84], [50, 84], [50, 296])], { width: 4 }),
                gate('xor', 'xor', 150, 320, [via('a', [120, 70], [30, 70], [30, 332]), via('b', [340, 84], [50, 84], [50, 356])], { width: 4 }),
                {
                    kind: 'mux', id: 'mux', x: 384, y: 112, w: 46, h: 256, sign: 'MUX',
                    inputs: ['sum:s', 'cf', 'and', 'or', 'xor', via('op:sel', [407, 68])],
                    pins: {
                        in: [[0, 18, 'left'], [0, 62, 'left'], [0, 112, 'left'], [0, 172, 'left'], [0, 232, 'left'], [23, 0, 'down']],
                        out: { r: [46, 100, 'right'], cf: [46, 196, 'right'] },
                    },
                    pinLabels: { in: ['Σ', 'c', '&', '≥1', '=1', ''] },
                    selects: [[0, 1], [2], [3], [4]],
                },
                gate('zf', 'nor', 470, 238, [via('mux:r', [450, 212], [450, 254])], { width: 4, reduce: 4 }),
                { kind: 'bus-out', id: 'r', x: 482, y: 212, bits: 4, from: 'mux:r', label: 'r', pin: [466, 212, 'left'] },
                output('ZF', 560, 254, 'zf', 'ZF'),
                output('CF', 560, 308, 'mux:cf', 'CF'),
            ],
            wireLabels: [{ x: 448, y: 61, text: 'sel' }, { x: 448, y: 85, text: 'sub' }],
            busLabels: [
                { from: 'sum:s', x: 330, y: 124 },
                { from: 'and', x: 330, y: 218 }, { from: 'or', x: 330, y: 278 }, { from: 'xor', x: 330, y: 338 },
            ],
            readout: (v) => {
                const bits = (x) => [3, 2, 1, 0].map((i) => (x >> i) & 1).join('');
                const op = ['ADD', 'SUB', 'AND', 'OR', 'XOR'][v.op];
                return `<code>${op} ${bits(v.a)}, ${bits(v.b)}</code> ergibt <code>${bits(v['mux:r'] || 0)}</code>, ZF = ${v.zf || 0}, CF = ${v['mux:cf'] || 0}` +
                    (v.op < 2 ? `<span class='lc-dec'>; im Zehnersystem ${v.a} ${v.op ? '−' : '+'} ${v.b} = ${v.op ? v.a - v.b : v.a + v.b}</span>` : '');
            },
        }),
    };

    // Q with a bar: the bar just above the letter (.lc-not), read »nicht Q«
    const QN = "<span class='lc-not'><span aria-hidden='true'>Q</span><span class='visually-hidden'>nicht Q</span></span>";

    function latchSay(v, stable) {
        if (!stable) {
            return `Q und ${QN} schalten bei jedem Schritt zugleich um: Die Schaltung kommt nicht zur Ruhe. In echter Hardware schalten zwei Gatter nie genau gleich schnell; welches gewinnt, lässt sich nicht vorhersagen.`;
        }
        if (v.s && v.r) return `S und R sind beide 1: Q und ${QN} sind beide 0. Das ist kein gespeichertes Bit mehr.`;
        if (v.s) return 'S ist 1: Q wird 1.';
        if (v.r) return 'R ist 1: Q wird 0.';
        return `S und R sind 0: Das Flipflop hält sein Bit, Q = ${v.q}.`;
    }

    // ------------------------------------------------------------------
    // Geometry: where the pins are, and the wires between them

    // a gate's height by the number of its inputs
    const gateHeightFor = (count) => (count === 1 ? 32 : PITCH * count);

    function gateHeight(n) {
        return gateHeightFor(n.reduce ? 1 : n.inputs.length);
    }

    // [x, y, dir] of a node's input i, dir where the wire comes from
    function inPin(n, i) {
        if (n.kind === 'out') {
            if (n.enter === 'up') return [n.x, n.y - LAMP, 'down'];
            if (n.enter === 'right') return [n.x + LAMP, n.y, 'left'];
            return [n.x - LAMP, n.y, 'right'];
        }
        if (n.kind === 'bus-out') return [n.pin[0], n.pin[1], 'right'];
        if (n.pins) {
            const [dx, dy, dir] = n.pins.in[i];
            return [n.x + dx, n.y + dy, dir === 'left' ? 'right' : dir === 'down' ? 'down' : 'left'];
        }
        const h = gateHeight(n);
        const count = n.reduce ? 1 : n.inputs.length;
        const dy = count === 1 ? h / 2 : PITCH / 2 + PITCH * i;
        return [n.x, n.y + dy, 'right'];
    }

    // [x, y, dir] of a source's pin, dir where the wire goes
    function outPin(nodes, src) {
        const [id, port] = keyOf(src).split(':');
        const n = nodes.find((x) => x.id === id);
        if (n.kind === 'in') {
            if (n.exit === 'down') return [n.x, n.y + SWITCH_H / 2, 'down'];
            if (n.fixed) return [n.x - 12, n.y, 'left'];
            return [n.x + SWITCH_W / 2, n.y, 'right'];
        }
        if (n.kind === 'bus-in') return [n.pin[0], n.pin[1], n.pin[2]];
        if (n.pins && n.pins.out) {
            const [dx, dy, dir] = n.pins.out[port];
            return [n.x + dx, n.y + dy, dir];
        }
        const h = gateHeight(n);
        return [n.x + GW + (GATES[n.kind].neg ? 2 * BUBBLE : 0), n.y + h / 2, 'right'];
    }

    // the points of a wire from src into node n's input i
    function route(nodes, src, n, i) {
        const [x0, y0, d0] = outPin(nodes, src);
        const [x1, y1] = inPin(n, i);
        const start = [x0, y0];
        const end = [x1, y1];
        if (typeof src === 'object' && src.via) return [start, ...src.via, end];
        if (x0 === x1 || y0 === y1) return [start, end];
        if (d0 === 'down') return [start, [x0, y1], end];
        const xm = typeof src === 'object' && src.x !== undefined ? src.x : Math.round((x0 + x1) / 2);
        return [start, [xm, y0], [xm, y1], end];
    }

    // every wire of the circuit: { src, points, width, control }
    function wires(circuit) {
        const list = [];
        const widthOf = (src) => {
            const [id, port] = keyOf(src).split(':');
            const n = circuit.nodes.find((x) => x.id === id);
            if (n.kind === 'bus-in') return n.bits;
            if (n.kind === 'choice') return port === 'sel' ? 2 : 1;
            if (n.kind === 'add') return port === 'c_out' ? 1 : n.width || 1;
            if (n.kind === 'mux') return port === 'r' ? 4 : 1;
            if (n.kind === 'inv') return n.width;
            return n.reduce ? 1 : n.width || 1;
        };
        for (const n of circuit.nodes) {
            const sources = n.kind === 'out' || n.kind === 'bus-out' ? [n.from] : n.inputs || [];
            sources.forEach((src, i) => {
                const control = keyOf(src).startsWith('op:');
                list.push({ src: keyOf(src), points: route(circuit.nodes, src, n, i), width: widthOf(src), control });
            });
        }
        return list;
    }

    // The dots where a wire branches: every point that three or more
    // pieces of the same source's wires meet, after cutting each piece at
    // every corner of that source's other wires.
    function junctions(list) {
        const bySrc = new Map();
        for (const w of list) {
            if (!bySrc.has(w.src)) bySrc.set(w.src, []);
            bySrc.get(w.src).push(w.points);
        }
        const dots = [];
        for (const [src, polylines] of bySrc) {
            if (polylines.length < 2) continue;
            const corners = polylines.flat();
            const pieces = new Set();
            for (const pts of polylines) {
                for (let k = 0; k + 1 < pts.length; k++) {
                    const [a, b] = [pts[k], pts[k + 1]];
                    const on = corners.filter((p) => onSegment(p, a, b)).concat([a, b]);
                    const along = a[0] === b[0] ? 1 : 0;
                    on.sort((p, q) => p[along] - q[along]);
                    for (let j = 0; j + 1 < on.length; j++) {
                        const [p, q] = [on[j], on[j + 1]];
                        if (p[0] === q[0] && p[1] === q[1]) continue;
                        pieces.add([p, q].map((r) => r.join(',')).sort().join(' '));
                    }
                }
            }
            const degree = new Map();
            for (const piece of pieces) for (const end of piece.split(' ')) degree.set(end, (degree.get(end) || 0) + 1);
            for (const [point, count] of degree) if (count >= 3) dots.push({ p: point.split(',').map(Number), src });
        }
        return dots;
    }

    function onSegment(p, a, b) {
        if (a[0] === b[0]) return p[0] === a[0] && p[1] > Math.min(a[1], b[1]) && p[1] < Math.max(a[1], b[1]);
        if (a[1] === b[1]) return p[1] === a[1] && p[0] > Math.min(a[0], b[0]) && p[0] < Math.max(a[0], b[0]);
        return false;
    }

    // ------------------------------------------------------------------
    // Netlists: circuits a person builds (circuit_editor.js, »Logik-Werkstatt«,
    // src/content/logikwerkstatt), as text, one part per line:
    //
    //   in a @40,60            a switch: its name, where it stands
    //   nand g1 a b @120,48    a gate: its id, then the source of each input
    //   HA h1 a g1             a block (a stored sub-circuit), by its name
    //   out y h1.s             a lamp and its source; h1.s is the output s of h1
    //   block HA               a block: its in lines are its inputs, its out
    //     …                    lines its outputs, in this order
    //   end
    //
    // »-« is an input without a wire, »#« starts a comment, »;« ends a line
    // as well (a netlist in an attribute). The same text is the answer the
    // check reads on the server (src/ruby/include/circuit_check.rb), which
    // parses, flattens and settles it the same way. Positions are optional:
    // without them the editor lays the circuit out itself.
    //
    // flatten() turns a netlist into a circuit of the engine above: every
    // block is replaced by its gates, their ids prefixed with the block's
    // (h1/g1), so that round() and settle() run it unchanged, feedback
    // through blocks included.

    const GATE_KINDS = ['and', 'or', 'xor', 'not', 'nand', 'nor'];
    const NAME = /^[A-Za-z_][A-Za-z0-9_]{0,15}$/;
    const BLOCK_NAME = /^[A-Za-z][A-Za-z0-9_]{0,11}$/;
    const KEYWORDS = ['in', 'out', 'block', 'end', ...GATE_KINDS];
    const LIMITS = { parts: 150, blocks: 16, depth: 8, gates: 1000, maxInputs: 4 };
    // what a gate is called in a message
    const GATE_NAMES = { and: 'UND', or: 'ODER', xor: 'XOR', not: 'NICHT', nand: 'NAND', nor: 'NOR' };

    function inputCount(kind, n) {
        if (kind === 'not') return n === 1;
        return n >= 2 && n <= LIMITS.maxInputs;
    }

    // { circuit: { parts, blocks }, errors: [{ line, message }] }; a part is
    // { id, kind, src: [source or null], x, y } (kind: in, out, a gate, or
    // block with block: its name)
    function parseNetlist(text) {
        const errors = [];
        const top = { parts: [] };
        const blocks = {};
        let scope = top;
        let scopeName = null;
        const fail = (line, message) => errors.push({ line, message });
        String(text || '').split(/\r?\n|;/).forEach((raw, i) => {
            const lineNo = i + 1;
            const line = raw.replace(/#.*$/, '').trim();
            if (!line) return;
            const tokens = line.split(/\s+/);
            const head = tokens[0];
            if (head === 'block') {
                if (scopeName) return fail(lineNo, `Ein Baustein steht nicht in einem anderen: ${scopeName} hat noch kein end.`);
                const name = tokens[1];
                if (tokens.length !== 2 || !BLOCK_NAME.test(name || '') || KEYWORDS.includes(name.toLowerCase())) {
                    return fail(lineNo, `Ein Baustein braucht einen Namen aus Buchstaben und Ziffern, etwa »block HA«.`);
                }
                if (blocks[name]) return fail(lineNo, `Den Baustein ${name} gibt es zweimal.`);
                if (Object.keys(blocks).length >= LIMITS.blocks) return fail(lineNo, `Mehr als ${LIMITS.blocks} Bausteine gehen nicht.`);
                scope = { parts: [] };
                blocks[name] = scope;
                scopeName = name;
                return;
            }
            if (head === 'end') {
                if (!scopeName) return fail(lineNo, 'Zu diesem end gibt es kein block.');
                scope = top;
                scopeName = null;
                return;
            }
            let x = null;
            let y = null;
            const last = tokens[tokens.length - 1];
            const at = /^@(\d{1,4}),(\d{1,4})$/.exec(last);
            if (at) {
                tokens.pop();
                x = Number(at[1]);
                y = Number(at[2]);
            } else if (last.startsWith('@')) {
                return fail(lineNo, `»${last}« ist keine Stelle: so etwa @120,48.`);
            }
            const [kind, id, ...src] = tokens;
            if (!id || !NAME.test(id)) return fail(lineNo, `»${id || ''}« taugt nicht als Name: Buchstaben, Ziffern und _, vorn ein Buchstabe.`);
            if (scope.parts.some((p) => p.id === id)) return fail(lineNo, `Den Namen ${id} gibt es hier zweimal.`);
            if (scope.parts.length >= LIMITS.parts) return fail(lineNo, `Mehr als ${LIMITS.parts} Teile gehen nicht.`);
            const part = { id, kind, src: src.map((s) => (s === '-' ? null : s)), x, y };
            if (kind === 'in') {
                if (src.length) return fail(lineNo, `Ein Schalter (in ${id}) hat keine Eingänge.`);
            } else if (kind === 'out') {
                if (src.length !== 1) return fail(lineNo, `Eine Lampe (out ${id}) hat genau eine Leitung, etwa »out ${id} g1«.`);
            } else if (GATE_KINDS.includes(kind)) {
                if (!inputCount(kind, src.length)) {
                    return fail(lineNo, kind === 'not' ? `Ein NICHT (${id}) hat genau einen Eingang.` : `Ein ${GATE_NAMES[kind]} (${id}) hat 2 bis ${LIMITS.maxInputs} Eingänge.`);
                }
            } else if (BLOCK_NAME.test(kind || '')) {
                part.kind = 'block';
                part.block = kind;
            } else {
                return fail(lineNo, `»${kind}« kenne ich nicht: in, out, ein Gatter (${GATE_KINDS.join(', ')}) oder der Name eines Bausteins.`);
            }
            for (const s of part.src) {
                if (s !== null && !/^[A-Za-z_][A-Za-z0-9_]{0,15}(\.[A-Za-z_][A-Za-z0-9_]{0,15})?$/.test(s)) return fail(lineNo, `»${s}« ist keine Quelle: ein Name wie g1 oder h1.s, oder - ohne Leitung.`);
            }
            scope.parts.push(part);
        });
        if (scopeName) fail(0, `Dem Baustein ${scopeName} fehlt sein end.`);
        const circuit = { parts: top.parts, blocks };
        if (!errors.length) errors.push(...checkNetlist(circuit));
        return { circuit, errors };
    }

    const pinsIn = (def) => def.parts.filter((p) => p.kind === 'in').map((p) => p.id);
    const pinsOut = (def) => def.parts.filter((p) => p.kind === 'out').map((p) => p.id);

    // sources that exist, blocks that exist with as many inputs as given,
    // no block inside itself
    function checkNetlist(circuit) {
        const errors = [];
        const where = (name) => (name ? ` im Baustein ${name}` : '');
        const scopes = [[null, circuit], ...Object.entries(circuit.blocks).map(([n, d]) => [n, d])];
        for (const [name, def] of scopes) {
            const byId = new Map(def.parts.map((p) => [p.id, p]));
            for (const p of def.parts) {
                if (p.kind === 'block') {
                    const inner = circuit.blocks[p.block];
                    if (!inner) {
                        errors.push({ line: 0, message: `Den Baustein ${p.block} (${p.id}${where(name)}) gibt es nicht.` });
                        continue;
                    }
                    if (inner.parts && p.src.length !== pinsIn(inner).length) {
                        errors.push({ line: 0, message: `${p.block} hat ${pinsIn(inner).length} Eingänge, ${p.id}${where(name)} bekommt ${p.src.length}.` });
                    }
                }
                for (const s of p.src) {
                    if (s === null) continue;
                    const [id, port] = s.split('.');
                    const from = byId.get(id);
                    const bad = (msg) => errors.push({ line: 0, message: `${p.id}${where(name)}: ${msg}` });
                    if (!from) bad(`Die Quelle ${s} gibt es nicht.`);
                    else if (from.kind === 'out') bad(`${s} ist eine Lampe; eine Leitung kommt aus einem Schalter, einem Gatter oder einem Baustein.`);
                    else if (from.kind === 'block') {
                        const inner = circuit.blocks[from.block];
                        if (inner && !port) bad(`Bei einem Baustein nennt die Quelle den Ausgang, etwa ${id}.${pinsOut(inner)[0] || 's'}.`);
                        else if (inner && !pinsOut(inner).includes(port)) bad(`${from.block} hat keinen Ausgang ${port}.`);
                    } else if (port) bad(`${id} hat nur einen Ausgang; die Quelle heißt ${id}.`);
                }
            }
        }
        // a block that contains itself, directly or through others
        const visiting = new Set();
        const done = new Set();
        const visit = (name, path) => {
            if (done.has(name)) return;
            if (visiting.has(name)) {
                errors.push({ line: 0, message: `Der Baustein ${name} enthält sich selbst (${[...path, name].join(' → ')}).` });
                return;
            }
            visiting.add(name);
            for (const p of (circuit.blocks[name] || { parts: [] }).parts) if (p.kind === 'block' && circuit.blocks[p.block]) visit(p.block, [...path, name]);
            visiting.delete(name);
            done.add(name);
        };
        Object.keys(circuit.blocks).forEach((n) => visit(n, []));
        return errors;
    }

    function partLine(p) {
        const kind = p.kind === 'block' ? p.block : p.kind;
        const src = p.src.map((s) => (s === null || s === undefined ? '-' : s));
        const at = Number.isFinite(p.x) && Number.isFinite(p.y) ? ` @${Math.round(p.x)},${Math.round(p.y)}` : '';
        return [kind, p.id, ...src].join(' ') + at;
    }

    // The text of a netlist: the blocks first, in the order they are needed.
    function formatNetlist(circuit) {
        const lines = [];
        const written = new Set();
        const write = (name) => {
            if (written.has(name) || !circuit.blocks[name]) return;
            written.add(name);
            for (const p of circuit.blocks[name].parts) if (p.kind === 'block') write(p.block);
            lines.push(`block ${name}`, ...circuit.blocks[name].parts.map((p) => `  ${partLine(p)}`), 'end');
        };
        Object.keys(circuit.blocks).forEach(write);
        lines.push(...circuit.parts.map(partLine));
        return lines.join('\n') + '\n';
    }

    // The blocks a circuit uses, directly or inside other blocks.
    function usedBlocks(circuit, parts = circuit.parts, seen = new Set()) {
        for (const p of parts) {
            if (p.kind !== 'block' || seen.has(p.block) || !circuit.blocks[p.block]) continue;
            seen.add(p.block);
            usedBlocks(circuit, circuit.blocks[p.block].parts, seen);
        }
        return seen;
    }

    // A netlist as a circuit of the engine: { circuit: { nodes }, resolve(src)
    // (the key whose value a wire of the top level carries), gates: count by
    // kind, inner: how many of them are inside blocks, blocks: instances at
    // the top level }. An input without a wire, and a loop of wires through
    // no gate, carry 0 (the key '#0', which nothing sets).
    function flatten(circuit) {
        const nodes = [];
        const alias = new Map();
        const gates = {};
        let inner = 0;
        const local = (prefix, s) => {
            if (s === null || s === undefined) return '#0';
            const [id, port] = s.split('.');
            return port ? `${prefix}${id}/${port}` : `${prefix}${id}`;
        };
        const expand = (def, prefix, depth) => {
            if (depth > LIMITS.depth) throw new Error(`Bausteine stecken mehr als ${LIMITS.depth}-mal ineinander.`);
            for (const p of def.parts) {
                if (p.kind === 'in') {
                    if (!prefix) nodes.push({ kind: 'in', id: p.id, label: p.id });
                } else if (p.kind === 'out') {
                    if (prefix) alias.set(`${prefix}${p.id}`, local(prefix, p.src[0]));
                    else nodes.push({ kind: 'out', id: `>${p.id}`, from: local(prefix, p.src[0]), label: p.id });
                } else if (p.kind === 'block') {
                    const block = circuit.blocks[p.block];
                    const inside = `${prefix}${p.id}/`;
                    pinsIn(block).forEach((pin, i) => alias.set(`${inside}${pin}`, local(prefix, p.src[i])));
                    expand(block, inside, depth + 1);
                } else {
                    gates[p.kind] = (gates[p.kind] || 0) + 1;
                    if (prefix) inner += 1;
                    if (nodes.length > LIMITS.gates) throw new Error(`Mehr als ${LIMITS.gates} Gatter gehen nicht.`);
                    nodes.push({ kind: p.kind, id: `${prefix}${p.id}`, inputs: p.src.map((s) => local(prefix, s)) });
                }
            }
        };
        expand(circuit, '', 0);
        const chase = (key) => {
            const seen = new Set();
            while (alias.has(key)) {
                if (seen.has(key)) return '#0';
                seen.add(key);
                key = alias.get(key);
            }
            return key;
        };
        for (const n of nodes) {
            if (n.inputs) n.inputs = n.inputs.map(chase);
            if (n.from) n.from = chase(n.from);
        }
        const blocks = circuit.parts.filter((p) => p.kind === 'block').length;
        return { circuit: { nodes }, resolve: (s) => chase(local('', s)), gates, inner, blocks };
    }

    // whether the gates feed back into themselves (a flip-flop): then the
    // circuit has a state and no truth table of its own
    function hasFeedback(flat) {
        const byId = new Map(flat.circuit.nodes.filter((n) => n.inputs).map((n) => [n.id, n]));
        const state = new Map();
        const visit = (id) => {
            if (state.get(id) === 1) return true;
            if (state.get(id) === 2 || !byId.has(id)) return false;
            state.set(id, 1);
            for (const k of byId.get(id).inputs) if (visit(k)) return true;
            state.set(id, 2);
            return false;
        };
        return [...byId.keys()].some(visit);
    }

    const Netlist = { parse: parseNetlist, format: formatNetlist, check: checkNetlist, flatten, hasFeedback, usedBlocks,
        pinsIn, pinsOut, GATE_KINDS, GATE_NAMES, NAME, BLOCK_NAME, KEYWORDS, LIMITS };

    const engine = { GATES, CIRCUITS, initial, round, settle, truthTable, setChoice, wires, junctions, valueAt, MAX_ROUNDS, Netlist,
        GW, PITCH, BUBBLE, SWITCH_W, SWITCH_H, LAMP, ROUND_MS, SHOWN_ROUNDS, gateHeightFor };
    if (typeof module !== 'undefined' && module.exports) {
        module.exports = engine;
        return;
    }

    // ------------------------------------------------------------------
    // The drawing

    const SVG = 'http://www.w3.org/2000/svg';
    const svg = (tag, attrs = {}, parent) => {
        const node = document.createElementNS(SVG, tag);
        for (const [k, v] of Object.entries(attrs)) if (v !== undefined && v !== null) node.setAttribute(k, v);
        if (parent) parent.appendChild(node);
        return node;
    };
    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'text') node.textContent = v;
            else if (k === 'html') node.innerHTML = v;
            else if (v !== undefined && v !== null) node.setAttribute(k, v);
        }
        for (const child of children) node.appendChild(typeof child === 'string' ? document.createTextNode(child) : child);
        return node;
    };
    const text = (parent, x, y, content, attrs = {}) => {
        const t = svg('text', Object.assign({ x, y }, attrs), parent);
        t.textContent = content;
        return t;
    };
    const pathOf = (points) => points.map((p, i) => `${i ? 'L' : 'M'}${p[0]} ${p[1]}`).join(' ');
    const bitsOf = (value, n) => Array.from({ length: n }, (_, i) => (value >> (n - 1 - i)) & 1).join('');

    const STYLE = `
.logic-circuit { --lc-one: #c98500; --lc-one-fill: #fff1cc; --lc-zero: #9aa1a9; --lc-ink: var(--bs-body-color);
    --lc-paper: var(--bs-body-bg); --lc-frame: #adb5bd; }
html[data-bs-theme="dark"] .logic-circuit { --lc-one: #f0a000; --lc-one-fill: #4a3a12; --lc-zero: #6c757d; --lc-frame: #5c636a; }
.logic-circuit { --widget: #5c677d; --widget-ink: #4a5264; --widget-chip: #ebecef; --widget-line: #dbdee3; --widget-tint: #fafafb;
    margin: 1.75em 0; padding: 0.9em 1.25em; border: 1px solid var(--widget-line); border-left: 5px solid var(--widget);
    border-radius: 0.6em; background: var(--widget-tint); box-shadow: 0 1px 3px rgba(92, 103, 125, 0.08); }
html[data-bs-theme="dark"] .logic-circuit { --widget: #858d9e; --widget-ink: #a5abb7; --widget-chip: #25272b; --widget-line: #2a2e34; --widget-tint: #1a1b1b; }
.logic-circuit::before { content: 'Zum Ausprobieren: Eingänge anklicken'; display: inline-block; padding: 0.15em 0.75em;
    border-radius: 1em; background-color: var(--widget-chip); color: var(--widget-ink); font-size: 80%; font-weight: 700;
    letter-spacing: 0.02em; margin-bottom: 0.7em; }
@media (pointer: coarse) { .logic-circuit::before { content: 'Zum Ausprobieren: Eingänge antippen'; } }
.logic-circuit .lc-title { font-weight: 600; font-size: 1.05em; margin: 0 0 0.5em; }
.logic-circuit .lc-presets { margin: 0 0 0.6em; }
.logic-circuit .lc-presets select { width: auto; max-width: 100%; }
.logic-circuit .lc-ops { display: flex; flex-wrap: wrap; gap: 0.35em; margin: 0 0 0.6em; align-items: center; }
.logic-circuit .lc-ops .btn { min-width: 3.4em; font-family: '0xProto Nerd Font Mono', monospace; }
.logic-circuit .lc-body { display: flex; flex-wrap: wrap; gap: 1em 1.5em; align-items: flex-start; }
.logic-circuit .lc-drawing { flex: 0 1 auto; min-width: 0; }
.logic-circuit svg.lc-svg { display: block; width: 100%; height: auto; overflow: visible; font-family: '0xProto Nerd Font Mono', monospace; }
.logic-circuit .lc-wire { fill: none; stroke: var(--lc-zero); stroke-width: 2; stroke-linejoin: round; transition: stroke 0.08s; }
.logic-circuit .lc-wire.lc-one { stroke: var(--lc-one); stroke-width: 3.2; }
.logic-circuit .lc-wire.lc-bus { stroke: color-mix(in srgb, var(--lc-ink) 50%, var(--lc-paper)); stroke-width: 4.5; }
.logic-circuit .lc-wire.lc-bus.lc-one { stroke: var(--lc-one); }
.logic-circuit .lc-wire.lc-control { stroke-dasharray: 5 4; }
.logic-circuit .lc-dot { fill: var(--lc-zero); }
.logic-circuit .lc-dot.lc-one { fill: var(--lc-one); }
.logic-circuit .lc-gate rect, .logic-circuit .lc-block rect.lc-box { fill: var(--lc-paper); stroke: var(--lc-ink); stroke-width: 1.6; }
.logic-circuit .lc-gate circle { fill: var(--lc-paper); stroke: var(--lc-ink); stroke-width: 1.6; }
.logic-circuit .lc-sign { fill: var(--lc-ink); font-size: calc(15px * var(--lc-grow, 1)); text-anchor: middle; dominant-baseline: central; }
.logic-circuit .lc-pin-label { fill: var(--bs-secondary-color); font-size: calc(11px * var(--lc-grow, 1)); dominant-baseline: central; }
.logic-circuit .lc-pin-label.lc-chosen { fill: var(--stepper-current); font-weight: 700; }
.logic-circuit .lc-label { fill: var(--lc-ink); font-size: calc(15px * var(--lc-grow, 1)); dominant-baseline: central; }
.logic-circuit .lc-label.lc-over { dominant-baseline: alphabetic; }
.logic-circuit .lc-bar { stroke: var(--lc-ink); stroke-width: 1.4; }
.lc-not { display: inline-block; line-height: 0.85; border-top: 0.07em solid currentColor; }
.logic-circuit .lc-frame { fill: none; stroke: var(--lc-frame); stroke-width: 1.2; stroke-dasharray: 4 4; }
.logic-circuit .lc-frame-label { fill: var(--bs-secondary-color); font-size: 11px; text-anchor: middle; font-family: 'IBM Plex Sans', sans-serif; }
.logic-circuit .lc-bus-label { fill: var(--lc-ink); font-size: calc(12px * var(--lc-grow, 1)); text-anchor: middle; paint-order: stroke; stroke: var(--widget-tint); stroke-width: 4px; }
.logic-circuit .lc-switch { cursor: pointer; outline: none; }
.logic-circuit .lc-switch .lc-hit { fill: transparent; }
.logic-circuit .lc-switch rect.lc-key { fill: var(--lc-paper); stroke: var(--lc-zero); stroke-width: 2; }
.logic-circuit .lc-switch.lc-one rect.lc-key { fill: var(--lc-one-fill); stroke: var(--lc-one); }
.logic-circuit .lc-switch text { fill: var(--lc-ink); font-size: 15px; font-weight: 700; text-anchor: middle; dominant-baseline: central; pointer-events: none; }
.logic-circuit .lc-switch .lc-focus { fill: none; stroke: var(--stepper-current); stroke-width: 2.5; opacity: 0; }
.logic-circuit .lc-switch:focus-visible .lc-focus { opacity: 1; }
.logic-circuit .lc-switch:hover rect.lc-key { stroke-width: 3; }
.logic-circuit .lc-fixed { fill: var(--bs-secondary-color); font-size: 14px; text-anchor: middle; dominant-baseline: central; }
.logic-circuit .lc-lamp circle { fill: var(--lc-paper); stroke: var(--lc-zero); stroke-width: 2; }
.logic-circuit .lc-lamp.lc-one circle { fill: var(--lc-one); stroke: var(--lc-one); }
.logic-circuit .lc-lamp text { fill: var(--lc-ink); font-size: 14px; font-weight: 700; text-anchor: middle; dominant-baseline: central; }
.logic-circuit .lc-lamp.lc-one text { fill: #1b1530; }
.logic-circuit .lc-choice rect { fill: var(--lc-paper); stroke: var(--lc-ink); stroke-width: 1.6; }
.logic-circuit .lc-choice .lc-value { fill: var(--lc-ink); font-size: 17px; font-weight: 700; text-anchor: middle; dominant-baseline: central; }
.logic-circuit .lc-table { flex: 0 0 auto; font-family: '0xProto Nerd Font Mono', monospace; }
.logic-circuit .lc-table table { border-collapse: collapse; margin: 0; }
.logic-circuit .lc-table th, .logic-circuit .lc-table td { min-width: 2.8em; padding: 0.15em 0.7em; text-align: center; border-bottom: 1px solid var(--widget-line); }
.logic-circuit .lc-table th { font-weight: 600; }
.logic-circuit .lc-table th.lc-gap, .logic-circuit .lc-table td.lc-gap { border-left: 2px solid var(--widget-line); }
.logic-circuit .lc-table tbody tr { cursor: pointer; }
.logic-circuit .lc-table tbody tr:hover { background: color-mix(in srgb, var(--stepper-current) 6%, transparent); }
.logic-circuit .lc-table tbody tr.lc-now { background: color-mix(in srgb, var(--stepper-current) 16%, transparent); box-shadow: inset 3px 0 0 var(--stepper-current); }
.logic-circuit .lc-table tbody tr:focus-visible { outline: 2px solid var(--stepper-current); outline-offset: -2px; }
.logic-circuit .lc-say, .logic-circuit .lc-readout { margin: 0.6em 0 0; min-height: 1.5em; }
.logic-circuit .lc-readout code, .logic-circuit .lc-say code { color: inherit; background: color-mix(in srgb, var(--stepper-current) 9%, transparent);
    padding: 0.05em 0.3em; border-radius: 0.25em; }
.logic-circuit .lc-legend { margin: 0.5em 0 0; font-size: 85%; color: var(--bs-secondary-color); display: flex; flex-wrap: wrap; gap: 0.3em 1.2em; }
.logic-circuit .lc-legend svg { vertical-align: middle; margin-right: 0.35em; }
.logic-circuit .lc-release { margin: 0.6em 0 0; }
/* a truth table in the text of the page, like the widgets' tables: every
   column as wide as the others, the values centred in the code font, a
   line between the inputs and the rest */
table.table.lc-truth-table { width: auto; table-layout: fixed; }
table.lc-truth-table th, table.lc-truth-table td { width: 4.6em; text-align: center; vertical-align: bottom; }
table.lc-truth-table th { font-weight: 600; line-height: 1.25; padding-bottom: 0.35em; white-space: normal; overflow-wrap: normal; }
table.lc-truth-table td { font-family: '0xProto Nerd Font Mono', monospace; }
table.lc-truth-table .lc-gap { border-left: 2px solid var(--bs-border-color); }
.logic-circuit .lc-inputs { display: none; }
.logic-circuit.lc-small .lc-inputs { display: flex; flex-wrap: wrap; gap: 0.5em 1em; margin: 0.6em 0 0; }
.logic-circuit .lc-input-group { display: inline-flex; align-items: center; gap: 0.25em; }
.logic-circuit .lc-input-name { font-family: '0xProto Nerd Font Mono', monospace; margin-right: 0.2em; }
.logic-circuit .lc-input-button { min-width: 44px; min-height: 44px; padding: 0; font-family: '0xProto Nerd Font Mono', monospace; font-weight: 700; }
.logic-circuit .lc-input-button.lc-one { background: var(--lc-one-fill); border-color: var(--lc-one); color: var(--lc-ink); }
`;

    // A gate as DIN EN 60617 draws it: the rectangle, its sign, the circle of
    // a negated output. Also the editor's (circuit_editor.js).
    function drawGateShape(parent, x, y, kind, count) {
        const g = svg('g', { class: 'lc-gate' }, parent);
        const h = gateHeightFor(count);
        svg('rect', { x, y, width: GW, height: h }, g);
        text(g, x + GW / 2, y + 15, GATES[kind].sign, { class: 'lc-sign' });
        if (GATES[kind].neg) svg('circle', { cx: x + GW + BUBBLE, cy: y + h / 2, r: BUBBLE }, g);
        return g;
    }

    function addStyle() {
        if (document.getElementById('logic-circuit-style')) return;
        const style = html('style', { id: 'logic-circuit-style' });
        style.textContent = STYLE;
        document.head.appendChild(style);
    }

    const reduced = () => window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    function mount(el) {
        const names = (el.dataset.circuit || '').trim().split(/\s+/).filter((n) => CIRCUITS[n]);
        if (!names.length) return;
        let choice = 0;
        const box = html('div', { class: 'lc-box' });
        const parts = [];
        if (el.dataset.title) parts.push(html('div', { class: 'lc-title', text: el.dataset.title }));
        if (names.length > 1) {
            const select = html('select', { class: 'form-select form-select-sm', 'aria-label': 'Schaltung' },
                names.map((n, i) => html('option', { value: String(i), text: CIRCUITS[n]().label || n })));
            select.addEventListener('change', () => { choice = Number(select.value); build(); });
            parts.push(html('div', { class: 'lc-presets' }, [select]));
        }
        parts.push(box);
        el.replaceChildren(...parts);
        build();

        function build() {
            const circuit = CIRCUITS[names[choice]]();
            show(box, circuit);
        }
    }

    // Draws one circuit into box and wires up its switches.
    function show(box, circuit) {
        let values = initial(circuit);
        let stable = true;
        let timer = null;
        const list = wires(circuit);
        const dots = junctions(list);

        const drawing = svg('svg', { class: 'lc-svg', viewBox: `0 0 ${circuit.w} ${circuit.h}`, role: 'group',
            'aria-label': `Schaltung: ${circuit.label || ''}` });
        drawing.style.maxWidth = `${Math.round(circuit.w * 1.15)}px`;
        const layers = { frames: svg('g', {}, drawing), wires: svg('g', {}, drawing), parts: svg('g', {}, drawing), labels: svg('g', {}, drawing) };

        for (const f of circuit.frames || []) {
            svg('rect', { class: 'lc-frame', x: f.x, y: f.y, width: f.w, height: f.h, rx: 6 }, layers.frames);
            text(layers.frames, f.x + f.w / 2, f.y + f.h + 13, f.label, { class: 'lc-frame-label' });
        }
        const wireEls = list.map((w) => ({ w, el: svg('path', { class: `lc-wire${w.width > 1 ? ' lc-bus' : ''}${w.control ? ' lc-control' : ''}`, d: pathOf(w.points) }, layers.wires) }));
        const dotEls = dots.map(({ p, src }) => {
            const width = list.find((w) => w.src === src).width;
            return { src, el: svg('circle', { class: 'lc-dot', cx: p[0], cy: p[1], r: width > 1 ? 4.5 : 3.5 }, layers.wires) };
        });
        const busLabels = (circuit.busLabels || []).map((b) => ({ b, el: text(layers.labels, b.x, b.y, '', { class: 'lc-bus-label' }) }));
        for (const l of circuit.wireLabels || []) text(layers.labels, l.x, l.y, l.text, { class: 'lc-pin-label', 'text-anchor': 'middle' });

        const switches = [];
        const lamps = [];
        const bars = [];
        const pinMarks = [];
        let choiceText = null;
        for (const n of circuit.nodes) {
            if (n.kind === 'in' && n.fixed) {
                text(layers.parts, n.x, n.y, '0', { class: 'lc-fixed' });
            } else if (n.kind === 'in') {
                switches.push(Object.assign(drawSwitch(layers.parts, n, n.id, null), { group: n.label || n.id }));
            } else if (n.kind === 'bus-in') {
                text(layers.parts, n.x - 30, n.y, n.label, { class: 'lc-label', 'text-anchor': 'end' });
                for (let i = 0; i < n.bits; i++) {
                    const bit = n.bits - 1 - i;
                    switches.push(Object.assign(drawSwitch(layers.parts, { x: n.x + i * 36, y: n.y, label: '', id: `${n.id}${bit}` }, n.id, bit), { group: n.label }));
                }
            } else if (n.kind === 'out') {
                lamps.push(drawLamp(layers.parts, n));
            } else if (n.kind === 'bus-out') {
                text(layers.parts, n.x + 1.5 * 30, n.y - 26, n.label, { class: 'lc-label', 'text-anchor': 'middle' });
                for (let i = 0; i < n.bits; i++) {
                    lamps.push(drawLamp(layers.parts, { x: n.x + 12 + i * 30, y: n.y, from: n.from, bit: n.bits - 1 - i, label: '', small: true }));
                }
            } else if (n.kind === 'choice') {
                const g = svg('g', { class: 'lc-choice' }, layers.parts);
                svg('rect', { x: n.x, y: n.y, width: n.w, height: n.h, rx: 4 }, g);
                text(g, n.x + n.w / 2, n.y + 13, n.label, { class: 'lc-pin-label', 'text-anchor': 'middle' });
                choiceText = text(g, n.x + n.w / 2, n.y + 35, '', { class: 'lc-value' });
            } else if (GATES[n.kind]) {
                drawGate(layers.parts, n);
            } else {
                pinMarks.push(...drawBlock(layers.parts, n));
            }
        }

        const body = html('div', { class: 'lc-body' });
        // on a narrow screen the drawing is small: the inputs once more as
        // buttons under it, large enough for a finger, and the labels grow
        const groups = [];
        for (const sw of switches) {
            if (!groups.length || groups[groups.length - 1].name !== sw.group || sw.bit === null) groups.push({ name: sw.group, items: [] });
            const b = html('button', { type: 'button', class: 'btn btn-outline-secondary lc-input-button' });
            b.addEventListener('click', sw.toggle);
            sw.button = b;
            groups[groups.length - 1].items.push(b);
        }
        const inputRow = html('div', { class: 'lc-inputs', role: 'group', 'aria-label': 'Eingänge' },
            groups.map((gr) => html('span', { class: 'lc-input-group' }, [html('span', { class: 'lc-input-name', text: gr.name }), ...gr.items])));
        const drawingBox = html('div', { class: 'lc-drawing' }, [drawing, inputRow]);
        drawingBox.style.width = drawing.style.maxWidth;
        const widget = box.closest('.logic-circuit');
        const fit = () => {
            const scale = drawing.getBoundingClientRect().width / circuit.w;
            if (!scale) return;
            widget.classList.toggle('lc-small', scale < 0.8);
            drawing.style.setProperty('--lc-grow', String(Math.min(1.3, Math.max(1, 1 / scale))));
        };
        // the bar of a negated name: as wide as its letters, 0.1em above
        // their cap height (the font's own, measured)
        const placeBars = () => {
            for (const { text: t, line } of bars) {
                const box = t.getBBox();
                if (!box.width) continue;
                const size = parseFloat(getComputedStyle(t).fontSize) || 15;
                const ctx = document.createElement('canvas').getContext('2d');
                ctx.font = `${size}px ${getComputedStyle(t).fontFamily}`;
                const cap = ctx.measureText(t.textContent).actualBoundingBoxAscent || 0.7 * size;
                const baseline = Number(t.getAttribute('y')) + 0.36 * size;
                const y = baseline - cap - 0.1 * size;
                const inset = 0.08 * size;
                for (const [k, v] of Object.entries({ x1: box.x + inset, x2: box.x + box.width - inset, y1: y, y2: y })) line.setAttribute(k, v.toFixed(2));
            }
        };
        if (window.ResizeObserver) new ResizeObserver(() => { fit(); placeBars(); }).observe(drawingBox);
        if (document.fonts && document.fonts.ready) document.fonts.ready.then(placeBars);
        body.appendChild(drawingBox);
        let tableRows = null;
        if (circuit.table) {
            const t = truthTable(circuit);
            const head = html('tr', {}, [...t.inputs.map((n) => html('th', { text: n })), ...t.outputs.map((n, i) => html('th', { class: i === 0 ? 'lc-gap' : null, text: n }))]);
            tableRows = t.rows.map((row) => {
                const tr = html('tr', { tabindex: '0', title: 'Diese Eingänge einstellen' },
                    [...row.inputs.map((x) => html('td', { text: String(x) })), ...row.outputs.map((x, i) => html('td', { class: i === 0 ? 'lc-gap' : null, text: String(x) }))]);
                const pick = () => {
                    const ins = circuit.nodes.filter((n) => n.kind === 'in' && !n.fixed);
                    const start = Object.assign({}, values);
                    ins.forEach((n, i) => { start[n.id] = row.inputs[i]; });
                    run(start);
                };
                tr.addEventListener('click', pick);
                tr.addEventListener('keydown', (e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); pick(); } });
                return { tr, row };
            });
            const table = html('table', { class: 'lc-truth', 'aria-label': 'Wahrheitstabelle' }, [html('thead', {}, [head]), html('tbody', {}, tableRows.map((r) => r.tr))]);
            body.appendChild(html('div', { class: 'lc-table' }, [table]));
        }

        const ops = [];
        const choiceNode = circuit.nodes.find((n) => n.kind === 'choice');
        const extra = [];
        if (choiceNode) {
            const row = html('div', { class: 'lc-ops', role: 'group', 'aria-label': 'Befehl' }, [html('span', { class: 'lc-ops-label', text: 'Befehl:' })]);
            choiceNode.options.forEach((option, i) => {
                const b = html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', 'aria-pressed': 'false', text: option.label });
                b.addEventListener('click', () => {
                    const start = Object.assign({}, values);
                    setChoice(circuit, start, choiceNode.id, i);
                    run(start);
                });
                ops.push(b);
                row.appendChild(b);
            });
            extra.push(row);
        }
        const say = html('div', { class: 'lc-say', 'aria-live': 'polite' });
        const readout = circuit.readout ? html('div', { class: 'lc-readout', 'aria-live': 'polite' }) : null;
        let release = null;
        if (circuit.release) {
            release = html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary lc-release',
                text: `${circuit.release.map((id) => circuit.nodes.find((n) => n.id === id).label).join(' und ')} gleichzeitig auf 0` });
            release.addEventListener('click', () => {
                const start = Object.assign({}, values);
                for (const id of circuit.release) start[id] = 0;
                run(start);
            });
        }
        const legend = html('div', { class: 'lc-legend' }, [
            legendItem('one', '1'), legendItem('zero', '0'),
            ...(dots.length ? [html('span', {}, [legendDot(), 'verbunden; Leitungen, die sich ohne Punkt kreuzen, nicht'])] : []),
        ]);
        box.replaceChildren(...extra, body, ...(readout ? [readout] : []), say, ...(release ? [release] : []), legend);

        function legendItem(kind, label) {
            const s = svg('svg', { width: 26, height: 10, 'aria-hidden': 'true' });
            svg('path', { d: 'M1 5 L25 5', class: `lc-wire${kind === 'one' ? ' lc-one' : ''}` }, s);
            return html('span', {}, [s, `Leitung mit ${label}`]);
        }

        function legendDot() {
            const s = svg('svg', { width: 12, height: 10, 'aria-hidden': 'true' });
            svg('circle', { cx: 6, cy: 5, r: 3.5, class: 'lc-dot' }, s);
            return s;
        }

        function drawSwitch(parent, n, id, bit) {
            const g = svg('g', { class: 'lc-switch', tabindex: '0', role: 'button' }, parent);
            svg('rect', { class: 'lc-hit', x: n.x - 22, y: n.y - 22, width: 44, height: 44 }, g);
            svg('rect', { class: 'lc-focus', x: n.x - SWITCH_W / 2 - 4, y: n.y - SWITCH_H / 2 - 4, width: SWITCH_W + 8, height: SWITCH_H + 8, rx: 8 }, g);
            svg('rect', { class: 'lc-key', x: n.x - SWITCH_W / 2, y: n.y - SWITCH_H / 2, width: SWITCH_W, height: SWITCH_H, rx: 5 }, g);
            const value = text(g, n.x, n.y + 1, '0');
            if (n.label) {
                if (n.labelAbove) text(parent, n.x, n.y - SWITCH_H / 2 - 10, n.label, { class: 'lc-label', 'text-anchor': 'middle' });
                else text(parent, n.x - SWITCH_W / 2 - 8, n.y, n.label, { class: 'lc-label', 'text-anchor': 'end' });
            }
            const toggle = () => {
                const start = Object.assign({}, values);
                if (bit === null) start[id] = values[id] ? 0 : 1;
                else start[id] = values[id] ^ (1 << bit);
                run(start);
            };
            g.addEventListener('click', toggle);
            g.addEventListener('keydown', (e) => { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); toggle(); } });
            return { g, value, id, bit, toggle, label: bit === null ? (n.label || id) : `${id}${bit}` };
        }

        function drawLamp(parent, n) {
            const g = svg('g', { class: 'lc-lamp' }, parent);
            const r = n.small ? 11 : LAMP;
            svg('circle', { cx: n.x, cy: n.y, r }, g);
            const value = text(g, n.x, n.y + 1, '0');
            if (n.label) {
                const attrs = { class: `lc-label${n.over ? ' lc-over' : ''}` };
                // a negated name: on its baseline, so that its bar can be put just above the letters
                if (n.over) Object.assign(attrs, { dy: '0.36em', 'aria-label': `nicht ${n.label}` });
                const t = n.labelBelow
                    ? text(parent, n.x, n.y + LAMP + 14, n.label, Object.assign(attrs, { 'text-anchor': 'middle' }))
                    : text(parent, n.x + LAMP + 7, n.y, n.label, attrs);
                if (n.over) bars.push({ text: t, line: svg('line', { class: 'lc-bar' }, parent) });
            }
            return { g, value, from: n.from, bit: n.bit === undefined ? null : n.bit };
        }

        function drawGate(parent, n) {
            drawGateShape(parent, n.x, n.y, n.kind, n.reduce ? 1 : n.inputs.length);
        }

        function drawBlock(parent, n) {
            const g = svg('g', { class: 'lc-block' }, parent);
            svg('rect', { class: 'lc-box', x: n.x, y: n.y, width: n.w, height: n.h }, g);
            const big = n.kind === 'add' || n.kind === 'mux';
            text(g, n.x + n.w / 2, n.kind === 'mux' ? n.y + 86 : n.y + n.h / 2, n.sign,
                { class: 'lc-sign', style: big && n.kind === 'add' ? 'font-size: 20px' : n.kind === 'mux' ? 'font-size: 12px' : null });
            const marks = [];
            const labels = n.pinLabels || {};
            // a label inside the block, beside the side its pin is on
            const label = (dx, dy, content) => {
                if (dx === 0) return text(g, n.x + 5, n.y + dy, content, { class: 'lc-pin-label' });
                if (dx === n.w) return text(g, n.x + n.w - 5, n.y + dy, content, { class: 'lc-pin-label', 'text-anchor': 'end' });
                if (dy === 0) return text(g, n.x + dx, n.y + 9, content, { class: 'lc-pin-label', 'text-anchor': 'middle' });
                return text(g, n.x + dx, n.y + n.h - 9, content, { class: 'lc-pin-label', 'text-anchor': 'middle' });
            };
            (labels.in || []).forEach((content, i) => {
                if (!content) return;
                const [dx, dy] = n.pins.in[i];
                marks.push({ node: n, index: i, el: label(dx, dy, content) });
            });
            for (const [port, content] of Object.entries(labels.out || {})) {
                const [dx, dy] = n.pins.out[port];
                label(dx, dy, content);
            }
            return marks;
        }

        // shows values: wires, dots, switches, lamps, the table, the text
        function paint(v, settled) {
            for (const { w, el } of wireEls) el.classList.toggle('lc-one', valueAt(v, w.src) !== 0);
            for (const { src, el } of dotEls) el.classList.toggle('lc-one', valueAt(v, src) !== 0);
            for (const s of switches) {
                const on = s.bit === null ? v[s.id] : (v[s.id] >> s.bit) & 1;
                s.g.classList.toggle('lc-one', !!on);
                s.value.textContent = on ? '1' : '0';
                s.g.setAttribute('aria-pressed', on ? 'true' : 'false');
                s.g.setAttribute('aria-label', `Eingang ${s.label}: ${on ? 1 : 0}, umschalten`);
                s.button.textContent = on ? '1' : '0';
                s.button.classList.toggle('lc-one', !!on);
                s.button.setAttribute('aria-pressed', on ? 'true' : 'false');
                s.button.setAttribute('aria-label', `Eingang ${s.label}: ${on ? 1 : 0}, umschalten`);
            }
            for (const l of lamps) {
                const x = valueAt(v, l.from);
                const on = l.bit === null ? x : (x >> l.bit) & 1;
                l.g.classList.toggle('lc-one', !!on);
                l.value.textContent = on ? '1' : '0';
            }
            for (const { b, el } of busLabels) el.textContent = bitsOf(valueAt(v, b.from), 4);
            if (choiceNode) {
                const i = v[choiceNode.id];
                choiceText.textContent = choiceNode.options[i].label;
                ops.forEach((b, k) => {
                    b.setAttribute('aria-pressed', k === i ? 'true' : 'false');
                    b.classList.toggle('btn-secondary', k === i);
                    b.classList.toggle('btn-outline-secondary', k !== i);
                });
                const mux = circuit.nodes.find((n) => n.kind === 'mux');
                const sel = v[`${choiceNode.id}:sel`];
                for (const m of pinMarks) if (m.node === mux) m.el.classList.toggle('lc-chosen', mux.selects[sel].includes(m.index));
            }
            if (tableRows) {
                const ins = circuit.nodes.filter((n) => n.kind === 'in' && !n.fixed).map((n) => v[n.id]);
                for (const { tr, row } of tableRows) tr.classList.toggle('lc-now', row.inputs.every((x, i) => x === ins[i]));
            }
            if (readout) readout.innerHTML = circuit.readout(v);
            if (circuit.say) say.innerHTML = circuit.say(v, settled);
            if (release) release.hidden = !circuit.release.every((id) => v[id]);
        }

        // from start (an input changed), the rounds one after another
        function run(start) {
            if (timer) clearTimeout(timer);
            const result = settle(circuit, start);
            stable = result.stable;
            const rounds = stable ? result.rounds : result.rounds.slice(0, SHOWN_ROUNDS + 1);
            if (reduced() || rounds.length <= 1) {
                values = rounds[rounds.length - 1];
                paint(values, stable);
                return;
            }
            let k = 0;
            const next = () => {
                values = rounds[k];
                const last = k === rounds.length - 1;
                paint(values, last ? stable : true);
                if (!last) {
                    k += 1;
                    timer = setTimeout(next, ROUND_MS);
                } else timer = null;
            };
            next();
        }

        // the start: whatever the circuit holds, settled at once
        const first = settle(circuit, values);
        values = first.rounds[first.rounds.length - 1];
        paint(values, first.stable);
    }

    function start() {
        addStyle();
        document.querySelectorAll('.logic-circuit[data-circuit]').forEach((el) => {
            if (!el.dataset.mounted) {
                el.dataset.mounted = '1';
                mount(el);
            }
        });
    }
    // what the editor draws with
    engine.draw = { svg, html, text, pathOf, gate: drawGateShape, addStyle };
    window.LogicCircuit = engine;
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
