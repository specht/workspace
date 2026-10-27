// Automatron in the browser (src/static/include/automatron.js) and the
// layout of its drawings (automaton_layout.js).
const test = require('node:test');
const assert = require('node:assert');
const Automatron = require('../../src/static/include/automatron.js');
require('../../src/static/include/automaton_layout.js');
const Layout = globalThis.AutomatonLayout;

const step = (dfa, s, ch) => dfa.transitions.get(s).get(ch);

test('a classroom expression works', () => {
    const r = Automatron.run('a(b|a)*a+');
    assert.ok(!r.error);
    assert.ok(r.automata.dfa.states.length <= Automatron.MAX_STATES);
    for (const w of r.samples_positive) assert.ok(Automatron.accepts(r.automata.min, w), w);
    for (const w of r.samples_negative) assert.ok(!Automatron.accepts(r.automata.min, w), w);
});

// The automata activity names these states: s0 to s4 are the lock's q0 to
// q4, and the arrows back go where its text says
test('the lock 1213 gets the states s0 to s4 of the activity', () => {
    const r = Automatron.run('(0|1|2|3|4|5|6|7|8|9)*1213');
    const min = r.automata.min;
    assert.deepStrictEqual(min.states, [0, 1, 2, 3, 4]);
    assert.strictEqual(min.start, 0);
    assert.deepStrictEqual([...min.accepts], [4]);
    assert.deepStrictEqual([step(min, 0, '1'), step(min, 1, '2'), step(min, 2, '1'), step(min, 3, '3')], [1, 2, 3, 4]);
    assert.deepStrictEqual([step(min, 1, '1'), step(min, 3, '2'), step(min, 3, '1'), step(min, 4, '1'), step(min, 0, '7')], [1, 2, 1, 1, 0]);
    assert.ok(!r.min_dfa.implicit_sink.has_sink);
});

test('an exploding expression stops early', () => {
    const started = Date.now();
    const r = Automatron.run('(a|b)*a' + '(a|b)'.repeat(19));
    assert.strictEqual(r.error, 'too_large');
    assert.strictEqual(r.max_states, Automatron.MAX_STATES);
    assert.ok(Date.now() - started < 2000);
});

// typos are the student's: a message, never an exception
test('broken expressions say what is wrong', () => {
    const expected = { 'a$b': 'character', '(ab': 'parentheses', 'ab)': 'parentheses', 'a|': 'incomplete', '|a': 'incomplete',
        '*a': 'incomplete', '()': 'incomplete', 'a(|b)': 'incomplete' };
    for (const [regex, error] of Object.entries(expected)) assert.strictEqual(Automatron.run(regex).error, error, regex);
    assert.strictEqual(Automatron.run('a$b').character, '$');
    assert.strictEqual(Automatron.run('a'.repeat(201)).error, 'regex too long');
});

test('a long expression keeps its smallest DEA small', () => {
    const [a, b, c] = ['(0|3|6|9)', '(1|4|7)', '(2|5|8)'];
    const r = Automatron.run(`(${a}|${c}${a}*${b}|(${b}|${c}${a}*${c})(${a}|${b}${a}*${c})*(${c}|${b}${a}*${b}))*`);
    assert.strictEqual(r.automata.min.states.length, 3);
    // the DEA on the way is too big to draw: its table instead
    assert.ok(r.dfa.implicit_sink.drawing.states.length > 14);
    assert.match(Layout.svg(r.dfa.implicit_sink.drawing), /^<table class="automaton-table">/);
    assert.match(Layout.svg(r.min_dfa.implicit_sink.drawing), /^<svg /);
});

test('symbols on one arrow become ranges', () => {
    assert.strictEqual(Layout.joinLabels(['9', '0', '2', '3', '4', '5', '6', '7', '8']), '0, 2–9');
    assert.strictEqual(Layout.joinLabels(['b', 'a', 'ε']), 'a, b, ε');
    assert.strictEqual(Layout.joinLabels(['a', 'c', 'd', 'e', 'x']), 'a, c–e, x');
});

test('every arrow ends on its target and every label keeps clear of the states', () => {
    for (const regex of ['(a|b)*abb', 'a(b|a)*a+', '(ab|ba)*', '(0|1)*1(0|1)(0|1)', '(a|b)*aba(a|b)*', '(0|1|2|3|4|5|6|7|8|9)*1213', 'ab*c|bc*a']) {
        const r = Automatron.run(regex);
        for (const drawing of [r.nfa.drawing, r.dfa.implicit_sink.drawing, r.min_dfa.implicit_sink.drawing]) {
            if (!Layout.svg(drawing).startsWith('<svg')) continue;
            const L = Layout.layout(drawing);
            for (const p of L.paths) {
                const end = p.d.trim().split(/[ ,]+/).slice(-2).map(Number);
                const [x, y] = L.pos.get(p.e.to);
                assert.ok(Math.abs(Math.hypot(end[0] - x, end[1] - y) - Layout.R) < 3, `${regex}: ${p.e.from} → ${p.e.to}`);
                for (const [sx, sy] of L.pos.values()) {
                    const b = p.box;
                    const dx = Math.max(b.x - sx, 0, sx - b.x - b.w);
                    const dy = Math.max(b.y - sy, 0, sy - b.y - b.h);
                    assert.ok(Math.hypot(dx, dy) >= Layout.R, `${regex}: label ${p.e.label}`);
                }
            }
        }
    }
});

// A pushdown automaton's or a Turing machine's labels have spaces and
// commas: they come in data-arrows, one line per transition, the box as
// tall as its lines and clear of the states
test('labels with spaces and commas stand one above the other', () => {
    const a = Layout.fromData({ states: 'q0 q1 qH', start: 'q0', accept: 'qH',
        arrows: 'q0 | 0 / 0, R | q0; q0 | 1 / 1, R | q0; q0 | _ / _, L | q1; q1 | 0 / 1, L | qH; q1 | _ / 1, L | qH' });
    assert.deepStrictEqual(a.edges.map((e) => e.label), ['0 / 0, R', '1 / 1, R', '_ / _, L', '0 / 1, L', '_ / 1, L']);
    const L = Layout.layout(a);
    const loop = L.paths.find((p) => p.e.from === 'q0' && p.e.to === 'q0');
    assert.strictEqual(loop.e.label, '0 / 0, R\n1 / 1, R');
    assert.strictEqual(loop.box.h, 30);
    for (const p of L.paths) for (const [sx, sy] of L.pos.values()) {
        const b = p.box;
        assert.ok(Math.hypot(Math.max(b.x - sx, 0, sx - b.x - b.w), Math.max(b.y - sy, 0, sy - b.y - b.h)) >= Layout.R, p.e.label);
    }
    const svg = Layout.svg(a);
    assert.match(svg, /1 \/ 1, R<\/tspan>/);
    // qH is q with a small H, like q0
    assert.match(svg, />q<tspan dy="0.3em" font-size="0.7em">H<\/tspan>/);
});
