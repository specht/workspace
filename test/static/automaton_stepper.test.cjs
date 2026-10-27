// The engine of the automaton stepper (src/static/include/automaton_stepper.js):
// a DEA, the bracket automaton of »Kellerautomaten« and the machines of
// »Turingmaschinen«, step by step, as the widget shows them.
const test = require('node:test');
const assert = require('node:assert');
const { machine, arrows, trace, written } = require('../../src/static/include/automaton_stepper.js');

const PLUS = {
    kind: 'turing', states: 'q0 q1 qH', start: 'q0', accept: 'qH',
    rules: 'q0 0 0 R q0; q0 1 1 R q0; q0 _ _ L q1; q1 1 0 L q1; q1 0 1 L qH; q1 _ 1 L qH',
};
const BRACKETS = {
    kind: 'pushdown', states: 'q0', start: 'q0', accept: 'q0',
    rules: 'q0 ( # #( q0; q0 ( ( (( q0; q0 ) ( ε q0',
};
const last = (steps) => steps[steps.length - 1];

test('a DEA reads as before', () => {
    const a = machine({ states: 'q0 q1', start: 'q0', accept: 'q0', transitions: 'q0 a q1, q0 b q0, q1 a q0, q1 b q1' });
    const steps = trace(a, 'abbab');
    assert.strictEqual(a.kind, 'dfa');
    assert.strictEqual(steps.length, 7);
    assert.deepStrictEqual(steps[1].used, [['q0', 'q1']]);
    assert.strictEqual(last(steps).accepted, true);
});

test('the Turing machine adds 1 to 1011, the head running right and then back', () => {
    const steps = trace(machine(PLUS), '1011');
    assert.deepStrictEqual(steps.map((s) => s.head), [0, 1, 2, 3, 4, 3, 2, 1, 0]);
    assert.deepStrictEqual(steps.map((s) => s.states[0]), ['q0', 'q0', 'q0', 'q0', 'q0', 'q1', 'q1', 'q1', 'qH']);
    assert.strictEqual(written(last(steps).tape), '1100');
    assert.ok(last(steps).end);
    assert.match(last(steps).message, /Auf dem Band steht jetzt »1100«/);
});

test('each step names what it read and wrote, and the arrow it took', () => {
    const steps = trace(machine(PLUS), '1011');
    // the fifth step reads the empty cell behind the number
    assert.strictEqual(steps[5].read, '_');
    assert.deepStrictEqual(steps[5].used, [['q0', 'q1']]);
    assert.strictEqual(steps[5].rule.label, '_ / _, L');
    assert.strictEqual(steps[5].at, 4);
    // the carry: a 1 becomes 0
    assert.strictEqual(steps[6].rule.label, '1 / 0, L');
    assert.strictEqual(steps[6].tape.get(3), '0');
    assert.match(steps[6].message, /schreibt »0«, geht nach links und bleibt in q1/);
});

test('111 gets a new digit in front, on the cell left of the input', () => {
    const steps = trace(machine(PLUS), '111');
    assert.strictEqual(written(last(steps).tape), '1000');
    assert.strictEqual(last(steps).tape.get(-1), '1');
});

test('a Turing machine without a rule for what it reads stops', () => {
    const a = machine({ kind: 'turing', result: 'accept', states: 'q0 qJ', start: 'q0', accept: 'qJ', rules: 'q0 a a R q0; q0 _ _ R qJ' });
    assert.strictEqual(last(trace(a, 'aa')).accepted, true);
    const no = last(trace(a, 'ab'));
    assert.strictEqual(no.accepted, false);
    assert.match(no.message, /keinen Übergang/);
});

test('a machine that never stops is cut off', () => {
    const a = machine({ kind: 'turing', states: 'q0 qH', start: 'q0', accept: 'qH', rules: 'q0 _ _ R q0' });
    const steps = trace(a, '');
    assert.ok(steps.length > 100 && steps.length < 300);
    assert.match(last(steps).message, /hält die Maschine noch immer nicht/);
});

test('the bracket automaton keeps a stack and accepts (()())', () => {
    const steps = trace(machine(BRACKETS), '(()())');
    assert.deepStrictEqual(steps.map((s) => s.stack.join('')), ['#', '#(', '#((', '#(', '#((', '#(', '#', '#']);
    assert.deepStrictEqual(steps[3].used, [['q0', 'q0']]);
    assert.strictEqual(steps[3].rule.label, '), ( / ε');
    assert.strictEqual(steps[2].rule.label, '(, ( / ((');
    assert.strictEqual(last(steps).accepted, true);
});

test('the bracket automaton rejects (() and ())', () => {
    const open = last(trace(machine(BRACKETS), '(()'));
    assert.strictEqual(open.accepted, false);
    assert.match(open.message, /noch #\(/);
    const closed = trace(machine(BRACKETS), '())');
    assert.strictEqual(last(closed).accepted, false);
    assert.strictEqual(closed.length, 4);
    assert.match(last(closed).message, /keinen Übergang/);
});

test('the drawing gets one arrow per rule, labelled as on the pages', () => {
    assert.deepStrictEqual(arrows(machine(BRACKETS)).map((e) => e.label), ['(, # / #(', '(, ( / ((', '), ( / ε']);
    assert.ok(arrows(machine(PLUS)).some((e) => e.from === 'q1' && e.to === 'qH' && e.label === '0 / 1, L'));
});
