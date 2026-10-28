// The engine of the circuit widget (src/static/include/logic_circuit.js):
// gates, adders, the flip-flops and the ALU of »Vom Gatter zum Rechenwerk«,
// computed round by round as the widget shows them.
const test = require('node:test');
const assert = require('node:assert');
const { CIRCUITS, initial, settle, truthTable, setChoice, wires, junctions, valueAt } = require('../../src/static/include/logic_circuit.js');

// the values after setting some inputs and letting the circuit settle
function after(circuit, values, inputs) {
    const start = Object.assign({}, values, inputs);
    const { rounds, stable } = settle(circuit, start);
    return { v: rounds[rounds.length - 1], stable, rounds: rounds.length };
}

const column = (table, i) => table.rows.map((r) => r.outputs[i]);

test('each gate has its truth table', () => {
    const want = { and: [0, 0, 0, 1], or: [0, 1, 1, 1], xor: [0, 1, 1, 0], nand: [1, 1, 1, 0], nor: [1, 0, 0, 0] };
    for (const [kind, outputs] of Object.entries(want)) {
        assert.deepStrictEqual(column(truthTable(CIRCUITS[`gate-${kind}`]()), 0), outputs, kind);
    }
    assert.deepStrictEqual(column(truthTable(CIRCUITS['gate-not']()), 0), [1, 0]);
});

test('NOT, AND, OR and XOR from NAND compute what the gates do', () => {
    assert.deepStrictEqual(column(truthTable(CIRCUITS['nand-not']()), 0), [1, 0]);
    assert.deepStrictEqual(column(truthTable(CIRCUITS['nand-and']()), 0), [0, 0, 0, 1]);
    assert.deepStrictEqual(column(truthTable(CIRCUITS['nand-or']()), 0), [0, 1, 1, 1]);
    assert.deepStrictEqual(column(truthTable(CIRCUITS['nand-xor']()), 0), [0, 1, 1, 0]);
});

test('De Morgan: both sides agree in every row', () => {
    const t = truthTable(CIRCUITS['de-morgan']());
    assert.deepStrictEqual(column(t, 0), [1, 0, 0, 0]);
    assert.deepStrictEqual(column(t, 1), column(t, 0));
});

test('the half adder and the full adder add', () => {
    const half = truthTable(CIRCUITS['half-adder']());
    assert.deepStrictEqual(half.rows.map((r) => r.outputs), [[0, 0], [1, 0], [1, 0], [0, 1]]);
    const full = truthTable(CIRCUITS['full-adder']());
    assert.deepStrictEqual(full.inputs, ['a', 'b', 'c_in']);
    for (const r of full.rows) {
        const sum = r.inputs.reduce((p, x) => p + x, 0);
        assert.deepStrictEqual(r.outputs, [sum % 2, sum >> 1], r.inputs.join(''));
    }
});

test('the 4-bit adder adds every pair, the carry running from right to left', () => {
    const c = CIRCUITS['adder-4']();
    for (let a = 0; a < 16; a++) {
        for (let b = 0; b < 16; b++) {
            const inputs = {};
            for (let i = 0; i < 4; i++) {
                inputs[`a${i}`] = (a >> i) & 1;
                inputs[`b${i}`] = (b >> i) & 1;
            }
            const { v, stable } = after(c, initial(c), inputs);
            const sum = [0, 1, 2, 3].reduce((n, i) => n + (v[`fa${i}:s`] << i), 0) + 16 * v['fa3:c_out'];
            assert.ok(stable);
            assert.strictEqual(sum, a + b, `${a} + ${b}`);
        }
    }
    // 0111 + 0001: the carry needs a round for each full adder
    const { rounds } = after(c, initial(c), { a0: 1, a1: 1, a2: 1, b0: 1 });
    assert.ok(rounds >= 5, `${rounds} rounds`);
    assert.match(c.readout(after(c, initial(c), { a3: 1, a2: 1, a1: 1, a0: 1, b0: 1 }).v), /<code>0000<\/code>, Übertrag <code>1<\/code>/);
});

test('the RS flip-flop sets, holds, resets, and does not come to rest when both are released at once', () => {
    const c = CIRCUITS['rs-latch']();
    let { v, stable } = after(c, initial(c), {});
    assert.ok(stable);
    assert.deepStrictEqual([v.q, v.qn], [0, 1]);
    ({ v } = after(c, v, { s: 1 }));
    assert.deepStrictEqual([v.q, v.qn], [1, 0]);
    ({ v } = after(c, v, { s: 0 }));
    assert.deepStrictEqual([v.q, v.qn], [1, 0], 'holds the 1');
    ({ v } = after(c, v, { r: 1 }));
    assert.deepStrictEqual([v.q, v.qn], [0, 1]);
    ({ v } = after(c, v, { r: 0 }));
    assert.deepStrictEqual([v.q, v.qn], [0, 1], 'holds the 0');
    ({ v } = after(c, v, { s: 1, r: 1 }));
    assert.deepStrictEqual([v.q, v.qn], [0, 0]);
    const both = after(c, v, { s: 0, r: 0 });
    assert.strictEqual(both.stable, false);
    assert.match(c.say(both.v, false), /kommt nicht zur Ruhe/);
    // one released after the other: the one still 1 wins
    ({ v } = after(c, after(c, v, { s: 1, r: 1 }).v, { r: 0 }));
    ({ v } = after(c, v, { s: 0 }));
    assert.deepStrictEqual([v.q, v.qn], [1, 0]);
});

test('the D flip-flop follows D while the clock is 1 and holds while it is 0', () => {
    const c = CIRCUITS['d-latch']();
    let { v } = after(c, initial(c), { e: 1, d: 1 });
    assert.strictEqual(v.q, 1);
    ({ v } = after(c, v, { e: 0 }));
    ({ v } = after(c, v, { d: 0 }));
    assert.strictEqual(v.q, 1, 'D changes, the clock is 0');
    ({ v } = after(c, v, { e: 1 }));
    assert.strictEqual(v.q, 0);
    assert.match(c.say(v, true), /Q folgt D/);
});

// The flags as the CPU Sandbox sets them (cpu.js, arith and logic): CF the
// carry out of the top bit for ADD, a borrow for SUB, 0 for AND, OR and
// XOR; ZF when the result is 0. Here with 4 bits instead of 8.
test('the ALU computes ADD, SUB, AND, OR and XOR with the flags of the CPU Sandbox', () => {
    const c = CIRCUITS.alu();
    const ops = ['ADD', 'SUB', 'AND', 'OR', 'XOR'];
    for (let op = 0; op < ops.length; op++) {
        for (let a = 0; a < 16; a++) {
            for (let b = 0; b < 16; b++) {
                const start = Object.assign(initial(c), { a, b });
                setChoice(c, start, 'op', op);
                const { rounds, stable } = settle(c, start);
                const v = rounds[rounds.length - 1];
                const exact = [a + b, a - b, a & b, a | b, a ^ b][op];
                const r = ((exact % 16) + 16) % 16;
                const cf = op === 0 ? +(exact > 15) : op === 1 ? +(exact < 0) : 0;
                assert.ok(stable);
                assert.deepStrictEqual([v['mux:r'], v.zf, v['mux:cf']], [r, +(r === 0), cf], `${ops[op]} ${a}, ${b}`);
            }
        }
    }
});

test('wires branch with a dot only where they branch', () => {
    const c = CIRCUITS['half-adder']();
    assert.deepStrictEqual(junctions(wires(c)).map((d) => d.p), [[105, 46], [125, 124]]);
    assert.deepStrictEqual(junctions(wires(CIRCUITS['gate-and']())), []);
    // every wire runs along the grid: no slanted piece
    for (const name of Object.keys(CIRCUITS)) {
        for (const w of wires(CIRCUITS[name]())) {
            for (let k = 0; k + 1 < w.points.length; k++) {
                const [p, q] = [w.points[k], w.points[k + 1]];
                assert.ok(p[0] === q[0] || p[1] === q[1], `${name}: ${w.src} from ${p} to ${q}`);
            }
        }
    }
});

// ----------------------------------------------------------------------
// Netlists of the Logik-Werkstatt (circuit_editor.js): read, laid out as
// text again, flattened and run. The same circuits the server's check runs
// (src/ruby/test/circuit_check_test.rb, fixtures/circuits.json).
const { Netlist } = require('../../src/static/include/logic_circuit.js');
const fixtures = require('../../src/ruby/test/fixtures/circuits.json');

test('netlists give the same truth tables as on the server', () => {
    for (const f of fixtures.tables) {
        const { circuit, errors } = Netlist.parse(f.text);
        assert.deepStrictEqual(errors, [], f.name);
        const flat = Netlist.flatten(circuit);
        if (f.unstable) {
            const start = initial(flat.circuit);
            assert.strictEqual(settle(flat.circuit, start).stable, false, f.name);
            continue;
        }
        assert.deepStrictEqual(truthTable(flat.circuit).rows.map((r) => r.outputs), f.rows, f.name);
    }
});

test('a netlist with memory keeps its state from step to step', () => {
    for (const f of fixtures.sequences) {
        const flat = Netlist.flatten(Netlist.parse(f.text).circuit);
        let v = initial(flat.circuit);
        for (const step of f.steps) {
            const r = settle(flat.circuit, Object.assign({}, v, step.set));
            assert.ok(r.stable, f.name);
            v = r.rounds[r.rounds.length - 1];
            const q = flat.circuit.nodes.find((n) => n.id === '>q');
            assert.strictEqual(valueAt(v, q.from), step.q, `${f.name}: ${JSON.stringify(step)}`);
        }
    }
});

test('a netlist that cannot be read says why, in the same words as the server', () => {
    for (const f of fixtures.errors) {
        const { errors } = Netlist.parse(f.text);
        assert.ok(errors.length && errors[0].message.includes(f.message), `${f.message}: ${JSON.stringify(errors)}`);
    }
});

test('format writes what parse reads, blocks first and only once', () => {
    const text = fixtures.tables[2].text;
    const { circuit } = Netlist.parse(text);
    const again = Netlist.format(circuit);
    assert.deepStrictEqual(Netlist.parse(again).circuit, circuit);
    assert.strictEqual(again.match(/^block HA$/gm).length, 1);
    assert.ok(again.indexOf('block HA') < again.indexOf('in a'));
    assert.deepStrictEqual([...Netlist.usedBlocks(circuit)], ['HA']);
});

test('flatten counts the gates, inside blocks too, and the blocks at the top', () => {
    const flat = Netlist.flatten(Netlist.parse(fixtures.tables[2].text).circuit);
    assert.deepStrictEqual(flat.gates, { xor: 2, and: 2, or: 1 });
    assert.strictEqual(flat.inner, 4);
    assert.strictEqual(flat.blocks, 2);
    assert.strictEqual(flat.resolve('h2.s'), 'h2/x');
    assert.strictEqual(flat.resolve(null), '#0');
    assert.ok(Netlist.hasFeedback(Netlist.flatten(Netlist.parse(fixtures.sequences[0].text).circuit)));
    assert.ok(!Netlist.hasFeedback(flat));
});
