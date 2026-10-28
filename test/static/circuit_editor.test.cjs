// The geometry of the circuit editor (src/static/include/circuit_editor.js):
// where the drawing begins and ends, and wires that never lie on top of
// each other. The editor itself has e2e/tests/logic-workshop.browser.spec.ts.
const test = require('node:test');
const assert = require('node:assert');
const { Netlist } = require('../../src/static/include/logic_circuit.js');
const { geom, route, wiresOf, labelLeft, viewBoxOf, shared, pieces } = require('../../src/static/include/circuit_editor.js');

const HA = 'block HA\n  in a\n  in b\n  xor x a b\n  and k a b\n  out s x\n  out c k\nend\n';

// a full adder as the editor places it by clicks: the half adders one above
// the other, the OR right of the upper one, the switches close to the edge
const FULL = `${HA}in a @48,40
in b @48,104
in c_in @48,168
HA ha1 a b @136,24
HA ha2 ha1.s c_in @136,120
or g1 ha1.c ha2.c @232,24
out s ha2.s @560,40
out c_out g1 @560,104
`;

// the picture on the card (tutorial-screenshot-hooks.mjs)
const TEASER = `${HA}in a @64,56
in b @64,104
in c_in @64,160
HA h1 a b @136,24
HA h2 h1.s c_in @232,104
or o h1.c h2.c @408,152
out s h2.s @536,136
out c_out o @536,176
`;

const parse = (text) => {
    const { circuit, errors } = Netlist.parse(text);
    assert.deepStrictEqual(errors, []);
    return circuit;
};

// pieces of two wires from different sources that lie on top of each other
function overlaps(wires) {
    const found = [];
    wires.forEach((w, i) => wires.slice(i + 1).forEach((v) => {
        if (w.src === v.src) return;
        for (const p of pieces(w.points)) for (const q of pieces(v.points)) if (shared(p, q)) found.push([w.src, v.src, p, q]);
    }));
    return found;
}

test('no two wires from different sources share a piece', () => {
    for (const text of [FULL, TEASER]) {
        const circuit = parse(text);
        assert.deepStrictEqual(overlaps(wiresOf(circuit)), []);
    }
    // without lanes the full adder has wires on top of each other: the test means something
    const circuit = parse(FULL);
    const plain = wiresOf(circuit).map((w) => Object.assign({}, w, { points: route(w.a, w.b, w.boxA, w.boxB) }));
    assert.ok(overlaps(plain).length > 0);
});

test('every wire still starts and ends at its pins, at right angles', () => {
    for (const w of wiresOf(parse(FULL))) {
        assert.deepStrictEqual(w.points[0], w.a);
        assert.deepStrictEqual(w.points[w.points.length - 1], w.b);
        for (const [p, q] of pieces(w.points)) assert.ok(p[0] === q[0] || p[1] === q[1], `${w.src}: ${p} ${q}`);
    }
});

test('the names of switches and lamps lie inside the drawing', () => {
    for (const text of [FULL, TEASER, 'in carry_in @8,40\nout carry_out carry_in @200,40\n']) {
        const circuit = parse(text);
        const [left, top, width, height] = viewBoxOf(circuit);
        for (const p of circuit.parts) {
            const [x, y, w, h] = geom(circuit, p).box;
            assert.ok(labelLeft(p, x) >= left + 4, `${p.id} begins at ${labelLeft(p, x)}, the drawing at ${left}`);
            if (p.kind === 'out') assert.ok(x + w + 7 + 9 * p.id.length <= left + width - 4, `${p.id} ends right of the drawing`);
            assert.ok(y >= top && y + h <= top + height);
        }
    }
});
