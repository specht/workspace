// Colour spaces in 3D (src/static/include/color_solid.js): every space goes
// to the screen and back, its solid holds only colours of RGB, and the
// 8 × 8 × 8 cubes cover the solid.
const test = require('node:test');
const assert = require('node:assert');
const CS = require('../static/include/color_solid.js');

const near = (a, b, eps, what) => assert.ok(Math.abs(a - b) < eps, `${what}: ${a} is not close to ${b}`);
const linear = (v) => v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4;

test('every space goes there and back', () => {
    for (const [name, space] of Object.entries(CS.SPACES)) {
        for (const c of [[0.2, 0.5, 0.9], [1, 0, 0], [0.5, 0.5, 0.5], [0.9, 0.8, 0.1], [0, 1, 1]]) {
            const back = space.back(...space.forward(...c));
            // CIELab and OKLab come back as light, the others as stored values
            const expected = name === 'lab' || name === 'oklab' ? c.map(linear) : c;
            back.forEach((v, i) => near(v, expected[i], 1e-3, `${name} ${c}`));
        }
    }
});

test('brighter is up, and grey is the axis', () => {
    for (const [name, space] of Object.entries(CS.SPACES)) {
        const black = space.forward(0, 0, 0), white = space.forward(1, 1, 1), grey = space.forward(0.5, 0.5, 0.5);
        assert.ok(white[1] > grey[1] && grey[1] > black[1], name);
        for (const p of [black, white, grey]) { near(p[0], 0, 1e-3, `${name} x`); near(p[2], 0, 1e-3, `${name} z`); }
    }
});

test('a section and the walls lie inside the colours of RGB', () => {
    for (const name of Object.keys(CS.SPACES)) {
        const mesh = CS.solid(name, { top: 0, cut: true });
        assert.ok(mesh.triangles > 1000, name);
        for (let i = 0; i < mesh.pos.length; i += 3 * 97) {
            const [x, y, z] = mesh.pos.subarray(i, i + 3);
            assert.ok(y < 1e-6, `${name}: ${y} above the cut`);
            assert.ok(CS.inside(CS.SPACES[name], x, y, z) > -0.01, `${name}: ${x} ${y} ${z} outside`);
            assert.ok(!(x > 1e-6 && z > 1e-6), `${name}: ${x} ${z} in the missing quarter`);
        }
    }
});

test('the 8 × 8 × 8 cubes, all or without a quarter', () => {
    assert.strictEqual(CS.cubes('lab').length, 512);
    const all = CS.solid('rgb', { split: true }).triangles;
    assert.strictEqual(all, 512 * 6 * 16 * 2);
    // the cube on its black corner: no centre lies exactly on x = 0 and z = 0 but the grey ones
    const cut = CS.solid('rgb', { split: true, cut: true }).triangles;
    assert.ok(cut < all * 0.8 && cut > all * 0.7, `${cut} of ${all}`);
});

test('what is in front follows the pointer, however the solid lies', () => {
    // where the turn puts a point of the space on the screen
    const apply = (rot, p) => [0, 1, 2].map((r) => rot[r] * p[0] + rot[r + 3] * p[1] + rot[r + 6] * p[2]);
    // also with the top tilted all the way towards the viewer
    for (const rot of [CS.startTurn(-Math.PI / 4, 0.45), CS.startTurn(0.7, Math.PI / 2), CS.screenTurn(CS.startTurn(0, 0), 2, 1)]) {
        // the point of the space that is in front: the row for z
        const front = [rot[2], rot[5], rot[8]];
        const right = apply(CS.screenTurn(rot, 0.1, 0), front);
        [Math.sin(0.1), 0, Math.cos(0.1)].forEach((v, i) => near(right[i], v, 1e-9, 'right'));
        const down = apply(CS.screenTurn(rot, 0, 0.1), front);
        [0, -Math.sin(0.1), Math.cos(0.1)].forEach((v, i) => near(down[i], v, 1e-9, 'down'));
    }
    // and it stays a turn: axes of length 1, at right angles
    let rot = CS.startTurn(0.3, 0.2);
    for (let i = 0; i < 5000; i++) rot = CS.screenTurn(rot, 0.013, -0.007);
    const col = (k) => rot.slice(3 * k, 3 * k + 3);
    const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    for (let k = 0; k < 3; k++) near(dot(col(k), col(k)), 1, 1e-9, 'length');
    near(dot(col(0), col(1)), 0, 1e-9, 'angle');
});
