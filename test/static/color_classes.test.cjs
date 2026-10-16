// Colour classes in 3D (src/static/include/color_classes.js): the colour
// maths agrees with the activity, the two divisions are equally fine, and
// the mesh draws every class as a closed solid.
const test = require('node:test');
const assert = require('node:assert');
const CC = require('../../src/static/include/color_classes.js');

const near = (a, b, eps) => assert.ok(Math.abs(a - b) < eps, `${a} is not close to ${b}`);

test('lab is the formula from the activity, and fromLab goes back', () => {
    const [L, a, b] = CC.lab(0xffffff);
    near(L, 100, 0.01); near(a, 0, 0.05); near(b, 0, 0.05);
    for (const c of [0x000000, 0x44a8e0, 0xff0000, 0x00ff00, 0x0000ff, 0x808080, 0x123456]) {
        assert.strictEqual(CC.fromLab(...CC.lab(c)), c, c.toString(16));
    }
    assert.strictEqual(CC.fromLab(50, 100, 100), -1);  // no screen shows that
});

test('the cubes of a division cover 0 to 255 without gaps', () => {
    for (const k of [2, 3, 6, 7]) {
        let next = 0;
        for (let i = 0; i < k; i++) {
            const [low, high] = CC.boxRange(i, k);
            assert.strictEqual(low, next);
            for (const v of [low, high]) assert.strictEqual(CC.boxCell(v, k) % k, i);
            next = high + 1;
        }
        assert.strictEqual(next, 256);
    }
});

test('a colour lies in a Lab box as big as its cube in RGB, on average', () => {
    for (const k of [4, 6, 8]) {
        const { n, size } = CC.labLevels(k);
        near(size / (16777216 / k ** 3), 1, 0.15);
        assert.ok(n[0] > n[1] && n[1] >= n[2], `${n}: the lightness gets the most steps`);
    }
});

test('with equally big classes, the colours of a Lab box are closer together', () => {
    const k = 6, { n } = CC.labLevels(k);
    let rgb = 0, labs = 0, rgbLight = 0, labLight = 0;
    for (const color of [0x44a8e0, 0x808080, 0xc04020, 0x30a050, 0x202040, 0xf0e0a0]) {
        const r = CC.meanDistance(CC.boxSwatch(color, k, 200).colors), l = CC.meanDistance(CC.labSwatch(color, n, 200).colors);
        rgb += r[0]; labs += l[0]; rgbLight += r[1]; labLight += l[1];
    }
    assert.ok(labs < rgb * 0.85, `${labs} against ${rgb}`);
    assert.ok(labLight < rgbLight * 0.7, `${labLight} against ${rgbLight}`);
});

test('every colour of a swatch belongs to the class', () => {
    const k = 6, { n } = CC.labLevels(k), color = 0x44a8e0;
    const box = CC.boxSwatch(color, k, 300);
    assert.ok(box.colors.every((c) => CC.boxCell(c, k) === CC.boxCell(color, k)));
    const cell = CC.labCell(...CC.lab(color), n);
    assert.ok(CC.labSwatch(color, n, 300).colors.every((c) => CC.labCell(...CC.lab(c), n) === cell));
});

test('the mesh draws the outside of every class', () => {
    // 2 × 2 × 2 cubes of 18 × 18 × 18 voxels: 8 × 6 × 18² faces
    const set = CC.rgbVoxels(2);
    const cells = Int32Array.from({ length: set.count }, (_, v) => CC.boxCell(set.color[v], 2));
    assert.strictEqual(CC.buildMesh(set, cells).faces, 8 * 6 * 18 * 18);
    // with a quarter out, two of the cubes are gone
    assert.strictEqual(CC.buildMesh(set, cells, { cut: true }).faces, 6 * 6 * 18 * 18);
    // unless one of them is the chosen class
    const kept = CC.boxCell(0xffff00, 2);
    assert.strictEqual(CC.buildMesh(set, cells, { cut: true, keep: kept }).faces, 7 * 6 * 18 * 18);
    // the whole cube, for the glass
    assert.strictEqual(CC.buildShell(set).faces, 6 * 36 * 36);
});

test('the Lab view holds only colours a screen can show', () => {
    const set = CC.labVoxels(CC.labLevels(4).n);
    assert.ok(set.count > 1000);
    for (let v = 0; v < set.count; v += 97) assert.ok(set.color[v] >= 0 && set.color[v] <= 0xffffff);
});
