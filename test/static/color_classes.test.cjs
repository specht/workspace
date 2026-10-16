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

// The area of all triangles of some classes in one view
function area(classes, space) {
    const mesh = CC.buildMesh(classes, space);
    let sum = 0;
    for (let i = 0; i < mesh.pos.length; i += 9) {
        const p = mesh.pos.subarray(i, i + 3), q = mesh.pos.subarray(i + 3, i + 6), r = mesh.pos.subarray(i + 6, i + 9);
        const u = [q[0] - p[0], q[1] - p[1], q[2] - p[2]], w = [r[0] - p[0], r[1] - p[1], r[2] - p[2]];
        sum += Math.hypot(u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2], u[0] * w[1] - u[1] * w[0]) / 2;
    }
    return sum;
}

test('the cubes of RGB are closed cubes', () => {
    const cubes = CC.cubeClasses(2);
    assert.strictEqual(cubes.size, 8);
    near(area(cubes, 'rgb'), 8 * 6 * 0.25, 1e-6);
    near(area(CC.shell(), 'rgb'), 6, 1e-6);
    // with a quarter out, two of the cubes are gone, unless one is the chosen class
    const triangles = CC.buildMesh(cubes, 'rgb').triangles;
    assert.strictEqual(CC.buildMesh(cubes, 'rgb', { cut: true }).triangles, triangles * 6 / 8);
    assert.strictEqual(CC.buildMesh(cubes, 'rgb', { cut: true, keep: CC.boxCell(0xffff00, 2) }).triangles, triangles * 7 / 8);
});

test('a box of Lab is its box, cut to the colours of RGB', () => {
    const { n } = CC.labLevels(4);
    const boxes = CC.boxClasses(n);
    for (const [cell, t] of boxes) {
        const [[l0, l1], [a0, a1], [b0, b1]] = CC.labRange(cell, n);
        for (let i = 0; i < t.length; i += 6) {
            const [L, a, b] = [t[i], t[i + 1], t[i + 2]];
            assert.ok(L > l0 - 1e-6 && L < l1 + 1e-6 && a > a0 - 1e-6 && a < a1 + 1e-6 && b > b0 - 1e-6 && b < b1 + 1e-6, `${cell}: ${L} ${a} ${b}`);
            assert.ok(CC.inside(L, a, b) > -0.02, `${cell}: ${L} ${a} ${b} lies outside`);
        }
    }
    // together the outside of all boxes covers at least the outside of the whole solid
    assert.ok(area(boxes, 'lab') > area(CC.shell(), 'lab'));
});
