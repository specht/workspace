// The iDCT stepper (src/static/include/idct_stepper.js): after the last
// coefficient, the block is what the reference decoder (src/content/jpeg/
// loesung/jpeg.c) computes for the first Y block of 44-baseline.jpg.
const test = require('node:test');
const assert = require('node:assert');
const IS = require('../../src/static/include/idct_stepper.js');

const COEF = '-600 27 9 0 7 0 0 0 -88 -12 4 6 0 0 0 0 24 -20 -10 0 0 0 0 0 8 5 -14 -9 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0'.split(' ').map(Number);
const DECODER = '40 41 43 46 46 43 41 42 45 40 37 39 41 42 43 44 53 45 36 35 37 40 41 43 62 54 46 42 40 38 39 41 66 63 59 55 49 43 42 45 67 68 69 67 60 53 52 55 71 71 71 71 68 63 63 65 77 73 71 71 70 68 68 69'.split(' ').map(Number);

test('one step per coefficient that is not zero, in zig-zag order', () => {
    const steps = IS.steps(COEF);
    assert.strictEqual(steps.length, COEF.filter((c) => c !== 0).length);
    assert.deepStrictEqual(steps.slice(0, 3).map((s) => [s.u, s.v, s.value]), [[0, 0, -600], [1, 0, 27], [0, 1, -88]]);
});

test('the DC coefficient alone gives an even block', () => {
    const first = IS.steps(COEF)[0];
    assert.ok(first.block.every((p) => p === 53));
});

test('after the last step the block is the decoder\'s', () => {
    const steps = IS.steps(COEF);
    assert.deepStrictEqual(steps[steps.length - 1].block, DECODER);
});
