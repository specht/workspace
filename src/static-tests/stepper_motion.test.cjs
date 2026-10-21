// The interpolation of stepper_motion.js: attributes that differ only in
// their numbers move, everything else changes at once.
//   node --test src/static-tests/*.test.cjs
const test = require('node:test');
const assert = require('node:assert');
const { mix } = require('../static/include/stepper_motion.js');

test('numbers move from the old to the new value', () => {
    assert.strictEqual(mix('10', '20', 0.5), '15');
    assert.strictEqual(mix('-4', '4', 0.25), '-2');
    assert.strictEqual(mix('0 0 300 100', '0 0 500 180', 0.5), '0 0 400 140');
    assert.strictEqual(mix('M10 20 L30 40', 'M20 20 L50 0', 0.5), 'M15 20 L40 20');
    assert.strictEqual(mix('0.35', '1', 1), '1');
});

test('the end values are exact', () => {
    assert.strictEqual(mix('3.333', '7.777', 1), '7.78');
    assert.strictEqual(mix('12', '12', 0.3), '12');
});

test('what does not differ only in numbers does not move', () => {
    assert.strictEqual(mix('var(--algo-accent)', 'var(--algo-done)', 0.5), null);
    assert.strictEqual(mix('M0 0 L1 1', 'M0 0 L1 1 L2 2', 0.5), null);
    assert.strictEqual(mix('none', '5 4', 0.5), null);
});
