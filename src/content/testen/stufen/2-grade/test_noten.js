const test = require('node:test');
const assert = require('node:assert');
const { grade } = require('./noten');

// die Enden: 0 und 15 Punkte
test('grade at the ends', () => {
  assert.strictEqual(grade(0), '6');
  assert.strictEqual(grade(15), '1+');
});

test('grade in between', () => {
  assert.strictEqual(grade(13), '1-');
  assert.strictEqual(grade(5), '4');
});

// gleich neben den Enden: Punkte, die es nicht gibt
test('grade outside', () => {
  assert.throws(() => grade(-1), RangeError);
  assert.throws(() => grade(16), RangeError);
});
