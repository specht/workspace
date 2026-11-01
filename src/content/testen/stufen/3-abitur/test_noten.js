const test = require('node:test');
const assert = require('node:assert');
const { grade, abiturGrade } = require('./noten');

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

// an den Grenzen der Tabelle der KMK
test('abiturGrade at the boundaries', () => {
  assert.strictEqual(abiturGrade(900), 1.0);
  assert.strictEqual(abiturGrade(823), 1.0);
  assert.strictEqual(abiturGrade(822), 1.1);
  assert.strictEqual(abiturGrade(805), 1.1);
  assert.strictEqual(abiturGrade(804), 1.2);
  assert.strictEqual(abiturGrade(300), 4.0);
});

test('abiturGrade outside', () => {
  assert.throws(() => abiturGrade(299), RangeError);
  assert.throws(() => abiturGrade(901), RangeError);
});
