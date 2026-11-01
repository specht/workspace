const test = require('node:test');
const assert = require('node:assert');
const { grade, abiturGrade, isAdmitted } = require('./noten');

const courses = (points, count) => Array(count).fill(points);

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

test('isAdmitted', () => {
  assert.ok(isAdmitted(courses(10, 20)));                                // genau 200 Punkte
  assert.ok(!isAdmitted([...courses(10, 19), 9]));                      // 199 Punkte
  assert.ok(isAdmitted([...courses(4, 7), ...courses(12, 15)]));        // 7 Defizite
  assert.ok(!isAdmitted([...courses(4, 8), ...courses(12, 15)]));       // 8 Defizite
  assert.ok(isAdmitted([...courses(5, 8), ...courses(12, 15)]));        // 5 Punkte sind kein Defizit
  assert.ok(!isAdmitted([0, ...courses(15, 19)]));                      // ein Kurs mit 0 Punkten
});
