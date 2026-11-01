const test = require('node:test');
const assert = require('node:assert');
const { grade } = require('./noten');

test('grade at the top', () => {
  assert.strictEqual(grade(15), '1+');
});
