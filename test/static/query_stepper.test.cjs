// The engine of the query stepper (src/static/include/query_stepper.js)
// against what MySQL answers for the same queries on terra1.
//   node --test test/static/*.test.cjs
const test = require('node:test');
const assert = require('node:assert');
const path = require('node:path');
const fs = require('node:fs');
const QS = require('../../src/static/include/query_stepper.js');

const data = JSON.parse(fs.readFileSync(path.join(__dirname, '../../src/static/query-stepper/terra1.json')));
const result = (sql) => QS.run(sql, data).result;
const clauses = (sql) => QS.run(sql, data).steps.map((s) => s.clause);
const fails = (sql, pattern) => assert.throws(() => QS.run(sql, data), (e) => e instanceof QS.SqlError && pattern.test(e.message));

test('the queries of the SQL introduction', () => {
    assert.strictEqual(result('SELECT * FROM land;').rows.length, 197);
    assert.deepStrictEqual(result("SELECT name, hauptstadt FROM land WHERE name = 'Deutschland';").rows[0].cells, ['Deutschland', 'Berlin']);
    // Text compares without regard to case, as latin1_german1_ci does
    assert.strictEqual(result("SELECT * FROM land WHERE name = 'deutschland'").rows.length, 1);
    assert.deepStrictEqual(result('SELECT COUNT(*) FROM land;').rows[0].cells, ['197']);
    assert.deepStrictEqual(result('SELECT * FROM land LIMIT 5;').rows.length, 5);
    const sorted = result('SELECT name, einwohner FROM land ORDER BY einwohner DESC LIMIT 2').rows.map((r) => r.cells[0]);
    assert.deepStrictEqual(sorted, ['China', 'Indien']);
    const like = result("SELECT name FROM land WHERE name LIKE '%land%' ORDER BY name").rows.map((r) => r.cells[0]);
    assert.ok(like.includes('Deutschland') && like.every((n) => /land/i.test(n)));
    const continents = result('SELECT DISTINCT kontinent FROM land ORDER BY kontinent').rows.map((r) => r.cells[0]);
    assert.deepStrictEqual(continents, [...new Set(continents)]);
    assert.ok(continents.length >= 6 && continents.length <= 8);
});

test('the steps come in the order the database works', () => {
    assert.deepStrictEqual(
        clauses('SELECT DISTINCT kontinent, COUNT(*) AS n FROM land WHERE einwohner > 1 GROUP BY kontinent HAVING COUNT(*) > 5 ORDER BY n DESC LIMIT 3'),
        ['FROM', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'DISTINCT', 'ORDER BY', 'LIMIT']);
    assert.deepStrictEqual(clauses('SELECT o.name FROM ort o JOIN land l ON o.land = l.name'), ['FROM', 'JOIN', 'SELECT']);
});

test('groups, HAVING and aliases', () => {
    const rows = result('SELECT kontinent, COUNT(*) AS laender FROM land GROUP BY kontinent HAVING laender > 40 ORDER BY laender DESC').rows;
    assert.ok(rows.length >= 2 && rows.every((r) => Number(r.cells[1]) > 40));
    assert.ok(Number(rows[0].cells[1]) >= Number(rows[1].cells[1]));
    const total = result('SELECT kontinent, COUNT(*) FROM land GROUP BY kontinent').rows.reduce((s, r) => s + Number(r.cells[1]), 0);
    assert.strictEqual(total, 197);
});

test('JOIN and LEFT JOIN', () => {
    const europe = result("SELECT o.name, l.name FROM ort o JOIN land l ON o.land = l.name WHERE l.kontinent = 'Europa' ORDER BY o.einwohner DESC LIMIT 3").rows;
    assert.strictEqual(europe.length, 3);
    assert.ok(europe.every((r) => r.cells.length === 2));
    const withNone = result('SELECT l.name, o.name FROM land l LEFT JOIN ort o ON o.land = l.name WHERE o.name IS NULL').rows;
    assert.ok(withNone.length > 0 && withNone.every((r) => r.cells[1] === 'NULL'));
});

test('NULL is neither true nor false', () => {
    assert.strictEqual(result('SELECT * FROM land WHERE NULL = NULL').rows.length, 0);
    assert.strictEqual(result('SELECT * FROM land WHERE NOT (NULL = 1)').rows.length, 0);
});

test('mistakes MySQL also rejects, with an explanation', () => {
    fails('SELECT einwohner / flaeche AS dichte FROM land WHERE dichte > 0.0001', /SELECT kommt nach WHERE/);
    fails('SELECT name FROM land WHERE COUNT(*) > 3', /HAVING/);
    fails('SELECT kontinent, name FROM land GROUP BY kontinent', /weder in GROUP BY/);
    fails('SELECT name FROM ort o JOIN land l ON o.land = l.name', /mehrdeutig/);
    fails('SELECT * FROM laender', /gibt es nicht/);
    fails('SELECT * FROM land ORDER BY name WHERE kontinent = \'Asien\'', /falschen Stelle/);
    fails("SELECT * FROM land WHERE name = 'Deutschland", /hört nicht auf/);
});
