const test = require('node:test');
const assert = require('node:assert/strict');
const lib = require('../lib.js');

test('files next to the model', () => {
    assert.equal(lib.layoutPath('/workspace/filmdatenbank/film.er'), '/workspace/filmdatenbank/.film.er.layout.json');
    assert.equal(lib.sqlPath('/workspace/filmdatenbank/film.er'), '/workspace/filmdatenbank/film.sql');
    assert.equal(lib.svgPath('/workspace/filmdatenbank/film.er', 'chen'), '/workspace/filmdatenbank/film-chen.svg');
});

test('only generated SQL is overwritten without asking', () => {
    assert.ok(lib.isGenerated('-- Erzeugt aus film.er mit der ER-Werkstatt 0.1.0.\n'));
    assert.ok(lib.isGenerated('-- Erzeugt mit der ER-Werkstatt 0.1.0.\n'));
    assert.ok(!lib.isGenerated('CREATE TABLE movie (id INT);'));
    assert.ok(!lib.isGenerated(''));
});

test('a broken layout file means no positions', () => {
    assert.deepEqual(lib.readLayout('nonsense'), { version: 1, chen: {}, crowfoot: {}, tables: {} });
    const layout = lib.readLayout(JSON.stringify({ chen: { 'E:movie': { x: 1, y: 2 }, bad: { x: 'a' } }, tables: 3 }));
    assert.deepEqual(layout.chen, { 'E:movie': { x: 1, y: 2 } });
    assert.deepEqual(layout.tables, {});
    assert.deepEqual(lib.readLayout(lib.writeLayout(layout)), layout);
});
