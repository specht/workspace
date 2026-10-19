// Checks the grammar without VS Code: every pattern compiles, and sample
// lines go where they belong (a floor plan row or a comment, the cells of a
// row). The patterns are Oniguruma; those used here mean the same in
// JavaScript.
'use strict';

const test = require('node:test');
const assert = require('node:assert');
const grammar = require('../syntaxes/room.tmLanguage.json');

const r = grammar.repository;
const js = (source) => new RegExp(source.replace(/\(\?!\.\*/g, '(?![^\\n]*').replace(/\(\?=\.\*/g, '(?=[^\\n]*'));

test('every pattern compiles', () => {
    const walk = (node) => {
        for (const key of ['match', 'begin', 'end']) if (node[key]) js(node[key]);
        for (const child of node.patterns ?? []) walk(child);
    };
    for (const node of Object.values(r)) walk(node);
});

test('rows of a floor plan are told from comments', () => {
    const row = js(r.row.begin), comment = js(r.comment.match);
    for (const line of ['#####', '5...6', '#.@.L.1', '1...F...L...#', '#.....S...S.#', '   #######', '#..S..M..S..#']) {
        assert.ok(row.test(line), `row: ${line}`);
    }
    for (const line of ['# Ein Saal hinter der Kammer.', '# höher als die Decke', '; Notiz']) {
        assert.ok(!row.test(line), `not a row: ${line}`);
        assert.ok(comment.test(line), `comment: ${line}`);
    }
    for (const line of ['height = 3, 7', 'name = Saal', '[map]']) assert.ok(!row.test(line), `not a row: ${line}`);
});

test('cells', () => {
    const scope = (cell) => r.cells.patterns.find((p) => new RegExp(`^(?:${p.match})$`).test(cell))?.name;
    assert.match(scope('###'), /wall/);
    assert.match(scope('...'), /floor/);
    assert.match(scope('@'), /start/);
    assert.match(scope('5'), /door/);
    assert.match(scope('L'), /thing/);
});

test('an entry: colour, numbers, words, a comment at the end', () => {
    const entry = js(r.entry.begin);
    assert.ok(entry.test('wallpaper = #9a8a4c'));
    // the spaces after = belong to the start: a colour right after it is no comment
    assert.strictEqual('light = #fff3cf'.replace(entry, ''), '#fff3cf');
    const [comment, colour, number] = r.entry.patterns.map((p) => js(p.match));
    assert.ok(colour.test('#9a8a4c'));
    assert.ok(comment.test('0.7, 7, 0.7      # höher als die Decke'));
    assert.ok(!comment.test('#9a8a4c'), 'a colour is not a comment');
    assert.ok(!comment.test('0.08, #5a4626'), 'a colour after a comma is not a comment either');
    assert.ok(number.test('2.5'));
});
