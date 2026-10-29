// The engine of »Wie Prolog sucht« (src/static/include/prolog_search.js)
// against what SWI-Prolog answers for the same program.
//   node --test test/static/*.test.cjs
const test = require('node:test');
const assert = require('node:assert');
const PS = require('../../src/static/include/prolog_search.js');

const FAMILY = `parent(christoph, johann_ambrosius).
parent(johann_ambrosius, johann_sebastian).
parent(maria_elisabeth, johann_sebastian).
parent(johann_michael, maria_barbara).
parent(johann_sebastian, wilhelm_friedemann).
parent(maria_barbara, wilhelm_friedemann).
grandparent(G, C) :- parent(G, P), parent(P, C).
sibling(X, Y) :- parent(P, X), parent(P, Y), X \\= Y.`;

const run = (query) => PS.trace(PS.parseProgram(FAMILY), query);

test('the grandparents come in the order of the facts, as in SWI-Prolog', () => {
    const steps = run('grandparent(G, wilhelm_friedemann)');
    assert.deepStrictEqual(steps.at(-1).answers, ['G = johann_ambrosius', 'G = maria_elisabeth', 'G = johann_michael']);
    assert.strictEqual(steps[0].kind, 'start');
    assert.strictEqual(steps.at(-1).kind, 'done');
});

test('a rule is marked at its line and replaced by its goals, with fresh variables', () => {
    const steps = run('grandparent(G, wilhelm_friedemann)');
    assert.strictEqual(steps[1].kind, 'fits');
    assert.strictEqual(steps[1].clause, 6);
    assert.deepStrictEqual(steps[1].goals, ['parent(G₁, P₁)', 'parent(P₁, wilhelm_friedemann)']);
    assert.match(steps[1].say, /Regel in Zeile 7/);
});

test('a goal that fits more than one clause leaves a choice point to come back to', () => {
    const steps = run('grandparent(G, wilhelm_friedemann)');
    assert.ok(steps[2].choices.length > 0);
    assert.ok(steps.some((s) => s.kind === 'fail'));
    assert.ok(steps.some((s) => s.kind === 'back'));
});

test('\\= leaves out the person themselves', () => {
    assert.deepStrictEqual(run('sibling(wilhelm_friedemann, Y)').at(-1).answers, []);
    assert.match(run('sibling(wilhelm_friedemann, Y)').at(-1).say, /false/);
});

test('a query without variables answers true or false', () => {
    assert.deepStrictEqual(run('grandparent(christoph, johann_sebastian)').at(-1).answers, ['true']);
    assert.deepStrictEqual(run('grandparent(christoph, wilhelm_friedemann)').at(-1).answers, []);
});
