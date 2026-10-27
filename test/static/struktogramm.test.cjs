// Struktogramme from pseudocode (src/static/include/struktogramm.js).
const test = require('node:test');
const assert = require('node:assert');
const S = require('../../src/static/include/struktogramm.js');

const EAN = `check_digit(digits):
    sum ← 0
    for i from 0 to 11:
        if i mod 2 == 0:
            sum ← sum + digits[i]
        else:
            sum ← sum + 3 * digits[i]
    return (10 - sum mod 10) mod 10`;

test('a function with a loop and a decision', () => {
    const { diagrams, errors } = S.parse(EAN);
    assert.deepStrictEqual(errors, []);
    assert.strictEqual(diagrams.length, 1);
    assert.strictEqual(diagrams[0].title, 'check_digit(digits)');
    const [init, loop, ret] = diagrams[0].body;
    assert.strictEqual(init.text, 'sum ← 0');
    assert.strictEqual(loop.type, 'loop');
    assert.strictEqual(loop.head, 'for i from 0 to 11');
    const decision = loop.body[0];
    assert.strictEqual(decision.type, 'if');
    assert.strictEqual(decision.cond, 'i mod 2 == 0');
    assert.strictEqual(decision.then[0].text, 'sum ← sum + digits[i]');
    assert.strictEqual(decision.else[0].text, 'sum ← sum + 3 * digits[i]');
    assert.strictEqual(ret.text, 'return (10 - sum mod 10) mod 10');
});

test('else if becomes a decision in the right column; one statement may follow the colon', () => {
    const { diagrams, errors } = S.parse(`if x < 0: sign ← -1
else if x == 0:
    sign ← 0
else:
    sign ← 1`);
    assert.deepStrictEqual(errors, []);
    const outer = diagrams[0].body[0];
    assert.strictEqual(outer.then[0].text, 'sign ← -1');
    assert.strictEqual(outer.else[0].type, 'if');
    assert.strictEqual(outer.else[0].cond, 'x == 0');
    assert.strictEqual(outer.else[0].else[0].text, 'sign ← 1');
});

test('repeat … until, while, and a colon inside brackets', () => {
    const { diagrams, errors } = S.parse(`repeat:
    x ← list[1:3]
until x == 0
while n > 0:
    n ← n div 10`);
    assert.deepStrictEqual(errors, []);
    const [rep, loop] = diagrams[0].body;
    assert.strictEqual(rep.type, 'repeat');
    assert.strictEqual(rep.cond, 'x == 0');
    assert.strictEqual(rep.body[0].text, 'x ← list[1:3]');
    assert.strictEqual(loop.head, 'while n > 0');
});

test('mistakes are named with their line', () => {
    const cases = {
        'if x > 0\n    y ← 1': /Doppelpunkt/,
        'if x > 0:\ny ← 1': /Nach dem Doppelpunkt fehlt/,
        'x ← 1\n    y ← 2': /tiefer eingerückt/,
        'else:\n    y ← 1': /gehört zu einem »if«/,
        'repeat:\n    x ← x - 1': /until/,
        'until x == 0': /beendet eine Schleife/,
        'while :\n    x ← 1': /wie oft/,
    };
    for (const [text, pattern] of Object.entries(cases)) {
        const { errors } = S.parse(text);
        assert.ok(errors.length > 0, text);
        assert.match(errors[0].msg, pattern, text);
        assert.ok(errors[0].line >= 1, text);
    }
});

test('the drawing: boxes, the triangle, both columns end together', () => {
    const result = S.svg(EAN);
    assert.deepStrictEqual(result.errors, []);
    assert.match(result.svg, /^<svg [^>]*role="img"/);
    assert.match(result.svg, /<desc>check_digit\(digits\):/);
    assert.match(result.svg, />wahr</);
    assert.match(result.svg, />falsch</);
    assert.match(result.svg, /<tspan class="sg-kw">for<\/tspan>/);
    // the two columns of the decision: their boxes end at the same height
    const rects = [...result.svg.matchAll(/<rect class="sg-box" x="([\d.]+)" y="([\d.]+)" width="([\d.]+)" height="([\d.]+)"/g)]
        .map((m) => m.slice(1).map(Number));
    const ends = rects.filter(([x, , w]) => w < result.width - 30).map(([, y, , h]) => +(y + h).toFixed(1));
    assert.ok(ends.length >= 2);
    assert.strictEqual(new Set(ends).size, 1);
    // text is escaped
    assert.match(S.svg('x ← a < b').svg, /a &lt; b/);
});

test('an empty column, and errors give no picture', () => {
    const result = S.svg('if x > 0:\n    y ← 1');
    assert.ok(result.svg);
    assert.strictEqual(S.svg('if x > 0\n    y ← 1').svg, null);
    assert.strictEqual(S.svg('   \n').svg, null);
});
