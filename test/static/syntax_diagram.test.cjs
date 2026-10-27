// Syntax diagrams and syntax trees from EBNF (src/static/include/syntax_diagram.js).
const test = require('node:test');
const assert = require('node:assert');
const SD = require('../../src/static/include/syntax_diagram.js');

const EXPRESSIONS = `expression = term { "+" term }
term       = factor { "*" factor }
factor     = number | "(" expression ")"`;

// the boxes of a picture: x, y, width, height, and whether a box is rounded
const boxes = (svg) => [...svg.matchAll(/<rect class="sd-(terminal|nonterminal)" x="([\d.-]+)" y="([\d.-]+)" width="([\d.]+)" height="([\d.]+)" rx="([\d.]+)"/g)]
    .map((m) => ({ kind: m[1], x: +m[2], y: +m[3], w: +m[4], h: +m[5], rx: +m[6] }));
const overlap = (a, b) => a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h;
const noOverlaps = (list) => {
    for (let i = 0; i < list.length; i++) for (let j = i + 1; j < list.length; j++) assert.ok(!overlap(list[i], list[j]), `${JSON.stringify(list[i])} ${JSON.stringify(list[j])}`);
};

test('the grammar of expressions: three rules, a { s a } as Wirth drew it', () => {
    const { rules, errors } = SD.parse(EXPRESSIONS);
    assert.deepStrictEqual(errors, []);
    assert.deepStrictEqual(rules.map((r) => r.name), ['expression', 'term', 'factor']);
    assert.deepStrictEqual(rules[0].expr, { type: 'loop', item: { type: 'n', name: 'term' }, sep: { type: 't', text: '+' } });
    const factor = rules[2].expr;
    assert.strictEqual(factor.type, 'alt');
    assert.deepStrictEqual(factor.items[0], { type: 'n', name: 'number' });
    assert.deepStrictEqual(factor.items[1].items.map((n) => n.text || n.name), ['(', 'expression', ')']);
});

test('{ } alone, [ ], groups, the ellipsis and rules over several lines', () => {
    const { rules, errors } = SD.parse(`number = [ "-" ] digit { digit }
digit = "0" | "1" | … | "9"
list = "<"
       ( number | list ) { "," ( number | list ) } ">" .
brackets = { "(" brackets ")" }`);
    assert.deepStrictEqual(errors, []);
    const [number, digit, list, brackets] = rules.map((r) => r.expr);
    assert.deepStrictEqual(number.items.map((n) => n.type), ['opt', 'loop']);
    assert.deepStrictEqual(number.items[1], { type: 'loop', item: { type: 'n', name: 'digit' }, sep: null });
    assert.deepStrictEqual(digit.items.map((n) => n.type), ['t', 't', 'gap', 't']);
    assert.deepStrictEqual(list.items.map((n) => n.type), ['t', 'loop', 't']);
    assert.strictEqual(list.items[1].sep.text, ',');
    assert.strictEqual(brackets.type, 'rep');
});

test('mistakes are named with their line', () => {
    assert.match(SD.parse('expression = term { "+" term').errors[0], /Am Ende: Zu \{ aus Zeile 1 fehlt \}/);
    assert.match(SD.parse('expression = term + term').errors[0], /Das Zeichen \+ gehört nicht zu EBNF/);
    assert.match(SD.parse('"a" "b"').errors[0], /Eine Regel beginnt mit ihrem Namen und =/);
    assert.match(SD.parse('a = ').errors[0], /Hier fehlt etwas/);
    assert.match(SD.parse('a = ""').errors[0], /Zwischen den Anführungszeichen steht nichts/);
    assert.strictEqual(SD.svg('a = (').svg, null);
});

test('a diagram per rule: terminals rounded, nonterminals square, nothing on top of each other', () => {
    const r = SD.svg(EXPRESSIONS + '\nnumber = [ "-" ] digit { digit } [ "." digit { digit } ]\ndigit = "0" | "1" | … | "9"');
    assert.deepStrictEqual(r.errors, []);
    assert.deepStrictEqual([...r.svg.matchAll(/class="sd-title"[^>]*>([^<]*)</g)].map((m) => m[1]), ['expression', 'term', 'factor', 'number', 'digit']);
    const all = boxes(r.svg);
    for (const b of all) assert.strictEqual(b.rx, b.kind === 'terminal' ? b.h / 2 : 2);
    assert.deepStrictEqual(all.filter((b) => b.kind === 'terminal').length, 4 + 2 + 3);
    noOverlaps(all);
    for (const b of all) assert.ok(b.x >= 0 && b.y >= 0 && b.x + b.w <= r.width && b.y + b.h <= r.height, JSON.stringify(b));
    assert.match(r.svg, /<desc>expression = term/);
});

test('only draws the rules it is asked for; a file gets its colours written out', () => {
    const r = SD.svg(EXPRESSIONS, { only: ['factor'], file: true, background: '#ffffff' });
    assert.deepStrictEqual([...r.svg.matchAll(/class="sd-title"[^>]*>([^<]*)</g)].map((m) => m[1]), ['factor']);
    assert.ok(!r.svg.includes('var(--sd'));
    assert.match(r.svg, /<rect width="\d+" height="\d+" fill="#ffffff"\/>/);
    assert.ok(SD.svg(EXPRESSIONS).svg.includes('var(--sd-ink,#1f2937)'));
});

test('a choice of choices: rows above and below keep clear of each other', () => {
    const r = SD.svg('a = [ "x" | "y" ] ( "p" | "q" "r" | { "s" } ) [ [ "t" ] "u" ]');
    assert.deepStrictEqual(r.errors, []);
    noOverlaps(boxes(r.svg));
});

test('a syntax tree: one box per node, the word from left to right, no boxes on top of each other', () => {
    const r = SD.treeSvg('expression(term(factor(number(digit("2")))) "+" term(factor(number(digit("3"))) "*" factor(number(digit("4")))))');
    assert.deepStrictEqual(r.errors, []);
    const all = boxes(r.svg);
    assert.strictEqual(all.length, 1 + 2 + 3 * 3 + 5);
    noOverlaps(all);
    const terminals = all.filter((b) => b.kind === 'terminal').sort((a, b) => a.x - b.x);
    const texts = [...r.svg.matchAll(/<text class="sd-text" x="([\d.]+)"[^>]*>([^<]*)</g)].sort((a, b) => a[1] - b[1]).map((m) => m[2]);
    assert.strictEqual(terminals.length, 5);
    assert.deepStrictEqual(texts, ['2', '+', '3', '*', '4']);
    assert.strictEqual([...r.svg.matchAll(/class="sd-edge"/g)].length, all.length - 1);
});

test('a tree with a mistake gives no picture', () => {
    assert.ok(SD.parseTree('a(b').errors.length > 0);
    assert.strictEqual(SD.treeSvg('a(b) c').svg, null);
});
