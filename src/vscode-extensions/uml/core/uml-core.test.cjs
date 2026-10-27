// node --test src/vscode-extensions/uml/core/
const test = require('node:test');
const assert = require('node:assert/strict');
const path = require('path');
const { execFileSync, spawnSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const core = require('./uml-core.js');

// The library of the activity about object orientation.
const LIBRARY = `# Eine Bibliothek
class Medium (abstract)
  - title: String
  - available: boolean
  + Medium(title: String)
  + lend(): void
  + isAvailable(): boolean

class Book extends Medium
  - author: String
  + Book(title: String, author: String)

class Dvd extends Medium
  - minutes: int

class Member
  - name: String
  + borrow(medium: Medium): void

class Library
  - name: String

association loans: Member 1 -- 0..* Medium
association owns: Library 1 -> 0..* Medium
`;

// A bigger one: ten classes, two levels of inheritance, associations
// within and across layers, two of a class with itself.
const ZOO = `class Zoo
  - name: String
  - enclosures: List<Enclosure>
  + Zoo(name: String)
  + open(): void
class Enclosure
  - number: int
  + add(animal: Animal): boolean
class Animal (abstract)
  - name: String
  + feed(food: Food): void
class Mammal extends Animal (abstract)
  - furColor: String
class Bird extends Animal
  + fly(): void
class Lion extends Mammal
  + roar(): void
class Elephant extends Mammal
  - trunkLength: double
class Keeper
  - name: String
  + care(enclosure: Enclosure): void
class Food
  - kind: String
class Visitor
  - ticket: int
association contains: Zoo 1 -- 1..* Enclosure
association lives: Enclosure 1 -- 0..* Animal
association cares: Keeper 1..* -> 1..* Enclosure
association employs: Zoo 1 -- 1..* Keeper
association visits: Visitor 0..* -- 1 Zoo
association eats: Animal * -> 1..3 Food
association mentors: Keeper 0..1 -- 0..* Keeper
association parentOf: Animal 0..2 -- 0..* Animal
`;

const errorsOf = (text) => core.parse(text).errors;
const warningsOf = (text) => core.parse(text).warnings;
// The one problem of a text, as [line, from, to] and its message.
function only(list) {
    assert.equal(list.length, 1, JSON.stringify(list, null, 1));
    return list[0];
}
const where = (p) => [p.line, p.from, p.to];

test('the library: the summary for the checks, exactly', () => {
    const s = core.summary(core.parse(LIBRARY));
    assert.deepEqual(s, {
        version: core.VERSION,
        errors: [],
        warnings: [],
        classes: [
            { name: 'Medium', abstract: true, extends: null, line: 2,
              attributes: [{ name: 'title', type: 'String', visibility: '-' }, { name: 'available', type: 'boolean', visibility: '-' }],
              methods: [
                  { name: 'Medium', constructor: true, returns: null, visibility: '+', params: [{ name: 'title', type: 'String' }] },
                  { name: 'lend', constructor: false, returns: 'void', visibility: '+', params: [] },
                  { name: 'isAvailable', constructor: false, returns: 'boolean', visibility: '+', params: [] },
              ] },
            { name: 'Book', abstract: false, extends: 'Medium', line: 9,
              attributes: [{ name: 'author', type: 'String', visibility: '-' }],
              methods: [{ name: 'Book', constructor: true, returns: null, visibility: '+',
                  params: [{ name: 'title', type: 'String' }, { name: 'author', type: 'String' }] }] },
            { name: 'Dvd', abstract: false, extends: 'Medium', line: 13,
              attributes: [{ name: 'minutes', type: 'int', visibility: '-' }], methods: [] },
            { name: 'Member', abstract: false, extends: null, line: 16,
              attributes: [{ name: 'name', type: 'String', visibility: '-' }],
              methods: [{ name: 'borrow', constructor: false, returns: 'void', visibility: '+', params: [{ name: 'medium', type: 'Medium' }] }] },
            { name: 'Library', abstract: false, extends: null, line: 20,
              attributes: [{ name: 'name', type: 'String', visibility: '-' }], methods: [] },
        ],
        associations: [
            { name: 'loans', from: 'Member', to: 'Medium', fromMult: '1', toMult: '0..*', directed: false, line: 23 },
            { name: 'owns', from: 'Library', to: 'Medium', fromMult: '1', toMult: '0..*', directed: true, line: 24 },
        ],
    });
    // keys in the documented order
    assert.deepEqual(Object.keys(s), ['version', 'errors', 'warnings', 'classes', 'associations']);
    assert.deepEqual(Object.keys(s.classes[0]), ['name', 'abstract', 'extends', 'line', 'attributes', 'methods']);
    assert.deepEqual(Object.keys(s.classes[0].methods[0]), ['name', 'constructor', 'returns', 'visibility', 'params']);
});

test('problems in the summary: line, from, to, msg', () => {
    const s = core.summary(core.parse('class Book\n  title: String\nclass Shelf\n  - books: Boook\n'));
    assert.deepEqual(Object.keys(s.errors[0]), ['line', 'from', 'to', 'msg']);
    assert.deepEqual(where(s.errors[0]), [2, 2, 15]);
    assert.deepEqual(where(s.warnings[0]), [4, 11, 16]);
});

test('comments, blank lines and indentation', () => {
    const m = core.parse('# a library\n\nclass Book   # the book\n\t- title: String # its title\n\n    + lend(): void\n  # name: int\n');
    assert.deepEqual(m.errors, []);
    const book = m.classes[0];
    assert.deepEqual(book.attributes.map((a) => a.name), ['title']);
    assert.deepEqual(book.methods.map((x) => x.name), ['lend']);
    // »# name: int« is a comment, not a protected attribute
    assert.equal(book.attributes.length, 1);
});

test('class headers: extends, (abstract), both', () => {
    const m = core.parse('class A (abstract)\nclass B extends A (abstract)\nclass C extends B\nclass D  extends  C  ( abstract )\n');
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.classes.map((c) => [c.name, c.extends, c.abstract]),
        [['A', null, true], ['B', 'A', true], ['C', 'B', false], ['D', 'C', true]]);
});

test('a method may end with ? or ! as in Ruby, an attribute may not', () => {
    const m = core.parse('class Book\n  + available?(): boolean\n  + give_back(): void\n  + renew!(): void\n');
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.classes[0].methods.map((x) => x.name), ['available?', 'give_back', 'renew!']);
    assert.notDeepEqual(core.parse('class Book\n  - lent?: boolean\n').errors, []);
});

test('types as in Java, spaces normalised', () => {
    const m = core.parse(`class Shelf
  - books: List<Book>
  - byTitle: Map<String,Book>
  - grid: int[][]
  - nested:Map< String , List<Book> >
  - members: Set<Member>
  - prices: double[]
  - letter: char
  - count: long
  - ok: Boolean
  + find( title :String , n:int ): List<Book>
class Book
class Member
`);
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.warnings, []);
    assert.deepEqual(m.classes[0].attributes.map((a) => a.type),
        ['List<Book>', 'Map<String, Book>', 'int[][]', 'Map<String, List<Book>>', 'Set<Member>', 'double[]', 'char', 'long', 'Boolean']);
    const find = m.classes[0].methods[0];
    assert.deepEqual(find.params, [{ name: 'title', type: 'String' }, { name: 'n', type: 'int' }]);
    assert.equal(find.returns, 'List<Book>');
});

test('constructors are named like their class, without return type', () => {
    const m = core.parse('class Book\n  + Book()\n  + Book(title: String)\n  - Book(title: String, year: int)\n');
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.classes[0].methods.map((x) => [x.constructor, x.returns, x.visibility, x.params.length]),
        [[true, null, '+', 0], [true, null, '+', 1], [true, null, '-', 2]]);
});

test('associations: undirected, directed, every kind of multiplicity', () => {
    const m = core.parse(`class A
class B
association a1: A 1 -- * B
association a2: A 0..1 -> 0..* B
association a3: A 1..* -- 3 B
association a4: A 2..4 -- 0..0 B
association a5: A 1--1 B
association self: A 0..1 -- 0..* A
`);
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.associations.map((a) => [a.name, a.from, a.fromMult, a.directed, a.toMult, a.to]), [
        ['a1', 'A', '1', false, '*', 'B'],
        ['a2', 'A', '0..1', true, '0..*', 'B'],
        ['a3', 'A', '1..*', false, '3', 'B'],
        ['a4', 'A', '2..4', false, '0..0', 'B'],
        ['a5', 'A', '1', false, '1', 'B'],
        ['self', 'A', '0..1', false, '0..*', 'A'],
    ]);
});

test('bad multiplicities: each with its own hint and range', () => {
    const base = 'class A\nclass B\n';
    let p = only(errorsOf(`${base}association x: A 5..2 -- 1 B\n`));
    assert.deepEqual(where(p), [3, 17, 21]);
    assert.match(p.msg, /untere Grenze größer als die obere/);
    p = only(errorsOf(`${base}association x: A 1 -- n B\n`));
    assert.deepEqual(where(p), [3, 22, 23]);
    assert.match(p.msg, /»\*« statt »n«/);
    p = only(errorsOf(`${base}association x: A 1 -- 0..n B\n`));
    assert.match(p.msg, /»0\.\.\*«/);
    p = only(errorsOf(`${base}association x: A 1 -- *..1 B\n`));
    assert.match(p.msg, /keine Multiplizität/);
    p = only(errorsOf(`${base}association x: A 1 -- 1.5 B\n`));
    assert.match(p.msg, /keine Multiplizität/);
    p = only(errorsOf(`${base}association x: A -- B\n`));
    assert.match(p.msg, /An beide Enden gehört eine Multiplizität/);
    p = only(errorsOf(`${base}association x: A 1 -- B\n`));
    assert.match(p.msg, /An beide Enden/);
});

test('association lines: arrows, names, colons', () => {
    const base = 'class A\nclass B\n';
    let p = only(errorsOf(`${base}association x: A 1 <- 1 B\n`));
    assert.deepEqual(where(p), [3, 19, 21]);
    assert.match(p.msg, /»B 1 -> 1 A«/);
    p = only(errorsOf(`${base}association x: A 1 <-> 1 B\n`));
    assert.match(p.msg, /ohne Pfeil: »--«/);
    p = only(errorsOf(`${base}association x: A 1 - 1 B\n`));
    assert.match(p.msg, /zwei Striche/);
    p = only(errorsOf(`${base}association x A 1 -- 1 B\n`));
    assert.match(p.msg, /Namen und dahinter einen Doppelpunkt/);
    p = only(errorsOf(`${base}association A 1 -- 1 B\n`));
    assert.match(p.msg, /Namen und dahinter einen Doppelpunkt/);
    p = only(errorsOf(`${base}association\n`));
    assert.match(p.msg, /Nach »association« kommt ihr Name/);
    p = only(errorsOf(`${base}association x: A 1 -- 1 B C\n`));
    assert.match(p.msg, /So sieht eine Assoziation aus/);
});

test('associations to unknown classes, and a name used twice', () => {
    const errors = errorsOf('class Member\nclass Medium\nassociation loans: Membr 1 -- 0..* Libary\n');
    assert.equal(errors.length, 2);
    assert.deepEqual(where(errors[0]), [3, 19, 24]);
    assert.match(errors[0].msg, /»Membr« ist keine Klasse dieser Datei\. Meinst du »Member«\?/);
    assert.deepEqual(where(errors[1]), [3, 35, 41]);
    const w = only(warningsOf('class A\nclass B\nassociation x: A 1 -- 1 B\nassociation x: B 1 -- 1 A\n'));
    assert.deepEqual(where(w), [4, 12, 13]);
    assert.match(w.msg, /schon die Assoziation in Zeile 3/);
});

test('members need + or -', () => {
    let p = only(errorsOf('class Book\n  title: String\n'));
    assert.deepEqual(where(p), [2, 2, 15]);
    assert.match(p.msg, /Sichtbarkeit: »\+« \(public\) oder »-« \(private\), zum Beispiel »- title: String«/);
    p = only(errorsOf('class Book\n  lend(): void\n'));
    assert.match(p.msg, /»- lend\(\): void«/);
    p = only(errorsOf('class Book\n  private title: String\n'));
    assert.deepEqual(where(p), [2, 2, 9]);
    assert.match(p.msg, /Statt »private« schreibst du »-«/);
    p = only(errorsOf('class Book\n  public lend(): void\n'));
    assert.match(p.msg, /Statt »public« schreibst du »\+«/);
    p = only(errorsOf('class Book\n  protected title: String\n'));
    assert.match(p.msg, /»protected« gibt es hier nicht/);
    p = only(errorsOf('class Book\n  ~ title: String\n'));
    assert.deepEqual(where(p), [2, 2, 3]);
    assert.match(p.msg, /»~« \(package\)/);
});

test('methods: return type required, constructors without one', () => {
    let p = only(errorsOf('class Book\n  + lend()\n'));
    assert.deepEqual(where(p), [2, 4, 10]);
    assert.match(p.msg, /braucht einen Rückgabetyp, »void«, wenn sie nichts zurückgibt/);
    p = only(errorsOf('class Book\n  + book(title: String)\n'));
    assert.match(p.msg, /Ein Konstruktor heißt genau wie die Klasse: »Book«/);
    p = only(errorsOf('class Book\n  + Book(title: String): void\n'));
    assert.deepEqual(where(p), [2, 23, 29]);
    assert.match(p.msg, /Ein Konstruktor hat keinen Rückgabetyp/);
    p = only(errorsOf('class Book\n  + lend():\n'));
    assert.match(p.msg, /fehlt der Rückgabetyp/);
    p = only(errorsOf('class Book\n  + void lend()\n'));
    assert.match(p.msg, /Rückgabetyp steht in UML hinten: »\+ lend\(…\): void«/);
    p = only(errorsOf('class Book\n  + lend(: void\n'));
    assert.match(p.msg, /schließende Klammer/);
});

test('parameters: name: Type, separated by commas', () => {
    let p = only(errorsOf('class Book\n  + f(a): void\n'));
    assert.deepEqual(where(p), [2, 6, 7]);
    assert.match(p.msg, /Der Parameter »a« braucht einen Typ/);
    p = only(errorsOf('class Book\n  + f(String title): void\n'));
    assert.deepEqual(where(p), [2, 6, 18]);
    assert.match(p.msg, /Erst der Name, dann ein Doppelpunkt und der Typ: »title: String«/);
    p = only(errorsOf('class Book\n  + f(a: int,): void\n'));
    assert.match(p.msg, /Hier fehlt ein Parameter/);
    p = only(errorsOf('class Book\n  + f(a: int, a: int): void\n'));
    assert.match(p.msg, /Den Parameter »a« gibt es in dieser Methode schon/);
    p = only(errorsOf('class Book\n  + f(a:): void\n'));
    assert.match(p.msg, /fehlt der Typ des Parameters »a«/);
});

test('attributes: a type after a colon, no semicolon', () => {
    let p = only(errorsOf('class Book\n  - title\n'));
    assert.match(p.msg, /Das Attribut »title« braucht einen Typ, zum Beispiel »- title: String«/);
    p = only(errorsOf('class Book\n  - String title\n'));
    assert.match(p.msg, /»- title: String«/);
    p = only(errorsOf('class Book\n  - title:\n'));
    assert.match(p.msg, /Nach dem Doppelpunkt fehlt der Typ/);
    p = only(errorsOf('class Book\n  - title: String;\n'));
    assert.deepEqual(where(p), [2, 17, 18]);
    assert.match(p.msg, /kein Semikolon/);
    p = only(errorsOf('class Book\n  - title: List<String\n'));
    assert.deepEqual(where(p), [2, 11, 22]);
    assert.match(p.msg, /»List<String« ist kein Typ/);
    p = only(errorsOf('class Book\n  - price: double = 3\n'));
    assert.match(p.msg, /ist kein Typ/);
    p = only(errorsOf('class Book\n  -\n'));
    assert.match(p.msg, /fehlt das Attribut oder die Methode/);
});

test('void only as a return type', () => {
    let p = only(errorsOf('class Book\n  - x: void\n'));
    assert.deepEqual(where(p), [2, 7, 11]);
    assert.match(p.msg, /»void« gibt es nur als Rückgabetyp/);
    p = only(errorsOf('class Book\n  + f(a: void): int\n'));
    assert.deepEqual(where(p), [2, 9, 13]);
    p = only(errorsOf('class Book\n  + f(): List<void>\n'));
    assert.match(p.msg, /»void« gibt es nur als Rückgabetyp/);
    p = only(errorsOf('class Book\n  + f(): void[]\n'));
    assert.match(p.msg, /»void«/);
});

test('members declared twice', () => {
    let p = only(errorsOf('class Book\n  - title: String\n  + title: int\n'));
    assert.deepEqual(where(p), [3, 4, 9]);
    assert.match(p.msg, /Das Attribut »title« gibt es in »Book« schon \(Zeile 2\)/);
    p = only(errorsOf('class Book\n  + f(a: int): void\n  - f(b: int): int\n'));
    assert.match(p.msg, /Die Methode »f\(int\)« gibt es in »Book« schon \(Zeile 2\)/);
    // overloading with other parameter types is fine
    assert.deepEqual(errorsOf('class Book\n  + f(a: int): void\n  + f(a: String): void\n  + f(): void\n'), []);
    p = only(errorsOf('class Book\n  + Book(a: int)\n  + Book(b: int)\n'));
    assert.match(p.msg, /Den Konstruktor »Book\(int\)« gibt es in »Book« schon/);
    // an attribute and a method may share a name
    assert.deepEqual(errorsOf('class Book\n  - title: String\n  + title(): String\n'), []);
});

test('classes: twice, extends unknown, cycles, lowercase names', () => {
    let p = only(errorsOf('class Book\nclass Book\n'));
    assert.deepEqual(where(p), [2, 6, 10]);
    assert.match(p.msg, /Die Klasse »Book« gibt es schon \(Zeile 1\)/);
    p = only(errorsOf('class Medium\nclass Book extends Medum\n'));
    assert.deepEqual(where(p), [2, 19, 24]);
    assert.match(p.msg, /»Medum« ist keine Klasse dieser Datei\. Meinst du »Medium«\?/);
    p = only(errorsOf('class A extends A\n'));
    assert.match(p.msg, /»A« kann nicht von sich selbst erben/);
    const cycle = errorsOf('class A extends C\nclass B extends A\nclass C extends B\nclass D extends A\n');
    assert.equal(cycle.length, 3);
    assert.deepEqual(cycle.map(where), [[1, 16, 17], [2, 16, 17], [3, 16, 17]]);
    assert.match(cycle[0].msg, /Kreis: A → C → B → A/);
    const w = only(warningsOf('class book\n'));
    assert.deepEqual(where(w), [1, 6, 10]);
    assert.match(w.msg, /Klassennamen schreibt man groß: »Book«/);
});

test('unknown types are warnings that ask', () => {
    let w = only(warningsOf('class Book\n  - author: Strin\n'));
    assert.deepEqual(where(w), [2, 12, 17]);
    assert.match(w.msg, /»Strin« ist weder ein Java-Typ noch eine Klasse dieser Datei\. Fehlt die Klasse noch, oder ist der Name falsch geschrieben\? Meinst du »String«\?/);
    w = only(warningsOf('class Book\n  - shelf: Shelf\n'));
    assert.doesNotMatch(w.msg, /Meinst du/);
    w = only(warningsOf('class Book\n  - name: string\n'));
    assert.match(w.msg, /Meinst du »String«\?/);
    w = only(warningsOf('class Book\n  + f(): List<Boook>\n'));
    assert.deepEqual(where(w), [2, 14, 19]);
    w = only(warningsOf('class Book\n  - tags: List<int>\n'));
    assert.match(w.msg, /»Integer« statt »int«/);
    w = only(warningsOf('class Book\n  - byName: Map<String>\n'));
    assert.match(w.msg, /zwei Typen/);
    w = only(warningsOf('class Book\n  - all: List\n'));
    assert.match(w.msg, /»List<Book>«/);
    w = only(warningsOf('class Book\n  - x: String<Book>\n'));
    assert.match(w.msg, /keine Typen in spitzen Klammern/);
    // a class later in the file is known
    assert.deepEqual(warningsOf('class Shelf\n  - books: Book[]\nclass Book\n'), []);
});

test('lines that match nothing say what was expected', () => {
    const cases = [
        ['Class Book', /klein/],
        ['klasse Book', /englisch/],
        ['relationship x: A 1 -- 1 B', /heißt eine Beziehung »association«/],
        ['abstract class Medium', /»class Medium \(abstract\)«/],
        ['interface Lendable', /»interface« gibt es in der UML-Werkstatt nicht/],
        ['- title: String', /eingerückt unter ihrer Klasse/],
        ['public class Book', /»public« steht hier nicht vor »class«/],
        ['class', /Nach »class« kommt der Name/],
        ['class Book (abstract) extends Medium', /steht am Ende der Zeile/],
        ['class Medium abstract', /in Klammern am Ende/],
        ['class Book implements Lendable', /»implements« gibt es hier nicht/],
        ['class Book extends', /Nach »extends« kommt der Name der Oberklasse/],
        ['class Book extends A, B', /genau einer Oberklasse/],
        ['class Book : Medium', /mit »extends«/],
        ['class Book(Medium)', /mit »extends«/],
        ['class 1Book', /ein Wort ohne Leerzeichen/],
        ['class My Book', /ein Wort ohne Leerzeichen/],
        ['hello world', /Unbekannte Zeile/],
    ];
    for (const [line, re] of cases) {
        const p = only(errorsOf(`${line}\n`));
        assert.deepEqual(where(p), [1, 0, line.length], line);
        assert.match(p.msg, re, line);
    }
});

test('indented lines outside a class', () => {
    let p = only(errorsOf('  - title: String\nclass Book\n'));
    assert.deepEqual(where(p), [1, 2, 17]);
    assert.match(p.msg, /gehört zu keiner Klasse/);
    p = only(errorsOf('class A\nclass B\nassociation x: A 1 -- 1 B\n  - title: String\n'));
    assert.match(p.msg, /Unter einer Assoziation steht nichts eingerückt/);
    // under a broken header: only the header is wrong
    p = only(errorsOf('class Book : Medium\n  - title: String\n  + Book(title: String)\n'));
    assert.equal(p.line, 1);
});

test('problems are sorted by line and column', () => {
    const list = errorsOf('class A\n  - x: void\n  y: int\nclass A\n');
    assert.deepEqual(list.map((p) => p.line), [2, 3, 4]);
});

// ---------- the diagram ------------------------------------------------------

const rect = (n) => ({ x0: n.x - n.w / 2, x1: n.x + n.w / 2, y0: n.y - n.h / 2, y1: n.y + n.h / 2 });
const overlap = (a, b, pad = 0) => a.x0 < b.x1 + pad && b.x0 < a.x1 + pad && a.y0 < b.y1 + pad && b.y0 < a.y1 + pad;

test('the svg: a box per class with its compartments, markers per edge', () => {
    const { svg, graph } = core.diagram(LIBRARY);
    for (const name of ['Medium', 'Book', 'Dvd', 'Member', 'Library']) assert.match(svg, new RegExp(`<g class="cls" data-class="${name}"`));
    assert.equal((svg.match(/<rect class="cbox"/g) || []).length, 5);
    // two separators per box: attributes, methods
    assert.equal((svg.match(/<line class="csep"/g) || []).length, 10);
    assert.equal((svg.match(/<polygon class="cinherit"/g) || []).length, 2);
    assert.equal((svg.match(/<g class="edge inherit" data-from="Book" data-to="Medium">/g) || []).length, 1);
    assert.equal((svg.match(/<path class="carrow"/g) || []).length, 1);
    assert.match(svg, /<g class="edge assoc directed" data-from="Library" data-to="Medium" data-name="owns">/);
    assert.match(svg, /<g class="edge assoc" data-from="Member" data-to="Medium" data-name="loans">/);
    assert.match(svg, />«abstract»</);
    assert.match(svg, /<text class="cname abs"[^>]*>Medium</);
    assert.match(svg, />- title: String</);
    assert.match(svg, />\+ Book\(title: String, author: String\)</);
    assert.match(svg, />\+ lend\(\): void</);
    assert.match(svg, /<text class="cmult"[^>]*>0\.\.\*</);
    assert.match(svg, /<text class="clbl"[^>]*>loans</);
    // styles only for the diagram
    for (const rule of svg.match(/<style>([\s\S]*)<\/style>/)[1].split('}').filter((r) => r.trim())) {
        assert.match(rule.trim(), /^\.cd-diagram /);
    }
    assert.equal(graph.nodes.length, 5);
});

test('the svg escapes generics and names', () => {
    const { svg } = core.diagram('class Shelf\n  - books: Map<String, Book>\nclass Book\n');
    assert.match(svg, />- books: Map&lt;String, Book&gt;</);
    assert.doesNotMatch(svg, /Map<String/);
});

test('layout: superclasses above, associated classes beside, no overlaps', () => {
    const { graph } = core.diagram(LIBRARY);
    const by = Object.fromEntries(graph.nodes.map((n) => [n.name, n]));
    assert.ok(by.Medium.y < by.Book.y && by.Medium.y < by.Dvd.y);
    assert.ok(rect(by.Medium).y1 < rect(by.Book).y0);
    // Member and Library only associated: in Medium's layer, on either side
    assert.equal(by.Member.y, by.Medium.y);
    assert.equal(by.Library.y, by.Medium.y);
    for (const a of graph.nodes) for (const b of graph.nodes) if (a !== b) assert.ok(!overlap(rect(a), rect(b), 10), `${a.name} ${b.name}`);
});

test('layout: the zoo, two levels of inheritance and many associations', () => {
    const { graph, svg } = core.diagram(ZOO);
    const by = Object.fromEntries(graph.nodes.map((n) => [n.name, n]));
    assert.ok(by.Animal.y < by.Mammal.y && by.Mammal.y < by.Lion.y);
    assert.equal(by.Mammal.y, by.Bird.y);
    for (const a of graph.nodes) for (const b of graph.nodes) if (a !== b) assert.ok(!overlap(rect(a), rect(b), 10), `${a.name} ${b.name}`);
    // no label on a box
    const geo = core.geometry(graph);
    for (const l of geo.labels) {
        const r = { x0: l.x - l.w / 2, x1: l.x + l.w / 2, y0: l.y - l.h / 2, y1: l.y + l.h / 2 };
        for (const n of graph.nodes) assert.ok(!overlap(r, rect(n)), `${l.text} on ${n.name}`);
    }
    // no label on another
    geo.labels.forEach((l, i) => geo.labels.slice(i + 1).forEach((m) => {
        assert.ok(Math.abs(l.x - m.x) >= (l.w + m.w) / 2 || Math.abs(l.y - m.y) >= (l.h + m.h) / 2, `${l.text} / ${m.text}`);
    }));
    // a picture not much wider than needed
    const vb = core.viewBox(graph);
    assert.ok(vb[2] < 2000, `width ${vb[2]}`);
    assert.equal((svg.match(/<g class="cls"/g) || []).length, 10);
    assert.equal((svg.match(/class="edge assoc/g) || []).length, 8);
});

test('lines end on the box borders; heads have their tip there', () => {
    const { graph } = core.diagram(LIBRARY);
    const geo = core.geometry(graph);
    const by = Object.fromEntries(graph.nodes.map((n) => [n.id, n]));
    const onBorder = (p, n) => {
        const r = rect(n);
        const inside = p.x >= r.x0 - 0.2 && p.x <= r.x1 + 0.2 && p.y >= r.y0 - 0.2 && p.y <= r.y1 + 0.2;
        const edge = [Math.abs(p.x - r.x0), Math.abs(p.x - r.x1), Math.abs(p.y - r.y0), Math.abs(p.y - r.y1)].some((d) => d < 0.2);
        return inside && edge;
    };
    const nums = (s) => s.match(/-?[\d.]+/g).map(Number);
    for (const l of geo.lines) {
        const [x0, y0] = nums(l.d);
        assert.ok(onBorder({ x: x0, y: y0 }, by[l.edge.a]), `start of ${l.edge.a}`);
        if (l.triangle) {
            const [tx, ty] = nums(l.triangle);
            assert.ok(onBorder({ x: tx, y: ty }, by[l.edge.b]), 'triangle tip');
        }
        if (l.head) {
            const p = nums(l.head);
            assert.ok(onBorder({ x: p[2], y: p[3] }, by[l.edge.b]), 'arrow tip');
            const end = nums(l.d).slice(-2);
            assert.deepEqual(end, [p[2], p[3]]);
        }
    }
});

test('side-by-side lines between the same two classes, and a loop', () => {
    const text = 'class A\nclass B extends A\nassociation x: B 1 -- 1 A\nassociation y: A 1 -> 1 B\nassociation me: A 0..1 -- * A\n';
    const { graph, svg } = core.diagram(text);
    const geo = core.geometry(graph);
    const starts = geo.lines.filter((l) => l.edge.a !== l.edge.b).map((l) => l.d.split(' ')[0]);
    assert.equal(new Set(starts).size, 3);
    assert.match(svg, /data-name="me"><path class="cln" d="M[-\d.,]+ C/);
    assert.equal(geo.labels.filter((l) => l.kind === 'mult').length, 6);
});

test('the layout is deterministic and handles empty and broken models', () => {
    assert.equal(core.diagram(ZOO).svg, core.diagram(ZOO).svg);
    const empty = core.diagram('');
    assert.match(empty.svg, /^<svg /);
    assert.deepEqual(core.viewBox(empty.graph), [0, 0, 600, 380]);
    // a cycle (an error) must not hang the layout
    const { graph } = core.diagram('class A extends B\nclass B extends A\nclass C extends A\n');
    assert.equal(graph.nodes.length, 3);
});

// ---------- command line -----------------------------------------------------

test('command line: --json, --svg, a missing file, usage', () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'uml-'));
    const file = path.join(dir, 'bibliothek.uml');
    fs.writeFileSync(file, LIBRARY);
    const script = path.join(__dirname, 'uml-core.js');
    const json = JSON.parse(execFileSync('node', [script, '--json', file], { encoding: 'utf8' }));
    assert.deepEqual(json, core.summary(core.parse(LIBRARY)));
    const svg = execFileSync('node', [script, '--svg', file], { encoding: 'utf8' });
    assert.match(svg, /^<svg [^>]*class="cd-diagram"/);
    assert.match(svg, /<rect x="[-\d]+" y="[-\d]+" width="\d+" height="\d+" fill="#ffffff"\/>/);
    const missing = JSON.parse(execFileSync('node', [script, '--json', path.join(dir, 'nope.uml')], { encoding: 'utf8' }));
    assert.deepEqual(missing, { version: core.VERSION, missing: true, errors: [{ line: 0, msg: `Die Datei ${path.join(dir, 'nope.uml')} gibt es nicht.` }] });
    const broken = path.join(dir, 'broken.uml');
    fs.writeFileSync(broken, 'class Book\n  title: String\n');
    const r = spawnSync('node', [script, '--svg', broken], { encoding: 'utf8' });
    assert.equal(r.status, 1);
    assert.match(r.stderr, /broken\.uml:2: Vor jedes Attribut/);
    const withErrors = JSON.parse(execFileSync('node', [script, '--json', broken], { encoding: 'utf8' }));
    assert.equal(withErrors.errors.length, 1);
    assert.equal(spawnSync('node', [script, '--sql', file]).status, 2);
    assert.equal(spawnSync('node', [script, '--json']).status, 2);
});

test('the core version is the extension version', () => {
    const pkg = JSON.parse(fs.readFileSync(path.join(__dirname, '..', 'package.json'), 'utf8'));
    assert.equal(core.VERSION, pkg.version);
});
