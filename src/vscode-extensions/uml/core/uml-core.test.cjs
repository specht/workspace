// node --test src/vscode-extensions/uml/core/
const test = require('node:test');
const assert = require('node:assert/strict');
const path = require('path');
const { execFileSync, spawnSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const core = require('./uml-core.js');

// The checkouts of the supermarket in the activity about object orientation.
const SUPERMARKET = `# Kassen im Supermarkt
class Customer
  - items: int
  - arrival: int
  + Customer(items: int, arrival: int)

class Checkout
  - freeAt: int
  + Checkout()
  + accepts(customer: Customer): boolean
  + serviceTime(customer: Customer): int
  + serve(customer: Customer): int

class ExpressCheckout extends Checkout
  + accepts(customer: Customer): boolean

class SelfCheckout extends Checkout
  + serviceTime(customer: Customer): int

class Supermarket
  + Supermarket(checkouts: List<Checkout>)
  + arrive(customer: Customer): int
  + averageWait(): double

association checkouts: Supermarket 1 -- 1..* Checkout
`;

// What the supermarket does not have: an abstract class and an association
// navigable in one direction.
const QUEUE = `class Checkout (abstract)
  - freeAt: int
  + serve(customer: Customer): int
class SelfCheckout extends Checkout
  + serviceTime(customer: Customer): int
class Customer
  - items: int
association queue: Checkout 1 -> 0..* Customer
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

test('the supermarket: the summary for the checks, exactly', () => {
    const s = core.summary(core.parse(SUPERMARKET));
    const customer = [{ name: 'customer', type: 'Customer' }];
    assert.deepEqual(s, {
        version: core.VERSION,
        errors: [],
        warnings: [],
        classes: [
            { name: 'Customer', abstract: false, extends: null, line: 2,
              attributes: [{ name: 'items', type: 'int', visibility: '-' }, { name: 'arrival', type: 'int', visibility: '-' }],
              methods: [{ name: 'Customer', constructor: true, returns: null, visibility: '+',
                  params: [{ name: 'items', type: 'int' }, { name: 'arrival', type: 'int' }] }] },
            { name: 'Checkout', abstract: false, extends: null, line: 7,
              attributes: [{ name: 'freeAt', type: 'int', visibility: '-' }],
              methods: [
                  { name: 'Checkout', constructor: true, returns: null, visibility: '+', params: [] },
                  { name: 'accepts', constructor: false, returns: 'boolean', visibility: '+', params: customer },
                  { name: 'serviceTime', constructor: false, returns: 'int', visibility: '+', params: customer },
                  { name: 'serve', constructor: false, returns: 'int', visibility: '+', params: customer },
              ] },
            { name: 'ExpressCheckout', abstract: false, extends: 'Checkout', line: 14, attributes: [],
              methods: [{ name: 'accepts', constructor: false, returns: 'boolean', visibility: '+', params: customer }] },
            { name: 'SelfCheckout', abstract: false, extends: 'Checkout', line: 17, attributes: [],
              methods: [{ name: 'serviceTime', constructor: false, returns: 'int', visibility: '+', params: customer }] },
            { name: 'Supermarket', abstract: false, extends: null, line: 20, attributes: [],
              methods: [
                  { name: 'Supermarket', constructor: true, returns: null, visibility: '+', params: [{ name: 'checkouts', type: 'List<Checkout>' }] },
                  { name: 'arrive', constructor: false, returns: 'int', visibility: '+', params: customer },
                  { name: 'averageWait', constructor: false, returns: 'double', visibility: '+', params: [] },
              ] },
        ],
        associations: [
            { name: 'checkouts', from: 'Supermarket', to: 'Checkout', fromMult: '1', toMult: '1..*', directed: false, line: 25 },
        ],
    });
    // an abstract class and a directed association
    const q = core.summary(core.parse(QUEUE));
    assert.deepEqual([q.errors, q.warnings], [[], []]);
    assert.deepEqual(q.classes.map((c) => [c.name, c.abstract, c.extends, c.line]),
        [['Checkout', true, null, 1], ['SelfCheckout', false, 'Checkout', 4], ['Customer', false, null, 6]]);
    assert.deepEqual(q.associations,
        [{ name: 'queue', from: 'Checkout', to: 'Customer', fromMult: '1', toMult: '0..*', directed: true, line: 8 }]);
    // keys in the documented order
    assert.deepEqual(Object.keys(s), ['version', 'errors', 'warnings', 'classes', 'associations']);
    assert.deepEqual(Object.keys(s.classes[0]), ['name', 'abstract', 'extends', 'line', 'attributes', 'methods']);
    assert.deepEqual(Object.keys(s.classes[0].methods[0]), ['name', 'constructor', 'returns', 'visibility', 'params']);
});

test('problems in the summary: line, from, to, msg', () => {
    const s = core.summary(core.parse('class Customer\n  items: int\nclass Checkout\n  - next: Custmer\n'));
    assert.deepEqual(Object.keys(s.errors[0]), ['line', 'from', 'to', 'msg']);
    assert.deepEqual(where(s.errors[0]), [2, 2, 12]);
    assert.deepEqual(where(s.warnings[0]), [4, 10, 17]);
});

test('comments, blank lines and indentation', () => {
    const m = core.parse('# a supermarket\n\nclass Checkout   # a till\n\t- freeAt: int # the minute it is free again\n\n    + Checkout()\n  # name: int\n');
    assert.deepEqual(m.errors, []);
    const checkout = m.classes[0];
    assert.deepEqual(checkout.attributes.map((a) => a.name), ['freeAt']);
    assert.deepEqual(checkout.methods.map((x) => x.name), ['Checkout']);
    // »# name: int« is a comment, not a protected attribute
    assert.equal(checkout.attributes.length, 1);
});

test('class headers: extends, (abstract), both', () => {
    const m = core.parse('class A (abstract)\nclass B extends A (abstract)\nclass C extends B\nclass D  extends  C  ( abstract )\n');
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.classes.map((c) => [c.name, c.extends, c.abstract]),
        [['A', null, true], ['B', 'A', true], ['C', 'B', false], ['D', 'C', true]]);
});

test('a method may end with ? or ! as in Ruby, an attribute may not', () => {
    const m = core.parse('class Checkout\n  + free?(): boolean\n  + serve_next(): void\n  + reset!(): void\n');
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.classes[0].methods.map((x) => x.name), ['free?', 'serve_next', 'reset!']);
    assert.notDeepEqual(core.parse('class Checkout\n  - busy?: boolean\n').errors, []);
});

test('types as in Java, spaces normalised', () => {
    const m = core.parse(`class Supermarket
  - queue: List<Customer>
  - byName: Map<String,Checkout>
  - grid: int[][]
  - nested:Map< String , List<Customer> >
  - open: Set<Checkout>
  - prices: double[]
  - letter: char
  - count: long
  - ok: Boolean
  + find( name :String , n:int ): List<Checkout>
class Checkout
class Customer
`);
    assert.deepEqual(m.errors, []);
    assert.deepEqual(m.warnings, []);
    assert.deepEqual(m.classes[0].attributes.map((a) => a.type),
        ['List<Customer>', 'Map<String, Checkout>', 'int[][]', 'Map<String, List<Customer>>', 'Set<Checkout>', 'double[]', 'char', 'long', 'Boolean']);
    const find = m.classes[0].methods[0];
    assert.deepEqual(find.params, [{ name: 'name', type: 'String' }, { name: 'n', type: 'int' }]);
    assert.equal(find.returns, 'List<Checkout>');
});

test('constructors are named like their class, without return type', () => {
    const m = core.parse('class Customer\n  + Customer()\n  + Customer(items: int)\n  - Customer(items: int, arrival: int)\n');
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
    const errors = errorsOf('class Supermarket\nclass Checkout\nassociation checkouts: Supermarkt 1 -- 1..* Chekout\n');
    assert.equal(errors.length, 2);
    assert.deepEqual(where(errors[0]), [3, 23, 33]);
    assert.match(errors[0].msg, /»Supermarkt« ist keine Klasse dieser Datei\. Meinst du »Supermarket«\?/);
    assert.deepEqual(where(errors[1]), [3, 44, 51]);
    assert.match(errors[1].msg, /Meinst du »Checkout«\?/);
    const w = only(warningsOf('class A\nclass B\nassociation x: A 1 -- 1 B\nassociation x: B 1 -- 1 A\n'));
    assert.deepEqual(where(w), [4, 12, 13]);
    assert.match(w.msg, /schon die Assoziation in Zeile 3/);
});

test('members need + or -', () => {
    let p = only(errorsOf('class Customer\n  items: int\n'));
    assert.deepEqual(where(p), [2, 2, 12]);
    assert.match(p.msg, /Sichtbarkeit: »\+« \(public\) oder »-« \(private\), zum Beispiel »- items: int«/);
    p = only(errorsOf('class Supermarket\n  averageWait(): double\n'));
    assert.match(p.msg, /»- averageWait\(\): double«/);
    p = only(errorsOf('class Customer\n  private items: int\n'));
    assert.deepEqual(where(p), [2, 2, 9]);
    assert.match(p.msg, /Statt »private« schreibst du »-«/);
    p = only(errorsOf('class Supermarket\n  public averageWait(): double\n'));
    assert.match(p.msg, /Statt »public« schreibst du »\+«/);
    p = only(errorsOf('class Customer\n  protected items: int\n'));
    assert.match(p.msg, /»protected« gibt es hier nicht/);
    p = only(errorsOf('class Customer\n  ~ items: int\n'));
    assert.deepEqual(where(p), [2, 2, 3]);
    assert.match(p.msg, /»~« \(package\)/);
});

test('methods: return type required, constructors without one', () => {
    let p = only(errorsOf('class Supermarket\n  + averageWait()\n'));
    assert.deepEqual(where(p), [2, 4, 17]);
    assert.match(p.msg, /braucht einen Rückgabetyp, »void«, wenn sie nichts zurückgibt/);
    p = only(errorsOf('class Supermarket\n  + supermarket(checkouts: int)\n'));
    assert.match(p.msg, /Ein Konstruktor heißt genau wie die Klasse: »Supermarket«/);
    p = only(errorsOf('class Customer\n  + Customer(items: int): void\n'));
    assert.deepEqual(where(p), [2, 24, 30]);
    assert.match(p.msg, /Ein Konstruktor hat keinen Rückgabetyp/);
    p = only(errorsOf('class Supermarket\n  + averageWait():\n'));
    assert.match(p.msg, /fehlt der Rückgabetyp/);
    p = only(errorsOf('class Supermarket\n  + double averageWait()\n'));
    assert.match(p.msg, /Rückgabetyp steht in UML hinten: »\+ averageWait\(…\): double«/);
    p = only(errorsOf('class Supermarket\n  + averageWait(: double\n'));
    assert.match(p.msg, /schließende Klammer/);
    p = only(errorsOf('class Supermarket\n  + averageWait)(: double\n'));
    assert.match(p.msg, /So sieht eine Methode aus: »\+ averageWait\(\): double«, mit Parametern »\+ serve\(customer: Customer\): int«; ein Konstruktor heißt wie die Klasse und hat keinen Rückgabetyp: »\+ Customer\(items: int, arrival: int\)«/);
});

test('parameters: name: Type, separated by commas', () => {
    let p = only(errorsOf('class Checkout\n  + f(a): void\n'));
    assert.deepEqual(where(p), [2, 6, 7]);
    assert.match(p.msg, /Der Parameter »a« braucht einen Typ/);
    p = only(errorsOf('class Checkout\n  + f(Customer customer): void\nclass Customer\n'));
    assert.deepEqual(where(p), [2, 6, 23]);
    assert.match(p.msg, /Erst der Name, dann ein Doppelpunkt und der Typ: »customer: Customer«/);
    p = only(errorsOf('class Checkout\n  + f(a: int,): void\n'));
    assert.match(p.msg, /Hier fehlt ein Parameter\. Mehrere Parameter trennst du mit Kommas: »items: int, arrival: int«/);
    p = only(errorsOf('class Checkout\n  + f(a: int, a: int): void\n'));
    assert.match(p.msg, /Den Parameter »a« gibt es in dieser Methode schon/);
    p = only(errorsOf('class Checkout\n  + f(a:): void\n'));
    assert.match(p.msg, /fehlt der Typ des Parameters »a«/);
    p = only(errorsOf('class Checkout\n  + f(a = 1): void\n'));
    assert.match(p.msg, /So sieht ein Parameter aus: »items: int«/);
});

test('attributes: a type after a colon, no semicolon', () => {
    let p = only(errorsOf('class Customer\n  - items\n'));
    assert.match(p.msg, /Das Attribut »items« braucht einen Typ, zum Beispiel »- items: String«/);
    p = only(errorsOf('class Customer\n  - int items\n'));
    assert.match(p.msg, /»- items: int«/);
    p = only(errorsOf('class Customer\n  - items:\n'));
    assert.match(p.msg, /Nach dem Doppelpunkt fehlt der Typ/);
    p = only(errorsOf('class Customer\n  - items: int;\n'));
    assert.deepEqual(where(p), [2, 14, 15]);
    assert.match(p.msg, /kein Semikolon/);
    p = only(errorsOf('class Customer\n  - items: List<int\n'));
    assert.deepEqual(where(p), [2, 11, 19]);
    assert.match(p.msg, /»List<int« ist kein Typ\. Typen schreibst du wie in Java, zum Beispiel int, double, boolean, String, List<Checkout> oder int\[\]/);
    p = only(errorsOf('class Customer\n  - price: double = 3\n'));
    assert.match(p.msg, /ist kein Typ/);
    p = only(errorsOf('class Customer\n  -\n'));
    assert.match(p.msg, /fehlt das Attribut oder die Methode, zum Beispiel »- items: int«/);
    p = only(errorsOf('class Customer\n  - 3 + 4\n'));
    assert.match(p.msg, /So sieht ein Attribut aus: »- items: int«, eine Methode: »\+ averageWait\(\): double«/);
});

test('void only as a return type', () => {
    let p = only(errorsOf('class Customer\n  - x: void\n'));
    assert.deepEqual(where(p), [2, 7, 11]);
    assert.match(p.msg, /»void« gibt es nur als Rückgabetyp/);
    p = only(errorsOf('class Customer\n  + f(a: void): int\n'));
    assert.deepEqual(where(p), [2, 9, 13]);
    p = only(errorsOf('class Customer\n  + f(): List<void>\n'));
    assert.match(p.msg, /»void« gibt es nur als Rückgabetyp/);
    p = only(errorsOf('class Customer\n  + f(): void[]\n'));
    assert.match(p.msg, /»void«/);
});

test('members declared twice', () => {
    let p = only(errorsOf('class Customer\n  - items: int\n  + items: String\n'));
    assert.deepEqual(where(p), [3, 4, 9]);
    assert.match(p.msg, /Das Attribut »items« gibt es in »Customer« schon \(Zeile 2\)/);
    p = only(errorsOf('class Customer\n  + f(a: int): void\n  - f(b: int): int\n'));
    assert.match(p.msg, /Die Methode »f\(int\)« gibt es in »Customer« schon \(Zeile 2\)/);
    // overloading with other parameter types is fine
    assert.deepEqual(errorsOf('class Customer\n  + f(a: int): void\n  + f(a: String): void\n  + f(): void\n'), []);
    p = only(errorsOf('class Customer\n  + Customer(a: int)\n  + Customer(b: int)\n'));
    assert.match(p.msg, /Den Konstruktor »Customer\(int\)« gibt es in »Customer« schon/);
    // an attribute and a method may share a name
    assert.deepEqual(errorsOf('class Customer\n  - items: int\n  + items(): int\n'), []);
});

test('classes: twice, extends unknown, cycles, lowercase names', () => {
    let p = only(errorsOf('class Checkout\nclass Checkout\n'));
    assert.deepEqual(where(p), [2, 6, 14]);
    assert.match(p.msg, /Die Klasse »Checkout« gibt es schon \(Zeile 1\)/);
    p = only(errorsOf('class Checkout\nclass ExpressCheckout extends Chekout\n'));
    assert.deepEqual(where(p), [2, 30, 37]);
    assert.match(p.msg, /»Chekout« ist keine Klasse dieser Datei\. Meinst du »Checkout«\?/);
    p = only(errorsOf('class A extends A\n'));
    assert.match(p.msg, /»A« kann nicht von sich selbst erben/);
    const cycle = errorsOf('class A extends C\nclass B extends A\nclass C extends B\nclass D extends A\n');
    assert.equal(cycle.length, 3);
    assert.deepEqual(cycle.map(where), [[1, 16, 17], [2, 16, 17], [3, 16, 17]]);
    assert.match(cycle[0].msg, /Kreis: A → C → B → A/);
    const w = only(warningsOf('class checkout\n'));
    assert.deepEqual(where(w), [1, 6, 14]);
    assert.match(w.msg, /Klassennamen schreibt man groß: »Checkout«/);
});

test('unknown types are warnings that ask', () => {
    let w = only(warningsOf('class Customer\n  - name: Strin\n'));
    assert.deepEqual(where(w), [2, 10, 15]);
    assert.match(w.msg, /»Strin« ist weder ein Java-Typ noch eine Klasse dieser Datei\. Fehlt die Klasse noch, oder ist der Name falsch geschrieben\? Meinst du »String«\?/);
    w = only(warningsOf('class Customer\n  - basket: Basket\n'));
    assert.doesNotMatch(w.msg, /Meinst du/);
    w = only(warningsOf('class Customer\n  - name: string\n'));
    assert.match(w.msg, /Meinst du »String«\?/);
    w = only(warningsOf('class Checkout\n  + f(): List<Chekout>\n'));
    assert.deepEqual(where(w), [2, 14, 21]);
    w = only(warningsOf('class Customer\n  - counts: List<int>\n'));
    assert.match(w.msg, /»Integer« statt »int«/);
    w = only(warningsOf('class Supermarket\n  - byName: Map<String>\n'));
    assert.match(w.msg, /braucht zwei Typen in spitzen Klammern, zum Beispiel »Map<String, Checkout>«/);
    w = only(warningsOf('class Supermarket\n  - all: List\n'));
    assert.match(w.msg, /»List<Checkout>«/);
    w = only(warningsOf('class Customer\n  - x: String<Checkout>\nclass Checkout\n'));
    assert.match(w.msg, /keine Typen in spitzen Klammern/);
    // a class later in the file is known
    assert.deepEqual(warningsOf('class Supermarket\n  - open: Checkout[]\nclass Checkout\n'), []);
});

test('lines that match nothing say what was expected', () => {
    const cases = [
        ['Class Checkout', /klein/],
        ['klasse Checkout', /englisch/],
        ['relationship x: A 1 -- 1 B', /heißt eine Beziehung »association«/],
        ['abstract class Checkout', /»class Checkout \(abstract\)«/],
        ['interface Payable', /»interface« gibt es in der UML-Werkstatt nicht\. Erwartet wird »class Name« oder »association checkouts: Supermarket 1 -- 1\.\.\* Checkout«/],
        ['- items: int', /eingerückt unter ihrer Klasse, zum Beispiel mit zwei Leerzeichen vor »- items: int«/],
        ['public class Checkout', /»public« steht hier nicht vor »class«/],
        ['class', /Nach »class« kommt der Name der Klasse, zum Beispiel »class Checkout«/],
        ['class ExpressCheckout (abstract) extends Checkout', /steht am Ende der Zeile: »class ExpressCheckout extends Checkout \(abstract\)«/],
        ['class Checkout abstract', /in Klammern am Ende/],
        ['class ExpressCheckout implements Payable', /»implements« gibt es hier nicht, nur »extends«: »class ExpressCheckout extends Checkout«/],
        // the example never lets a class extend itself
        ['class Checkout implements Payable', /»class Checkout extends Oberklasse«/],
        ['class ExpressCheckout extends', /Nach »extends« kommt der Name der Oberklasse/],
        ['class ExpressCheckout extends A, B', /genau einer Oberklasse/],
        ['class ExpressCheckout : Checkout', /mit »extends«/],
        ['class ExpressCheckout(Checkout)', /mit »extends«/],
        ['class 1Checkout', /ein Wort ohne Leerzeichen, das mit einem Buchstaben beginnt, zum Beispiel »class Checkout«/],
        ['class Express Checkout', /ein Wort ohne Leerzeichen/],
        ['hello world', /Unbekannte Zeile/],
    ];
    for (const [line, re] of cases) {
        const p = only(errorsOf(`${line}\n`));
        assert.deepEqual(where(p), [1, 0, line.length], line);
        assert.match(p.msg, re, line);
    }
});

test('indented lines outside a class', () => {
    let p = only(errorsOf('  - items: int\nclass Customer\n'));
    assert.deepEqual(where(p), [1, 2, 14]);
    assert.match(p.msg, /gehört zu keiner Klasse/);
    p = only(errorsOf('class A\nclass B\nassociation x: A 1 -- 1 B\n  - items: int\n'));
    assert.match(p.msg, /Unter einer Assoziation steht nichts eingerückt/);
    // under a broken header: only the header is wrong
    p = only(errorsOf('class ExpressCheckout : Checkout\n  - items: int\n  + ExpressCheckout()\n'));
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
    const { svg, graph } = core.diagram(SUPERMARKET);
    for (const name of ['Customer', 'Checkout', 'ExpressCheckout', 'SelfCheckout', 'Supermarket']) assert.match(svg, new RegExp(`<g class="cls" data-class="${name}"`));
    assert.equal((svg.match(/<rect class="cbox"/g) || []).length, 5);
    // two separators per box: attributes, methods
    assert.equal((svg.match(/<line class="csep"/g) || []).length, 10);
    assert.equal((svg.match(/<polygon class="cinherit"/g) || []).length, 2);
    assert.equal((svg.match(/<g class="edge inherit" data-from="ExpressCheckout" data-to="Checkout">/g) || []).length, 1);
    assert.equal((svg.match(/<g class="edge inherit" data-from="SelfCheckout" data-to="Checkout">/g) || []).length, 1);
    assert.equal((svg.match(/<path class="carrow"/g) || []).length, 0);
    assert.match(svg, /<g class="edge assoc" data-from="Supermarket" data-to="Checkout" data-name="checkouts">/);
    assert.doesNotMatch(svg, /«abstract»/);
    assert.match(svg, />- items: int</);
    assert.match(svg, />\+ Customer\(items: int, arrival: int\)</);
    assert.match(svg, />\+ Supermarket\(checkouts: List&lt;Checkout&gt;\)</);
    assert.match(svg, />\+ averageWait\(\): double</);
    assert.match(svg, /<text class="cmult"[^>]*>1\.\.\*</);
    assert.match(svg, /<text class="clbl"[^>]*>checkouts</);
    // styles only for the diagram
    for (const rule of svg.match(/<style>([\s\S]*)<\/style>/)[1].split('}').filter((r) => r.trim())) {
        assert.match(rule.trim(), /^\.cd-diagram /);
    }
    assert.equal(graph.nodes.length, 5);
    // an abstract class, and an open arrowhead for a directed association
    const q = core.diagram(QUEUE).svg;
    assert.match(q, />«abstract»</);
    assert.match(q, /<text class="cname abs"[^>]*>Checkout</);
    assert.equal((q.match(/<path class="carrow"/g) || []).length, 1);
    assert.match(q, /<g class="edge assoc directed" data-from="Checkout" data-to="Customer" data-name="queue">/);
    assert.match(q, /<text class="cmult"[^>]*>0\.\.\*</);
});

test('the svg escapes generics and names', () => {
    const { svg } = core.diagram('class Supermarket\n  - byName: Map<String, Checkout>\nclass Checkout\n');
    assert.match(svg, />- byName: Map&lt;String, Checkout&gt;</);
    assert.doesNotMatch(svg, /Map<String/);
});

test('layout: superclasses above, associated classes beside, no overlaps', () => {
    for (const text of [SUPERMARKET, QUEUE]) {
        const { graph } = core.diagram(text);
        const by = Object.fromEntries(graph.nodes.map((n) => [n.name, n]));
        assert.ok(by.Checkout.y < by.SelfCheckout.y);
        assert.ok(rect(by.Checkout).y1 < rect(by.SelfCheckout).y0);
        for (const a of graph.nodes) for (const b of graph.nodes) if (a !== b) assert.ok(!overlap(rect(a), rect(b), 10), `${a.name} ${b.name}`);
    }
    const by = Object.fromEntries(core.diagram(SUPERMARKET).graph.nodes.map((n) => [n.name, n]));
    assert.equal(by.ExpressCheckout.y, by.SelfCheckout.y);
    // Supermarket only associated: in the layer of Checkout, beside it
    assert.equal(by.Supermarket.y, by.Checkout.y);
    // in the queue Customer only associated, too
    const q = Object.fromEntries(core.diagram(QUEUE).graph.nodes.map((n) => [n.name, n]));
    assert.equal(q.Customer.y, q.Checkout.y);
});

test('layout: a class without lines comes last, in a row of its own below', () => {
    const { graph } = core.diagram(SUPERMARKET);
    const by = Object.fromEntries(graph.nodes.map((n) => [n.name, n]));
    // Customer has no lines: not between the subclasses, below every other box, centred
    const others = graph.nodes.filter((n) => n.name !== 'Customer');
    for (const n of others) assert.ok(rect(n).y1 < rect(by.Customer).y0, n.name);
    const x0 = Math.min(...others.map((n) => rect(n).x0));
    const x1 = Math.max(...others.map((n) => rect(n).x1));
    assert.ok(Math.abs(by.Customer.x - (x0 + x1) / 2) < 1);
    // the same layout as without Customer
    const without = core.diagram(SUPERMARKET.replace(/class Customer\n(  .*\n)*/, '')).graph.nodes;
    const dx = by.Checkout.x - without.find((n) => n.name === 'Checkout').x;
    for (const n of without) assert.ok(Math.abs(by[n.name].x - n.x - dx) < 0.2 && by[n.name].y === n.y, n.name);
    // only classes without lines: one row, a new one when it gets too wide
    const lone = core.diagram('class A\nclass B\nclass C\nclass D\nclass E\nclass F\nclass G\nclass H\n').graph.nodes;
    assert.deepEqual([...new Set(lone.map((n) => n.y))].length, 2);
    assert.deepEqual(lone.filter((n) => n.y === lone[0].y).map((n) => n.name), ['A', 'B', 'C', 'D', 'E']);
    const two = core.diagram('class Customer\nclass Checkout\n').graph.nodes;
    assert.equal(two[0].y, two[1].y);
    for (const a of lone) for (const b of lone) if (a !== b) assert.ok(!overlap(rect(a), rect(b), 10), `${a.name} ${b.name}`);
});

test('layout: subclasses together, centred under their class', () => {
    const { graph } = core.diagram(SUPERMARKET);
    const by = Object.fromEntries(graph.nodes.map((n) => [n.name, n]));
    assert.ok(Math.abs((by.ExpressCheckout.x + by.SelfCheckout.x) / 2 - by.Checkout.x) < 1);
    // each inheritance line ends on the bottom edge of Checkout, not at its corner
    const tips = core.geometry(graph).lines.filter((l) => l.triangle).map((l) => l.triangle.match(/-?[\d.]+/g).map(Number));
    assert.equal(tips.length, 2);
    const r = rect(by.Checkout);
    for (const [tx, ty] of tips) {
        assert.ok(Math.abs(ty - r.y1) < 0.2, 'on the bottom edge');
        assert.ok(tx > r.x0 + 20 && tx < r.x1 - 20, 'away from the corners');
    }
    // in the zoo: no other box between two subclasses of one class
    const zoo = core.diagram(ZOO).graph.nodes;
    const z = Object.fromEntries(zoo.map((n) => [n.name, n]));
    for (const [a, b] of [['Mammal', 'Bird'], ['Lion', 'Elephant']]) {
        assert.equal(z[a].y, z[b].y);
        const lo = Math.min(z[a].x, z[b].x);
        const hi = Math.max(z[a].x, z[b].x);
        for (const n of zoo) if (n.y === z[a].y && n !== z[a] && n !== z[b]) assert.ok(n.x < lo || n.x > hi, `${n.name} between ${a} and ${b}`);
    }
    assert.ok(Math.abs((z.Lion.x + z.Elephant.x) / 2 - z.Mammal.x) < 1);
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
    for (const text of [SUPERMARKET, QUEUE]) {
        const { graph } = core.diagram(text);
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
    const file = path.join(dir, 'supermarkt.uml');
    fs.writeFileSync(file, SUPERMARKET);
    const script = path.join(__dirname, 'uml-core.js');
    const json = JSON.parse(execFileSync('node', [script, '--json', file], { encoding: 'utf8' }));
    assert.deepEqual(json, core.summary(core.parse(SUPERMARKET)));
    const svg = execFileSync('node', [script, '--svg', file], { encoding: 'utf8' });
    assert.match(svg, /^<svg [^>]*class="cd-diagram"/);
    assert.match(svg, /<rect x="[-\d]+" y="[-\d]+" width="\d+" height="\d+" fill="#ffffff"\/>/);
    const missing = JSON.parse(execFileSync('node', [script, '--json', path.join(dir, 'nope.uml')], { encoding: 'utf8' }));
    assert.deepEqual(missing, { version: core.VERSION, missing: true, errors: [{ line: 0, msg: `Die Datei ${path.join(dir, 'nope.uml')} gibt es nicht.` }] });
    const broken = path.join(dir, 'broken.uml');
    fs.writeFileSync(broken, 'class Customer\n  items: int\n');
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
