// UML-Werkstatt core: a UML class diagram written as text (.uml),
// checked while typing, summarised as JSON for the checks and drawn as SVG.
//
// One module for every place that shows or checks a diagram: the VS Code
// extension (hackschule-uml), the website and the Workspace checks.
// Plain JavaScript without dependencies; no DOM. Keywords and identifiers
// are English, messages for students German.
//
//   # comments start with #
//   class Customer
//     - items: int
//     + Customer(items: int, arrival: int)    constructor: named like its class
//
//   class Checkout                    abstract: class Checkout (abstract)
//     + serve(customer: Customer): int
//
//   class ExpressCheckout extends Checkout
//     + accepts(customer: Customer): boolean
//
//   class Supermarket
//     + averageWait(): double
//
//   association checkouts: Supermarket 1 -- 1..* Checkout    undirected
//   association queue: Checkout 1 -> 0..* Customer           navigable from Checkout
//
// Command line (used by the Workspace checks and for exports):
//   node uml-core.js --json supermarkt.uml      classes, associations and problems as JSON
//   node uml-core.js --svg supermarkt.uml       the diagram as SVG, on white
(function (root, factory) {
    if (typeof module === 'object' && module.exports) module.exports = factory();
    else root.UmlCore = factory();
})(typeof self !== 'undefined' ? self : this, function () {
    'use strict';

    const VERSION = '0.1.0';
    const NAME = '\\p{L}[\\p{L}\\p{N}_]*';
    // A method may end with »?« or »!«, as in Ruby (available?, sort!):
    // the diagram uses the names of the code.
    const METHOD_NAME = `${NAME}[?!]?`;
    const RE_CLASS = new RegExp(`^class\\s+(${NAME})(?:\\s+extends\\s+(${NAME}))?(?:\\s*\\(\\s*(abstract)\\s*\\))?\\s*$`, 'du');
    // Loose on purpose: multiplicities and the arrow are checked one by one,
    // so a wrong one gets a message of its own.
    const MULT = '[\\p{L}\\p{N}_.*]+';
    const ARROW = '<->|<-|->|--|=>|[-–—]';
    const RE_ASSOC = new RegExp(`^association\\s+(${NAME})\\s*:\\s*(${NAME})(?:\\s+(${MULT}))?\\s*(${ARROW})\\s*(?:(${MULT})\\s+)?(${NAME})\\s*$`, 'du');
    const RE_METHOD = new RegExp(`^\\s*(${METHOD_NAME})\\s*\\((.*)\\)\\s*(?::\\s*(.*?))?\\s*$`, 'du');
    const RE_ATTRIBUTE = new RegExp(`^\\s*(${NAME})\\s*:\\s*(.*?)\\s*$`, 'du');
    const RE_PARAM = new RegExp(`^(${NAME})\\s*:\\s*(.*?)\\s*$`, 'du');
    const RE_ONE_NAME = new RegExp(`^(${NAME})$`, 'u');

    // Types as Java writes them. Generic containers with the number of types
    // in their angle brackets.
    const PRIMITIVES = ['int', 'double', 'long', 'float', 'boolean', 'char', 'byte', 'short'];
    const WRAPPERS = { int: 'Integer', double: 'Double', long: 'Long', float: 'Float', boolean: 'Boolean', char: 'Character', byte: 'Byte', short: 'Short' };
    const BUILTIN = ['String', 'Object', ...Object.values(WRAPPERS)];
    const GENERIC = { List: 1, ArrayList: 1, LinkedList: 1, Set: 1, HashSet: 1, TreeSet: 1, Queue: 1, Deque: 1, ArrayDeque: 1, Stack: 1, Map: 2, HashMap: 2, TreeMap: 2 };
    const TYPE_EXAMPLES = 'int, double, boolean, String, List<Checkout> oder int[]';
    const MULT_LIST = '1, *, 0..1, 0..*, 1..*, eine Zahl wie 3 oder ein Bereich wie 2..4';
    const EXAMPLE_ASSOC = '»association checkouts: Supermarket 1 -- 1..* Checkout«';

    // ---------- reading ----------------------------------------------------------

    // Problems carry line and column range (1-based line, 0-based columns) so an
    // editor can underline them.
    function problem(line, msg, raw, from, to) {
        const start = from === undefined ? (raw ? raw.search(/\S|$/) : 0) : from;
        const end = to === undefined ? (raw ? raw.replace(/\s+$/, '').length : start) : to;
        return { line, from: start, to: Math.max(end, start + 1), msg };
    }

    // Edit distance, for »Meinst du …?«
    function distance(a, b) {
        const d = Array.from({ length: b.length + 1 }, (_, j) => j);
        for (let i = 1; i <= a.length; i++) {
            let prev = d[0];
            d[0] = i;
            for (let j = 1; j <= b.length; j++) {
                const keep = d[j];
                d[j] = Math.min(d[j] + 1, d[j - 1] + 1, prev + (a[i - 1] === b[j - 1] ? 0 : 1));
                prev = keep;
            }
        }
        return d[b.length];
    }

    function closest(name, candidates) {
        const lower = name.toLowerCase();
        const same = candidates.find((c) => c.toLowerCase() === lower);
        if (same) return same;
        let best = null;
        let bestD = name.length <= 4 ? 2 : 3;
        for (const c of candidates) {
            const dd = distance(lower, c.toLowerCase());
            if (dd < bestD) { best = c; bestD = dd; }
        }
        return best;
    }

    const suggest = (name, candidates) => {
        const c = closest(name, candidates);
        return c && c !== name ? ` Meinst du »${c}«?` : '';
    };

    // A type as Java writes it: Name, Name<T, U>, T[], nested. text starts at
    // column at. Returns { node } with node = { name, from, to, generic, args, dims },
    // or null when it is no type at all.
    function readType(text, at) {
        const toks = [];
        const re = /\s+|\p{L}[\p{L}\p{N}_]*|[<>,[\]]|./uy;
        for (let i = 0; i < text.length;) {
            re.lastIndex = i;
            const v = re.exec(text)[0];
            if (!/^\s/.test(v)) {
                const kind = /^\p{L}/u.test(v) ? 'name' : /^[<>,[\]]$/.test(v) ? v : 'bad';
                toks.push({ v, kind, from: at + i, to: at + i + v.length });
            }
            i += v.length;
        }
        const FAIL = {};
        let k = 0;
        const one = () => {
            const t = toks[k++];
            if (!t || t.kind !== 'name') throw FAIL;
            const node = { name: t.v, from: t.from, to: t.to, generic: false, args: [], dims: 0 };
            if (toks[k] && toks[k].kind === '<') {
                k++;
                node.generic = true;
                for (;;) {
                    node.args.push(one());
                    const sep = toks[k++];
                    if (sep && sep.kind === ',') continue;
                    if (sep && sep.kind === '>') break;
                    throw FAIL;
                }
            }
            while (toks[k] && toks[k].kind === '[') {
                k++;
                if (!toks[k] || toks[k].kind !== ']') throw FAIL;
                k++;
                node.dims++;
            }
            return node;
        };
        try {
            const node = one();
            if (k !== toks.length) return null;
            return node;
        } catch (e) {
            if (e !== FAIL) throw e;
            return null;
        }
    }

    // The type as text, spaces normalised: Map<String, Checkout>, int[]
    function typeText(node) {
        return node.name + (node.generic ? `<${node.args.map(typeText).join(', ')}>` : '') + '[]'.repeat(node.dims);
    }

    // Splits at commas outside angle brackets; parts keep their offset.
    function splitList(s) {
        const parts = [];
        let depth = 0;
        let start = 0;
        for (let i = 0; i < s.length; i++) {
            const ch = s[i];
            if (ch === '<') depth++;
            else if (ch === '>') depth = Math.max(0, depth - 1);
            else if (ch === ',' && depth === 0) {
                parts.push({ text: s.slice(start, i), from: start, comma: i });
                start = i + 1;
            }
        }
        parts.push({ text: s.slice(start), from: start, comma: -1 });
        return parts;
    }

    // A message for a top-level line that is neither a class nor an association.
    function headerHint(s) {
        let m;
        if (/^(Class|CLASS|Association|ASSOCIATION)\b/.test(s) || /^class\b.*\s(Extends|EXTENDS)\b/.test(s)) {
            return 'Schlüsselwörter schreibst du klein: »class«, »extends«, »association«.';
        }
        if (/^(klasse|assoziation|beziehung|vererbung|erbt)\b/i.test(s) || /^class\b.*\s(erbt|von)\s/.test(s)) {
            return 'Die Schlüsselwörter sind englisch: »class«, »extends« und »association«.';
        }
        if (/^relationship\b/.test(s)) return `Im Klassendiagramm heißt eine Beziehung »association«: ${EXAMPLE_ASSOC}.`;
        if ((m = s.match(new RegExp(`^abstract\\s+class\\s+(${NAME})`, 'u')))) {
            return `»(abstract)« steht in Klammern hinter dem Namen: »class ${m[1]} (abstract)«.`;
        }
        if ((m = s.match(/^(interface|enum|record|struct|object)\b/))) {
            return `»${m[1]}« gibt es in der UML-Werkstatt nicht. Erwartet wird »class Name« oder ${EXAMPLE_ASSOC}.`;
        }
        if ((m = s.match(new RegExp(`^(public|private|protected|final|static)\\s+(?:abstract\\s+)?class\\s+(${NAME})`, 'u')))) {
            return `»${m[1]}« steht hier nicht vor »class«: »class ${m[2]}«.`;
        }
        if (/^[+\-~]/.test(s) || /^(public|private|protected)\b/.test(s)) {
            return 'Attribute und Methoden stehen eingerückt unter ihrer Klasse, zum Beispiel mit zwei Leerzeichen vor »- items: int«.';
        }
        if (/^class\b/.test(s)) {
            const name = (s.match(new RegExp(`^class\\s+(${NAME})`, 'u')) || [])[1] || 'ExpressCheckout';
            // the superclass of the examples, never the class itself
            const sup = name === 'Checkout' ? 'Oberklasse' : 'Checkout';
            if (/^class\s*$/.test(s)) return 'Nach »class« kommt der Name der Klasse, zum Beispiel »class Checkout«.';
            if (/\(\s*abstract\s*\)\s*\S/.test(s)) return `»(abstract)« steht am Ende der Zeile: »class ${name} extends ${sup} (abstract)«.`;
            if (/\babstract\b/.test(s) && !/\(\s*abstract\s*\)/.test(s)) return `»abstract« steht in Klammern am Ende: »class ${name} (abstract)«.`;
            if (/\bimplements\b/.test(s)) return `»implements« gibt es hier nicht, nur »extends«: »class ${name} extends ${sup}«.`;
            if (new RegExp(`^class\\s+${NAME}\\s+extends\\s*(\\(\\s*abstract\\s*\\))?\\s*$`, 'u').test(s)) {
                return `Nach »extends« kommt der Name der Oberklasse: »class ${name} extends ${sup}«.`;
            }
            if (new RegExp(`^class\\s+${NAME}\\s+extends\\s+${NAME}\\s*(,|\\s${NAME}|&)`, 'u').test(s)) {
                return `Eine Klasse erbt von genau einer Oberklasse: »class ${name} extends ${sup}«.`;
            }
            if (new RegExp(`^class\\s+${NAME}\\s*(:|<|\\(|\\bextend\\b|\\binherits\\b)`, 'u').test(s)) {
                return `Die Oberklasse schreibst du mit »extends«: »class ${name} extends ${sup}«.`;
            }
            return 'Der Name einer Klasse ist ein Wort ohne Leerzeichen, das mit einem Buchstaben beginnt, zum Beispiel »class Checkout«. Danach können »extends Oberklasse« und »(abstract)« stehen.';
        }
        return `Unbekannte Zeile. Erwartet wird »class Name« oder ${EXAMPLE_ASSOC} (Schlüsselwörter klein und auf Englisch); Attribute und Methoden stehen eingerückt unter ihrer Klasse.`;
    }

    // A multiplicity, or the message why it is none.
    function multProblem(m) {
        if (/^(\*|\d+)$/.test(m)) return null;
        const range = m.match(/^(\d+)\.\.(\d+|\*)$/);
        if (range) {
            if (range[2] !== '*' && Number(range[1]) > Number(range[2])) return `Bei »${m}« ist die untere Grenze größer als die obere.`;
            return null;
        }
        if (/^[nmNM]$/.test(m)) return `Für »beliebig viele« schreibst du in UML »*« statt »${m}«, zum Beispiel »0..*«.`;
        const low = m.match(/^(\d+)\.\.[nmNM]$/);
        if (low) return `»${m}« schreibst du in UML als »${low[1]}..*«.`;
        return `»${m}« ist keine Multiplizität. Möglich sind ${MULT_LIST}.`;
    }

    // Returns { classes, associations, errors, warnings, byName }.
    // class:       { name, abstract, extends (or null), line, attributes, methods }
    // attribute:   { name, type, visibility, line }
    // method:      { name, constructor, returns (or null), visibility, params: [{ name, type }], line }
    // association: { name, from, to, fromMult, toMult, directed, line }
    function parse(text) {
        const classes = [];
        const associations = [];
        const errors = [];
        const warnings = [];
        // Types are checked at the end, when every class of the file is known.
        const pending = [];
        let current = null;
        let underAssociation = false;

        String(text).split('\n').forEach((raw0, i) => {
            const raw = raw0.replace(/\r$/, '');
            const line = i + 1;
            const code = raw.replace(/#.*$/, '');
            if (!code.trim()) return;
            const s = code.trim();
            const at = code.search(/\S/);
            const err = (msg, from, to) => errors.push(problem(line, msg, code, from, to));
            if (at === 0) {
                let m;
                if ((m = RE_CLASS.exec(s))) {
                    const [ni, ei] = [m.indices[1], m.indices[2]];
                    current = { name: m[1], abstract: !!m[3], extends: m[2] || null, line, attributes: [], methods: [],
                        nameRange: ni, extendsRange: ei || null };
                    classes.push(current);
                    underAssociation = false;
                } else if (/^association\b/.test(s)) {
                    current = null;
                    underAssociation = true;
                    const a = readAssociation(s, line, err);
                    if (a) associations.push(a);
                } else {
                    // Lines below a broken header are still read for their own
                    // mistakes, but belong to no class.
                    const name = (s.match(new RegExp(`^class\\s+(${NAME})`, 'u')) || [])[1] || null;
                    current = { broken: true, name, attributes: [], methods: [] };
                    underAssociation = false;
                    err(headerHint(s));
                }
                return;
            }
            if (!current) {
                err(underAssociation
                    ? 'Unter einer Assoziation steht nichts eingerückt. Attribute und Methoden gehören unter »class Name«.'
                    : 'Diese eingerückte Zeile gehört zu keiner Klasse. Attribute und Methoden stehen eingerückt unter »class Name«.');
                return;
            }
            const member = readMember(s, at, line, current, err, pending);
            if (!member || current.broken) return;
            if (member.kind === 'attribute') {
                const prev = current.attributes.find((x) => x.name === member.name);
                if (prev) {
                    err(`Das Attribut »${member.name}« gibt es in »${current.name}« schon (Zeile ${prev.line}).`, member.nameRange[0], member.nameRange[1]);
                    return;
                }
                current.attributes.push(member);
            } else {
                const key = (x) => `${x.name}(${x.params.map((p) => p.type).join(', ')})`;
                const prev = current.methods.find((x) => key(x) === key(member));
                if (prev) {
                    err(member.constructor
                        ? `Den Konstruktor »${key(member)}« gibt es in »${current.name}« schon (Zeile ${prev.line}).`
                        : `Die Methode »${key(member)}« gibt es in »${current.name}« schon (Zeile ${prev.line}).`, member.nameRange[0], member.nameRange[1]);
                    return;
                }
                current.methods.push(member);
            }
        });

        // ---- the file as a whole
        const byName = new Map();
        for (const c of classes) {
            const prev = byName.get(c.name);
            if (prev) {
                errors.push(problem(c.line, `Die Klasse »${c.name}« gibt es schon (Zeile ${prev.line}).`, null, c.nameRange[0], c.nameRange[1]));
                continue;
            }
            byName.set(c.name, c);
            if (/^\p{Ll}/u.test(c.name)) {
                warnings.push(problem(c.line, `Klassennamen schreibt man groß: »${c.name[0].toUpperCase()}${c.name.slice(1)}«.`, null, c.nameRange[0], c.nameRange[1]));
            }
        }
        const names = [...byName.keys()];
        for (const c of byName.values()) {
            if (!c.extends) continue;
            const [from, to] = c.extendsRange;
            if (c.extends === c.name) {
                errors.push(problem(c.line, `»${c.name}« kann nicht von sich selbst erben.`, null, from, to));
            } else if (!byName.has(c.extends)) {
                errors.push(problem(c.line, `»${c.extends}« ist keine Klasse dieser Datei.${suggest(c.extends, names.filter((n) => n !== c.name))}`, null, from, to));
            } else {
                // A cycle: following the superclasses comes back to c.
                const path = [c.name];
                let cur = byName.get(c.extends);
                for (let guard = 0; cur && guard <= byName.size; guard++) {
                    path.push(cur.name);
                    if (cur === c) {
                        errors.push(problem(c.line, `Die Vererbung bildet einen Kreis: ${path.join(' → ')}. Eine Klasse kann nicht über andere von sich selbst erben.`, null, from, to));
                        break;
                    }
                    cur = cur.extends ? byName.get(cur.extends) : null;
                }
            }
        }

        const known = new Set([...PRIMITIVES, ...BUILTIN, ...Object.keys(GENERIC), ...names]);
        const candidates = [...PRIMITIVES, ...BUILTIN, ...Object.keys(GENERIC), ...names];
        const checkType = (node, p, top, asReturn, inside) => {
            const where = [node.from, node.to];
            const warn = (msg) => warnings.push(problem(p.line, msg, null, where[0], where[1]));
            if (node.name === 'void') {
                if (!(top && asReturn && node.dims === 0 && !node.generic)) {
                    errors.push(problem(p.line, '»void« gibt es nur als Rückgabetyp einer Methode, die nichts zurückgibt.', null, where[0], where[1]));
                }
            } else if (!known.has(node.name)) {
                warn(`»${node.name}« ist weder ein Java-Typ noch eine Klasse dieser Datei. Fehlt die Klasse noch, oder ist der Name falsch geschrieben?${suggest(node.name, candidates)}`);
            } else if (GENERIC[node.name]) {
                const n = GENERIC[node.name];
                const example = n === 2 ? `${node.name}<String, Checkout>` : `${node.name}<Checkout>`;
                if (!node.generic) warn(`Schreib in spitzen Klammern dazu, was in »${node.name}« steht, zum Beispiel »${example}«.`);
                else if (node.args.length !== n) warn(`»${node.name}« braucht ${n === 2 ? 'zwei Typen' : 'einen Typ'} in spitzen Klammern, zum Beispiel »${example}«.`);
            } else if (node.generic) {
                warn(`»${node.name}« hat keine Typen in spitzen Klammern.`);
            }
            if (inside && node.dims === 0 && WRAPPERS[node.name]) {
                warn(`In spitzen Klammern steht in Java kein einfacher Typ: »${WRAPPERS[node.name]}« statt »${node.name}«.`);
            }
            for (const a of node.args) checkType(a, p, false, asReturn, true);
        };
        for (const p of pending) checkType(p.node, p, true, p.asReturn, false);

        const assocNames = new Map();
        for (const a of associations) {
            for (const end of ['from', 'to']) {
                if (!byName.has(a[end])) {
                    const [f, t] = a.ranges[end];
                    errors.push(problem(a.line, `»${a[end]}« ist keine Klasse dieser Datei.${suggest(a[end], names)}`, null, f, t));
                }
            }
            const prev = assocNames.get(a.name);
            if (prev) {
                warnings.push(problem(a.line, `Den Namen »${a.name}« hat schon die Assoziation in Zeile ${prev.line}. Gib jeder Assoziation einen eigenen Namen.`, null, a.ranges.name[0], a.ranges.name[1]));
            } else assocNames.set(a.name, a);
        }

        const order = (p, q) => p.line - q.line || p.from - q.from;
        errors.sort(order);
        warnings.sort(order);
        return { classes, associations, errors, warnings, byName };
    }

    // association name: A mult -- mult B
    function readAssociation(s, line, err) {
        const m = RE_ASSOC.exec(s);
        if (!m) {
            if (/^association\s+\S+\s+\S/.test(s) && !s.includes(':')) {
                err(`Eine Assoziation braucht einen Namen und dahinter einen Doppelpunkt: ${EXAMPLE_ASSOC}.`);
            } else if (/^association\s*:/.test(s) || /^association\s*$/.test(s)) {
                err(`Nach »association« kommt ihr Name: ${EXAMPLE_ASSOC}.`);
            } else {
                err(`So sieht eine Assoziation aus: ${EXAMPLE_ASSOC}; mit »->« statt »--«, wenn sie nur in eine Richtung navigierbar ist.`);
            }
            return null;
        }
        const [, name, from, multA, arrow, multB, to] = m;
        const I = m.indices;
        let ok = true;
        if (arrow !== '--' && arrow !== '->') {
            ok = false;
            const [f, t] = I[4];
            if (arrow === '<-') err(`Den Pfeil gibt es nur nach rechts. Schreib die Klassen andersherum: »${to} ${multB || '1'} -> ${multA || '1'} ${from}«.`, f, t);
            else if (arrow === '<->') err('Eine Assoziation, die in beide Richtungen navigierbar ist, schreibst du ohne Pfeil: »--«.', f, t);
            else err('Die Linie sind zwei Striche: »--« ohne Pfeil oder »->« mit Pfeil.', f, t);
        }
        if (!multA || !multB) {
            ok = false;
            err(`An beide Enden gehört eine Multiplizität: »${from} ${multA || '1'} ${arrow} ${multB || '0..*'} ${to}«. Möglich sind ${MULT_LIST}.`, I[2][0], I[6][1]);
        }
        for (const [mult, idx] of [[multA, 3], [multB, 5]]) {
            if (!mult) continue;
            const msg = multProblem(mult);
            if (msg) { ok = false; err(msg, I[idx][0], I[idx][1]); }
        }
        if (!ok) return null;
        return { name, from, to, fromMult: multA, toMult: multB, directed: arrow === '->', line,
            ranges: { name: I[1], from: I[2], to: I[6] } };
    }

    // One member line: s starts at column at. Returns the member, or null
    // after reporting what is wrong.
    function readMember(s, at, line, cls, err, pending) {
        let m;
        if (!/^[+-]/.test(s)) {
            if ((m = s.match(/^(public|private|protected)\b/))) {
                const word = m[1];
                if (word === 'protected') err('»protected« gibt es hier nicht. Schreib »+« (public) oder »-« (private) davor.', at, at + word.length);
                else err(`Statt »${word}« schreibst du »${word === 'public' ? '+' : '-'}«: »${word === 'public' ? '+' : '-'} ${s.slice(word.length).trim()}«.`, at, at + word.length);
            } else if (s[0] === '~') {
                err('»~« (package) gibt es hier nicht. Schreib »+« (public) oder »-« (private) davor.', at, at + 1);
            } else {
                err(`Vor jedes Attribut und jede Methode kommt die Sichtbarkeit: »+« (public) oder »-« (private), zum Beispiel »- ${s}«.`, at, at + s.length);
            }
            return null;
        }
        const visibility = s[0];
        let body = s.slice(1);
        const bodyAt = at + 1;
        const semi = body.search(/;\s*$/);
        if (semi >= 0) {
            err('Am Ende der Zeile steht hier kein Semikolon.', bodyAt + semi, bodyAt + semi + 1);
            body = body.slice(0, semi);
        }
        const abs = (range) => [bodyAt + range[0], bodyAt + range[1]];
        const typeAt = (text, from, asReturn, what) => {
            const lead = text.search(/\S|$/);
            const trimmed = text.trim();
            const node = readType(trimmed, from + lead);
            if (!node) {
                err(`»${trimmed}« ist kein Typ${what}. Typen schreibst du wie in Java, zum Beispiel ${TYPE_EXAMPLES}.`, from + lead, from + lead + trimmed.length);
                return null;
            }
            pending.push({ node, line, asReturn });
            return node;
        };

        if (body.includes('(')) {
            m = RE_METHOD.exec(body);
            if (!m) {
                let j;
                if ((j = body.match(new RegExp(`^\\s*(\\S+)\\s+(${NAME})\\s*\\(`, 'u')))) {
                    err(`Der Rückgabetyp steht in UML hinten: »${visibility} ${j[2]}(…): ${j[1]}«.`);
                } else if (!body.includes(')')) {
                    err('Nach den Parametern fehlt die schließende Klammer »)«.');
                } else {
                    err('So sieht eine Methode aus: »+ averageWait(): double«, mit Parametern »+ serve(customer: Customer): int«; ein Konstruktor heißt wie die Klasse und hat keinen Rückgabetyp: »+ Customer(items: int, arrival: int)«.');
                }
                return null;
            }
            const name = m[1];
            const nameRange = abs(m.indices[1]);
            const isConstructor = name === cls.name;
            const closeAt = bodyAt + body.lastIndexOf(')');
            let ok = true;
            // parameters
            const params = [];
            const paramsAt = bodyAt + m.indices[2][0];
            if (m[2].trim()) {
                for (const part of splitList(m[2])) {
                    const lead = part.text.search(/\S|$/);
                    const p = part.text.trim();
                    const pAt = paramsAt + part.from + lead;
                    if (!p) {
                        const c = part.comma >= 0 ? part.comma : part.from - 1;
                        err('Hier fehlt ein Parameter. Mehrere Parameter trennst du mit Kommas: »items: int, arrival: int«.', paramsAt + c, paramsAt + c + 1);
                        ok = false;
                        continue;
                    }
                    const pm = RE_PARAM.exec(p);
                    if (pm) {
                        if (!pm[2]) {
                            err(`Nach dem Doppelpunkt fehlt der Typ des Parameters »${pm[1]}«.`, pAt, pAt + p.length);
                            ok = false;
                            continue;
                        }
                        const node = typeAt(pm[2], pAt + pm.indices[2][0], false, ` für den Parameter »${pm[1]}«`);
                        if (!node) { ok = false; continue; }
                        if (params.some((q) => q.name === pm[1])) {
                            err(`Den Parameter »${pm[1]}« gibt es in dieser Methode schon.`, pAt, pAt + pm[1].length);
                            ok = false;
                        }
                        params.push({ name: pm[1], type: typeText(node) });
                        continue;
                    }
                    ok = false;
                    let j;
                    if (RE_ONE_NAME.test(p)) err(`Der Parameter »${p}« braucht einen Typ, zum Beispiel »${p}: String«.`, pAt, pAt + p.length);
                    else if ((j = p.match(new RegExp(`^(\\S.*?)\\s+(${NAME})$`, 'u'))) && !p.includes(':')) {
                        err(`Erst der Name, dann ein Doppelpunkt und der Typ: »${j[2]}: ${j[1]}«.`, pAt, pAt + p.length);
                    } else err('So sieht ein Parameter aus: »items: int«.', pAt, pAt + p.length);
                }
            }
            // return type
            let returns = null;
            if (m[3] !== undefined) {
                const retFrom = bodyAt + body.indexOf(':', closeAt - bodyAt);
                if (isConstructor) {
                    err(`Ein Konstruktor hat keinen Rückgabetyp: »${visibility} ${name}(…)«. Eine Methode heißt nicht wie ihre Klasse.`, retFrom, at + s.replace(/\s+$/, '').length - (semi >= 0 ? 1 : 0));
                    return null;
                }
                if (!m[3]) {
                    err(`Nach dem Doppelpunkt fehlt der Rückgabetyp, »void«, wenn die Methode nichts zurückgibt.`, retFrom, retFrom + 1);
                    return null;
                }
                const node = typeAt(m[3], bodyAt + m.indices[3][0], true, ' für den Rückgabewert');
                if (!node) return null;
                returns = typeText(node);
            } else if (!isConstructor) {
                const hint = name.toLowerCase() === String(cls.name).toLowerCase() ? ` Ein Konstruktor heißt genau wie die Klasse: »${cls.name}«.` : '';
                err(`Die Methode »${name}« braucht einen Rückgabetyp, »void«, wenn sie nichts zurückgibt: »${visibility} ${name}(…): void«.${hint}`, nameRange[0], closeAt + 1);
                return null;
            }
            if (!ok) return null;
            return { kind: 'method', name, constructor: isConstructor, returns, visibility, params, line, nameRange };
        }

        m = RE_ATTRIBUTE.exec(body);
        if (m) {
            const nameRange = abs(m.indices[1]);
            if (!m[2]) {
                err(`Nach dem Doppelpunkt fehlt der Typ, zum Beispiel »${visibility} ${m[1]}: String«.`);
                return null;
            }
            const node = typeAt(m[2], bodyAt + m.indices[2][0], false, '');
            if (!node) return null;
            return { kind: 'attribute', name: m[1], type: typeText(node), visibility, line, nameRange };
        }
        const t = body.trim();
        let j;
        if (RE_ONE_NAME.test(t)) {
            err(`Das Attribut »${t}« braucht einen Typ, zum Beispiel »${visibility} ${t}: String«.`);
        } else if ((j = t.match(new RegExp(`^(\\S.*?)\\s+(${NAME})$`, 'u'))) && !t.includes(':')) {
            err(`Erst der Name, dann ein Doppelpunkt und der Typ: »${visibility} ${j[2]}: ${j[1]}«.`);
        } else if (!t) {
            err(`Nach »${visibility}« fehlt das Attribut oder die Methode, zum Beispiel »${visibility} items: int«.`);
        } else {
            err('So sieht ein Attribut aus: »- items: int«, eine Methode: »+ averageWait(): double«.');
        }
        return null;
    }

    // parse, with the name the extension and the preview use.
    function compile(text) {
        return parse(text);
    }

    // What a check needs to know, as plain data.
    function summary(model) {
        const problems = (list) => list.map((p) => ({ line: p.line, from: p.from, to: p.to, msg: p.msg }));
        return {
            version: VERSION,
            errors: problems(model.errors),
            warnings: problems(model.warnings),
            classes: model.classes.map((c) => ({
                name: c.name, abstract: c.abstract, extends: c.extends, line: c.line,
                attributes: c.attributes.map((a) => ({ name: a.name, type: a.type, visibility: a.visibility })),
                methods: c.methods.map((x) => ({ name: x.name, constructor: x.constructor, returns: x.returns, visibility: x.visibility,
                    params: x.params.map((p) => ({ name: p.name, type: p.type })) })),
            })),
            associations: model.associations.map((a) => ({ name: a.name, from: a.from, to: a.to, fromMult: a.fromMult,
                toMult: a.toMult, directed: a.directed, line: a.line })),
        };
    }

    // ---------- the diagram -----------------------------------------------------

    // Width of a text in px at 13px IBM Plex Sans, without a browser (as in
    // the ER-Werkstatt). Good enough to size boxes; a renderer with a canvas
    // can pass its own.
    function estimateWidth(s, weight) {
        let w = 0;
        for (const ch of String(s)) {
            if ('iljI.,:;!|\''.includes(ch)) w += 3.6;
            else if ('mwMW'.includes(ch)) w += 10.4;
            else if (/[A-ZÄÖÜ]/.test(ch)) w += 8.6;
            else if (ch === ' ') w += 3.6;
            else w += 7.0;
        }
        return w * (weight && weight >= 600 ? 1.06 : 1);
    }

    // Members are set in a monospaced font (IBM Plex Mono, 12px): 0.6em a character.
    const MONO = 12;
    const monoWidth = (s) => [...String(s)].length * MONO * 0.6;
    const NAME_SIZE = 14;
    const LABEL_SIZE = 12;
    const ROW_H = 17;
    const PAD_X = 10;
    const PAD_Y = 5;
    const EMPTY_H = 8;
    const LAYER_GAP = 72;
    const LAYER_GAP_ASSOC = 104;
    const BOX_GAP = 56;
    // the least width of a row of classes without lines
    const LONE_ROW = 900;
    const PAR_GAP = 26;
    const TRI_LEN = 15;
    const TRI_HALF = 8.5;
    const HEAD_LEN = 12;
    const HEAD_HALF = 6;
    const LOOP = 38;

    function memberText(x) {
        if (x.kind === 'attribute' || x.params === undefined) return `${x.visibility} ${x.name}: ${x.type}`;
        return `${x.visibility} ${x.name}(${x.params.map((p) => `${p.name}: ${p.type}`).join(', ')})${x.returns ? `: ${x.returns}` : ''}`;
    }

    const labelWidth = (s, tw) => tw(s) * LABEL_SIZE / 13 + 4;

    // Boxes and lines. node: { id, name, abstract, attrs, methods (lines of
    // text), w, h, headH, attrH, methH }; edge: { kind: 'inherit', a: subclass,
    // b: superclass } or { kind: 'assoc', a, b, name, multA, multB, directed }.
    function classGraph(model, measure) {
        const tw = measure || estimateWidth;
        const nodes = [];
        const edges = [];
        const compartment = (rows) => (rows ? rows * ROW_H + 2 * PAD_Y : EMPTY_H);
        const firsts = model.classes.filter((c) => model.byName.get(c.name) === c);
        for (const c of firsts) {
            const attrs = c.attributes.map(memberText);
            const methods = c.methods.map(memberText);
            const nameW = tw(c.name, 700) * NAME_SIZE / 13;
            const stereoW = c.abstract ? tw('«abstract»') * 11 / 13 : 0;
            const rowW = Math.max(0, ...attrs.concat(methods).map(monoWidth));
            const w = Math.ceil(Math.max(110, nameW + 36, stereoW + 36, rowW + 2 * PAD_X + 2));
            const headH = c.abstract ? 42 : 30;
            const attrH = compartment(attrs.length);
            const methH = compartment(methods.length);
            nodes.push({ id: `C:${c.name}`, name: c.name, abstract: c.abstract, attrs, methods, w, h: headH + attrH + methH, headH, attrH, methH });
        }
        const ids = new Set(nodes.map((n) => n.id));
        for (const c of firsts) {
            if (c.extends && c.extends !== c.name && ids.has(`C:${c.extends}`)) edges.push({ kind: 'inherit', a: `C:${c.name}`, b: `C:${c.extends}` });
        }
        for (const a of model.associations) {
            if (!ids.has(`C:${a.from}`) || !ids.has(`C:${a.to}`)) continue;
            edges.push({ kind: 'assoc', a: `C:${a.from}`, b: `C:${a.to}`, name: a.name, multA: a.fromMult, multB: a.toMult, directed: a.directed });
        }
        return { nodes, edges };
    }

    // Segment p-q against segment r-s (proper crossing only).
    function segmentsCross(p, q, r, s) {
        const cross = (o, a, b) => (a.x - o.x) * (b.y - o.y) - (a.y - o.y) * (b.x - o.x);
        const d1 = cross(r, s, p);
        const d2 = cross(r, s, q);
        const d3 = cross(p, q, r);
        const d4 = cross(p, q, s);
        return ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0));
    }

    // Does the segment touch the rectangle { x, y, w, h } grown by pad? (Liang-Barsky)
    function segmentHitsRect(p, q, r, pad) {
        const x0 = r.x - pad; const x1 = r.x + r.w + pad;
        const y0 = r.y - pad; const y1 = r.y + r.h + pad;
        const dx = q.x - p.x;
        const dy = q.y - p.y;
        let t0 = 0;
        let t1 = 1;
        for (const [pp, qq] of [[-dx, p.x - x0], [dx, x1 - p.x], [-dy, p.y - y0], [dy, y1 - p.y]]) {
            if (pp === 0) {
                if (qq < 0) return false;
            } else {
                const t = qq / pp;
                if (pp < 0) { if (t > t1) return false; if (t > t0) t0 = t; } else { if (t < t0) return false; if (t < t1) t1 = t; }
            }
        }
        return t0 <= t1;
    }

    const rectsOverlap = (a, b, pad) => a.x < b.x + b.w + pad && b.x < a.x + a.w + pad && a.y < b.y + b.h + pad && b.y < a.y + a.h + pad;
    const boxRect = (n) => ({ x: n.x - n.w / 2, y: n.y - n.h / 2, w: n.w, h: n.h });

    // All orders of a small list, the given order first.
    function permutations(list) {
        if (list.length <= 1) return [list.slice()];
        const out = [];
        list.forEach((first, i) => {
            for (const rest of permutations(list.slice(0, i).concat(list.slice(i + 1)))) out.push([first, ...rest]);
        });
        return out;
    }

    // Places the boxes; deterministic (same model, same picture). Sets x and y
    // of every node (its centre) and returns { id: { x, y } }.
    //
    // Layers by inheritance: a superclass right above its subclasses. Every
    // inheritance tree (a class without super- or subclasses is a tree of
    // its own) gets a vertical offset as a whole: first beside the classes it
    // is associated with, then each tree is tried a few layers up or down
    // while that makes the picture better. »Better« is measured on the real
    // positions: lines through boxes, crossings, associations over several
    // layers, line lengths and a picture far wider than high. The order in
    // each layer is chosen the same way (all orders of up to six boxes, else
    // swaps of neighbours); x pulls every box towards its neighbours without
    // overlap, with room between two boxes for the labels of an association
    // between them. Subclasses of one class stay together under it.
    //
    // A class without lines to other classes takes no part in this: it would
    // only push connected boxes apart. Such classes come last, in rows of
    // their own below the others, centred, as wide as the rest of the
    // picture (at least LONE_ROW).
    function layout(graph, options) {
        const tw = (options && options.measure) || estimateWidth;
        const { nodes, edges } = graph;
        if (!nodes.length) return {};
        const byId = new Map(nodes.map((n) => [n.id, n]));
        const links = edges.filter((e) => e.a !== e.b && byId.has(e.a) && byId.has(e.b));
        const lone = new Set(nodes.map((n) => n.id));
        for (const e of links) { lone.delete(e.a); lone.delete(e.b); }
        const parent = new Map();
        for (const e of edges) if (e.kind === 'inherit' && !parent.has(e.a)) parent.set(e.a, e.b);
        // depth and root, safe against cycles (only in models with errors)
        const depth = new Map();
        const rootOf = new Map();
        for (const n of nodes) {
            let d = 0;
            let cur = n.id;
            const seen = new Set([cur]);
            while (parent.has(cur) && !seen.has(parent.get(cur))) { cur = parent.get(cur); seen.add(cur); d++; }
            depth.set(n.id, d);
            rootOf.set(n.id, cur);
        }
        const trees = [];
        const treeOf = new Map();
        nodes.forEach((n, i) => {
            if (lone.has(n.id)) return;
            const r = rootOf.get(n.id);
            let t = trees.find((x) => x.root === r);
            if (!t) { t = { root: r, members: [], first: i }; trees.push(t); }
            t.members.push(n.id);
            treeOf.set(n.id, t);
        });
        const assoc = edges.filter((e) => e.kind === 'assoc' && e.a !== e.b && byId.has(e.a) && byId.has(e.b));
        // Room a self-association needs on the right of its box.
        const loopRoom = new Map();
        for (const e of edges) {
            if (e.kind !== 'assoc' || e.a !== e.b) continue;
            const extra = LOOP * 0.6 + Math.max(labelWidth(e.name, tw), monoWidth(e.multA), monoWidth(e.multB)) + 16;
            loopRoom.set(e.a, Math.max(loopRoom.get(e.a) || 0, extra));
        }
        const packW = (id) => byId.get(id).w + (loopRoom.get(id) || 0);
        const pairKey = (u, v) => (u < v ? `${u}|${v}` : `${v}|${u}`);
        // The gap two neighbours in a layer need: room for the labels of the
        // associations between them (used when they share a layer).
        const labelGap = new Map();
        for (const e of assoc) {
            const need = Math.max(labelWidth(e.name, tw) + 34, monoWidth(e.multA) + monoWidth(e.multB) + 44, e.directed ? 70 : 0);
            const key = pairKey(e.a, e.b);
            labelGap.set(key, Math.max(labelGap.get(key) || BOX_GAP, need));
        }

        // Everything below depends on the layers: rank, layers, near, yOf.
        const rank = new Map();
        let layers = [];
        let near = new Map();
        let beside = new Map();
        let yOf = [];
        const x = new Map();
        const gap = (u, v) => (packW(u) + packW(v)) / 2 + (rank.get(u) === rank.get(v) && labelGap.get(pairKey(u, v)) || BOX_GAP);
        const pack = (layer, want) => {
            const fwd = want.slice();
            for (let i = 1; i < layer.length; i++) fwd[i] = Math.max(fwd[i], fwd[i - 1] + gap(layer[i - 1], layer[i]));
            const bwd = want.slice();
            for (let i = layer.length - 2; i >= 0; i--) bwd[i] = Math.min(bwd[i], bwd[i + 1] - gap(layer[i], layer[i + 1]));
            const mix = want.map((_, i) => (fwd[i] + bwd[i]) / 2);
            for (let i = 1; i < layer.length; i++) mix[i] = Math.max(mix[i], mix[i - 1] + gap(layer[i - 1], layer[i]));
            layer.forEach((id, i) => x.set(id, mix[i]));
        };
        // y: one band per layer, as high as its highest box, boxes centred in it.
        const bands = () => {
            yOf = [];
            let top = 0;
            layers.forEach((layer, r) => {
                const h = Math.max(0, ...layer.map((id) => byId.get(id).h));
                yOf.push(top + h / 2);
                const across = assoc.some((e) => (rank.get(e.a) === r && rank.get(e.b) > r) || (rank.get(e.b) === r && rank.get(e.a) > r));
                const loopBelow = r + 1 < layers.length && layers[r + 1].some((id) => loopRoom.has(id));
                top += h + (across ? LAYER_GAP_ASSOC : LAYER_GAP) + (loopBelow ? 30 : 0);
            });
        };
        // finish: one more pass downward, so that subclasses end centred
        // under their class (only for the final picture: the choice of
        // layers and orders is made without it)
        const placeX = (finish) => {
            for (const all of layers) {
                const layer = all.filter((id) => !lone.has(id));
                const want = [];
                let at = 0;
                layer.forEach((id, i) => { if (i) at += gap(layer[i - 1], id); want.push(at); });
                const shift = at / 2;
                pack(layer, want.map((v) => v - shift));
            }
            for (let it = 0; it < (finish ? 11 : 10); it++) {
                const downward = it % 2 === 0;
                const order = downward ? [...layers.keys()] : [...layers.keys()].reverse();
                for (const r of order) {
                    const layer = layers[r].filter((id) => !lone.has(id));
                    const want = layer.map((id) => {
                        const all = near.get(id);
                        const side = all.filter((o) => (downward ? rank.get(o) < r : rank.get(o) > r));
                        // without neighbours in other layers: next to the associated classes in its own
                        const ns = side.length ? side : all.length ? all : beside.get(id);
                        return ns.length ? ns.reduce((s, o) => s + x.get(o), 0) / ns.length : x.get(id);
                    });
                    pack(layer, want);
                }
            }
            // rows of classes without lines: centred under the rest
            let left = Infinity;
            let right = -Infinity;
            for (const layer of layers) {
                for (const id of layer) {
                    if (lone.has(id)) continue;
                    left = Math.min(left, x.get(id) - packW(id) / 2);
                    right = Math.max(right, x.get(id) + packW(id) / 2);
                }
            }
            const middle = left <= right ? (left + right) / 2 : 0;
            for (const layer of layers) {
                if (!layer.length || !lone.has(layer[0])) continue;
                let at = 0;
                layer.forEach((id, i) => { if (i) at += gap(layer[i - 1], id); x.set(id, at); });
                const shift = middle - (at + packW(layer[layer.length - 1]) / 2 - packW(layer[0]) / 2) / 2;
                for (const id of layer) x.set(id, x.get(id) + shift);
            }
        };
        // How good the picture is: few lines through boxes and crossings,
        // short lines, not too wide or high.
        const cost = () => {
            placeX();
            const pos = (id) => ({ x: x.get(id) - (loopRoom.get(id) || 0) / 2, y: yOf[rank.get(id)] });
            const rects = nodes.filter((n) => rank.has(n.id)).map((n) => { const p = pos(n.id); return { id: n.id, x: p.x - n.w / 2, y: p.y - n.h / 2, w: n.w, h: n.h }; });
            const segs = links.map((e) => ({ e, p: pos(e.a), q: pos(e.b) }));
            let c = 0;
            for (let i = 0; i < segs.length; i++) {
                const s = segs[i];
                c += Math.hypot(s.q.x - s.p.x, s.q.y - s.p.y) * 0.004;
                // associated classes beside each other, where it fits
                if (s.e.kind === 'assoc') c += Math.abs(rank.get(s.e.a) - rank.get(s.e.b)) * 8;
                for (const r of rects) if (r.id !== s.e.a && r.id !== s.e.b && segmentHitsRect(s.p, s.q, r, 6)) c += 40;
                for (let j = i + 1; j < segs.length; j++) {
                    const t = segs[j];
                    if (s.e.a === t.e.a || s.e.a === t.e.b || s.e.b === t.e.a || s.e.b === t.e.b) continue;
                    if (segmentsCross(s.p, s.q, t.p, t.q)) c += 10;
                }
            }
            // subclasses of one class side by side, nothing between them
            for (const layer of layers) {
                const at = new Map();
                layer.forEach((id, i) => {
                    const p = parent.get(id);
                    if (!p) return;
                    if (!at.has(p)) at.set(p, []);
                    at.get(p).push(i);
                });
                for (const list of at.values()) c += (list[list.length - 1] - list[0] + 1 - list.length) * 15;
            }
            let x0 = Infinity; let x1 = -Infinity; let y0 = Infinity; let y1 = -Infinity;
            for (const r of rects) { x0 = Math.min(x0, r.x); x1 = Math.max(x1, r.x + r.w); y0 = Math.min(y0, r.y); y1 = Math.max(y1, r.y + r.h); }
            // a picture far wider than high (or the other way) is hard to read
            const w = x1 - x0;
            const h = y1 - y0;
            return c + (w + h) * 0.004 + Math.max(0, w - 2 * h - 300) * 0.006 + Math.max(0, h - 1.5 * w) * 0.006;
        };
        // Layers from the offsets of the trees; the order in each layer
        // either tried in full (thorough) or by swapping neighbours.
        const arrange = (offsets, thorough) => {
            rank.clear();
            for (const t of trees) for (const id of t.members) rank.set(id, depth.get(id) + offsets.get(t));
            const low = Math.min(...rank.values());
            for (const [id, r] of rank) rank.set(id, r - low);
            const nLayers = Math.max(...rank.values()) + 1;
            layers = Array.from({ length: nLayers }, () => []);
            for (const n of nodes) if (rank.has(n.id)) layers[rank.get(n.id)].push(n.id);
            near = new Map(nodes.map((n) => [n.id, []]));
            beside = new Map(nodes.map((n) => [n.id, []]));
            for (const e of links) {
                if (rank.get(e.a) === rank.get(e.b)) {
                    beside.get(e.a).push(e.b);
                    beside.get(e.b).push(e.a);
                    continue;
                }
                near.get(e.a).push(e.b);
                near.get(e.b).push(e.a);
            }
            bands();
            // Start: lower layers in the order of their neighbours above.
            for (let r = 1; r < layers.length; r++) {
                const above = new Map(layers.slice(0, r).flat().map((id, i) => [id, i]));
                const key = (id) => {
                    const ns = near.get(id).filter((o) => above.has(o));
                    return ns.length ? ns.reduce((s, o) => s + above.get(o), 0) / ns.length : Infinity;
                };
                layers[r] = layers[r].map((id, i) => [id, key(id), i]).sort((p, q) => (p[1] - q[1]) || (p[2] - q[2])).map((p) => p[0]);
            }
            let best = cost();
            for (let round = 0; round < 3; round++) {
                let better = false;
                for (let r = 0; r < layers.length; r++) {
                    let cur = layers[r];
                    if (thorough && cur.length <= 6) {
                        for (const order of permutations(cur)) {
                            layers[r] = order;
                            const c = cost();
                            if (c < best - 1e-9) { best = c; cur = order; better = true; }
                        }
                    } else {
                        for (let pass = 0; pass < cur.length; pass++) {
                            let moved = false;
                            for (let i = 0; i + 1 < cur.length; i++) {
                                const next = cur.slice();
                                [next[i], next[i + 1]] = [next[i + 1], next[i]];
                                layers[r] = next;
                                const c = cost();
                                if (c < best - 1e-9) { best = c; cur = next; moved = true; better = true; }
                            }
                            if (!moved) break;
                        }
                    }
                    layers[r] = cur;
                }
                if (!better) break;
            }
            return { cost: best, layers: layers.map((l) => l.slice()) };
        };

        // First guess for the offsets: every tree beside the classes it is
        // associated with (the median of what its associations want).
        const offsets = new Map();
        const remaining = trees.slice().sort((p, q) => q.members.length - p.members.length || p.first - q.first);
        while (remaining.length) {
            const count = (t) => assoc.filter((e) => (treeOf.get(e.a) === t && offsets.has(treeOf.get(e.b))) || (treeOf.get(e.b) === t && offsets.has(treeOf.get(e.a)))).length;
            let best = 0;
            let most = -1;
            remaining.forEach((t, i) => { const c = count(t); if (c > most) { most = c; best = i; } });
            const t = remaining.splice(best, 1)[0];
            const wants = [];
            const rankOf = (id) => depth.get(id) + offsets.get(treeOf.get(id));
            for (const e of assoc) {
                if (treeOf.get(e.a) === t && offsets.has(treeOf.get(e.b))) wants.push(rankOf(e.b) - depth.get(e.a));
                if (treeOf.get(e.b) === t && offsets.has(treeOf.get(e.a))) wants.push(rankOf(e.a) - depth.get(e.b));
            }
            wants.sort((p, q) => p - q);
            offsets.set(t, wants.length ? Math.max(0, wants[Math.floor((wants.length - 1) / 2)]) : 0);
        }
        // Then each tree tried a few layers up or down, while that helps.
        let bestCost = trees.length ? arrange(offsets, false).cost : 0;
        if (trees.length > 1) {
            const height = Math.max(...depth.values()) + 1;
            const maxOff = Math.min(trees.length, 4) + height;
            for (let round = 0; round < 2; round++) {
                let better = false;
                for (const t of trees) {
                    const keep = offsets.get(t);
                    let bestOff = keep;
                    for (let off = 0; off <= maxOff; off++) {
                        if (off === keep) continue;
                        offsets.set(t, off);
                        const c = arrange(offsets, false).cost;
                        if (c < bestCost - 1e-9) { bestCost = c; bestOff = off; better = true; }
                    }
                    offsets.set(t, bestOff);
                }
                if (!better) break;
            }
        }
        if (trees.length) arrange(offsets, true);
        else layers = [];
        // The classes without lines, in rows below, a new row when one
        // would be wider than the rest of the picture
        const loneIds = nodes.filter((n) => lone.has(n.id)).map((n) => n.id);
        if (loneIds.length) {
            let width = LONE_ROW;
            if (layers.length) {
                bands();
                placeX(true);
                let left = Infinity;
                let right = -Infinity;
                for (const layer of layers) for (const id of layer) { left = Math.min(left, x.get(id) - packW(id) / 2); right = Math.max(right, x.get(id) + packW(id) / 2); }
                width = Math.max(width, right - left);
            }
            let row = [];
            let used = 0;
            for (const id of loneIds) {
                const add = packW(id) + (row.length ? BOX_GAP : 0);
                if (row.length && used + add > width) { layers.push(row); row = []; used = 0; }
                used += packW(id) + (row.length ? BOX_GAP : 0);
                row.push(id);
            }
            layers.push(row);
            layers.forEach((layer, r) => { for (const id of layer) rank.set(id, r); });
            for (const id of loneIds) if (!near.has(id)) { near.set(id, []); beside.set(id, []); }
        }
        bands();
        placeX(true);
        const result = {};
        for (const n of nodes) {
            n.x = Math.round((x.get(n.id) - (loopRoom.get(n.id) || 0) / 2) * 10) / 10;
            n.y = Math.round(yOf[rank.get(n.id)] * 10) / 10;
            result[n.id] = { x: n.x, y: n.y };
        }
        return result;
    }

    // Where a ray from (px, py) inside box n in direction (dx, dy) leaves it.
    function exitPoint(n, px, py, dx, dy) {
        let t = Infinity;
        if (dx > 0) t = Math.min(t, (n.x + n.w / 2 - px) / dx);
        if (dx < 0) t = Math.min(t, (n.x - n.w / 2 - px) / dx);
        if (dy > 0) t = Math.min(t, (n.y + n.h / 2 - py) / dy);
        if (dy < 0) t = Math.min(t, (n.y - n.h / 2 - py) / dy);
        if (!Number.isFinite(t)) t = 0;
        return { x: px + dx * t, y: py + dy * t };
    }

    function bezier(p0, p1, p2, p3, t) {
        const u = 1 - t;
        return {
            x: u * u * u * p0.x + 3 * u * u * t * p1.x + 3 * u * t * t * p2.x + t * t * t * p3.x,
            y: u * u * u * p0.y + 3 * u * u * t * p1.y + 3 * u * t * t * p2.y + t * t * t * p3.y,
        };
    }

    // The lines, arrowheads and labels of a laid-out graph:
    // { lines: [{ edge, d, head }], labels: [{ kind: 'mult'|'name', text, x, y, w, h }], bounds }.
    // Lines aim at the centres of the boxes and end on their borders; heads
    // follow the line with their tip on the border. Labels go beside the
    // lines: candidates near the right place, the one that touches no box,
    // no other label and no line wins.
    function geometry(graph, options) {
        const tw = (options && options.measure) || estimateWidth;
        const { nodes, edges } = graph;
        const byId = new Map(nodes.map((n) => [n.id, n]));
        const boxes = nodes.map(boxRect);
        const segs = [];
        const lines = [];
        const pending = [];
        // Several lines between the same two boxes run side by side.
        const groups = new Map();
        edges.forEach((e, i) => {
            if (e.a === e.b || !byId.has(e.a) || !byId.has(e.b)) return;
            const key = [e.a, e.b].sort().join('|');
            if (!groups.has(key)) groups.set(key, []);
            groups.get(key).push(i);
        });
        const offsetOf = new Map();
        for (const list of groups.values()) list.forEach((i, k) => offsetOf.set(i, (k - (list.length - 1) / 2) * PAR_GAP));
        const loops = new Map();
        const f = (v) => (Math.round(v * 10) / 10).toFixed(1);
        const P = (p) => `${f(p.x)},${f(p.y)}`;

        edges.forEach((e, i) => {
            const a = byId.get(e.a);
            const b = byId.get(e.b);
            if (!a || !b) return;
            if (a === b) {
                // A loop over one corner: out of the vertical side, back into
                // the horizontal one. Corners in turn: top right, bottom right,
                // bottom left, top left.
                const k = loops.get(a.id) || 0;
                loops.set(a.id, k + 1);
                const sx = [1, 1, -1, -1][k % 4];
                const sy = [-1, 1, 1, -1][k % 4];
                const cx = a.x + sx * a.w / 2;
                const cy = a.y + sy * a.h / 2;
                const S = { x: cx, y: cy - sy * Math.min(16, a.h / 4) };
                const E = { x: cx - sx * Math.min(26, a.w / 4), y: cy };
                const c1 = { x: S.x + sx * LOOP * 1.3, y: S.y };
                const c2 = { x: E.x, y: E.y + sy * LOOP * 1.3 };
                const pts = Array.from({ length: 13 }, (_, j) => bezier(S, c1, c2, E, j / 12));
                for (let j = 0; j + 1 < pts.length; j++) segs.push([pts[j], pts[j + 1]]);
                const line = { edge: e, d: `M${P(S)} C${P(c1)} ${P(c2)} ${P(E)}`, head: null };
                if (e.directed) line.head = arrowHead(E, 0, -sy, segs);
                lines.push(line);
                pending.push({ e, ends: [{ p: S, u: { x: sx, y: 0 }, text: e.multA }, { p: E, u: { x: 0, y: sy }, text: e.multB }],
                    name: { at: (t) => bezier(S, c1, c2, E, 0.25 + t * 0.5), out: { x: sx * Math.SQRT1_2, y: sy * Math.SQRT1_2 } } });
                return;
            }
            // the offset of side-by-side lines is measured in a direction
            // that does not depend on which end is a and which is b
            const [p, q] = e.a < e.b ? [a, b] : [b, a];
            const len = Math.hypot(q.x - p.x, q.y - p.y) || 1;
            const nx = -(q.y - p.y) / len;
            const ny = (q.x - p.x) / len;
            const limit = Math.max(0, Math.min(a.w, a.h, b.w, b.h) / 2 - 6);
            const off = Math.max(-limit, Math.min(limit, offsetOf.get(i) || 0));
            const dx = b.x - a.x;
            const dy = b.y - a.y;
            const pa = exitPoint(a, a.x + nx * off, a.y + ny * off, dx, dy);
            const pb = exitPoint(b, b.x + nx * off, b.y + ny * off, -dx, -dy);
            const l = Math.hypot(pb.x - pa.x, pb.y - pa.y) || 1;
            const ux = (pb.x - pa.x) / l;
            const uy = (pb.y - pa.y) / l;
            if (e.kind === 'inherit') {
                // hollow triangle, tip on the superclass
                const base = { x: pb.x - ux * TRI_LEN, y: pb.y - uy * TRI_LEN };
                const c1 = { x: base.x - uy * TRI_HALF, y: base.y + ux * TRI_HALF };
                const c2 = { x: base.x + uy * TRI_HALF, y: base.y - ux * TRI_HALF };
                segs.push([pa, base], [pb, c1], [c1, c2], [c2, pb]);
                lines.push({ edge: e, d: `M${P(pa)} L${P(base)}`, triangle: `${P(pb)} ${P(c1)} ${P(c2)}` });
                return;
            }
            segs.push([pa, pb]);
            const line = { edge: e, d: `M${P(pa)} L${P(pb)}`, head: null };
            if (e.directed) line.head = arrowHead(pb, ux, uy, segs);
            lines.push(line);
            // labels of side-by-side lines go on their outer side
            const outer = off ? { x: Math.sign(off) * nx, y: Math.sign(off) * ny } : null;
            pending.push({ e, outer, ends: [{ p: pa, u: { x: ux, y: uy }, text: e.multA }, { p: pb, u: { x: -ux, y: -uy }, text: e.multB }],
                name: { at: (t) => ({ x: pa.x + (pb.x - pa.x) * t, y: pa.y + (pb.y - pa.y) * t }), u: { x: ux, y: uy } } });
        });

        const labels = [];
        const taken = [];
        const place = (text, kind, w, h, candidates) => {
            let best = null;
            for (const c of candidates) {
                const r = { x: c.x - w / 2, y: c.y - h / 2, w, h };
                let cost = c.cost;
                for (const b of boxes) if (rectsOverlap(r, b, 3)) cost += 1000;
                for (const t of taken) if (rectsOverlap(r, t, 3)) cost += 800;
                for (const [s0, s1] of segs) if (segmentHitsRect(s0, s1, r, 2)) cost += 300;
                if (!best || cost < best.cost) best = { x: c.x, y: c.y, cost, r };
            }
            taken.push(best.r);
            labels.push({ kind, text, x: Math.round(best.x * 10) / 10, y: Math.round(best.y * 10) / 10, w, h });
        };
        // the preferred side of a line: above a flat line, right of a steep one
        const preferred = (u) => {
            const n = { x: -u.y, y: u.x };
            const flip = Math.abs(u.x) >= Math.abs(u.y) ? n.y > 0 : n.x < 0;
            return flip ? { x: -n.x, y: -n.y } : n;
        };
        const H = 15;
        const inner = (item, n) => (item.outer && n.x * item.outer.x + n.y * item.outer.y < 0 ? 200 : 0);
        // multiplicities first: they have to stay near their end
        for (const item of pending) {
            for (const end of item.ends) {
                const w = monoWidth(end.text) + 4;
                const u = end.u;
                const n0 = preferred(u);
                const pref = { x: -n0.x, y: -n0.y };
                const supN = (n) => (w / 2) * Math.abs(n.x) + (H / 2) * Math.abs(n.y);
                const supU = (w / 2) * Math.abs(u.x) + (H / 2) * Math.abs(u.y);
                const cands = [];
                [pref, n0].forEach((n, side) => {
                    [3, 8, 14, 21, 30, 42, 56].forEach((d, k) => {
                        cands.push({ x: end.p.x + u.x * (d + supU) + n.x * (supN(n) + 3), y: end.p.y + u.y * (d + supU) + n.y * (supN(n) + 3), cost: k * 4 + side * 10 + inner(item, n) });
                    });
                });
                place(end.text, 'mult', w, H, cands);
            }
        }
        for (const item of pending) {
            const w = labelWidth(item.e.name, tw);
            const cands = [];
            if (item.name.u) {
                const n0 = preferred(item.name.u);
                [n0, { x: -n0.x, y: -n0.y }].forEach((n, side) => {
                    const sup = (w / 2) * Math.abs(n.x) + (H / 2) * Math.abs(n.y) + 4;
                    [0.5, 0.42, 0.58, 0.34, 0.66, 0.26, 0.74].forEach((t, k) => {
                        const m = item.name.at(t);
                        cands.push({ x: m.x + n.x * sup, y: m.y + n.y * sup, cost: k * 5 + side * 12 + inner(item, n) });
                    });
                });
                // further out, when both sides are full
                [0.5, 0.4, 0.6].forEach((t, k) => [1, -1].forEach((s) => {
                    const m = item.name.at(t);
                    const sup = (w / 2) * Math.abs(n0.x) + (H / 2) * Math.abs(n0.y) + 4;
                    cands.push({ x: m.x + n0.x * s * (sup + 18), y: m.y + n0.y * s * (sup + 18), cost: 80 + k * 5 + inner(item, { x: n0.x * s, y: n0.y * s }) });
                }));
            } else {
                const o = item.name.out;
                [0.5, 0.35, 0.65].forEach((t, k) => [0, 8, 18].forEach((d, j) => {
                    const m = item.name.at(t);
                    const sup = (w / 2) * Math.abs(o.x) + (H / 2) * Math.abs(o.y) + 4 + d;
                    cands.push({ x: m.x + o.x * sup, y: m.y + o.y * sup, cost: k * 5 + j * 4 });
                }));
            }
            place(item.e.name, 'name', w, H, cands);
        }

        // bounds: boxes, labels and every line
        let x0 = Infinity; let y0 = Infinity; let x1 = -Infinity; let y1 = -Infinity;
        const grow = (x, y) => { x0 = Math.min(x0, x); x1 = Math.max(x1, x); y0 = Math.min(y0, y); y1 = Math.max(y1, y); };
        for (const b of boxes) { grow(b.x, b.y); grow(b.x + b.w, b.y + b.h); }
        for (const l of labels) { grow(l.x - l.w / 2, l.y - l.h / 2); grow(l.x + l.w / 2, l.y + l.h / 2); }
        for (const [s0, s1] of segs) { grow(s0.x, s0.y); grow(s1.x, s1.y); }
        const bounds = Number.isFinite(x0) ? [x0, y0, x1 - x0, y1 - y0] : [0, 0, 0, 0];
        return { lines, labels, bounds };
    }

    // An open arrowhead with its tip at p, pointing along (ux, uy).
    function arrowHead(p, ux, uy, segs) {
        const back = { x: p.x - ux * HEAD_LEN, y: p.y - uy * HEAD_LEN };
        const l = { x: back.x - uy * HEAD_HALF, y: back.y + ux * HEAD_HALF };
        const r = { x: back.x + uy * HEAD_HALF, y: back.y - ux * HEAD_HALF };
        segs.push([l, p], [p, r]);
        const f = (v) => (Math.round(v * 10) / 10).toFixed(1);
        return `M${f(l.x)},${f(l.y)} L${f(p.x)},${f(p.y)} L${f(r.x)},${f(r.y)}`;
    }

    // The area of the drawing, with a margin.
    function viewBox(graph, options) {
        if (!graph.nodes.length) return [0, 0, 600, 380];
        const b = geometry(graph, options).bounds;
        const pad = 24;
        const x = Math.floor(b[0] - pad);
        const y = Math.floor(b[1] - pad);
        return [x, y, Math.ceil(b[0] + b[2] + pad) - x, Math.ceil(b[1] + b[3] + pad) - y];
    }

    function esc(s) {
        return String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    }

    // Colours as CSS variables with light defaults, so a page can theme them.
    // Every rule is limited to the diagram (.cd-diagram): an SVG embedded in a
    // page carries its <style> into the whole page.
    const SANS = '"IBM Plex Sans",system-ui,sans-serif';
    const MONO_FONT = '"IBM Plex Mono",ui-monospace,monospace';
    const STYLE_RULES = `
    .cbox{fill:var(--cd-shape,#fff);stroke:var(--cd-ink,#1f2937);stroke-width:1.5}
    .chead{fill:var(--cd-pen-soft,#e8effc)}
    .csep{stroke:var(--cd-ink,#1f2937);stroke-width:1}
    .cname{font-family:${SANS};font-size:${NAME_SIZE}px;font-weight:600;fill:var(--cd-ink,#1f2937);text-anchor:middle}
    .cname.abs{font-style:italic}
    .cstereo{font-family:${SANS};font-size:11px;fill:var(--cd-muted,#6b7280);text-anchor:middle}
    .cmem{font-family:${MONO_FONT};font-size:${MONO}px;fill:var(--cd-ink,#1f2937)}
    .cln{stroke:var(--cd-ink,#1f2937);stroke-width:1.3;fill:none}
    .cinherit{fill:var(--cd-shape,#fff);stroke:var(--cd-ink,#1f2937);stroke-width:1.3;stroke-linejoin:miter}
    .carrow{fill:none;stroke:var(--cd-ink,#1f2937);stroke-width:1.3;stroke-linejoin:miter}
    .cmult{font-family:${MONO_FONT};font-size:${MONO}px;font-weight:500;fill:var(--cd-pen,#2b5fb4);text-anchor:middle;paint-order:stroke;stroke:var(--cd-paper,#fff);stroke-width:4px;stroke-linejoin:round}
    .clbl{font-family:${SANS};font-size:${LABEL_SIZE}px;font-style:italic;fill:var(--cd-pen,#2b5fb4);text-anchor:middle;paint-order:stroke;stroke:var(--cd-paper,#fff);stroke-width:4px;stroke-linejoin:round}`;
    const STYLE = STYLE_RULES.replace(/(^|\})\s*(\.[^{]+)\{/g, '$1\n    .cd-diagram $2{');

    // The SVG of a laid-out graph, as a string.
    function renderSVG(graph, options) {
        const opts = Object.assign({ background: null, measure: estimateWidth, title: 'Klassendiagramm' }, options || {});
        const { nodes } = graph;
        const vb = opts.viewBox || viewBox(graph, opts);
        const geo = geometry(graph, opts);
        const f = (v) => (Math.round(v * 10) / 10).toFixed(1);
        const S = [];
        S.push(`<svg xmlns="http://www.w3.org/2000/svg" class="cd-diagram" viewBox="${vb.join(' ')}" width="${vb[2]}" height="${vb[3]}" role="img" aria-label="${esc(opts.title)}">`);
        S.push(`<style>${STYLE}</style>`);
        if (opts.background) S.push(`<rect x="${vb[0]}" y="${vb[1]}" width="${vb[2]}" height="${vb[3]}" fill="${esc(opts.background)}"/>`);
        for (const l of geo.lines) {
            const e = l.edge;
            const id = `data-from="${esc(e.a.slice(2))}" data-to="${esc(e.b.slice(2))}"`;
            if (e.kind === 'inherit') {
                S.push(`<g class="edge inherit" ${id}><path class="cln" d="${l.d}"/><polygon class="cinherit" points="${l.triangle}"/></g>`);
            } else {
                S.push(`<g class="edge assoc${e.directed ? ' directed' : ''}" ${id} data-name="${esc(e.name)}"><path class="cln" d="${l.d}"/>` +
                    (l.head ? `<path class="carrow" d="${l.head}"/>` : '') + '</g>');
            }
        }
        const baseline = (y, size) => f(y + size * 0.35);
        for (const n of nodes) {
            const x0 = -n.w / 2;
            const y0 = -n.h / 2;
            const parts = [];
            parts.push(`<rect class="cbox" x="${f(x0)}" y="${f(y0)}" width="${f(n.w)}" height="${f(n.h)}"/>`);
            parts.push(`<rect class="chead" x="${f(x0 + 0.75)}" y="${f(y0 + 0.75)}" width="${f(n.w - 1.5)}" height="${f(n.headH - 0.75)}"/>`);
            const ya = y0 + n.headH;
            const ym = ya + n.attrH;
            parts.push(`<line class="csep" x1="${f(x0)}" y1="${f(ya)}" x2="${f(-x0)}" y2="${f(ya)}"/><line class="csep" x1="${f(x0)}" y1="${f(ym)}" x2="${f(-x0)}" y2="${f(ym)}"/>`);
            if (n.abstract) {
                parts.push(`<text class="cstereo" x="0" y="${baseline(y0 + 12, 11)}">«abstract»</text>`);
                parts.push(`<text class="cname abs" x="0" y="${baseline(y0 + 28, NAME_SIZE)}">${esc(n.name)}</text>`);
            } else {
                parts.push(`<text class="cname" x="0" y="${baseline(y0 + n.headH / 2, NAME_SIZE)}">${esc(n.name)}</text>`);
            }
            const rows = (list, top) => list.map((t, i) => `<text class="cmem" x="${f(x0 + PAD_X)}" y="${baseline(top + PAD_Y + ROW_H * (i + 0.5), MONO)}">${esc(t)}</text>`).join('');
            parts.push(rows(n.attrs, ya), rows(n.methods, ym));
            S.push(`<g class="cls" data-class="${esc(n.name)}" transform="translate(${f(n.x)},${f(n.y)})">${parts.join('')}</g>`);
        }
        for (const l of geo.labels) {
            S.push(`<text class="${l.kind === 'mult' ? 'cmult' : 'clbl'}" x="${f(l.x)}" y="${baseline(l.y, l.kind === 'mult' ? MONO : LABEL_SIZE)}">${esc(l.text)}</text>`);
        }
        S.push('</svg>');
        return S.join('');
    }

    // One call for a picture. Returns { svg, graph, model }.
    function diagram(text, options) {
        const opts = Object.assign({ measure: estimateWidth }, options || {});
        const model = parse(text);
        const graph = classGraph(model, opts.measure);
        layout(graph, opts);
        return { svg: renderSVG(graph, opts), graph, model };
    }

    return {
        VERSION, parse, compile, summary, classGraph, layout, geometry, renderSVG, diagram, viewBox,
        estimateWidth, esc, typeText, readType,
    };
});

// ---------- command line --------------------------------------------------------
if (typeof require !== 'undefined' && typeof module !== 'undefined' && require.main === module) {
    const fs = require('fs');
    const core = module.exports;
    const args = process.argv.slice(2);
    const usage = 'node uml-core.js --json|--svg <datei.uml>';
    const [mode, file] = args;
    if (!['--json', '--svg'].includes(mode) || !file || args.length !== 2) {
        process.stderr.write(`${usage}\n`);
        process.exit(2);
    }
    let text;
    try {
        text = fs.readFileSync(file, 'utf8');
    } catch (e) {
        if (mode === '--json') {
            process.stdout.write(JSON.stringify({ version: core.VERSION, missing: true, errors: [{ line: 0, msg: `Die Datei ${file} gibt es nicht.` }] }));
            process.exit(0);
        }
        process.stderr.write(`Die Datei ${file} gibt es nicht.\n`);
        process.exit(1);
    }
    const model = core.parse(text);
    if (mode === '--json') {
        process.stdout.write(JSON.stringify(core.summary(model)));
    } else if (model.errors.length) {
        for (const p of model.errors) process.stderr.write(`${file}:${p.line}: ${p.msg}\n`);
        process.exit(1);
    } else {
        // A white background, like »Diagramm als SVG speichern«: the file is also read on dark pages.
        process.stdout.write(core.diagram(text, { background: '#ffffff' }).svg + '\n');
    }
}
