// ER-Werkstatt core: an ER model written as text (.er), its translation into
// tables, MySQL, and the diagrams (Chen and crow's foot) as SVG strings.
//
// One module for every place that shows or checks a model: the VS Code
// extension (hackschule-er), the website and the Workspace checks. Plain
// JavaScript without dependencies; no DOM. Keywords and identifiers are
// English, messages for students German.
//
//   # comments start with #
//   entity movie
//     id: int, key
//     title: text
//     rating: decimal(3,1), optional
//
//   entity genre
//     id: int, key
//     name: text, unique
//
//   relationship has_genre: movie n:m genre        -> table movie_genre
//   relationship credit: movie, crew, job as movie_crew   (three-way)
//
// Command line (used by the Workspace checks and for exports):
//   node er-core.js --json film.er     model, tables and problems as JSON
//   node er-core.js --sql film.er      MySQL
//   node er-core.js --svg chen|crowfoot|tables film.er
(function (root, factory) {
    if (typeof module === 'object' && module.exports) module.exports = factory();
    else root.ErCore = factory();
})(typeof self !== 'undefined' ? self : this, function () {
    'use strict';

    const VERSION = '0.1.0';
    const NAME = '[\\p{L}_][\\p{L}\\p{N}_]*';
    const RE_ENTITY = new RegExp(`^entity\\s+(${NAME})\\s*$`, 'u');
    const RE_BINARY = new RegExp(`^relationship\\s+(${NAME})\\s*:\\s*(${NAME})\\s+([1nm])\\s*:\\s*([1nm])\\s+(${NAME})(?:\\s+as\\s+(${NAME}))?\\s*$`, 'u');
    const RE_NARY = new RegExp(`^relationship\\s+(${NAME})\\s*:\\s*(${NAME}(?:\\s*,\\s*${NAME})+)(?:\\s+as\\s+(${NAME}))?\\s*$`, 'u');
    const RE_ATTR = new RegExp(`^(${NAME})\\s*(?::\\s*(.*))?$`, 'u');
    const RE_TYPE = /^(int|text|varchar\(\s*\d+\s*\)|decimal\(\s*\d+\s*,\s*\d+\s*\)|float|date|datetime|bool)$/;
    const FLAGS = ['key', 'optional', 'unique'];
    const TYPE_LIST = 'int, text, varchar(n), decimal(p,s), float, date, datetime, bool';

    // ---------- reading ----------------------------------------------------------

    function splitTopLevel(s) {
        const out = [];
        let depth = 0;
        let cur = '';
        for (const ch of s) {
            if (ch === '(') depth++;
            if (ch === ')') depth--;
            if (ch === ',' && depth === 0) { out.push(cur); cur = ''; } else cur += ch;
        }
        if (cur.trim() !== '') out.push(cur);
        return out.map((x) => x.trim()).filter(Boolean);
    }

    // Problems carry line and column range (1-based line, 0-based columns) so an
    // editor can underline them.
    function problem(line, msg, raw, from, to) {
        const start = from === undefined ? (raw ? raw.search(/\S|$/) : 0) : from;
        const end = to === undefined ? (raw ? raw.replace(/\s+$/, '').length : start) : to;
        return { line, msg, from: start, to: Math.max(end, start + 1) };
    }

    // Returns { entities, relations, errors, warnings, byName }.
    // entity:   { name, attrs, line }
    // relation: { name, ends: [entity names], card: ['1'|'n', ...] (binary only),
    //             cardText, nary, table (from »as«, or null), attrs, line }
    // attr:     { name, type, key, optional, unique, line }
    function parse(text) {
        const entities = [];
        const relations = [];
        const errors = [];
        const warnings = [];
        let current = null;
        String(text).split('\n').forEach((raw0, i) => {
            const raw = raw0.replace(/\r$/, '');
            const line = i + 1;
            const noComment = raw.replace(/#.*$/, '');
            if (!noComment.trim()) return;
            const indented = /^\s/.test(noComment);
            const s = noComment.trim();
            if (!indented) {
                let m;
                if ((m = s.match(RE_ENTITY))) {
                    current = { kind: 'entity', name: m[1], attrs: [], line };
                    entities.push(current);
                } else if ((m = s.match(RE_BINARY))) {
                    const cardText = [m[3], m[4]];
                    const card = cardText.map((c) => (c.toLowerCase() === '1' ? '1' : 'n'));
                    current = { kind: 'relation', name: m[1], ends: [m[2], m[5]], card, cardText,
                        nary: false, table: m[6] || null, attrs: [], line };
                    relations.push(current);
                } else if ((m = s.match(RE_NARY))) {
                    const ends = m[2].split(',').map((x) => x.trim());
                    current = { kind: 'relation', name: m[1], ends, card: ends.map(() => 'n'), cardText: ends.map(() => 'n'),
                        nary: true, table: m[3] || null, attrs: [], line };
                    relations.push(current);
                    if (ends.length === 2) {
                        errors.push(problem(line, 'Eine Beziehung zwischen zwei Entitäten braucht ihre Kardinalität, zum Beispiel »relationship has_genre: movie n:m genre«.', raw));
                    }
                } else {
                    current = null;
                    let msg = 'Unbekannte Zeile. Erwartet wird »entity name« oder »relationship name: a 1:n b« (Schlüsselwörter klein und auf Englisch).';
                    if (/^(Entity|ENTITY|Relationship|RELATIONSHIP)\b/.test(s)) msg = 'Schlüsselwörter schreibst du klein: »entity«, »relationship«.';
                    else if (/^(entität|entitaet|beziehung|tabelle)\b/i.test(s)) msg = 'Die Schlüsselwörter sind englisch: »entity« und »relationship«.';
                    else if (/^relationship\b/.test(s) && !/^relationship\s+\S+?\s*:/.test(s)) msg = 'Nach dem Namen der Beziehung kommt ein Doppelpunkt: »relationship has_genre: movie n:m genre«.';
                    else if (/^relationship\b/.test(s)) msg = 'So sieht eine Beziehung aus: »relationship has_genre: movie n:m genre«. Kardinalitäten: 1:1, 1:n, n:1, n:m.';
                    else if (/^entity\b/.test(s)) msg = 'Der Name einer Entität ist ein Wort ohne Leerzeichen, zum Beispiel »entity movie«.';
                    errors.push(problem(line, msg, raw));
                }
                return;
            }
            if (!current) {
                errors.push(problem(line, 'Eingerückte Zeile ohne »entity« oder »relationship« darüber.', raw));
                return;
            }
            const m = s.match(RE_ATTR);
            if (!m) {
                errors.push(problem(line, 'Attribute schreibst du als »name: type, zusatz«, zum Beispiel »id: int, key«.', raw));
                return;
            }
            const attr = { name: m[1], type: 'text', key: false, optional: false, unique: false, line };
            const offset = raw.indexOf(s);
            for (const part of splitTopLevel(m[2] || '')) {
                const at = raw.indexOf(part, offset);
                const p = part.replace(/\s+/g, '');
                if (RE_TYPE.test(p)) attr.type = p;
                else if (FLAGS.includes(p)) attr[p] = true;
                else if (RE_TYPE.test(p.toLowerCase()) || FLAGS.includes(p.toLowerCase())) {
                    errors.push(problem(line, `»${part}« schreibst du klein: »${p.toLowerCase()}«.`, raw, at, at + part.length));
                } else {
                    errors.push(problem(line, `»${part}« ist weder ein Datentyp (${TYPE_LIST}) noch ein Zusatz (key, optional, unique).`, raw, at, at + part.length));
                }
            }
            if (attr.key && attr.optional) errors.push(problem(line, 'Ein Schlüsselattribut kann nicht optional sein.', raw));
            if (current.attrs.some((a) => a.name.toLowerCase() === attr.name.toLowerCase())) {
                errors.push(problem(line, `Das Attribut »${attr.name}« gibt es hier schon.`, raw));
            }
            current.attrs.push(attr);
        });

        const byName = new Map();
        for (const e of entities) {
            const k = e.name.toLowerCase();
            if (byName.has(k)) errors.push(problem(e.line, `Die Entität »${e.name}« gibt es schon.`));
            byName.set(k, e);
            if (!e.attrs.some((a) => a.key)) {
                errors.push(problem(e.line, `»${e.name}« braucht ein Schlüsselattribut (»key«), das jeden Datensatz eindeutig macht.`));
            }
        }
        const relNames = new Set();
        for (const r of relations) {
            for (const side of r.ends) {
                if (!byName.has(side.toLowerCase())) errors.push(problem(r.line, `Die Entität »${side}« ist nicht definiert.`));
            }
            const k = r.name.toLowerCase();
            if (relNames.has(k)) errors.push(problem(r.line, `Die Beziehung »${r.name}« gibt es schon.`));
            relNames.add(k);
        }
        return { entities, relations, errors, warnings, byName };
    }

    // ---------- translating into tables -----------------------------------------

    function sqlName(name) {
        return String(name).replace(/ä/g, 'ae').replace(/ö/g, 'oe').replace(/ü/g, 'ue')
            .replace(/Ä/g, 'Ae').replace(/Ö/g, 'Oe').replace(/Ü/g, 'Ue').replace(/ß/g, 'ss').toLowerCase();
    }

    function sqlType(t) {
        if (t === 'int') return 'INT';
        if (t === 'text') return 'VARCHAR(255)';
        if (t === 'float') return 'DOUBLE';
        if (t === 'date') return 'DATE';
        if (t === 'datetime') return 'DATETIME';
        if (t === 'bool') return 'BOOLEAN';
        return t.toUpperCase();
    }

    // The table of an n:m or three-way relationship: »as« if given, otherwise
    // the entity names joined (movie_genre), for a relationship of an entity
    // with itself its own name.
    function joinTableName(r) {
        if (r.table) return sqlName(r.table);
        const ends = r.ends.map(sqlName);
        if (new Set(ends).size < ends.length) return sqlName(r.name);
        return ends.join('_');
    }

    // Returns { tables, steps, warnings }. Problems that only show up in the
    // translation (a name clash) are added to model.errors.
    // table: { name, columns: [{ name, type, nullable }], pk, uniques, fks: [{ cols, refTable, refCols }], from }
    // step:  { tag, text } – text uses `backticks` for names
    function transform(model) {
        const tables = new Map();
        const steps = [];
        const warnings = [];
        const tableOf = (e) => tables.get(sqlName(e.name));
        for (const e of model.entities) {
            const t = { name: sqlName(e.name), columns: [], pk: [], uniques: [], fks: [], from: e.name };
            for (const a of e.attrs) {
                t.columns.push({ name: sqlName(a.name), type: sqlType(a.type), nullable: a.optional });
                if (a.key) t.pk.push(sqlName(a.name));
                if (a.unique && !a.key) t.uniques.push([sqlName(a.name)]);
            }
            tables.set(t.name, t);
            steps.push({ tag: 'Entität', text: `Die Entität ${e.name} wird zur Tabelle \`${t.name}\` mit dem Primärschlüssel \`(${t.pk.join(', ')})\`.` });
        }
        const keyColumns = (target, into, prefix) => {
            const tt = tableOf(target);
            return tt.pk.map((k) => {
                let name = `${tt.name}_${k}`;
                if (prefix || into.columns.some((c) => c.name === name)) name = `${prefix || 'ref'}_${name}`;
                const keyCol = tt.columns.find((c) => c.name === k);
                return { name, type: keyCol.type, ref: k, refTable: tt.name };
            });
        };
        for (const r of model.relations) {
            const ends = r.ends.map((n) => model.byName.get(n.toLowerCase()));
            if (ends.some((e) => !e || !tableOf(e) || tableOf(e).pk.length === 0)) continue;
            const relAttrs = r.attrs.map((a) => ({ name: sqlName(a.name), type: sqlType(a.type), nullable: a.optional, key: a.key }));
            const many = r.card.every((c) => c === 'n');
            if (many) {
                const name = joinTableName(r);
                if (tables.has(name)) {
                    model.errors.push(problem(r.line, `Die Beziehung »${r.name}« bräuchte eine Tabelle \`${name}\`, die es schon gibt. Gib ihr mit »as« einen anderen Namen.`));
                    continue;
                }
                const t = { name, columns: [], pk: [], uniques: [], fks: [], from: r.name };
                const seen = new Map();
                ends.forEach((e, i) => {
                    const twice = ends.filter((x) => x === e).length > 1;
                    const count = (seen.get(e) || 0) + 1;
                    seen.set(e, count);
                    const prefix = twice ? (count === 1 ? 'from' : count === 2 ? 'to' : `side${i + 1}`) : null;
                    const cols = keyColumns(e, t, prefix);
                    for (const c of cols) {
                        t.columns.push({ name: c.name, type: c.type, nullable: false });
                        t.pk.push(c.name);
                    }
                    t.fks.push({ cols: cols.map((c) => c.name), refTable: cols[0].refTable, refCols: cols.map((c) => c.ref) });
                });
                for (const ra of relAttrs) {
                    t.columns.push({ name: ra.name, type: ra.type, nullable: ra.nullable });
                    if (ra.key) t.pk.push(ra.name);
                }
                tables.set(t.name, t);
                const extra = relAttrs.filter((x) => x.key).map((x) => x.name);
                const who = ends.map((e) => e.name);
                const what = r.nary ? `Die Beziehung ${r.name} verbindet ${who.slice(0, -1).join(', ')} und ${who[who.length - 1]}` : `Die n:m-Beziehung ${r.name} zwischen ${who[0]} und ${who[1]}`;
                steps.push({ tag: r.nary ? `${ends.length}-stellig` : 'n:m', text: `${what} und bekommt eine eigene Tabelle \`${t.name}\`. Sie enthält die Schlüssel ${r.nary ? 'aller' : 'beider'} Seiten als Fremdschlüssel; zusammen${extra.length ? ` mit \`${extra.join(', ')}\`` : ''} bilden sie den Primärschlüssel \`(${t.pk.join(', ')})\`.` +
                    (relAttrs.length ? ` Die Attribute der Beziehung (${relAttrs.map((x) => `\`${x.name}\``).join(', ')}) kommen in diese Tabelle.` : '') });
            } else {
                // 1:n, n:1 or 1:1: the foreign key goes to the n side (for 1:1: the right side, made unique)
                const [L, R] = ends;
                const [cl, cr] = r.card;
                const one = cl === '1' && cr === 'n' ? L : cl === 'n' && cr === '1' ? R : L;
                const manySide = one === L ? R : L;
                const t = tableOf(manySide);
                const cols = keyColumns(one, t, L === R ? sqlName(r.name) : null);
                // a table that points to itself needs NULL somewhere, or the first row could never be inserted
                for (const c of cols) t.columns.push({ name: c.name, type: c.type, nullable: L === R });
                t.fks.push({ cols: cols.map((c) => c.name), refTable: cols[0].refTable, refCols: cols.map((c) => c.ref) });
                const isOne = cl === '1' && cr === '1';
                if (isOne) t.uniques.push(cols.map((c) => c.name));
                for (const ra of relAttrs) {
                    t.columns.push({ name: ra.name, type: ra.type, nullable: ra.nullable });
                    if (ra.key) warnings.push(problem(r.line, `Bei einer ${r.cardText.join(':')}-Beziehung wird »${ra.name}« ein normales Attribut von \`${t.name}\`; »key« wirkt hier nicht.`));
                }
                if (r.table) warnings.push(problem(r.line, `»as« gilt nur für Beziehungen mit eigener Tabelle (n:m oder mehrstellig).`));
                const fk = cols.map((c) => c.name).join(', ');
                steps.push({ tag: isOne ? '1:1' : '1:n', text: isOne
                    ? `Die 1:1-Beziehung ${r.name}: \`${t.name}\` bekommt den Fremdschlüssel \`${fk}\`, der zusätzlich eindeutig (UNIQUE) ist. So gehört zu jedem Datensatz höchstens ein Partner.`
                    : `Die 1:n-Beziehung ${r.name}: Jede Zeile in \`${t.name}\` merkt sich mit dem Fremdschlüssel \`${fk}\`, zu welchem ${one.name} sie gehört. Eine eigene Tabelle ist nicht nötig.` +
                      (relAttrs.length ? ` Die Attribute der Beziehung kommen ebenfalls in \`${t.name}\`.` : '') });
            }
        }
        return { tables: [...tables.values()], steps, warnings };
    }

    // ---------- MySQL -------------------------------------------------------------

    // options.source: the .er file it came from (named in the header)
    // options.drop:   start with DROP TABLE IF EXISTS (default true), so a
    //                 changed model is built again with one command
    function toSQL(tables, options) {
        const opts = Object.assign({ drop: true, source: null }, options || {});
        const out = [];
        out.push(`-- Erzeugt${opts.source ? ` aus ${opts.source}` : ''} mit der ER-Werkstatt ${VERSION}.`);
        out.push('-- Nicht von Hand ändern: Beim nächsten Erzeugen wird diese Datei überschrieben.');
        if (opts.drop && tables.length) {
            out.push('-- Vorhandene Tabellen mit diesen Namen werden gelöscht, samt Inhalt.');
            out.push('');
            out.push('SET FOREIGN_KEY_CHECKS = 0;');
            out.push(`DROP TABLE IF EXISTS ${tables.map((t) => t.name).join(', ')};`);
            out.push('SET FOREIGN_KEY_CHECKS = 1;');
        }
        for (const t of tables) {
            const width = Math.max(...t.columns.map((c) => c.name.length), 0) + 1;
            const lines = t.columns.map((c) => `  ${c.name.padEnd(width)} ${c.type}${c.nullable ? ' NULL' : ' NOT NULL'}`);
            if (t.pk.length) lines.push(`  PRIMARY KEY (${t.pk.join(', ')})`);
            for (const u of t.uniques) lines.push(`  UNIQUE (${u.join(', ')})`);
            out.push('');
            out.push(`CREATE TABLE ${t.name} (\n${lines.join(',\n')}\n);`);
        }
        for (const t of tables) {
            for (const fk of t.fks) {
                out.push('');
                out.push(`ALTER TABLE ${t.name}\n  ADD FOREIGN KEY (${fk.cols.join(', ')})\n  REFERENCES ${fk.refTable} (${fk.refCols.join(', ')});`);
            }
        }
        return out.join('\n') + '\n';
    }

    // What a check needs to know, as plain data.
    function summary(model, result) {
        return {
            version: VERSION,
            errors: model.errors,
            warnings: model.warnings.concat(result ? result.warnings : []),
            entities: model.entities.map((e) => ({ name: e.name, line: e.line,
                attributes: e.attrs.map((a) => ({ name: a.name, type: a.type, key: a.key, optional: a.optional, unique: a.unique })) })),
            relationships: model.relations.map((r) => ({ name: r.name, line: r.line, ends: r.ends, cardinality: r.nary ? r.card : r.cardText.map((c) => c.toLowerCase()),
                table: r.card.every((c) => c === 'n') ? joinTableName(r) : null,
                attributes: r.attrs.map((a) => ({ name: a.name, type: a.type, key: a.key, optional: a.optional })) })),
            tables: result ? result.tables : [],
        };
    }

    // parse + transform in one go; a model with errors has no tables.
    function compile(text) {
        const model = parse(text);
        const result = model.errors.length ? { tables: [], steps: [], warnings: [] } : transform(model);
        return { model, result };
    }

    // ---------- diagrams --------------------------------------------------------

    // Width of a text in px at 13px IBM Plex Sans, without a browser. Good
    // enough to size boxes; a renderer with a canvas can pass its own.
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

    const ROW_H = 20;
    const HEAD_H = 28;

    // Chen: entities (boxes), relationships (diamonds), attributes (ellipses).
    function chenGraph(model, measure) {
        const tw = measure || estimateWidth;
        const nodes = [];
        const edges = [];
        for (const e of model.entities) {
            nodes.push({ id: `E:${e.name}`, kind: 'E', label: e.name, w: Math.max(90, tw(e.name, 600) + 30), h: 40 });
            for (const a of e.attrs) {
                nodes.push({ id: `A:${e.name}.${a.name}`, kind: 'A', label: a.name, key: a.key, owner: `E:${e.name}`, w: tw(a.name) + 26, h: 30 });
                edges.push({ a: `E:${e.name}`, b: `A:${e.name}.${a.name}`, kind: 'attr' });
            }
        }
        for (const r of model.relations) {
            const ends = r.ends.map((n) => model.byName.get(n.toLowerCase()));
            if (ends.some((e) => !e)) continue;
            const id = `R:${r.name}`;
            nodes.push({ id, kind: 'R', label: r.name, w: Math.max(100, tw(r.name, 500) * 1.6 + 36), h: 56, ends: ends.map((e) => `E:${e.name}`) });
            ends.forEach((e, i) => {
                const earlier = ends.slice(0, i).filter((x) => x === e).length;
                edges.push({ a: id, b: `E:${e.name}`, kind: 'rel', card: r.cardText[i], offset: earlier * 14 });
            });
            for (const a of r.attrs) {
                nodes.push({ id: `A:${r.name}.${a.name}`, kind: 'A', label: a.name, key: a.key, owner: id, w: tw(a.name) + 26, h: 30 });
                edges.push({ a: id, b: `A:${r.name}.${a.name}`, kind: 'attr' });
            }
        }
        return { nodes, edges };
    }

    function boxNode(id, label, rows, tw) {
        const w = Math.max(tw(label, 600) + 28, ...rows.map((r) => 48 + tw(r.name) + 16 + tw(r.type) * 0.92 + 12));
        return { id, kind: 'T', label, rows, w: Math.max(120, w), h: HEAD_H + rows.length * ROW_H + 6 };
    }

    // Crow's foot. resolved = false: the model (entities, n:m as one line, a
    // three-way relationship as its own box); true: the tables after the
    // translation, join tables and foreign keys visible.
    function crowfootGraph(model, result, resolved, measure) {
        const tw = measure || estimateWidth;
        const nodes = [];
        const edges = [];
        if (!resolved) {
            for (const e of model.entities) {
                nodes.push(boxNode(`C:${e.name}`, e.name, e.attrs.map((a) => ({ name: a.name, type: a.type, tag: a.key ? 'PK' : '' })), tw));
            }
            for (const r of model.relations) {
                const ends = r.ends.map((n) => model.byName.get(n.toLowerCase()));
                if (ends.some((e) => !e)) continue;
                if (r.nary) {
                    nodes.push(boxNode(`N:${r.name}`, r.name, r.attrs.map((a) => ({ name: a.name, type: a.type, tag: a.key ? 'PK' : '' })), tw));
                    for (const e of ends) edges.push({ a: `N:${r.name}`, b: `C:${e.name}`, kind: 'cf', endA: 'many', endB: 'one', label: '' });
                    continue;
                }
                const label = r.name + (r.attrs.length ? ` (${r.attrs.map((a) => a.name).join(', ')})` : '');
                edges.push({ a: `C:${ends[0].name}`, b: `C:${ends[1].name}`, kind: 'cf', endA: r.card[0] === '1' ? 'one' : 'many', endB: r.card[1] === '1' ? 'one' : 'many', label });
            }
        } else {
            for (const t of result.tables) {
                const fkCols = new Set(t.fks.flatMap((f) => f.cols));
                nodes.push(boxNode(`T:${t.name}`, t.name, t.columns.map((c) => ({ name: c.name, type: c.type.toLowerCase(),
                    tag: [t.pk.includes(c.name) ? 'PK' : '', fkCols.has(c.name) ? 'FK' : ''].filter(Boolean).join(',') })), tw));
            }
            for (const t of result.tables) {
                for (const fk of t.fks) {
                    const unique = t.uniques.some((u) => u.join() === fk.cols.join()) || t.pk.join() === fk.cols.join();
                    edges.push({ a: `T:${t.name}`, b: `T:${fk.refTable}`, kind: 'cf', endA: unique ? 'one' : 'many', endB: 'one', label: '' });
                }
            }
        }
        return { nodes, edges };
    }

    // Places the nodes: deterministic (same model, same picture). positions:
    // { id: { x, y } } from earlier (dragged boxes stay where they are).
    // Returns the positions of all nodes.
    function layout(graph, positions) {
        const { nodes, edges } = graph;
        const kept = positions || {};
        const byId = new Map(nodes.map((n) => [n.id, n]));
        const fixed = new Set();
        const take = (n) => {
            const p = kept[n.id];
            if (p && Number.isFinite(p.x) && Number.isFinite(p.y)) {
                n.x = p.x; n.y = p.y; fixed.add(n.id);
                return true;
            }
            return false;
        };
        const main = nodes.filter((n) => n.kind === 'E' || n.kind === 'T' || n.kind === 'N');
        main.forEach((n, i) => {
            if (take(n)) return;
            const ang = (i / Math.max(1, main.length)) * Math.PI * 2 - Math.PI / 2;
            const R = 140 + main.length * 45;
            n.x = Math.cos(ang) * R; n.y = Math.sin(ang) * R * 0.7;
        });
        for (const n of nodes.filter((x) => x.kind === 'R')) {
            if (take(n)) continue;
            const ends = n.ends.map((id) => byId.get(id));
            const same = new Set(ends).size < ends.length;
            n.x = ends.reduce((s, e) => s + e.x, 0) / ends.length + (same ? 160 : 0);
            n.y = ends.reduce((s, e) => s + e.y, 0) / ends.length + (same ? 0 : 12);
        }
        const attrsOf = new Map();
        for (const n of nodes.filter((x) => x.kind === 'A')) {
            if (!attrsOf.has(n.owner)) attrsOf.set(n.owner, []);
            attrsOf.get(n.owner).push(n);
        }
        for (const [owner, list] of attrsOf) {
            const o = byId.get(owner);
            list.forEach((n, i) => {
                if (take(n)) return;
                const away = Math.atan2(o.y, o.x || 0.001);
                const spread = Math.min(Math.PI * 1.6, 0.6 * list.length);
                const ang = away + (list.length === 1 ? 0 : -spread / 2 + spread * i / (list.length - 1));
                n.x = o.x + Math.cos(ang) * 110; n.y = o.y + Math.sin(ang) * 85;
            });
        }
        const iters = fixed.size ? 160 : 420;
        for (let it = 0; it < iters; it++) {
            const t = 1 - it / iters;
            for (let i = 0; i < nodes.length; i++) {
                const a = nodes[i];
                for (let j = i + 1; j < nodes.length; j++) {
                    const b = nodes[j];
                    let dx = b.x - a.x;
                    let dy = b.y - a.y;
                    const d = Math.hypot(dx, dy) || 0.01;
                    const min = (Math.max(a.w, a.h) + Math.max(b.w, b.h)) / 2 + 26;
                    if (d < min * 1.6) {
                        const f = ((min * 1.6 - d) / d) * 0.12 * t;
                        dx *= f; dy *= f;
                        if (!fixed.has(a.id)) { a.x -= dx; a.y -= dy; }
                        if (!fixed.has(b.id)) { b.x += dx; b.y += dy; }
                    }
                }
            }
            for (const e of edges) {
                const a = byId.get(e.a);
                const b = byId.get(e.b);
                if (!a || !b || a === b) continue;
                const want = e.kind === 'attr' ? 105 : e.kind === 'cf' ? 250 : 175;
                let dx = b.x - a.x;
                let dy = b.y - a.y;
                const d = Math.hypot(dx, dy) || 0.01;
                const f = ((d - want) / d) * 0.05 * t;
                dx *= f; dy *= f;
                if (!fixed.has(a.id)) { a.x += dx; a.y += dy; }
                if (!fixed.has(b.id)) { b.x -= dx; b.y -= dy; }
            }
        }
        const out = {};
        for (const n of nodes) {
            n.x = Math.round(n.x * 10) / 10;
            n.y = Math.round(n.y * 10) / 10;
            out[n.id] = { x: n.x, y: n.y };
        }
        return out;
    }

    function boundaryPoint(n, tx, ty) {
        const dx = tx - n.x;
        const dy = ty - n.y;
        if (!dx && !dy) return { x: n.x, y: n.y };
        if (n.kind === 'E' || n.kind === 'T' || n.kind === 'N') {
            const s = Math.min((n.w / 2) / Math.abs(dx || 1e-9), (n.h / 2) / Math.abs(dy || 1e-9));
            return { x: n.x + dx * s, y: n.y + dy * s };
        }
        if (n.kind === 'R') {
            const s = 1 / (Math.abs(dx) / (n.w / 2) + Math.abs(dy) / (n.h / 2));
            return { x: n.x + dx * s, y: n.y + dy * s };
        }
        const rx = n.w / 2;
        const ry = n.h / 2;
        const s = 1 / Math.sqrt((dx * dx) / (rx * rx) + (dy * dy) / (ry * ry));
        return { x: n.x + dx * s, y: n.y + dy * s };
    }

    function viewBox(nodes) {
        let x0 = Infinity; let y0 = Infinity; let x1 = -Infinity; let y1 = -Infinity;
        for (const n of nodes) {
            x0 = Math.min(x0, n.x - n.w / 2); x1 = Math.max(x1, n.x + n.w / 2);
            y0 = Math.min(y0, n.y - n.h / 2); y1 = Math.max(y1, n.y + n.h / 2);
        }
        if (!Number.isFinite(x0)) return [0, 0, 600, 380];
        const pad = 40;
        return [x0 - pad, y0 - pad, x1 - x0 + 2 * pad, Math.max(300, y1 - y0 + 2 * pad)].map((v) => Math.round(v));
    }

    function esc(s) {
        return String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    }

    // Colours as CSS variables with light defaults, so a page can theme them.
    const STYLE = `
    .ln{stroke:var(--er-muted,#6b7280);stroke-width:1.3;fill:none}
    .ent{fill:var(--er-shape,#fff);stroke:var(--er-ink,#1f2937);stroke-width:1.8}
    .rel{fill:var(--er-pen-soft,#e8effc);stroke:var(--er-pen,#2b5fb4);stroke-width:1.6}
    .att{fill:var(--er-shape,#fff);stroke:var(--er-muted,#6b7280);stroke-width:1.2}
    .att.key{stroke:var(--er-red,#b42318)}
    .t{font-family:"IBM Plex Sans",system-ui,sans-serif;font-size:13px;fill:var(--er-ink,#1f2937);dominant-baseline:central;text-anchor:middle}
    .te{font-weight:600}.tr{fill:var(--er-pen,#2b5fb4);font-weight:500}.tk{fill:var(--er-red,#b42318)}
    .card{font-family:"IBM Plex Mono",monospace;font-size:13px;font-weight:500;fill:var(--er-pen,#2b5fb4);text-anchor:middle;dominant-baseline:central}
    .box{fill:var(--er-shape,#fff);stroke:var(--er-ink,#1f2937);stroke-width:1.6}
    .boxhead{fill:var(--er-pen-soft,#e8effc)}
    .cfl{stroke:var(--er-ink,#1f2937);stroke-width:1.4;fill:none}
    .row{font-family:"IBM Plex Sans",system-ui,sans-serif;font-size:12.5px;fill:var(--er-ink,#1f2937);dominant-baseline:central}
    .tag{font-family:"IBM Plex Mono",monospace;font-size:10.5px;font-weight:500;fill:var(--er-pen,#2b5fb4);dominant-baseline:central}
    .tag.pk{fill:var(--er-red,#b42318)}
    .typ{font-family:"IBM Plex Mono",monospace;font-size:11px;fill:var(--er-muted,#6b7280);dominant-baseline:central;text-anchor:end}
    .lbl{font-family:"IBM Plex Sans",system-ui,sans-serif;font-size:12px;fill:var(--er-pen,#2b5fb4);font-style:italic;text-anchor:middle;dominant-baseline:central;paint-order:stroke;stroke:var(--er-paper,#fff);stroke-width:5px}`;

    // The SVG of a laid-out graph, as a string.
    function renderSVG(graph, options) {
        const opts = Object.assign({ background: null, measure: estimateWidth, title: 'ER-Diagramm' }, options || {});
        const { nodes, edges } = graph;
        const byId = new Map(nodes.map((n) => [n.id, n]));
        const vb = opts.viewBox || viewBox(nodes);
        const S = [];
        S.push(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="${vb.join(' ')}" width="${vb[2]}" height="${vb[3]}" role="img" aria-label="${esc(opts.title)}">`);
        S.push(`<style>${STYLE}</style>`);
        if (opts.background) S.push(`<rect x="${vb[0]}" y="${vb[1]}" width="${vb[2]}" height="${vb[3]}" fill="${esc(opts.background)}"/>`);
        const f = (v) => v.toFixed(1);
        const footMark = (p, ux, uy, kind) => {
            const px = -uy;
            const py = ux;
            if (kind === 'many') {
                const q = { x: p.x + ux * 16, y: p.y + uy * 16 };
                return [-1, 0, 1].map((s) => `<line class="cfl" x1="${f(q.x)}" y1="${f(q.y)}" x2="${f(p.x + px * 9 * s)}" y2="${f(p.y + py * 9 * s)}"/>`).join('');
            }
            const r = { x: p.x + ux * 12, y: p.y + uy * 12 };
            return `<line class="cfl" x1="${f(r.x + px * 8)}" y1="${f(r.y + py * 8)}" x2="${f(r.x - px * 8)}" y2="${f(r.y - py * 8)}"/>`;
        };
        for (const e of edges) {
            const a = byId.get(e.a);
            const b = byId.get(e.b);
            if (!a || !b) continue;
            if (e.kind === 'cf') {
                if (a === b) {
                    const x = a.x + a.w / 2;
                    const y1 = a.y - 10;
                    const y2 = a.y + 10;
                    S.push(`<path class="cfl" d="M${f(x)},${f(y1)} C${f(x + 70)},${f(y1 - 50)} ${f(x + 70)},${f(y2 + 50)} ${f(x)},${f(y2)}"/>`);
                    S.push(footMark({ x, y: y1 }, 1, 0, e.endA) + footMark({ x, y: y2 }, 1, 0, e.endB));
                    if (e.label) S.push(`<text class="lbl" x="${f(x + 62)}" y="${f(a.y)}">${esc(e.label)}</text>`);
                    continue;
                }
                const pa = boundaryPoint(a, b.x, b.y);
                const pb = boundaryPoint(b, a.x, a.y);
                const len = Math.hypot(pb.x - pa.x, pb.y - pa.y) || 1;
                const ux = (pb.x - pa.x) / len;
                const uy = (pb.y - pa.y) / len;
                S.push(`<line class="cfl" x1="${f(pa.x)}" y1="${f(pa.y)}" x2="${f(pb.x)}" y2="${f(pb.y)}"/>`);
                S.push(footMark(pa, ux, uy, e.endA) + footMark(pb, -ux, -uy, e.endB));
                if (e.label) S.push(`<text class="lbl" x="${f((pa.x + pb.x) / 2)}" y="${f((pa.y + pb.y) / 2 - 10)}">${esc(e.label)}</text>`);
                continue;
            }
            const ax = a.x;
            const ay = a.y + (e.offset || 0);
            const bx = b.x;
            const by = b.y + (e.offset || 0);
            S.push(`<line class="ln" x1="${f(ax)}" y1="${f(ay)}" x2="${f(bx)}" y2="${f(by)}"/>`);
            if (e.kind === 'rel' && e.card) {
                const p = boundaryPoint(b, ax, ay);
                const len = Math.hypot(ax - p.x, ay - p.y) || 1;
                const ux = (ax - p.x) / len;
                const uy = (ay - p.y) / len;
                S.push(`<text class="card" x="${f(p.x + ux * 14 - uy * 10)}" y="${f(p.y + uy * 14 + ux * 10)}">${esc(e.card)}</text>`);
            }
        }
        for (const n of nodes) {
            const g = `<g class="node" data-id="${esc(n.id)}" transform="translate(${f(n.x)},${f(n.y)})">`;
            if (n.kind === 'T') {
                const x0 = -n.w / 2;
                const y0 = -n.h / 2;
                const rows = n.rows.map((r, i) => {
                    const y = y0 + HEAD_H + 3 + ROW_H * i + ROW_H / 2;
                    return (r.tag ? `<text class="tag${r.tag.startsWith('PK') ? ' pk' : ''}" x="${f(x0 + 8)}" y="${f(y)}">${esc(r.tag.replace(',', '·'))}</text>` : '') +
                        `<text class="row" x="${f(x0 + 48)}" y="${f(y)}">${esc(r.name)}</text><text class="typ" x="${f(-x0 - 8)}" y="${f(y)}">${esc(r.type)}</text>`;
                }).join('');
                S.push(`${g}<rect class="box" x="${f(x0)}" y="${f(y0)}" width="${f(n.w)}" height="${f(n.h)}"/><rect class="boxhead" x="${f(x0 + 0.8)}" y="${f(y0 + 0.8)}" width="${f(n.w - 1.6)}" height="${f(HEAD_H - 0.8)}"/>` +
                    `<line x1="${f(x0)}" y1="${f(y0 + HEAD_H)}" x2="${f(-x0)}" y2="${f(y0 + HEAD_H)}" stroke="var(--er-ink,#1f2937)" stroke-width="1"/>` +
                    `<text class="t te" x="0" y="${f(y0 + HEAD_H / 2)}">${esc(n.label)}</text>${rows}</g>`);
            } else if (n.kind === 'E') {
                S.push(`${g}<rect class="ent" x="${f(-n.w / 2)}" y="${f(-n.h / 2)}" width="${f(n.w)}" height="${f(n.h)}"/><text class="t te" x="0" y="0">${esc(n.label)}</text></g>`);
            } else if (n.kind === 'R') {
                S.push(`${g}<polygon class="rel" points="0,${f(-n.h / 2)} ${f(n.w / 2)},0 0,${f(n.h / 2)} ${f(-n.w / 2)},0"/><text class="t tr" x="0" y="0">${esc(n.label)}</text></g>`);
            } else {
                const tw = opts.measure(n.label);
                S.push(`${g}<ellipse class="att${n.key ? ' key' : ''}" rx="${f(n.w / 2)}" ry="${f(n.h / 2)}"/><text class="t${n.key ? ' tk' : ''}" x="0" y="0">${esc(n.label)}</text>` +
                    (n.key ? `<line x1="${f(-tw / 2)}" y1="9" x2="${f(tw / 2)}" y2="9" stroke="var(--er-red,#b42318)" stroke-width="1.2"/>` : '') + '</g>');
            }
        }
        S.push('</svg>');
        return S.join('');
    }

    // One call for a picture: kind 'chen', 'crowfoot' (the model) or 'tables'
    // (crow's foot after the translation). Returns { svg, positions }.
    function diagram(text, kind, options) {
        const opts = Object.assign({ positions: null, measure: estimateWidth }, options || {});
        const { model, result } = compile(text);
        const graph = kind === 'chen' ? chenGraph(model, opts.measure)
            : crowfootGraph(model, result, kind === 'tables', opts.measure);
        const positions = layout(graph, opts.positions);
        const label = { chen: 'ER-Diagramm (Chen)', crowfoot: 'ER-Diagramm (Krähenfuß)', tables: 'Tabellen (Krähenfuß)' }[kind] || 'ER-Diagramm';
        return { svg: renderSVG(graph, { measure: opts.measure, title: label, background: opts.background }), positions, graph };
    }

    return {
        VERSION, parse, transform, compile, toSQL, summary, sqlName, sqlType, joinTableName,
        chenGraph, crowfootGraph, layout, renderSVG, diagram, estimateWidth, esc,
    };
});

// ---------- command line --------------------------------------------------------
if (typeof require !== 'undefined' && typeof module !== 'undefined' && require.main === module) {
    const fs = require('fs');
    const path = require('path');
    const core = module.exports;
    const args = process.argv.slice(2);
    const usage = 'node er-core.js --json|--sql|--svg chen|crowfoot|tables <datei.er>';
    const mode = args[0];
    const kind = mode === '--svg' ? args[1] : null;
    const file = mode === '--svg' ? args[2] : args[1];
    if (!['--json', '--sql', '--svg'].includes(mode) || !file || (mode === '--svg' && !['chen', 'crowfoot', 'tables'].includes(kind))) {
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
    const { model, result } = core.compile(text);
    if (mode === '--json') {
        process.stdout.write(JSON.stringify(core.summary(model, result)));
    } else if (model.errors.length) {
        for (const p of model.errors) process.stderr.write(`${file}:${p.line}: ${p.msg}\n`);
        process.exit(1);
    } else if (mode === '--sql') {
        process.stdout.write(core.toSQL(result.tables, { source: path.basename(file) }));
    } else {
        process.stdout.write(core.diagram(text, kind).svg + '\n');
    }
}
