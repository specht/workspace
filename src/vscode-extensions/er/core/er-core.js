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

    const VERSION = '0.3.0';
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
                errors.push(problem(line, 'Attribute schreibst du als »name: typ, zusatz«, zum Beispiel »id: int, primary key«.', raw));
                return;
            }
            const attr = { name: m[1], type: 'text', key: false, optional: false, unique: false, line };
            const offset = raw.indexOf(s);
            for (const part of splitTopLevel(m[2] || '')) {
                const at = raw.indexOf(part, offset);
                const p = part.replace(/\s+/g, '');
                // »primary key«, as in SQL; »key« alone still works, with a hint.
                if (/^primary\s+key$/.test(part.trim())) attr.key = true;
                else if (/^primary\s+key$/i.test(part.trim())) {
                    errors.push(problem(line, `»${part}« schreibst du klein: »primary key«.`, raw, at, at + part.length));
                } else if (p === 'key') {
                    attr.key = true;
                    warnings.push(problem(line, 'Schreib »primary key« statt »key«: So heißt der Primärschlüssel auch in SQL.', raw, at, at + part.length));
                } else if (RE_TYPE.test(p)) attr.type = p;
                else if (FLAGS.includes(p)) attr[p] = true;
                else if (RE_TYPE.test(p.toLowerCase()) || FLAGS.includes(p.toLowerCase())) {
                    const lower = p.toLowerCase() === 'key' ? 'primary key' : p.toLowerCase();
                    errors.push(problem(line, `»${part}« schreibst du klein: »${lower}«.`, raw, at, at + part.length));
                } else {
                    errors.push(problem(line, `»${part}« ist weder ein Datentyp (${TYPE_LIST}) noch ein Zusatz (primary key, optional, unique).`, raw, at, at + part.length));
                }
            }
            if (attr.key && attr.optional) errors.push(problem(line, 'Ein Attribut des Primärschlüssels kann nicht optional sein.', raw));
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
                errors.push(problem(e.line, `»${e.name}« braucht einen Primärschlüssel (»primary key«), der jeden Datensatz eindeutig macht.`));
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
                steps.push({ tag: r.nary ? `${ends.length}-stellig` : 'n:m', text: `${what}${r.nary ? ' und' : ''} bekommt eine eigene Tabelle \`${t.name}\`. Sie enthält die Schlüssel ${r.nary ? 'aller' : 'beider'} Seiten als Fremdschlüssel; zusammen${extra.length ? ` mit \`${extra.join(', ')}\`` : ''} bilden sie den Primärschlüssel \`(${t.pk.join(', ')})\`.` +
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
                    if (ra.key) warnings.push(problem(r.line, `Bei einer ${r.cardText.join(':')}-Beziehung wird »${ra.name}« ein normales Attribut von \`${t.name}\`; »primary key« wirkt hier nicht.`));
                }
                if (r.table) warnings.push(problem(r.line, `»as« gilt nur für Beziehungen mit eigener Tabelle (n:m oder mehrstellig).`));
                const fk = cols.map((c) => c.name).join(', ');
                steps.push({ tag: isOne ? '1:1' : '1:n', text: isOne
                    ? `Die 1:1-Beziehung ${r.name}: \`${t.name}\` bekommt den Fremdschlüssel \`${fk}\`, der zusätzlich eindeutig (UNIQUE) ist. So gehört zu jedem Datensatz höchstens ein Partner.`
                    : `Die 1:n-Beziehung ${r.name}: Jede Zeile in \`${t.name}\` merkt sich mit dem Fremdschlüssel \`${fk}\`, zu welcher Zeile in \`${one.name}\` sie gehört. Eine eigene Tabelle ist nicht nötig.` +
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

    // A box in three columns, type | name | key, as Mermaid draws tables.
    function boxNode(id, label, rows, tw) {
        const cols = {
            type: Math.max(0, ...rows.map((r) => tw(r.type) * 0.92)),
            name: Math.max(0, ...rows.map((r) => tw(r.name))),
            key: rows.some((r) => r.tag) ? Math.max(...rows.map((r) => tw(r.tag.replace(',', ', ')) * 0.85)) : 0,
        };
        const w = Math.max(tw(label, 600) + 28, 10 + cols.type + 20 + cols.name + (cols.key ? 20 + cols.key : 0) + 10);
        return { id, kind: 'T', label, rows, cols, w: Math.max(120, w), h: HEAD_H + rows.length * ROW_H + 6 };
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

    // Angles in (-PI, PI].
    function normAngle(a) {
        let x = a % (Math.PI * 2);
        if (x <= -Math.PI) x += Math.PI * 2;
        if (x > Math.PI) x -= Math.PI * 2;
        return x;
    }

    // Where k attributes go around their owner, as angles: in the gaps
    // between the owner's lines to relationships (taken), each gap getting
    // attributes in proportion to its size, evenly spaced inside it. Without
    // lines, evenly around the owner starting at the top. Clockwise, so the
    // attributes keep the order in which they are written.
    function attributeAngles(k, taken) {
        if (k === 0) return [];
        const full = Math.PI * 2;
        if (taken.length === 0) {
            return Array.from({ length: k }, (_, i) => normAngle(-Math.PI / 2 + full * i / k));
        }
        const sorted = taken.map(normAngle).sort((a, b) => a - b);
        // Keep clear of each line, so narrow gaps between two lines stay empty.
        const margin = 0.45;
        let gaps = sorted.map((a, i) => {
            const b = i + 1 < sorted.length ? sorted[i + 1] : sorted[0] + full;
            return { from: a + margin, size: b - a - 2 * margin };
        }).filter((g) => g.size > 0.6);
        if (gaps.length === 0) {
            gaps = sorted.map((a, i) => {
                const b = i + 1 < sorted.length ? sorted[i + 1] : sorted[0] + full;
                return { from: a, size: b - a };
            });
        }
        // Largest remainder: whole shares first, the rest to the largest leftovers.
        const total = gaps.reduce((s, g) => s + g.size, 0) || full;
        const exact = gaps.map((g) => (k * g.size) / total);
        const count = exact.map(Math.floor);
        let left = k - count.reduce((s, c) => s + c, 0);
        exact.map((x, i) => ({ i, rest: x - Math.floor(x) }))
            .sort((a, b) => b.rest - a.rest || gaps[b.i].size - gaps[a.i].size || a.i - b.i)
            .forEach(({ i }) => { if (left > 0) { count[i] += 1; left -= 1; } });
        const angles = [];
        gaps.forEach((g, i) => {
            for (let j = 0; j < count[i]; j++) angles.push(normAngle(g.from + (g.size * (j + 0.5)) / count[i]));
        });
        // Clockwise from the top: the first attribute is the one nearest to 12 o'clock.
        const fromTop = (a) => normAngle(a + Math.PI / 2) + (normAngle(a + Math.PI / 2) < 0 ? full : 0);
        return angles.sort((a, b) => fromTop(a) - fromTop(b));
    }

    // How far attributes sit from their owner's centre: far enough that the
    // ellipses clear the owner and do not touch each other.
    function attributeRing(owner, attrs, angles) {
        let rx = owner.w / 2 + Math.max(...attrs.map((a) => a.w / 2)) + 34;
        let ry = owner.h / 2 + Math.max(...attrs.map((a) => a.h / 2)) + 30;
        const overlap = () => attrs.some((a, i) => attrs.some((b, j) => {
            if (j <= i) return false;
            const ax = Math.cos(angles[i]) * rx; const ay = Math.sin(angles[i]) * ry;
            const bx = Math.cos(angles[j]) * rx; const by = Math.sin(angles[j]) * ry;
            return Math.abs(ax - bx) < (a.w + b.w) / 2 + 10 && Math.abs(ay - by) < (a.h + b.h) / 2 + 6;
        }));
        for (let i = 0; i < 12 && overlap(); i++) { rx *= 1.12; ry *= 1.12; }
        return { rx, ry };
    }

    // Places the nodes: deterministic (same model, same picture). positions:
    // { id: { x, y } } from earlier (dragged boxes stay where they are).
    // Returns the positions of all nodes.
    //
    // Entities, relationships and boxes are placed first, each entity
    // claiming room for the ring of its attributes. Attributes are placed
    // last, around their owner in the gaps between its relationship lines,
    // so no attribute sits on a line and they are spread evenly.
    function layout(graph, positions) {
        const chen = graph.nodes.some((n) => n.kind === 'E' || n.kind === 'R' || n.kind === 'A');
        return chen ? forceLayout(graph, positions) : layeredLayout(graph, positions);
    }

    // Crow's foot and tables in layers, as Mermaid (dagre) draws them: the
    // side with »one« above the side with »many«, so a referenced table sits
    // above the tables that point to it. Steps: break cycles, give every box
    // a layer (longest path, a box with nothing above it moved down next to
    // what it points to), stand-ins for lines over several layers, order each
    // layer so few lines cross (barycentres, sweeping down and up), then x
    // towards the neighbours without overlap. Kept positions win; a new box
    // that would sit on one moves aside.
    const LAYER_GAP = 90;
    const BOX_GAP = 56;

    function layeredLayout(graph, positions) {
        const { nodes, edges } = graph;
        const kept = positions || {};
        const index = new Map(nodes.map((n, i) => [n.id, i]));
        // parent -> child: the »one« end above
        const pairs = [];
        const pairEdge = [];
        for (const e of edges) {
            delete e.via;
            if (e.a === e.b || !index.has(e.a) || !index.has(e.b)) continue;
            pairs.push(e.endB === 'one' && e.endA !== 'one' ? [e.b, e.a] : [e.a, e.b]);
            pairEdge.push(e);
        }
        // Break cycles: an edge back to a node on the current path turns round.
        const state = new Map();
        const out = new Map(nodes.map((n) => [n.id, []]));
        pairs.forEach(([u, v], i) => out.get(u).push({ v, i }));
        const dag = pairs.map((p) => p.slice());
        const visit = (u) => {
            state.set(u, 1);
            for (const { v, i } of out.get(u)) {
                if (state.get(v) === 1) dag[i] = [v, u];
                else if (!state.get(v)) visit(v);
            }
            state.set(u, 2);
        };
        nodes.forEach((n) => { if (!state.get(n.id)) visit(n.id); });
        const preds = new Map(nodes.map((n) => [n.id, []]));
        const succs = new Map(nodes.map((n) => [n.id, []]));
        for (const [u, v] of dag) { succs.get(u).push(v); preds.get(v).push(u); }
        // Layers by the longest path from the top.
        const rank = new Map();
        const rankOf = (v) => {
            if (rank.has(v)) return rank.get(v);
            rank.set(v, 0);
            const r = preds.get(v).length ? Math.max(...preds.get(v).map((u) => rankOf(u) + 1)) : 0;
            rank.set(v, r);
            return r;
        };
        nodes.forEach((n) => rankOf(n.id));
        // A box with nothing above it goes right above the highest of its children.
        for (let pass = 0; pass < nodes.length; pass++) {
            let moved = false;
            for (const n of nodes) {
                const kids = succs.get(n.id);
                if (preds.get(n.id).length || !kids.length) continue;
                const r = Math.min(...kids.map((k) => rank.get(k))) - 1;
                if (r > rank.get(n.id)) { rank.set(n.id, r); moved = true; }
            }
            if (!moved) break;
        }
        // A box with a loop to itself needs the loop's room on its right.
        const loopRoom = new Map();
        for (const e of edges) {
            if (e.a !== e.b || !index.has(e.a)) continue;
            const n = nodes[index.get(e.a)];
            loopRoom.set(n.id, selfLoop(n).out - (n.x || 0) - n.w / 2 + (e.label ? estimateWidth(e.label) + 10 : 0));
        }
        // Stand-ins where a line spans several layers.
        const vertices = nodes.map((n) => ({ id: n.id, w: n.w + (loopRoom.get(n.id) || 0), h: n.h, rank: rank.get(n.id) }));
        const vById = new Map(vertices.map((v) => [v.id, v]));
        const links = [];
        const standIns = [];
        dag.forEach(([u, v], i) => {
            let prev = u;
            const chain = [];
            for (let r = rank.get(u) + 1; r < rank.get(v); r++) {
                const id = `~${i}:${r}`;
                const d = { id, w: 24, h: 0, rank: r, dummy: true };
                vertices.push(d); vById.set(id, d);
                links.push([prev, id]);
                chain.push(id);
                prev = id;
            }
            links.push([prev, v]);
            if (chain.length) standIns.push({ edge: pairEdge[i], top: u, chain });
        });
        const up = new Map(vertices.map((v) => [v.id, []]));
        const down = new Map(vertices.map((v) => [v.id, []]));
        for (const [u, v] of links) { down.get(u).push(v); up.get(v).push(u); }
        const depth = Math.max(0, ...vertices.map((v) => v.rank)) + 1;
        let layers = Array.from({ length: depth }, () => []);
        vertices.forEach((v) => layers[v.rank].push(v.id));
        const pos = new Map();
        const renumber = () => layers.forEach((l) => l.forEach((id, i) => pos.set(id, i)));
        renumber();
        const crossings = () => {
            let c = 0;
            for (let r = 0; r + 1 < depth; r++) {
                const es = layers[r].flatMap((u) => down.get(u).map((v) => [pos.get(u), pos.get(v)]));
                for (let i = 0; i < es.length; i++) {
                    for (let j = i + 1; j < es.length; j++) {
                        if ((es[i][0] - es[j][0]) * (es[i][1] - es[j][1]) < 0) c++;
                    }
                }
            }
            return c;
        };
        let best = layers.map((l) => l.slice());
        let fewest = crossings();
        for (let it = 0; it < 16 && fewest > 0; it++) {
            const downward = it % 2 === 0;
            const order = downward ? [...Array(depth).keys()].slice(1) : [...Array(depth).keys()].reverse().slice(1);
            for (const r of order) {
                const near = downward ? up : down;
                const bary = (id) => {
                    const ns = near.get(id);
                    return ns.length ? ns.reduce((sum, n) => sum + pos.get(n), 0) / ns.length : pos.get(id);
                };
                layers[r] = layers[r].map((id) => [id, bary(id)]).sort((x, y) => x[1] - y[1] || pos.get(x[0]) - pos.get(y[0])).map((x) => x[0]);
                layers[r].forEach((id, i) => pos.set(id, i));
            }
            const c = crossings();
            if (c < fewest) { fewest = c; best = layers.map((l) => l.slice()); }
        }
        layers = best;
        renumber();
        // x: packed, then towards the neighbours without overlap.
        const x = new Map();
        const pack = (layer, want) => {
            const gap = (i) => (vById.get(layer[i - 1]).w + vById.get(layer[i]).w) / 2 + BOX_GAP;
            const fwd = want.slice();
            for (let i = 1; i < layer.length; i++) fwd[i] = Math.max(fwd[i], fwd[i - 1] + gap(i));
            const bwd = want.slice();
            for (let i = layer.length - 2; i >= 0; i--) bwd[i] = Math.min(bwd[i], bwd[i + 1] - gap(i + 1));
            const mix = want.map((_, i) => (fwd[i] + bwd[i]) / 2);
            for (let i = 1; i < layer.length; i++) mix[i] = Math.max(mix[i], mix[i - 1] + gap(i));
            layer.forEach((id, i) => x.set(id, mix[i]));
        };
        for (const layer of layers) {
            const total = layer.reduce((s, id) => s + vById.get(id).w, 0) + BOX_GAP * (layer.length - 1);
            let at = -total / 2;
            const want = layer.map((id) => { const c = at + vById.get(id).w / 2; at += vById.get(id).w + BOX_GAP; return c; });
            pack(layer, want);
        }
        for (let it = 0; it < 12; it++) {
            const downward = it % 2 === 0;
            const order = downward ? [...Array(depth).keys()] : [...Array(depth).keys()].reverse();
            for (const r of order) {
                const near = downward ? up : down;
                const layer = layers[r];
                const want = layer.map((id) => {
                    const ns = near.get(id).length ? near.get(id) : [...up.get(id), ...down.get(id)];
                    return ns.length ? ns.reduce((sum, n) => sum + x.get(n), 0) / ns.length : x.get(id);
                });
                pack(layer, want);
            }
        }
        // y: one band per layer, as high as its highest box.
        let top = 0;
        const yOf = [];
        for (const layer of layers) {
            const h = Math.max(0, ...layer.map((id) => vById.get(id).h));
            yOf.push(top + h / 2);
            top += h + LAYER_GAP;
        }
        const fixed = new Set();
        // moved: kept somewhere else than the layout would put it (dragged,
        // or kept from before the model changed)
        const moved = new Set();
        const round = (v) => Math.round(v * 10) / 10;
        for (const n of nodes) {
            const p = kept[n.id];
            const lx = x.get(n.id) - (loopRoom.get(n.id) || 0) / 2;
            const ly = yOf[rank.get(n.id)];
            if (p && Number.isFinite(p.x) && Number.isFinite(p.y)) {
                n.x = p.x; n.y = p.y; fixed.add(n.id);
                if (Math.abs(p.x - round(lx)) > 0.05 || Math.abs(p.y - round(ly)) > 0.05) moved.add(n.id);
            } else {
                n.x = lx; n.y = ly;
            }
        }
        // A line over several layers goes through the places of its
        // stand-ins, as long as its two boxes are where the layout put them.
        for (const { edge, top, chain } of standIns) {
            if (moved.has(edge.a) || moved.has(edge.b)) continue;
            const points = chain.map((id) => ({ x: Math.round(x.get(id) * 10) / 10, y: Math.round(yOf[vById.get(id).rank] * 10) / 10 }));
            edge.via = edge.a === top ? points : points.reverse();
        }
        // A new box among kept ones: aside, to the right, until it is free.
        if (fixed.size) {
            const placed = nodes.filter((n) => fixed.has(n.id));
            for (const n of nodes.filter((m) => !fixed.has(m.id))) {
                for (let guard = 0; guard < 50; guard++) {
                    const hit = placed.find((m) => Math.abs(m.x - n.x) < (m.w + n.w) / 2 + 30 && Math.abs(m.y - n.y) < (m.h + n.h) / 2 + 30);
                    if (!hit) break;
                    n.x = hit.x + (hit.w + n.w) / 2 + BOX_GAP;
                }
                placed.push(n);
            }
        }
        const result = {};
        for (const n of nodes) {
            n.x = Math.round(n.x * 10) / 10;
            n.y = Math.round(n.y * 10) / 10;
            result[n.id] = { x: n.x, y: n.y };
        }
        for (const e of edges) {
            if (!e.via) continue;
            const a = nodes[index.get(e.a)];
            const b = nodes[index.get(e.b)];
            e.viaFrom = { a: [a.x, a.y], b: [b.x, b.y] };
        }
        return result;
    }

    // The stand-in points of a line, while its boxes have not moved since the layout.
    function waypoints(e, a, b) {
        const w = e.viaFrom;
        if (!e.via || !w || w.a[0] !== a.x || w.a[1] !== a.y || w.b[0] !== b.x || w.b[1] !== b.y) return [];
        return e.via;
    }

    function forceLayout(graph, positions) {
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
        const attrsOf = new Map();
        for (const n of nodes.filter((x) => x.kind === 'A')) {
            if (!attrsOf.has(n.owner)) attrsOf.set(n.owner, []);
            attrsOf.get(n.owner).push(n);
        }
        // How far the attributes of a node reach from its centre (0 without).
        const ringOf = (n) => {
            const list = attrsOf.get(n.id);
            if (!list) return 0;
            const ring = attributeRing(n, list, attributeAngles(list.length, []));
            return Math.max(ring.rx, ring.ry);
        };
        // Room a node needs: itself, plus the ring of its attributes.
        const reach = (n) => {
            const own = Math.max(n.w, n.h) / 2;
            const list = attrsOf.get(n.id);
            if (!list) return own;
            const ring = attributeRing(n, list, attributeAngles(list.length, []));
            return Math.max(ring.rx, ring.ry) + Math.max(...list.map((a) => a.w / 2));
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

        const placed = nodes.filter((n) => n.kind !== 'A');
        const room = new Map(placed.map((n) => [n.id, reach(n)]));
        const ring = new Map(placed.map((n) => [n.id, ringOf(n)]));
        // A relationship sits in a gap of its entities' rings, so the two only
        // keep their own sizes apart; the line length does the rest.
        const linked = new Set(edges.filter((e) => e.kind === 'rel').flatMap((e) => [`${e.a}|${e.b}`, `${e.b}|${e.a}`]));
        const iters = fixed.size ? 160 : 420;
        for (let it = 0; it < iters; it++) {
            const t = 1 - it / iters;
            for (let i = 0; i < placed.length; i++) {
                const a = placed[i];
                for (let j = i + 1; j < placed.length; j++) {
                    const b = placed[j];
                    let dx = b.x - a.x;
                    let dy = b.y - a.y;
                    const d = Math.hypot(dx, dy) || 0.01;
                    // Neighbouring rings may interleave a little.
                    const min = linked.has(`${a.id}|${b.id}`)
                        ? (Math.max(a.w, a.h) + Math.max(b.w, b.h)) / 2 + 26
                        : (room.get(a.id) + room.get(b.id)) * 0.8 + 26;
                    if (d < min * 1.25) {
                        const f = ((min * 1.25 - d) / d) * 0.12 * t;
                        dx *= f; dy *= f;
                        if (!fixed.has(a.id)) { a.x -= dx; a.y -= dy; }
                        if (!fixed.has(b.id)) { b.x += dx; b.y += dy; }
                    }
                }
            }
            for (const e of edges) {
                if (e.kind === 'attr') continue;
                const a = byId.get(e.a);
                const b = byId.get(e.b);
                if (!a || !b || a === b) continue;
                // A relationship line runs between the attributes and ends
                // just beyond their ring.
                const want = e.kind === 'cf' ? 250 : Math.max(160, ring.get(b.id) + Math.max(a.w, a.h) / 2 + 20);
                let dx = b.x - a.x;
                let dy = b.y - a.y;
                const d = Math.hypot(dx, dy) || 0.01;
                const f = ((d - want) / d) * 0.05 * t;
                dx *= f; dy *= f;
                if (!fixed.has(a.id)) { a.x += dx; a.y += dy; }
                if (!fixed.has(b.id)) { b.x -= dx; b.y -= dy; }
            }
            // No line through a box or a diamond: a node too close to a line
            // between two others moves off it, the line the other way.
            for (const e of edges) {
                if (e.kind !== 'rel') continue;
                const a = byId.get(e.a);
                const b = byId.get(e.b);
                if (!a || !b || a === b) continue;
                const sx = b.x - a.x;
                const sy = b.y - a.y;
                const len2 = sx * sx + sy * sy || 1;
                for (const n of placed) {
                    if (n === a || n === b) continue;
                    const u = ((n.x - a.x) * sx + (n.y - a.y) * sy) / len2;
                    if (u <= 0.02 || u >= 0.98) continue;
                    let ox = n.x - (a.x + u * sx);
                    let oy = n.y - (a.y + u * sy);
                    const d = Math.hypot(ox, oy) || 0.01;
                    const min = Math.max(n.w, n.h) / 2 + 24;
                    if (d >= min) continue;
                    const f = ((min - d) / d) * 0.2 * t;
                    ox *= f; oy *= f;
                    if (!fixed.has(n.id)) { n.x += ox; n.y += oy; }
                    if (!fixed.has(a.id)) { a.x -= ox / 2; a.y -= oy / 2; }
                    if (!fixed.has(b.id)) { b.x -= ox / 2; b.y -= oy / 2; }
                }
            }
        }

        for (const [owner, list] of attrsOf) {
            const o = byId.get(owner);
            if (!o) continue;
            // The directions of the owner's lines to relationships or entities.
            const taken = edges
                .filter((e) => e.kind !== 'attr' && (e.a === owner || e.b === owner))
                .map((e) => byId.get(e.a === owner ? e.b : e.a))
                .filter((other) => other && other !== o)
                .map((other) => Math.atan2(other.y - o.y, other.x - o.x));
            const free = list.filter((n) => !take(n));
            const angles = attributeAngles(free.length, taken);
            const ring = attributeRing(o, free, angles);
            free.forEach((n, i) => {
                n.x = o.x + Math.cos(angles[i]) * ring.rx;
                n.y = o.y + Math.sin(angles[i]) * ring.ry;
            });
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

    // Where the crow's-foot lines meet their boxes: on the sides that face
    // each other (top and bottom when the boxes are one above the other,
    // else left and right), several lines on one side spread along it in the
    // order of their other ends, so two foreign keys to the same table get
    // two lines. Returns for each edge index { pa, na, pb, nb }: the points
    // and the outward normals of their sides.
    // How far a crow's-foot line runs straight out of its box: past the foot (16) and the bar (12).
    const STUB = 22;

    // The loop of a relationship of a box with itself, on its right side
    function selfLoop(n) {
        const x = n.x + n.w / 2;
        const d = Math.max(14, Math.min(26, n.h / 2 - 10));
        return { x, y1: n.y - d, y2: n.y + d, out: x + STUB + 52 };
    }

    function crowfootRoutes(byId, edges) {
        const sideOf = (a, b) => {
            const gapV = Math.max(b.y - b.h / 2 - (a.y + a.h / 2), a.y - a.h / 2 - (b.y + b.h / 2));
            const gapH = Math.max(b.x - b.w / 2 - (a.x + a.w / 2), a.x - a.w / 2 - (b.x + b.w / 2));
            // One above the other at all: top and bottom, as in the layers.
            if (gapV > 0 || gapV >= gapH) return b.y > a.y ? 'bottom' : 'top';
            return b.x > a.x ? 'right' : 'left';
        };
        const normal = { top: [0, -1], bottom: [0, 1], left: [-1, 0], right: [1, 0] };
        const ends = [];
        edges.forEach((e, i) => {
            const a = byId.get(e.a);
            const b = byId.get(e.b);
            if (e.kind !== 'cf' || !a || !b || a === b) return;
            const via = waypoints(e, a, b);
            const toA = via.length ? Object.assign({ w: 0, h: 0 }, via[0]) : b;
            const toB = via.length ? Object.assign({ w: 0, h: 0 }, via[via.length - 1]) : a;
            ends.push({ i, end: 'a', node: a, other: toA, side: sideOf(a, toA) });
            ends.push({ i, end: 'b', node: b, other: toB, side: sideOf(b, toB) });
        });
        const groups = new Map();
        for (const end of ends) {
            const key = `${end.node.id}|${end.side}`;
            if (!groups.has(key)) groups.set(key, []);
            groups.get(key).push(end);
        }
        const routes = {};
        for (const list of groups.values()) {
            const horizontal = list[0].side === 'top' || list[0].side === 'bottom';
            list.sort((p, q) => (horizontal ? p.other.x - q.other.x : p.other.y - q.other.y) || p.i - q.i || (p.end < q.end ? -1 : 1));
            const n = list[0].node;
            const length = horizontal ? n.w : n.h;
            const step = list.length > 1 ? Math.min(40, (length * 0.7) / (list.length - 1)) : 0;
            list.forEach((end, k) => {
                const offset = (k - (list.length - 1) / 2) * step;
                const [nx, ny] = normal[end.side];
                const p = horizontal ? { x: n.x + offset, y: n.y + ny * n.h / 2 } : { x: n.x + nx * n.w / 2, y: n.y + offset };
                routes[end.i] = Object.assign(routes[end.i] || {}, end.end === 'a' ? { pa: p, na: [nx, ny] } : { pb: p, nb: [nx, ny] });
            });
        }
        return routes;
    }

    // The area of the drawing: its boxes, and the points long lines pass through.
    function viewBox(nodes, edges) {
        let x0 = Infinity; let y0 = Infinity; let x1 = -Infinity; let y1 = -Infinity;
        for (const n of nodes) {
            x0 = Math.min(x0, n.x - n.w / 2); x1 = Math.max(x1, n.x + n.w / 2);
            y0 = Math.min(y0, n.y - n.h / 2); y1 = Math.max(y1, n.y + n.h / 2);
        }
        const byId = new Map(nodes.map((n) => [n.id, n]));
        for (const e of edges || []) {
            const a = byId.get(e.a);
            const b = byId.get(e.b);
            if (!a || !b) continue;
            for (const p of waypoints(e, a, b)) {
                x0 = Math.min(x0, p.x - 20); x1 = Math.max(x1, p.x + 20);
            }
            if (e.kind === 'cf' && a === b) {
                x1 = Math.max(x1, selfLoop(a).out + (e.label ? estimateWidth(e.label) + 10 : 0));
            }
        }
        if (!Number.isFinite(x0)) return [0, 0, 600, 380];
        const pad = 40;
        return [x0 - pad, y0 - pad, x1 - x0 + 2 * pad, Math.max(300, y1 - y0 + 2 * pad)].map((v) => Math.round(v));
    }

    function esc(s) {
        return String(s).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    }

    // Colours as CSS variables with light defaults, so a page can theme them.
    // Every rule is limited to the diagram (.er-diagram): an SVG embedded in a
    // page carries its <style> into the whole page, and names like .row or
    // .card would otherwise restyle the page's own elements.
    const STYLE_RULES = `
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
    .typ{font-family:"IBM Plex Mono",monospace;font-size:11px;fill:var(--er-muted,#6b7280);dominant-baseline:central}
    .zebra{fill:var(--er-pen-soft,#e8effc);opacity:.45}
    .colrule{stroke:var(--er-muted,#6b7280);stroke-width:.6;opacity:.5}
    .lbl{font-family:"IBM Plex Sans",system-ui,sans-serif;font-size:12px;fill:var(--er-pen,#2b5fb4);font-style:italic;text-anchor:middle;dominant-baseline:central;paint-order:stroke;stroke:var(--er-paper,#fff);stroke-width:5px}`;
    const STYLE = STYLE_RULES.replace(/(^|\})\s*(\.[^{]+)\{/g, '$1\n    .er-diagram $2{');

    // The SVG of a laid-out graph, as a string.
    function renderSVG(graph, options) {
        const opts = Object.assign({ background: null, measure: estimateWidth, title: 'ER-Diagramm' }, options || {});
        const { nodes, edges } = graph;
        const byId = new Map(nodes.map((n) => [n.id, n]));
        const vb = opts.viewBox || viewBox(nodes, edges);
        const S = [];
        S.push(`<svg xmlns="http://www.w3.org/2000/svg" class="er-diagram" viewBox="${vb.join(' ')}" width="${vb[2]}" height="${vb[3]}" role="img" aria-label="${esc(opts.title)}">`);
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
        const routes = crowfootRoutes(byId, edges);
        const cards = [];
        edges.forEach((e, i) => {
            const a = byId.get(e.a);
            const b = byId.get(e.b);
            if (!a || !b) return;
            if (e.kind === 'cf') {
                if (a === b) {
                    // A loop on the right side: its two ends far enough apart
                    // for a foot and a bar each, straight out of the box first.
                    const { x, y1, y2, out } = selfLoop(a);
                    S.push(`<path class="cfl" d="M${f(x)},${f(y1)} L${f(x + STUB)},${f(y1)} C${f(out)},${f(y1 - 12)} ${f(out)},${f(y2 + 12)} ${f(x + STUB)},${f(y2)} L${f(x)},${f(y2)}"/>`);
                    S.push(footMark({ x, y: y1 }, 1, 0, e.endA) + footMark({ x, y: y2 }, 1, 0, e.endB));
                    if (e.label) S.push(`<text class="lbl" x="${f(out - 12 + 10 + opts.measure(e.label) / 2)}" y="${f(a.y)}">${esc(e.label)}</text>`);
                    return;
                }
                // A curve that leaves each box at a right angle to its side.
                // Through the stand-ins of a long line, upright at each of them.
                // The first and last bit straight out of the box, through the
                // crow's foot or the bar, so the mark sits square on the edge.
                const { pa, na, pb, nb } = routes[i];
                const via = waypoints(e, a, b);
                const qa = { x: pa.x + na[0] * STUB, y: pa.y + na[1] * STUB };
                const qb = { x: pb.x + nb[0] * STUB, y: pb.y + nb[1] * STUB };
                const pts = [qa, ...via, qb];
                const dirs = [na, ...via.map(() => [0, pb.y > pa.y ? 1 : -1]), [-nb[0], -nb[1]]];
                let d = `M${f(pa.x)},${f(pa.y)} L${f(qa.x)},${f(qa.y)}`;
                let middle = null;
                for (let k = 0; k + 1 < pts.length; k++) {
                    const p = pts[k];
                    const q = pts[k + 1];
                    const reach = Math.max(30, Math.hypot(q.x - p.x, q.y - p.y) * 0.4);
                    const c1 = { x: p.x + dirs[k][0] * reach, y: p.y + dirs[k][1] * reach };
                    const c2 = { x: q.x - dirs[k + 1][0] * reach, y: q.y - dirs[k + 1][1] * reach };
                    d += ` C${f(c1.x)},${f(c1.y)} ${f(c2.x)},${f(c2.y)} ${f(q.x)},${f(q.y)}`;
                    if (k === Math.floor((pts.length - 2) / 2)) {
                        middle = { x: (p.x + 3 * c1.x + 3 * c2.x + q.x) / 8, y: (p.y + 3 * c1.y + 3 * c2.y + q.y) / 8 };
                    }
                }
                d += ` L${f(pb.x)},${f(pb.y)}`;
                S.push(`<path class="cfl" d="${d}"/>`);
                S.push(footMark(pa, na[0], na[1], e.endA) + footMark(pb, nb[0], nb[1], e.endB));
                if (e.label) S.push(`<text class="lbl" x="${f(middle.x)}" y="${f(middle.y)}">${esc(e.label)}</text>`);
                return;
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
                // The second line of a relationship with itself runs parallel
                // to the first; its label goes on the other side.
                const side = e.offset ? -1 : 1;
                // Further out along the line while it would sit on another label.
                let along = 14;
                const at = () => ({ x: p.x + ux * along - uy * 10 * side, y: p.y + uy * along + ux * 10 * side });
                for (let k = 0; k < 8 && cards.some((c) => Math.abs(c.x - at().x) < 14 && Math.abs(c.y - at().y) < 15); k++) along += 12;
                const q = at();
                cards.push(q);
                S.push(`<text class="card" x="${f(q.x)}" y="${f(q.y)}">${esc(e.card)}</text>`);
            }
        });
        for (const n of nodes) {
            const g = `<g class="node" data-id="${esc(n.id)}" transform="translate(${f(n.x)},${f(n.y)})">`;
            if (n.kind === 'T') {
                const x0 = -n.w / 2;
                const y0 = -n.h / 2;
                // Three columns, type | name | key, every other row shaded.
                const cols = n.cols || { type: 40, name: n.w - 80, key: 0 };
                const xName = x0 + 10 + cols.type + 20;
                const xKey = xName + cols.name + 20;
                const rowTop = (i) => y0 + HEAD_H + 3 + ROW_H * i;
                const shade = n.rows.map((r, i) => (i % 2 ? `<rect class="zebra" x="${f(x0 + 0.8)}" y="${f(rowTop(i))}" width="${f(n.w - 1.6)}" height="${f(ROW_H)}"/>` : '')).join('');
                const bottom = y0 + n.h;
                const rules = n.rows.length
                    ? `<line class="colrule" x1="${f(xName - 10)}" y1="${f(y0 + HEAD_H)}" x2="${f(xName - 10)}" y2="${f(bottom)}"/>` +
                      (cols.key ? `<line class="colrule" x1="${f(xKey - 10)}" y1="${f(y0 + HEAD_H)}" x2="${f(xKey - 10)}" y2="${f(bottom)}"/>` : '')
                    : '';
                const rows = n.rows.map((r, i) => {
                    const y = rowTop(i) + ROW_H / 2;
                    return `<text class="typ" x="${f(x0 + 10)}" y="${f(y)}">${esc(r.type)}</text><text class="row" x="${f(xName)}" y="${f(y)}">${esc(r.name)}</text>` +
                        (r.tag ? `<text class="tag${r.tag.startsWith('PK') ? ' pk' : ''}" x="${f(xKey)}" y="${f(y)}">${esc(r.tag.replace(',', ', '))}</text>` : '');
                }).join('');
                S.push(`${g}<rect class="box" x="${f(x0)}" y="${f(y0)}" width="${f(n.w)}" height="${f(n.h)}"/><rect class="boxhead" x="${f(x0 + 0.8)}" y="${f(y0 + 0.8)}" width="${f(n.w - 1.6)}" height="${f(HEAD_H - 0.8)}"/>` +
                    `${shade}${rules}<line x1="${f(x0)}" y1="${f(y0 + HEAD_H)}" x2="${f(-x0)}" y2="${f(y0 + HEAD_H)}" stroke="var(--er-ink,#1f2937)" stroke-width="1"/>` +
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

    // The model or its tables as schema lines, one per entity type or table,
    // as the Handbuch writes them: the key underlined, a foreign key with ↑.
    // level 'entities': the entity types and their attributes; 'tables': the
    // tables after the translation.
    function schemaLines(model, result, level) {
        if (level === 'entities') {
            return model.entities.map((e) => ({ name: e.name, cols: e.attrs.map((a) => ({ name: a.name, key: !!a.key, fk: false })) }));
        }
        return result.tables.map((t) => {
            const fk = new Set(t.fks.flatMap((f) => f.cols));
            return { name: t.name, cols: t.columns.map((c) => ({ name: c.name, key: t.pk.includes(c.name), fk: fk.has(c.name) })) };
        });
    }

    // The lines as HTML (<u> for the key) or as text, the key underlined with
    // a combining low line (U+0332), so it survives a paste into a document.
    function schemaText(lines, format) {
        const html = format === 'html';
        return lines.map((l) => {
            const cols = l.cols.map((c) => {
                let s = html ? esc(c.name) : c.name;
                if (c.key) s = html ? `<u>${s}</u>` : [...s].map((ch) => `${ch}\u0332`).join('');
                if (c.fk) s = html ? `<span class="fk">↑${s}</span>` : `↑${s}`;
                return s;
            });
            return html ? `<div><b>${esc(l.name)}</b>(${cols.join(', ')})</div>` : `${l.name}(${cols.join(', ')})`;
        }).join(html ? '' : '\n');
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
        chenGraph, crowfootGraph, layout, renderSVG, diagram, estimateWidth, esc, viewBox, schemaLines, schemaText,
    };
});

// ---------- command line --------------------------------------------------------
if (typeof require !== 'undefined' && typeof module !== 'undefined' && require.main === module) {
    const fs = require('fs');
    const path = require('path');
    const core = module.exports;
    const args = process.argv.slice(2);
    const usage = 'node er-core.js --json|--sql|--svg chen|crowfoot|tables|--schema entities|tables <datei.er>';
    const mode = args[0];
    const withKind = mode === '--svg' || mode === '--schema';
    const kind = withKind ? args[1] : null;
    const file = withKind ? args[2] : args[1];
    const kinds = mode === '--schema' ? ['entities', 'tables'] : ['chen', 'crowfoot', 'tables'];
    if (!['--json', '--sql', '--svg', '--schema'].includes(mode) || !file || (withKind && !kinds.includes(kind))) {
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
    } else if (mode === '--schema') {
        process.stdout.write(`${core.schemaText(core.schemaLines(model, result, kind), 'text')}\n`);
    } else {
        // A white background, like »Als SVG speichern«: the file is also read on dark pages.
        process.stdout.write(core.diagram(text, kind, { background: '#ffffff' }).svg + '\n');
    }
}
