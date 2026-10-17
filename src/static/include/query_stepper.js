// The query stepper: a SELECT taken apart in the order the database works
// through it, FROM, JOIN, WHERE, GROUP BY, HAVING, SELECT, DISTINCT,
// ORDER BY, LIMIT, with the table after each step. ← and → step through it
// (stepper_keys.js). The data are the countries of the world from the SQL
// introduction and the movies of »Filmdatenbank«
// (src/scripts/query-stepper-data.rb).
//
// The engine below understands the SELECT the pages teach and follows MySQL
// where a student would notice a difference: text compares without regard to
// case and accents, as the collation utf8mb4_unicode_ci does; NULL is
// neither true nor false and drops out of WHERE; an alias from SELECT does
// not exist yet in WHERE; with GROUP BY every other column needs an
// aggregate (ONLY_FULL_GROUP_BY). What it does not understand, it says.
//
// <div class='query-stepper' data-stepper data-query='SELECT …'></div>
// on a page, with <script src='/include/query_stepper.js'></script>;
// data-dataset='movies' for the movie tables, data-choose for a box to pick
// the tables (the tool page).

(function (root) {
    'use strict';

    // --- Tokens -------------------------------------------------------------

    const KEYWORDS = new Set(['SELECT', 'DISTINCT', 'FROM', 'AS', 'JOIN', 'INNER', 'LEFT', 'OUTER', 'ON', 'WHERE',
        'GROUP', 'BY', 'HAVING', 'ORDER', 'ASC', 'DESC', 'LIMIT', 'OFFSET', 'AND', 'OR', 'NOT', 'LIKE', 'IN',
        'BETWEEN', 'IS', 'NULL', 'TRUE', 'FALSE']);
    const AGGREGATES = new Set(['COUNT', 'SUM', 'AVG', 'MIN', 'MAX']);
    const MAX_JOINED = 200000;
    const FUNCTIONS = new Set(['ROUND', 'UPPER', 'LOWER', 'LENGTH', 'ABS', 'CONCAT']);

    class SqlError extends Error {}
    const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

    function tokenize(text) {
        const tokens = [];
        let i = 0;
        while (i < text.length) {
            const c = text[i];
            if (/\s/.test(c)) { i++; continue; }
            if (c === '-' && text[i + 1] === '-') { while (i < text.length && text[i] !== '\n') i++; continue; }
            const start = i;
            if (/[0-9]/.test(c) || (c === '.' && /[0-9]/.test(text[i + 1] || ''))) {
                while (i < text.length && /[0-9.]/.test(text[i])) i++;
                tokens.push({ type: 'num', value: Number(text.slice(start, i)), start, end: i });
            } else if (c === "'" || c === '"') {
                let value = '';
                i++;
                for (;;) {
                    if (i >= text.length) throw new SqlError('Ein Text in Anführungszeichen hört nicht auf: Es fehlt das schließende ' + c + '.');
                    if (text[i] === '\\') { value += text[i + 1]; i += 2; continue; }
                    if (text[i] === c && text[i + 1] === c) { value += c; i += 2; continue; }
                    if (text[i] === c) { i++; break; }
                    value += text[i++];
                }
                tokens.push({ type: 'str', value, start, end: i });
            } else if (/[A-Za-z_äöüÄÖÜß]/.test(c)) {
                while (i < text.length && /[A-Za-z0-9_äöüÄÖÜß]/.test(text[i])) i++;
                const word = text.slice(start, i);
                const upper = word.toUpperCase();
                tokens.push(KEYWORDS.has(upper) ? { type: 'kw', value: upper, start, end: i } : { type: 'id', value: word, start, end: i });
            } else if (c === '`') {
                i++;
                while (i < text.length && text[i] !== '`') i++;
                if (i >= text.length) throw new SqlError('Ein Name in `…` hört nicht auf.');
                tokens.push({ type: 'id', value: text.slice(start + 1, i), start, end: ++i });
            } else {
                const two = text.slice(i, i + 2);
                if (['<>', '!=', '<=', '>='].includes(two)) { tokens.push({ type: 'op', value: two, start, end: i + 2 }); i += 2; continue; }
                if ('=<>(),.*+-/;%'.includes(c)) { tokens.push({ type: 'op', value: c, start, end: ++i }); continue; }
                throw new SqlError(`Das Zeichen »${c}« an Stelle ${i + 1} kennt die Zeitlupe nicht.`);
            }
        }
        return tokens;
    }

    // --- Parser -------------------------------------------------------------

    function parse(text) {
        const tokens = tokenize(text);
        let pos = 0;
        const peek = (k = 0) => tokens[pos + k];
        const isKw = (word, k = 0) => peek(k) && peek(k).type === 'kw' && peek(k).value === word;
        const isOp = (op, k = 0) => peek(k) && peek(k).type === 'op' && peek(k).value === op;
        const describe = (t) => t ? `»${text.slice(t.start, t.end)}«` : 'das Ende';
        const expectKw = (word) => {
            if (!isKw(word)) throw new SqlError(`Hier fehlt ${word}, gefunden: ${describe(peek())}.`);
            return tokens[pos++];
        };
        const expectOp = (op) => {
            if (!isOp(op)) throw new SqlError(`Hier fehlt »${op}«, gefunden: ${describe(peek())}.`);
            return tokens[pos++];
        };
        const identifier = (what) => {
            const t = peek();
            if (!t || t.type !== 'id') throw new SqlError(`Hier fehlt ${what}, gefunden: ${describe(t)}.`);
            pos++;
            return t.value;
        };

        function expression() { return or(); }
        function or() {
            let left = and();
            while (isKw('OR')) { pos++; left = { type: 'bin', op: 'OR', left, right: and() }; }
            return left;
        }
        function and() {
            let left = not();
            while (isKw('AND')) { pos++; left = { type: 'bin', op: 'AND', left, right: not() }; }
            return left;
        }
        function not() {
            if (isKw('NOT')) { pos++; return { type: 'not', expr: not() }; }
            return comparison();
        }
        function comparison() {
            const left = additive();
            if (peek() && peek().type === 'op' && ['=', '<>', '!=', '<', '>', '<=', '>='].includes(peek().value)) {
                const op = tokens[pos++].value;
                return { type: 'bin', op: op === '!=' ? '<>' : op, left, right: additive() };
            }
            let negated = false;
            if (isKw('NOT') && (isKw('LIKE', 1) || isKw('IN', 1) || isKw('BETWEEN', 1))) { pos++; negated = true; }
            if (isKw('LIKE')) { pos++; return { type: 'like', expr: left, pattern: additive(), not: negated }; }
            if (isKw('IN')) {
                pos++;
                expectOp('(');
                const list = [expression()];
                while (isOp(',')) { pos++; list.push(expression()); }
                expectOp(')');
                return { type: 'in', expr: left, list, not: negated };
            }
            if (isKw('BETWEEN')) {
                pos++;
                const lo = additive();
                expectKw('AND');
                return { type: 'between', expr: left, lo, hi: additive(), not: negated };
            }
            if (isKw('IS')) {
                pos++;
                const isNot = isKw('NOT') ? (pos++, true) : false;
                expectKw('NULL');
                return { type: 'isnull', expr: left, not: isNot };
            }
            return left;
        }
        function additive() {
            let left = multiplicative();
            while (isOp('+') || isOp('-')) { const op = tokens[pos++].value; left = { type: 'bin', op, left, right: multiplicative() }; }
            return left;
        }
        function multiplicative() {
            let left = unary();
            while (isOp('*') || isOp('/') || isOp('%')) { const op = tokens[pos++].value; left = { type: 'bin', op, left, right: unary() }; }
            return left;
        }
        function unary() {
            if (isOp('-')) { pos++; return { type: 'neg', expr: unary() }; }
            return primary();
        }
        function primary() {
            const t = peek();
            if (!t) throw new SqlError('Der Ausdruck hört zu früh auf.');
            if (t.type === 'num') { pos++; return { type: 'lit', value: t.value }; }
            if (t.type === 'str') { pos++; return { type: 'lit', value: t.value }; }
            if (isKw('NULL')) { pos++; return { type: 'lit', value: null }; }
            if (isKw('TRUE') || isKw('FALSE')) { pos++; return { type: 'lit', value: t.value === 'TRUE' ? 1 : 0 }; }
            if (isOp('(')) {
                pos++;
                if (isKw('SELECT')) throw new SqlError('Unterabfragen kann die Zeitlupe nicht zeigen.');
                const inner = expression();
                expectOp(')');
                return inner;
            }
            if (t.type === 'id') {
                pos++;
                const upper = t.value.toUpperCase();
                if (isOp('(')) {
                    pos++;
                    if (!AGGREGATES.has(upper) && !FUNCTIONS.has(upper)) throw new SqlError(`Die Funktion ${t.value} kennt die Zeitlupe nicht.`);
                    const call = { type: 'call', name: upper, args: [], star: false, distinct: false };
                    if (upper === 'COUNT' && isOp('*')) { pos++; call.star = true; }
                    else if (!isOp(')')) {
                        if (isKw('DISTINCT')) { pos++; call.distinct = true; }
                        call.args.push(expression());
                        while (isOp(',')) { pos++; call.args.push(expression()); }
                    }
                    expectOp(')');
                    return call;
                }
                if (isOp('.')) {
                    pos++;
                    if (isOp('*')) throw new SqlError(`${t.value}.* geht nur direkt nach SELECT.`);
                    return { type: 'col', table: t.value, name: identifier('ein Spaltenname nach dem Punkt') };
                }
                return { type: 'col', table: null, name: t.value };
            }
            throw new SqlError(`Hier fehlt ein Wert oder eine Spalte, gefunden: ${describe(t)}.`);
        }

        const query = { distinct: false, select: [], joins: [], where: null, groupBy: [], having: null, orderBy: [], limit: null };
        // What a clause says, as written, for the explanation of its step
        query.sources = {};
        const clauseStart = () => tokens[pos].start;
        const clauseText = (start) => text.slice(start, tokens[pos - 1].end).replace(/\s+/g, ' ');
        const selectStart = peek() ? peek().start : 0;
        expectKw('SELECT');
        if (isKw('DISTINCT')) { pos++; query.distinct = true; }
        do {
            if (query.select.length) pos++;
            const start = peek() ? peek().start : text.length;
            if (isOp('*')) { pos++; query.select.push({ star: true, table: null }); continue; }
            if (peek() && peek().type === 'id' && isOp('.', 1) && isOp('*', 2)) {
                const table = peek().value;
                pos += 3;
                query.select.push({ star: true, table });
                continue;
            }
            const expr = expression();
            const source = text.slice(start, tokens[pos - 1].end);
            let alias = null;
            if (isKw('AS')) { pos++; alias = identifier('ein Name nach AS'); }
            else if (peek() && peek().type === 'id') alias = tokens[pos++].value;
            query.select.push({ expr, alias, source });
        } while (isOp(','));
        query.sources.select = clauseText(selectStart);

        expectKw('FROM');
        const tableRef = () => {
            const name = identifier('ein Tabellenname');
            let alias = null;
            if (isKw('AS')) { pos++; alias = identifier('ein Name nach AS'); }
            else if (peek() && peek().type === 'id') alias = tokens[pos++].value;
            return { name, alias: alias || name };
        };
        query.from = tableRef();
        for (;;) {
            let type = null;
            const joinStart = peek() ? peek().start : 0;
            if (isKw('JOIN')) { type = 'INNER'; pos++; }
            else if (isKw('INNER') && isKw('JOIN', 1)) { type = 'INNER'; pos += 2; }
            else if (isKw('LEFT')) { type = 'LEFT'; pos++; if (isKw('OUTER')) pos++; expectKw('JOIN'); }
            else if (isOp(',')) throw new SqlError('Mehrere Tabellen mit Komma verbindet die Zeitlupe nicht; schreib JOIN … ON.');
            else break;
            const at = joinStart;
            const ref = tableRef();
            expectKw('ON');
            const on = expression();
            query.joins.push({ type, ...ref, on, source: clauseText(at) });
        }
        if (isKw('WHERE')) { const at = clauseStart(); pos++; query.where = expression(); query.sources.where = clauseText(at); }
        if (isKw('GROUP')) {
            const at = clauseStart();
            pos++;
            expectKw('BY');
            query.groupBy.push(expression());
            while (isOp(',')) { pos++; query.groupBy.push(expression()); }
            query.sources.groupBy = clauseText(at);
        }
        if (isKw('HAVING')) { const at = clauseStart(); pos++; query.having = expression(); query.sources.having = clauseText(at); }
        if (isKw('ORDER')) {
            query.sources.orderStart = clauseStart();
            pos++;
            expectKw('BY');
            do {
                if (query.orderBy.length) pos++;
                const expr = expression();
                let desc = false;
                if (isKw('DESC')) { pos++; desc = true; } else if (isKw('ASC')) pos++;
                query.orderBy.push({ expr, desc });
            } while (isOp(','));
            query.sources.orderBy = clauseText(query.sources.orderStart);
        }
        if (isKw('LIMIT')) {
            pos++;
            const number = () => {
                const t = peek();
                if (!t || t.type !== 'num' || !Number.isInteger(t.value)) throw new SqlError('Nach LIMIT steht eine ganze Zahl.');
                pos++;
                return t.value;
            };
            let count = number();
            let offset = 0;
            if (isOp(',')) { pos++; offset = count; count = number(); }
            else if (isKw('OFFSET')) { pos++; offset = number(); }
            query.limit = { count, offset };
        }
        if (isOp(';')) pos++;
        if (pos < tokens.length) {
            const t = peek();
            const clause = t.type === 'kw' && ['WHERE', 'GROUP', 'HAVING', 'ORDER', 'LIMIT', 'FROM'].includes(t.value);
            throw new SqlError(clause ?
                `${t.value} steht an der falschen Stelle. Die Reihenfolge ist SELECT, FROM, JOIN, WHERE, GROUP BY, HAVING, ORDER BY, LIMIT.` :
                `Unerwartet: ${describe(t)}.`);
        }
        return query;
    }

    // --- Values -------------------------------------------------------------

    const collator = typeof Intl !== 'undefined' ? new Intl.Collator('de', { sensitivity: 'base' }) : null;
    const textCompare = (a, b) => collator ? collator.compare(a, b) : (a.toLowerCase() < b.toLowerCase() ? -1 : a.toLowerCase() > b.toLowerCase() ? 1 : 0);
    const isNumeric = (v) => typeof v === 'number' || (typeof v === 'string' && v.trim() !== '' && !isNaN(Number(v)));

    // -1, 0 or 1; null if one side is NULL
    function compare(a, b) {
        if (a === null || b === null) return null;
        if (typeof a === 'number' || typeof b === 'number') {
            if (isNumeric(a) && isNumeric(b)) return Math.sign(Number(a) - Number(b));
        }
        return Math.sign(textCompare(String(a), String(b)));
    }

    function truth(v) {
        if (v === null) return null;
        if (typeof v === 'number') return v !== 0;
        if (typeof v === 'boolean') return v;
        return Number(v) !== 0 && !isNaN(Number(v));
    }

    const bool = (b) => b === null ? null : (b ? 1 : 0);

    function likeToRegex(pattern) {
        let out = '';
        for (const c of String(pattern)) {
            if (c === '%') out += '.*';
            else if (c === '_') out += '.';
            else out += c.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
        }
        return new RegExp('^' + out + '$', 'is');
    }
    const fold = (s) => String(s).normalize('NFD').replace(/[̀-ͯ]/g, '').replace(/ß/g, 'ss');

    // Values that compare as equal get the same key: for a hash join, for
    // groups and for DISTINCT, instead of comparing every pair.
    function keyOf(v) {
        if (v === null || v === undefined) return '\u0000';
        if (typeof v === 'number') return 'n' + v;
        return 's' + fold(v).toLowerCase();
    }
    const keysOf = (values) => values.map(keyOf).join('\u0001');

    // A table of one step. Its cells are made only for the rows that are
    // shown; a JOIN can have many thousands. rows (for tests and callers)
    // is built on first use.
    function lazyView(columns, highlight, size, cellsAt, marks, groups) {
        let rows = null;
        return {
            columns, highlight, size, cellsAt,
            markAt: (i) => marks ? marks[i] : null,
            groupAt: (i) => groups ? groups[i] : undefined,
            get rows() {
                if (!rows) rows = Array.from({ length: size }, (_, i) => ({ cells: cellsAt(i), mark: marks ? marks[i] : null, group: groups ? groups[i] : undefined }));
                return rows;
            },
        };
    }

    function show(v) {
        if (v === null || v === undefined) return 'NULL';
        if (typeof v === 'number') return Number.isInteger(v) ? String(v) : String(Number(v.toFixed(4)));
        return String(v);
    }

    // --- Running a query ----------------------------------------------------

    function run(text, data) {
        const query = parse(text);
        const tables = data.tables;
        const tableByName = (name) => {
            const key = Object.keys(tables).find((t) => t.toLowerCase() === name.toLowerCase());
            if (!key) throw new SqlError(`Die Tabelle ${name} gibt es nicht. Es gibt: ${Object.keys(tables).join(', ')}.`);
            return key;
        };
        const steps = [];
        const joined = query.joins.length > 0;

        // Columns of the joined rows: { table (alias), name }
        const fromKey = tableByName(query.from.name);
        let columns = tables[fromKey].columns.map((name) => ({ table: query.from.alias, name }));
        let rows = tables[fromKey].rows.map((r) => r.slice());
        const label = (c) => joined ? `${c.table}.${c.name}` : c.name;
        // Before SELECT, only the columns the query uses: after a JOIN the
        // table gets wide, and the columns that matter would be off to the
        // right. With SELECT * every column counts.
        const refs = [];
        (function collect(e) {
            if (!e || typeof e !== 'object') return;
            if (Array.isArray(e)) { e.forEach(collect); return; }
            if (e.type === 'col') refs.push(e);
            Object.keys(e).forEach((k) => { if (!k.startsWith('cached') && k !== 'type') collect(e[k]); });
        })([query.select.map((x) => x.expr), query.where, query.joins.map((j) => j.on), query.groupBy, query.having, query.orderBy.map((o) => o.expr)]);
        const star = query.select.some((x) => x.star);
        const visibleOf = (cols) => {
            if (star) return cols.map((c, i) => i);
            const keep = cols.map((c, i) => i).filter((i) => refs.some((r) => r.name.toLowerCase() === cols[i].name.toLowerCase() &&
                (!r.table || r.table.toLowerCase() === cols[i].table.toLowerCase())));
            return keep.length ? keep : cols.map((c, i) => i);
        };
        const view = (cols, rws, marks, extra = {}) => {
            const shown = visibleOf(cols);
            const highlight = (extra.highlight || []).map((i) => shown.indexOf(i)).filter((i) => i >= 0);
            const v = lazyView(shown.map((i) => label(cols[i])), highlight, rws.length,
                (r) => shown.map((i) => show(rws[r][i])), marks, extra.groups);
            v.hidden = cols.length - shown.length;
            return v;
        };
        // The columns an expression reads, to show them in the table of its step
        const used = (expr) => {
            const out = new Set();
            (function walk(e) {
                if (!e || typeof e !== 'object') return;
                if (e.type === 'col') {
                    columns.forEach((c, i) => {
                        if (c.name.toLowerCase() === e.name.toLowerCase() && (!e.table || c.table.toLowerCase() === e.table.toLowerCase())) out.add(i);
                    });
                }
                ['left', 'right', 'expr', 'pattern', 'lo', 'hi', 'on'].forEach((k) => walk(e[k]));
                (e.list || []).forEach(walk);
                (e.args || []).forEach(walk);
            })(expr);
            return [...out];
        };
        const code = (source) => `<code>${esc(source)}</code>`;
        const note = tables[fromKey].note ? ` (${tables[fromKey].note})` : '';
        steps.push({ clause: 'FROM', text: `<code>FROM ${query.from.name}</code>: alle ${rows.length} Zeilen der Tabelle ${query.from.name}${note}.`, table: view(columns, rows) });

        // Finds a column in the current rows; where says in which clause, for the message.
        function resolve(cols, ref, where, aliases) {
            const matches = [];
            cols.forEach((c, i) => {
                if (c.name.toLowerCase() !== ref.name.toLowerCase()) return;
                if (ref.table && c.table.toLowerCase() !== ref.table.toLowerCase()) return;
                matches.push(i);
            });
            if (matches.length === 1) return matches[0];
            if (matches.length > 1) {
                const names = matches.map((i) => `${cols[i].table}.${cols[i].name}`).join(' oder ');
                throw new SqlError(`${ref.name} ist mehrdeutig: Es gibt ${names}. Schreib die Tabelle davor.`);
            }
            if (aliases && aliases.has(ref.name.toLowerCase())) return -1;
            const selectAlias = query.select.find((s) => s.alias && s.alias.toLowerCase() === ref.name.toLowerCase());
            if (selectAlias && where === 'WHERE') {
                throw new SqlError(`${ref.name} gibt es bei WHERE noch nicht: Den Namen bekommt die Spalte erst bei SELECT, und SELECT kommt nach WHERE. Schreib den Ausdruck selbst in WHERE.`);
            }
            throw new SqlError(`Die Spalte ${ref.table ? ref.table + '.' : ''}${ref.name} gibt es ${where ? 'bei ' + where + ' ' : ''}nicht.`);
        }

        // Evaluates expr for one row (cols, row), or for a group (rows) when
        // aggregates are allowed; aliases: values already computed by SELECT.
        function evaluate(expr, ctx) {
            switch (expr.type) {
                case 'lit': return expr.value;
                case 'col': {
                    // Where the column is, once per table layout, not once per row
                    let i;
                    const withAliases = !!ctx.aliases;
                    if (expr.cachedCols === ctx.cols && expr.cachedWithAliases === withAliases) i = expr.cachedIndex;
                    else {
                        i = resolve(ctx.cols, expr, ctx.where, ctx.aliases);
                        expr.cachedCols = ctx.cols;
                        expr.cachedWithAliases = withAliases;
                        expr.cachedIndex = i;
                    }
                    if (i === -1) return ctx.aliases.get(expr.name.toLowerCase());
                    const row = ctx.row || (ctx.group && ctx.group[0]);
                    return row ? row[i] : null;
                }
                case 'neg': { const v = evaluate(expr.expr, ctx); return v === null ? null : -Number(v); }
                case 'not': { const v = truth(evaluate(expr.expr, ctx)); return v === null ? null : bool(!v); }
                case 'bin': {
                    if (expr.op === 'AND' || expr.op === 'OR') {
                        const a = truth(evaluate(expr.left, ctx));
                        const b = truth(evaluate(expr.right, ctx));
                        if (expr.op === 'AND') return (a === false || b === false) ? 0 : (a === null || b === null) ? null : 1;
                        return (a === true || b === true) ? 1 : (a === null || b === null) ? null : 0;
                    }
                    const a = evaluate(expr.left, ctx);
                    const b = evaluate(expr.right, ctx);
                    if (['+', '-', '*', '/', '%'].includes(expr.op)) {
                        if (a === null || b === null) return null;
                        const x = Number(a), y = Number(b);
                        if (expr.op === '+') return x + y;
                        if (expr.op === '-') return x - y;
                        if (expr.op === '*') return x * y;
                        if (y === 0) return null;
                        return expr.op === '/' ? x / y : x % y;
                    }
                    const c = compare(a, b);
                    if (c === null) return null;
                    return bool({ '=': c === 0, '<>': c !== 0, '<': c < 0, '>': c > 0, '<=': c <= 0, '>=': c >= 0 }[expr.op]);
                }
                case 'like': {
                    const v = evaluate(expr.expr, ctx), p = evaluate(expr.pattern, ctx);
                    if (v === null || p === null) return null;
                    const hit = likeToRegex(fold(p)).test(fold(v));
                    return bool(expr.not ? !hit : hit);
                }
                case 'in': {
                    const v = evaluate(expr.expr, ctx);
                    if (v === null) return null;
                    let unknown = false;
                    for (const item of expr.list) {
                        const c = compare(v, evaluate(item, ctx));
                        if (c === 0) return bool(!expr.not);
                        if (c === null) unknown = true;
                    }
                    return unknown ? null : bool(expr.not);
                }
                case 'between': {
                    const v = evaluate(expr.expr, ctx);
                    const lo = compare(v, evaluate(expr.lo, ctx)), hi = compare(v, evaluate(expr.hi, ctx));
                    if (lo === null || hi === null) return null;
                    const inside = lo >= 0 && hi <= 0;
                    return bool(expr.not ? !inside : inside);
                }
                case 'isnull': { const v = evaluate(expr.expr, ctx); return bool(expr.not ? v !== null : v === null); }
                case 'call': {
                    if (AGGREGATES.has(expr.name)) {
                        if (!ctx.group) throw new SqlError(`${expr.name}(…) fasst mehrere Zeilen zusammen und geht darum nicht ${ctx.where === 'WHERE' ? 'bei WHERE; dafür gibt es HAVING' : 'hier'}.`);
                        if (expr.star) return ctx.group.length;
                        let values = ctx.group.map((row) => evaluate(expr.args[0], { ...ctx, row, group: null, inAggregate: true })).filter((v) => v !== null);
                        if (expr.distinct) { const seen = new Set(); values = values.filter((v) => !seen.has(keyOf(v)) && seen.add(keyOf(v))); }
                        if (expr.name === 'COUNT') return values.length;
                        if (!values.length) return null;
                        if (expr.name === 'SUM') return values.reduce((s, v) => s + Number(v), 0);
                        if (expr.name === 'AVG') return values.reduce((s, v) => s + Number(v), 0) / values.length;
                        const pick = expr.name === 'MIN' ? -1 : 1;
                        return values.reduce((best, v) => (compare(v, best) === pick ? v : best));
                    }
                    const args = expr.args.map((a) => evaluate(a, ctx));
                    if (expr.name !== 'CONCAT' && args[0] === null) return null;
                    switch (expr.name) {
                        case 'ROUND': { const d = args[1] === undefined ? 0 : Number(args[1]); const f = 10 ** d; return Math.round(Number(args[0]) * f) / f; }
                        case 'UPPER': return String(args[0]).toUpperCase();
                        case 'LOWER': return String(args[0]).toLowerCase();
                        case 'LENGTH': return new TextEncoder().encode(String(args[0])).length;
                        case 'ABS': return Math.abs(Number(args[0]));
                        case 'CONCAT': return args.some((a) => a === null) ? null : args.map(show).join('');
                    }
                    return null;
                }
            }
            throw new SqlError('Diesen Ausdruck kann die Zeitlupe nicht ausrechnen.');
        }

        const hasAggregate = (expr) => !!expr && (expr.type === 'call' && AGGREGATES.has(expr.name) ||
            ['left', 'right', 'expr', 'pattern', 'lo', 'hi'].some((k) => hasAggregate(expr[k])) ||
            (expr.list || []).some(hasAggregate) || (expr.type === 'call' && !AGGREGATES.has(expr.name) && expr.args.some(hasAggregate)));

        // JOIN
        for (const join of query.joins) {
            const key = tableByName(join.name);
            const right = tables[key].columns.map((name) => ({ table: join.alias, name }));
            const both = columns.concat(right);
            const out = [];
            let unmatched = 0;
            // ON a = b with one column on each side: look the partners up by
            // key (a hash join) instead of trying every pair.
            const side = (ref) => {
                const inLeft = columns.some((c) => c.name.toLowerCase() === ref.name.toLowerCase() && (!ref.table || c.table.toLowerCase() === ref.table.toLowerCase()));
                const inRight = right.some((c) => c.name.toLowerCase() === ref.name.toLowerCase() && (!ref.table || c.table.toLowerCase() === ref.table.toLowerCase()));
                return inLeft && !inRight ? 'left' : inRight && !inLeft ? 'right' : null;
            };
            const on = join.on;
            const equi = on.type === 'bin' && on.op === '=' && on.left.type === 'col' && on.right.type === 'col' &&
                side(on.left) && side(on.right) && side(on.left) !== side(on.right);
            let partners;
            if (equi) {
                const leftRef = side(on.left) === 'left' ? on.left : on.right;
                const rightRef = leftRef === on.left ? on.right : on.left;
                const li = resolve(columns, leftRef, 'ON');
                const ri = resolve(right, rightRef, 'ON');
                const buckets = new Map();
                for (const r of tables[key].rows) {
                    if (r[ri] === null) continue;
                    const k = keyOf(r[ri]);
                    if (!buckets.has(k)) buckets.set(k, []);
                    buckets.get(k).push(r);
                }
                partners = (left) => left[li] === null ? [] : (buckets.get(keyOf(left[li])) || []);
            } else {
                partners = (left) => tables[key].rows.filter((r) => truth(evaluate(on, { cols: both, row: left.concat(r), where: 'ON' })) === true);
            }
            for (const left of rows) {
                const found = partners(left);
                for (const r of found) out.push(left.concat(r));
                if (!found.length && join.type === 'LEFT') { out.push(left.concat(right.map(() => null))); unmatched++; }
                if (!found.length && join.type === 'INNER') unmatched++;
                if (out.length > MAX_JOINED) throw new SqlError(`Der JOIN ergibt mehr als ${MAX_JOINED.toLocaleString('de')} Zeilen; so viele kann die Zeitlupe nicht zeigen. Grenz die Abfrage ein, etwa mit einer genaueren Bedingung bei ON.`);
            }
            columns = both;
            rows = out;
            const extra = tables[key].note ? ` (${tables[key].note})` : '';
            const what = join.type === 'LEFT' ?
                `Jede Zeile bleibt; wo ${join.name}${extra} nichts Passendes hat, stehen dort NULL-Werte (${unmatched} Zeilen).` :
                `Nur Zeilen, zu denen es in ${join.name}${extra} etwas Passendes gibt${unmatched ? `; ${unmatched} Zeilen ohne Partner fallen weg` : ''}.`;
            steps.push({ clause: join.type === 'LEFT' ? 'LEFT JOIN' : 'JOIN', text: `${code(join.source)}: Jede Zeile wird mit jeder Zeile aus ${join.name} verbunden, für die die Bedingung gilt. ${what} Jetzt sind es ${rows.length} Zeilen.`, table: view(columns, rows, null, { highlight: used(join.on) }) });
        }

        // WHERE
        if (query.where) {
            if (hasAggregate(query.where)) {
                const call = (function find(e) { return e && (e.type === 'call' && AGGREGATES.has(e.name) ? e : ['left', 'right', 'expr'].map((k) => find(e[k])).find(Boolean)); })(query.where);
                throw new SqlError(`${call.name}(…) geht nicht bei WHERE: WHERE prüft jede Zeile für sich, noch bevor Gruppen entstehen. Eine Bedingung über eine Gruppe gehört zu HAVING.`);
            }
            const marks = rows.map((row) => {
                const v = truth(evaluate(query.where, { cols: columns, row, where: 'WHERE' }));
                return v === true ? 'keep' : (v === null ? 'null' : 'drop');
            });
            const kept = rows.filter((_, i) => marks[i] === 'keep');
            const nulls = marks.filter((m) => m === 'null').length;
            steps.push({ clause: 'WHERE', text: `${code(query.sources.where)}: Die Bedingung wird für jede Zeile geprüft. ${kept.length} von ${rows.length} Zeilen bleiben.${nulls ? ` Bei ${nulls} ist das Ergebnis NULL, weil ein Wert fehlt; auch sie fallen weg.` : ''}`, table: view(columns, rows, marks, { highlight: used(query.where) }) });
            rows = kept;
        }

        // GROUP BY (or one group for an aggregate without GROUP BY)
        const grouping = query.groupBy.length > 0 || query.select.some((s) => s.expr && hasAggregate(s.expr)) || hasAggregate(query.having);
        let groups = null;
        if (grouping) {
            const groupCols = query.groupBy.map((e) => e.type === 'col' ? resolve(columns, e, 'GROUP BY') : null);
            if (query.groupBy.length) {
                const groupKey = (row) => query.groupBy.map((e) => evaluate(e, { cols: columns, row, where: 'GROUP BY' }));
                groups = [];
                const byKey = new Map();
                for (const row of rows) {
                    const key = groupKey(row);
                    const k = keysOf(key);
                    let g = byKey.get(k);
                    if (!g) { g = { key, rows: [] }; groups.push(g); byKey.set(k, g); }
                    g.rows.push(row);
                }
                const flat = [], marks = [], ids = [];
                groups.forEach((g, gi) => g.rows.forEach((r) => { flat.push(r); marks.push('group' + (gi % 2)); ids.push(gi); }));
                steps.push({ clause: 'GROUP BY', text: `${code(query.sources.groupBy)}: Zeilen mit demselben Wert kommen in eine Gruppe. Aus ${rows.length} Zeilen werden ${groups.length} Gruppen; jede wird am Ende zu einer einzigen Zeile.`, table: view(columns, flat, marks, { groups: ids, highlight: query.groupBy.flatMap(used) }) });
            } else {
                groups = [{ key: [], rows }];
            }

            // ONLY_FULL_GROUP_BY: a column outside an aggregate must be grouped by
            const check = (expr, where) => {
                if (!expr) return;
                if (expr.type === 'call' && AGGREGATES.has(expr.name)) return;
                if (expr.type === 'col') {
                    const aliasHit = where !== 'SELECT' && query.select.some((s) => s.alias && s.alias.toLowerCase() === expr.name.toLowerCase());
                    if (aliasHit) return;
                    const i = resolve(columns, expr, where);
                    if (!groupCols.includes(i)) {
                        throw new SqlError(`${expr.name} steht bei ${where} weder in GROUP BY noch in einer Funktion wie COUNT oder MAX. Eine Gruppe hat viele Zeilen und damit viele Werte für ${expr.name}: Welcher soll es sein?`);
                    }
                    return;
                }
                ['left', 'right', 'expr', 'pattern', 'lo', 'hi'].forEach((k) => check(expr[k], where));
                (expr.list || []).forEach((e) => check(e, where));
                if (expr.type === 'call') expr.args.forEach((e) => check(e, where));
            };
            query.select.forEach((s) => {
                if (s.star) throw new SqlError('SELECT * geht nicht mit Gruppen: Eine Gruppe hat viele Zeilen. Nenn die Spalten aus GROUP BY und Funktionen wie COUNT(*).');
                check(s.expr, 'SELECT');
            });
            check(query.having, 'HAVING');
            query.orderBy.forEach((o) => check(o.expr, 'ORDER BY'));
        }

        // SELECT values (for a group or a row), with the aliases for HAVING and ORDER BY
        const outColumns = [];
        query.select.forEach((s) => {
            if (s.star) {
                columns.forEach((c) => { if (!s.table || c.table.toLowerCase() === s.table.toLowerCase()) outColumns.push({ label: label(c), col: c }); });
                if (s.table && !columns.some((c) => c.table.toLowerCase() === s.table.toLowerCase())) throw new SqlError(`Eine Tabelle ${s.table} gibt es in dieser Abfrage nicht.`);
            } else {
                outColumns.push({ label: s.alias || s.source, expr: s.expr, alias: s.alias });
            }
        });
        outColumns.forEach((o) => { if (o.col) o.index = columns.indexOf(o.col); });
        const selectValues = (ctx) => {
            const was = ctx.where;
            ctx.where = 'SELECT';
            const values = outColumns.map((o) => o.col ? ctx.row[o.index] : evaluate(o.expr, ctx));
            ctx.where = was;
            return values;
        };
        const aliasMap = (values) => new Map(outColumns.map((o, i) => [o.alias && o.alias.toLowerCase(), values[i]]).filter(([k]) => k));

        let results;   // [{ values, ctx }]
        if (groups) {
            // HAVING
            if (query.having) {
                const kept = [], marks = [], flat = [], ids = [];
                groups.forEach((g, gi) => {
                    const ctx = { cols: columns, group: g.rows, where: 'HAVING' };
                    ctx.aliases = aliasMap(selectValues(ctx));
                    const ok = truth(evaluate(query.having, ctx)) === true;
                    if (ok) kept.push(g);
                    g.rows.forEach((r) => { flat.push(r); marks.push(ok ? 'group' + (gi % 2) : 'drop'); ids.push(gi); });
                });
                steps.push({ clause: 'HAVING', text: `${code(query.sources.having)}: Die Bedingung wird für jede Gruppe geprüft, nicht für jede Zeile. ${kept.length} von ${groups.length} Gruppen bleiben.`, table: view(columns, flat, marks, { groups: ids, highlight: query.groupBy.flatMap(used) }) });
                groups = kept;
            }
            results = groups.map((g) => {
                const ctx = { cols: columns, group: g.rows };
                return { values: selectValues(ctx), ctx };
            });
        } else {
            results = rows.map((row) => {
                const ctx = { cols: columns, row };
                return { values: selectValues(ctx), ctx };
            });
        }
        const resultView = (list, marks) => lazyView(outColumns.map((o) => o.label), [], list.length, (i) => list[i].values.map(show), marks, null);
        steps.push({ clause: 'SELECT', text: groups ?
            `${code(query.sources.select)}: Aus jeder Gruppe wird eine Zeile, mit den Spalten aus SELECT. ${results.length} ${results.length === 1 ? 'Zeile' : 'Zeilen'}.` :
            `${code(query.sources.select)}: Erst jetzt werden die Spalten ausgewählt und ausgerechnet${outColumns.some((o) => o.alias) ? ', und sie bekommen ihre Namen aus AS' : ''}. ${results.length} Zeilen, ${outColumns.length} ${outColumns.length === 1 ? 'Spalte' : 'Spalten'}.`,
            table: resultView(results) });

        // DISTINCT
        if (query.distinct) {
            const seen = new Set();
            const marks = results.map((r) => { const k = keysOf(r.values); if (seen.has(k)) return 'drop'; seen.add(k); return 'keep'; });
            const kept = results.filter((_, i) => marks[i] === 'keep');
            steps.push({ clause: 'DISTINCT', text: `<code>DISTINCT</code>: Gleiche Zeilen bleiben nur einmal. ${kept.length} von ${results.length} Zeilen bleiben.`, table: resultView(results, marks) });
            results = kept;
        }

        // ORDER BY
        if (query.orderBy.length) {
            const keys = results.map((r) => query.orderBy.map((o) => {
                if (o.expr.type === 'lit' && Number.isInteger(o.expr.value)) {
                    if (o.expr.value < 1 || o.expr.value > outColumns.length) throw new SqlError(`ORDER BY ${o.expr.value}: So viele Spalten hat SELECT nicht.`);
                    return r.values[o.expr.value - 1];
                }
                if (!r.ctx.aliases) r.ctx.aliases = aliasMap(r.values);
                r.ctx.where = 'ORDER BY';
                return evaluate(o.expr, r.ctx);
            }));
            const order = results.map((_, i) => i).sort((a, b) => {
                for (let k = 0; k < query.orderBy.length; k++) {
                    const x = keys[a][k], y = keys[b][k];
                    let c = x === null && y === null ? 0 : x === null ? -1 : y === null ? 1 : compare(x, y);
                    if (query.orderBy[k].desc) c = -c;
                    if (c) return c;
                }
                return a - b;
            });
            results = order.map((i) => results[i]);
            const how = query.orderBy.map((o) => o.desc ? 'absteigend' : 'aufsteigend');
            steps.push({ clause: 'ORDER BY', text: `${code(query.sources.orderBy)}: Die Zeilen werden sortiert, ${how.length === 1 ? how[0] : how.join(', dann ')}. Weil SELECT schon war, gehen hier auch die Namen aus AS.`, table: resultView(results) });
        }

        // LIMIT
        if (query.limit) {
            const { count, offset } = query.limit;
            const marks = results.map((_, i) => (i >= offset && i < offset + count) ? 'keep' : 'drop');
            const kept = results.filter((_, i) => marks[i] === 'keep');
            const what = offset ? `Die ersten ${offset} Zeilen werden übersprungen, dann bleiben die nächsten ${count}.` : `Nur die ersten ${count} Zeilen bleiben.`;
            const fewer = kept.length < count ? ` Es gibt nur ${kept.length}.` : '';
            steps.push({ clause: 'LIMIT', text: `<code>LIMIT ${offset ? offset + ', ' : ''}${count}</code>: ${what}${fewer}`, table: resultView(results, marks) });
            results = kept;
        }

        return { steps, result: resultView(results) };
    }

    // --- The widget ---------------------------------------------------------

    const ORDER = ['FROM', 'JOIN', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'DISTINCT', 'ORDER BY', 'LIMIT'];
    const MAX_ROWS = 12;
    // The tables to work on: countries (»Einführung in SQL«) or movies (»Filmdatenbank«)
    const DATASETS = { countries: 'Länder der Welt', movies: 'Filme (Filmdatenbank)' };
    const loaded = new Map();
    const loadData = (name) => {
        if (!loaded.has(name)) loaded.set(name, fetch(`/query-stepper/${name}.json`).then((r) => r.json()));
        return loaded.get(name);
    };

    // What to show of a long table: the first rows; if some fall out, enough
    // of both kinds to see the difference; with groups, a few rows of each
    // group. Items are { index } or { more, mark } for a »… und N weitere«.
    function shownRows(table) {
        const n = table.size;
        const all = Array.from({ length: n }, (_, i) => i);
        if (n <= MAX_ROWS) return all.map((index) => ({ index }));
        const more = (count, mark, what = 'Zeilen') => ({ more: `… und ${count.toLocaleString('de')} weitere ${count === 1 ? what.replace(/en$/, 'e').replace(/n$/, '') : what}`, mark });
        if (table.groupAt(0) !== undefined) {
            const groups = [];
            for (let i = 0; i < n; i++) { const g = table.groupAt(i); (groups[g] = groups[g] || []).push(i); }
            const shownGroups = groups.slice(0, 8);
            const per = Math.max(2, Math.floor(MAX_ROWS / shownGroups.length));
            const items = [];
            shownGroups.forEach((g) => {
                g.slice(0, per).forEach((index) => items.push({ index }));
                if (g.length > per) items.push(more(g.length - per, table.markAt(g[0])));
            });
            if (groups.length > shownGroups.length) items.push(more(groups.length - shownGroups.length, null, 'Gruppen'));
            return items;
        }
        const out = (i) => table.markAt(i) === 'drop' || table.markAt(i) === 'null';
        let picked = all.slice(0, MAX_ROWS);
        if (all.some(out)) {
            const keep = [], drop = [];
            for (const i of all) {
                (out(i) ? drop : keep).push(i);
                if (keep.length >= MAX_ROWS && drop.length >= MAX_ROWS) break;
            }
            const half = Math.min(drop.length, Math.max(MAX_ROWS - keep.length, MAX_ROWS / 2));
            picked = keep.slice(0, MAX_ROWS - half).concat(drop.slice(0, half)).sort((x, y) => x - y);
        }
        return picked.map((index) => ({ index })).concat([more(n - picked.length, null)]);
    }

    function tableHtml(table) {
        const hi = new Set(table.highlight || []);
        const cell = (k, value) => `<td class='${[hi.has(k) ? 'qs-col' : '', value === 'NULL' ? 'qs-null-value' : ''].join(' ').trim()}'>${esc(value)}</td>`;
        const head = table.columns.map((c, k) => `<th${hi.has(k) ? " class='qs-col'" : ''}>${esc(c)}</th>`).join('');
        const body = shownRows(table).map((item) => {
            if (item.more) return `<tr class='qs-more${item.mark ? ' qs-' + item.mark : ''}'><td colspan='${table.columns.length}'>${esc(item.more)}</td></tr>`;
            const mark = table.markAt(item.index);
            return `<tr${mark ? ` class='qs-${mark}'` : ''}>${table.cellsAt(item.index).map((c, k) => cell(k, c)).join('')}</tr>`;
        }).join('');
        const empty = table.size ? '' : `<tr class='qs-more'><td colspan='${table.columns.length}'>keine Zeilen</td></tr>`;
        const hidden = table.hidden ? `<div class='qs-hidden'>${table.hidden} ${table.hidden === 1 ? 'Spalte, die' : 'Spalten, die'} die Abfrage nicht benutzt, ${table.hidden === 1 ? 'ist' : 'sind'} ausgeblendet.</div>` : '';
        return `<div class='qs-table'><table class='table table-sm'><thead><tr>${head}</tr></thead><tbody>${body}${empty}</tbody></table></div>${hidden}`;
    }

    // The tables as schema lines, one per table: the primary key underlined, a
    // foreign key with ↑ (as _schema(name) on a page, src/ruby/include/schemas.rb).
    function schemaHtml(tables) {
        return Object.entries(tables).map(([name, table]) => {
            const key = new Set(table.key || []);
            const references = table.references || {};
            const columns = table.columns.map((column) => {
                let text = esc(column);
                if (references[column]) text = `<span class='schema-fk' title='verweist auf ${esc(references[column])}'><span class='schema-arrow'>↑</span>${text}</span>`;
                return key.has(column) ? `<u>${text}</u>` : text;
            });
            return `<div><span class='schema-table'>${esc(name)}</span>(${columns.join(', ')})</div>`;
        }).join('');
    }

    function mount(rootElement) {
        let dataset = DATASETS[rootElement.dataset.dataset] ? rootElement.dataset.dataset : 'countries';
        const start = rootElement.dataset.query || 'SELECT * FROM country;';
        const choose = 'choose' in rootElement.dataset;
        const options = Object.entries(DATASETS).map(([k, v]) => `<option value='${k}'>${esc(v)}</option>`).join('');
        rootElement.innerHTML = `
            ${choose ? `<label class='qs-dataset'>Tabellen: <select class='form-select form-select-sm' aria-label='Tabellen'>${options}</select></label>
            <div class='qs-schema schema-lines' aria-label='Die Tabellen mit ihren Spalten'></div>` : ''}
            <textarea class='form-control qs-input' rows='3' spellcheck='false' aria-label='SQL-Abfrage'></textarea>
            <div class='qs-order' aria-hidden='true'></div>
            <div class='qs-controls'>
                <button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
                <span class='qs-count'></span>
            </div>
            <div class='qs-say' aria-live='polite'></div>
            <div class='qs-view'></div>`;
        const input = rootElement.querySelector('.qs-input');
        const orderBox = rootElement.querySelector('.qs-order');
        const say = rootElement.querySelector('.qs-say');
        const viewBox = rootElement.querySelector('.qs-view');
        const count = rootElement.querySelector('.qs-count');
        const buttons = (name) => rootElement.querySelector(`[data-go='${name}']`);
        input.value = start;
        const fit = () => { input.rows = Math.max(2, Math.min(12, input.value.split('\n').length)); };
        fit();
        let steps = [];
        let current = 0;
        let data = null;
        const select = rootElement.querySelector('.qs-dataset select');
        if (select) select.value = dataset;

        function draw() {
            const step = steps[current];
            const present = steps.map((s) => s.clause.replace('LEFT JOIN', 'JOIN'));
            orderBox.innerHTML = ORDER.filter((c) => present.includes(c)).map((c) =>
                `<span class='qs-clause${c === step.clause.replace('LEFT JOIN', 'JOIN') ? ' qs-current' : ''}'>${c}</span>`).join("<i class='bi bi-arrow-right'></i>");
            say.innerHTML = step.text;
            viewBox.innerHTML = tableHtml(step.table);
            count.textContent = `Schritt ${current + 1} von ${steps.length}`;
            buttons('first').disabled = buttons('prev').disabled = current <= 0;
            buttons('next').disabled = buttons('last').disabled = current >= steps.length - 1;
        }

        function refresh() {
            try {
                steps = run(input.value, data).steps;
            } catch (error) {
                if (!(error instanceof SqlError)) throw error;
                steps = [];
                orderBox.innerHTML = '';
                say.innerHTML = `<span class='qs-error'>${esc(error.message)}</span>`;
                viewBox.innerHTML = '';
                count.textContent = '';
                rootElement.querySelectorAll('[data-go]').forEach((b) => { b.disabled = true; });
                return;
            }
            current = Math.min(current, steps.length - 1);
            draw();
        }

        const go = (i) => { current = Math.max(0, Math.min(steps.length - 1, i)); draw(); };
        buttons('first').addEventListener('click', () => go(0));
        buttons('prev').addEventListener('click', () => go(current - 1));
        buttons('next').addEventListener('click', () => go(current + 1));
        buttons('last').addEventListener('click', () => go(steps.length - 1));
        let timer = null;
        input.addEventListener('input', () => { fit(); clearTimeout(timer); timer = setTimeout(() => { current = 0; refresh(); }, 250); });
        const use = (name) => {
            dataset = name;
            if (select) select.value = name;
            data = null;
            say.textContent = 'Die Tabellen werden geladen …';
            return loadData(name).then((d) => {
                if (dataset !== name) return;
                data = d;
                const schema = rootElement.querySelector('.qs-schema');
                if (schema) schema.innerHTML = schemaHtml(d.tables);
                refresh();
            });
        };
        if (select) select.addEventListener('change', () => { current = 0; use(select.value); });
        use(dataset);

        return {
            setQuery(q, name) {
                input.value = q;
                fit();
                current = 0;
                if (name && name !== dataset) use(name);
                else if (data) refresh();
            },
        };
    }

    const api = { tokenize, parse, run, show, schemaHtml, SqlError, mount };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.QueryStepper = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.query-stepper').forEach((el) => {
            const widget = mount(el);
            // Buttons with data-query right after the widget load an example
            const examples = el.nextElementSibling;
            if (examples && examples.classList.contains('query-stepper-examples')) {
                examples.querySelectorAll('[data-query]').forEach((b) => b.addEventListener('click', () => widget.setQuery(b.dataset.query, b.dataset.dataset)));
            }
        });
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
