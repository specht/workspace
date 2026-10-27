// Syntax diagrams (railroad diagrams) and syntax trees from EBNF, in the
// notation of »Grammatiken« and »Ein Rechner für Ausdrücke« (Wirth's EBNF):
//
//   expression = term { "+" term }
//   term       = factor { "*" factor }
//   factor     = number | "(" expression ")"
//
// A name is a nonterminal (a rectangle, its name in italics), "…" or '…' a
// terminal (a rounded box), | or, ( ) a group, [ ] maybe, { } any number of
// times, also none; … between alternatives stands for the ones left out
// ("0" | … | "9"). A rule may go on over several lines; a line that starts
// with a name and = starts the next one. One module for the ```syntaxdiagramm
// and ```syntaxbaum blocks of a page, the pictures beside a page
// (src/content/grammatiken/bilder/) and the tests
// (test/static/syntax_diagram.test.cjs). No DOM in parse, svg, parseTree and
// treeSvg.
//
// A diagram is read from the left along the lines: a terminal is written as
// it is, a nonterminal stands for its own diagram. `a { s a }` is drawn the
// way Wirth drew it for Pascal: a once, then back below through s as often
// as one likes.
//
// A tree is written as name(child child …) with terminals in quotes:
//
//   sentence(subject("Pip") verb("findet") object("die Lampe"))
//
// and drawn with the same boxes; the terminals, read from left to right,
// give the word.
//
// In a page: a ```syntaxdiagramm or ```syntaxbaum block stays highlighted text
// on the server (code_highlight.rb) and is replaced by its picture here;
// without JavaScript the text stays.
(function (root, factory) {
    if (typeof module === 'object' && module.exports) module.exports = factory();
    else root.SyntaxDiagram = factory();
})(typeof self !== 'undefined' ? self : this, function () {
    'use strict';

    const FONT_SIZE = 14;
    const MONO = "'0xProto Nerd Font Mono', ui-monospace, monospace";
    const BOX = 28;          // height of a box
    const PAD = 10;          // text to the side of its box
    const GAP = 16;          // line between two boxes in a row
    const R = 10;            // radius of a curve where a line branches
    const VGAP = 10;         // between the rows of a choice
    const ROW = 62;          // between the rows of a tree
    const BASELINE = FONT_SIZE * 0.35;

    // The colours of a picture as a file (render-svg.rb knows them); on a page
    // the variables (default.css, dark.css) set them.
    const COLOURS = { ink: '#1f2937', line: '#6b7280', paper: '#ffffff', accent: '#7c3aed' };

    function estimateWidth(text, size) {
        return Array.from(String(text)).length * (size || FONT_SIZE) * 0.6;
    }

    function esc(text) {
        return String(text).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    const f = (n) => Math.round(n * 10) / 10;

    // ---------- parsing EBNF -------------------------------------------------------

    // Tokens with their line: names, terminals, the symbols = | ( ) [ ] { } .
    // and the ellipsis
    function tokenize(text, errors) {
        const tokens = [];
        String(text).replace(/\r\n?/g, '\n').split('\n').forEach((line, i) => {
            const re = /\s*(?:([\p{L}_][\p{L}\p{N}_-]*)|"([^"]*)"|'([^']*)'|(…|\.\.\.)|([=|()[\]{}.])|(\S))/uy;
            let m;
            while (re.lastIndex < line.length && (m = re.exec(line))) {
                if (m[0].trim() === '') break;
                const at = i + 1;
                if (m[1] !== undefined) tokens.push({ kind: 'name', value: m[1], line: at });
                else if (m[2] !== undefined || m[3] !== undefined) {
                    const value = m[2] !== undefined ? m[2] : m[3];
                    if (value === '') errors.push(`Zeile ${at}: Zwischen den Anführungszeichen steht nichts.`);
                    else tokens.push({ kind: 'terminal', value, line: at });
                } else if (m[4] !== undefined) tokens.push({ kind: 'gap', value: '…', line: at });
                else if (m[5] !== undefined) tokens.push({ kind: m[5], value: m[5], line: at });
                else errors.push(`Zeile ${at}: Das Zeichen ${m[6]} gehört nicht zu EBNF. Ein Terminal steht in Anführungszeichen.`);
            }
        });
        return tokens;
    }

    // rules = { name "=" alternatives [ "." ] }, by recursive descent:
    // one function per rule of this grammar
    function parse(text) {
        const errors = [];
        const tokens = tokenize(text, errors);
        let i = 0;
        const peek = (k = 0) => tokens[i + k];
        const at = () => (peek() ? `Zeile ${peek().line}` : 'Am Ende');
        const startsRule = () => peek() && peek().kind === 'name' && peek(1) && peek(1).kind === '=';

        function alternatives(close) {
            const items = [sequence(close)];
            while (peek() && peek().kind === '|') {
                i++;
                items.push(sequence(close));
            }
            return items.length === 1 ? items[0] : { type: 'alt', items };
        }

        function sequence(close) {
            const items = [];
            while (peek() && !['|', ')', ']', '}', '.'].includes(peek().kind) && !startsRule()) {
                const item = factor();
                if (!item) break;
                items.push(item);
            }
            if (items.length === 0 && !close) errors.push(`${at()}: Hier fehlt etwas, ein Name oder ein Terminal.`);
            return wirthLoops(items.length === 1 ? items[0] : { type: 'seq', items });
        }

        function factor() {
            const t = peek();
            if (t.kind === 'name') { i++; return { type: 'n', name: t.value }; }
            if (t.kind === 'terminal') { i++; return { type: 't', text: t.value }; }
            if (t.kind === 'gap') { i++; return { type: 'gap' }; }
            const pairs = { '(': ')', '[': ']', '{': '}' };
            if (pairs[t.kind]) {
                i++;
                const inner = alternatives(pairs[t.kind]);
                if (!peek() || peek().kind !== pairs[t.kind]) {
                    errors.push(`${at()}: Zu ${t.kind} aus Zeile ${t.line} fehlt ${pairs[t.kind]}.`);
                } else i++;
                if (t.kind === '(') return inner;
                return { type: t.kind === '[' ? 'opt' : 'rep', item: inner };
            }
            errors.push(`${at()}: ${t.value} steht hier falsch.`);
            i++;
            return null;
        }

        const rules = [];
        while (i < tokens.length) {
            if (!startsRule()) {
                errors.push(`${at()}: Eine Regel beginnt mit ihrem Namen und =, etwa: number = digit { digit }`);
                while (i < tokens.length && !startsRule()) i++;
                continue;
            }
            const name = tokens[i].value;
            i += 2;
            const expr = alternatives(null);
            if (peek() && peek().kind === '.') i++;
            if (peek() && !startsRule()) {
                errors.push(`${at()}: ${peek().value} passt hier nicht; fehlt davor eine Klammer?`);
                while (i < tokens.length && !startsRule()) i++;
            }
            rules.push({ name, expr });
        }
        return { rules, errors };
    }

    const same = (a, b) => JSON.stringify(a) === JSON.stringify(b);

    // a { s a } → a loop that passes a once and comes back through s, as
    // Wirth drew it; in a sequence the a in front goes into the loop
    function wirthLoops(node) {
        if (node.type !== 'seq') return node;
        const items = [];
        for (const item of node.items) {
            const last = items[items.length - 1];
            if (item.type === 'rep' && last) {
                const body = item.item.type === 'seq' ? item.item.items : [item.item];
                const tail = body[body.length - 1];
                if (body.length >= 2 && same(tail, last)) {
                    const sep = body.length === 2 ? body[0] : { type: 'seq', items: body.slice(0, -1) };
                    items[items.length - 1] = { type: 'loop', item: last, sep };
                    continue;
                }
                if (body.length === 1 && same(body[0], last)) {
                    items[items.length - 1] = { type: 'loop', item: last, sep: null };
                    continue;
                }
            }
            items.push(item);
        }
        return items.length === 1 ? items[0] : { type: 'seq', items };
    }

    // ---------- railroad layout --------------------------------------------------------

    // Every part has a width, how far it reaches above (up) and below (down)
    // the line it sits on, and draw(x, y, out): its line enters at (x, y) and
    // leaves at (x + w, y).
    function measure(node, m) {
        switch (node.type) {
        case 't':
        case 'n': {
            const w = Math.max(BOX, Math.ceil(m(node.type === 't' ? node.text : node.name) + 2 * PAD));
            return { w, up: BOX / 2, down: BOX / 2, node };
        }
        case 'gap':
            return { w: Math.ceil(m('…') + 8), up: BOX / 2, down: BOX / 2, node };
        case 'skip':
            return { w: 0, up: 0, down: 0, node };
        case 'seq': {
            const parts = node.items.map((n) => measure(n, m));
            return {
                w: parts.reduce((s, p) => s + p.w, 0) + GAP * (parts.length - 1),
                up: Math.max(...parts.map((p) => p.up)),
                down: Math.max(...parts.map((p) => p.down)),
                node, parts,
            };
        }
        case 'alt':
            return choice(node.items.map((n) => measure(n, m)), 0, node);
        case 'opt':
            return choice([measure({ type: 'skip' }, m), measure(node.item, m)], 1, node);
        case 'rep':
            return choice([measure({ type: 'skip' }, m), measure({ type: 'loop', item: node.item, sep: null }, m)], 1, node);
        case 'loop': {
            const item = measure(node.item, m);
            const sep = node.sep ? measure(node.sep, m) : null;
            const inner = Math.max(item.w, sep ? sep.w : 0);
            const dy = Math.max(2 * R, item.down + VGAP + (sep ? sep.up : 0));
            return { w: inner + 4 * R, up: item.up, down: dy + (sep ? sep.down : 0), node, item, sep, inner, dy };
        }
        default:
            throw new Error(`unknown part ${node.type}`);
        }
    }

    // Rows of a choice: main on the line, those before it above, those after
    // it below, each at least 2 R away so that its curves fit
    function choice(rows, main, node) {
        const inner = Math.max(...rows.map((r) => r.w));
        const offsets = [];
        offsets[main] = 0;
        for (let k = main - 1; k >= 0; k--) {
            offsets[k] = offsets[k + 1] - Math.max(2 * R, rows[k + 1].up + VGAP + rows[k].down);
        }
        for (let k = main + 1; k < rows.length; k++) {
            offsets[k] = offsets[k - 1] + Math.max(2 * R, rows[k - 1].down + VGAP + rows[k].up);
        }
        const up = Math.max(...rows.map((r, k) => r.up - offsets[k]));
        const down = Math.max(...rows.map((r, k) => r.down + offsets[k]));
        return { w: inner + 4 * R, up, down, node, rows, main, offsets, inner };
    }

    const line = (x1, y1, x2, y2) => (Math.abs(x2 - x1) < 0.05 && Math.abs(y2 - y1) < 0.05) ? '' : `<path class="sd-line" d="M${f(x1)} ${f(y1)}L${f(x2)} ${f(y2)}"/>`;
    // an arrowhead with its tip at (x, y), pointing along (dx, dy)
    const head = (x, y, dx, dy) => {
        const n = Math.hypot(dx, dy);
        const [ux, uy] = [dx / n, dy / n];
        const [bx, by] = [x - ux * 8, y - uy * 8];
        return `<path class="sd-head" d="M${f(x)} ${f(y)}L${f(bx - uy * 4)} ${f(by + ux * 4)}L${f(bx + uy * 4)} ${f(by - ux * 4)}z"/>`;
    };

    // accent: names and terminals drawn in the accent colour
    function draw(p, x, y, out, accent) {
        const node = p.node;
        if (node.type === 't' || node.type === 'n') {
            const label = node.type === 't' ? node.text : node.name;
            const rx = node.type === 't' ? BOX / 2 : 2;
            const hot = accent && accent.includes(label) ? ' sd-accent' : '';
            out.push(`<rect class="${node.type === 't' ? 'sd-terminal' : 'sd-nonterminal'}${hot}" x="${f(x)}" y="${f(y - BOX / 2)}" width="${p.w}" height="${BOX}" rx="${rx}"/>`);
            out.push(`<text class="${node.type === 't' ? 'sd-text' : 'sd-text sd-name'}${hot}" x="${f(x + p.w / 2)}" y="${f(y + BASELINE)}" text-anchor="middle">${esc(label)}</text>`);
        } else if (node.type === 'gap') {
            out.push(`<text class="sd-text sd-gap" x="${f(x + p.w / 2)}" y="${f(y + BASELINE)}" text-anchor="middle">…</text>`);
        } else if (node.type === 'seq') {
            let cx = x;
            p.parts.forEach((part, k) => {
                if (k > 0) { out.push(line(cx, y, cx + GAP, y)); cx += GAP; }
                draw(part, cx, y, out, accent);
                cx += part.w;
            });
        } else if (p.rows) {
            const right = x + p.w;
            p.rows.forEach((row, k) => {
                const ry = y + p.offsets[k];
                const start = x + 2 * R + (p.inner - row.w) / 2;
                if (row.node.type === 'gap') { draw(row, start, ry, out, accent); return; }
                if (k === p.main) {
                    out.push(line(x, y, start, y));
                    draw(row, start, y, out, accent);
                    out.push(line(start + row.w, y, right, y));
                    return;
                }
                // down (or up) on the left, along the row, back on the right
                const s = ry > y ? 1 : -1;
                const a = s > 0 ? 1 : 0;
                out.push(`<path class="sd-line" d="M${f(x)} ${f(y)}A${R} ${R} 0 0 ${a} ${f(x + R)} ${f(y + s * R)}` +
                    `L${f(x + R)} ${f(ry - s * R)}A${R} ${R} 0 0 ${1 - a} ${f(x + 2 * R)} ${f(ry)}L${f(start)} ${f(ry)}"/>`);
                draw(row, start, ry, out, accent);
                out.push(`<path class="sd-line" d="M${f(start + row.w)} ${f(ry)}L${f(right - 2 * R)} ${f(ry)}` +
                    `A${R} ${R} 0 0 ${1 - a} ${f(right - R)} ${f(ry - s * R)}L${f(right - R)} ${f(y + s * R)}A${R} ${R} 0 0 ${a} ${f(right)} ${f(y)}"/>`);
            });
        } else if (node.type === 'loop') {
            const right = x + p.w;
            const start = x + 2 * R + (p.inner - p.item.w) / 2;
            out.push(line(x, y, start, y));
            draw(p.item, start, y, out, accent);
            out.push(line(start + p.item.w, y, right, y));
            // back below, from right to left
            const by = y + p.dy;
            const parts = [`M${f(right - 2 * R)} ${f(y)}A${R} ${R} 0 0 1 ${f(right - R)} ${f(y + R)}L${f(right - R)} ${f(by - R)}A${R} ${R} 0 0 1 ${f(right - 2 * R)} ${f(by)}`];
            if (p.sep) {
                const sx = x + 2 * R + (p.inner - p.sep.w) / 2;
                parts.push(`L${f(sx + p.sep.w)} ${f(by)}`);
                out.push(`<path class="sd-line" d="${parts.join('')}"/>`);
                draw(p.sep, sx, by, out, accent);
                out.push(`<path class="sd-line" d="M${f(sx)} ${f(by)}L${f(x + 2 * R)} ${f(by)}A${R} ${R} 0 0 1 ${f(x + R)} ${f(by - R)}L${f(x + R)} ${f(y + R)}A${R} ${R} 0 0 1 ${f(x + 2 * R)} ${f(y)}"/>`);
                // the way back points up on the left, where there is room
                out.push(head(x + R, y + R + (p.dy - 2 * R) / 2 - 4, 0, -1));
            } else {
                parts.push(`L${f(x + 2 * R)} ${f(by)}A${R} ${R} 0 0 1 ${f(x + R)} ${f(by - R)}L${f(x + R)} ${f(y + R)}A${R} ${R} 0 0 1 ${f(x + 2 * R)} ${f(y)}`);
                out.push(`<path class="sd-line" d="${parts.join('')}"/>`);
                out.push(head(x + p.w / 2 - 4, by, -1, 0));
            }
        }
    }

    function style(file) {
        const v = (name, fallback) => (file ? fallback : `var(--sd-${name},${fallback})`);
        return `.sd-line{fill:none;stroke:${v('line', COLOURS.line)};stroke-width:1.6}` +
            `.sd-head{fill:${v('line', COLOURS.line)}}` +
            `.sd-terminal,.sd-nonterminal{fill:${v('paper', COLOURS.paper)};stroke:${v('ink', COLOURS.ink)};stroke-width:1.5}` +
            `.sd-text{font-family:${MONO};font-size:${FONT_SIZE}px;fill:${v('ink', COLOURS.ink)};white-space:pre}` +
            `.sd-name{font-style:italic}` +
            `.sd-gap{fill:${v('line', COLOURS.line)}}` +
            `.sd-title{font-family:${MONO};font-size:${FONT_SIZE}px;font-weight:700;fill:${v('ink', COLOURS.ink)}}` +
            `.sd-edge{stroke:${v('line', COLOURS.line)};stroke-width:1.4}` +
            `rect.sd-accent{stroke:${v('accent', COLOURS.accent)};stroke-width:2.5}text.sd-accent{fill:${v('accent', COLOURS.accent)}}`;
    }

    function wrap(width, height, inner, opts, text) {
        const label = opts.label || 'Syntaxdiagramm';
        const scale = opts.scale || 1;
        const W = Math.ceil(width);
        const H = Math.ceil(height);
        return `<svg xmlns="http://www.w3.org/2000/svg" class="syntax-diagram-svg" viewBox="0 0 ${W} ${H}" width="${Math.ceil(W * scale)}" height="${Math.ceil(H * scale)}" role="img" aria-label="${esc(label)}">` +
            `<title>${esc(label)}</title><desc>${esc(text.trim())}</desc><style>${style(opts.file)}</style>` +
            (opts.background ? `<rect width="${W}" height="${H}" fill="${esc(opts.background)}"/>` : '') +
            `${inner}</svg>`;
    }

    // The diagrams of a grammar as one SVG, one rule under the other, its
    // name over it; with errors { svg: null, errors }. options: measure(text,
    // size) in pixels (a canvas in the browser), file (colours written out,
    // for render-svg.rb), background, label (alt text), scale, only (the
    // names of the rules to draw), accent (names and terminals in the accent
    // colour).
    function svg(text, options) {
        const opts = Object.assign({ measure: estimateWidth, file: false, background: null, label: null, only: null }, options || {});
        const m = (t) => opts.measure(t, FONT_SIZE);
        const parsed = parse(text);
        if (parsed.errors.length || parsed.rules.length === 0) return { svg: null, errors: parsed.errors, rules: parsed.rules };
        const rules = opts.only ? parsed.rules.filter((r) => opts.only.includes(r.name)) : parsed.rules;
        const out = [];
        const margin = 4;
        const lead = 18;      // the line from the start to the first box, and after the last
        let y = margin;
        let width = 0;
        rules.forEach((rule, k) => {
            const p = measure(rule.expr, m);
            if (k > 0) y += 18;
            out.push(`<text class="sd-title" x="${margin}" y="${f(y + FONT_SIZE)}">${esc(rule.name)}</text>`);
            y += FONT_SIZE + 8;
            const ly = y + p.up;
            const x0 = margin + 6;
            out.push(`<circle class="sd-head" cx="${f(x0)}" cy="${f(ly)}" r="3.5"/>`);
            out.push(line(x0, ly, x0 + lead, ly));
            draw(p, x0 + lead, ly, out, opts.accent);
            const end = x0 + lead + p.w + lead;
            out.push(line(x0 + lead + p.w, ly, end - 1, ly));
            out.push(head(end, ly, 1, 0));
            out.push(`<path class="sd-line" d="M${f(end + 1)} ${f(ly - 7)}L${f(end + 1)} ${f(ly + 7)}"/>`);
            width = Math.max(width, end + 4, margin + m(rule.name));
            y += p.up + p.down;
        });
        const W = width + margin;
        const H = y + margin;
        return { svg: wrap(W, H, out.join(''), opts, text), errors: [], rules: parsed.rules, width: W, height: H };
    }

    // ---------- syntax trees ----------------------------------------------------------

    // node = name [ "(" { node } ")" ] | terminal
    function parseTree(text) {
        const errors = [];
        const tokens = tokenize(text, errors).filter((t) => t.kind !== '.');
        let i = 0;
        function node() {
            const t = tokens[i];
            if (!t) { errors.push('Am Ende fehlt ein Knoten.'); return null; }
            i++;
            if (t.kind === 'terminal') return { type: 't', text: t.value, children: [] };
            if (t.kind !== 'name') { errors.push(`Zeile ${t.line}: ${t.value} steht hier falsch.`); return null; }
            const n = { type: 'n', name: t.value, children: [] };
            if (tokens[i] && tokens[i].kind === '(') {
                i++;
                while (tokens[i] && tokens[i].kind !== ')') {
                    const child = node();
                    if (!child) return n;
                    n.children.push(child);
                }
                if (!tokens[i]) errors.push(`Zu ( nach ${t.value} fehlt ).`);
                else i++;
            }
            return n;
        }
        const tree = node();
        if (i < tokens.length) errors.push(`Zeile ${tokens[i].line}: Nach dem Baum steht noch ${tokens[i].value}.`);
        return { tree, errors };
    }

    // Every subtree gets a span as wide as its own box or its children side by
    // side, whichever is wider; a node stands in the middle of its span, its
    // children side by side in it, one row lower. (Terminals all in the
    // bottom row would read like the word, but their long edges pass behind
    // other boxes.)
    function treeSvg(text, options) {
        const opts = Object.assign({ measure: estimateWidth, file: false, background: null, label: null }, options || {});
        const m = (t) => opts.measure(t, FONT_SIZE);
        const { tree, errors } = parseTree(text);
        if (errors.length || !tree) return { svg: null, errors };
        const SIB = 12;
        const boxW = (n) => Math.max(BOX, Math.ceil(m(n.type === 't' ? n.text : n.name) + 2 * PAD));
        const depth = (n) => (n.children.length ? 1 + Math.max(...n.children.map(depth)) : 0);
        const bottom = depth(tree);
        const span = (n) => (n.span = Math.max(boxW(n), n.children.reduce((s, c) => s + span(c), 0) + SIB * Math.max(0, n.children.length - 1)));
        span(tree);
        const margin = 4;
        const placed = [];
        function place(n, left, level) {
            n.x = left + n.span / 2;
            n.level = level;
            placed.push(n);
            const kids = n.children.reduce((s, c) => s + c.span, 0) + SIB * Math.max(0, n.children.length - 1);
            let cx = left + (n.span - kids) / 2;
            for (const c of n.children) { place(c, cx, level + 1); cx += c.span + SIB; }
        }
        place(tree, margin, 0);
        const yOf = (n) => margin + BOX / 2 + n.level * ROW;
        const out = [];
        for (const n of placed) for (const c of n.children) out.push(`<path class="sd-edge" d="M${f(n.x)} ${f(yOf(n))}L${f(c.x)} ${f(yOf(c))}"/>`);
        for (const n of placed) {
            const w = boxW(n);
            draw({ node: n, w }, n.x - w / 2, yOf(n), out, opts.accent);
        }
        const W = tree.span + 2 * margin;
        const H = bottom * ROW + BOX + 2 * margin;
        return { svg: wrap(W, H, out.join(''), Object.assign({ label: 'Syntaxbaum' }, opts), text), errors: [], tree, width: W, height: H };
    }

    // ---------- in the browser -----------------------------------------------------

    function canvasMeasure() {
        if (typeof document === 'undefined') return estimateWidth;
        const ctx = document.createElement('canvas').getContext('2d');
        if (!ctx) return estimateWidth;
        return (text, size) => {
            ctx.font = `${size || FONT_SIZE}px ${MONO}`;
            return ctx.measureText(String(text)).width;
        };
    }

    // Every ```syntaxdiagramm and ```syntaxbaum block of the page becomes its
    // picture; the text stays readable for screen readers in <desc>.
    function renderPage() {
        const blocks = document.querySelectorAll('pre.code-syntaxdiagramm, pre.code-syntaxbaum');
        if (!blocks.length) return;
        const measureText = canvasMeasure();
        blocks.forEach((pre) => {
            const text = pre.textContent;
            const isTree = pre.classList.contains('code-syntaxbaum');
            const label = pre.dataset.label || (isTree ? 'Syntaxbaum' : `Syntaxdiagramm: ${text.trim().split('\n')[0]}`);
            // a little larger than drawn, so that the boxes read like the text beside them
            const opts = { measure: measureText, label, scale: 1.2 };
            const result = isTree ? treeSvg(text, opts) : svg(text, opts);
            if (!result.svg) return; // an error: the text stays
            const figure = document.createElement('div');
            figure.className = 'syntax-diagram';
            figure.innerHTML = result.svg;
            const box = pre.parentElement && pre.parentElement.classList.contains('code-copy-box') ? pre.parentElement : pre;
            box.replaceWith(figure);
        });
    }

    if (typeof document !== 'undefined') {
        const start = () => (document.fonts && document.fonts.ready ? document.fonts.ready : Promise.resolve()).then(renderPage);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
        else start();
    }

    return { parse, svg, parseTree, treeSvg, estimateWidth, canvasMeasure, esc, COLOURS };
});
