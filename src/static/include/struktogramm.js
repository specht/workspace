// Struktogramme (Nassi-Shneiderman diagrams, DIN 66261) from the Handbuch's
// pseudocode (Rouge::Lexers::Pseudocode): one module for the ```struktogramm
// blocks of a page, the playground of reference/struktogramme and the tests
// (test/static/struktogramm.test.cjs). No DOM in parse and svg.
//
//   check_digit(digits):            a function: its name over the diagram
//       sum ← 0                     a statement: a box
//       for i from 0 to 11:         a loop that counts (also: for each x in list)
//           if i mod 2 == 0:        a decision: the condition in a triangle,
//               sum ← sum + digits[i]       »wahr« left, »falsch« right
//           else:                   (else if: a decision in the right column)
//               sum ← sum + 3 · digits[i]
//       return (10 - sum mod 10) mod 10
//   while x > 0: …                  a loop that checks first
//   repeat: … until x == 0          a loop that checks last
//
// A block is indented under the line with the colon; one statement may also
// follow the colon on the same line (if x < 0: x ← -x).
//
// In a page: a ```struktogramm block is highlighted like pseudocode on the
// server (code_highlight.rb) and replaced by its diagram here; without
// JavaScript the pseudocode stays.
(function (root, factory) {
    if (typeof module === 'object' && module.exports) module.exports = factory();
    else root.Struktogramm = factory();
})(typeof self !== 'undefined' ? self : this, function () {
    'use strict';

    const FONT_SIZE = 14;
    const MONO = "'0xProto Nerd Font Mono', ui-monospace, monospace";
    const SANS = "'IBM Plex Sans', system-ui, sans-serif";
    const ROW = 30;          // height of a statement
    const PAD = 9;           // text to the border
    const BAR = 24;          // the bar left of a loop's body
    const HEAD = 58;         // height of a decision's triangle
    const EMPTY = 48;        // width of an empty column
    const BASELINE = ROW / 2 + FONT_SIZE * 0.35;
    // Depths in a decision's triangle, as shares of its height: the
    // condition's baseline, and the labels »wahr« and »falsch«.
    const COND_DEPTH = (BASELINE - 2) / HEAD;
    const LABEL_DEPTH = (HEAD - 7) / HEAD;
    const KEYWORDS = new Set(['if', 'else', 'while', 'for', 'each', 'in', 'from', 'to', 'down', 'repeat',
        'until', 'return', 'not', 'and', 'or', 'mod', 'div', 'break', 'continue']);
    const NAME = '[\\p{L}_][\\p{L}\\p{N}_]*[?!]?';
    const RE_FUNCTION = new RegExp(`^(${NAME})\\s*\\((.*)\\)\\s*:\\s*$`, 'u');

    // A monospace character is 0.6 of the font size (0xProto, Menlo, DejaVu Sans Mono).
    function estimateWidth(text, size) {
        return Array.from(String(text)).length * (size || FONT_SIZE) * 0.6;
    }

    function esc(text) {
        return String(text).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    // ---------- parsing --------------------------------------------------------

    function readLines(text) {
        const lines = [];
        String(text).replace(/\r\n?/g, '\n').split('\n').forEach((raw, i) => {
            const line = raw.replace(/\t/g, '    ').replace(/\s+$/, '');
            if (line.trim() === '') return;
            lines.push({ number: i + 1, indent: line.length - line.trimStart().length, text: line.trim() });
        });
        return lines;
    }

    // The keyword a header line starts with, or null for a statement.
    function headerOf(text) {
        let m;
        if ((m = /^else\s+if\b\s*(.*)$/u.exec(text))) return { kind: 'elseif', rest: m[1] };
        if ((m = /^(if|while|for|else|repeat)\b\s*(.*)$/u.exec(text))) return { kind: m[1], rest: m[2] };
        if ((m = /^until\b\s*(.*)$/u.exec(text))) return { kind: 'until', rest: m[1] };
        return null;
    }

    // »cond: body« → [cond, body]; the colon that ends the condition is the
    // last one outside brackets that is followed by a space or the end
    // (so list[1:3] stays whole).
    function splitColon(rest) {
        let depth = 0;
        let at = -1;
        for (let i = 0; i < rest.length; i++) {
            const c = rest[i];
            if ('([{'.includes(c)) depth++;
            else if (')]}'.includes(c)) depth--;
            else if (c === ':' && depth === 0 && (i === rest.length - 1 || /\s/.test(rest[i + 1]))) {
                at = i;
                break;
            }
        }
        if (at < 0) return null;
        return [rest.slice(0, at).trim(), rest.slice(at + 1).trim()];
    }

    function parse(text) {
        const lines = readLines(text);
        const errors = [];
        const error = (line, msg) => errors.push({ line: line.number, msg });
        let i = 0;

        // the block under lines[i - 1], or the statement after its colon
        function body(owner, inline) {
            if (inline) return [statementOrHeader({ number: owner.number, indent: owner.indent + 1, text: inline }, true)];
            if (i < lines.length && lines[i].indent > owner.indent) return block(lines[i].indent);
            error(owner, 'Nach dem Doppelpunkt fehlt, was geschehen soll: Schreib es eingerückt in die Zeilen darunter.');
            return [];
        }

        function statementOrHeader(line, inline) {
            const head = headerOf(line.text);
            if (inline && head) {
                error(line, 'Hinter dem Doppelpunkt darf nur eine einfache Anweisung stehen. Schreib den Block eingerückt in die nächsten Zeilen.');
                return { type: 'statement', text: line.text, line: line.number };
            }
            return { type: 'statement', text: line.text, line: line.number };
        }

        function decision(line, rest) {
            const parts = splitColon(rest);
            if (!parts || parts[0] === '') {
                error(line, parts ? 'Zwischen »if« und dem Doppelpunkt fehlt die Bedingung.' : 'Hinter der Bedingung fehlt ein Doppelpunkt: if x > 0:');
                return { type: 'statement', text: line.text, line: line.number };
            }
            const node = { type: 'if', cond: parts[0], then: body(line, parts[1]), else: [], line: line.number };
            if (i < lines.length && lines[i].indent === line.indent) {
                const next = headerOf(lines[i].text);
                if (next && next.kind === 'elseif') {
                    const elseLine = lines[i++];
                    node.else = [decision(elseLine, next.rest)];
                } else if (next && next.kind === 'else') {
                    const elseLine = lines[i++];
                    const parts2 = splitColon(next.rest);
                    if (!parts2 || parts2[0] !== '') {
                        error(elseLine, '»else« steht allein, mit einem Doppelpunkt: else:');
                    } else {
                        node.else = body(elseLine, parts2[1]);
                    }
                }
            }
            return node;
        }

        function block(indent) {
            const nodes = [];
            while (i < lines.length && lines[i].indent >= indent) {
                const line = lines[i];
                if (line.indent > indent) {
                    error(line, 'Diese Zeile ist tiefer eingerückt als die davor, aber dort endet keine Zeile mit einem Doppelpunkt.');
                    i++;
                    continue;
                }
                i++;
                const head = headerOf(line.text);
                const fn = RE_FUNCTION.exec(line.text);
                if (fn && !head) {
                    if (indent > 0) error(line, 'Eine Funktion steht ganz links, nicht eingerückt.');
                    nodes.push({ type: 'function', name: fn[1], params: fn[2].trim(), body: body(line, ''), line: line.number });
                } else if (!head) {
                    nodes.push({ type: 'statement', text: line.text, line: line.number });
                } else if (head.kind === 'if') {
                    nodes.push(decision(line, head.rest));
                } else if (head.kind === 'else' || head.kind === 'elseif') {
                    error(line, '»else« gehört zu einem »if« direkt darüber, genauso weit eingerückt.');
                } else if (head.kind === 'until') {
                    error(line, '»until« beendet eine Schleife, die mit »repeat:« beginnt, genauso weit eingerückt.');
                } else if (head.kind === 'repeat') {
                    const parts = splitColon(head.rest);
                    if (!parts || parts[0] !== '') {
                        error(line, '»repeat« steht allein, mit einem Doppelpunkt: repeat:');
                        continue;
                    }
                    const inner = body(line, parts[1]);
                    const end = lines[i];
                    const until = end && end.indent === line.indent ? headerOf(end.text) : null;
                    if (!until || until.kind !== 'until' || until.rest === '') {
                        error(line, 'Zu »repeat:« fehlt darunter, genauso weit eingerückt, »until« mit der Bedingung, bei der die Schleife aufhört.');
                        nodes.push({ type: 'repeat', cond: '', body: inner, line: line.number });
                        continue;
                    }
                    i++;
                    nodes.push({ type: 'repeat', cond: until.rest.replace(/:\s*$/, ''), body: inner, line: line.number });
                } else {
                    // while, for
                    const parts = splitColon(head.rest);
                    if (!parts || parts[0] === '') {
                        error(line, parts ? `Hinter »${head.kind}« fehlt, wie oft die Schleife läuft.` :
                            `Am Ende der Zeile fehlt ein Doppelpunkt: ${head.kind === 'for' ? 'for i from 0 to 9:' : 'while x > 0:'}`);
                        nodes.push({ type: 'statement', text: line.text, line: line.number });
                        continue;
                    }
                    nodes.push({ type: 'loop', head: `${head.kind} ${parts[0]}`, body: body(line, parts[1]), line: line.number });
                }
            }
            return nodes;
        }

        const top = block(lines.length ? lines[0].indent : 0);
        while (i < lines.length) top.push(...block(lines[i].indent));
        // statements outside a function form a diagram of their own
        const diagrams = [];
        let loose = null;
        for (const node of top) {
            if (node.type === 'function') {
                loose = null;
                diagrams.push({ title: `${node.name}(${node.params})`, body: node.body, line: node.line });
            } else {
                if (!loose) diagrams.push(loose = { title: null, body: [], line: node.line });
                loose.body.push(node);
            }
        }
        return { diagrams, errors };
    }

    // ---------- layout -----------------------------------------------------------

    function measureAll(nodes, m) {
        if (nodes.length === 0) return { w: EMPTY, h: ROW };
        let w = 0;
        let h = 0;
        for (const node of nodes) {
            const s = measure(node, m);
            w = Math.max(w, s.w);
            h += s.h;
        }
        return { w, h };
    }

    function measure(node, m) {
        if (node.size) return node.size;
        let size;
        if (node.type === 'statement') {
            size = { w: m(node.text) + 2 * PAD, h: ROW };
        } else if (node.type === 'if') {
            // Each column is wide enough for its label in the corner below
            // the line; the condition fits between the two lines at the
            // depth of its baseline.
            const label = (text) => (m(text, 12) + PAD + 12) / LABEL_DEPTH;
            const a = measureAll(node.then, m);
            const b = measureAll(node.else, m);
            const cols = [{ w: Math.max(a.w, label('wahr')), h: a.h }, { w: Math.max(b.w, label('falsch')), h: b.h }];
            const cond = (m(node.cond) + PAD + 6) / (1 - COND_DEPTH);
            size = { w: Math.max(cols[0].w + cols[1].w, cond), h: HEAD + Math.max(a.h, b.h) };
            node.cols = cols;
        } else if (node.type === 'loop' || node.type === 'repeat') {
            const b = measureAll(node.body, m);
            const head = m(node.type === 'loop' ? node.head : `until ${node.cond}`) + 2 * PAD;
            size = { w: Math.max(head, BAR + b.w), h: ROW + b.h };
        }
        node.size = size;
        return size;
    }

    function textSVG(text, x, y, cls) {
        // keywords in the colour of the pseudocode's keywords
        const parts = String(text).split(/([\p{L}_][\p{L}\p{N}_]*)/u);
        const inner = parts.map((part) => KEYWORDS.has(part) ? `<tspan class="sg-kw">${esc(part)}</tspan>` : esc(part)).join('');
        return `<text class="${cls}" x="${x.toFixed(1)}" y="${y.toFixed(1)}">${inner}</text>`;
    }

    function drawAll(nodes, x, y, w, h, out, m) {
        if (nodes.length === 0) {
            out.push(`<rect class="sg-box" x="${x.toFixed(1)}" y="${y.toFixed(1)}" width="${w.toFixed(1)}" height="${h.toFixed(1)}"/>`);
            return;
        }
        let top = y;
        nodes.forEach((node, k) => {
            const natural = measure(node, m).h;
            // the last one takes what is left, so that both columns of a decision end together
            const height = k === nodes.length - 1 ? y + h - top : natural;
            draw(node, x, top, w, height, out, m);
            top += height;
        });
    }

    function draw(node, x, y, w, h, out, m) {
        const rect = (rx, ry, rw, rh, cls = 'sg-box') =>
            out.push(`<rect class="${cls}" x="${rx.toFixed(1)}" y="${ry.toFixed(1)}" width="${rw.toFixed(1)}" height="${rh.toFixed(1)}"/>`);
        if (node.type === 'statement') {
            rect(x, y, w, h);
            out.push(textSVG(node.text, x + PAD, y + BASELINE, 'sg-text'));
        } else if (node.type === 'if') {
            const [a, b] = node.cols;
            const extra = w - (a.w + b.w);
            const left = a.w + extra * a.w / (a.w + b.w);
            const split = x + left;
            rect(x, y, w, HEAD, 'sg-head');
            out.push(`<path class="sg-line" d="M${x.toFixed(1)},${y.toFixed(1)} L${split.toFixed(1)},${(y + HEAD).toFixed(1)} L${(x + w).toFixed(1)},${y.toFixed(1)}"/>`);
            // the condition centred between the two lines at its baseline
            const cw = m(node.cond);
            const from = x + left * COND_DEPTH;
            const to = x + w - (w - left) * COND_DEPTH;
            const centre = Math.min(Math.max((from + to) / 2, from + 3 + cw / 2), to - 3 - cw / 2);
            out.push(textSVG(node.cond, centre - cw / 2, y + BASELINE - 2, 'sg-text'));
            out.push(`<text class="sg-label" x="${(x + PAD).toFixed(1)}" y="${(y + HEAD - 7).toFixed(1)}">wahr</text>`);
            out.push(`<text class="sg-label sg-end" x="${(x + w - PAD).toFixed(1)}" y="${(y + HEAD - 7).toFixed(1)}">falsch</text>`);
            drawAll(node.then, x, y + HEAD, left, h - HEAD, out, m);
            drawAll(node.else, split, y + HEAD, w - left, h - HEAD, out, m);
        } else if (node.type === 'loop') {
            rect(x, y, w, h);
            out.push(textSVG(node.head, x + PAD, y + BASELINE, 'sg-text'));
            drawAll(node.body, x + BAR, y + ROW, w - BAR, h - ROW, out, m);
        } else if (node.type === 'repeat') {
            rect(x, y, w, h);
            drawAll(node.body, x + BAR, y, w - BAR, h - ROW, out, m);
            out.push(textSVG(`until ${node.cond}`, x + PAD, y + h - ROW + BASELINE, 'sg-text'));
        }
    }

    const STYLE = `
    .sg-box{fill:var(--sg-paper,#fff);stroke:var(--sg-ink,#1f2937);stroke-width:1.3}
    .sg-head{fill:var(--sg-soft,#e8effc);stroke:var(--sg-ink,#1f2937);stroke-width:1.3}
    .sg-line{fill:none;stroke:var(--sg-ink,#1f2937);stroke-width:1.3}
    .sg-text{font-family:${MONO};font-size:${FONT_SIZE}px;fill:var(--sg-ink,#1f2937);white-space:pre}
    .sg-kw{fill:var(--sg-pen,#2b5fb4);font-weight:600}
    .sg-label{font-family:${SANS};font-size:12px;font-style:italic;fill:var(--sg-muted,#6b7280)}
    .sg-end{text-anchor:end}
    .sg-title{font-family:${MONO};font-size:${FONT_SIZE}px;font-weight:700;fill:var(--sg-ink,#1f2937)}`;

    // The diagrams of a text as one SVG, one under the other; with errors
    // { svg: null, errors }. options: measure(text, size) in pixels (a canvas
    // in the browser), background (a colour, for a file), label (alt text).
    function svg(text, options) {
        const opts = Object.assign({ measure: estimateWidth, background: null, label: null }, options || {});
        const m = (t, size) => opts.measure(t, size || FONT_SIZE);
        const parsed = parse(text);
        if (parsed.errors.length || parsed.diagrams.length === 0) return { svg: null, errors: parsed.errors, diagrams: parsed.diagrams };
        const out = [];
        const margin = 2;
        let y = margin;
        let width = 0;
        for (const d of parsed.diagrams) {
            const size = measureAll(d.body, m);
            const w = Math.max(size.w, d.title ? m(d.title) + 2 * PAD : 0);
            if (d.title) {
                out.push(`<text class="sg-title" x="${margin}" y="${(y + FONT_SIZE).toFixed(1)}">${esc(d.title)}</text>`);
                y += FONT_SIZE + 8;
            }
            drawAll(d.body, margin, y, w, size.h, out, m);
            y += size.h + 22;
            width = Math.max(width, w);
        }
        const W = Math.ceil(width + 2 * margin);
        const H = Math.ceil(y - 22 + margin);
        const label = opts.label || 'Struktogramm';
        const head = `<svg xmlns="http://www.w3.org/2000/svg" class="struktogramm-svg" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}" role="img" aria-label="${esc(label)}">` +
            `<title>${esc(label)}</title><desc>${esc(text)}</desc><style>${STYLE}</style>` +
            (opts.background ? `<rect width="${W}" height="${H}" fill="${esc(opts.background)}"/>` : '');
        return { svg: `${head}${out.join('')}</svg>`, errors: [], diagrams: parsed.diagrams, width: W, height: H };
    }

    // ---------- in the browser -----------------------------------------------------

    // Widths measured with the font the diagram uses, once it has loaded.
    function canvasMeasure() {
        if (typeof document === 'undefined') return estimateWidth;
        const ctx = document.createElement('canvas').getContext('2d');
        if (!ctx) return estimateWidth;
        return (text, size) => {
            ctx.font = `${size || FONT_SIZE}px ${size && size !== FONT_SIZE ? SANS : MONO}`;
            return ctx.measureText(String(text)).width;
        };
    }

    // Every ```struktogramm block of the page (pre.code-struktogramm) becomes
    // its diagram; the pseudocode stays readable for screen readers in <desc>.
    function renderPage() {
        const blocks = document.querySelectorAll('pre.code-struktogramm');
        if (!blocks.length) return;
        const measureText = canvasMeasure();
        blocks.forEach((pre) => {
            const text = pre.textContent;
            const result = svg(text, { measure: measureText, label: `Struktogramm: ${text.trim().split('\n')[0]}` });
            if (!result.svg) return; // an error: the pseudocode stays
            const figure = document.createElement('div');
            figure.className = 'struktogramm';
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

    return { parse, svg, estimateWidth, canvasMeasure, esc };
});
