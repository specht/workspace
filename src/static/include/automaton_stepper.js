// The automaton stepper (automata, Kellerautomaten, Turingmaschinen): an
// automaton at work, one step at a time. Above the automaton its tape, the
// cell it reads framed in purple; the state it is in wears the purple ring,
// and the arrow it takes pulses, its label (the line of it that was used)
// purple too.
//
// Three kinds, one module:
// - A DEA or a NEA reads a word, one symbol per step. A NEA can be in
//   several states at once: every one of them wears a ring, a new ring comes
//   out of the state it is reached from, and the ring of a way that ends (no
//   arrow for the symbol: the error state, which is not drawn) goes.
//
//     <div class='automaton-stepper' data-stepper data-title='…'
//          data-states='q0 q1 q2' data-start='q0' data-accept='q0'
//          data-transitions='q0 0 q0, q0 1 q1, q1 0 q2, …' data-input='10010'
//          data-alphabet='01'></div>
//
//   A transition 'q0 0-9 q0' stands for ten.
// - A pushdown automaton (data-kind='pushdown') reads a word and has a
//   stack beside the drawing, # at the bottom: each rule is 'state read top
//   push next', push written bottom to top, ε for nothing. It accepts when
//   the word is read, it is in an accepting state and only # is left; a
//   symbol without a rule rejects at once (as on the Kellerautomaten page).
//
//     data-kind='pushdown' data-rules='q0 ( # #( q0; q0 ( ( (( q0; q0 ) ( ε q0'
//
// - A Turing machine (data-kind='turing') works on its tape: each rule is
//   'state read write move next' with L or R, _ an empty cell. It stops in
//   a state of data-accept (the result is the tape) or where no rule fits;
//   with data-result='accept' it accepts in data-accept and rejects where no
//   rule fits.
//
//     data-kind='turing' data-rules='q0 0 0 R q0; q0 _ _ L q1; …'
//
// data-fixed: the input cannot be edited (a widget a task's answer must not
// come from). Steps, play, the timeline, the keys and the lightbox come from
// stepper_keys.js, the movement from stepper_motion.js. The engine (machine,
// trace) runs without a page and is tested on its own
// (test/static/automaton_stepper.test.cjs).
(function (root) {
    'use strict';
    // '0-9' is ten symbols
    const expand = (symbols) => symbols.replace(/(.)-(.)/g, (_, a, b) => {
        let out = '';
        for (let c = a.charCodeAt(0); c <= b.charCodeAt(0); c++) out += String.fromCharCode(c);
        return out;
    });
    // the symbols of an arrow, runs shortened again: 0123456789 is 0–9
    const shorten = (symbols) => {
        const sorted = [...new Set(symbols)].sort();
        const runs = [];
        for (const s of sorted) {
            const last = runs[runs.length - 1];
            if (last && s.charCodeAt(0) === last[1].charCodeAt(0) + 1) last[1] = s;
            else runs.push([s, s]);
        }
        return runs.map(([a, b]) => (a === b ? a : b.charCodeAt(0) === a.charCodeAt(0) + 1 ? `${a}, ${b}` : `${a}–${b}`)).join(', ');
    };
    const sets = (list) => list.join(', ');
    const BLANK = '_';
    const EPS = 'ε';
    // the longest run a Turing machine is shown (one that never stops)
    const MAX_STEPS = 200;

    // The machine from the widget's data attributes
    function machine(data) {
        const states = data.states.trim().split(/\s+/);
        const accept = new Set((data.accept || '').trim().split(/\s+/).filter(Boolean));
        const start = data.start || states[0];
        const alphabet = expand(data.alphabet || '').replace(EPS, '');
        if (data.kind === 'pushdown' || data.kind === 'turing') {
            const rules = [];
            for (const part of (data.rules || '').split(';').map((s) => s.trim()).filter(Boolean)) {
                const f = part.split(/\s+/);
                if (data.kind === 'turing') {
                    const [from, read, write, move, to] = f;
                    rules.push({ from, read, write, move, to, label: `${read} / ${write}, ${move}` });
                } else {
                    const [from, read, top, push, to] = f;
                    rules.push({ from, read, top, push: push === EPS ? '' : push, to, label: `${read}, ${top} / ${push}` });
                }
            }
            return { kind: data.kind, states, start, accept, rules, alphabet, columns: data.columns,
                bottom: data.bottom || '#', result: data.result === 'accept' ? 'accept' : 'tape' };
        }
        const delta = new Map();
        for (const part of (data.transitions || '').split(',').map((s) => s.trim()).filter(Boolean)) {
            const [from, symbols, to] = part.split(/\s+/);
            for (const s of expand(symbols)) {
                const key = `${from} ${s}`;
                if (!delta.has(key)) delta.set(key, []);
                if (!delta.get(key).includes(to)) delta.get(key).push(to);
            }
        }
        const nfa = data.kind === 'nfa' || [...delta.values()].some((to) => to.length > 1) || [...delta.keys()].some((k) => k.endsWith(` ${EPS}`));
        return { kind: nfa ? 'nfa' : 'dfa', states, start, accept, delta, nfa, columns: data.columns, alphabet };
    }

    // The arrows to draw: one per symbol or rule, AutomatonLayout joins the
    // labels of arrows between the same two states
    function arrows(a) {
        if (a.rules) return a.rules.map((r) => ({ from: r.from, to: r.to, label: r.label }));
        const edges = [];
        for (const [key, to] of a.delta) {
            const [from, symbol] = key.split(' ');
            for (const t of to) edges.push({ from, to: t, label: symbol });
        }
        return edges;
    }

    // ε: the states reached from these without reading a symbol, and the
    // arrows that bring each new one
    function closure(a, states) {
        const all = [...states];
        const taken = [];
        for (let i = 0; i < all.length; i++) {
            for (const t of a.delta.get(`${all[i]} ${EPS}`) || []) {
                if (all.includes(t)) continue;
                all.push(t);
                taken.push([all[i], t]);
            }
        }
        all.sort((x, y) => a.states.indexOf(x) - a.states.indexOf(y));
        return [all, taken];
    }

    // Every step of a DEA or a NEA: where the head is, the states the
    // automaton can be in, the arrows it took to get there (from, to) and
    // the message
    function finiteTrace(a, word) {
        const [first, firstEps] = closure(a, [a.start]);
        const reads = `Er liest ${word ? `»${word}«` : 'nichts'}.`;
        const steps = [{ at: -1, states: first, used: [], eps: firstEps, message: !a.nfa
            ? `Am Anfang ist der DEA in ${a.start}. ${reads}`
            : firstEps.length ? `Am Anfang ist der NEA in ${a.start} und, ohne ein Zeichen zu lesen (ε), auch in ${sets(first.filter((s) => s !== a.start))}. ${reads}`
                : `Am Anfang ist der NEA nur in ${a.start}. ${reads}` }];
        let current = first;
        [...word].forEach((symbol, i) => {
            const used = [];
            const next = [];
            const ended = [];
            for (const s of current) {
                const to = a.delta.get(`${s} ${symbol}`) || [];
                if (!to.length) ended.push(s);
                for (const t of to) {
                    used.push([s, t]);
                    if (!next.includes(t)) next.push(t);
                }
            }
            next.sort((x, y) => a.states.indexOf(x) - a.states.indexOf(y));
            // then everything an ε reaches from there
            const [withEps, eps] = closure(a, next);
            const reached = withEps.filter((t) => !next.includes(t));
            next.splice(0, next.length, ...withEps);
            let message;
            if (!a.nfa) message = next.length ? `${symbol}: von ${current[0]} nach ${next[0]}.` : `${symbol}: Aus ${current[0]} führt kein Pfeil mit ${symbol}. Der DEA steckt fest.`;
            else {
                // the ways that go on; of the others only that they end
                const ways = current.filter((s) => (a.delta.get(`${s} ${symbol}`) || []).length).map((s) => `${s} → ${sets(a.delta.get(`${s} ${symbol}`))}`);
                const end = ended.length === 0 ? '' : ways.length === 0 ? '' : ended.length === 1 ? ` Der Weg in ${ended[0]} endet.` : ' Die übrigen Wege enden.';
                message = ways.length
                    ? `${symbol}: ${ways.join('; ')}.${end}${reached.length ? ` Mit ε dazu: ${sets(reached)}.` : ''} Jetzt in ${sets(next)}.`
                    : `${symbol}: Aus ${sets(current)} führt kein Pfeil mit ${symbol}. Kein Weg geht weiter.`;
            }
            steps.push({ at: i, symbol, states: next, before: current, used, eps, ended, message });
            current = next;
        });
        // the end of the word: accepted if a state it can be in accepts
        const yes = current.some((s) => a.accept.has(s));
        const where = a.nfa ? `in ${current.length ? sets(current) : 'keinem Zustand mehr'}` : `in ${current[0] || 'keinem Zustand'}`;
        steps.push({ at: word.length, states: current, used: [], end: true, accepted: yes,
            message: `Das Wort ist zu Ende, der ${a.nfa ? 'NEA' : 'DEA'} ist ${where}. ${yes ? `${a.nfa ? 'Einer davon' : 'Das'} ist ein akzeptierender Zustand: angenommen.` : 'Kein akzeptierender Zustand: abgelehnt.'}` });
        return steps;
    }

    // a symbol in a message
    const quote = (s) => `»${s}«`;
    // what lies on the stack, bottom to top
    const pile = (stack) => (stack.length ? stack.join('') : 'nichts');

    // Every step of a pushdown automaton: the symbol read, the top it saw,
    // what it put in the top's place, the state and the whole stack
    function pushdownTrace(a, word) {
        let state = a.start;
        const stack = [a.bottom];
        const steps = [{ at: -1, states: [state], stack: [...stack], used: [],
            message: `Am Anfang ist der Kellerautomat in ${state}, auf dem Stack liegt nur ${a.bottom}. Er liest ${word ? quote(word) : 'nichts'}.` }];
        for (const [i, symbol] of [...word].entries()) {
            const top = stack[stack.length - 1];
            const rule = a.rules.find((r) => r.from === state && r.read === symbol && r.top === top);
            if (!rule) {
                steps.push({ at: i, symbol, top, states: [state], stack: [...stack], used: [], end: true, accepted: false,
                    message: `Er liest ${quote(symbol)}, ${top === undefined ? 'der Stack ist leer' : `oben liegt ${quote(top)}`}. Dafür hat ${state} keinen Übergang: Das Wort ist abgelehnt.` });
                return steps;
            }
            stack.pop();
            stack.push(...rule.push);
            let what;
            if (rule.push === '') what = `Er nimmt ${quote(top)} herunter.`;
            else if (rule.push === top) what = `${quote(top)} bleibt liegen.`;
            else if (rule.push.startsWith(top)) what = `${quote(rule.push.slice(top.length))} kommt oben drauf.`;
            else what = `An die Stelle von ${quote(top)} legt er ${quote(rule.push)}.`;
            const go = rule.to === state ? '' : ` Er wechselt nach ${rule.to}.`;
            steps.push({ at: i, symbol, top, push: rule.push, rule, states: [rule.to], before: [state], stack: [...stack], used: [[state, rule.to]],
                message: `Er liest ${quote(symbol)}, oben liegt ${quote(top)}. ${what}${go}` });
            state = rule.to;
        }
        const only = stack.length === 1 && stack[0] === a.bottom;
        const yes = a.accept.has(state) && only;
        const why = yes ? `Er ist in ${state}, einem akzeptierenden Zustand, und auf dem Stack liegt nur noch ${a.bottom}: angenommen.`
            : !a.accept.has(state) ? `${state} ist kein akzeptierender Zustand: abgelehnt.`
                : `Auf dem Stack liegt noch ${pile(stack)}, nicht nur ${a.bottom}: abgelehnt.`;
        steps.push({ at: word.length, states: [state], stack: [...stack], used: [], end: true, accepted: yes, message: `Das Wort ist zu Ende. ${why}` });
        return steps;
    }

    // the tape between its first and last written cell, without the empty ones at the edges
    const written = (tape) => {
        const used = [...tape.keys()].filter((i) => tape.get(i) !== BLANK);
        if (!used.length) return '';
        let out = '';
        for (let i = Math.min(...used); i <= Math.max(...used); i++) out += tape.get(i) ?? BLANK;
        return out;
    };

    // Every step of a Turing machine: the tape, the head, the state, and
    // what the step read, wrote and where the head went
    function turingTrace(a, input) {
        const tape = new Map([...input].map((c, i) => [i, c]));
        let head = 0;
        let state = a.start;
        const steps = [{ head, states: [state], tape: new Map(tape), used: [],
            message: `Am Anfang steht ${input ? quote(input) : 'nichts'} auf dem Band, der Kopf auf dem ${input ? 'ersten Zeichen' : 'leeren Feld'}. Die Maschine ist in ${state}.` }];
        const halted = (s) => a.accept.has(s);
        while (!halted(state)) {
            const read = tape.get(head) ?? BLANK;
            const rule = a.rules.find((r) => r.from === state && r.read === read);
            const seen = read === BLANK ? 'ein leeres Feld (_)' : quote(read);
            if (!rule) {
                steps.push({ head, states: [state], tape: new Map(tape), used: [], end: true, accepted: a.result === 'accept' ? false : undefined,
                    message: `In ${state} liest der Kopf ${seen}. Dafür gibt es keinen Übergang: Die Maschine hält${a.result === 'accept' ? ' und lehnt das Wort ab' : ''}.` });
                return steps;
            }
            if (steps.length > MAX_STEPS) {
                steps.push({ head, states: [state], tape: new Map(tape), used: [], end: true, accepted: false,
                    message: `Nach ${MAX_STEPS} Schritten hält die Maschine noch immer nicht.` });
                return steps;
            }
            const at = head;
            tape.set(head, rule.write);
            head += rule.move === 'R' ? 1 : -1;
            const did = rule.write === read ? (read === BLANK ? 'lässt es leer' : 'lässt es stehen') : `schreibt ${rule.write === BLANK ? 'ein leeres Feld (_)' : quote(rule.write)}`;
            const where = rule.move === 'R' ? 'rechts' : 'links';
            const next = rule.to === state ? `bleibt in ${state}` : `wechselt nach ${rule.to}`;
            let message = `In ${state} liest der Kopf ${seen}. Die Maschine ${did}, geht nach ${where} und ${next}.`;
            const step = { head, at, read, rule, states: [rule.to], before: [state], tape: new Map(tape), used: [[state, rule.to]] };
            if (halted(rule.to)) {
                step.end = true;
                if (a.result === 'accept') { step.accepted = true; message += ` In ${rule.to} hält sie und nimmt das Wort an.`; }
                else message += ` In ${rule.to} hält sie. Auf dem Band steht jetzt ${quote(written(tape))}.`;
            }
            step.message = message;
            steps.push(step);
            state = rule.to;
        }
        return steps;
    }

    function trace(a, input) {
        if (a.kind === 'pushdown') return pushdownTrace(a, input);
        if (a.kind === 'turing') return turingTrace(a, input);
        return finiteTrace(a, input);
    }

    const engine = { machine, arrows, trace, written };
    if (typeof module !== 'undefined' && module.exports) module.exports = engine;
    if (typeof document === 'undefined') return;

    const SVG = 'http://www.w3.org/2000/svg';
    const make = (tag, attrs = {}, children = []) => {
        const node = document.createElementNS(SVG, tag);
        for (const [k, v] of Object.entries(attrs)) if (k === 'text') node.textContent = v; else node.setAttribute(k, v);
        for (const child of children) node.appendChild(child);
        return node;
    };
    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) if (k === 'text') node.textContent = v; else node.setAttribute(k, v);
        for (const child of children) node.appendChild(child);
        return node;
    };
    const MONO = "'0xProto Nerd Font Mono', monospace";
    // a state's name: q with a small 0 and in italics, as names in
    // mathematics (q0, qH, qJ, as in the drawings); the symbols the
    // automaton reads stay upright, as on the tape. In a text: every name in it
    const NAME = /^([A-Za-z])(\d+|[FHJ])$/;
    const names = (text, value) => {
        let low = false;
        for (const part of String(value).split(/\b([A-Za-z](?:\d+|[FHJ]))\b/)) {
            const m = part.match(NAME);
            if (m) {
                text.appendChild(make('tspan', { 'font-style': 'italic', ...(low ? { dy: '-0.3em' } : {}), text: m[1] }));
                text.appendChild(make('tspan', { 'font-style': 'italic', dy: '0.3em', 'font-size': '0.7em', text: m[2] }));
                low = true;
            } else if (part) {
                text.appendChild(make('tspan', { ...(low ? { dy: '-0.3em' } : {}), text: part }));
                low = false;
            }
        }
        return text;
    };
    const stateLabel = (name, x, y, key) => names(make('text', { 'data-key': key, x, y: y + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-family': MONO, fill: 'var(--automaton-ink)' }), name);
    // the same in a message, as in the page's text: q<sub>0</sub>
    const withNames = (message, states) => {
        let out = message.replace(/&/g, '&amp;').replace(/</g, '&lt;');
        for (const s of states) {
            const m = s.match(NAME);
            if (m) out = out.replace(new RegExp(`\\b${s}\\b`, 'g'), `${m[1]}<sub>${m[2]}</sub>`);
        }
        return out;
    };

    // The automaton drawn by automaton_layout.js, as the drawings in the
    // page: columns by the distance from the start, arrows bent to their
    // left, labels clear of everything, also of the rings
    // the variables on top, the tape under them, the states under that
    const VARS = 6;
    const TAPE = 38;
    const TOP = 82;
    const CELL = 26;
    const R = root.AutomatonLayout.R;
    // the ring around a state the automaton is in (stroke 3)
    const RING = R + 6;
    // a pushdown automaton's stack: a column of cells right of the drawing
    const STACK_ROOM = 80;
    // cells: how many tape cells the picture must hold; stack: the highest stack
    function geometry(a, cells, stack) {
        const L = root.AutomatonLayout.layout({ states: a.states.map((id) => ({ id, label: id })), start: a.start, accept: [...a.accept], edges: arrows(a), columns: a.columns }, { ring: root.AutomatonLayout.RING_ROOM });
        const [vx, vy, vw, vh] = L.view;
        const room = a.kind === 'pushdown' ? STACK_ROOM : 0;
        // wide enough for the longest word on the tape, so that the picture
        // keeps its size while it is edited
        // and for the variables above the tape (a pushdown automaton's and a
        // Turing machine's four)
        const width = Math.max(vw + room, CELL * cells + 16, a.rules ? 340 : 0);
        const dx = (width - room - vw) / 2 - vx;
        const dy = TOP - vy;
        const move = (d) => { let k = 0; return d.replace(/-?\d+(?:\.\d+)?(?:e-?\d+)?/g, (n) => String(Math.round((Number(n) + (k++ % 2 === 0 ? dx : dy)) * 10) / 10)); };
        const pos = new Map([...L.pos].map(([id, [x, y]]) => [id, [x + dx, y + dy]]));
        const stackHeight = a.kind === 'pushdown' ? CELL * stack + 24 : 0;
        const height = TOP + Math.max(vh, stackHeight);
        return {
            pos: (s) => pos.get(s),
            edges: L.paths.map((p) => ({ from: p.e.from, to: p.e.to, label: p.e.label, d: move(p.d), path: (inset) => move(p.end(inset)),
                lx: p.box.x + p.box.w / 2 + dx, ly: p.box.y + 12 + dy, left: p.box.x + 2 + dx, key: `edge-${p.e.from}-${p.e.to}`,
                // a label along its arrow (AutomatonLayout): the line it stands on
                along: p.label && p.label.type === 'curve' ? { d: move(p.label.d), at: p.label.at } : null })),
            width,
            height,
            // the stack's column: its middle and the bottom of its lowest cell
            stackX: width - STACK_ROOM / 2,
            stackBottom: height - 22,
        };
    }

    // the tape's cells: [index, symbol, faint, written]; and the framed cell
    function tapeOf(a, word, step, range) {
        if (a.kind === 'turing') {
            const cells = [];
            for (let i = range[0]; i <= range[1]; i++) {
                const symbol = step.tape.get(i) ?? BLANK;
                cells.push([i, symbol, symbol === BLANK, i === step.at]);
            }
            return { cells, head: step.head, first: range[0] };
        }
        const cells = [...word].map((symbol, i) => [i, symbol, i < step.at || (step.end && i < word.length), false]);
        return { cells, head: step.at >= 0 && step.at < word.length ? step.at : null, first: 0 };
    }

    // the variables of the code, each a box with its value beside its name
    function variables(a, step) {
        const state = a.nfa ? ['states', step.states.length ? step.states.join(', ') : '–'] : ['state', step.states[0] || '–'];
        if (a.kind === 'turing') {
            return step.rule ? [state, ['read', step.read], ['write', step.rule.write], ['move', step.rule.move]] : [state];
        }
        if (a.kind === 'pushdown') {
            return step.symbol !== undefined ? [state, ['symbol', step.symbol], ['top', step.top ?? '–'], ...(step.rule ? [['push', step.push || EPS]] : [])] : [state];
        }
        return [...(step.symbol !== undefined ? [['symbol', step.symbol]] : []), state];
    }

    function draw(svg, a, geo, word, step, marker, range) {
        svg.replaceChildren();
        svg.appendChild(make('defs', {}, [['', 'var(--automaton-line)'], ['-accent', 'var(--automaton-accent)']].map(([suffix, fill]) =>
            make('marker', { id: `${marker}${suffix}`, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 9, markerHeight: 9, markerUnits: 'userSpaceOnUse', orient: 'auto-start-reverse' },
                [make('path', { d: 'M0 0L10 5L0 10z', fill })]))));
        // the tape: the word, the symbol being read framed (a Turing machine:
        // its cells, the one just written purple)
        const tape = tapeOf(a, word, step, range);
        const room = a.kind === 'pushdown' ? STACK_ROOM : 0;
        const tapeLeft = Math.max(8, (geo.width - room - tape.cells.length * CELL) / 2);
        const cellX = (i) => tapeLeft + (i - tape.first) * CELL;
        for (const [i, symbol, faint, fresh] of tape.cells) {
            const x = cellX(i);
            svg.appendChild(make('rect', { 'data-key': `cell-${i}`, x, y: TAPE, width: CELL - 2, height: 26, rx: 4, fill: 'var(--automaton-paper)', stroke: 'var(--automaton-line)', 'stroke-width': 1 }));
            svg.appendChild(make('text', { 'data-key': `symbol-${i}`, x: x + (CELL - 2) / 2, y: TAPE + 18, 'text-anchor': 'middle', 'font-size': 14, 'font-family': MONO,
                fill: fresh ? 'var(--automaton-accent)' : faint ? 'var(--automaton-faint)' : 'var(--automaton-ink)', 'font-weight': fresh ? 700 : 400, text: symbol }));
        }
        if (tape.head !== null && tape.head !== undefined) svg.appendChild(make('rect', { 'data-key': 'head', x: cellX(tape.head) - 2, y: TAPE - 2, width: CELL + 2, height: 30, rx: 6, fill: 'none', stroke: 'var(--automaton-accent)', 'stroke-width': 2.5 }));
        let vx = 8;
        for (const [name, value] of variables(a, step)) {
            const w = 22 + 7.5 * (name.length + String(value).length);
            const split = vx + 10 + 7.5 * name.length;
            svg.appendChild(make('rect', { 'data-key': `var-${name}`, x: vx, y: VARS, width: w, height: 20, rx: 4, fill: 'var(--automaton-paper)', stroke: 'var(--automaton-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(make('line', { 'data-key': `var-${name}-split`, x1: split, y1: VARS, x2: split, y2: VARS + 20, stroke: 'var(--automaton-accent)', 'stroke-width': 1 }));
            svg.appendChild(make('text', { 'data-key': `var-${name}-label`, x: vx + 5 + 3.75 * name.length, y: VARS + 14, 'text-anchor': 'middle', 'font-size': 12, 'font-family': MONO, fill: 'var(--automaton-accent)', text: name }));
            const text = make('text', { 'data-key': `var-${name}-value`, x: split + (vx + w - split) / 2, y: VARS + 14, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, 'font-family': MONO, fill: 'var(--automaton-ink)' });
            svg.appendChild(/^states?$/.test(name) ? names(text, value) : (text.textContent = value, text));
            vx += w + 8;
        }
        // a pushdown automaton's stack, bottom up, the top framed purple
        if (a.kind === 'pushdown') {
            const x = geo.stackX - (CELL - 2) / 2;
            step.stack.forEach((symbol, k) => {
                const y = geo.stackBottom - (k + 1) * CELL;
                const top = k === step.stack.length - 1;
                svg.appendChild(make('rect', { 'data-key': `stack-${k}`, x, y, width: CELL - 2, height: CELL - 2, rx: 4, fill: 'var(--automaton-paper)', stroke: top ? 'var(--automaton-accent)' : 'var(--automaton-line)', 'stroke-width': top ? 2.5 : 1 }));
                svg.appendChild(make('text', { 'data-key': `stack-${k}-symbol`, x: geo.stackX, y: y + 17, 'text-anchor': 'middle', 'font-size': 14, 'font-family': MONO, fill: 'var(--automaton-ink)', 'font-weight': top ? 700 : 400, text: symbol }));
            });
            svg.appendChild(make('text', { 'data-key': 'stack-name', x: geo.stackX, y: geo.stackBottom + 14, 'text-anchor': 'middle', 'font-size': 12, 'font-family': MONO, fill: 'var(--automaton-faint)', text: 'Stack' }));
        }
        // the arrows, the ones just taken purple
        const eps = step.eps || [];
        const used = new Set([...step.used, ...eps].map(([f, t]) => `edge-${f}-${t}`));
        for (const e of geo.edges) {
            const on = used.has(e.key);
            // into a state with a ring: the tip outside the ring
            const ringed = step.states.includes(e.to);
            svg.appendChild(make('path', { 'data-key': e.key, 'data-edge': e.key, 'data-head-on': `url(#${marker}-accent)`, d: ringed ? e.path(RING + 1.5 + 2) : e.d, fill: 'none', stroke: on ? 'var(--automaton-accent)' : 'var(--automaton-line)', 'stroke-width': on ? 2.5 : 1.6, 'marker-end': `url(#${marker}${on ? '-accent' : ''})` }));
        }
        // the arrows read pulse at once, the ε arrows after them
        step.used.forEach(([f, t], i) => svg.appendChild(make('path', { 'data-key': `pulse-${i}`, 'data-pulse': `edge-${f}-${t}`, 'data-delay': 0, 'data-length': eps.length ? 0.55 : 0.85, 'data-swell': 2.5, d: 'M0 0', opacity: 0 })));
        eps.forEach(([f, t], i) => svg.appendChild(make('path', { 'data-key': `pulse-eps-${i}`, 'data-pulse': `edge-${f}-${t}`, 'data-delay': step.used.length ? 0.5 : 0, 'data-length': 0.5, 'data-swell': 2.5, d: 'M0 0', opacity: 0 })));
        // a wide invisible line on every arrow to point at (AutomatonLayout.hover)
        for (const e of geo.edges) svg.appendChild(make('path', { 'data-key': `pick-${e.key}`, 'data-edge': e.key, class: 'automaton-pick', d: e.d }));
        // the labels: a label of several lines (a pushdown automaton's, a
        // Turing machine's) flush left, a line per rule; only the line of the
        // rule just used purple
        const usedLine = step.rule ? step.rule.label : null;
        for (const e of geo.edges) {
            const on = used.has(e.key);
            const look = (lit) => ({ fill: lit ? 'var(--automaton-accent)' : 'var(--automaton-ink)', 'font-weight': lit ? 700 : 400 });
            const base = { 'data-key': `label-${e.key}`, 'data-edge': e.key, 'font-size': 13, 'font-family': MONO };
            const lines = String(e.label).split('\n');
            if (lines.length > 1) {
                svg.appendChild(make('text', { ...base, x: e.left, y: e.ly, 'text-anchor': 'start', fill: 'var(--automaton-ink)' },
                    lines.map((line, i) => make('tspan', { x: e.left, ...(i ? { dy: 15 } : {}), ...look(on && (usedLine === null || line === usedLine)), text: line }))));
                continue;
            }
            if (!e.along) { svg.appendChild(make('text', { ...base, ...look(on), x: e.lx, y: e.ly, 'text-anchor': 'middle', text: e.label })); continue; }
            const id = `${marker}-along-${e.key}`;
            svg.appendChild(make('path', { 'data-key': `along-${e.key}`, id, d: e.along.d, fill: 'none', stroke: 'none' }));
            svg.appendChild(make('text', { ...base, ...look(on), 'text-anchor': 'middle' }, [make('textPath', { href: `#${id}`, startOffset: e.along.at, 'text-anchor': 'middle', text: e.label })]));
        }
        // the start: an arrow from the left
        const [sx, sy] = geo.pos(a.start);
        svg.appendChild(make('path', { 'data-key': 'start', d: `M${sx - R - 34} ${sy} L${sx - (step.states.includes(a.start) ? RING + 3.5 : R + 2)} ${sy}`, fill: 'none', stroke: 'var(--automaton-line)', 'stroke-width': 1.6, 'marker-end': `url(#${marker})` }));
        // the states; at the end a state that accepts the word green
        for (const s of a.states) {
            const [x, y] = geo.pos(s);
            const won = step.end && step.accepted && step.states.includes(s) && a.accept.has(s);
            svg.appendChild(make('circle', { 'data-key': `state-${s}`, cx: x, cy: y, r: R, fill: won ? 'var(--automaton-yes)' : 'var(--automaton-paper)', stroke: 'var(--automaton-ink)', 'stroke-width': 1.5 }));
            if (a.accept.has(s)) svg.appendChild(make('circle', { 'data-key': `accept-${s}`, cx: x, cy: y, r: R - 4, fill: 'none', stroke: 'var(--automaton-ink)', 'stroke-width': 1.2 }));
            svg.appendChild(stateLabel(s, x, y, `name-${s}`));
        }
        // the rings: where the automaton is (a NEA: everywhere it can be);
        // a new one comes out of the state it is reached from; a ring travels
        // along the arrow it takes, with its pulse (data-along): a DEA's from
        // state to state, a NEA's new one from where it comes, one that stays
        // around its loop
        for (const s of step.states) {
            const key = a.nfa ? `ring-${s}` : 'ring';
            const before = step.before || [];
            // the way in: an arrow read, or an ε from a state that had a
            // ring already (a ring that is new itself grows in place)
            const way = a.nfa
                ? (before.includes(s) ? step.used.find(([f, t]) => f === s && t === s)
                    : step.used.find(([, t]) => t === s) || eps.find(([, t]) => t === s))
                : step.used[0];
            // reached by ε from a state that has only just got its ring: out
            // of that ring, once it is there (data-along-from)
            const fresh = a.nfa && way && !before.includes(s) && !before.includes(way[0]);
            svg.appendChild(make('circle', { 'data-key': key, 'data-grow': '',
                ...(a.nfa && way && !before.includes(s) && before.includes(way[0]) ? { 'data-from': `ring-${way[0]}` } : {}),
                ...(fresh ? { 'data-along-from': `state-${way[0]}` } : {}),
                ...(way ? { 'data-along': `edge-${way[0]}-${way[1]}` } : {}),
                cx: geo.pos(s)[0], cy: geo.pos(s)[1], r: RING, fill: 'none', stroke: 'var(--automaton-accent)', 'stroke-width': 3 }));
        }
        svg.setAttribute('viewBox', `0 0 ${geo.width} ${geo.height}`);
    }

    // what the widget calls the automaton, in its label and the input's
    const called = (a) => ({ dfa: 'DEA', nfa: 'NEA', pushdown: 'Kellerautomat', turing: 'Turingmaschine' })[a.kind];

    // A Turing machine's cells from first to last written or visited, one
    // empty cell more on either side; the highest stack of a pushdown
    // automaton: so that the picture keeps its size through the steps
    function extent(a, steps, word) {
        if (a.kind === 'turing') {
            const all = steps.flatMap((s) => [...s.tape.keys(), ...(s.head === undefined ? [] : [s.head])]);
            const lo = Math.min(0, ...all) - 1;
            const hi = Math.max(word.length - 1, ...all) + 1;
            return { range: [lo, hi], cells: hi - lo + 1, stack: 0 };
        }
        if (a.kind === 'pushdown') return { range: null, cells: word.length, stack: Math.max(1, ...steps.map((s) => s.stack.length)) };
        return { range: null, cells: word.length, stack: 0 };
    }

    let count = 0;
    function mount(el) {
        const a = machine(el.dataset);
        const fixed = 'fixed' in el.dataset;
        // an editable widget keeps its size for the longest input (16)
        const roomFor = (word, steps) => {
            const e = extent(a, steps, word);
            if (fixed) return e;
            return { ...e, cells: Math.max(e.cells, a.kind === 'turing' ? 20 : 16), stack: Math.max(e.stack, a.kind === 'pushdown' ? 9 : 0) };
        };
        const marker = `automaton-arrow-${++count}`;
        const input = html('input', { class: 'form-control', type: 'text', value: el.dataset.input || '', spellcheck: 'false', maxlength: 16, 'aria-label': a.kind === 'turing' ? 'Was am Anfang auf dem Band der Turingmaschine steht' : `Das Wort, das der ${called(a)} liest` });
        const button = (go, icon, label) => html('button', { class: 'btn btn-sm btn-success', 'data-go': go, 'aria-label': label, type: 'button' }, [html('i', { class: `bi ${icon}`, style: 'margin-right: 0;' })]);
        const buttons = [button('first', 'bi-chevron-bar-left', 'Zum Anfang'), button('prev', 'bi-chevron-left', 'Einen Schritt zurück'),
            button('next', 'bi-chevron-right', 'Nächster Schritt'), button('last', 'bi-chevron-bar-right', 'Zum Ende')];
        const counter = html('span', { class: 'automaton-count' });
        const say = html('div', { class: 'automaton-say', 'aria-live': 'polite' });
        const stops = a.kind === 'turing' && a.result !== 'accept' ? `${[...a.accept].join(', ')} ${a.accept.size === 1 ? 'hält' : 'halten'}` : `${[...a.accept].join(', ')} ${a.accept.size === 1 ? 'nimmt' : 'nehmen'} an`;
        const picture = make('svg', { class: 'automaton-picture', role: 'img', 'aria-label': `${a.kind === 'turing' ? 'Die' : 'Der'} ${called(a)} mit den Zuständen ${a.states.join(', ')}; ${a.start} ist der Start, ${stops}` });
        root.AutomatonLayout.hover(picture);
        el.replaceChildren(...(el.dataset.title ? [html('h4', { class: 'automaton-title', text: el.dataset.title })] : []),
            ...(fixed ? [] : [input]), html('div', { class: 'automaton-controls' }, [...buttons, counter]), say, picture);
        let steps = [];
        let current = 0;
        let word = '';
        let geo = null;
        let room = null;
        // a step back is the step forward played backwards
        let before = 0;
        const show = (animate) => {
            const step = steps[current];
            const back = animate && current === before - 1;
            before = current;
            const next = make('svg', {});
            draw(next, a, geo, word, step, marker, room.range);
            if (root.StepperMotion) root.StepperMotion.morph(picture, next, { duration: animate ? 700 : 0, reverse: back });
            else { picture.setAttribute('viewBox', next.getAttribute('viewBox')); picture.replaceChildren(...next.childNodes); }
            say.innerHTML = withNames(step.message, a.states);
            say.classList.toggle('yes', !!step.end && step.accepted === true);
            say.classList.toggle('no', !!step.end && step.accepted === false);
            counter.textContent = `Schritt ${current} von ${steps.length - 1}`;
            buttons[0].disabled = buttons[1].disabled = current <= 0;
            buttons[2].disabled = buttons[3].disabled = current >= steps.length - 1;
            // for the timeline, play and the lightbox (stepper_keys.js)
            el.dataset.step = current;
            el.dataset.steps = steps.length;
        };
        const go = (k, animate) => { current = Math.max(0, Math.min(steps.length - 1, k)); show(animate); };
        el.stepperGo = (k) => go(k, false);
        buttons[0].addEventListener('click', () => go(0, false));
        buttons[1].addEventListener('click', () => go(current - 1, true));
        buttons[2].addEventListener('click', () => go(current + 1, true));
        buttons[3].addEventListener('click', () => go(steps.length - 1, false));
        const refresh = () => {
            // without data-alphabet every symbol is allowed
            const wrong = a.alphabet ? [...input.value].find((c) => !a.alphabet.includes(c)) : undefined;
            if (wrong !== undefined) {
                say.textContent = `»${wrong}« gibt es im Alphabet dieses Automaten nicht. Erlaubt ${a.alphabet.length === 1 ? 'ist' : 'sind'}: ${shorten(a.alphabet)}.`;
                for (const b of buttons) b.disabled = true;
                return;
            }
            word = input.value;
            steps = trace(a, word);
            const want = roomFor(word, steps);
            // the drawing anew only where the tape or the stack needs more room
            if (!geo || !room || want.cells !== room.cells || want.stack !== room.stack) geo = geometry(a, want.cells, want.stack);
            room = want;
            // at most as large as a drawing of the same automaton (1.2)
            picture.style.maxWidth = `${geo.width * 1.2}px`;
            current = 0;
            show(false);
        };
        input.addEventListener('input', refresh);
        refresh();
    }

    // mount(el): a widget built after the page has loaded (Automatron)
    root.AutomatonStepper = { ...engine, mount: (el) => { mount(el); document.dispatchEvent(new CustomEvent('stepper-added', { detail: el })); } };
    const start = () => document.querySelectorAll('.automaton-stepper').forEach(mount);
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})(typeof window !== 'undefined' ? window : globalThis);
