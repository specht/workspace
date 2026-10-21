// The automaton stepper (automata): a DEA or a NEA reads a word, one
// symbol per step. Above the automaton the word as a tape, the symbol being
// read under the head; the state the automaton is in wears the purple ring,
// and the arrow it takes pulses. A NEA can be in several states at once:
// every one of them wears a ring, a new ring comes out of the state it is
// reached from, and the ring of a way that ends (no arrow for the symbol:
// the error state, which is not drawn) goes.
//
//   <div class='automaton-stepper' data-stepper data-title='…'
//        data-states='q0 q1 q2' data-start='q0' data-accept='q0'
//        data-transitions='q0 0 q0, q0 1 q1, q1 0 q2, …' data-input='10010'
//        data-alphabet='01'></div>
//
// A transition 'q0 0-9 q0' stands for ten. The word can be edited; steps,
// play, the timeline, the keys and the lightbox come from stepper_keys.js,
// the movement from stepper_motion.js.
(function () {
    'use strict';
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
    // q0 as q with a small 0
    const stateLabel = (name, x, y, key) => {
        const m = name.match(/^([A-Za-z]+)(\w*)$/);
        const text = make('text', { 'data-key': key, x, y: y + 5, 'text-anchor': 'middle', 'font-size': 15, fill: 'var(--automaton-ink)' });
        if (m && m[2]) {
            text.appendChild(document.createTextNode(m[1]));
            text.appendChild(make('tspan', { dy: '0.3em', 'font-size': '0.7em', text: m[2] }));
        } else text.textContent = name;
        return text;
    };
    const sets = (list) => list.join(', ');

    function automaton(el) {
        const states = el.dataset.states.trim().split(/\s+/);
        const accept = new Set((el.dataset.accept || '').trim().split(/\s+/).filter(Boolean));
        const delta = new Map();
        for (const part of (el.dataset.transitions || '').split(',').map((s) => s.trim()).filter(Boolean)) {
            const [from, symbols, to] = part.split(/\s+/);
            for (const s of expand(symbols)) {
                const key = `${from} ${s}`;
                if (!delta.has(key)) delta.set(key, []);
                if (!delta.get(key).includes(to)) delta.get(key).push(to);
            }
        }
        const nfa = el.dataset.kind === 'nfa' || [...delta.values()].some((to) => to.length > 1);
        return { states, start: el.dataset.start || states[0], accept, delta, nfa, alphabet: expand(el.dataset.alphabet || '') };
    }

    // Every step: where the head is, the states the automaton can be in,
    // the arrows it took to get there (from, to) and the message
    function trace(a, word) {
        const steps = [{ at: -1, states: [a.start], used: [], message: a.nfa
            ? `Am Anfang ist der NEA nur in ${a.start}. Er liest ${word ? `»${word}«` : 'nichts'}.`
            : `Am Anfang ist der DEA in ${a.start}. Er liest ${word ? `»${word}«` : 'nichts'}.` }];
        let current = [a.start];
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
            let message;
            if (!a.nfa) message = next.length ? `${symbol}: von ${current[0]} nach ${next[0]}.` : `${symbol}: Aus ${current[0]} führt kein Pfeil mit ${symbol}. Der DEA steckt fest.`;
            else {
                const ways = current.map((s) => {
                    const to = a.delta.get(`${s} ${symbol}`) || [];
                    return to.length ? `${s} → ${sets(to)}` : `${s} endet`;
                });
                message = `${symbol}: ${ways.join('; ')}. ${next.length ? `Jetzt in ${sets(next)}.` : 'Kein Weg geht weiter.'}`;
            }
            steps.push({ at: i, symbol, states: next, used, ended, message });
            current = next;
        });
        // the end of the word: accepted if a state it can be in accepts
        const yes = current.some((s) => a.accept.has(s));
        const where = a.nfa ? `in ${current.length ? sets(current) : 'keinem Zustand mehr'}` : `in ${current[0] || 'keinem Zustand'}`;
        steps.push({ at: word.length, states: current, used: [], end: true, accepted: yes,
            message: `Das Wort ist zu Ende, der ${a.nfa ? 'NEA' : 'DEA'} ist ${where}. ${yes ? `${a.nfa ? 'Einer davon' : 'Das'} ist ein akzeptierender Zustand: angenommen.` : 'Kein akzeptierender Zustand: abgelehnt.'}` });
        return steps;
    }

    // The automaton in a row: an arrow to the next state straight (a
    // little bent when one comes back), arrows further on above, arrows
    // back below, a loop above its state
    const GAP = 120;
    const LEFT = 70;
    const TOP = 112;
    const R = 24;
    function geometry(a) {
        const x = (s) => LEFT + a.states.indexOf(s) * GAP;
        const pairs = new Map();
        for (const [key, to] of a.delta) {
            const [from, symbol] = key.split(' ');
            for (const t of to) {
                const k = `${from} ${t}`;
                pairs.set(k, (pairs.get(k) || '') + symbol);
            }
        }
        const edges = [...pairs].map(([k, symbols]) => {
            const [from, to] = k.split(' ');
            const [x1, x2] = [x(from), x(to)];
            const label = shorten(symbols);
            let d;
            let lx;
            let ly;
            if (from === to) {
                d = `M${x1 - 10} ${TOP - R + 2} C${x1 - 34} ${TOP - 78} ${x1 + 34} ${TOP - 78} ${x1 + 10} ${TOP - R + 2}`;
                [lx, ly] = [x1, TOP - 68];
            } else {
                const forward = x2 > x1;
                const back = pairs.has(`${to} ${from}`);
                const span = Math.abs(x2 - x1) / GAP;
                // above going forward, below going back; a neighbour with no
                // arrow back straight
                const bend = span === 1 && forward && !back ? 0 : (forward ? -1 : 1) * (span === 1 ? 22 : 30 + 14 * span);
                const mx = (x1 + x2) / 2;
                const my = TOP + bend;
                if (bend === 0) {
                    d = `M${x1 + R} ${TOP} L${x2 - R - 2} ${TOP}`;
                    [lx, ly] = [mx, TOP - 8];
                } else {
                    // a curve whose middle is at my: its control point twice
                    // as far from the line; it leaves and enters the circles
                    // on their borders, aimed at the control point, so that
                    // the head follows the curve
                    const cy = 2 * my - TOP;
                    const towards = (xa, r) => { const l = Math.hypot(mx - xa, cy - TOP); return [xa + ((mx - xa) / l) * r, TOP + ((cy - TOP) / l) * r]; };
                    const [sx, sy] = towards(x1, R);
                    const [tx, ty] = towards(x2, R + 2);
                    d = `M${sx} ${sy} Q${mx} ${cy} ${tx} ${ty}`;
                    [lx, ly] = [mx, bend < 0 ? my - 6 : my + 16];
                }
            }
            return { from, to, d, label, lx, ly, key: `edge-${from}-${to}` };
        });
        const width = LEFT * 2 + (a.states.length - 1) * GAP;
        // room under the row for the arrows back
        const below = edges.some((e) => a.states.indexOf(e.to) < a.states.indexOf(e.from)) ? 70 : 34;
        return { x, edges, width, height: TOP + R + below };
    }

    function draw(svg, a, geo, word, step, marker) {
        svg.replaceChildren();
        svg.appendChild(make('defs', {}, [['', 'var(--automaton-line)'], ['-accent', 'var(--automaton-accent)']].map(([suffix, fill]) =>
            make('marker', { id: `${marker}${suffix}`, viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 9, markerHeight: 9, markerUnits: 'userSpaceOnUse', orient: 'auto-start-reverse' },
                [make('path', { d: 'M0 0L10 5L0 10z', fill })]))));
        // the tape: the word, the symbol being read under the head
        const CELL = 26;
        const tapeLeft = Math.max(8, (geo.width - word.length * CELL) / 2);
        [...word].forEach((symbol, i) => {
            const x = tapeLeft + i * CELL;
            const done = i < step.at || (step.end && i < word.length);
            svg.appendChild(make('rect', { 'data-key': `cell-${i}`, x, y: 6, width: CELL - 2, height: 26, rx: 4, fill: 'var(--automaton-paper)', stroke: 'var(--automaton-line)', 'stroke-width': 1 }));
            svg.appendChild(make('text', { 'data-key': `symbol-${i}`, x: x + (CELL - 2) / 2, y: 24, 'text-anchor': 'middle', 'font-size': 14, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: done ? 'var(--automaton-faint)' : 'var(--automaton-ink)', text: symbol }));
        });
        if (step.at >= 0 && step.at < word.length) svg.appendChild(make('rect', { 'data-key': 'head', x: tapeLeft + step.at * CELL - 2, y: 4, width: CELL + 2, height: 30, rx: 6, fill: 'none', stroke: 'var(--automaton-accent)', 'stroke-width': 2.5 }));
        // the arrows, the ones just taken purple
        const used = new Set(step.used.map(([f, t]) => `edge-${f}-${t}`));
        for (const e of geo.edges) {
            const on = used.has(e.key);
            svg.appendChild(make('path', { 'data-key': e.key, d: e.d, fill: 'none', stroke: on ? 'var(--automaton-accent)' : 'var(--automaton-line)', 'stroke-width': on ? 2.5 : 1.6, 'marker-end': `url(#${marker}${on ? '-accent' : ''})` }));
        }
        step.used.forEach(([f, t], i) => svg.appendChild(make('path', { 'data-key': `pulse-${i}`, 'data-pulse': `edge-${f}-${t}`, 'data-delay': 0, 'data-length': 0.85, 'data-swell': 2.5, d: 'M0 0', opacity: 0 })));
        for (const e of geo.edges) svg.appendChild(make('text', { 'data-key': `label-${e.key}`, x: e.lx, y: e.ly, 'text-anchor': 'middle', 'font-size': 13, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: used.has(e.key) ? 'var(--automaton-accent)' : 'var(--automaton-ink)', 'font-weight': used.has(e.key) ? 700 : 400, text: e.label }));
        // the start: an arrow from the left
        const sx = geo.x(a.start);
        svg.appendChild(make('path', { 'data-key': 'start', d: `M${sx - R - 34} ${TOP} L${sx - R - 2} ${TOP}`, fill: 'none', stroke: 'var(--automaton-line)', 'stroke-width': 1.6, 'marker-end': `url(#${marker})` }));
        // the states; at the end a state that accepts the word green
        for (const s of a.states) {
            const x = geo.x(s);
            const won = step.end && step.accepted && step.states.includes(s) && a.accept.has(s);
            svg.appendChild(make('circle', { 'data-key': `state-${s}`, cx: x, cy: TOP, r: R, fill: won ? 'var(--automaton-yes)' : 'var(--automaton-paper)', stroke: 'var(--automaton-ink)', 'stroke-width': 1.5 }));
            if (a.accept.has(s)) svg.appendChild(make('circle', { 'data-key': `accept-${s}`, cx: x, cy: TOP, r: R - 4, fill: 'none', stroke: 'var(--automaton-ink)', 'stroke-width': 1.2 }));
            svg.appendChild(stateLabel(s, x, TOP, `name-${s}`));
        }
        // the rings: where the automaton is (a NEA: everywhere it can be);
        // a new one comes out of the state it is reached from
        for (const s of step.states) {
            const from = step.used.find(([, t]) => t === s);
            const key = a.nfa ? `ring-${s}` : 'ring';
            svg.appendChild(make('circle', { 'data-key': key, 'data-grow': '', ...(a.nfa && from ? { 'data-from': `ring-${from[0]}` } : {}), cx: geo.x(s), cy: TOP, r: R + 6, fill: 'none', stroke: 'var(--automaton-accent)', 'stroke-width': 3 }));
        }
        svg.setAttribute('viewBox', `0 0 ${geo.width} ${geo.height}`);
    }

    let count = 0;
    function mount(el) {
        const a = automaton(el);
        const geo = geometry(a);
        const marker = `automaton-arrow-${++count}`;
        const input = html('input', { class: 'form-control', type: 'text', value: el.dataset.input || '', spellcheck: 'false', maxlength: 16, 'aria-label': `Das Wort, das der ${a.nfa ? 'NEA' : 'DEA'} liest` });
        const button = (go, icon, label) => html('button', { class: 'btn btn-sm btn-success', 'data-go': go, 'aria-label': label, type: 'button' }, [html('i', { class: `bi ${icon}`, style: 'margin-right: 0;' })]);
        const buttons = [button('first', 'bi-chevron-bar-left', 'Zum Anfang'), button('prev', 'bi-chevron-left', 'Einen Schritt zurück'),
            button('next', 'bi-chevron-right', 'Nächster Schritt'), button('last', 'bi-chevron-bar-right', 'Zum Ende')];
        const counter = html('span', { class: 'automaton-count' });
        const say = html('div', { class: 'automaton-say', 'aria-live': 'polite' });
        const picture = make('svg', { class: 'automaton-picture', role: 'img', 'aria-label': `Der ${a.nfa ? 'NEA' : 'DEA'} mit den Zuständen ${a.states.join(', ')}; ${a.start} ist der Start, ${[...a.accept].join(', ')} ${a.accept.size === 1 ? 'nimmt' : 'nehmen'} an` });
        picture.style.maxWidth = `${geo.width * 1.25}px`;
        el.replaceChildren(...(el.dataset.title ? [html('h4', { class: 'automaton-title', text: el.dataset.title })] : []),
            input, html('div', { class: 'automaton-controls' }, [...buttons, counter]), say, picture);
        let steps = [];
        let current = 0;
        let word = '';
        const show = (animate) => {
            const step = steps[current];
            const next = make('svg', {});
            draw(next, a, geo, word, step, marker);
            if (window.StepperMotion) window.StepperMotion.morph(picture, next, { duration: animate ? 700 : 0 });
            else { picture.setAttribute('viewBox', next.getAttribute('viewBox')); picture.replaceChildren(...next.childNodes); }
            say.textContent = step.message;
            say.classList.toggle('yes', !!step.end && step.accepted);
            say.classList.toggle('no', !!step.end && !step.accepted);
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
            const wrong = a.alphabet && [...input.value].find((c) => !a.alphabet.includes(c));
            if (wrong !== undefined && wrong !== null && wrong !== false) {
                say.textContent = `»${wrong}« gibt es im Alphabet dieses Automaten nicht. Erlaubt ${a.alphabet.length === 1 ? 'ist' : 'sind'}: ${shorten(a.alphabet)}.`;
                for (const b of buttons) b.disabled = true;
                return;
            }
            word = input.value;
            steps = trace(a, word);
            current = 0;
            show(false);
        };
        input.addEventListener('input', refresh);
        refresh();
    }

    const start = () => document.querySelectorAll('.automaton-stepper').forEach(mount);
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
