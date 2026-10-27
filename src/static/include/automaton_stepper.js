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
    // Names of states as q with a small 0 and in italics, as names in
    // mathematics; the symbols the automaton reads stay upright, as on the
    // tape. In a text: every name in it ('q0, q1')
    const names = (text, value) => {
        let low = false;
        for (const part of String(value).split(/([A-Za-z]+\d+)/)) {
            const m = part.match(/^([A-Za-z]+)(\d+)$/);
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
    const stateLabel = (name, x, y, key) => names(make('text', { 'data-key': key, x, y: y + 5, 'text-anchor': 'middle', 'font-size': 15, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--automaton-ink)' }), name);
    // the same in a message, as in the page's text: q<sub>0</sub>
    const withNames = (message, states) => {
        let out = message.replace(/&/g, '&amp;').replace(/</g, '&lt;');
        for (const s of states) {
            const m = s.match(/^([A-Za-z]+)(\d+)$/);
            if (m) out = out.replace(new RegExp(`\\b${s}\\b`, 'g'), `${m[1]}<sub>${m[2]}</sub>`);
        }
        return out;
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
        const nfa = el.dataset.kind === 'nfa' || [...delta.values()].some((to) => to.length > 1) || [...delta.keys()].some((k) => k.endsWith(` ${EPS}`));
        return { states, start: el.dataset.start || states[0], accept, delta, nfa, columns: el.dataset.columns, alphabet: expand(el.dataset.alphabet || '').replace(EPS, '') };
    }

    // ε: the states reached from these without reading a symbol, and the
    // arrows that bring each new one
    const EPS = 'ε';
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

    // Every step: where the head is, the states the automaton can be in,
    // the arrows it took to get there (from, to) and the message
    function trace(a, word) {
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

    // The automaton drawn by automaton_layout.js, as the drawings in the
    // page: columns by the distance from the start, arrows bent to their
    // left, labels clear of everything, also of the rings
    // the variables on top, the tape under them, the states under that
    const VARS = 6;
    const TAPE = 38;
    const TOP = 82;
    const R = window.AutomatonLayout.R;
    // the ring around a state the automaton is in (stroke 3)
    const RING = R + 6;
    function geometry(a) {
        const edges = [];
        for (const [key, to] of a.delta) {
            const [from, symbol] = key.split(' ');
            for (const t of to) edges.push({ from, to: t, label: symbol });
        }
        const L = window.AutomatonLayout.layout({ states: a.states.map((id) => ({ id, label: id })), start: a.start, accept: [...a.accept], edges, columns: a.columns }, { ring: window.AutomatonLayout.RING_ROOM });
        const [vx, vy, vw, vh] = L.view;
        // wide enough for the longest word on the tape (16, the input's
        // maxlength), so that the picture keeps its size while it is edited
        const width = Math.max(vw, 26 * 16 + 16);
        const dx = (width - vw) / 2 - vx;
        const dy = TOP - vy;
        const move = (d) => { let k = 0; return d.replace(/-?\d+(?:\.\d+)?(?:e-?\d+)?/g, (n) => String(Math.round((Number(n) + (k++ % 2 === 0 ? dx : dy)) * 10) / 10)); };
        const pos = new Map([...L.pos].map(([id, [x, y]]) => [id, [x + dx, y + dy]]));
        return {
            pos: (s) => pos.get(s),
            edges: L.paths.map((p) => ({ from: p.e.from, to: p.e.to, label: p.e.label, d: move(p.d), path: (inset) => move(p.end(inset)),
                lx: p.box.x + p.box.w / 2 + dx, ly: p.box.y + 12 + dy, key: `edge-${p.e.from}-${p.e.to}`,
                // a label along its arrow (AutomatonLayout): the line it stands on
                along: p.label && p.label.type === 'curve' ? { d: move(p.label.d), at: p.label.at } : null })),
            width,
            height: TOP + vh,
        };
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
            svg.appendChild(make('rect', { 'data-key': `cell-${i}`, x, y: TAPE, width: CELL - 2, height: 26, rx: 4, fill: 'var(--automaton-paper)', stroke: 'var(--automaton-line)', 'stroke-width': 1 }));
            svg.appendChild(make('text', { 'data-key': `symbol-${i}`, x: x + (CELL - 2) / 2, y: TAPE + 18, 'text-anchor': 'middle', 'font-size': 14, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: done ? 'var(--automaton-faint)' : 'var(--automaton-ink)', text: symbol }));
        });
        if (step.at >= 0 && step.at < word.length) svg.appendChild(make('rect', { 'data-key': 'head', x: tapeLeft + step.at * CELL - 2, y: TAPE - 2, width: CELL + 2, height: 30, rx: 6, fill: 'none', stroke: 'var(--automaton-accent)', 'stroke-width': 2.5 }));
        // the variables of the code: the symbol just read, the state (a
        // NEA: the set of states), each a box with its value beside its name
        const vars = [...(step.symbol !== undefined ? [['symbol', step.symbol]] : []), [a.nfa ? 'states' : 'state', a.nfa ? (step.states.length ? step.states.join(', ') : '–') : (step.states[0] || '–')]];
        let vx = 8;
        for (const [name, value] of vars) {
            const w = 22 + 7.5 * (name.length + String(value).length);
            const split = vx + 10 + 7.5 * name.length;
            svg.appendChild(make('rect', { 'data-key': `var-${name}`, x: vx, y: VARS, width: w, height: 20, rx: 4, fill: 'var(--automaton-paper)', stroke: 'var(--automaton-accent)', 'stroke-width': 1.5 }));
            svg.appendChild(make('line', { 'data-key': `var-${name}-split`, x1: split, y1: VARS, x2: split, y2: VARS + 20, stroke: 'var(--automaton-accent)', 'stroke-width': 1 }));
            svg.appendChild(make('text', { 'data-key': `var-${name}-label`, x: vx + 5 + 3.75 * name.length, y: VARS + 14, 'text-anchor': 'middle', 'font-size': 12, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--automaton-accent)', text: name }));
            const text = make('text', { 'data-key': `var-${name}-value`, x: split + (vx + w - split) / 2, y: VARS + 14, 'text-anchor': 'middle', 'font-size': 12, 'font-weight': 600, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: 'var(--automaton-ink)' });
            svg.appendChild(name === 'symbol' ? (text.textContent = value, text) : names(text, value));
            vx += w + 8;
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
        for (const e of geo.edges) {
            const look = { 'data-key': `label-${e.key}`, 'data-edge': e.key, 'text-anchor': 'middle', 'font-size': 13, 'font-family': "'0xProto Nerd Font Mono', monospace", fill: used.has(e.key) ? 'var(--automaton-accent)' : 'var(--automaton-ink)', 'font-weight': used.has(e.key) ? 700 : 400 };
            if (!e.along) { svg.appendChild(make('text', { ...look, x: e.lx, y: e.ly, text: e.label })); continue; }
            const id = `${marker}-along-${e.key}`;
            svg.appendChild(make('path', { 'data-key': `along-${e.key}`, id, d: e.along.d, fill: 'none', stroke: 'none' }));
            svg.appendChild(make('text', look, [make('textPath', { href: `#${id}`, startOffset: e.along.at, 'text-anchor': 'middle', text: e.label })]));
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
        // at most as large as a drawing of the same automaton (1.2)
        picture.style.maxWidth = `${geo.width * 1.2}px`;
        window.AutomatonLayout.hover(picture);
        el.replaceChildren(...(el.dataset.title ? [html('h4', { class: 'automaton-title', text: el.dataset.title })] : []),
            input, html('div', { class: 'automaton-controls' }, [...buttons, counter]), say, picture);
        let steps = [];
        let current = 0;
        let word = '';
        // a step back is the step forward played backwards
        let before = 0;
        const show = (animate) => {
            const step = steps[current];
            const back = animate && current === before - 1;
            before = current;
            const next = make('svg', {});
            draw(next, a, geo, word, step, marker);
            if (window.StepperMotion) window.StepperMotion.morph(picture, next, { duration: animate ? 700 : 0, reverse: back });
            else { picture.setAttribute('viewBox', next.getAttribute('viewBox')); picture.replaceChildren(...next.childNodes); }
            say.innerHTML = withNames(step.message, a.states);
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

    // mount(el): a widget built after the page has loaded (Automatron)
    window.AutomatonStepper = { mount: (el) => { mount(el); document.dispatchEvent(new CustomEvent('stepper-added', { detail: el })); } };
    const start = () => document.querySelectorAll('.automaton-stepper').forEach(mount);
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
