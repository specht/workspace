// Automatron in the browser: a regular expression → ε-NEA (Thompson) →
// DEA (subset construction) → the smallest DEA (table method) → a regular
// grammar, and words that belong to the language and words that do not.
//
// The expressions of a lesson: symbols a–z, A–Z, 0–9, concatenation, |, *,
// + and ? and parentheses. Everything is counted as it always was (the
// numbers of the states are those the automata activity names), so the
// order in which states, symbols and arrows are visited is kept with care:
// a Map and a Set iterate in the order their entries were added.
//
//   Automatron.run('a(b|a)*a+')  → { nfa, dfa, min_dfa, grammar, … }
//                                  or { error: 'character', character }
//
// The automata come as data for automaton_layout.js (drawing), the notes and
// grammars as HTML.
(function (root) {
    'use strict';
    const EPS = 'ε';
    // more states than anyone can read; the subset construction can grow
    // exponentially, so it stops here instead of running on
    const MAX_STATES = 60;
    const MAX_REGEX = 200;

    class Problem extends Error {
        constructor(kind, extra = {}) { super(kind); this.kind = kind; this.extra = extra; }
    }

    // ------------------- the expression -------------------
    const UNARY = new Set(['*', '+', '?']);
    const CONCAT = '.';
    const PRECEDENCE = { '|': 1, [CONCAT]: 2, '*': 3, '+': 3, '?': 3 };
    const LEFT = new Set(['|', CONCAT]);

    function tokenize(regex) {
        const tokens = [];
        for (const ch of regex.trim()) {
            if (ch === '(') tokens.push({ type: 'lparen', value: ch });
            else if (ch === ')') tokens.push({ type: 'rparen', value: ch });
            else if ('|*+?'.includes(ch)) tokens.push({ type: 'op', value: ch });
            else if (/^[a-z0-9]$/i.test(ch)) tokens.push({ type: 'sym', value: ch });
            else if (/^\s$/.test(ch)) continue;
            else throw new Problem('character', { character: ch });
        }
        return tokens;
    }

    function needsConcat(a, b) {
        if ((a.type === 'sym' || a.type === 'rparen') && (b.type === 'sym' || b.type === 'lparen')) return true;
        return a.type === 'op' && UNARY.has(a.value) && (b.type === 'sym' || b.type === 'lparen');
    }

    // shunting-yard to postfix
    function postfix(regex) {
        const plain = tokenize(regex);
        const tokens = [];
        plain.forEach((t, i) => {
            tokens.push(t);
            if (plain[i + 1] && needsConcat(t, plain[i + 1])) tokens.push({ type: 'op', value: CONCAT });
        });
        const output = [];
        const stack = [];
        const top = () => stack[stack.length - 1];
        for (const t of tokens) {
            if (t.type === 'sym') output.push(t);
            else if (t.type === 'op') {
                if (UNARY.has(t.value)) {
                    while (stack.length && top().type === 'op' && PRECEDENCE[top().value] > PRECEDENCE[t.value]) output.push(stack.pop());
                } else {
                    while (stack.length && top().type === 'op' && (PRECEDENCE[top().value] > PRECEDENCE[t.value]
                        || (PRECEDENCE[top().value] === PRECEDENCE[t.value] && LEFT.has(t.value)))) output.push(stack.pop());
                }
                stack.push(t);
            } else if (t.type === 'lparen') stack.push(t);
            else {
                while (stack.length && top().type !== 'lparen') output.push(stack.pop());
                if (!stack.length) throw new Problem('parentheses');
                stack.pop();
            }
        }
        while (stack.length) {
            if (top().type === 'lparen') throw new Problem('parentheses');
            output.push(stack.pop());
        }
        return output;
    }

    // ------------------- Thompson ε-NEA -------------------
    // states: Map id → Map symbol → Set of ids (ε for the empty word)
    const add = (trans, sym, to) => {
        if (!trans.has(sym)) trans.set(sym, new Set());
        trans.get(sym).add(to);
    };

    function nfaFromRegex(regex) {
        let next = 0;
        const stack = [];
        const alphabet = new Set();
        for (const t of postfix(regex)) {
            if (t.type === 'sym') {
                const s = next++;
                const e = next++;
                const states = new Map([[s, new Map()], [e, new Map()]]);
                add(states.get(s), t.value, e);
                stack.push([s, e, states]);
                alphabet.add(t.value);
                continue;
            }
            // an operator without its operands (»a|«, »*a«): the expression
            // is incomplete
            if (stack.length < (t.value === CONCAT || t.value === '|' ? 2 : 1)) throw new Problem('incomplete');
            if (t.value === CONCAT || t.value === '|') {
                const [s2, e2, st2] = stack.pop();
                const [s1, e1, st1] = stack.pop();
                const states = new Map([...st1, ...st2]);
                if (t.value === CONCAT) {
                    add(states.get(e1), EPS, s2);
                    stack.push([s1, e2, states]);
                } else {
                    const ns = next++;
                    const ne = next++;
                    states.set(ns, new Map());
                    states.set(ne, new Map());
                    add(states.get(ns), EPS, s1);
                    add(states.get(ns), EPS, s2);
                    add(states.get(e1), EPS, ne);
                    add(states.get(e2), EPS, ne);
                    stack.push([ns, ne, states]);
                }
                continue;
            }
            const [s1, e1, states] = stack.pop();
            const ns = next++;
            const ne = next++;
            states.set(ns, new Map());
            states.set(ne, new Map());
            if (t.value === '*') {
                add(states.get(ns), EPS, s1);
                add(states.get(ns), EPS, ne);
                add(states.get(e1), EPS, s1);
                add(states.get(e1), EPS, ne);
            } else if (t.value === '+') {
                add(states.get(ns), EPS, s1);
                add(states.get(e1), EPS, s1);
                add(states.get(e1), EPS, ne);
            } else {
                add(states.get(ns), EPS, s1);
                add(states.get(ns), EPS, ne);
                add(states.get(e1), EPS, ne);
            }
            stack.push([ns, ne, states]);
        }
        if (stack.length !== 1) throw new Problem('incomplete');
        const [start, accept, states] = stack.pop();
        const nfa = { start, accept, states, alphabet };
        collapseSimpleEps(nfa);
        return nfa;
    }

    // Linear ε-connectors u --ε--> v, where u has no other way out and v no
    // other way in, become one state, until there are none left
    function collapseSimpleEps(nfa) {
        for (;;) {
            const epsIn = new Map();
            const inAny = new Map();
            const into = (m, k, v) => { if (!m.has(k)) m.set(k, new Set()); m.get(k).add(v); };
            for (const [from, trans] of nfa.states) {
                for (const [sym, set] of trans) for (const to of set) {
                    into(inAny, to, from);
                    if (sym === EPS) into(epsIn, to, from);
                }
            }
            let changed = false;
            for (const [u, su] of nfa.states) {
                // (reading ε creates it, as it did: the order of the symbols
                // decides how the states are numbered later)
                if (!su.has(EPS)) su.set(EPS, new Set());
                const epsOut = su.get(EPS);
                if (epsOut.size !== 1) continue;
                const v = [...epsOut][0];
                const onlyThat = [...su].every(([sym, set]) => (sym === EPS ? set.size === 1 && set.has(v) : set.size === 0));
                if (!onlyThat) continue;
                const vin = inAny.get(v) || new Set();
                const veps = epsIn.get(v) || new Set();
                if (!(vin.size === 1 && vin.has(u) && veps.size === 1 && veps.has(u))) continue;
                const sv = nfa.states.get(v);
                su.get(EPS).delete(v);
                for (const [sym, set] of sv) {
                    if (sym === EPS && set.has(u)) continue;
                    for (const w of set) add(su, sym, w);
                }
                for (const sp of nfa.states.values()) {
                    for (const set of sp.values()) if (set.delete(v)) set.add(u);
                }
                if (nfa.start === v) nfa.start = u;
                if (nfa.accept === v) nfa.accept = u;
                nfa.states.delete(v);
                changed = true;
                break;
            }
            if (!changed) return;
        }
    }

    // the states renumbered by map, the symbols in their order, empty ones gone
    function renumber(nfa, map) {
        const states = new Map();
        for (const [old, trans] of nfa.states) {
            const fresh = new Map();
            for (const [sym, set] of trans) for (const t of set) add(fresh, sym, map.get(t));
            states.set(map.get(old), fresh);
        }
        nfa.states = states;
        nfa.start = map.get(nfa.start);
        nfa.accept = map.get(nfa.accept);
    }

    // 0, 1, 2, … in the order of the old numbers
    function compactIds(nfa) {
        const map = new Map([...nfa.states.keys()].sort((a, b) => a - b).map((old, i) => [old, i]));
        renumber(nfa, map);
    }

    // numbered along the way from the start (depth first, reversed post
    // order), so that q0 is the start and the numbers grow along the arrows
    function topoRelabel(nfa) {
        const visited = new Set();
        const order = [];
        const dfs = (u) => {
            if (visited.has(u)) return;
            visited.add(u);
            for (const set of nfa.states.get(u).values()) for (const v of set) dfs(v);
            order.push(u);
        };
        dfs(nfa.start);
        order.reverse();
        const map = new Map(order.map((old, i) => [old, i]));
        for (const old of [...nfa.states.keys()].filter((k) => !visited.has(k)).sort((a, b) => a - b)) map.set(old, map.size);
        renumber(nfa, map);
    }

    // ------------------- subset construction -------------------
    const keyOf = (set) => [...set].sort((a, b) => a - b).join(',');
    const sorted = (set) => [...set].sort((a, b) => a - b);

    function subsetConstruction(nfa) {
        const alphabet = [...nfa.alphabet];
        const eps = (set) => {
            const stack = [...set];
            const seen = new Set(set);
            while (stack.length) {
                const x = stack.pop();
                for (const y of nfa.states.get(x).get(EPS) || []) {
                    if (seen.has(y)) continue;
                    seen.add(y);
                    stack.push(y);
                }
            }
            return seen;
        };
        const epsClosures = new Map([...nfa.states.keys()].map((q) => [q, sorted(eps(new Set([q])))]));
        const move = (set, sym) => {
            const out = new Set();
            for (const s of set) for (const t of nfa.states.get(s).get(sym) || []) out.add(t);
            return out;
        };
        let next = 0;
        const idOf = new Map();
        const sets = new Map();
        const transitions = new Map();
        const accepts = new Set();
        const startSet = eps(new Set([nfa.start]));
        const startId = next++;
        idOf.set(keyOf(startSet), startId);
        sets.set(startId, startSet);
        const queue = [startSet];
        const trace = { alphabet, startClosure: sorted(startSet), epsClosures, stateMap: new Map([[startId, sorted(startSet)]]), steps: [] };
        while (queue.length) {
            const curr = queue.shift();
            const sid = idOf.get(keyOf(curr));
            if (!transitions.has(sid)) transitions.set(sid, new Map());
            if (curr.has(nfa.accept)) accepts.add(sid);
            for (const sym of alphabet) {
                const m = move(curr, sym);
                const u = eps(m);
                const row = { from: sorted(curr), symbol: sym, move: sorted(m), closure: sorted(u) };
                if (!u.size) { row.to = null; trace.steps.push(row); continue; }
                const k = keyOf(u);
                if (!idOf.has(k)) {
                    if (sets.size >= MAX_STATES) throw new Problem('too_large', { max_states: MAX_STATES });
                    const nid = next++;
                    idOf.set(k, nid);
                    sets.set(nid, u);
                    queue.push(u);
                    trace.stateMap.set(nid, sorted(u));
                }
                transitions.get(sid).set(sym, idOf.get(k));
                row.to = idOf.get(k);
                trace.steps.push(row);
            }
        }
        // complete, with an error state where an arrow is missing
        let dead = null;
        for (const sid of [...sets.keys()]) {
            if (!transitions.has(sid)) transitions.set(sid, new Map());
            if (alphabet.some((sym) => !transitions.get(sid).has(sym))) {
                if (dead === null) {
                    dead = next++;
                    transitions.set(dead, new Map(alphabet.map((sym) => [sym, dead])));
                    sets.set(dead, new Set());
                    trace.stateMap.set(dead, []);
                }
            }
            for (const sym of alphabet) if (!transitions.get(sid).has(sym)) transitions.get(sid).set(sym, dead);
        }
        return [{ states: [...sets.keys()], start: startId, accepts, alphabet, transitions }, trace];
    }

    // the error state: not accepting, every arrow back to itself
    const findSink = (dfa) => {
        const s = dfa.states.find((q) => !dfa.accepts.has(q) && dfa.alphabet.every((a) => dfa.transitions.get(q).get(a) === q));
        return s === undefined ? null : s;
    };

    // ------------------- minimization (table method) -------------------
    function minimize(dfa) {
        const sink = findSink(dfa);
        const states = [...dfa.states].sort((a, b) => a - b);
        const index = new Map(states.map((s, i) => [s, i]));
        const n = states.length;
        const pair = (i, j) => `${Math.min(i, j)},${Math.max(i, j)}`;
        const marked = new Set();
        const initialMarked = new Set();
        for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) {
            if (dfa.accepts.has(states[i]) !== dfa.accepts.has(states[j])) { marked.add(pair(i, j)); initialMarked.add(pair(i, j)); }
        }
        const witnesses = new Map();
        let changed = true;
        while (changed) {
            changed = false;
            for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) {
                if (marked.has(pair(i, j))) continue;
                for (const a of dfa.alphabet) {
                    const ii = index.get(dfa.transitions.get(states[i]).get(a));
                    const jj = index.get(dfa.transitions.get(states[j]).get(a));
                    if (marked.has(pair(ii, jj))) {
                        marked.add(pair(i, j));
                        witnesses.set(pair(i, j), { symbol: a, to: pair(ii, jj) });
                        changed = true;
                        break;
                    }
                }
            }
        }
        const parent = states.map((_, i) => i);
        const rank = states.map(() => 0);
        const find = (x) => { while (parent[x] !== x) { parent[x] = parent[parent[x]]; x = parent[x]; } return x; };
        const unite = (x, y) => {
            const rx = find(x);
            const ry = find(y);
            if (rx === ry) return;
            if (rank[rx] < rank[ry]) parent[rx] = ry;
            else if (rank[rx] > rank[ry]) parent[ry] = rx;
            else { parent[ry] = rx; rank[rx] += 1; }
        };
        for (let i = 0; i < n; i++) for (let j = i + 1; j < n; j++) if (!marked.has(pair(i, j))) unite(i, j);
        const blockOfRoot = new Map();
        const blockId = new Map();
        for (let i = 0; i < n; i++) {
            const r = find(i);
            if (!blockOfRoot.has(r)) blockOfRoot.set(r, blockOfRoot.size);
            blockId.set(states[i], blockOfRoot.get(r));
        }
        const count = blockOfRoot.size;
        const reps = new Map();
        for (const [old, b] of blockId) if (!reps.has(b)) reps.set(b, old);
        const transitions = new Map();
        for (let b = 0; b < count; b++) transitions.set(b, new Map(dfa.alphabet.map((a) => [a, blockId.get(dfa.transitions.get(reps.get(b)).get(a))])));
        const min = { states: [...Array(count).keys()], start: blockId.get(dfa.start), accepts: new Set([...dfa.accepts].map((s) => blockId.get(s))), alphabet: dfa.alphabet, transitions };
        const trace = { states, accepts: states.map((s) => dfa.accepts.has(s)), initialMarked, witnesses,
            blocks: [...Array(count).keys()].map((b) => states.filter((s) => blockId.get(s) === b)), sink };
        return [min, trace];
    }

    // ------------------- notes -------------------
    const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    const q = (x) => `q<sub>${x}</sub>`;

    function subsetNotes(trace) {
        const alpha = trace.alphabet;
        const toDfa = new Map([...trace.stateMap].map(([id, set]) => [set.join(','), id]));
        const byFrom = new Map();
        for (const r of trace.steps) {
            const k = r.from.join(',');
            if (!byFrom.has(k)) byFrom.set(k, new Map());
            byFrom.get(k).set(r.symbol, r);
        }
        const keys = [...byFrom.keys()].sort((a, b) => {
            const ka = toDfa.has(a) ? [0, toDfa.get(a)] : [1, a];
            const kb = toDfa.has(b) ? [0, toDfa.get(b)] : [1, b];
            return ka[0] - kb[0] || (ka[1] < kb[1] ? -1 : ka[1] > kb[1] ? 1 : 0);
        });
        const head = `<tr><th>Zustand (DEA)</th><th>Zustände (NEA)</th>${alpha.map((a) => `<th>${esc(a)}</th>`).join('')}</tr>`;
        const body = keys.map((k) => {
            const tag = toDfa.has(k) ? `r<sub>${toDfa.get(k)}</sub>` : '';
            const cells = alpha.map((sym) => {
                const r = byFrom.get(k).get(sym);
                if (!r) return '<td>—</td>';
                if (!r.closure.length) return '<td>∅</td>';
                return `<td>r<sub>${toDfa.get(r.closure.join(','))}</sub></td>`;
            }).join('');
            return `<tr><td>${tag}</td><td>{${k.split(',').map(q).join('; ')}}</td>${cells}</tr>`;
        }).join('');
        const closures = [...trace.epsClosures].sort((a, b) => a[0] - b[0]).map(([s, set]) => `<tr><td>${q(s)}</td><td>{${set.map(q).join('; ')}}</td></tr>`).join('');
        return `<section class="automatron-notes">
    <div>
    <h4>ε-Hüllen aller Zustände</h4>
    <div style="max-width: 100%; overflow-x: auto;">
    <table class='table' style='width: unset;'>
    <tr><th>q</th><th>E(q)</th></tr>
    ${closures}
    </table>
    </div>
    </div>
    <div>
    <h4>Übergänge</h4>
    <div style="max-width: 100%; overflow-x: auto;">
    <table class='table' style='width: unset;'>
    ${head}
    ${body}
    </table>
    </div>
    </div>
</section>
`;
    }

    function minimizationNotes(trace) {
        const { states, sink } = trace;
        const visible = sink === null ? states : states.filter((s) => s !== sink);
        const orig = new Map(states.map((s, i) => [s, i]));
        const head = `<tr><th></th>${visible.map((s) => `<th>r<sub>${s}</sub></th>`).join('')}</tr>`;
        const cell = (iv, jv) => {
            const i0 = orig.get(visible[iv]);
            const j0 = orig.get(visible[jv]);
            const key = `${Math.min(i0, j0)},${Math.max(i0, j0)}`;
            if (iv === jv) return ['—', 'diag'];
            if (jv < iv) return ['—', 'na'];
            if (trace.initialMarked.has(key)) return ["<i class='bi bi-x text-danger'></i>", 'marked'];
            const w = trace.witnesses.get(key);
            if (w) return [`<i class='bi bi-x text-danger'></i> <span class='reason'>(${esc(w.symbol)})</span>`, 'marked'];
            return ["<i class='bi bi-check text-success'></i>", 'equiv'];
        };
        const body = visible.map((s, i) => `<tr><th>r<sub>${s}</sub></th>${visible.map((_, j) => { const [html, cls] = cell(i, j); return `<td class="${cls}">${html}</td>`; }).join('')}</tr>`).join('');
        const blocks = sink === null ? trace.blocks : trace.blocks.filter((b) => !b.includes(sink));
        const parts = blocks.map((b, i) => `<tr><td>s<sub>${i}</sub></td><td>{${b.map((s) => `r<sub>${s}</sub>`).join('; ')}}</td></tr>`).join('');
        return `<section>
  <div style="max-width: 100%; overflow-x: auto;">
  <table class="min-table">
    ${head}
    ${body}
  </table>
  </div>
  <h4>Resultierende Partitionen</h4>
  <div style="max-width: 100%; overflow-x: auto;">
  <table class='table' style='width: unset;'>
  <tr><th>Zustand (min. DEA)</th><th>Partition</th></tr>
  ${parts}
  </table>
  </div>
</section>
`;
    }

    // ------------------- the regular grammar -------------------
    function grammarOf(dfa) {
        const letters = 'ABCDEFGHIJKLMNOPQRTUVWXYZ';
        const names = new Map([[dfa.start, 'S']]);
        const others = dfa.states.filter((s) => s !== dfa.start).sort((a, b) => a - b);
        others.forEach((s, i) => names.set(s, letters[i % letters.length]));
        const productions = new Map();
        for (const s of dfa.states) {
            const lhs = names.get(s);
            if (!productions.has(lhs)) productions.set(lhs, []);
            for (const a of dfa.alphabet) productions.get(lhs).push(`${a}${names.get(dfa.transitions.get(s).get(a))}`);
            if (dfa.accepts.has(s)) productions.get(lhs).push(EPS);
        }
        return { nonterminals: [...new Set(names.values())], terminals: [...dfa.alphabet], start: 'S', productions };
    }

    // the nonterminals that can end (derive ε)
    function productive(g) {
        const rev = new Map();
        for (const [a, list] of g.productions) for (const rhs of list) {
            if (rhs === EPS) continue;
            const b = rhs.slice(1);
            if (!rev.has(b)) rev.set(b, new Set());
            rev.get(b).add(a);
        }
        const out = new Set();
        const queue = [];
        for (const [a, list] of g.productions) if (list.includes(EPS)) { out.add(a); queue.push(a); }
        while (queue.length) {
            const x = queue.shift();
            for (const p of rev.get(x) || []) if (!out.has(p)) { out.add(p); queue.push(p); }
        }
        return out;
    }

    const orderedLhs = (keys) => ['S', ...keys.filter((k) => k !== 'S').sort()];

    function grammarHtml(g) {
        const prod = productive(g);
        const lines = [`Nicht-Terminale: ${g.nonterminals.join(', ')}<br>`, `Terminale: ${g.terminals.join(', ')}<br>`, `Start: ${g.start}<br>`, 'Produktionsregeln:<br>', '<ul>'];
        for (const lhs of orderedLhs([...g.productions.keys()])) {
            if (!g.productions.has(lhs)) continue;
            const rhs = [...new Set(g.productions.get(lhs))].filter((r) => r === EPS || prod.has(r.slice(1)));
            if (rhs.length) lines.push(`<li>${lhs} → ${rhs.join(' | ')}</li>`);
        }
        lines.push('</ul>');
        return lines.join('\n');
    }

    // Without rules that can never end, and with chains inlined (S → ab)
    function simplify(g) {
        const start = g.start;
        const prod = productive(g);
        // [w, Y]: a terminal word and a nonterminal (null: none)
        const p2 = new Map();
        for (const [lhs, list] of g.productions) p2.set(lhs, list.map((rhs) => (rhs === EPS ? ['', null] : [rhs[0], rhs.slice(1)])).filter(([, y]) => y === null || prod.has(y)));
        for (const lhs of [...p2.keys()]) if (!p2.get(lhs).length && lhs !== start) p2.delete(lhs);
        let changed = true;
        const replace = (x, by) => {
            let used = false;
            for (const list of p2.values()) list.forEach(([w, y], i) => { if (y === x) { list[i] = by(w); used = true; } });
            return used;
        };
        while (changed) {
            changed = false;
            for (const x of [...p2.keys()]) {
                const arr = p2.get(x);
                if (!arr || !(arr.length === 1 && arr[0][0] === '' && arr[0][1] === null)) continue;
                if (replace(x, (w) => [w, null])) changed = true;
                if (x !== start) p2.delete(x);
            }
            for (const x of [...p2.keys()]) {
                if (x === start || !p2.has(x)) continue;
                const arr = p2.get(x);
                if (arr.length !== 1) continue;
                const [w1, y1] = arr[0];
                if (y1 !== null || w1 === '') continue;
                if (replace(x, (w) => [w + w1, null])) { changed = true; p2.delete(x); }
            }
            for (const x of [...p2.keys()]) {
                if (x === start || !p2.has(x)) continue;
                const arr = p2.get(x);
                if (arr.length !== 1) continue;
                const [w1, y1] = arr[0];
                if (y1 === null) continue;
                if (replace(x, (w) => [w + w1, y1])) { changed = true; p2.delete(x); }
            }
        }
        const productions = new Map([...p2].map(([lhs, list]) => [lhs, list.map(([w, y]) => (y ? `${w}${y}` : w === '' ? EPS : w))]));
        const still = new Set(productions.keys());
        for (const list of productions.values()) for (const rhs of list) {
            if (rhs === EPS) continue;
            const m = rhs.match(/[A-Z]+$/);
            if (m) still.add(m[0]);
        }
        return { nonterminals: [...still], terminals: g.terminals, start, productions };
    }

    function compactHtml(g) {
        const lines = [`Nicht-Terminale: ${[...g.nonterminals].sort().join(', ')}<br>`, `Terminale: ${[...g.terminals].sort().join(', ')}<br>`, `Start: ${g.start}<br>`, 'Produktionsregeln:<br>', '<ul>'];
        for (const lhs of orderedLhs([...g.productions.keys()])) {
            if (!g.productions.has(lhs)) continue;
            // ε first, then words, then rules that go on
            const rank = (r) => (r === EPS ? -1 : /[A-Z]+$/.test(r) ? 1 : 0);
            const rhs = [...new Set(g.productions.get(lhs))].sort((a, b) => rank(a) - rank(b));
            lines.push(`<li>${lhs} → ${rhs.join(' | ')}</li>`);
        }
        lines.push('</ul>');
        return lines.join('\n');
    }

    // ------------------- words -------------------
    const byLength = (words) => [...new Set(words)].sort((a, b) => a.length - b.length || (a < b ? -1 : a > b ? 1 : 0));

    // words of the language, from the grammar; ending is a little likelier
    function positives(g, count = 30, maxSteps = 30, epsBias = 1, random = Math.random) {
        const prod = productive(g);
        const words = [];
        for (let attempts = 0; words.length < count && attempts < count * 50; attempts++) {
            let out = '';
            let nt = g.start;
            let ok = false;
            for (let steps = 0; nt && steps < maxSteps; steps++) {
                const choices = g.productions.get(nt);
                if (!choices || !choices.length) break;
                let usable = choices.filter((r) => r === EPS || prod.has(r.slice(1)));
                if (!usable.length) {
                    if (choices.includes(EPS)) usable = [EPS];
                    else break;
                }
                const weighted = usable.map((r) => [r, r === EPS ? 1 + epsBias : 1]);
                const total = weighted.reduce((s, [, w]) => s + w, 0);
                const r = random() * total;
                let acc = 0;
                let pick = weighted[weighted.length - 1][0];
                for (const [rhs, w] of weighted) { acc += w; if (r <= acc) { pick = rhs; break; } }
                if (pick === EPS) { ok = true; break; }
                out += pick[0];
                nt = pick.slice(1);
            }
            if (ok) words.push(out);
        }
        return byLength(words);
    }

    function accepts(dfa, word) {
        let s = dfa.start;
        for (const ch of word) {
            if (!dfa.alphabet.includes(ch)) return false;
            s = dfa.transitions.get(s).get(ch);
            if (s === undefined || s === null) return false;
        }
        return dfa.accepts.has(s);
    }

    // words that do not belong: random words the smallest DEA rejects
    function negatives(dfa, count = 30, maxLen = 20, stop = 0.25, random = Math.random) {
        const alphabet = [...dfa.alphabet].sort();
        const out = [];
        for (let attempts = 0; out.length < count && attempts < count * 200; attempts++) {
            let s = '';
            while (s.length < maxLen) {
                if (s.length > 0 && random() < stop) break;
                s += alphabet[Math.floor(random() * alphabet.length)];
            }
            if (s && !accepts(dfa, s)) out.push(s);
        }
        return byLength(out);
    }

    // ------------------- for the drawings -------------------
    // automaton_layout.js: states with names, the start, the accepting
    // states, an arrow for every symbol
    function nfaDrawing(nfa) {
        const edges = [];
        for (const [id, trans] of nfa.states) for (const [sym, set] of trans) for (const to of set) edges.push({ from: `q${id}`, to: `q${to}`, label: sym });
        return { states: [...nfa.states.keys()].sort((x, y) => x - y).map((id) => ({ id: `q${id}`, label: `q${id}` })), start: `q${nfa.start}`, accept: [`q${nfa.accept}`], edges,
            // numbered along its arrows: columns by the longest way, so that
            // it reads as the construction built it
            columns: 'order' };
    }

    // without the error state and the arrows into it
    function dfaDrawing(dfa, letter) {
        const sink = findSink(dfa);
        const name = (s) => `${letter}${s}`;
        const states = dfa.states.filter((s) => s !== sink);
        const edges = [];
        for (const s of states) for (const a of dfa.alphabet) {
            const t = dfa.transitions.get(s).get(a);
            if (t !== sink) edges.push({ from: name(s), to: name(t), label: a });
        }
        return { states: states.map((s) => ({ id: name(s), label: name(s) })), start: name(dfa.start), accept: states.filter((s) => dfa.accepts.has(s)).map(name), edges };
    }

    function run(regex, random = Math.random) {
        try {
            if (!regex || !regex.trim()) throw new Problem('incomplete');
            if (regex.length > MAX_REGEX) throw new Problem('regex too long');
            const nfa = nfaFromRegex(regex);
            compactIds(nfa);
            topoRelabel(nfa);
            const [dfa, subsetTrace] = subsetConstruction(nfa);
            const [min, minTrace] = minimize(dfa);
            const grammar = grammarOf(min);
            return {
                nfa: { drawing: nfaDrawing(nfa) },
                dfa: { implicit_sink: { drawing: dfaDrawing(dfa, 'r'), has_sink: findSink(dfa) !== null } },
                min_dfa: { implicit_sink: { drawing: dfaDrawing(min, 's'), has_sink: findSink(min) !== null } },
                grammar: grammarHtml(grammar),
                grammar_compact: compactHtml(simplify(grammar)),
                samples_positive: positives(grammar, 30, 30, 1, random),
                samples_negative: negatives(min, 30, 20, 0.25, random),
                notes_subset_html: subsetNotes(subsetTrace),
                notes_minimization_html: minimizationNotes(minTrace),
                automata: { nfa, dfa, min },
            };
        } catch (e) {
            if (e instanceof Problem) return { error: e.kind, ...e.extra };
            throw e;
        }
    }

    const api = { run, MAX_STATES, MAX_REGEX, accepts, _internals: { nfaFromRegex, compactIds, topoRelabel, subsetConstruction, minimize, grammarOf, simplify, grammarHtml, compactHtml } };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    else root.Automatron = api;

    // In a page: <div class='automaton-drawing' data-regex='a|b'
    // data-automaton='nfa'> draws what Automatron draws for that expression
    // (nfa, dfa or min), so that a Handbuch page shows the same automata
    if (typeof document === 'undefined') return;
    const draw = () => document.querySelectorAll('.automaton-drawing[data-regex]').forEach((el) => {
        if (el.dataset.drawn || !root.AutomatonLayout) return;
        el.dataset.drawn = '1';
        const r = run(el.dataset.regex);
        if (r.error) return;
        const which = el.dataset.automaton || 'nfa';
        const a = which === 'min' ? r.min_dfa.implicit_sink.drawing : which === 'dfa' ? r.dfa.implicit_sink.drawing : r.nfa.drawing;
        el.innerHTML = root.AutomatonLayout.svg(a);
        if (el.querySelector('svg')) root.AutomatonLayout.hover(el.querySelector('svg'));
    }) || (root.AutomatonLayout && root.AutomatonLayout.fitGalleries());
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', draw);
    else draw();
})(typeof window !== 'undefined' ? window : globalThis);
