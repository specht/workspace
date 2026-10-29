// How Prolog finds its answers (content/prolog, »Wie Prolog sucht«):
// <div class='prolog-search' data-stepper data-query='grandparent(G, wilhelm_friedemann)'>
// with the program in a <script type='text/plain'> inside. A small resolver
// works through the query depth first, as Prolog does, and records every
// step: which goal it tries to prove, which clause of the program fits
// (marked in the program), what it remembers to come back to (the choice
// points) and every answer. The steps go with stepper_keys.js (keys,
// play, timeline): data-step, data-steps and stepperGo. It knows atoms,
// variables, compound terms and \=, which is all the family on the page
// needs: no lists, numbers or arithmetic, and _ is an ordinary variable.
(function () {
    'use strict';

    // ----- terms: { a: 'atom' }, { v: 'Name' }, { f: 'name', args: [...] } -----

    function parseProgram(text) {
        const clauses = [];
        text.split('\n').forEach((raw, index) => {
            const line = raw.replace(/%.*$/, '').trim();
            if (!line) return;
            const body = line.replace(/\.\s*$/, '');
            const [head, rest] = body.split(':-');
            clauses.push({ head: parseTerm(head.trim()), body: rest ? splitGoals(rest).map(parseTerm) : [], line: index, text: line });
        });
        return clauses;
    }

    function splitGoals(text) {
        const goals = [];
        let depth = 0;
        let start = 0;
        for (let i = 0; i < text.length; i++) {
            if (text[i] === '(') depth++;
            else if (text[i] === ')') depth--;
            else if (text[i] === ',' && depth === 0) {
                goals.push(text.slice(start, i).trim());
                start = i + 1;
            }
        }
        goals.push(text.slice(start).trim());
        return goals;
    }

    function parseTerm(text) {
        const unequal = text.split('\\=');
        if (unequal.length === 2) return { f: '\\=', args: [parseTerm(unequal[0].trim()), parseTerm(unequal[1].trim())] };
        const open = text.indexOf('(');
        if (open < 0) return /^[A-Z_]/.test(text) ? { v: text } : { a: text };
        return { f: text.slice(0, open), args: splitGoals(text.slice(open + 1, text.lastIndexOf(')'))).map(parseTerm) };
    }

    // a fresh copy of a clause: its variables get a number, G becomes G₁
    function rename(term, n) {
        if (term.v) return { v: term.v, n };
        if (term.a) return term;
        return { f: term.f, args: term.args.map((t) => rename(t, n)) };
    }

    const key = (v) => v.v + '#' + (v.n || 0);

    function walk(term, subst) {
        while (term.v && subst.has(key(term))) term = subst.get(key(term));
        return term;
    }

    function resolve(term, subst) {
        term = walk(term, subst);
        if (term.f) return { f: term.f, args: term.args.map((t) => resolve(t, subst)) };
        return term;
    }

    function unify(x, y, subst) {
        x = walk(x, subst);
        y = walk(y, subst);
        if (x.v && y.v && key(x) === key(y)) return subst;
        if (x.v) return new Map(subst).set(key(x), y);
        if (y.v) return new Map(subst).set(key(y), x);
        if (x.a || y.a) return x.a === y.a ? subst : null;
        if (x.f !== y.f || x.args.length !== y.args.length) return null;
        for (let i = 0; i < x.args.length; i++) {
            subst = unify(x.args[i], y.args[i], subst);
            if (!subst) return null;
        }
        return subst;
    }

    const SUB = '₀₁₂₃₄₅₆₇₈₉';
    function show(term) {
        if (term.a) return term.a;
        if (term.v) return term.v + (term.n ? String(term.n).split('').map((d) => SUB[d]).join('') : '');
        if (term.f === '\\=') return show(term.args[0]) + ' \\= ' + show(term.args[1]);
        return term.f + '(' + term.args.map(show).join(', ') + ')';
    }

    // ----- the search, recorded step by step -----

    function trace(clauses, query, limit = 400) {
        const goal = parseTerm(query);
        const queryVars = [];
        (function collect(t) {
            if (t.v && !queryVars.includes(t.v)) queryVars.push(t.v);
            if (t.f) t.args.forEach(collect);
        })(goal);
        const steps = [];
        const answers = [];
        let renames = 0;
        // a choice point: the goals and bindings before a goal was proved,
        // and the clause to try next for it
        const choices = [];
        const snapshot = (kind, say, goals, subst, clause) => {
            steps.push({
                kind, say, clause,
                goals: goals.map((g) => show(resolve(g, subst))),
                choices: choices.map((c) => ({ goal: show(resolve(c.goals[0], c.subst)), next: c.next })),
                answers: answers.slice(),
            });
        };
        const answerText = (subst) => queryVars.map((v) => v + ' = ' + show(resolve({ v }, subst))).join(', ') || 'true';

        let goals = [goal];
        let subst = new Map();
        let from = 0;
        snapshot('start', 'Die Anfrage ist das erste Ziel. Prolog beweist die Ziele von links nach rechts.', goals, subst, null);
        while (steps.length < limit) {
            if (goals.length === 0) {
                answers.push(answerText(subst));
                snapshot('answer', 'Alle Ziele sind bewiesen: eine Antwort, ' + answerText(subst) + '. Wer mit ; nach der nächsten fragt, schickt Prolog zum letzten Merkpunkt zurück.', goals, subst, null);
                if (!backtrack()) break;
                continue;
            }
            const first = goals[0];
            if (first.f === '\\=') {
                const left = resolve(first.args[0], subst);
                const right = resolve(first.args[1], subst);
                const different = unify(left, right, subst) === null;
                if (different) {
                    goals = goals.slice(1);
                    snapshot('fits', show(left) + ' und ' + show(right) + ' sind verschieden: Das Ziel ist bewiesen.', goals, subst, null);
                } else {
                    snapshot('fail', show(left) + ' und ' + show(right) + ' sind gleich: Das Ziel scheitert.', goals, subst, null);
                    if (!backtrack()) break;
                }
                continue;
            }
            let found = -1;
            let next = null;
            for (let i = from; i < clauses.length; i++) {
                const fresh = rename(clauses[i].head, renames + 1);
                const s = unify(first, fresh, subst);
                if (s) {
                    found = i;
                    next = s;
                    break;
                }
            }
            if (found < 0) {
                snapshot('fail', 'Für ' + show(resolve(first, subst)) + ' passt ' + (from > 0 ? 'keine weitere Klausel' : 'keine Klausel') + ' im Programm: Das Ziel scheitert.', goals, subst, null);
                if (!backtrack()) break;
                continue;
            }
            renames++;
            const clause = clauses[found];
            // more clauses after this one might fit: remember to come back
            const more = clauses.slice(found + 1).some((c) => unify(first, rename(c.head, 0), subst));
            if (more) choices.push({ goals, subst, next: found + 1 });
            const body = clause.body.map((t) => rename(t, renames));
            const before = goals;
            goals = body.concat(goals.slice(1));
            const bound = [];
            (function collect(t) {
                if (t.v) {
                    const r = resolve(t, next);
                    if (!r.v) bound.push(show(t) + ' = ' + show(r));
                }
                if (t.f) t.args.forEach(collect);
            })(before[0]);
            const what = clause.body.length ? 'zur Regel in Zeile ' + (found + 1) : 'zum Fakt in Zeile ' + (found + 1);
            const instead = clause.body.length ? ' An die Stelle des Ziels treten die Ziele der Regel.' : '';
            snapshot('fits', show(resolve(first, subst)) + ' passt ' + what + (bound.length ? ', mit ' + bound.join(' und ') : '') + '.' + instead +
                (more ? ' Weitere Klauseln könnten passen: Prolog merkt sich die Stelle.' : ''), goals, next, found);
            subst = next;
            from = 0;
        }
        snapshot('done', answers.length ? 'Kein Merkpunkt mehr: Prolog hat alle Antworten gefunden.' : 'Kein Merkpunkt mehr: Es gibt keine Antwort, Prolog sagt false.', [], new Map(), null);
        return steps;

        function backtrack() {
            const c = choices.pop();
            if (!c) return false;
            goals = c.goals;
            subst = c.subst;
            from = c.next;
            snapshot('back', 'Zurück zum letzten Merkpunkt: Für ' + show(resolve(goals[0], subst)) + ' probiert Prolog die Klauseln ab Zeile ' + (from + 1) + '.', goals, subst, null);
            return true;
        }
    }

    // ----- the widget -----

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    function mount(box) {
        const source = box.querySelector('script[type="text/plain"]');
        const program = source ? source.textContent : '';
        const clauses = parseProgram(program);
        const steps = trace(clauses, box.dataset.query);
        box.replaceChildren();
        const title = el('div', 'prolog-search-title');
        title.append('?- ', el('code', '', box.dataset.query + '.'));
        const controls = el('div', 'prolog-search-controls');
        const button = (go, icon, label) => {
            const b = el('button', 'btn btn-sm btn-success');
            b.dataset.go = go;
            b.setAttribute('aria-label', label);
            b.innerHTML = "<i class='bi " + icon + "' style='margin-right: 0;'></i>";
            return b;
        };
        const count = el('span', 'prolog-search-count');
        controls.append(button('first', 'bi-chevron-bar-left', 'Zum Anfang'), button('prev', 'bi-chevron-left', 'Einen Schritt zurück'),
            button('next', 'bi-chevron-right', 'Nächster Schritt'), button('last', 'bi-chevron-bar-right', 'Zum Ende'), count);
        const say = el('div', 'prolog-search-say');
        say.setAttribute('aria-live', 'polite');
        const goals = el('div', 'prolog-search-goals');
        const choices = el('div', 'prolog-search-choices');
        const answers = el('div', 'prolog-search-answers');
        const code = el('ol', 'prolog-search-program');
        const lines = clauses.map((c) => {
            const li = el('li');
            li.appendChild(el('code', 'prolog-line', c.text));
            code.appendChild(li);
            return li;
        });
        const state = el('div', 'prolog-search-state');
        const part = (label, content) => {
            const p = el('div', 'prolog-search-part');
            p.append(el('div', 'prolog-search-label', label), content);
            return p;
        };
        state.append(part('Noch zu beweisen', goals), part('Merkpunkte', choices), part('Antworten', answers));
        const body = el('div', 'prolog-search-body');
        body.append(state, code);
        box.append(title, controls, say, body);

        let current = 0;
        const draw = () => {
            const s = steps[current];
            say.textContent = s.say;
            say.dataset.kind = s.kind;
            goals.replaceChildren(...(s.goals.length ? s.goals.map((g, i) => el('code', i === 0 ? 'prolog-goal current' : 'prolog-goal', g)) : [el('span', 'prolog-none', 'nichts')]));
            choices.replaceChildren(...(s.choices.length ? s.choices.slice().reverse().map((c) => el('div', 'prolog-choice', c.goal + ': ab Zeile ' + (c.next + 1))) : [el('span', 'prolog-none', 'keine')]));
            answers.replaceChildren(...(s.answers.length ? s.answers.map((a) => el('code', 'prolog-answer', a)) : [el('span', 'prolog-none', 'noch keine')]));
            lines.forEach((li, i) => li.classList.toggle('current', i === s.clause));
            if (s.clause !== null && lines[s.clause]) {
                const li = lines[s.clause];
                code.scrollTop = Math.max(0, li.offsetTop - code.clientHeight / 2);
            }
            count.textContent = 'Schritt ' + (current + 1) + ' von ' + steps.length;
            box.dataset.step = current;
            box.dataset.steps = steps.length;
            controls.querySelector("[data-go='first']").disabled = current === 0;
            controls.querySelector("[data-go='prev']").disabled = current === 0;
            controls.querySelector("[data-go='next']").disabled = current === steps.length - 1;
            controls.querySelector("[data-go='last']").disabled = current === steps.length - 1;
        };
        const to = (i) => {
            current = Math.max(0, Math.min(steps.length - 1, i));
            draw();
        };
        box.stepperGo = to;
        controls.querySelector("[data-go='first']").addEventListener('click', () => to(0));
        controls.querySelector("[data-go='prev']").addEventListener('click', () => to(current - 1));
        controls.querySelector("[data-go='next']").addEventListener('click', () => to(current + 1));
        controls.querySelector("[data-go='last']").addEventListener('click', () => to(steps.length - 1));
        draw();
        document.dispatchEvent(new CustomEvent('stepper-added', { detail: box }));
    }

    const api = { parseProgram, parseTerm, trace };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.prolog-search').forEach((box) => {
            if (box.dataset.ready) return;
            box.dataset.ready = '1';
            mount(box);
        });
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
        else start();
    }
})();
