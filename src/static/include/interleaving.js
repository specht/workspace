// Threads side by side, one step at a time, in an order the student picks
// (»Nebenläufigkeit«). Each thread has a few lines of the site's
// pseudocode; a button per thread runs its next line. The widget shows each
// thread's current line, its own variables, the shared variables and who
// holds which lock, and says what the step did. A thread waiting for a
// lock another one holds cannot step; when none can and not all are done,
// that is a deadlock.
//
// <div class='interleaving' data-program='{"shared": {"counter": 0},
//   "locks": [], "threads": {"A": ["a ← counter", …], "B": […]}}'></div>
// with <script src='/include/interleaving.js'></script>. In a task card
// (check type interleaving, InterleavingCheck on the server, with the same
// rules) the order goes into the card's hidden answer, »A A B B A B«.
(function () {
    'use strict';

    const MAX_STEPS = 100;
    const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');

    // A line as InterleavingCheck.parse_line reads it
    function parseLine(line) {
        const text = String(line).trim();
        let m = text.match(/^(lock|unlock)\s+([a-z_][a-z0-9_]*)$/);
        if (m) return { kind: m[1], lock: m[2] };
        m = text.match(/^([a-z_][a-z0-9_]*)\s*←\s*(.+)$/);
        if (!m) return null;
        const tokens = m[2].match(/[a-z_][a-z0-9_]*|\d+|[+-]|\S/g) || [];
        const terms = [];
        let sign = 1;
        let expectTerm = true;
        for (const t of tokens) {
            if (expectTerm) {
                if (!/^([a-z_][a-z0-9_]*|\d+)$/.test(t)) return null;
                terms.push([sign, /^\d+$/.test(t) ? Number(t) : t]);
                expectTerm = false;
            } else {
                if (t !== '+' && t !== '-') return null;
                sign = t === '+' ? 1 : -1;
                expectTerm = true;
            }
        }
        if (expectTerm) return null;
        return { kind: 'assign', target: m[1], terms };
    }

    // The state after an order of steps, with what the last step did
    function replay(program, order) {
        const names = Object.keys(program.threads);
        const code = Object.fromEntries(names.map((t) => [t, program.threads[t].map(parseLine)]));
        const state = {
            shared: { ...program.shared },
            locals: Object.fromEntries(names.map((t) => [t, {}])),
            at: Object.fromEntries(names.map((t) => [t, 0])),
            holder: Object.fromEntries((program.locks || []).map((l) => [l, null])),
            last: null,
        };
        const done = (t) => state.at[t] >= code[t].length;
        const blocked = (t) => {
            if (done(t)) return { done: true };
            const s = code[t][state.at[t]];
            if (s.kind !== 'lock') return null;
            const other = state.holder[s.lock];
            return other && other !== t ? { lock: s.lock, by: other } : null;
        };
        const value = (t, term) => typeof term === 'number' ? term : term in state.shared ? state.shared[term] : (state.locals[t][term] ?? 0);
        for (const t of order) {
            if (!names.includes(t) || blocked(t)) break;
            const line = program.threads[t][state.at[t]];
            const s = code[t][state.at[t]];
            if (s.kind === 'lock') {
                state.holder[s.lock] = t;
                state.last = { t, line, text: `${t} nimmt den Lock ${s.lock}. Bis ${t} ihn freigibt, wartet jeder andere Thread, der ihn will.` };
            } else if (s.kind === 'unlock') {
                if (state.holder[s.lock] === t) state.holder[s.lock] = null;
                state.last = { t, line, text: `${t} gibt den Lock ${s.lock} frei.` };
            } else {
                const v = s.terms.reduce((sum, [sign, term]) => sum + sign * value(t, term), 0);
                const shared = s.target in state.shared;
                (shared ? state.shared : state.locals[t])[s.target] = v;
                state.last = { t, line, text: `${t} rechnet ${line.replace(/^.*←\s*/, '')} und schreibt ${v} nach ${s.target}${shared ? ', die gemeinsame Variable' : ', seine eigene Variable'}.` };
            }
            state.at[t] += 1;
        }
        state.names = names;
        state.done = done;
        state.blocked = blocked;
        return state;
    }

    function mount(root) {
        const program = JSON.parse(root.dataset.program);
        const names = Object.keys(program.threads);
        const card = root.closest('.task-card');
        const field = card ? card.querySelector('.interleaving-answer') : null;
        let order = [];

        root.innerHTML = `
            ${root.dataset.title ? `<div class='il-title'>${esc(root.dataset.title)}</div>` : ''}
            <div class='il-shared'></div>
            <div class='il-threads'>${names.map((t) => `
                <div class='il-thread' data-thread='${t}'>
                    <div class='il-head'>Thread ${t}</div>
                    <ol class='il-code'>${program.threads[t].map((l) => `<li><code>${esc(l)}</code></li>`).join('')}</ol>
                    <div class='il-locals'></div>
                </div>`).join('')}
            </div>
            <div class='il-controls'>
                ${names.map((t) => `<button type='button' class='btn btn-sm btn-outline-primary il-step' data-thread='${t}'>${t}: nächster Schritt</button>`).join('')}
                <span class='il-spacer'></span>
                <button type='button' class='btn btn-sm btn-outline-secondary il-undo'><i class='bi bi-arrow-counterclockwise' aria-hidden='true'></i> Zurück</button>
                <button type='button' class='btn btn-sm btn-outline-secondary il-reset'>Von vorn</button>
            </div>
            <div class='il-order' aria-live='polite'></div>
            <div class='il-say' aria-live='polite'></div>`;

        const box = (name, value, extra = '') => `<span class='il-box${extra}'><span class='il-name'>${esc(name)}</span><span class='il-value'>${esc(value)}</span></span>`;

        function draw() {
            const s = replay(program, order);
            root.querySelector('.il-shared').innerHTML =
                `<span class='il-label'>gemeinsam</span>` +
                Object.entries(s.shared).map(([k, v]) => box(k, v)).join('') +
                Object.entries(s.holder).map(([l, by]) => box(`Lock ${l}`, by ? `hält ${by}` : 'frei', by ? ' il-held' : '')).join('');
            for (const t of names) {
                const col = root.querySelector(`.il-thread[data-thread='${t}']`);
                const why = s.blocked(t);
                col.classList.toggle('il-finished', s.done(t));
                col.classList.toggle('il-waiting', !!(why && why.lock));
                col.querySelectorAll('.il-code li').forEach((li, i) => {
                    li.classList.toggle('il-current', i === s.at[t]);
                    li.classList.toggle('il-past', i < s.at[t]);
                });
                const locals = Object.entries(s.locals[t]);
                col.querySelector('.il-locals').innerHTML =
                    (locals.length ? locals.map(([k, v]) => box(k, v)).join('') : `<span class='il-none'>noch keine eigenen Variablen</span>`) +
                    (s.done(t) ? `<span class='il-state'><i class='bi bi-check-lg' aria-hidden='true'></i> fertig</span>` :
                        why && why.lock ? `<span class='il-state il-wait'>wartet auf ${esc(why.lock)}</span>` : '');
                const button = root.querySelector(`.il-step[data-thread='${t}']`);
                button.disabled = !!why || order.length >= MAX_STEPS;
                button.title = why ? (why.done ? `${t} ist fertig.` : `${t} wartet auf den Lock ${why.lock}, den ${why.by} hält.`) : '';
            }
            root.querySelector('.il-undo').disabled = root.querySelector('.il-reset').disabled = order.length === 0;
            root.querySelector('.il-order').innerHTML = order.length ?
                `<span class='il-label'>Reihenfolge</span>${order.map((t, i) => `<span class='il-chip${i === order.length - 1 ? ' il-latest' : ''}'>${t}</span>`).join('')}` : '';
            const all = names.every((t) => s.done(t));
            const stuck = !all && names.every((t) => s.blocked(t));
            const shared = Object.entries(s.shared).map(([k, v]) => `${k} = ${v}`).join(', ');
            let say = s.last ? `<code>${esc(s.last.line)}</code>: ${esc(s.last.text)}` : 'Wähle, welcher Thread den nächsten Schritt macht.';
            if (all) say += ` Alle Threads sind fertig: ${esc(shared)}.`;
            if (stuck) say += ` <strong>Keiner kann weiter:</strong> ${names.map((t) => `${t} wartet auf ${esc(s.blocked(t).lock)}, den ${s.blocked(t).by} hält`).join(', ')}. Das ist ein Deadlock.`;
            root.querySelector('.il-say').innerHTML = say;
            if (field) field.value = order.join(' ');
        }

        root.querySelectorAll('.il-step').forEach((b) => b.addEventListener('click', () => { order.push(b.dataset.thread); draw(); }));
        root.querySelector('.il-undo').addEventListener('click', () => { order.pop(); draw(); });
        root.querySelector('.il-reset').addEventListener('click', () => { order = []; draw(); });
        if (field) {
            field.addEventListener('task-answer', (e) => {
                const saved = String(e.detail.answer || '').split(/[\s,]+/).filter((t) => names.includes(t));
                // only as far as the saved order can go
                order = [];
                for (const t of saved) {
                    const s = replay(program, order);
                    if (s.blocked(t)) break;
                    order.push(t);
                }
                draw();
            });
        }
        draw();
    }

    const start = () => document.querySelectorAll('.interleaving[data-program]').forEach((el) => {
        if (!el.dataset.mounted) { el.dataset.mounted = 'true'; mount(el); }
    });
    if (typeof module !== 'undefined' && module.exports) module.exports = { parseLine, replay };
    if (typeof document !== 'undefined') {
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})();
