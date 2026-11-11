// Dark Patterns (Handbuch, reference/dark-patterns): four small demos, each
// beside its pattern, made up (no real shop or site).
//
//   <script src='/include/dark_patterns_demo.js'></script>
//   <div class='dp-demo' data-demo='banner'></div>     declining a cookie banner: clicks counted
//   <div class='dp-demo' data-demo='wording'></div>    a newsletter box in double negation
//   <div class='dp-demo' data-demo='urgency'></div>    a countdown that starts over
//   <div class='dp-demo' data-demo='costs'></div>      a price that grows at the till
//
// Each shows the unfair version beside a fair one where there is one. They
// use the frame and the panels of the accessibility demos (a11y_demo.js,
// default.css: .a11y-panels, .a11y-panel, .a11y-sample).
(function () {
    'use strict';

    const el = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'class') node.className = v;
            else if (k === 'text') node.textContent = v;
            else node.setAttribute(k, v);
        }
        for (const c of [].concat(children)) node.append(c);
        return node;
    };
    const icon = (name) => el('i', { class: `bi bi-${name}`, 'aria-hidden': 'true' });
    const button = (text, cls, onClick) => {
        const b = el('button', { type: 'button', class: `btn btn-sm ${cls}`, text });
        b.addEventListener('click', onClick);
        return b;
    };
    const panel = (title, body) => el('figure', { class: 'a11y-panel' }, [el('figcaption', { text: title }), el('div', { class: 'a11y-sample' }, body)]);
    const euro = (cents) => `${(cents / 100).toLocaleString('de-DE', { minimumFractionDigits: 2 })} €`;
    let counter = 0;

    // ------------------------------------------------------------------
    // declining a cookie banner, unfair and fair, with the clicks it takes
    function bannerDemo(root) {
        root.dataset.chip = 'Zum Ausprobieren: ablehnen';
        // the outcome in the card: green for declined, red for consent the
        // banner pushed someone into, amber for half, grey for a free choice
        const KINDS = { ok: 'check-circle-fill', bad: 'exclamation-triangle-fill', half: 'exclamation-circle-fill', neutral: 'info-circle-fill' };
        const outcome = (kind, text) => el('div', { class: `dp-result dp-result-${kind}` }, [icon(KINDS[kind]), el('strong', { text })]);
        const link = (text, onClick) => {
            const b = el('button', { type: 'button', class: 'dp-link', text });
            b.addEventListener('click', onClick);
            return el('div', {}, [b]);
        };
        // a card with the made-up banner, and under it, outside the card, the
        // clicks so far and »Noch einmal«
        function setUp(title, run) {
            const box = el('div', { class: 'dp-banner' });
            const clicks = el('span', { class: 'dp-status', 'aria-live': 'polite' });
            const again = button('Noch einmal', 'btn-sm btn-outline-secondary', () => start());
            const figure = el('figure', { class: 'a11y-panel' }, [el('figcaption', { text: title }),
                el('div', { class: 'a11y-sample' }, [box]), el('div', { class: 'dp-meter' }, [clicks, again])]);
            let n = 0;
            const show = (done) => {
                clicks.replaceChildren(done ? `${done} nach ` : 'Bisher ', el('strong', { text: `${n} ${n === 1 ? 'Klick' : 'Klicks'}` }));
                again.hidden = !done;
            };
            const count = () => { n += 1; show(''); };
            const finish = (kind, text, word) => { box.replaceChildren(outcome(kind, text)); show(word); };
            function start() { n = 0; show(''); run(box, count, finish); }
            start();
            return figure;
        }

        const unfair = setUp('Unfair', (box, count, finish) => {
            const accept = () => { count(); finish('bad', 'Du hast allem zugestimmt.', 'Zugestimmt'); };
            // the second layer: everything already switched on, »Alle akzeptieren« big again
            function settings() {
                const toggles = [['Notwendig', true], ['Statistik', false], ['Marketing', false], ['Partner', false]];
                const list = el('div', { class: 'dp-toggles' }, toggles.map(([name, locked], i) => {
                    const id = `dp-toggle-${counter}-${i}`;
                    const input = el('input', { type: 'checkbox', class: 'form-check-input', role: 'switch', id });
                    input.checked = true;
                    if (locked) input.disabled = true;
                    input.addEventListener('change', count);
                    return el('div', { class: 'form-check form-switch' }, [input, el('label', { class: 'form-check-label', for: id, text: name })]);
                }));
                box.replaceChildren(el('p', { text: 'Deine Auswahl' }), list,
                    button('Alle akzeptieren', 'btn-success dp-big', accept),
                    link('Auswahl speichern', () => {
                        count();
                        const off = [...list.querySelectorAll('input:not(:disabled)')].every((x) => !x.checked);
                        if (off) finish('ok', 'Abgelehnt.', 'Abgelehnt');
                        else finish('half', 'Gespeichert, aber einiges ist noch an.', 'Gespeichert');
                    }));
            }
            box.replaceChildren(
                el('p', { text: 'Wir und unsere 847 Partner verwenden Cookies, um dir das beste Erlebnis zu bieten.' }),
                button('Alle akzeptieren', 'btn-success dp-big', accept),
                link('Einstellungen', () => { count(); settings(); }));
        });
        const fair = setUp('Fair', (box, count, finish) => {
            box.replaceChildren(
                el('p', { text: 'Dürfen wir mit einem Cookie zählen, welche Seiten du liest?' }),
                el('div', { class: 'dp-choices' }, [
                    button('Ablehnen', 'btn-outline-primary', () => { count(); finish('ok', 'Abgelehnt.', 'Abgelehnt'); }),
                    button('Zustimmen', 'btn-outline-primary', () => { count(); finish('bad', 'Zugestimmt.', 'Zugestimmt'); })]));
        });
        counter += 1;
        root.append(el('div', { class: 'a11y-panels' }, [unfair, fair]));
    }

    // ------------------------------------------------------------------
    // the newsletter box in double negation, and what it means
    function wordingDemo(root) {
        root.dataset.chip = 'Zum Ausprobieren: ankreuzen und bestellen';
        const id = `dp-wording-${++counter}`;
        const box = el('input', { type: 'checkbox', class: 'form-check-input', id });
        box.checked = true;
        const result = el('div', { 'aria-live': 'polite' });
        const order = button('Bestellen', 'btn-success', () => {
            result.replaceChildren(el('div', { class: `dp-result dp-result-${box.checked ? 'bad' : 'ok'}` }, [
                icon(box.checked ? 'envelope-fill' : 'envelope-slash'),
                el('strong', { text: box.checked ? 'Du bekommst ab jetzt Werbung per E-Mail.' : 'Du bekommst keine Werbung.' })]));
        });
        box.addEventListener('change', () => result.replaceChildren());
        const fairId = `dp-wording-fair-${counter}`;
        const fairBox = el('input', { type: 'checkbox', class: 'form-check-input', id: fairId });
        root.append(el('div', { class: 'a11y-panels' }, [
            panel('Unfair', [el('div', { class: 'form-check' }, [box, el('label', { class: 'form-check-label', for: id,
                text: 'Entfernen Sie das Häkchen nicht, wenn Sie keine Angebote nicht verpassen möchten.' })]), order, result]),
            panel('Fair', [el('div', { class: 'form-check' }, [fairBox, el('label', { class: 'form-check-label', for: fairId,
                text: 'Ja, schickt mir Angebote per E-Mail.' })]), el('p', { class: 'dp-note', text: 'Nichts ist vorausgewählt, und der Satz sagt geradeaus, was ein Häkchen bedeutet.' })]),
        ]));
    }

    // ------------------------------------------------------------------
    // a countdown that starts over after a reload
    function urgencyDemo(root) {
        root.dataset.chip = 'Zum Ausprobieren: neu laden';
        let left = 300;
        const clock = el('span', { class: 'dp-clock' });
        const show = () => { clock.textContent = `${String(Math.floor(left / 60)).padStart(2, '0')}:${String(left % 60).padStart(2, '0')}`; };
        show();
        const timer = setInterval(() => {
            if (!root.isConnected) { clearInterval(timer); return; }
            left = Math.max(0, left - 1);
            show();
        }, 1000);
        const reloads = el('p', { class: 'dp-note', 'aria-live': 'polite' });
        let count = 0;
        root.append(el('div', { class: 'dp-offer' }, [
            el('p', { class: 'dp-offer-title' }, [icon('lightning-charge-fill'), ' Nur heute: 30 % auf alle Kurse!']),
            el('p', {}, ['Das Angebot endet in ', clock]),
            button('Seite neu laden', 'btn-outline-secondary', () => {
                left = 300; show(); count += 1;
                reloads.textContent = `${count}-mal neu geladen: Die Zeit beginnt jedes Mal wieder bei 05:00. Das Angebot endet nie.`;
            }),
            reloads]));
    }

    // ------------------------------------------------------------------
    // a price that grows step by step at the till
    function costsDemo(root) {
        root.dataset.chip = 'Zum Ausprobieren: zur Kasse';
        const items = [['Konzertkarte', 3900], ['Servicegebühr', 490], ['Versand (für ein E-Ticket)', 250], ['Gebühr für die Zahlung', 195]];
        let step = 1;
        const list = el('ul', { class: 'dp-bill' });
        const total = el('p', { class: 'dp-total', 'aria-live': 'polite' });
        const next = el('button', { type: 'button', class: 'btn btn-sm btn-success' });
        function show() {
            list.replaceChildren(...items.slice(0, step).map(([name, cents], i) =>
                el('li', { class: i > 0 ? 'dp-added' : '' }, [el('span', { text: name }), el('span', { text: euro(cents) })])));
            const sum = items.slice(0, step).reduce((s, [, c]) => s + c, 0);
            total.replaceChildren('Zu zahlen: ', el('strong', { text: euro(sum) }),
                step > 1 ? ` (angezeigt waren ${euro(items[0][1])}, ${Math.round(100 * (sum / items[0][1] - 1))} % mehr)` : '');
            next.textContent = step < items.length ? 'Weiter zur Kasse' : 'Noch einmal';
        }
        next.addEventListener('click', () => { step = step < items.length ? step + 1 : 1; show(); });
        show();
        const all = items.reduce((s, [, c]) => s + c, 0);
        root.append(el('div', { class: 'a11y-panels' }, [
            panel('Unfair', [list, total, next]),
            panel('Fair', [el('ul', { class: 'dp-bill' }, [el('li', {}, [el('span', { text: 'Konzertkarte, alles inklusive' }), el('span', { text: euro(all) })])]),
                el('p', { class: 'dp-note', text: 'Der Preis, der am Anfang steht, ist der, den man am Ende zahlt.' })]),
        ]));
    }

    const DEMOS = { banner: bannerDemo, wording: wordingDemo, urgency: urgencyDemo, costs: costsDemo };
    const start = () => document.querySelectorAll('.dp-demo').forEach((root) => {
        if (root.dataset.ready) return;
        root.dataset.ready = '1';
        (DEMOS[root.dataset.demo] || (() => {}))(root);
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
