// Barrierefreiheit (Handbuch, reference/accessibility): three small demos
// that show what a rule is for, each beside its rule.
//
//   <script src='/include/a11y_demo.js'></script>
//   <div class='a11y-demo' data-demo='color'></div>     only colour, seen otherwise
//   <div class='a11y-demo' data-demo='contrast'></div>  a grey and its contrast
//   <div class='a11y-demo' data-demo='reader'></div>    what a screen reader says
//
// color: four results marked only by green and red dots beside the same with
// a sign and a word, and a field whose error is only a red frame beside one
// with a sign and a sentence; »So sehen« shows both as with red-green
// colour blindness (the matrices of Machado, Oliveira and Fernandes 2009,
// severity 1, applied in linear RGB as SVG filters) and in grey.
// contrast: text in a grey on white, the ratio as WCAG computes it
// (relative luminance), against 4.5 : 1 and 3 : 1.
// reader: one card built twice, with what a screen reader reads out for
// each, roughly (every screen reader says it a little differently).
// The demo that is bad on purpose has a description for screen readers.
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
    let counter = 0;

    // ------------------------------------------------------------------
    // colour blindness as SVG filters, once per page
    const MATRICES = {
        deut: '0.367322 0.860646 -0.227968 0 0 0.280085 0.672501 0.047413 0 0 -0.011820 0.042940 0.968881 0 0 0 0 0 1 0',
        prot: '0.152286 1.052583 -0.204868 0 0 0.114503 0.786281 0.099216 0 0 -0.003882 -0.048116 1.051998 0 0 0 0 0 1 0',
    };
    function ensureFilters() {
        if (document.getElementById('a11y-filters')) return;
        const ns = 'http://www.w3.org/2000/svg';
        const svg = document.createElementNS(ns, 'svg');
        svg.id = 'a11y-filters';
        svg.setAttribute('aria-hidden', 'true');
        svg.setAttribute('width', '0');
        svg.setAttribute('height', '0');
        svg.style.position = 'absolute';
        for (const [name, values] of Object.entries(MATRICES)) {
            const filter = document.createElementNS(ns, 'filter');
            filter.id = `a11y-${name}`;
            filter.setAttribute('color-interpolation-filters', 'linearRGB');
            const matrix = document.createElementNS(ns, 'feColorMatrix');
            matrix.setAttribute('type', 'matrix');
            matrix.setAttribute('values', values);
            filter.append(matrix);
            svg.append(filter);
        }
        document.body.append(svg);
    }
    const VIEWS = [['normal', 'normal', ''], ['deut', 'Rot-Grün-Schwäche (grün)', 'url(#a11y-deut)'],
        ['prot', 'Rot-Grün-Schwäche (rot)', 'url(#a11y-prot)'], ['grey', 'in Grau', 'grayscale(1)']];

    function choice(name, label, items, onPick) {
        const box = el('div', { class: 'btn-group btn-group-sm a11y-choice', role: 'group', 'aria-label': label });
        items.forEach(([value, text], i) => {
            const id = `${name}-${value}`;
            const input = el('input', { type: 'radio', class: 'btn-check', name, id, autocomplete: 'off' });
            if (i === 0) input.checked = true;
            input.addEventListener('change', () => { if (input.checked) onPick(value); });
            box.append(input, el('label', { class: 'btn btn-outline-secondary', for: id, text }));
        });
        return el('div', { class: 'a11y-controls' }, [el('span', { class: 'a11y-label', text: label }), box]);
    }

    function colorDemo(root) {
        ensureFilters();
        root.dataset.chip = 'Zum Ausprobieren: mit anderen Augen sehen';
        const results = [['Aufgabe 1', true], ['Aufgabe 2', false], ['Aufgabe 3', false], ['Aufgabe 4', true]];
        const onlyList = el('ul', { class: 'a11y-results' }, results.map(([name, ok]) =>
            el('li', {}, [el('span', { class: `a11y-dot ${ok ? 'a11y-ok' : 'a11y-bad'}` }), name])));
        const bothList = el('ul', { class: 'a11y-results' }, results.map(([name, ok]) =>
            el('li', {}, [el('span', { class: ok ? 'a11y-ok-text' : 'a11y-bad-text' }, [icon(ok ? 'check-circle-fill' : 'x-circle-fill')]),
                `${name}: `, el('strong', { text: ok ? 'gelöst' : 'falsch' })])));
        const field = (good) => el('div', { class: 'a11y-field' }, [
            el('span', { class: 'a11y-field-label', text: 'Dein Name' }),
            el('span', { class: 'a11y-input a11y-input-error' }),
            good ? el('span', { class: 'a11y-bad-text a11y-field-error' }, [icon('exclamation-triangle-fill'), ' Bitte gib deinen Namen ein.']) : '',
        ]);
        const only = el('figure', { class: 'a11y-panel' }, [
            el('figcaption', { text: 'Nur mit Farbe' }),
            el('div', { class: 'a11y-sample', role: 'img',
                'aria-label': 'Beispiel ohne Wörter: vier Aufgaben, jede nur mit einem grünen oder roten Punkt markiert, und ein Feld, dessen Fehler nur ein roter Rahmen zeigt' },
            [onlyList, field(false)])]);
        const both = el('figure', { class: 'a11y-panel' }, [
            el('figcaption', { text: 'Mit Zeichen und Wörtern' }),
            el('div', { class: 'a11y-sample' }, [bothList, field(true)])]);
        const panels = el('div', { class: 'a11y-panels' }, [only, both]);
        root.append(choice(`a11y-view-${++counter}`, 'So sehen', VIEWS.map(([v, t]) => [v, t]), (value) => {
            panels.style.filter = VIEWS.find(([v]) => v === value)[2];
        }), panels);
    }

    // ------------------------------------------------------------------
    // contrast after WCAG: relative luminance of each colour, (L1 + 0.05) / (L2 + 0.05)
    const channel = (v) => { const c = v / 255; return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4; };
    const luminance = (r, g, b) => 0.2126 * channel(r) + 0.7152 * channel(g) + 0.0722 * channel(b);
    const de = (n) => n.toLocaleString('de-DE', { minimumFractionDigits: 1, maximumFractionDigits: 1 });

    function contrastDemo(root) {
        root.dataset.chip = 'Zum Ausprobieren: Grau wählen';
        const id = `a11y-grey-${++counter}`;
        const slider = el('input', { type: 'range', class: 'form-range a11y-slider', id, min: 0, max: 230, step: 1, value: 160 });
        const value = el('span', { class: 'a11y-value' });
        const sample = el('div', { class: 'a11y-contrast-sample' }, [
            el('p', { class: 'a11y-small', text: 'Normaler Text: Die Anmeldung schließt am Freitag.' }),
            el('p', { class: 'a11y-large', text: 'Großer Text: Hacker-AG' })]);
        const verdict = el('ul', { class: 'a11y-verdict', 'aria-live': 'polite' });
        function update() {
            const g = Number(slider.value);
            const hex = `#${g.toString(16).padStart(2, '0').repeat(3)}`;
            sample.style.color = hex;
            const ratio = (1 + 0.05) / (luminance(g, g, g) + 0.05);
            value.textContent = `${hex} auf Weiß: ${de(ratio)} : 1`;
            const row = (ok, text) => el('li', { class: ok ? 'a11y-ok-text' : 'a11y-bad-text' },
                [icon(ok ? 'check-circle-fill' : 'x-circle-fill'), ` ${text}: ${ok ? 'reicht' : 'zu wenig'}`]);
            verdict.replaceChildren(row(ratio >= 4.5, 'normaler Text braucht 4,5 : 1'), row(ratio >= 3, 'großer Text braucht 3 : 1'));
        }
        slider.addEventListener('input', update);
        root.append(el('div', { class: 'a11y-controls' }, [el('label', { class: 'a11y-label', for: id, text: 'Grau des Texts' }), slider, value]),
            sample, verdict);
        update();
    }

    // ------------------------------------------------------------------
    // one card built twice, and what a screen reader reads out
    function readerDemo(root) {
        root.dataset.chip = 'Zum Vergleichen: was ein Screenreader vorliest';
        const card = (good) => el('div', { class: 'a11y-card' }, [
            el('img', good ? { src: '/images/rocket.png', alt: 'Eine Rakete beim Start' } : { src: '/images/rocket.png', alt: '', 'data-no-alt': '' }),
            el(good ? 'h3' : 'div', { class: 'a11y-card-title', text: 'Hacker-AG' }),
            good ? el('button', { type: 'button', class: 'btn btn-sm btn-success' }, ['Zur Anmeldung ', icon('arrow-right')])
                : el('div', { class: 'a11y-fake-button', 'aria-hidden': 'true' }, [icon('arrow-right')]),
        ]);
        // a box of its own, not <pre>: the page would give that a copy button and inline-code colours
        const code = (text) => el('div', { class: 'a11y-code', text });
        const said = (lines) => el('ol', { class: 'a11y-said' }, lines.map((l) => el('li', { text: l })));
        const BAD = '<img src="rocket.png">\n<div class="title">Hacker-AG</div>\n<div onclick="weiter()">→</div>';
        const GOOD = '<img src="rocket.png" alt="Eine Rakete beim Start">\n<h3>Hacker-AG</h3>\n<button>Zur Anmeldung →</button>';
        const panel = (title, good) => el('figure', { class: 'a11y-panel' }, [
            el('figcaption', { text: title }),
            el('div', { class: 'a11y-sample', inert: '' }, [card(good)]),
            code(good ? GOOD : BAD),
            el('p', { class: 'a11y-said-title', text: 'Ein Screenreader liest etwa:' }),
            said(good ? ['»Grafik: Eine Rakete beim Start«', '»Überschrift Ebene 3: Hacker-AG«', '»Schalter: Zur Anmeldung«']
                : ['»Grafik: rocket«, der Name der Datei', '»Hacker-AG«, als wäre es irgendein Text', 'den Pfeil gar nicht: Mit der Tastatur kommt man nicht hin, und niemand erfährt, dass man ihn anklicken kann']),
        ]);
        root.append(el('div', { class: 'a11y-panels' }, [panel('So nicht', false), panel('So', true)]));
    }

    const DEMOS = { color: colorDemo, contrast: contrastDemo, reader: readerDemo };
    const start = () => document.querySelectorAll('.a11y-demo').forEach((root) => {
        if (root.dataset.ready) return;
        root.dataset.ready = '1';
        (DEMOS[root.dataset.demo] || (() => {}))(root);
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
