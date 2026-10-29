// How fast effort grows (content/laufzeit, »Wie schnell Dinge wachsen«):
// <div class='growth-widget'></div> becomes a slider for n, from 1 to a
// million, and a table of the steps that 1, log n, n, n log n, n², 2ⁿ and
// n! take, with the time they need at a hundred million steps a second and
// a bar on a logarithmic scale. Numbers too large for JavaScript are
// computed as their logarithm (Stirling's formula for n!).
(function () {
    'use strict';

    const PER_SECOND = 1e8;
    const LOG10E = Math.LOG10E;
    // the age of the universe, about 13.8 billion years, in seconds
    const UNIVERSE = 13.8e9 * 365.25 * 24 * 3600;
    const ROWS = [
        { label: '1', log: () => 0 },
        { label: 'log n', log: (n) => (n > 1 ? Math.log10(Math.log2(n)) : -Infinity) },
        { label: 'n', log: (n) => Math.log10(n) },
        { label: 'n log n', log: (n) => (n > 1 ? Math.log10(n * Math.log2(n)) : -Infinity) },
        { label: 'n²', log: (n) => 2 * Math.log10(n) },
        { label: '2ⁿ', log: (n) => n * Math.log10(2) },
        { label: 'n!', log: (n) => (n < 2 ? 0 : n * Math.log10(n) - n * LOG10E + 0.5 * Math.log10(2 * Math.PI * n)) },
    ];
    const SUPER = { 0: '⁰', 1: '¹', 2: '²', 3: '³', 4: '⁴', 5: '⁵', 6: '⁶', 7: '⁷', 8: '⁸', 9: '⁹' };

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    // 1 234 567, or 10¹²³ once it gets too long to read
    function steps(log) {
        if (log === -Infinity) return '0';
        if (log < 12) return Math.round(Math.pow(10, log)).toLocaleString('de-DE');
        const exponent = Math.floor(log);
        return '10' + String(exponent).split('').map((d) => SUPER[d]).join('');
    }

    function plural(value, one, many) {
        const rounded = value >= 10 ? Math.round(value) : Math.round(value * 10) / 10;
        return rounded.toLocaleString('de-DE') + ' ' + (rounded === 1 ? one : many);
    }

    // the time at PER_SECOND steps a second, said as a person would
    function time(log) {
        if (log === -Infinity) return 'nichts';
        const logSeconds = log - Math.log10(PER_SECOND);
        if (logSeconds > Math.log10(UNIVERSE)) return 'länger, als es das Weltall gibt';
        const s = Math.pow(10, logSeconds);
        if (s < 1e-6) return 'unter einer Mikrosekunde';
        if (s < 1e-3) return plural(s * 1e6, 'Mikrosekunde', 'Mikrosekunden');
        if (s < 1) return plural(s * 1e3, 'Millisekunde', 'Millisekunden');
        if (s < 60) return plural(s, 'Sekunde', 'Sekunden');
        if (s < 3600) return plural(s / 60, 'Minute', 'Minuten');
        if (s < 86400) return plural(s / 3600, 'Stunde', 'Stunden');
        if (s < 365.25 * 86400) return plural(s / 86400, 'Tag', 'Tage');
        const years = s / (365.25 * 86400);
        if (years < 1e6) return plural(years, 'Jahr', 'Jahre');
        if (years < 1e9) return plural(years / 1e6, 'Million Jahre', 'Millionen Jahre');
        return plural(years / 1e9, 'Milliarde Jahre', 'Milliarden Jahre');
    }

    // the largest n whose steps fit into one second
    function oneSecond(row) {
        const limit = Math.log10(PER_SECOND);
        if (row.log(1e12) <= limit) return 'jedes';
        let lo = 1;
        let hi = 1e12;
        while (hi - lo > 1) {
            const mid = Math.floor((lo + hi) / 2);
            if (row.log(mid) <= limit) lo = mid;
            else hi = mid;
        }
        return lo.toLocaleString('de-DE');
    }

    function mount(box) {
        const head = el('div', 'growth-head');
        const label = el('label', 'growth-label');
        const value = el('output', 'growth-n');
        const slider = el('input', 'form-range growth-slider');
        slider.type = 'range';
        slider.min = '0';
        slider.max = '600';
        slider.step = '1';
        slider.value = '200';
        const id = 'growth-' + Math.random().toString(36).slice(2, 8);
        slider.id = id;
        label.htmlFor = id;
        label.append('n = ', value);
        const quick = el('div', 'btn-group btn-group-sm growth-quick');
        quick.setAttribute('role', 'group');
        quick.setAttribute('aria-label', 'n wählen');
        [10, 20, 100, 1000, 1000000].forEach((n) => {
            const b = el('button', 'btn btn-outline-secondary', n.toLocaleString('de-DE'));
            b.type = 'button';
            b.addEventListener('click', () => {
                slider.value = String(Math.round(Math.log10(n) * 100));
                show();
            });
            quick.appendChild(b);
        });
        head.append(label, slider, quick);

        // fixed column widths (CSS), so that nothing moves while n changes
        const table = el('table', 'table growth-table');
        const columns = el('colgroup');
        ['name', 'steps', 'time', 'bar', 'max'].forEach((c) => columns.appendChild(el('col', 'growth-col-' + c)));
        table.appendChild(columns);
        const thead = el('thead');
        const tr = el('tr');
        ['wächst wie', 'Schritte', 'Zeit', '', 'in einer Sekunde bis n ='].forEach((t, i) => tr.appendChild(el('th', i === 1 || i === 4 ? 'growth-right' : '', t)));
        thead.appendChild(tr);
        const tbody = el('tbody');
        const cells = ROWS.map((row) => {
            const line = el('tr');
            const bar = el('span', 'growth-bar');
            const barCell = el('td', 'growth-bar-cell');
            barCell.appendChild(bar);
            const c = { steps: el('td', 'growth-steps'), time: el('td', 'growth-time'), bar };
            line.append(el('td', 'growth-name', row.label), c.steps, c.time, barCell, el('td', 'growth-max', oneSecond(row)));
            tbody.appendChild(line);
            return c;
        });
        table.append(thead, tbody);
        const wrap = el('div', 'table-container');
        wrap.appendChild(table);
        box.append(head, wrap, el('p', 'growth-note', 'Angenommen ist ein Rechner, der hundert Millionen Schritte in der Sekunde schafft. Die Balken wachsen mit der Zahl der Stellen der Schritte.'));

        function show() {
            const n = Math.max(1, Math.round(Math.pow(10, Number(slider.value) / 100)));
            value.textContent = n.toLocaleString('de-DE');
            ROWS.forEach((row, i) => {
                const log = row.log(n);
                cells[i].steps.textContent = steps(log);
                cells[i].time.textContent = time(log);
                // a bar for up to 10³⁰ steps, the full width beyond
                const width = log === -Infinity ? 0 : Math.max(1, Math.min(100, (log + 1) / 31 * 100));
                cells[i].bar.style.width = width + '%';
                cells[i].bar.classList.toggle('growth-too-long', log - Math.log10(PER_SECOND) > Math.log10(3600 * 24 * 365.25));
            });
        }
        slider.addEventListener('input', show);
        show();
    }

    function start() {
        document.querySelectorAll('.growth-widget').forEach((box) => {
            if (box.dataset.ready) return;
            box.dataset.ready = '1';
            mount(box);
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
