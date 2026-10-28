// Paint over a QR code and see whether it still reads (»Prüfziffern«,
// src/content/check-digits): the same code at the levels L, M, Q and H,
// a sticker (light), a felt pen (dark) and an eraser, one module or 3 × 3
// at a time. After every change jsQR (src/static/include/jsqr, Apache 2.0)
// reads the painted code as an image, the way an app reads a photo; beside
// it qr_code.js counts what the paint has turned: the codewords (data and
// correction apart) against what the level can repair, and the finder
// patterns, timing lines, alignment pattern and format information.
//
//   <script src='/include/jsqr/jsQR.js'></script>
//   <script src='/include/qr_code.js'></script>
//   <script src='/include/qr_damage.js'></script>
//   <div class='qr-damage' data-text='4260111020434' data-level='M'
//       data-l='1111111… …' data-m='…' data-q='…' data-h='…'></div>
//
// The matrices (rows of 0 and 1, separated by spaces) and the div come from
// src/scripts/qr-code.cjs. The paint stays when the level changes, so that
// one spot can be tried at every level. Mouse, touch (the code does not
// scroll the page while painting) and keyboard: Tab to the code, the arrow
// keys move a frame, space or Enter paints, Entf or Backspace erases,
// Strg+Z undoes.
(function () {
    'use strict';
    const SVG = 'http://www.w3.org/2000/svg';
    const MARGIN = 2;            // light modules around the code in the picture
    const NONE = 0;
    const STICKER = 1;
    const PEN = 2;
    const TOOLS = [
        { id: 'sticker', paint: STICKER, label: 'Aufkleber', icon: 'bi-sticky' },
        { id: 'pen', paint: PEN, label: 'Filzstift', icon: 'bi-pen' },
        { id: 'eraser', paint: NONE, label: 'Radierer', icon: 'bi-eraser' },
    ];
    // the stickers of the prediction on the page: row, column, height, width
    // (the same as STICKERS in src/scripts/qr-code.cjs)
    const PRESETS = { A: [1, 1, 5, 5], B: [9, 20, 5, 5] };

    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'text') node.textContent = v;
            else node.setAttribute(k, v);
        }
        for (const child of children) node.appendChild(typeof child === 'string' ? document.createTextNode(child) : child);
        return node;
    };
    const svg = (tag, attrs = {}) => {
        const node = document.createElementNS(SVG, tag);
        for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
        return node;
    };
    const icon = (name) => html('i', { class: `bi ${name}`, 'aria-hidden': 'true' });

    // what jsQR reads from a matrix drawn at 8 px a module with a quiet zone of 4
    function decode(m) {
        const px = 8;
        const quiet = 4;
        const w = (m.length + 2 * quiet) * px;
        const data = new Uint8ClampedArray(w * w * 4).fill(255);
        for (let r = 0; r < m.length; r++) {
            for (let c = 0; c < m.length; c++) {
                if (!m[r][c]) continue;
                for (let y = 0; y < px; y++) {
                    let i = (((r + quiet) * px + y) * w + (c + quiet) * px) * 4;
                    for (let x = 0; x < px; x++, i += 4) data[i] = data[i + 1] = data[i + 2] = 0;
                }
            }
        }
        const result = window.jsQR(data, w, w, { inversionAttempts: 'dontInvert' });
        return result ? result.data : null;
    }

    let count = 0;
    function mount(el) {
        const Q = window.QrCode;
        if (!Q || !window.jsQR) return;
        const id = `qr-damage-${++count}`;
        const codes = {};
        for (const level of Q.LEVELS) {
            const rows = el.dataset[level.toLowerCase()];
            if (rows) codes[level] = Q.parse(rows);
        }
        const levels = Q.LEVELS.filter((level) => codes[level]);
        if (!levels.length) return;
        const text = el.dataset.text || '';
        const n = codes[levels[0]].length;
        const lay = Q.layout((n - 17) / 4);
        let level = levels.includes(el.dataset.level) ? el.dataset.level : levels[0];
        let tool = TOOLS[0];
        let brush = 1;
        let showWords = false;
        let paint = Array.from({ length: n }, () => new Array(n).fill(NONE));
        const history = [];
        let cursor = [Math.floor(n / 2), Math.floor(n / 2)];
        let cursorShown = false;

        // the controls: level, tool, brush; presets, undo, clear; codewords
        const group = (name, label, items, checked, onPick) => {
            const box = html('div', { class: 'btn-group btn-group-sm', role: 'group', 'aria-label': label });
            items.forEach((item) => {
                const inputId = `${id}-${name}-${item.value}`;
                const input = html('input', { type: 'radio', class: 'btn-check', name: `${id}-${name}`, id: inputId, autocomplete: 'off' });
                if (item.value === checked) input.checked = true;
                input.addEventListener('change', () => { if (input.checked) onPick(item.value); });
                const children = item.icon ? [icon(item.icon), ' ', item.label] : [item.label];
                box.append(input, html('label', { class: 'btn btn-outline-secondary', for: inputId }, children));
            });
            return html('div', { class: 'qr-damage-group' }, [html('span', { class: 'qr-damage-label', text: label }), box]);
        };
        const levelGroup = group('level', 'Stufe', levels.map((l) => ({ value: l, label: l })), level, (value) => { level = value; draw(); });
        const toolGroup = group('tool', 'Werkzeug', TOOLS.map((t) => ({ value: t.id, label: t.label, icon: t.icon })), tool.id,
            (value) => { tool = TOOLS.find((t) => t.id === value); });
        const brushGroup = group('brush', 'Größe', [{ value: '1', label: '1 Modul' }, { value: '3', label: '3 × 3' }], '1',
            (value) => { brush = Number(value); });
        const button = (label, iconName, onClick, extra = {}) => {
            const b = html('button', Object.assign({ type: 'button', class: 'btn btn-sm btn-outline-secondary' }, extra),
                iconName ? [icon(iconName), ' ', label] : [label]);
            b.addEventListener('click', onClick);
            return b;
        };
        const presetButtons = Object.entries(PRESETS).map(([name, [r0, c0, h, w]]) => button(`Aufkleber ${name}`, 'bi-sticky', () => {
            remember();
            paint = paint.map((row) => row.map(() => NONE));
            for (let r = r0; r < r0 + h; r++) for (let c = c0; c < c0 + w; c++) paint[r][c] = STICKER;
            draw();
        }));
        const undo = button('Rückgängig', 'bi-arrow-counterclockwise', () => back(), { disabled: '' });
        const clear = button('Alles entfernen', 'bi-x-lg', () => {
            if (!paint.some((row) => row.some((v) => v !== NONE))) return;
            remember();
            paint = paint.map((row) => row.map(() => NONE));
            draw();
        });
        const wordsBox = html('input', { type: 'checkbox', class: 'form-check-input', id: `${id}-words` });
        wordsBox.addEventListener('change', () => { showWords = wordsBox.checked; draw(); });
        const picture = svg('svg', {
            class: 'qr-damage-picture',
            viewBox: `${-MARGIN} ${-MARGIN} ${n + 2 * MARGIN} ${n + 2 * MARGIN}`,
            tabindex: '0',
            role: 'application',
            'aria-label': 'QR-Code zum Bemalen: Pfeiltasten bewegen den Rahmen, Leertaste malt mit dem gewählten Werkzeug, Entf radiert, Strg+Z macht rückgängig',
        });
        const say = html('div', { class: 'qr-damage-say', 'aria-live': 'polite' });
        el.replaceChildren(
            html('div', { class: 'qr-damage-controls' }, [levelGroup, brushGroup, toolGroup]),
            html('div', { class: 'qr-damage-controls' }, [
                ...presetButtons, undo, clear,
                html('div', { class: 'form-check' }, [wordsBox, html('label', { class: 'form-check-label', for: `${id}-words`, text: 'Codewörter zeigen' })]),
            ]),
            html('div', { class: 'qr-damage-body' }, [picture, say]));

        function remember() {
            history.push(paint.map((row) => row.slice()));
            if (history.length > 100) history.shift();
            undo.disabled = false;
        }
        function back() {
            if (!history.length) return;
            paint = history.pop();
            undo.disabled = history.length === 0;
            draw();
        }
        // paint the brush around a module; true if anything changed
        function dab(r, c) {
            const half = brush === 3 ? 1 : 0;
            let changed = false;
            for (let y = r - half; y <= r + half; y++) {
                for (let x = c - half; x <= c + half; x++) {
                    if (y < 0 || x < 0 || y >= n || x >= n || paint[y][x] === tool.paint) continue;
                    paint[y][x] = tool.paint;
                    changed = true;
                }
            }
            return changed;
        }
        const effective = () => codes[level].map((row, r) => row.map((v, c) =>
            (paint[r][c] === STICKER ? 0 : (paint[r][c] === PEN ? 1 : v))));

        function draw() {
            const original = codes[level];
            const painted = effective();
            const damage = Q.damage(original, painted, level);
            const broken = new Set(damage.codewords);
            picture.replaceChildren();
            picture.append(svg('rect', { class: 'qr-damage-paper', x: -MARGIN, y: -MARGIN, width: n + 2 * MARGIN, height: n + 2 * MARGIN, rx: 0.6 }));
            let dark = '';
            let sticker = '';
            let pen = '';
            let hit = '';
            for (let r = 0; r < n; r++) {
                for (let c = 0; c < n; c++) {
                    const cell = `M${c} ${r}h1v1h-1z`;
                    if (paint[r][c] === STICKER) sticker += cell;
                    else if (paint[r][c] === PEN) pen += cell;
                    else if (original[r][c]) dark += cell;
                    if (showWords && lay.role[r][c] === 'data' && broken.has(lay.codeword[r][c])) hit += cell;
                }
            }
            picture.append(svg('path', { class: 'qr-damage-dark', d: dark }));
            // the damaged codewords red, under the paint, so that the paint keeps its colour
            if (showWords) picture.append(svg('path', { class: 'qr-damage-hit', d: hit }));
            picture.append(svg('path', { class: 'qr-damage-sticker', d: sticker }));
            picture.append(svg('path', { class: 'qr-damage-pen', d: pen }));
            if (showWords) {
                const key = (r, c) => (r < 0 || c < 0 || r >= n || c >= n || lay.role[r][c] !== 'data' || lay.codeword[r][c] < 0 ? -1 : lay.codeword[r][c]);
                let data = '';
                let ec = '';
                for (let r = 0; r < n; r++) {
                    for (let c = 0; c < n; c++) {
                        const k = key(r, c);
                        if (k < 0) continue;
                        let d = '';
                        if (key(r - 1, c) !== k) d += `M${c} ${r}h1`;
                        if (key(r + 1, c) !== k) d += `M${c} ${r + 1}h1`;
                        if (key(r, c - 1) !== k) d += `M${c} ${r}v1`;
                        if (key(r, c + 1) !== k) d += `M${c + 1} ${r}v1`;
                        if (k < damage.capacity.data) data += d;
                        else ec += d;
                    }
                }
                picture.append(svg('path', { class: 'qr-damage-edge-data', d: data }));
                picture.append(svg('path', { class: 'qr-damage-edge-ec', d: ec }));
            }
            if (cursorShown) {
                const half = brush === 3 ? 1 : 0;
                picture.append(svg('rect', {
                    class: 'qr-damage-cursor',
                    x: cursor[1] - half, y: cursor[0] - half, width: 2 * half + 1, height: 2 * half + 1,
                }));
            }
            report(painted, damage);
        }

        function report(painted, damage) {
            const read = decode(painted);
            const first = html('p', { class: 'qr-damage-verdict' });
            if (read === text) {
                first.classList.add('ok');
                first.append(icon('bi-check-circle-fill'), ' Gelesen: ', html('code', { text: read }));
            } else if (read === null) {
                first.classList.add('fail');
                first.append(icon('bi-x-circle-fill'), ' Nicht lesbar.');
            } else {
                first.classList.add('fail');
                first.append(icon('bi-exclamation-triangle-fill'), ' Falsch gelesen: ', html('code', { text: read }));
            }
            const cap = damage.capacity;
            const lines = [];
            if (damage.codewords.length) {
                lines.push(`Beschädigte Codewörter: ${damage.codewords.length} von ${cap.total} ` +
                    `(${damage.data} Daten, ${damage.ec} Korrektur). Stufe ${level} repariert bis zu ${cap.repairs}.`);
            } else {
                lines.push(`Kein Codewort beschädigt. Stufe ${level} repariert bis zu ${cap.repairs} von ${cap.total}.`);
            }
            const parts = [];
            if (damage.finder.length === 1) parts.push(`das Suchmuster ${damage.finder[0]}`);
            if (damage.finder.length > 1) parts.push(`${damage.finder.length} Suchmuster`);
            if (damage.timing) parts.push('eine Taktlinie');
            if (damage.alignment) parts.push('das Ausrichtungsmuster');
            if (damage.format.length === 2) parts.push('die Formatinformation, beide Male');
            else if (damage.format.length === 1) parts.push('die Formatinformation, einmal');
            if (parts.length) {
                const list = parts.length === 1 ? parts[0] : `${parts.slice(0, -1).join(', ')} und ${parts[parts.length - 1]}`;
                lines.push(`Außerdem beschädigt: ${list}.`);
            }
            say.replaceChildren(first, ...lines.map((line) => html('p', { text: line })));
        }

        // the module under a pointer, or null
        function cellAt(event) {
            const ctm = picture.getScreenCTM();
            if (!ctm) return null;
            const p = new DOMPoint(event.clientX, event.clientY).matrixTransform(ctm.inverse());
            const r = Math.floor(p.y);
            const c = Math.floor(p.x);
            return r >= 0 && c >= 0 && r < n && c < n ? [r, c] : null;
        }
        let painting = false;
        let stroke = null;
        picture.addEventListener('pointerdown', (event) => {
            if (event.button !== 0) return;
            const cell = cellAt(event);
            if (!cell) return;
            event.preventDefault();
            picture.setPointerCapture(event.pointerId);
            painting = true;
            stroke = paint.map((row) => row.slice());
            cursorShown = false;
            if (dab(...cell)) draw();
        });
        picture.addEventListener('pointermove', (event) => {
            if (!painting) return;
            const cell = cellAt(event);
            if (cell && dab(...cell)) draw();
        });
        const finish = () => {
            if (!painting) return;
            painting = false;
            if (stroke && stroke.some((row, r) => row.some((v, c) => v !== paint[r][c]))) {
                history.push(stroke);
                if (history.length > 100) history.shift();
                undo.disabled = false;
            }
            stroke = null;
        };
        picture.addEventListener('pointerup', finish);
        picture.addEventListener('pointercancel', finish);
        picture.addEventListener('keydown', (event) => {
            const moves = { ArrowUp: [-1, 0], ArrowDown: [1, 0], ArrowLeft: [0, -1], ArrowRight: [0, 1] };
            if (moves[event.key]) {
                const [dr, dc] = moves[event.key];
                cursor = [Math.min(n - 1, Math.max(0, cursor[0] + dr)), Math.min(n - 1, Math.max(0, cursor[1] + dc))];
                cursorShown = true;
                event.preventDefault();
                draw();
            } else if (event.key === ' ' || event.key === 'Enter' || event.key === 'Delete' || event.key === 'Backspace') {
                event.preventDefault();
                cursorShown = true;
                const before = paint.map((row) => row.slice());
                const saved = tool;
                if (event.key === 'Delete' || event.key === 'Backspace') tool = TOOLS[2];
                if (dab(...cursor)) {
                    history.push(before);
                    if (history.length > 100) history.shift();
                    undo.disabled = false;
                }
                tool = saved;
                draw();
            } else if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'z') {
                event.preventDefault();
                back();
            }
        });
        picture.addEventListener('focus', () => { if (!painting) { cursorShown = true; draw(); } });
        picture.addEventListener('blur', () => { cursorShown = false; draw(); });
        draw();
    }

    function mountAll() {
        document.querySelectorAll('.qr-damage').forEach(mount);
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', mountAll);
    else mountAll();
}());
