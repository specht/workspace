// A drawing pad for »Ein Neuron lernt« (src/content/perzeptron): draw a digit
// with a smooth pen, see it turned into 32 × 32 dots, as the digits in
// digits-train.csv were (the drawing scaled to fill the dots and centred),
// and the 64 values the neuron gets (how many of the 16 dots in each square
// of 4 × 4 are drawn, 0 to 16, as in the UCI data); then what a neuron says
// whose weights the student pastes in: the 65 numbers learn.rb or learn.py
// prints, 64 weights and the bias. It decides nothing of a task: the
// weights are the student's own.
//
//   <div class='neuron-pad' data-neuron-pad></div>
(function () {
    'use strict';

    const SIZE = 320;     // the drawing, in pixels of its canvas
    const PEN = 22;       // the pen's width in those pixels
    const DOTS = 32;
    const CELLS = 8;
    const BLOCK = DOTS / CELLS;
    const INK = 0.3;      // a dot is drawn when this share of its area is ink

    function el(tag, className, parent, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        if (parent) parent.appendChild(node);
        return node;
    }

    function setUp(box) {
        let weights = null;
        let bias = 0;

        box.innerHTML = '';
        const row = el('div', 'neuron-pad-row', box);

        const drawing = el('div', 'neuron-pad-drawing', row);
        const canvas = el('canvas', 'neuron-pad-canvas', drawing);
        canvas.width = canvas.height = SIZE;
        canvas.setAttribute('role', 'img');
        canvas.setAttribute('aria-label', 'Zeichenfläche; mit der Maus, einem Stift oder dem Finger zeichnest du eine Ziffer.');
        const clear = el('button', 'btn btn-sm btn-outline-secondary neuron-pad-clear', drawing);
        clear.type = 'button';
        clear.innerHTML = "<i class='bi bi-eraser' aria-hidden='true'></i>Löschen";

        const side = el('div', 'neuron-pad-side', row);
        const dotsFigure = el('figure', 'neuron-pad-figure', side);
        const dotsCanvas = el('canvas', 'neuron-pad-dots', dotsFigure);
        dotsCanvas.width = dotsCanvas.height = DOTS * 4;
        dotsCanvas.setAttribute('aria-hidden', 'true');
        el('figcaption', null, dotsFigure, '32 × 32 Punkte');
        const valuesFigure = el('figure', 'neuron-pad-figure', side);
        const values = el('div', 'neuron-pad-grid', valuesFigure);
        values.setAttribute('aria-hidden', 'true');
        const cells = [];
        for (let i = 0; i < CELLS * CELLS; i++) cells.push(el('span', null, values));
        el('figcaption', null, valuesFigure, 'Die 64 Werte, 0 bis 16');
        const weightsFigure = el('figure', 'neuron-pad-figure', side);
        const picture = el('div', 'neuron-pad-grid neuron-pad-weights', weightsFigure);
        picture.setAttribute('aria-hidden', 'true');
        const weightCells = [];
        for (let i = 0; i < CELLS * CELLS; i++) weightCells.push(el('span', null, picture));
        el('figcaption', null, weightsFigure, 'Die Gewichte: blau dafür, orange dagegen');

        const label = el('label', 'form-label neuron-pad-label', box, 'Die 65 Zahlen, die dein Programm ausgibt: 64 Gewichte und der Bias');
        const input = el('textarea', 'form-control neuron-pad-input', box);
        input.rows = 3;
        input.spellcheck = false;
        input.id = 'neuron-pad-input-' + Math.random().toString(36).slice(2, 8);
        label.htmlFor = input.id;
        input.placeholder = '0.0 -3.0 12.0 … und am Ende der Bias';
        // an example neuron (data-example: the 65 numbers the page's own
        // solution learns), so that the pad can be tried without a program
        if (box.dataset.example) {
            const example = el('button', 'btn btn-sm btn-outline-secondary neuron-pad-example', box);
            example.type = 'button';
            example.innerHTML = "<i class='bi bi-lightning' aria-hidden='true'></i>Beispiel: ein Neuron für 0 gegen 1";
            example.addEventListener('click', () => { input.value = box.dataset.example; readWeights(); });
        }
        const verdict = el('p', 'neuron-pad-verdict', box);
        verdict.setAttribute('aria-live', 'polite');

        const g = canvas.getContext('2d', { willReadFrequently: true });
        function blank() {
            g.fillStyle = '#ffffff';
            g.fillRect(0, 0, SIZE, SIZE);
        }

        // The drawing as 32 × 32 dots: the part with ink scaled to fill
        // them, keeping its proportions, and centred; a dot is drawn when
        // at least INK of its area is ink.
        function dots() {
            const data = g.getImageData(0, 0, SIZE, SIZE).data;
            const ink = (x, y) => data[(y * SIZE + x) * 4] < 128;
            let left = SIZE, top = SIZE, right = -1, bottom = -1;
            for (let y = 0; y < SIZE; y++) {
                for (let x = 0; x < SIZE; x++) {
                    if (!ink(x, y)) continue;
                    if (x < left) left = x;
                    if (x > right) right = x;
                    if (y < top) top = y;
                    if (y > bottom) bottom = y;
                }
            }
            const out = new Uint8Array(DOTS * DOTS);
            if (right < 0) return out;
            const side = Math.max(right - left + 1, bottom - top + 1);
            const scale = side / DOTS;
            const x0 = left - (side - (right - left + 1)) / 2;
            const y0 = top - (side - (bottom - top + 1)) / 2;
            for (let dy = 0; dy < DOTS; dy++) {
                for (let dx = 0; dx < DOTS; dx++) {
                    // the source area of this dot, sampled on a small grid
                    let hits = 0;
                    let samples = 0;
                    for (let sy = 0; sy < 4; sy++) {
                        for (let sx = 0; sx < 4; sx++) {
                            const x = Math.floor(x0 + (dx + (sx + 0.5) / 4) * scale);
                            const y = Math.floor(y0 + (dy + (sy + 0.5) / 4) * scale);
                            samples += 1;
                            if (x >= 0 && y >= 0 && x < SIZE && y < SIZE && ink(x, y)) hits += 1;
                        }
                    }
                    out[dy * DOTS + dx] = hits / samples >= INK ? 1 : 0;
                }
            }
            return out;
        }

        function update() {
            const bitmap = dots();
            const d = dotsCanvas.getContext('2d');
            const s = dotsCanvas.width / DOTS;
            d.fillStyle = '#ffffff';
            d.fillRect(0, 0, dotsCanvas.width, dotsCanvas.height);
            d.fillStyle = '#1f2937';
            for (let y = 0; y < DOTS; y++) {
                for (let x = 0; x < DOTS; x++) {
                    if (bitmap[y * DOTS + x]) d.fillRect(x * s, y * s, s, s);
                }
            }
            const features = new Array(CELLS * CELLS).fill(0);
            for (let y = 0; y < DOTS; y++) {
                for (let x = 0; x < DOTS; x++) {
                    if (bitmap[y * DOTS + x]) features[Math.floor(y / BLOCK) * CELLS + Math.floor(x / BLOCK)] += 1;
                }
            }
            features.forEach((v, i) => {
                cells[i].style.setProperty('--level', v / 16);
                cells[i].textContent = v;
            });
            decide(features);
        }

        function decide(features) {
            if (!weights) {
                if (!input.classList.contains('is-invalid')) verdict.textContent = 'Noch kein Neuron: Füge die 65 Zahlen deines Programms ein, oder nimm das Beispiel.';
                return;
            }
            if (features.every((v) => v === 0)) {
                verdict.textContent = 'Zeichne eine Ziffer.';
                return;
            }
            let sum = bias;
            for (let i = 0; i < features.length; i++) sum += weights[i] * features[i];
            verdict.innerHTML = `Summe ${sum.toFixed(1)}: Das Neuron sagt <strong>${sum > 0 ? 1 : 0}</strong>, ${sum > 0 ? 'die zweite' : 'die erste'} der beiden Ziffern, aus denen es gelernt hat.`;
        }

        function readWeights() {
            const numbers = input.value.replace(/[\[\](),]/g, ' ').trim().split(/\s+/).filter(Boolean).map(Number);
            input.classList.remove('is-invalid');
            weights = null;
            if (input.value.trim() && (numbers.length !== CELLS * CELLS + 1 || numbers.some((n) => !Number.isFinite(n)))) {
                input.classList.add('is-invalid');
                verdict.textContent = `Das sind ${numbers.length} Zahlen; gebraucht werden 65, die 64 Gewichte und am Ende der Bias.`;
            } else if (input.value.trim()) {
                weights = numbers.slice(0, CELLS * CELLS);
                bias = numbers[CELLS * CELLS];
            }
            const top = weights ? Math.max(...weights.map(Math.abs), 1e-9) : 1;
            picture.classList.toggle('neuron-pad-empty', !weights);
            weightCells.forEach((c, i) => {
                if (!weights) { c.className = ''; c.style.removeProperty('--level'); return; }
                c.className = weights[i] > 0 ? 'positive' : 'negative';
                c.style.setProperty('--level', Math.abs(weights[i]) / top);
            });
            update();
        }

        // drawing: a round pen, with the mouse, a stylus or a finger
        let last = null;
        function point(event) {
            const rect = canvas.getBoundingClientRect();
            return [(event.clientX - rect.left) / rect.width * SIZE, (event.clientY - rect.top) / rect.height * SIZE];
        }
        function stroke(to) {
            g.strokeStyle = '#000000';
            g.lineWidth = PEN;
            g.lineCap = 'round';
            g.lineJoin = 'round';
            g.beginPath();
            g.moveTo(...(last || to));
            g.lineTo(...to);
            g.stroke();
            last = to;
        }
        canvas.addEventListener('pointerdown', (event) => {
            canvas.setPointerCapture(event.pointerId);
            last = null;
            stroke(point(event));
            update();
        });
        canvas.addEventListener('pointermove', (event) => {
            if (last === null) return;
            stroke(point(event));
            update();
        });
        const lift = () => { last = null; };
        canvas.addEventListener('pointerup', lift);
        canvas.addEventListener('pointercancel', lift);
        clear.addEventListener('click', () => { blank(); update(); });
        input.addEventListener('input', readWeights);

        blank();
        readWeights();
    }

    function all() { document.querySelectorAll('[data-neuron-pad]').forEach(setUp); }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', all);
    else all();
})();
