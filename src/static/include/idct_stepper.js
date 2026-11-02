// The iDCT stepper: an 8 × 8 block of a JPEG put together from its
// coefficients, one at a time in zig-zag order. Every coefficient adds its
// basis picture, a pattern of cosines, scaled by its value; after the last
// one the block is exactly what the decoder computes. ← and → step it
// (stepper_keys.js).
//
// <div class='idct-stepper' data-stepper data-coef='-600 27 9 …'></div>
// with the 64 dequantised coefficients in natural order (row by row: v * 8 + u).
// With data-src='/jpeg/44-baseline.jpg' (and jpeg_image.js on the page) a
// picker above it chooses any block of the photo: an MCU on the photo, then
// one of its blocks; data-coef is the block shown first. A »jpeg-block«
// event on the document ({ comp, index, mx, my }) chooses one from outside.

(function (root) {
    'use strict';

    const ZIGZAG = [
        0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5,
        12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28,
        35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
        58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
    ];
    const C = (u) => u === 0 ? Math.SQRT1_2 : 1;
    // basis(u, v)[y * 8 + x]: what one unit of the coefficient (u, v) adds to the pixel (x, y)
    const basis = (u, v) => Array.from({ length: 64 }, (_, i) => {
        const x = i % 8, y = Math.floor(i / 8);
        return C(u) * C(v) / 4 * Math.cos((2 * x + 1) * u * Math.PI / 16) * Math.cos((2 * y + 1) * v * Math.PI / 16);
    });
    const pixels = (sum) => sum.map((s) => Math.max(0, Math.min(255, Math.round(s + 128))));

    // The steps: one for every coefficient that is not zero, in zig-zag
    // order, and always one for the DC coefficient, even when it is 0.
    function steps(coef) {
        const sum = new Array(64).fill(0);
        const out = [];
        ZIGZAG.forEach((natural, k) => {
            const value = coef[natural];
            if (!value && k > 0) return;
            const u = natural % 8, v = Math.floor(natural / 8);
            const b = basis(u, v);
            for (let i = 0; i < 64; i++) sum[i] += value * b[i];
            out.push({ k, natural, u, v, value, basis: b, block: pixels(sum), added: out.map((s) => s.natural).concat(natural) });
        });
        return out;
    }

    // --- The widget -----------------------------------------------------------

    const grey = (value) => `rgb(${value}, ${value}, ${value})`;

    function describe(step, index, total) {
        const { u, v, value } = step;
        if (u === 0 && v === 0) {
            return `Zuerst nur der DC-Koeffizient ${value}. Sein Basisbild ist überall gleich: Er gibt dem ganzen Block dieselbe Helligkeit, ${value} / 8 + 128 = ${Math.round(value / 8 + 128)}.`;
        }
        const waves = (n, dative) => n === 1 ? (dative ? 'einer halben Welle' : 'eine halbe Welle') : `${n} ${dative ? 'halben' : 'halbe'} Wellen`;
        const where = v === 0 ? `ein Verlauf von links nach rechts, mit ${waves(u, true)}` :
            u === 0 ? `ein Verlauf von oben nach unten, mit ${waves(v, true)}` :
            `ein Muster aus beiden Richtungen, ${waves(u, false)} nach rechts und ${waves(v, false)} nach unten`;
        const sign = value < 0 ? ' Weil der Koeffizient negativ ist, kommt das Muster umgekehrt dazu: hell wird dunkel.' : '';
        const last = index === total - 1 ? ' Das war der letzte Koeffizient, der nicht 0 ist: Der Block ist fertig, genau wie im Decoder.' : '';
        return `Koeffizient ${index + 1} von ${total}: ${value} an der Stelle u = ${u}, v = ${v} (Platz ${step.k} im Zick-Zack). Sein Basisbild ist ${where}.${sign}${last}`;
    }

    function cells(values, render) {
        return values.map((value, i) => render(value, i)).join('');
    }

    function mount(element) {
        let coef = element.dataset.coef.trim().split(/\s+/).map(Number);
        let list = steps(coef);
        element.innerHTML = `
            ${element.dataset.src ? `<div class='idct-choose'><div class='idct-pick'></div><div class='idct-blocks' role='group' aria-label='Block der MCU'></div></div>` : ''}
            <div class='idct-controls'>
                <button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
                <button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
                <span class='idct-count'></span>
            </div>
            <div class='idct-panels'>
                <figure><div class='idct-grid idct-coef'></div><figcaption>Koeffizienten</figcaption></figure>
                <figure><div class='idct-grid idct-basis'></div><figcaption class='idct-basis-caption'></figcaption></figure>
                <figure><div class='idct-grid idct-block'></div><figcaption>Der Block bisher, mit verstärktem Kontrast</figcaption></figure>
                <figure><div class='idct-grid idct-numbers'></div><figcaption>Pixelwerte</figcaption></figure>
            </div>
            <div class='idct-say' aria-live='polite'></div>`;
        const say = element.querySelector('.idct-say');
        const count = element.querySelector('.idct-count');
        const go = (name) => element.querySelector(`[data-go='${name}']`);
        let current = 0;
        // The grey picture of the block stretched to the range of the finished
        // block, so that small changes show; the numbers stay as they are.
        let low = 0, high = 255;
        const range = () => {
            const final = list[list.length - 1].block;
            low = Math.min(...final);
            high = Math.max(...final);
        };
        range();
        const stretch = (p) => Math.max(0, Math.min(255, Math.round((p - low) / Math.max(1, high - low) * 235 + 10)));

        function draw() {
            const step = list[current];
            const added = new Set(step.added);
            element.querySelector('.idct-coef').innerHTML = cells(coef, (value, i) => {
                const cls = i === step.natural ? 'now' : added.has(i) ? 'done' : value ? 'todo' : 'zero';
                return `<span class='${cls}'>${value}</span>`;
            });
            // The basis picture: −1 … 1 of one unit, shown as dark … light
            const max = Math.max(...step.basis.map(Math.abs)) || 1;
            element.querySelector('.idct-basis').innerHTML = cells(step.basis, (b) => {
                const shade = Math.round(128 + 127 * (step.value < 0 ? -b : b) / max);
                return `<span style='background: ${grey(shade)}'></span>`;
            });
            element.querySelector('.idct-basis-caption').textContent = `Basisbild u = ${step.u}, v = ${step.v}, mal ${step.value}`;
            element.querySelector('.idct-block').innerHTML = cells(step.block, (p) => `<span style='background: ${grey(stretch(p))}'></span>`);
            element.querySelector('.idct-numbers').innerHTML = cells(step.block, (p, i) => {
                const before = current > 0 ? list[current - 1].block[i] : null;
                return `<span class='${before !== null && before !== p ? 'changed' : ''}'>${p}</span>`;
            });
            say.textContent = describe(step, current, list.length);
            count.textContent = `Schritt ${current + 1} von ${list.length}`;
            go('first').disabled = go('prev').disabled = current <= 0;
            go('next').disabled = go('last').disabled = current >= list.length - 1;
            // for the timeline (stepper_keys.js)
            element.dataset.step = current;
            element.dataset.steps = list.length;
        }

        const to = (i) => { current = Math.max(0, Math.min(list.length - 1, i)); draw(); };
        element.stepperGo = to;
        go('first').addEventListener('click', () => to(0));
        go('prev').addEventListener('click', () => to(current - 1));
        go('next').addEventListener('click', () => to(current + 1));
        go('last').addEventListener('click', () => to(list.length - 1));
        draw();
        if (element.dataset.src && root.JpegImage) choose(element, (c) => { coef = c; list = steps(coef); range(); to(0); });
    }

    // The picker above the stepper: the MCU on the photo, then one of its
    // blocks, in the order of the file. Starts at the first block of the
    // first MCU, which data-coef shows before the photo is loaded.
    function choose(element, show) {
        const pickBox = element.querySelector('.idct-pick');
        const blocks = element.querySelector('.idct-blocks');
        pickBox.textContent = 'Das Foto wird geladen …';
        root.JpegImage.load(element.dataset.src).then((image) => {
            let mx = 0, my = 0, comp = 0, index = 0;
            const names = [];
            image.comps.forEach((c, ci) => {
                for (let i = 0; i < c.h * c.v; i++) names.push({ comp: ci, index: i, label: c.h * c.v > 1 ? `${c.name}<sub>${i}</sub>` : c.name });
            });
            blocks.innerHTML = names.map((n, i) => `<button type='button' class='btn btn-sm btn-outline-secondary' data-block='${i}' aria-pressed='${i === 0}'>${n.label}</button>`).join('');
            // the chosen block, outlined in the magnifier
            const mark = (ctx, zoom) => {
                const c = image.comps[comp];
                const sw = image.hmax / c.h * 8 * zoom, sh = image.vmax / c.v * 8 * zoom;
                const x = (index % c.h) * sw, y = Math.floor(index / c.h) * sh;
                ctx.setLineDash([6, 4]);
                ctx.lineWidth = 3;
                ctx.strokeStyle = '#ffd43b';
                ctx.strokeRect(x + 5, y + 5, sw - 10, sh - 10);
            };
            const picker = root.JpegImage.picker(pickBox, image, {
                mx, my, mark,
                onPick: (x, y) => { mx = x; my = y; update(); },
            });
            function update() {
                blocks.querySelectorAll('button').forEach((b) => {
                    const n = names[Number(b.dataset.block)];
                    b.setAttribute('aria-pressed', String(n.comp === comp && n.index === index));
                });
                picker.redraw();
                show(image.block(comp, mx, my, index));
            }
            blocks.addEventListener('click', (event) => {
                const button = event.target.closest('button[data-block]');
                if (!button) return;
                ({ comp, index } = names[Number(button.dataset.block)]);
                update();
            });
            // a block chosen above, at the Huffman stepper of the page
            document.addEventListener('jpeg-block', (event) => {
                ({ comp, index, mx, my } = event.detail);
                picker.set(mx, my);
                update();
            });
        }).catch((error) => { pickBox.textContent = `Das Foto ließ sich nicht laden: ${error.message}`; });
    }

    const api = { ZIGZAG, basis, steps, mount };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.IdctStepper = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.idct-stepper').forEach(mount);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
