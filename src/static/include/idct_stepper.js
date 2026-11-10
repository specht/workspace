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
// one of its blocks; data-start='mx my comp index' is the block shown
// first, data-coef its coefficients until the photo has loaded. A »jpeg-block«
// event on the document ({ comp, index, mx, my }) chooses one from outside.
// With data-picture and an <img> inside (the page resolves its address),
// the picture stands above it with a grid
// of 8 × 8 blocks; the chosen block (data-block='x y', multiples of 8) is
// dragged, tapped or moved with the arrow keys, shown beside the picture
// in Y as the goal of the steps (and the panel of pixel values becomes
// the error: the goal minus the block so far), and its coefficients are
// computed here: the DCT of Y − 128 (Y = 0,299 R + 0,587 G + 0,114 B),
// rounded to multiples of data-quantize (1 if not given).

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
        const pictureImg = element.querySelector('img');
        let coef = element.dataset.coef.trim().split(/\s+/).map(Number);
        let list = steps(coef);
        element.innerHTML = `
            ${element.dataset.src ? `<div class='idct-picture-row'><div class='idct-choose'><div class='idct-pick'></div><div class='idct-blocks' role='group' aria-label='Kanal'></div><p class='idct-lens-legend'>Wähle Y, Cb oder Cr, dann einen Block: in der Lupe mit der Maus darüber und klicken, oder auf dem Foto. Ziehen verschiebt die Lupe. Weiß ist die MCU, gelb der Block der Zeitlupe, gestrichelt der unter der Maus.</p></div><figure class='idct-target-box'><div class='idct-grid idct-target' role='img' aria-label='Der fertige Block: das Ziel der Schritte'></div><figcaption>Ziel: der fertige Block</figcaption></figure></div>` : ''}
            ${element.dataset.picture !== undefined ? `<div class='idct-picture-row'><figure class='idct-picture-box'><div class='idct-picture-wrap'><canvas class='idct-picture' tabindex='0' role='img' aria-label='Das Bild in Blöcken aus 8 × 8 Pixeln, ein Block ist markiert; er lässt sich ziehen, antippen oder mit den Pfeiltasten verschieben.'></canvas><canvas class='idct-picture-grid' aria-hidden='true'></canvas></div><figcaption>Gelb der Block der Zeitlupe, gestrichelt der unter der Maus: ziehen, klicken oder die Pfeiltasten</figcaption><div class='idct-blocks' role='group' aria-label='Kanal'><button type='button' class='btn btn-sm btn-outline-secondary' data-channel='0' aria-pressed='true'>Y</button><button type='button' class='btn btn-sm btn-outline-secondary' data-channel='1' aria-pressed='false'>Cb</button><button type='button' class='btn btn-sm btn-outline-secondary' data-channel='2' aria-pressed='false'>Cr</button></div></figure><figure class='idct-target-box'><div class='idct-grid idct-target' role='img' aria-label='Der markierte Block in Y: das Ziel der Schritte'></div><figcaption class='idct-target-caption'>Ziel in Y</figcaption></figure></div>` : ''}
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
                <figure><div class='idct-grid idct-block'></div><figcaption>Der Block bisher</figcaption></figure>
                <figure><div class='idct-grid idct-error' role='img' aria-label='Der Fehler: Ziel minus Block bisher'></div><figcaption class='idct-error-caption'>Fehler</figcaption></figure>
            </div>
            <div class='idct-say' aria-live='polite'></div>`;
        const say = element.querySelector('.idct-say');
        const count = element.querySelector('.idct-count');
        const go = (name) => element.querySelector(`[data-go='${name}']`);
        let current = 0;

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
            element.querySelector('.idct-block').innerHTML = cells(step.block, (p) => `<span style='background: ${grey(p)}'></span>`);
            // the error, the goal minus the block so far, orange where the
            // block is still too dark, blue where too bright; the goal is the
            // picture's block in Y, or without a picture the finished block
            const error = element.querySelector('.idct-error');
            const goal = element.idctTarget || list[list.length - 1].block;
            const target = element.querySelector('.idct-target');
            if (target) target.innerHTML = cells(goal, (g) => `<span style='background: ${grey(g)}'></span>`);
            if (error) {
                const diffs = step.block.map((p, i) => goal[i] - p);
                // each cell with its error as a number, light on dark, dark on bright
                error.innerHTML = cells(diffs, (d) => {
                    const t = Math.min(1, Math.abs(d) / 40);
                    const [r, g, b] = d > 0 ? [245, 158, 11] : [59, 130, 246];
                    const [cr, cg, cb] = [r * t, g * t, b * t].map(Math.round);
                    const light = 0.299 * cr + 0.587 * cg + 0.114 * cb > 110;
                    const text = d > 0 ? `+${d}` : d < 0 ? `−${-d}` : '0';
                    return `<span style='background: rgb(${cr}, ${cg}, ${cb}); color: ${!d ? '#6b7280' : light ? '#1f2937' : '#e5e7eb'}'>${text}</span>`;
                });
                const mean = diffs.reduce((sum, d) => sum + Math.abs(d), 0) / 64;
                element.querySelector('.idct-error-caption').textContent = `Fehler, im Mittel ${mean.toLocaleString('de-DE', { maximumFractionDigits: 1 })}`;
            }
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
        if (element.dataset.src && root.JpegImage) choose(element, (c) => { coef = c; list = steps(coef); to(0); });
        if (element.dataset.picture !== undefined && pictureImg) pickFromPicture(element, pictureImg, (c) => { coef = c; list = steps(coef); to(0); });
    }

    // The picker above the stepper: the MCU on the photo, then one of its
    // blocks, in the order of the file. Starts at the first block of the
    // first MCU, which data-coef shows before the photo is loaded.
    function choose(element, show) {
        const pickBox = element.querySelector('.idct-pick');
        const blocks = element.querySelector('.idct-blocks');
        pickBox.textContent = 'Das Foto wird geladen …';
        root.JpegImage.load(element.dataset.src).then((image) => {
            // the block shown first (data-start: MCU column, MCU row, channel, block)
            let [mx, my, comp, index] = (element.dataset.start || '0 0 0 0').split(/\s+/).map(Number);
            const mcuW = 8 * image.hmax, mcuH = 8 * image.vmax;
            // one button per channel (Y, Cb, Cr); the block is chosen in the
            // magnifier or on the photo
            blocks.innerHTML = image.comps.map((c, ci) => `<button type='button' class='btn btn-sm btn-outline-secondary' data-comp='${ci}' aria-pressed='${ci === 0}'>${c.name}</button>`).join('')
                + `<span class='idct-block-name' aria-live='polite'></span>`;
            const blockName = blocks.querySelector('.idct-block-name');
            // a block of the channel, in pixels of the photo
            const blockSize = () => { const c = image.comps[comp]; return [mcuW / c.h, mcuH / c.v]; };
            // the chosen block, in pixels of the photo, for the magnifier's frame
            const markBlock = () => {
                const c = image.comps[comp];
                const sw = mcuW / c.h, sh = mcuH / c.v;
                return [mx * mcuW + (index % c.h) * sw, my * mcuH + Math.floor(index / c.h) * sh, sw, sh];
            };
            const picker = root.JpegImage.picker(pickBox, image, {
                mx, my, markBlock, blockSize,
                onPick: (x, y) => { mx = x; my = y; update(); },
                // a block chosen by its pixel on the photo or in the magnifier
                onBlock: (px, py) => {
                    const c = image.comps[comp];
                    const [bw, bh] = blockSize();
                    mx = Math.floor(px / mcuW);
                    my = Math.floor(py / mcuH);
                    index = Math.min(c.v - 1, Math.floor((py - my * mcuH) / bh)) * c.h + Math.min(c.h - 1, Math.floor((px - mx * mcuW) / bw));
                    update();
                },
            });
            function update() {
                blocks.querySelectorAll('button').forEach((b) => b.setAttribute('aria-pressed', String(Number(b.dataset.comp) === comp)));
                const c = image.comps[comp];
                blockName.innerHTML = c.h * c.v > 1 ? `Block ${c.name}<sub>${index}</sub> der MCU` : `Block ${c.name} der MCU`;
                picker.redraw();
                show(image.block(comp, mx, my, index));
            }
            blocks.addEventListener('click', (event) => {
                const button = event.target.closest('button[data-comp]');
                if (!button) return;
                comp = Number(button.dataset.comp);
                index = 0;
                update();
            });
            // a block chosen above, at the Huffman stepper of the page
            document.addEventListener('jpeg-block', (event) => {
                ({ comp, index, mx, my } = event.detail);
                picker.set(mx, my);
                update();
            });
            update();
        }).catch((error) => { pickBox.textContent = `Das Foto ließ sich nicht laden: ${error.message}`; });
    }

    // The coefficients of an 8 × 8 block (values row by row) in natural
    // order, rounded to multiples of q: the DCT the page computes in Python.
    function forward(block, q = 1) {
        return Array.from({ length: 64 }, (_, i) => {
            const u = i % 8, v = Math.floor(i / 8);
            let sum = 0;
            for (let y = 0; y < 8; y++) {
                for (let x = 0; x < 8; x++) {
                    sum += block[y * 8 + x] * Math.cos((2 * x + 1) * u * Math.PI / 16) * Math.cos((2 * y + 1) * v * Math.PI / 16);
                }
            }
            const c = C(u) * C(v) / 4 * sum;
            return Math.round(c / q) * q + 0;
        });
    }

    // The picture above the stepper (data-picture): the chosen block marked,
    // a grid of 8 × 8 blocks; dragging, a tap or the arrow keys move it.
    function pickFromPicture(element, img, show) {
        const canvas = element.querySelector('.idct-picture');
        const q = Number(element.dataset.quantize) || 1;
        const ready = img.complete && img.naturalWidth ? Promise.resolve() : new Promise((done) => img.addEventListener('load', done, { once: true }));
        ready.then(() => {
            const w = img.naturalWidth, h = img.naturalHeight;
            const off = document.createElement('canvas');
            off.width = w; off.height = h;
            const octx = off.getContext('2d', { willReadFrequently: true });
            octx.drawImage(img, 0, 0);
            const rgba = octx.getImageData(0, 0, w, h).data;
            // Y, Cb or Cr of a pixel, with the formulas of »Farbe sparen«
            let channel = 0;
            // a block covers 8 × 8 pixels in Y, 16 × 16 in Cb and Cr: there the
            // colour has half the resolution in each direction, as in 4:2:0, and
            // each value of the block is the mean of 2 × 2 pixels
            const size = () => (channel ? 16 : 8);
            const value = (x, y) => {
                const i = 4 * (y * w + x), r = rgba[i], g = rgba[i + 1], b = rgba[i + 2];
                if (channel === 1) return 128 - 0.168736 * r - 0.331264 * g + 0.5 * b;
                if (channel === 2) return 128 + 0.5 * r - 0.418688 * g - 0.081312 * b;
                return 0.299 * r + 0.587 * g + 0.114 * b;
            };
            const SCALE = 4;
            canvas.width = w * SCALE; canvas.height = h * SCALE;
            const ctx = canvas.getContext('2d');
            ctx.imageSmoothingEnabled = false;
            let [bx, by] = (element.dataset.block || '0 0').split(/\s+/).map(Number);
            let hover = null;          // the block under the mouse: its top left corner
            // the picture on its canvas; the grid and the frames on a second one
            // in device pixels, at whole pixels, so that nothing wavers: each grid
            // line a light and a dark device pixel, the chosen block solid yellow,
            // the block under the mouse dashed, both just outside its pixels
            const grid = element.querySelector('.idct-picture-grid');
            const draw = () => {
                ctx.drawImage(img, 0, 0, w * SCALE, h * SCALE);
                const box = canvas.getBoundingClientRect();
                const ratio = window.devicePixelRatio || 1;
                const gw = Math.round(box.width * ratio), gh = Math.round(box.height * ratio);
                if (!gw || !gh) return;
                if (grid.width !== gw || grid.height !== gh) { grid.width = gw; grid.height = gh; }
                const g = grid.getContext('2d');
                g.clearRect(0, 0, gw, gh);
                const k = gw / w;                      // device pixels per pixel of the picture
                const X = (px) => Math.round(px * k), Y = (py) => Math.round(py * k);
                for (let x = size(); x < w; x += size()) {
                    g.fillStyle = 'rgba(255, 255, 255, 0.22)'; g.fillRect(X(x) - 1, 0, 1, gh);
                    g.fillStyle = 'rgba(0, 0, 0, 0.22)'; g.fillRect(X(x), 0, 1, gh);
                }
                for (let y = size(); y < h; y += size()) {
                    g.fillStyle = 'rgba(255, 255, 255, 0.22)'; g.fillRect(0, Y(y) - 1, gw, 1);
                    g.fillStyle = 'rgba(0, 0, 0, 0.22)'; g.fillRect(0, Y(y), gw, 1);
                }
                const thick = Math.max(2, Math.round(ratio * 1.5));
                const frame = (x0, y0, dash) => {
                    const r = [X(x0), Y(y0), X(x0 + size()), Y(y0 + size())];
                    g.setLineDash([]);
                    g.lineWidth = 1;
                    g.strokeStyle = 'rgba(0, 0, 0, 0.55)';
                    g.strokeRect(r[0] - thick - 0.5, r[1] - thick - 0.5, r[2] - r[0] + 2 * thick + 1, r[3] - r[1] + 2 * thick + 1);
                    g.lineWidth = thick;
                    g.strokeStyle = '#ffd43b';
                    g.setLineDash(dash ? [3 * thick, 2 * thick] : []);
                    g.strokeRect(r[0] - thick / 2, r[1] - thick / 2, r[2] - r[0] + thick, r[3] - r[1] + thick);
                    g.setLineDash([]);
                };
                frame(bx, by, false);
                if (hover && (hover[0] !== bx || hover[1] !== by)) frame(hover[0], hover[1], true);
            };
            if (typeof ResizeObserver !== 'undefined') new ResizeObserver(() => draw()).observe(canvas);
            const pick = (x, y, again) => {
                const n = size();
                const nx = Math.max(0, Math.min(w - n, Math.floor(x / n) * n)), ny = Math.max(0, Math.min(h - n, Math.floor(y / n) * n));
                if (nx === bx && ny === by && canvas.dataset.ready && !again) return;
                bx = nx; by = ny;
                canvas.dataset.ready = '1';
                draw();
                const block = [];
                const k = n / 8;              // 1 in Y, 2 in Cb and Cr: the mean of k × k pixels
                for (let yy = 0; yy < 8; yy++) {
                    for (let xx = 0; xx < 8; xx++) {
                        let sum = 0;
                        for (let dy = 0; dy < k; dy++) for (let dx = 0; dx < k; dx++) sum += value(bx + k * xx + dx, by + k * yy + dy);
                        block.push(sum / (k * k) - 128);
                    }
                }
                // the goal: the block in its channel, beside the picture and for the error
                element.idctTarget = block.map((value) => Math.max(0, Math.min(255, Math.round(value + 128))));
                show(forward(block, q));
            };
            const at = (event) => {
                const box = canvas.getBoundingClientRect();
                return [(event.clientX - box.left) * w / box.width, (event.clientY - box.top) * h / box.height];
            };
            let holding = false;
            canvas.addEventListener('pointerdown', (event) => { holding = true; canvas.setPointerCapture(event.pointerId); pick(...at(event)); event.preventDefault(); });
            canvas.addEventListener('pointermove', (event) => {
                const [x, y] = at(event);
                if (holding) { pick(x, y); return; }
                const n = size();
                const hx = Math.max(0, Math.min(w - n, Math.floor(x / n) * n)), hy = Math.max(0, Math.min(h - n, Math.floor(y / n) * n));
                if (!hover || hover[0] !== hx || hover[1] !== hy) { hover = [hx, hy]; draw(); }
            });
            canvas.addEventListener('pointerleave', () => { if (hover) { hover = null; draw(); } });
            canvas.addEventListener('pointerup', () => { holding = false; });
            canvas.addEventListener('pointercancel', () => { holding = false; });
            canvas.addEventListener('keydown', (event) => {
                const n = size();
                const step = { ArrowUp: [0, -n], ArrowDown: [0, n], ArrowLeft: [-n, 0], ArrowRight: [n, 0] }[event.key];
                if (!step) return;
                event.preventDefault();
                event.stopPropagation();
                pick(bx + step[0], by + step[1]);
            });
            // the channel: Y, Cb or Cr
            element.querySelectorAll('[data-channel]').forEach((button) => button.addEventListener('click', () => {
                channel = Number(button.dataset.channel);
                hover = null;
                element.querySelectorAll('[data-channel]').forEach((b) => b.setAttribute('aria-pressed', String(b === button)));
                element.querySelector('.idct-target-caption').textContent = `Ziel in ${button.textContent}`;
                pick(bx, by, true);
            }));
            pick(bx, by);
        });
    }

    const api = { ZIGZAG, basis, steps, mount, forward };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.IdctStepper = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.idct-stepper').forEach(mount);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
