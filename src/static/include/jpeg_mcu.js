// One MCU of the demo photo, taken apart (»JPEG«, »Rekonstruktion der
// MCU«): its Y blocks in their places, Cb and Cr, each a value for
// hmax × vmax pixels, and the colours put together from them. The MCU is
// chosen on the photo (JpegImage.picker). A pixel pointed at, tapped or
// moved to with the arrow keys shows its Y, Cb and Cr and how R, G and B
// come out of them, with the page's formulas (as in C, with points).
//
// <div class='jpeg-mcu' data-src='/jpeg/44-baseline.jpg' data-mcu='13 17'></div>
// with jpeg_image.js before jpeg_mcu.js; data-mcu: column and row of the
// MCU shown first.

(function () {
    'use strict';

    const CELL = 16;
    const signed = (n) => n < 0 ? `(−${-n})` : String(n);

    function mount(element, image) {
        const [startX, startY] = (element.dataset.mcu || '0 0').trim().split(/\s+/).map(Number);
        let mx = startX, my = startY;
        const w = 8 * image.hmax, h = 8 * image.vmax;
        // the pixel of the MCU that the readout explains
        let px = 0, py = 0;
        const [Y, Cb, Cr] = image.comps;
        const yBlocks = Y.h * Y.v;
        element.innerHTML = `
            <div class='jpeg-mcu-pick'></div>
            <div class='jpeg-mcu-panels'>
                <figure><canvas data-show='y'></canvas><figcaption>Y: ${yBlocks === 1 ? 'ein Block' : `${yBlocks} Blöcke, in der Reihenfolge der Datei`}</figcaption></figure>
                <figure><canvas data-show='cb'></canvas><figcaption>Cb: ${Cb.h * Cb.v === 1 && w > 8 ? `ein Block, jeder Wert für ${image.hmax * image.vmax / (Cb.h * Cb.v)} Pixel` : 'ein Block'}, als Farbe bei mittlerem Y und Cr</figcaption></figure>
                <figure><canvas data-show='cr'></canvas><figcaption>Cr: ${Cr.h * Cr.v === 1 && w > 8 ? `ein Block, jeder Wert für ${image.hmax * image.vmax / (Cr.h * Cr.v)} Pixel` : 'ein Block'}, als Farbe bei mittlerem Y und Cb</figcaption></figure>
                <figure><canvas data-show='rgb'></canvas><figcaption>Zusammen: R, G und B</figcaption></figure>
            </div>
            <div class='jpeg-mcu-readout' aria-live='polite'></div>`;
        const canvases = [...element.querySelectorAll('.jpeg-mcu-panels canvas')];
        const readout = element.querySelector('.jpeg-mcu-readout');
        canvases.forEach((canvas) => {
            canvas.width = w * CELL;
            canvas.height = h * CELL;
            canvas.tabIndex = 0;
            canvas.dataset.noSwipe = '';
        });

        // what a panel shows of the pixel (x, y) of the photo
        const shade = {
            y: (p) => [p.ycc[0], p.ycc[0], p.ycc[0]],
            cb: (p) => JpegImage.rgb(128, p.ycc[1], 128),
            cr: (p) => JpegImage.rgb(128, 128, p.ycc[2]),
            rgb: (p) => p.rgb,
        };

        function draw() {
            const x0 = mx * w, y0 = my * h;
            for (const canvas of canvases) {
                const show = canvas.dataset.show;
                const ctx = canvas.getContext('2d');
                for (let y = 0; y < h; y++) {
                    for (let x = 0; x < w; x++) {
                        const [r, g, b] = shade[show](image.pixel(x0 + x, y0 + y));
                        ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
                        ctx.fillRect(x * CELL, y * CELL, CELL, CELL);
                        // beyond the photo: in the file, never drawn
                        if (y0 + y >= image.height || x0 + x >= image.width) {
                            ctx.fillStyle = 'rgba(128, 128, 128, 0.75)';
                            ctx.fillRect(x * CELL, y * CELL, CELL, CELL);
                        }
                    }
                }
                // the borders: the Y blocks, or the pixels that share one Cb or Cr value
                const comp = show === 'cb' ? Cb : show === 'cr' ? Cr : null;
                const stepX = comp ? image.hmax / comp.h : 8, stepY = comp ? image.vmax / comp.v : 8;
                if (show !== 'rgb' && (stepX > 1 || show === 'y')) {
                    ctx.strokeStyle = show === 'y' ? 'rgba(255, 220, 80, 0.9)' : 'rgba(0, 0, 0, 0.25)';
                    ctx.lineWidth = show === 'y' ? 2 : 1;
                    ctx.beginPath();
                    for (let x = stepX; x < w; x += stepX) { ctx.moveTo(x * CELL, 0); ctx.lineTo(x * CELL, h * CELL); }
                    for (let y = stepY; y < h; y += stepY) { ctx.moveTo(0, y * CELL); ctx.lineTo(w * CELL, y * CELL); }
                    ctx.stroke();
                }
                if (show === 'y' && yBlocks > 1) {
                    ctx.font = `bold ${CELL * 1.1}px '0xProto Nerd Font Mono', monospace`;
                    ctx.textBaseline = 'top';
                    for (let i = 0; i < yBlocks; i++) {
                        const bx = (i % Y.h) * 8 * CELL + 4, by = Math.floor(i / Y.h) * 8 * CELL + 4;
                        const label = `Y${i}`;
                        ctx.fillStyle = 'rgba(0, 0, 0, 0.6)';
                        ctx.fillRect(bx, by, ctx.measureText(label).width + 8, CELL * 1.3);
                        ctx.fillStyle = '#fff';
                        ctx.fillText(label, bx + 4, by + 2);
                    }
                }
                // the pixel explained below; in Cb and Cr all pixels of its value
                const cx = comp ? Math.floor(px / stepX) * stepX : px, cy = comp ? Math.floor(py / stepY) * stepY : py;
                const cw = comp ? stepX : 1, ch = comp ? stepY : 1;
                ctx.lineWidth = 3;
                ctx.strokeStyle = '#000';
                ctx.strokeRect(cx * CELL + 1.5, cy * CELL + 1.5, cw * CELL - 3, ch * CELL - 3);
                ctx.lineWidth = 1.5;
                ctx.strokeStyle = '#fff';
                ctx.strokeRect(cx * CELL + 1.5, cy * CELL + 1.5, cw * CELL - 3, ch * CELL - 3);
            }
            explain();
        }

        function explain() {
            const x = mx * w + px, y = my * h + py;
            const p = image.pixel(x, y);
            const [vy, vb, vr] = p.ycc;
            const [r, g, b] = p.rgb;
            const block = Math.floor(py / 8) * Y.h + Math.floor(px / 8);
            const shared = image.hmax * image.vmax / (Cb.h * Cb.v);
            const db = vb - 128, dr = vr - 128;
            const outside = y >= image.height || x >= image.width;
            readout.innerHTML = `
                <div class='jpeg-mcu-pixel'><span class='jpeg-mcu-swatch' style='background: rgb(${r}, ${g}, ${b})'></span>
                    <strong>Pixel x = ${x}, y = ${y}</strong>${outside ? ' liegt außerhalb des Bilds: Es steht in der Datei, damit die MCU voll ist, aber dein Decoder zeichnet es nicht.' : ''}</div>
                <div>Y = ${vy}${yBlocks > 1 ? ` aus Block Y${block}` : ''}, Cb = ${vb} und Cr = ${vr}${shared > 1 ? `, beide gemeinsam für ${shared} Pixel` : ''}.</div>
                <div class='jpeg-mcu-formulas'>
                    <div><code>R = Y + 1.402 · (Cr − 128)</code> <code>= ${vy} + 1.402 · ${signed(dr)} = ${(vy + 1.402 * dr).toFixed(1)} → ${r}</code></div>
                    <div><code>G = Y − 0.344136 · (Cb − 128) − 0.714136 · (Cr − 128)</code> <code>= ${vy} − 0.344136 · ${signed(db)} − 0.714136 · ${signed(dr)} = ${(vy - 0.344136 * db - 0.714136 * dr).toFixed(1)} → ${g}</code></div>
                    <div><code>B = Y + 1.772 · (Cb − 128)</code> <code>= ${vy} + 1.772 · ${signed(db)} = ${(vy + 1.772 * db).toFixed(1)} → ${b}</code></div>
                </div>`;
        }

        const pick = JpegImage.picker(element.querySelector('.jpeg-mcu-pick'), image, {
            mx, my,
            onPick: (x, y) => { mx = x; my = y; draw(); },
        });
        const toPixel = (canvas, event) => {
            const rect = canvas.getBoundingClientRect();
            px = Math.max(0, Math.min(w - 1, Math.floor((event.clientX - rect.left) / rect.width * w)));
            py = Math.max(0, Math.min(h - 1, Math.floor((event.clientY - rect.top) / rect.height * h)));
            draw();
        };
        canvases.forEach((canvas) => {
            canvas.addEventListener('pointermove', (event) => { if (event.pointerType === 'mouse') toPixel(canvas, event); });
            canvas.addEventListener('pointerdown', (event) => toPixel(canvas, event));
            canvas.setAttribute('role', 'img');
            canvas.setAttribute('aria-label', canvas.nextElementSibling.textContent + '. Die Pfeiltasten wählen ein Pixel, darunter steht, wie seine Farbe entsteht.');
            canvas.addEventListener('keydown', (event) => {
                const moves = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] };
                const move = moves[event.key];
                if (!move || event.ctrlKey || event.altKey || event.metaKey) return;
                event.preventDefault();
                px = Math.max(0, Math.min(w - 1, px + move[0]));
                py = Math.max(0, Math.min(h - 1, py + move[1]));
                draw();
            });
        });
        pick.set(mx, my);
        draw();
    }

    function start() {
        document.querySelectorAll('.jpeg-mcu[data-src]').forEach((element) => {
            if (element.dataset.mounted) return;
            element.dataset.mounted = 'true';
            element.textContent = 'Das Foto wird geladen …';
            JpegImage.load(element.dataset.src).then((image) => mount(element, image))
                .catch((error) => { element.textContent = `Das Foto ließ sich nicht laden: ${error.message}`; });
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
})();
