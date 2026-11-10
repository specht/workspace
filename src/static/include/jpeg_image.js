// The demo photo of »JPEG«, decoded here in the browser the way the page's
// decoder does it (src/content/jpeg/loesung/jpeg.c): baseline, Huffman,
// the iDCT with doubles, Cb and Cr read at smaller coordinates. Every
// block's coefficients are kept, so that a widget can show any part of the
// photo: the iDCT stepper (idct_stepper.js) one block, the MCU widget
// (jpeg_mcu.js) one MCU. The picker chooses the MCU on the photo.
//
// JpegImage.load(url) decodes once per page and resolves to
//   { width, height, hmax, vmax, mcusX, mcusY,
//     comps: [{ name, h, v, blocksX, plane, planeWidth, coef }],
//     block(c, mx, my, i), bits(c, mx, my, i), pixel(x, y), photo() }
// coef: the dequantised coefficients of every block, 64 in natural order
// (row by row: v * 8 + u), blocks row by row in the component. bits()
// gives what the Huffman stepper needs to read one block again: its bytes
// in the file, where its bits start and end, the DC coefficient before it,
// its tables (Huffman codes as {'1011': 0x04}) and its quantisation table.

(function (root) {
    'use strict';

    const ZIGZAG = [
        0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5,
        12, 19, 26, 33, 40, 48, 41, 34, 27, 20, 13, 6, 7, 14, 21, 28,
        35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51,
        58, 59, 52, 45, 38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63,
    ];
    const NAMES = ['Y', 'Cb', 'Cr'];
    // C's lround: halves away from zero
    const lround = (v) => v < 0 ? -Math.round(-v) : Math.round(v);
    const clamp = (v) => Math.max(0, Math.min(255, lround(v)));

    // idct_cos[x][u] = C(u) · cos((2x + 1) u π / 16), as in jpeg.c
    const COS = Array.from({ length: 8 }, (_, x) => Array.from({ length: 8 }, (_, u) =>
        (u === 0 ? Math.SQRT1_2 : 1) * Math.cos((2 * x + 1) * u * Math.PI / 16)));

    // 64 pixel values of a block from its coefficients
    function idct(coef) {
        const tmp = new Float64Array(64);
        for (let v = 0; v < 8; v++) {
            for (let x = 0; x < 8; x++) {
                let sum = 0;
                for (let u = 0; u < 8; u++) sum += COS[x][u] * coef[v * 8 + u];
                tmp[v * 8 + x] = sum;
            }
        }
        const out = new Uint8Array(64);
        for (let y = 0; y < 8; y++) {
            for (let x = 0; x < 8; x++) {
                let sum = 0;
                for (let v = 0; v < 8; v++) sum += COS[y][v] * tmp[v * 8 + x];
                out[y * 8 + x] = clamp(sum / 4 + 128);
            }
        }
        return out;
    }

    // R, G and B from Y, Cb and Cr, the formulas on the page
    function rgb(Y, Cb, Cr) {
        return [clamp(Y + 1.402 * (Cr - 128)), clamp(Y - 0.344136 * (Cb - 128) - 0.714136 * (Cr - 128)), clamp(Y + 1.772 * (Cb - 128))];
    }

    function decode(bytes) {
        if (bytes[0] !== 0xFF || bytes[1] !== 0xD8) throw new Error('Das ist kein JPEG.');
        let pos = 2;
        const word = () => { const w = (bytes[pos] << 8) | bytes[pos + 1]; pos += 2; return w; };
        const quant = [];
        const tables = { dc: [], ac: [] };
        const comps = [];
        let width = 0, height = 0;
        for (;;) {
            if (bytes[pos] !== 0xFF) throw new Error(`Kein Marker bei Byte ${pos}.`);
            const marker = bytes[pos + 1];
            pos += 2;
            const length = word();
            const end = pos + length - 2;
            if (marker === 0xDB) {
                while (pos < end) {
                    const slot = bytes[pos++] & 15;
                    const table = new Int32Array(64);
                    for (let k = 0; k < 64; k++) table[ZIGZAG[k]] = bytes[pos++];
                    quant[slot] = table;
                }
            } else if (marker === 0xC0) {
                pos++;
                height = word();
                width = word();
                const n = bytes[pos++];
                for (let i = 0; i < n; i++) {
                    comps.push({ id: bytes[pos], h: bytes[pos + 1] >> 4, v: bytes[pos + 1] & 15, tq: bytes[pos + 2], name: NAMES[i] });
                    pos += 3;
                }
            } else if (marker === 0xC4) {
                while (pos < end) {
                    const tcTh = bytes[pos++];
                    const counts = bytes.subarray(pos, pos + 16);
                    pos += 16;
                    const total = counts.reduce((a, b) => a + b, 0);
                    const t = { values: bytes.slice(pos, pos + total), min: [], max: [], ptr: [], codes: {} };
                    pos += total;
                    let code = 0, k = 0;
                    for (let len = 1; len <= 16; len++) {
                        t.ptr[len] = k;
                        t.min[len] = code;
                        code += counts[len - 1];
                        k += counts[len - 1];
                        t.max[len] = counts[len - 1] ? code - 1 : -1;
                        for (let c = t.min[len]; c <= t.max[len]; c++) t.codes[c.toString(2).padStart(len, '0')] = t.values[t.ptr[len] + c - t.min[len]];
                        code <<= 1;
                    }
                    (tcTh >> 4 ? tables.ac : tables.dc)[tcTh & 15] = t;
                }
            } else if (marker === 0xDA) {
                const n = bytes[pos++];
                for (let i = 0; i < n; i++) {
                    const c = comps.find((c) => c.id === bytes[pos]);
                    c.td = bytes[pos + 1] >> 4;
                    c.ta = bytes[pos + 1] & 15;
                    pos += 2;
                }
                pos = end;
                break;
            } else if (marker >= 0xC1 && marker <= 0xCF && marker !== 0xC4 && marker !== 0xC8 && marker !== 0xCC) {
                throw new Error('Nur Baseline-JPEGs.');
            } else if (marker === 0xDD) {
                throw new Error('Restart Marker kann dieser Decoder nicht.');
            }
            pos = end;
        }

        let buffer = 0, left = 0;
        const bit = () => {
            if (left === 0) {
                buffer = bytes[pos++];
                if (buffer === 0xFF && bytes[pos++] !== 0x00) throw new Error('Ein Marker mitten in den Bilddaten.');
                left = 8;
            }
            left--;
            return (buffer >> left) & 1;
        };
        const bits = (n) => { let v = 0; for (let i = 0; i < n; i++) v = (v << 1) | bit(); return v; };
        // where the next bit is, counted in bits of the file; a byte 0xFF
        // read last was followed by its 0x00, so it lies two bytes back
        const at = () => left === 0 ? pos * 8 : (pos - (buffer === 0xFF ? 2 : 1)) * 8 + 8 - left;
        const huffman = (t) => {
            let code = 0;
            for (let len = 1; len <= 16; len++) {
                code = (code << 1) | bit();
                if (code <= t.max[len]) return t.values[t.ptr[len] + code - t.min[len]];
            }
            throw new Error('Kein gültiger Huffman-Code.');
        };
        const extend = (v, n) => n === 0 ? 0 : v < (1 << (n - 1)) ? v - (1 << n) + 1 : v;

        const hmax = Math.max(...comps.map((c) => c.h));
        const vmax = Math.max(...comps.map((c) => c.v));
        const mcusX = Math.ceil(width / (8 * hmax));
        const mcusY = Math.ceil(height / (8 * vmax));
        for (const c of comps) {
            c.blocksX = mcusX * c.h;
            c.blocksY = mcusY * c.v;
            c.planeWidth = c.blocksX * 8;
            c.plane = new Uint8Array(c.planeWidth * c.blocksY * 8);
            c.coef = new Int16Array(c.blocksX * c.blocksY * 64);
            c.span = new Int32Array(c.blocksX * c.blocksY * 2);
            c.before = new Int32Array(c.blocksX * c.blocksY);
            c.prediction = 0;
        }
        for (let my = 0; my < mcusY; my++) {
            for (let mx = 0; mx < mcusX; mx++) {
                for (const c of comps) {
                    for (let v = 0; v < c.v; v++) {
                        for (let h = 0; h < c.h; h++) {
                            const bx = mx * c.h + h, by = my * c.v + v;
                            const n = by * c.blocksX + bx;
                            const coef = c.coef.subarray(n * 64, n * 64 + 64);
                            c.span[2 * n] = at();
                            c.before[n] = c.prediction;
                            const s = huffman(tables.dc[c.td]);
                            c.prediction += extend(bits(s), s);
                            coef[0] = c.prediction * quant[c.tq][0];
                            for (let k = 1; k < 64; k++) {
                                const rs = huffman(tables.ac[c.ta]);
                                const run = rs >> 4, size = rs & 15;
                                if (size === 0) {
                                    if (run === 15) { k += 15; continue; }
                                    break;
                                }
                                k += run;
                                if (k > 63) throw new Error('Zu viele Koeffizienten in einem Block.');
                                coef[ZIGZAG[k]] = extend(bits(size), size) * quant[c.tq][ZIGZAG[k]];
                            }
                            c.span[2 * n + 1] = at();
                            const block = idct(coef);
                            for (let y = 0; y < 8; y++) c.plane.set(block.subarray(y * 8, y * 8 + 8), (by * 8 + y) * c.planeWidth + bx * 8);
                        }
                    }
                }
            }
        }

        const image = {
            width, height, hmax, vmax, mcusX, mcusY, comps,
            // the coefficients of block i (in the order of the file) of
            // component c in the MCU (mx, my)
            block(c, mx, my, i) {
                const comp = comps[c];
                const bx = mx * comp.h + i % comp.h, by = my * comp.v + Math.floor(i / comp.h);
                return Array.from(comp.coef.subarray((by * comp.blocksX + bx) * 64, (by * comp.blocksX + bx + 1) * 64));
            },
            bits(c, mx, my, i) {
                const comp = comps[c];
                const n = (my * comp.v + Math.floor(i / comp.h)) * comp.blocksX + mx * comp.h + i % comp.h;
                const start = comp.span[2 * n], end = comp.span[2 * n + 1];
                const from = start >> 3, to = (end + 7) >> 3;
                return {
                    offset: from, bytes: Array.from(bytes.subarray(from, to)), startBit: start - from * 8, endBit: end - from * 8,
                    previousDC: comp.before[n], dcSlot: comp.td, acSlot: comp.ta, quantSlot: comp.tq,
                    dc: tables.dc[comp.td].codes, ac: tables.ac[comp.ta].codes, quant: Array.from(ZIGZAG, (natural) => quant[comp.tq][natural]),
                };
            },
            // a pixel, also one beyond the photo in the last MCUs: Y, Cb,
            // Cr where the decoder reads them, and the colour
            pixel(x, y) {
                const ycc = comps.map((c) => c.plane[Math.floor(y * c.v / vmax) * c.planeWidth + Math.floor(x * c.h / hmax)]);
                return { ycc, rgb: rgb(...ycc) };
            },
            // the whole photo as a canvas, made once
            photo() {
                if (image.canvas) return image.canvas;
                const canvas = document.createElement('canvas');
                canvas.width = width;
                canvas.height = height;
                const ctx = canvas.getContext('2d');
                const data = ctx.createImageData(width, height);
                for (let y = 0; y < height; y++) {
                    for (let x = 0; x < width; x++) {
                        const [r, g, b] = image.pixel(x, y).rgb;
                        const i = (y * width + x) * 4;
                        data.data[i] = r;
                        data.data[i + 1] = g;
                        data.data[i + 2] = b;
                        data.data[i + 3] = 255;
                    }
                }
                ctx.putImageData(data, 0, 0);
                image.canvas = canvas;
                return canvas;
            },
        };
        return image;
    }

    const loading = {};
    function load(url) {
        if (!loading[url]) {
            loading[url] = fetch(url).then((response) => {
                if (!response.ok) throw new Error(`${url}: ${response.status}`);
                return response.arrayBuffer();
            }).then((buffer) => decode(new Uint8Array(buffer)));
        }
        return loading[url];
    }

    // --- The picker -----------------------------------------------------------
    //
    // The photo with the chosen MCU marked, the rest a little darker, and
    // beside it a magnifier: three by three MCUs around the chosen one, eight
    // times larger. A click on either chooses an MCU, dragging over the
    // photo moves it along, the arrow keys move it by one. Dragging the
    // magnifier moves it like a map, smoothly, the MCU in its middle the
    // chosen one, settling on it when let go. With options.blockSize() (the
    // size of a block in photo pixels) and options.onBlock(px, py), the
    // magnifier draws the blocks too, lights the block under the pointer,
    // and a click on the photo or in the magnifier chooses that block; a tap in it chooses the MCU tapped, or,
    // inside the chosen MCU, calls onInside(fx, fy) with where (0 to 1). picker(element, image, { mx, my, onPick(mx, my), mark }),
    // mark(ctx, scale): draws more into the chosen MCU of the magnifier (the
    // block the iDCT stepper shows). Returns { set(mx, my), redraw() }.

    const AROUND = 1;

    function picker(element, image, options) {
        const mcuW = 8 * image.hmax, mcuH = 8 * image.vmax;
        let mx = options.mx, my = options.my;
        element.classList.add('jpeg-picker');
        element.innerHTML = `
            <canvas class='jpeg-picker-photo' tabindex='0' data-no-swipe role='img'></canvas>
            <canvas class='jpeg-picker-lens' tabindex='-1' data-no-swipe aria-hidden='true'></canvas>
            <div class='jpeg-picker-where' aria-live='polite'></div>`;
        const photo = element.querySelector('.jpeg-picker-photo');
        const lens = element.querySelector('.jpeg-picker-lens');
        const where = element.querySelector('.jpeg-picker-where');
        photo.width = image.width;
        photo.height = image.height;
        const span = 2 * AROUND + 1;
        const zoom = 8;
        let ox = 0, oy = 0;
        let hover = null;              // the photo pixel under the pointer in the magnifier, for the block lit there            // while the magnifier is dragged: how far the view is off the MCU, in pixels of the photo
        lens.width = span * mcuW * zoom;
        lens.height = span * mcuH * zoom;

        function draw() {
            const ctx = photo.getContext('2d');
            ctx.drawImage(image.photo(), 0, 0);
            const x = mx * mcuW, y = my * mcuH;
            // darker around the MCU, so that it stands out at any size
            ctx.fillStyle = 'rgba(0, 0, 0, 0.45)';
            ctx.beginPath();
            ctx.rect(0, 0, image.width, image.height);
            ctx.rect(x - 6, y - 6, mcuW + 12, mcuH + 12);
            ctx.fill('evenodd');
            ctx.lineWidth = 3;
            ctx.strokeStyle = '#fff';
            ctx.strokeRect(x - 4.5, y - 4.5, mcuW + 9, mcuH + 9);

            const lctx = lens.getContext('2d');
            lctx.imageSmoothingEnabled = false;
            lctx.fillStyle = '#777';
            lctx.fillRect(0, 0, lens.width, lens.height);
            const sx = (mx - AROUND) * mcuW + ox, sy = (my - AROUND) * mcuH + oy;
            lctx.drawImage(image.photo(), sx, sy, span * mcuW, span * mcuH, 0, 0, lens.width, lens.height);
            // the MCU grid, faint, and the chosen MCU
            const gx = (mx - AROUND) * mcuW - sx, gy = (my - AROUND) * mcuH - sy;   // where the grid starts, in photo pixels
            // 3 pixels of the canvas, about 2 on the screen, so that they do not flicker
            lctx.strokeStyle = 'rgba(255, 255, 255, 0.55)';
            lctx.lineWidth = 3;
            for (let i = -1; i <= span + 1; i++) {
                lctx.beginPath();
                lctx.moveTo((gx + i * mcuW) * zoom, 0);
                lctx.lineTo((gx + i * mcuW) * zoom, lens.height);
                lctx.moveTo(0, (gy + i * mcuH) * zoom);
                lctx.lineTo(lens.width, (gy + i * mcuH) * zoom);
                lctx.stroke();
            }
            // the blocks of the channel, finer, where a block is smaller than an MCU
            const [bw, bh] = options.blockSize ? options.blockSize() : [mcuW, mcuH];
            if (bw < mcuW || bh < mcuH) {
                lctx.strokeStyle = 'rgba(255, 255, 255, 0.3)';
                lctx.lineWidth = 2;
                for (let i = -1; i <= span * mcuW / bw + 1; i++) {
                    lctx.beginPath();
                    lctx.moveTo((gx + i * bw) * zoom, 0);
                    lctx.lineTo((gx + i * bw) * zoom, lens.height);
                    lctx.stroke();
                }
                for (let i = -1; i <= span * mcuH / bh + 1; i++) {
                    lctx.beginPath();
                    lctx.moveTo(0, (gy + i * bh) * zoom);
                    lctx.lineTo(lens.width, (gy + i * bh) * zoom);
                    lctx.stroke();
                }
            }
            // the block under the pointer, lit
            if (hover && options.onBlock) {
                const hx = Math.floor(hover[0] / bw) * bw, hy = Math.floor(hover[1] / bh) * bh;
                lctx.fillStyle = 'rgba(255, 212, 59, 0.25)';
                lctx.fillRect((hx - sx) * zoom, (hy - sy) * zoom, bw * zoom, bh * zoom);
                lctx.strokeStyle = '#ffd43b';
                lctx.lineWidth = 2;
                lctx.strokeRect((hx - sx) * zoom + 1, (hy - sy) * zoom + 1, bw * zoom - 2, bh * zoom - 2);
            }
            const cx = (gx + AROUND * mcuW) * zoom, cy = (gy + AROUND * mcuH) * zoom;
            lctx.lineWidth = 2;
            lctx.strokeStyle = '#000';
            lctx.strokeRect(cx - 1, cy - 1, mcuW * zoom + 2, mcuH * zoom + 2);
            lctx.strokeStyle = '#fff';
            lctx.lineWidth = 1.5;
            lctx.strokeRect(cx + 0.75, cy + 0.75, mcuW * zoom - 1.5, mcuH * zoom - 1.5);
            if (options.mark) {
                lctx.save();
                lctx.translate(cx, cy);
                options.mark(lctx, zoom);
                lctx.restore();
            }

            const x1 = Math.min(x + mcuW, image.width) - 1, y1 = Math.min(y + mcuH, image.height) - 1;
            const label = `MCU in Spalte ${mx}, Reihe ${my}: x = ${x} bis ${x1}, y = ${y} bis ${y1}`;
            where.textContent = label + (y + mcuH > image.height ? `; ragt unten über das Bild hinaus` : '') + '.';
            photo.setAttribute('aria-label', `Das Foto. Die Pfeiltasten wählen eine andere MCU. Gewählt: ${label}.`);
        }

        function set(x, y, quiet) {
            x = Math.max(0, Math.min(image.mcusX - 1, x));
            y = Math.max(0, Math.min(image.mcusY - 1, y));
            if (x === mx && y === my) return;
            mx = x;
            my = y;
            draw();
            if (!quiet) options.onPick(mx, my);
        }
        const at = (canvas, event) => {
            const rect = canvas.getBoundingClientRect();
            return [(event.clientX - rect.left) * canvas.width / rect.width, (event.clientY - rect.top) * canvas.height / rect.height];
        };
        // a click or a drag over the photo: the MCU follows the pointer
        let holding = false;
        const pickPixel = (px, py) => {
            px = Math.max(0, Math.min(image.width - 1, px));
            py = Math.max(0, Math.min(image.height - 1, py));
            set(Math.floor(px / mcuW), Math.floor(py / mcuH), true);
            options.onBlock(px, py);
            draw();
        };
        const follow = (event) => {
            const [x, y] = at(photo, event);
            if (options.onBlock) pickPixel(x, y);
            else set(Math.floor(x / mcuW), Math.floor(y / mcuH));
        };
        photo.addEventListener('pointerdown', (event) => {
            holding = true;
            photo.setPointerCapture(event.pointerId);
            follow(event);
            event.preventDefault();
        });
        photo.addEventListener('pointermove', (event) => { if (holding) follow(event); });
        photo.addEventListener('pointerup', () => { holding = false; });
        photo.addEventListener('pointercancel', () => { holding = false; });
        // the magnifier: dragged like a map, smoothly, or tapped
        let grab = null;
        lens.addEventListener('pointerdown', (event) => {
            const [x, y] = at(lens, event);
            grab = { x, y, mx, my, moved: false };
            lens.setPointerCapture(event.pointerId);
            event.preventDefault();
        });
        // the photo pixel under a point of the magnifier
        const lensPixel = (x, y) => [(mx - AROUND) * mcuW + ox + x / zoom, (my - AROUND) * mcuH + oy + y / zoom];
        lens.addEventListener('pointermove', (event) => {
            const [x, y] = at(lens, event);
            if (!grab) {
                if (options.onBlock) { hover = lensPixel(x, y); draw(); }
                return;
            }
            // how far the view has moved, in photo pixels; the MCU nearest the middle is the chosen one
            const dx = (grab.x - x) / zoom, dy = (grab.y - y) / zoom;
            if (Math.abs(dx) > 1 || Math.abs(dy) > 1) grab.moved = true;
            if (!grab.moved) return;
            const nx = Math.max(0, Math.min(image.mcusX - 1, grab.mx + Math.round(dx / mcuW)));
            const ny = Math.max(0, Math.min(image.mcusY - 1, grab.my + Math.round(dy / mcuH)));
            ox = grab.mx * mcuW + dx - nx * mcuW;
            oy = grab.my * mcuH + dy - ny * mcuH;
            if (nx !== mx || ny !== my) set(nx, ny); else draw();
        });
        lens.addEventListener('pointerup', (event) => {
            if (!grab) return;
            const tap = !grab.moved;
            grab = null;
            ox = 0; oy = 0;
            if (!tap) { draw(); return; }
            const [x, y] = at(lens, event);
            if (options.onBlock) {
                const [px, py] = lensPixel(x, y);
                if (px >= 0 && py >= 0 && px < image.width && py < image.height) pickPixel(px, py);
                return;
            }
            const cx = Math.floor(x / (mcuW * zoom)) - AROUND, cy = Math.floor(y / (mcuH * zoom)) - AROUND;
            set(mx + cx, my + cy);
        });
        lens.addEventListener('pointercancel', () => { grab = null; ox = 0; oy = 0; draw(); });
        lens.addEventListener('pointerleave', () => { if (hover) { hover = null; draw(); } });
        photo.addEventListener('keydown', (event) => {
            const moves = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, -1], ArrowDown: [0, 1] };
            const move = moves[event.key];
            if (!move || event.ctrlKey || event.altKey || event.metaKey) return;
            // the photo's keys, not the stepper's (stepper_keys.js)
            event.preventDefault();
            set(mx + move[0], my + move[1]);
        });
        draw();
        return { set: (x, y) => set(x, y, true), redraw: draw };
    }

    const api = { ZIGZAG, decode, load, idct, rgb, picker };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.JpegImage = api;
})(typeof window !== 'undefined' ? window : globalThis);
