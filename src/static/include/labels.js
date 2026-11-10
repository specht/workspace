// /etiketten: a sticker to draw on (label_printer.rb, docs/etiketten.md).
//
// The sticker is a bitmap of the printer's dots, 472 × 268, each black (1)
// or white (0), so what one sees is what prints. Every tool writes into it
// directly, without the canvas's soft edges:
// - pen and eraser draw round dots along the stroke; fill floods a region
//   of one colour with a pattern; shapes are drawn while dragging;
// - »Spiegeln« repeats every operation mirrored left-right, or into all
//   four corners; »Pixel« works on cells of 8 × 8 dots instead of dots, and
//   writes back only the cells that changed;
// - pictures (Bootstrap Icons), text and a photo float above the sticker
//   until »Fertig«: they can be dragged and resized; a photo becomes dots by
//   Atkinson dithering, on this device.
// »Drucken« sends the dots as hexadecimal digits (ZPL's ^GFA data).
(function () {
    'use strict';
    const W = 472, H = 268, CELL = 8;
    const CW = Math.ceil(W / CELL), CH = Math.ceil(H / CELL);
    const STAMPS = [
        ['emoji-smile', 0xf327], ['emoji-sunglasses', 0xf329], ['heart-fill', 0xf415], ['star-fill', 0xf586],
        ['rocket-takeoff', 0xf845], ['robot', 0xf6b1], ['bug', 0xf1dc], ['cpu', 0xf2d6], ['controller', 0xf2d4],
        ['code-slash', 0xf2c6], ['terminal', 0xf5c3], ['lightning-charge-fill', 0xf46c], ['sun', 0xf5a2],
        ['moon-stars', 0xf496], ['cloud-sun', 0xf2be], ['tree', 0xf5e2], ['flower1', 0xf3cd],
        ['music-note-beamed', 0xf49e], ['airplane', 0xf7cd], ['bicycle', 0xf18d], ['car-front', 0xf7e1],
        ['house', 0xf425], ['trophy', 0xf5e7], ['gem', 0xf3e6], ['puzzle', 0xf503], ['camera', 0xf220],
        ['telephone', 0xf5c1], ['cup-hot', 0xf7eb], ['balloon-heart', 0xf708], ['gift', 0xf3ec],
        ['snow2', 0xf56e], ['umbrella', 0xf5fd], ['globe2', 0xf3ef], ['award', 0xf154],
    ];
    // ordered dithering for the gradient
    const BAYER = [[0, 8, 2, 10], [12, 4, 14, 6], [3, 11, 1, 9], [15, 7, 13, 5]];
    // a pattern: (x, y, box) => 1 for black; box: the region's extent
    const PATTERNS = [
        ['Schwarz', () => 1],
        ['Weiß', () => 0],
        ['Raster', (x, y) => (x + y) % 2],
        ['Punkte', (x, y) => +(x % 3 === 0 && y % 3 === 0)],
        ['Streifen', (x, y) => +((x + y) % 6 < 2)],
        ['Linien', (x, y) => +(y % 4 === 0)],
        ['Mauer', (x, y) => +(y % 6 === 0 || ((Math.floor(y / 6) % 2 ? x + 4 : x) % 8 === 0))],
        ['Verlauf', (x, y, box) => +((x - box.x0) / Math.max(1, box.x1 - box.x0) * 16 > BAYER[y % 4][x % 4] + 0.5)],
    ];

    const key = new URLSearchParams(location.search).get('k') || '';
    const $ = (s) => document.querySelector(s);
    const art = $('.lb-art');
    const ui = $('.lb-ui');
    const actx = art.getContext('2d');
    const uctx = ui.getContext('2d');
    const image = actx.createImageData(W, H);

    let base = new Uint8Array(W * H);
    let tool = 'pen', width = 2, pattern = 0, shape = 'line', filled = false;
    let pixel = false, mirror = 0, photoUsed = false;
    let floating = null;          // { kind, x, y, size, ink, text|code|img, brightness }
    let gesture = null;           // what the pointer is doing
    const undo = [];

    // ---- the bitmap ---------------------------------------------------

    // a target: the sticker's dots, or its cells in pixel mode
    function target(data, w, h) {
        return {
            w, h, data,
            get: (x, y) => (x >= 0 && y >= 0 && x < w && y < h ? data[y * w + x] : -1),
            set: (x, y, v) => { if (x >= 0 && y >= 0 && x < w && y < h) data[y * w + x] = v; },
        };
    }

    function cells() {
        const low = new Uint8Array(CW * CH);
        for (let y = 0; y < CH; y++) {
            for (let x = 0; x < CW; x++) {
                low[y * CW + x] = base[Math.min(H - 1, y * CELL + CELL / 2) * W + Math.min(W - 1, x * CELL + CELL / 2)];
            }
        }
        return low;
    }

    // back from cells to dots: only cells that changed
    function writeCells(low, before) {
        for (let cy = 0; cy < CH; cy++) {
            for (let cx = 0; cx < CW; cx++) {
                const i = cy * CW + cx;
                if (low[i] === before[i]) continue;
                for (let y = cy * CELL; y < Math.min(H, cy * CELL + CELL); y++) {
                    base.fill(low[i], y * W + cx * CELL, y * W + Math.min(W, cx * CELL + CELL));
                }
            }
        }
    }

    // the mirror images of a point in a target of w × h
    function mirrors(w, h) {
        const list = [(x, y) => [x, y]];
        if (mirror >= 1) list.push((x, y) => [w - 1 - x, y]);
        if (mirror >= 2) list.push((x, y) => [x, h - 1 - y], (x, y) => [w - 1 - x, h - 1 - y]);
        return list;
    }

    // op(t, map) draws into target t, passing every point through map;
    // runs once per mirror image, on dots or on cells
    function draw(op) {
        if (pixel) {
            const low = cells();
            const before = low.slice();
            const t = target(low, CW, CH);
            mirrors(CW, CH).forEach((m) => op(t, m, CELL));
            writeCells(low, before);
        } else {
            const t = target(base, W, H);
            mirrors(W, H).forEach((m) => op(t, m, 1));
        }
    }

    // a point of the sticker in the target's units
    const unit = (v, scale) => Math.floor(v / scale);

    function disk(t, x, y, r, v) {
        const rr = r * r + r * 0.8;
        for (let dy = -r; dy <= r; dy++) {
            for (let dx = -r; dx <= r; dx++) {
                if (dx * dx + dy * dy <= rr) t.set(x + dx, y + dy, v);
            }
        }
    }

    function line(t, x0, y0, x1, y1, r, v) {
        const steps = Math.max(Math.abs(x1 - x0), Math.abs(y1 - y0), 1);
        for (let i = 0; i <= steps; i++) {
            disk(t, Math.round(x0 + (x1 - x0) * i / steps), Math.round(y0 + (y1 - y0) * i / steps), r, v);
        }
    }

    function fillBox(t, x0, y0, x1, y1, inside, pat) {
        const box = { x0, x1, y0, y1 };
        for (let y = Math.max(0, y0); y <= Math.min(t.h - 1, y1); y++) {
            for (let x = Math.max(0, x0); x <= Math.min(t.w - 1, x1); x++) {
                if (inside(x, y)) t.set(x, y, PATTERNS[pat][1](x, y, box));
            }
        }
    }

    function flood(t, sx, sy, pat) {
        const from = t.get(sx, sy);
        if (from < 0) return;
        const seen = new Uint8Array(t.w * t.h);
        const stack = [sx, sy];
        const region = [];
        const box = { x0: sx, x1: sx, y0: sy, y1: sy };
        while (stack.length) {
            const y = stack.pop(), x = stack.pop();
            if (x < 0 || y < 0 || x >= t.w || y >= t.h) continue;
            const i = y * t.w + x;
            if (seen[i] || t.data[i] !== from) continue;
            seen[i] = 1;
            region.push(x, y);
            box.x0 = Math.min(box.x0, x); box.x1 = Math.max(box.x1, x);
            box.y0 = Math.min(box.y0, y); box.y1 = Math.max(box.y1, y);
            stack.push(x + 1, y, x - 1, y, x, y + 1, x, y - 1);
        }
        for (let i = 0; i < region.length; i += 2) t.set(region[i], region[i + 1], PATTERNS[pat][1](region[i], region[i + 1], box));
    }

    // a shape from (x0, y0) to (x1, y1), in the target's units
    function shapeOp(t, a, b, r) {
        const [x0, y0] = a, [x1, y1] = b;
        const lx = Math.min(x0, x1), hx = Math.max(x0, x1), ly = Math.min(y0, y1), hy = Math.max(y0, y1);
        if (shape === 'line') { line(t, x0, y0, x1, y1, r, 1); return; }
        if (shape === 'rect') {
            if (filled) fillBox(t, lx, ly, hx, hy, () => true, pattern);
            line(t, lx, ly, hx, ly, r, 1); line(t, hx, ly, hx, hy, r, 1);
            line(t, hx, hy, lx, hy, r, 1); line(t, lx, hy, lx, ly, r, 1);
            return;
        }
        const cx = (lx + hx) / 2, cy = (ly + hy) / 2, rx = Math.max(0.5, (hx - lx) / 2), ry = Math.max(0.5, (hy - ly) / 2);
        if (filled) fillBox(t, lx, ly, hx, hy, (x, y) => ((x - cx) / rx) ** 2 + ((y - cy) / ry) ** 2 <= 1, pattern);
        const n = Math.max(24, Math.ceil(Math.PI * (rx + ry) * 1.5));
        for (let i = 0; i < n; i++) {
            const angle = i / n * Math.PI * 2;
            disk(t, Math.round(cx + rx * Math.cos(angle)), Math.round(cy + ry * Math.sin(angle)), r, 1);
        }
    }

    // ---- floating pictures, text and photos -----------------------------

    // the floating object as a patch of dots: 1 black, 0 white, 2 nothing
    function patch(f) {
        const c = document.createElement('canvas');
        const g = c.getContext('2d', { willReadFrequently: true });
        let w, h;
        if (f.kind === 'photo') {
            w = Math.max(8, Math.round(W * f.size / 260));
            h = Math.max(8, Math.round(w * f.img.height / f.img.width));
            c.width = w; c.height = h;
            g.drawImage(f.img, 0, 0, w, h);
            return { w, h, data: dither(g.getImageData(0, 0, w, h).data, w, h, f.brightness) };
        }
        const font = f.kind === 'stamp' ? `${f.size}px bootstrap-icons` : `${Math.round(f.size * 0.6)}px '${f.font}'`;
        const text = f.kind === 'stamp' ? String.fromCodePoint(f.code) : f.text;
        g.font = font;
        const m = g.measureText(text);
        w = Math.max(1, Math.ceil(m.actualBoundingBoxLeft + m.actualBoundingBoxRight) + 2);
        h = Math.max(1, Math.ceil(m.actualBoundingBoxAscent + m.actualBoundingBoxDescent) + 2);
        c.width = w; c.height = h;
        g.font = font;
        g.fillStyle = '#000';
        g.fillText(text, m.actualBoundingBoxLeft + 1, m.actualBoundingBoxAscent + 1);
        const px = g.getImageData(0, 0, w, h).data;
        const data = new Uint8Array(w * h);
        for (let i = 0; i < w * h; i++) data[i] = px[i * 4 + 3] >= 128 ? f.ink : 2;
        return { w, h, data };
    }

    // a photo in dots: Atkinson's dithering, brightness from -80 to 80
    function dither(px, w, h, brightness) {
        const grey = new Float32Array(w * h);
        let sum = 0;
        for (let i = 0; i < w * h; i++) { grey[i] = px[i * 4] * 0.299 + px[i * 4 + 1] * 0.587 + px[i * 4 + 2] * 0.114; sum += grey[i]; }
        // a little more contrast around the photo's own middle
        const mean = sum / (w * h);
        for (let i = 0; i < w * h; i++) grey[i] = (grey[i] - mean) * 1.3 + 128 + brightness;
        const out = new Uint8Array(w * h);
        const spread = [[1, 0], [2, 0], [-1, 1], [0, 1], [1, 1], [0, 2]];
        for (let y = 0; y < h; y++) {
            for (let x = 0; x < w; x++) {
                const i = y * w + x;
                const v = grey[i] < 128 ? 0 : 255;
                out[i] = v ? 0 : 1;
                const err = (grey[i] - v) / 8;
                spread.forEach(([dx, dy]) => {
                    const xx = x + dx, yy = y + dy;
                    if (xx >= 0 && xx < w && yy < h) grey[yy * w + xx] += err;
                });
            }
        }
        return out;
    }

    let patchCache = null, patchKey = '';
    function floatingPatch() {
        const k = JSON.stringify([floating.kind, floating.size, floating.ink, floating.text, floating.code, floating.font, floating.brightness, floating.id]);
        if (k !== patchKey) { patchKey = k; patchCache = patch(floating); }
        return patchCache;
    }

    function floatBox() {
        const p = floatingPatch();
        return { x: Math.round(floating.x - p.w / 2), y: Math.round(floating.y - p.h / 2), w: p.w, h: p.h };
    }

    // puts the patch into data (the sticker or a copy), mirrored as asked;
    // in pixel mode each dot of a patch at an eighth of the size is a cell
    function stamp(data) {
        const p = floatingPatch();
        const b = floatBox();
        const put = (x, y, v) => { if (v !== 2 && x >= 0 && y >= 0 && x < W && y < H) data[y * W + x] = v; };
        const flips = mirrors(W, H);
        for (let y = 0; y < p.h; y++) {
            for (let x = 0; x < p.w; x++) {
                const v = p.data[y * p.w + x];
                if (v === 2) continue;
                let sx = b.x + x, sy = b.y + y;
                if (pixel) { sx = Math.floor(sx / CELL) * CELL; sy = Math.floor(sy / CELL) * CELL; }
                flips.forEach((m) => {
                    if (pixel) {
                        const [cx, cy] = m(Math.floor(sx / CELL), Math.floor(sy / CELL));
                        for (let yy = 0; yy < CELL; yy++) for (let xx = 0; xx < CELL; xx++) put(cx * CELL + xx, cy * CELL + yy, v);
                    } else {
                        const [mx, my] = m(sx, sy);
                        put(mx, my, v);
                    }
                });
            }
        }
    }

    function commit() {
        if (!floating) return;
        remember();
        stamp(base);
        if (floating.kind === 'photo') photoUsed = true;
        floating = null;
        showFloat();
        render();
    }

    // ---- showing -------------------------------------------------------

    function render() {
        let data = base;
        if (floating) { data = base.slice(); stamp(data); }
        const px = image.data;
        for (let i = 0; i < W * H; i++) {
            const v = data[i] ? 0 : 255;
            px[i * 4] = px[i * 4 + 1] = px[i * 4 + 2] = v;
            px[i * 4 + 3] = 255;
        }
        actx.putImageData(image, 0, 0);
        renderUi();
    }

    // the helpers above the sticker: grid, mirror lines, safe area, the
    // floating object's frame; sharp at the screen's resolution
    function renderUi() {
        const r = ui.getBoundingClientRect();
        const dpr = window.devicePixelRatio || 1;
        if (ui.width !== Math.round(r.width * dpr)) { ui.width = Math.round(r.width * dpr); ui.height = Math.round(r.height * dpr); }
        const sx = ui.width / W, sy = ui.height / H;
        uctx.clearRect(0, 0, ui.width, ui.height);
        uctx.lineWidth = Math.max(1, dpr);
        if (pixel) {
            uctx.strokeStyle = 'rgba(13, 110, 253, 0.18)';
            uctx.beginPath();
            for (let x = 1; x < CW; x++) { uctx.moveTo(Math.round(x * CELL * sx) + 0.5, 0); uctx.lineTo(Math.round(x * CELL * sx) + 0.5, ui.height); }
            for (let y = 1; y < CH; y++) { uctx.moveTo(0, Math.round(y * CELL * sy) + 0.5); uctx.lineTo(ui.width, Math.round(y * CELL * sy) + 0.5); }
            uctx.stroke();
        }
        uctx.setLineDash([6 * dpr, 5 * dpr]);
        uctx.strokeStyle = 'rgba(13, 110, 253, 0.45)';
        uctx.strokeRect(16 * sx, 16 * sy, (W - 32) * sx, (H - 32) * sy);
        if (mirror >= 1) {
            uctx.strokeStyle = 'rgba(214, 51, 132, 0.55)';
            uctx.beginPath();
            uctx.moveTo(ui.width / 2, 0); uctx.lineTo(ui.width / 2, ui.height);
            if (mirror >= 2) { uctx.moveTo(0, ui.height / 2); uctx.lineTo(ui.width, ui.height / 2); }
            uctx.stroke();
        }
        if (floating) {
            const b = floatBox();
            uctx.setLineDash([4 * dpr, 4 * dpr]);
            uctx.strokeStyle = 'rgba(13, 110, 253, 0.9)';
            uctx.lineWidth = 2 * dpr;
            uctx.strokeRect((b.x - 3) * sx, (b.y - 3) * sy, (b.w + 6) * sx, (b.h + 6) * sy);
        }
        uctx.setLineDash([]);
    }

    function remember() {
        undo.push(base.slice());
        if (undo.length > 40) undo.shift();
    }

    // ---- the pointer -----------------------------------------------------

    function point(e) {
        const r = art.getBoundingClientRect();
        return [Math.max(0, Math.min(W - 1, Math.floor((e.clientX - r.left) * W / r.width))),
            Math.max(0, Math.min(H - 1, Math.floor((e.clientY - r.top) * H / r.height)))];
    }

    const radius = (scale) => (scale > 1 ? [0, 0, 1, 1][width] : [0, 1, 3, 7][width]);

    art.addEventListener('pointerdown', (e) => {
        e.preventDefault();
        art.setPointerCapture(e.pointerId);
        const [x, y] = point(e);
        if (floating) {
            const b = floatBox();
            if (x < b.x - 6 || y < b.y - 6 || x > b.x + b.w + 6 || y > b.y + b.h + 6) { floating.x = x; floating.y = y; }
            gesture = { kind: 'move', dx: floating.x - x, dy: floating.y - y };
            render();
            return;
        }
        if (tool === 'stamp' || tool === 'text' || tool === 'photo' || tool === 'frame') return;
        remember();
        if (tool === 'fill') {
            draw((t, m, s) => { const [mx, my] = m(unit(x, s), unit(y, s)); flood(t, mx, my, pattern); });
            render();
            return;
        }
        if (tool === 'shape') {
            gesture = { kind: 'shape', from: [x, y], start: base.slice() };
            return;
        }
        const v = tool === 'eraser' ? 0 : 1;
        gesture = { kind: 'stroke', last: [x, y] };
        draw((t, m, s) => { const [mx, my] = m(unit(x, s), unit(y, s)); disk(t, mx, my, radius(s), v); });
        render();
    });

    art.addEventListener('pointermove', (e) => {
        if (!gesture) return;
        const [x, y] = point(e);
        if (gesture.kind === 'move') {
            floating.x = x + gesture.dx;
            floating.y = y + gesture.dy;
        } else if (gesture.kind === 'stroke') {
            const [lx, ly] = gesture.last;
            const v = tool === 'eraser' ? 0 : 1;
            draw((t, m, s) => {
                const [ax, ay] = m(unit(lx, s), unit(ly, s));
                const [bx, by] = m(unit(x, s), unit(y, s));
                line(t, ax, ay, bx, by, radius(s), v);
            });
            gesture.last = [x, y];
        } else if (gesture.kind === 'shape') {
            base = gesture.start.slice();
            const [fx, fy] = gesture.from;
            draw((t, m, s) => shapeOp(t, m(unit(fx, s), unit(fy, s)), m(unit(x, s), unit(y, s)), radius(s)));
        }
        render();
    });

    const end = () => { gesture = null; };
    art.addEventListener('pointerup', end);
    art.addEventListener('pointercancel', end);

    // ---- the panel -------------------------------------------------------

    function pressed(group, button, attr = 'aria-pressed') {
        group.querySelectorAll('button').forEach((b) => {
            if (attr === 'active') b.classList.toggle('active', b === button);
            else b.setAttribute(attr, b === button ? 'true' : 'false');
        });
    }

    function showFloat() {
        const box = $('.lb-float');
        box.hidden = !floating;
        if (!floating) return;
        $('.lb-size').value = floating.size;
        $('.lb-brightness-row').hidden = floating.kind !== 'photo';
        $('.lb-ink').hidden = floating.kind === 'photo';
        pressed($('.lb-ink'), $(`.lb-ink [data-ink='${floating.ink}']`), 'active');
    }

    function place(f) {
        commit();
        floating = Object.assign({ x: W / 2, y: H / 2, ink: 1, id: Math.random() }, f);
        showFloat();
        render();
    }

    function selectTool(name) {
        commit();
        tool = name;
        pressed($('.lb-palette'), $(`.lb-palette [data-tool='${name}']`));
        document.querySelectorAll('.lb-opt[data-for]').forEach((o) => { o.hidden = !o.dataset.for.split(' ').includes(tool); });
        if (name === 'text') $('.lb-text').focus();
    }

    $('.lb-palette').addEventListener('click', (e) => {
        const b = e.target.closest('[data-tool]');
        if (b) selectTool(b.dataset.tool);
    });

    $('.lb-actions').addEventListener('click', (e) => {
        const b = e.target.closest('button');
        if (!b) return;
        const action = b.dataset.action;
        if (action === 'pixel') {
            pixel = !pixel;
            b.classList.toggle('active', pixel);
            b.setAttribute('aria-pressed', pixel ? 'true' : 'false');
        } else if (action === 'mirror') {
            mirror = (mirror + 1) % 3;
            b.classList.toggle('active', mirror > 0);
            b.querySelector('.lb-mirror-label').textContent = `Spiegeln: ${['aus', 'zwei', 'vier'][mirror]}`;
        } else if (action === 'invert') {
            commit();
            remember();
            for (let i = 0; i < base.length; i++) base[i] ^= 1;
        } else if (action === 'undo') {
            if (floating) { floating = null; showFloat(); } else if (undo.length) base = undo.pop();
        } else if (action === 'clear') {
            floating = null;
            showFloat();
            remember();
            base = new Uint8Array(W * H);
            photoUsed = false;
        }
        render();
    });

    $('.lb-panel').addEventListener('click', (e) => {
        const b = e.target.closest('button');
        if (!b) return;
        if (b.dataset.width) { width = +b.dataset.width; pressed(b.parentElement, b, 'active'); }
        if (b.dataset.shape) { shape = b.dataset.shape; pressed(b.parentElement, b, 'active'); }
        if (b.dataset.filled) { filled = b.dataset.filled === '1'; pressed(b.parentElement, b, 'active'); }
        if (b.dataset.pattern) { pattern = +b.dataset.pattern; document.querySelectorAll('.lb-swatches').forEach((g) => pressed(g, g.querySelector(`[data-pattern='${pattern}']`))); }
        if (b.dataset.stamp) place({ kind: 'stamp', code: STAMPS[+b.dataset.stamp][1], size: 110 });
        if (b.dataset.frame) { commit(); remember(); FRAMES[+b.dataset.frame][1](target(base, W, H)); render(); }
        if (b.dataset.ink && floating) { floating.ink = +b.dataset.ink; pressed(b.parentElement, b, 'active'); render(); }
        if (b.dataset.float === 'done') commit();
        if (b.dataset.float === 'drop') { floating = null; showFloat(); render(); }
    });

    $('.lb-size').addEventListener('input', (e) => { if (floating) { floating.size = +e.target.value; render(); } });
    $('.lb-brightness').addEventListener('input', (e) => { if (floating) { floating.brightness = +e.target.value; render(); } });

    // text floats as soon as there is some, and follows the typing
    function textChanged() {
        const text = $('.lb-text').value.trim();
        if (!text) { if (floating && floating.kind === 'text') { floating = null; showFloat(); render(); } return; }
        if (floating && floating.kind === 'text') { floating.text = text; floating.font = $('.lb-font').value; render(); return; }
        place({ kind: 'text', text, font: $('.lb-font').value, size: 90 });
    }
    $('.lb-text').addEventListener('input', textChanged);
    $('.lb-font').addEventListener('change', () => { document.fonts.load(`48px '${$('.lb-font').value}'`).then(textChanged, textChanged); });

    $('.lb-photo-input').addEventListener('change', (e) => {
        const file = e.target.files && e.target.files[0];
        if (!file) return;
        const img = new Image();
        img.onload = () => { place({ kind: 'photo', img, size: 120, brightness: 0 }); URL.revokeObjectURL(img.src); };
        img.src = URL.createObjectURL(file);
        e.target.value = '';
    });

    // ---- frames ------------------------------------------------------------

    function textInto(t, text, font, cx, cy, v) {
        const c = document.createElement('canvas');
        c.width = W; c.height = H;
        const g = c.getContext('2d', { willReadFrequently: true });
        g.font = font; g.textAlign = 'center'; g.textBaseline = 'middle'; g.fillStyle = '#000';
        g.fillText(text, cx, cy);
        const px = g.getImageData(0, 0, W, H).data;
        for (let i = 0; i < W * H; i++) if (px[i * 4 + 3] >= 128) t.data[i] = v;
    }

    function rounded(t, x0, y0, x1, y1, rad, r, v) {
        line(t, x0 + rad, y0, x1 - rad, y0, r, v); line(t, x0 + rad, y1, x1 - rad, y1, r, v);
        line(t, x0, y0 + rad, x0, y1 - rad, r, v); line(t, x1, y0 + rad, x1, y1 - rad, r, v);
        [[x0 + rad, y0 + rad, Math.PI], [x1 - rad, y0 + rad, Math.PI * 1.5], [x1 - rad, y1 - rad, 0], [x0 + rad, y1 - rad, Math.PI / 2]].forEach(([cx, cy, a0]) => {
            for (let i = 0; i <= 24; i++) {
                const a = a0 + i / 24 * Math.PI / 2;
                disk(t, Math.round(cx + rad * Math.cos(a)), Math.round(cy + rad * Math.sin(a)), r, v);
            }
        });
    }

    const FRAMES = [
        ['Rand', (t) => rounded(t, 14, 14, W - 15, H - 15, 18, 2, 1)],
        ['Doppelt', (t) => { rounded(t, 12, 12, W - 13, H - 13, 16, 1, 1); rounded(t, 22, 22, W - 23, H - 23, 10, 1, 1); }],
        ['Ticket', (t) => {
            rounded(t, 14, 14, W - 15, H - 15, 12, 2, 1);
            // a notch on each side, as torn from a roll of tickets
            [[14, H / 2], [W - 15, H / 2]].forEach(([cx, cy]) => {
                disk(t, cx, cy, 22, 0);
                for (let i = 0; i <= 40; i++) { const a = i / 40 * Math.PI * 2; disk(t, Math.round(cx + 22 * Math.cos(a)), Math.round(cy + 22 * Math.sin(a)), 1, 1); }
            });
            for (let y = 30; y < H - 30; y += 12) disk(t, W - 110, y, 2, 1);
        }],
        ['Punkte', (t) => {
            for (let x = 18; x <= W - 18; x += 14) { disk(t, x, 18, 3, 1); disk(t, x, H - 19, 3, 1); }
            for (let y = 32; y <= H - 32; y += 14) { disk(t, 18, y, 3, 1); disk(t, W - 19, y, 3, 1); }
        }],
        ['Namensschild', (t) => {
            rounded(t, 12, 12, W - 13, H - 13, 18, 2, 1);
            fillBox(t, 12, 12, W - 13, 92, (x, y) => !(x < 30 && y < 30 && (x - 30) ** 2 + (y - 30) ** 2 > 18 * 18) && !(x > W - 31 && y < 30 && (x - (W - 31)) ** 2 + (y - 30) ** 2 > 18 * 18), 0);
            textInto(t, 'HALLO', "52px 'Patua One'", W / 2, 46, 0);
            textInto(t, 'ich bin', "22px 'IBM Plex Sans'", W / 2, 80, 0);
        }],
        ['Sprechblase', (t) => {
            rounded(t, 14, 14, W - 15, H - 50, 30, 2, 1);
            // the tail: the bubble's border opens, two lines meet below
            line(t, 94, H - 50, 126, H - 50, 2, 0);
            line(t, 92, H - 50, 84, H - 12, 2, 1);
            line(t, 128, H - 50, 84, H - 12, 2, 1);
        }],
    ];

    // ---- thumbnails ----------------------------------------------------------

    function thumb(w, h, data) {
        const c = document.createElement('canvas');
        c.width = w; c.height = h;
        const g = c.getContext('2d');
        const img = g.createImageData(w, h);
        for (let i = 0; i < w * h; i++) {
            const v = data[i] ? 0 : 255;
            img.data[i * 4] = img.data[i * 4 + 1] = img.data[i * 4 + 2] = v;
            img.data[i * 4 + 3] = 255;
        }
        g.putImageData(img, 0, 0);
        return c;
    }

    function buildPanel() {
        document.querySelectorAll('.lb-swatches').forEach((group) => {
            PATTERNS.forEach(([name], i) => {
                const b = document.createElement('button');
                b.type = 'button';
                b.dataset.pattern = i;
                b.title = name;
                b.setAttribute('aria-label', name);
                b.setAttribute('aria-pressed', i === pattern ? 'true' : 'false');
                const data = new Uint8Array(40 * 24);
                const t = target(data, 40, 24);
                fillBox(t, 0, 0, 39, 23, () => true, i);
                b.appendChild(thumb(40, 24, data));
                group.appendChild(b);
            });
        });
        const stamps = $('.lb-stamps');
        STAMPS.forEach(([name], i) => {
            const b = document.createElement('button');
            b.type = 'button';
            b.dataset.stamp = i;
            b.setAttribute('aria-label', name.replace(/-fill$/, '').replace(/-/g, ' '));
            b.innerHTML = `<i class='bi bi-${name}'></i>`;
            stamps.appendChild(b);
        });
    }

    // ---- printing ------------------------------------------------------------

    let browser = '';
    try {
        browser = localStorage.getItem('label-browser') || '';
        if (!browser) { browser = Math.random().toString(36).slice(2) + Date.now().toString(36); localStorage.setItem('label-browser', browser); }
    } catch (e) { browser = Math.random().toString(36).slice(2); }

    function post(url, data) {
        return fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(data) })
            .then((r) => r.json().catch(() => ({})).then((body) => ({ status: r.status, body })));
    }

    function bits() {
        let hex = '';
        for (let y = 0; y < H; y++) {
            for (let bx = 0; bx < W / 8; bx++) {
                let byte = 0;
                for (let b = 0; b < 8; b++) if (base[y * W + bx * 8 + b]) byte |= 0x80 >> b;
                hex += byte.toString(16).padStart(2, '0');
            }
        }
        return hex;
    }

    const status = $('.lb-status');
    const send = $('.lb-send');
    const say = (html) => { status.innerHTML = html; };

    function follow(id) {
        post('/api/labels/status', { key, id }).then(({ status: code, body }) => {
            if (code === 404 && body.error === 'gone' && follow.photo) { say("<i class='bi bi-check-lg'></i> Gedruckt! Hol dir deinen Aufkleber am Drucker."); send.disabled = false; return; }
            if (code !== 200) { say('Der Aufkleber ist nicht mehr in der Warteschlange.'); send.disabled = false; return; }
            if (body.state === 'printed') {
                say("<i class='bi bi-check-lg'></i> Gedruckt! Hol dir deinen Aufkleber am Drucker.");
                send.disabled = false;
                return;
            }
            if (body.state === 'printing') say("<i class='bi bi-printer'></i> Wird gedruckt …");
            else if (!body.station) say('Dein Aufkleber wartet. Der Drucker ist gerade nicht verbunden; sag am Stand Bescheid.');
            else say(body.ahead > 0 ? `Dein Aufkleber wartet, ${body.ahead === 1 ? 'einer ist' : `${body.ahead} sind`} vor dir dran.` : 'Dein Aufkleber ist als Nächster dran.');
            setTimeout(() => follow(id), 2000);
        });
    }

    send.addEventListener('click', () => {
        commit();
        if (!base.some((v) => v)) { say('Der Aufkleber ist noch leer.'); return; }
        send.disabled = true;
        say('Wird geschickt …');
        follow.photo = photoUsed;
        post('/api/labels/print', Object.assign({ key, browser, bits: bits() }, photoUsed ? { photo: true } : {})).then(({ status: code, body }) => {
            if (code === 200) { follow(body.id); return; }
            send.disabled = false;
            if (body.error === 'wait') say(`Einen Moment noch: In ${body.seconds} Sekunden kannst du den nächsten drucken.`);
            else if (body.error === 'busy') say('Gerade warten sehr viele Aufkleber. Versuch es gleich noch einmal.');
            else if (body.error === 'labels_off' || body.error === 'wrong_key') say('Gerade druckt hier nichts mehr.');
            else say('Das hat nicht geklappt. Versuch es noch einmal.');
        }).catch(() => { send.disabled = false; say('Keine Verbindung. Versuch es noch einmal.'); });
    });

    // ---- start ---------------------------------------------------------------

    buildPanel();
    window.addEventListener('resize', renderUi);
    // the frames' thumbnails use the fonts
    function buildFrames() {
        const frames = $('.lb-frames');
        frames.innerHTML = '';
        FRAMES.forEach(([name, drawFrame], i) => {
            const b = document.createElement('button');
            b.type = 'button';
            b.dataset.frame = i;
            b.title = name;
            b.setAttribute('aria-label', `Rahmen ${name}`);
            const data = new Uint8Array(W * H);
            drawFrame(target(data, W, H));
            const canvas = thumb(W, H, data);
            canvas.style.imageRendering = 'auto';
            b.appendChild(canvas);
            frames.appendChild(b);
        });
    }
    Promise.all(['64px bootstrap-icons', "48px 'Patua One'", "48px 'Pixelify Sans'", "48px 'IBM Plex Sans'", "48px '0xProto'"]
        .map((f) => document.fonts.load(f))).catch(() => {}).then(buildFrames);
    render();
    post('/api/labels/open', { key }).then(({ body }) => {
        if (body.on && body.key_ok) { $('.lb-app').hidden = false; render(); return; }
        const closed = $('.lb-closed');
        closed.hidden = false;
        closed.textContent = body.on
            ? 'Aufkleber druckt nur, wer den QR-Code am Stand scannt.'
            : 'Gerade druckt hier nichts. Am Tag der offenen Tür steht hier ein Aufkleberdrucker.';
    });
}());
