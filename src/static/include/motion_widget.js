// The block-matching widget (»Video komprimieren«): a block of the frame
// now and the frame before it. One drags the block over the frame now (it
// snaps to the grid of blocks; a tap or the arrow keys work too); in the
// frame before, a square of the same size can be dragged (mouse, finger, or
// the arrow keys) within the search area around the block's own place.
// Grid and frames lie on a canvas in device pixels over each picture, at
// whole pixels, outside the pixels they mark: the block and the square in
// the widget's purple, the block under the mouse dashed, the search area
// dashed. Beside them, enlarged: the block, the square under it and
// their difference with its sign (orange where the block is brighter, blue
// where it is darker, black where they agree), with the sum of the differences (SAD) and the vector (dx, dy)
// from the block to the square. »Beste Verschiebung suchen« tries every
// shift in the search area and leaves the reader's square where it is: the
// best shift gets a yellow frame of its own in the frame before, and the
// sum of every shift shows as a small map, the darker the smaller, the best
// ringed yellow and the reader's own square white; a click on the map puts
// the square on that shift. A second line compares the reader's square
// with the search.
// Once the reader has touched the widget (until they click elsewhere), the
// arrow keys move the square by a pixel and, with Shift, the block by a
// block, wherever the focus is; before that they scroll the page as usual.
//
//   <div class='motion-widget' data-block='96 48'>
//   <img data-noconvert src='motion-after.png' alt='…' data-label='jetzt'>
//   <img data-noconvert src='motion-before.png' alt='…' data-label='vorher'></div>
//
// The pictures are the <img>s inside: first the frame now, then the frame
// before, of the same size. data-block is the block shown first (x and y of
// its top left corner, multiples of 16). Brightness is Y as on the page:
// 0,299 R + 0,587 G + 0,114 B.
(function () {
    'use strict';
    const N = 16;                // the size of a block
    const R = 16;                // how far the search goes in each direction
    const ZOOM = 8;
    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) if (k === 'text') node.textContent = v; else node.setAttribute(k, v);
        for (const child of children) node.appendChild(typeof child === 'string' ? document.createTextNode(child) : child);
        return node;
    };
    const de = (n) => n.toLocaleString('de-DE');
    const signed = (n) => (n > 0 ? `+${n}` : n < 0 ? `−${-n}` : '0');

    function pixels(img) {
        const c = document.createElement('canvas');
        c.width = img.naturalWidth;
        c.height = img.naturalHeight;
        const ctx = c.getContext('2d', { willReadFrequently: true });
        ctx.drawImage(img, 0, 0);
        const rgba = ctx.getImageData(0, 0, c.width, c.height).data;
        const luma = new Float32Array(c.width * c.height);
        for (let i = 0; i < luma.length; i++) luma[i] = 0.299 * rgba[4 * i] + 0.587 * rgba[4 * i + 1] + 0.114 * rgba[4 * i + 2];
        return { canvas: c, luma, width: c.width, height: c.height };
    }

    function mount(el) {
        const [nowImg, beforeImg] = el.querySelectorAll('img');
        if (!nowImg || !beforeImg) return;
        const nowCanvas = html('canvas', { class: 'motion-frame motion-drag', tabindex: '0', role: 'img',
            'aria-label': `${nowImg.alt} Ein Block ist markiert; er lässt sich ziehen oder mit den Pfeiltasten verschieben.` });
        const beforeCanvas = html('canvas', { class: 'motion-frame motion-drag', tabindex: '0', role: 'img',
            'aria-label': `${beforeImg.alt} Ein Quadrat lässt sich ziehen oder mit den Pfeiltasten verschieben.` });
        const nowOverlay = html('canvas', { class: 'motion-overlay', 'aria-hidden': 'true' });
        const beforeOverlay = html('canvas', { class: 'motion-overlay', 'aria-hidden': 'true' });
        const zoomBlock = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Der Block, vergrößert' });
        const zoomCandidate = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Das Quadrat im Bild davor, vergrößert' });
        const zoomDiff = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Der Unterschied, vergrößert' });
        const map = html('canvas', { class: 'motion-zoom motion-map', width: (2 * R + 1) * 4, height: (2 * R + 1) * 4, role: 'img', 'aria-label': 'Die Summe der Unterschiede für jede Verschiebung; ein Klick setzt das Quadrat auf diese Verschiebung' });
        const mapFigure = html('figure', { class: 'motion-figure' }, [map, html('figcaption', { text: 'Karte' })]);
        const search = html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', text: 'Beste Verschiebung suchen' });
        // the reader's square and the best shift, in a small table that keeps
        // its size; a sentence says what the search does
        const cellOf = () => html('span', { class: 'motion-num' });
        const mineVector = cellOf(), mineSum = cellOf(), bestVector = cellOf(), bestSum = cellOf();
        const mineMark = html('i', { class: 'bi bi-check-lg motion-same', 'aria-hidden': 'true', hidden: '' });
        const stats = html('div', { class: 'motion-stats', role: 'group', 'aria-live': 'polite' }, [
            html('span', { class: 'motion-head', text: '' }), html('span', { class: 'motion-head', text: 'Vektor' }), html('span', { class: 'motion-head', text: 'Summe der Unterschiede' }),
            html('span', { class: 'motion-row' }, ['Dein Quadrat ', mineMark]), mineVector, mineSum,
            html('span', { class: 'motion-row', text: 'Beste Verschiebung' }), bestVector, bestSum,
        ]);
        const count = (2 * R + 1) * (2 * R + 1);
        const explain = html('p', { class: 'motion-explain', text: `Die Suche probiert alle ${2 * R + 1} × ${2 * R + 1} = ${de(count)} Verschiebungen im Suchfeld aus, bis zu ${R} Pixel in jede Richtung, und nimmt die mit der kleinsten Summe.` });
        // places to start: blocks whose motion differs (data-presets='Name: x y | …')
        const presets = (el.dataset.presets || '').split('|').map((p) => p.trim()).filter(Boolean).map((p) => {
            const [name, where] = p.split(':');
            const [x, y] = where.trim().split(/\s+/).map(Number);
            const button = html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', text: name.trim() });
            button.addEventListener('click', () => { if (now) choose(x, y); });
            return button;
        });
        const legend = html('p', { class: 'motion-legend', text: 'Unterschied: orange ist der Block heller, blau dunkler, schwarz gleich. Karte: die Summe für jede Verschiebung, dunkel ist klein, gelb die beste, weiß dein Quadrat; ein Klick setzt dein Quadrat dorthin.' });
        el.replaceChildren(
            html('div', { class: 'motion-frames' }, [
                html('figure', { class: 'motion-figure' }, [html('div', { class: 'motion-wrap' }, [nowCanvas, nowOverlay]), html('figcaption', { text: `${nowImg.dataset.label || 'jetzt'}: den Block ziehen` })]),
                html('figure', { class: 'motion-figure' }, [html('div', { class: 'motion-wrap' }, [beforeCanvas, beforeOverlay]), html('figcaption', { text: `${beforeImg.dataset.label || 'vorher'}: das Quadrat ziehen` })]),
            ]),
            ...(presets.length ? [html('div', { class: 'motion-presets', role: 'group', 'aria-label': 'Blöcke zum Ausprobieren' }, [html('span', { class: 'motion-presets-label', text: 'Probier:' }), ...presets])] : []),
            html('div', { class: 'motion-controls' }, [search, stats]),
            explain,
            html('div', { class: 'motion-zooms' }, [
                html('figure', { class: 'motion-figure' }, [zoomBlock, html('figcaption', { text: 'Block' })]),
                html('figure', { class: 'motion-figure' }, [zoomCandidate, html('figcaption', { text: 'Quadrat davor' })]),
                html('figure', { class: 'motion-figure' }, [zoomDiff, html('figcaption', { text: 'Unterschied' })]),
                mapFigure,
            ]),
            legend);

        let now = null, before = null;
        let bx = 0, by = 0;            // the block's top left corner
        let vy = 0, vx = 0;            // the vector from the block to the square
        let sums = null;               // the sum for every shift, after a search
        let found = null;              // what the search found: { vx, vy, s }
        let hover = null;              // the block under the mouse in the frame now: its top left corner

        const sad = (dy, dx) => {
            let s = 0;
            for (let y = 0; y < N; y++) {
                const a = (by + y) * now.width + bx, b = (by + dy + y) * before.width + bx + dx;
                for (let x = 0; x < N; x++) s += Math.abs(now.luma[a + x] - before.luma[b + x]);
            }
            return Math.round(s);
        };
        const fits = (dy, dx) => by + dy >= 0 && bx + dx >= 0 && by + dy + N <= before.height && bx + dx + N <= before.width;

        // the picture on its canvas; grid and frames on the canvas over it, in
        // device pixels at whole pixels
        function drawFrame(canvas, overlay, source, square, before, best) {
            canvas.getContext('2d').drawImage(source.canvas, 0, 0);
            const box = canvas.getBoundingClientRect();
            const ratio = window.devicePixelRatio || 1;
            const w = Math.round(box.width * ratio), h = Math.round(box.height * ratio);
            if (!w || !h) return;
            if (overlay.width !== w || overlay.height !== h) { overlay.width = w; overlay.height = h; }
            const g = overlay.getContext('2d');
            g.clearRect(0, 0, w, h);
            const k = w / canvas.width;                       // device pixels per pixel of the picture
            const P = (v) => Math.round(v * k);
            const thick = Math.max(2, Math.round(ratio * 1.5));
            // a frame of `width` device pixels just outside a rectangle, with a dark edge outside it
            // (outset: that many device pixels further out, so that two frames
            // on the same place are both seen)
            const frame = (x, y, size, colour, width, dash, outset = 0) => {
                const r = [P(x) - outset, P(y) - outset, P(x + size) + outset, P(y + size) + outset];
                g.setLineDash([]);
                g.lineWidth = 1;
                g.strokeStyle = 'rgba(0, 0, 0, 0.55)';
                g.strokeRect(r[0] - width - 0.5, r[1] - width - 0.5, r[2] - r[0] + 2 * width + 1, r[3] - r[1] + 2 * width + 1);
                g.lineWidth = width;
                g.strokeStyle = colour;
                g.setLineDash(dash ? [3 * width, 2 * width] : []);
                g.strokeRect(r[0] - width / 2, r[1] - width / 2, r[2] - r[0] + width, r[3] - r[1] + width);
                g.setLineDash([]);
            };
            const purple = getComputedStyle(el).getPropertyValue('--motion-accent').trim() || '#6741d9';
            if (before) {
                // the search area, dashed white with a dark edge
                frame(bx - R, by - R, N + 2 * R, '#ffffff', Math.max(1, Math.round(ratio)), true);
            } else {
                // the grid of blocks: a light and a dark device pixel side by side
                for (let x = R; x <= canvas.width - R; x += N) {
                    g.fillStyle = 'rgba(255, 255, 255, 0.28)'; g.fillRect(P(x) - 1, P(R), 1, P(canvas.height - R) - P(R));
                    g.fillStyle = 'rgba(0, 0, 0, 0.28)'; g.fillRect(P(x), P(R), 1, P(canvas.height - R) - P(R));
                }
                for (let y = R; y <= canvas.height - R; y += N) {
                    g.fillStyle = 'rgba(255, 255, 255, 0.28)'; g.fillRect(P(R), P(y) - 1, P(canvas.width - R) - P(R), 1);
                    g.fillStyle = 'rgba(0, 0, 0, 0.28)'; g.fillRect(P(R), P(y), P(canvas.width - R) - P(R), 1);
                }
                if (hover && (hover[0] !== bx || hover[1] !== by)) frame(hover[0], hover[1], N, purple, thick, true);
            }
            // the best shift, yellow, just outside where the reader's square would be
            if (best) frame(best[0], best[1], N, '#facc15', thick, false, best[0] === square[0] && best[1] === square[1] ? thick + 1 : 0);
            frame(square[0], square[1], N, purple, thick, false);
        }

        // the difference with its sign: orange where the block is brighter,
        // blue where it is darker, black where they agree; full at 40
        function drawSigned(canvas, values) {
            const ctx = canvas.getContext('2d');
            for (let y = 0; y < N; y++) {
                for (let x = 0; x < N; x++) {
                    const d = values(x, y);
                    const t = Math.min(1, Math.abs(d) / 40);
                    const [r, g, b] = d > 0 ? [245, 158, 11] : [59, 130, 246];
                    ctx.fillStyle = `rgb(${Math.round(r * t)}, ${Math.round(g * t)}, ${Math.round(b * t)})`;
                    ctx.fillRect(x * ZOOM, y * ZOOM, ZOOM, ZOOM);
                }
            }
        }

        function drawZoom(canvas, values) {
            const ctx = canvas.getContext('2d');
            for (let y = 0; y < N; y++) {
                for (let x = 0; x < N; x++) {
                    const v = Math.max(0, Math.min(255, Math.round(values(x, y))));
                    ctx.fillStyle = `rgb(${v}, ${v}, ${v})`;
                    ctx.fillRect(x * ZOOM, y * ZOOM, ZOOM, ZOOM);
                }
            }
        }

        // the map in device pixels, so that its cells and rings are sharp
        function drawMap() {
            const box = map.getBoundingClientRect();
            const ratio = window.devicePixelRatio || 1;
            const size = Math.round(box.width * ratio);
            if (size && map.width !== size) { map.width = size; map.height = size; }
            const ctx = map.getContext('2d');
            // before a search: a quiet empty map, so that nothing moves when it comes
            if (!found) {
                ctx.fillStyle = getComputedStyle(el).getPropertyValue('--widget-chip').trim() || '#ebecef';
                ctx.fillRect(0, 0, map.width, map.height);
                return;
            }
            const count = 2 * R + 1, cell = map.width / count;
            const edge = (i) => Math.round(i * cell);
            let lo = Infinity, hi = 0;
            for (const s of sums) if (s !== null) { lo = Math.min(lo, s); hi = Math.max(hi, s); }
            sums.forEach((s, i) => {
                const dy = Math.floor(i / count) - R, dx = (i % count) - R;
                // from deep purple (smallest) to a pale violet (largest)
                const t = s === null ? 1 : Math.sqrt((s - lo) / Math.max(1, hi - lo));
                const r = Math.round(103 + (243 - 103) * t), g = Math.round(65 + (240 - 65) * t), b = Math.round(217 + (255 - 217) * t);
                ctx.fillStyle = s === null ? '#9ca3af' : `rgb(${r}, ${g}, ${b})`;
                ctx.fillRect(edge(dx + R), edge(dy + R), edge(dx + R + 1) - edge(dx + R), edge(dy + R + 1) - edge(dy + R));
            });
            // rings large enough to find, with a dark edge: the reader's square white, the best yellow
            const ring = (x, y, colour) => {
                const cx = (x + R + 0.5) * cell, cy = (y + R + 0.5) * cell, radius = 2.5 * cell;
                ctx.beginPath();
                ctx.arc(cx, cy, radius, 0, 2 * Math.PI);
                ctx.lineWidth = 3.5 * ratio;
                ctx.strokeStyle = 'rgba(0, 0, 0, 0.35)';
                ctx.stroke();
                ctx.lineWidth = 2 * ratio;
                ctx.strokeStyle = colour;
                ctx.stroke();
            };
            if (vx !== found.vx || vy !== found.vy) ring(vx, vy, '#ffffff');
            ring(found.vx, found.vy, '#facc15');
        }

        function draw() {
            if (!now) return;
            drawFrame(nowCanvas, nowOverlay, now, [bx, by], false);
            drawFrame(beforeCanvas, beforeOverlay, before, [bx + vx, by + vy], true, found ? [bx + found.vx, by + found.vy] : null);
            drawZoom(zoomBlock, (x, y) => now.luma[(by + y) * now.width + bx + x]);
            drawZoom(zoomCandidate, (x, y) => before.luma[(by + vy + y) * before.width + bx + vx + x]);
            drawSigned(zoomDiff, (x, y) => now.luma[(by + y) * now.width + bx + x] - before.luma[(by + vy + y) * before.width + bx + vx + x]);
            drawMap();
            map.classList.toggle('motion-clickable', !!found);
            const vector = (x, y) => `(${signed(x)}, ${signed(y)})`;
            mineVector.textContent = vector(vx, vy);
            mineSum.textContent = de(sad(vy, vx));
            bestVector.textContent = found ? vector(found.vx, found.vy) : '–';
            bestSum.textContent = found ? de(found.s) : '–';
            // a check mark where the reader's square is as good as the best
            mineMark.hidden = !(found && sad(vy, vx) === found.s);
        }

        function choose(x, y) {
            bx = Math.max(R, Math.min(now.width - R - N, Math.floor(x / N) * N));
            by = Math.max(R, Math.min(now.height - R - N, Math.floor(y / N) * N));
            // a new block: no answer until the reader searches again
            vx = 0; vy = 0; sums = null; found = null;
            draw();
        }
        function move(dy, dx) {
            const ny = Math.max(-R, Math.min(R, dy)), nx = Math.max(-R, Math.min(R, dx));
            if (!fits(ny, nx) || (ny === vy && nx === vx)) return;
            vy = ny; vx = nx;
            draw();
        }

        const toPicture = (canvas, event) => {
            const box = canvas.getBoundingClientRect();
            return [(event.clientX - box.left) * canvas.width / box.width, (event.clientY - box.top) * canvas.height / box.height];
        };
        // dragging the block: it follows the pointer from block to block
        let holding = false;
        const follow = (event) => {
            const [x, y] = toPicture(nowCanvas, event);
            const nx = Math.max(R, Math.min(now.width - R - N, Math.floor(x / N) * N));
            const ny = Math.max(R, Math.min(now.height - R - N, Math.floor(y / N) * N));
            if (nx !== bx || ny !== by) choose(nx, ny);
        };
        nowCanvas.addEventListener('pointerdown', (event) => {
            if (!now) return;
            holding = true;
            nowCanvas.setPointerCapture(event.pointerId);
            follow(event);
            event.preventDefault();
        });
        nowCanvas.addEventListener('pointermove', (event) => {
            if (holding) { follow(event); return; }
            if (!now) return;
            // the block under the mouse, dashed
            const [x, y] = toPicture(nowCanvas, event);
            const hx = Math.max(R, Math.min(now.width - R - N, Math.floor(x / N) * N));
            const hy = Math.max(R, Math.min(now.height - R - N, Math.floor(y / N) * N));
            if (!hover || hover[0] !== hx || hover[1] !== hy) { hover = [hx, hy]; draw(); }
        });
        nowCanvas.addEventListener('pointerleave', () => { if (hover) { hover = null; draw(); } });
        nowCanvas.addEventListener('pointerup', () => { holding = false; });
        nowCanvas.addEventListener('pointercancel', () => { holding = false; });

        // dragging the square: it keeps the place where it was grabbed
        let grab = null;
        beforeCanvas.addEventListener('pointerdown', (event) => {
            const [x, y] = toPicture(beforeCanvas, event);
            const sx = bx + vx, sy = by + vy;
            grab = (x >= sx && x < sx + N && y >= sy && y < sy + N) ? [x - sx, y - sy] : [N / 2, N / 2];
            beforeCanvas.setPointerCapture(event.pointerId);
            move(Math.round(y - grab[1]) - by, Math.round(x - grab[0]) - bx);
            event.preventDefault();
        });
        beforeCanvas.addEventListener('pointermove', (event) => {
            if (!grab) return;
            const [x, y] = toPicture(beforeCanvas, event);
            move(Math.round(y - grab[1]) - by, Math.round(x - grab[0]) - bx);
        });
        const release = () => { grab = null; };
        beforeCanvas.addEventListener('pointerup', release);
        beforeCanvas.addEventListener('pointercancel', release);

        // the arrow keys, once the reader has used the widget
        el.motionKey = (dx, dy, block) => {
            if (!now) return;
            if (block) choose(bx + dx * N, by + dy * N);
            else move(vy + dy, vx + dx);
        };
        el.addEventListener('pointerdown', () => { engaged = el; }, true);
        el.addEventListener('focusin', () => { engaged = el; });

        // every shift in the search area: the map and the best one
        function runSearch() {
            sums = [];
            let best = null;
            for (let dy = -R; dy <= R; dy++) {
                for (let dx = -R; dx <= R; dx++) {
                    const s = fits(dy, dx) ? sad(dy, dx) : null;
                    sums.push(s);
                    if (s !== null && (best === null || s < best[0])) best = [s, dy, dx];
                }
            }
            found = { vx: best[2], vy: best[1], s: best[0] };
        }
        search.addEventListener('click', () => {
            if (!now) return;
            runSearch();
            draw();
        });
        // a click on the map: the square on that shift
        map.addEventListener('click', (event) => {
            if (!now || !found) return;
            const box = map.getBoundingClientRect();
            const count = 2 * R + 1;
            const dx = Math.floor((event.clientX - box.left) / box.width * count) - R;
            const dy = Math.floor((event.clientY - box.top) / box.height * count) - R;
            engaged = el;
            move(dy, dx);
        });

        const ready = (img) => (img.complete && img.naturalWidth ? Promise.resolve() : new Promise((done) => img.addEventListener('load', done, { once: true })));
        Promise.all([ready(nowImg), ready(beforeImg)]).then(() => {
            now = pixels(nowImg);
            before = pixels(beforeImg);
            for (const c of [nowCanvas, beforeCanvas]) { c.width = now.width; c.height = now.height; }
            // a new size (window, zoom): grid and frames anew
            if (typeof ResizeObserver !== 'undefined') new ResizeObserver(() => draw()).observe(nowCanvas);
            const [x, y] = (el.dataset.block || '').split(/\s+/).map(Number);
            choose(Number.isFinite(x) ? x : now.width / 2, Number.isFinite(y) ? y : now.height / 2);
        });
    }

    // the widget the keys belong to: the one used last, until a click elsewhere
    let engaged = null;
    document.addEventListener('pointerdown', (event) => {
        if (engaged && !engaged.contains(event.target)) engaged = null;
    }, true);
    // in the capture phase, before stepper_keys.js, which leaves a key alone
    // once it is taken
    window.addEventListener('keydown', (event) => {
        const step = { ArrowUp: [0, -1], ArrowDown: [0, 1], ArrowLeft: [-1, 0], ArrowRight: [1, 0] }[event.key];
        if (!step || !engaged || !engaged.isConnected || event.ctrlKey || event.altKey || event.metaKey) return;
        const target = event.target;
        if (target instanceof HTMLElement && (target.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(target.tagName))) return;
        if (document.querySelector('dialog[open]')) return;
        event.preventDefault();
        event.stopPropagation();
        engaged.motionKey(step[0], step[1], event.shiftKey);
    }, true);

    const go = () => document.querySelectorAll('.motion-widget').forEach((el) => {
        if (!el.dataset.ready) { el.dataset.ready = '1'; mount(el); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
