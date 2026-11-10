// The block-matching widget (»Video komprimieren«): a block of the frame
// now and the frame before it. One drags the block over the frame now (it
// snaps to the grid of blocks; a tap or the arrow keys work too); in the
// frame before, a square of the same size can be dragged (mouse, finger, or
// the arrow keys) within the search area around the block's own place.
// The square carries the block as an onion skin, half transparent, so that
// it merges with the picture where it fits and shows doubled edges where
// it does not. Beside them, enlarged: the block, the square under it and
// their difference with its sign (orange where the block is brighter, blue
// where it is darker, black where they agree), with the sum of the differences (SAD) and the vector (dx, dy)
// from the block to the square. »Beste Verschiebung suchen« tries every
// shift in the search area, puts the square on the best one and shows the
// sum of every shift as a small map, the darker the smaller, the best ringed
// yellow and the reader's own square white; a second line compares the
// reader's square with the search.
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
        const zoomBlock = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Der Block, vergrößert' });
        const zoomCandidate = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Das Quadrat im Bild davor, vergrößert' });
        const zoomDiff = html('canvas', { class: 'motion-zoom', width: N * ZOOM, height: N * ZOOM, role: 'img', 'aria-label': 'Der Unterschied, vergrößert' });
        const map = html('canvas', { class: 'motion-zoom', width: (2 * R + 1) * 4, height: (2 * R + 1) * 4, role: 'img', 'aria-label': 'Die Summe der Unterschiede für jede Verschiebung' });
        const mapFigure = html('figure', { class: 'motion-figure', hidden: '' }, [map, html('figcaption', { text: 'Summe für jede Verschiebung, dunkel ist klein' })]);
        const search = html('button', { type: 'button', class: 'btn btn-sm btn-outline-secondary', text: 'Beste Verschiebung suchen' });
        const say = html('p', { class: 'motion-say', 'aria-live': 'polite' });
        const compare = html('p', { class: 'motion-compare', 'aria-live': 'polite' });
        el.replaceChildren(
            html('div', { class: 'motion-frames' }, [
                html('figure', { class: 'motion-figure' }, [nowCanvas, html('figcaption', { text: `${nowImg.dataset.label || 'jetzt'}: den Block ziehen` })]),
                html('figure', { class: 'motion-figure' }, [beforeCanvas, html('figcaption', { text: `${beforeImg.dataset.label || 'vorher'}: das Quadrat ziehen` })]),
            ]),
            html('div', { class: 'motion-controls' }, [search, html('div', {}, [say, compare])]),
            html('div', { class: 'motion-zooms' }, [
                html('figure', { class: 'motion-figure' }, [zoomBlock, html('figcaption', { text: 'Block' })]),
                html('figure', { class: 'motion-figure' }, [zoomCandidate, html('figcaption', { text: 'Quadrat davor' })]),
                html('figure', { class: 'motion-figure' }, [zoomDiff, html('figcaption', { text: 'Unterschied: orange heller, blau dunkler' })]),
                mapFigure,
            ]));

        let now = null, before = null;
        let bx = 0, by = 0;            // the block's top left corner
        let vy = 0, vx = 0;            // the vector from the block to the square
        let sums = null;               // the sum for every shift, after a search
        let manual = null;             // the reader's square when they searched: { vx, vy, s }
        let found = null;              // what the search found: { vx, vy, s }

        const sad = (dy, dx) => {
            let s = 0;
            for (let y = 0; y < N; y++) {
                const a = (by + y) * now.width + bx, b = (by + dy + y) * before.width + bx + dx;
                for (let x = 0; x < N; x++) s += Math.abs(now.luma[a + x] - before.luma[b + x]);
            }
            return Math.round(s);
        };
        const fits = (dy, dx) => by + dy >= 0 && bx + dx >= 0 && by + dy + N <= before.height && bx + dx + N <= before.width;

        function drawFrame(canvas, source, square, dashed) {
            const ctx = canvas.getContext('2d');
            ctx.drawImage(source.canvas, 0, 0);
            const purple = getComputedStyle(el).getPropertyValue('--motion-accent').trim() || '#6741d9';
            if (dashed) {
                // the search area: the rest of the picture darker
                ctx.fillStyle = 'rgba(0, 0, 0, 0.45)';
                ctx.beginPath();
                ctx.rect(0, 0, canvas.width, canvas.height);
                ctx.rect(bx - R, by - R, N + 2 * R, N + 2 * R);
                ctx.fill('evenodd');
                ctx.setLineDash([4, 3]);
                ctx.strokeStyle = 'rgba(255, 255, 255, 0.9)';
                ctx.lineWidth = 1;
                ctx.strokeRect(bx - R + 0.5, by - R + 0.5, N + 2 * R - 1, N + 2 * R - 1);
                ctx.setLineDash([]);
                // the onion skin: the block of the frame now, half transparent
                ctx.globalAlpha = 0.5;
                ctx.drawImage(now.canvas, bx, by, N, N, square[0], square[1], N, N);
                ctx.globalAlpha = 1;
            } else {
                ctx.strokeStyle = 'rgba(255, 255, 255, 0.18)';
                ctx.lineWidth = 1;
                for (let x = R; x <= canvas.width - R; x += N) { ctx.beginPath(); ctx.moveTo(x + 0.5, R); ctx.lineTo(x + 0.5, canvas.height - R); ctx.stroke(); }
                for (let y = R; y <= canvas.height - R; y += N) { ctx.beginPath(); ctx.moveTo(R, y + 0.5); ctx.lineTo(canvas.width - R, y + 0.5); ctx.stroke(); }
            }
            ctx.lineWidth = 2;
            ctx.strokeStyle = '#ffffff';
            ctx.strokeRect(square[0] - 1, square[1] - 1, N + 2, N + 2);
            ctx.strokeStyle = purple;
            ctx.strokeRect(square[0] + 1, square[1] + 1, N - 2, N - 2);
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

        function drawMap() {
            if (!found) { mapFigure.hidden = true; return; }
            mapFigure.hidden = false;
            const ctx = map.getContext('2d');
            const cell = map.width / (2 * R + 1);
            let lo = Infinity, hi = 0;
            for (const s of sums) if (s !== null) { lo = Math.min(lo, s); hi = Math.max(hi, s); }
            sums.forEach((s, i) => {
                const dy = Math.floor(i / (2 * R + 1)) - R, dx = (i % (2 * R + 1)) - R;
                // from deep purple (smallest) to a pale violet (largest)
                const t = s === null ? 1 : Math.sqrt((s - lo) / Math.max(1, hi - lo));
                const r = Math.round(103 + (243 - 103) * t), g = Math.round(65 + (240 - 65) * t), b = Math.round(217 + (255 - 217) * t);
                ctx.fillStyle = s === null ? '#9ca3af' : `rgb(${r}, ${g}, ${b})`;
                ctx.fillRect((dx + R) * cell, (dy + R) * cell, cell, cell);
            });
            // rings large enough to find: the reader's square white, the best yellow
            const ring = (x, y, colour) => {
                ctx.lineWidth = 2;
                ctx.strokeStyle = colour;
                ctx.beginPath();
                ctx.arc((x + R + 0.5) * cell, (y + R + 0.5) * cell, 2.5 * cell, 0, 2 * Math.PI);
                ctx.stroke();
            };
            if (manual && (manual.vx !== found.vx || manual.vy !== found.vy)) ring(manual.vx, manual.vy, '#ffffff');
            ring(found.vx, found.vy, '#facc15');
        }

        function draw() {
            if (!now) return;
            drawFrame(nowCanvas, now, [bx, by], false);
            drawFrame(beforeCanvas, before, [bx + vx, by + vy], true);
            drawZoom(zoomBlock, (x, y) => now.luma[(by + y) * now.width + bx + x]);
            drawZoom(zoomCandidate, (x, y) => before.luma[(by + vy + y) * before.width + bx + vx + x]);
            drawSigned(zoomDiff, (x, y) => now.luma[(by + y) * now.width + bx + x] - before.luma[(by + vy + y) * before.width + bx + vx + x]);
            drawMap();
            say.textContent = `Vektor (${signed(vx)}, ${signed(vy)}): Summe der Unterschiede ${de(sad(vy, vx))}.`;
            if (!found) {
                compare.textContent = '';
            } else {
                const count = de(sums.filter((s) => s !== null).length);
                const vector = (v) => `(${signed(v.vx)}, ${signed(v.vy)})`;
                compare.textContent = manual.s === found.s
                    ? `Du hast die beste Verschiebung gefunden: ${vector(found)} mit ${de(found.s)}, die kleinste von ${count}.`
                    : `Dein Quadrat: ${vector(manual)} mit ${de(manual.s)}. Die Suche: ${vector(found)} mit ${de(found.s)}, die kleinste von ${count}.`;
            }
        }

        function choose(x, y) {
            bx = Math.max(R, Math.min(now.width - R - N, Math.floor(x / N) * N));
            by = Math.max(R, Math.min(now.height - R - N, Math.floor(y / N) * N));
            vx = 0; vy = 0; sums = null; manual = null; found = null;
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
        nowCanvas.addEventListener('pointermove', (event) => { if (holding) follow(event); });
        nowCanvas.addEventListener('pointerup', () => { holding = false; });
        nowCanvas.addEventListener('pointercancel', () => { holding = false; });
        nowCanvas.addEventListener('keydown', (event) => {
            const step = { ArrowUp: [0, -N], ArrowDown: [0, N], ArrowLeft: [-N, 0], ArrowRight: [N, 0] }[event.key];
            if (!step || !now) return;
            event.preventDefault();
            choose(bx + step[0], by + step[1]);
        });

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
        beforeCanvas.addEventListener('keydown', (event) => {
            const step = { ArrowUp: [-1, 0], ArrowDown: [1, 0], ArrowLeft: [0, -1], ArrowRight: [0, 1] }[event.key];
            if (!step) return;
            event.preventDefault();
            move(vy + step[0], vx + step[1]);
        });

        search.addEventListener('click', () => {
            if (!now) return;
            sums = [];
            let best = null;
            for (let dy = -R; dy <= R; dy++) {
                for (let dx = -R; dx <= R; dx++) {
                    const s = fits(dy, dx) ? sad(dy, dx) : null;
                    sums.push(s);
                    if (s !== null && (best === null || s < best[0])) best = [s, dy, dx];
                }
            }
            manual = { vx, vy, s: sad(vy, vx) };
            found = { vx: best[2], vy: best[1], s: best[0] };
            vy = best[1]; vx = best[2];
            draw();
        });

        const ready = (img) => (img.complete && img.naturalWidth ? Promise.resolve() : new Promise((done) => img.addEventListener('load', done, { once: true })));
        Promise.all([ready(nowImg), ready(beforeImg)]).then(() => {
            now = pixels(nowImg);
            before = pixels(beforeImg);
            for (const c of [nowCanvas, beforeCanvas]) { c.width = now.width; c.height = now.height; }
            const [x, y] = (el.dataset.block || '').split(/\s+/).map(Number);
            choose(Number.isFinite(x) ? x : now.width / 2, Number.isFinite(y) ? y : now.height / 2);
        });
    }

    const go = () => document.querySelectorAll('.motion-widget').forEach((el) => {
        if (!el.dataset.ready) { el.dataset.ready = '1'; mount(el); }
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', go);
    else go();
})();
