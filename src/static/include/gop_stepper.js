// »Ein Decoder bei der Arbeit« (»Video komprimieren«, I-, P- und
// B-Frames): 17 frames of slope.mp4, I B B B P B B B I B B B P B B B I, in
// the order they are shown, each built up in steps as a decoder builds it.
//
// Above, a film strip of what the file stores for each frame: an I-frame
// is a picture, a P- or B-frame its residual (grey is 0). It is a cover
// flow of all 17: the frame being built and the frames it looks to pop out,
// flat and large, with their bytes and a bar to scale; the others stand
// stacked between them, turned towards them. Arrows from the frame being built to the frames
// it looks to come on with the step that uses them. Under it the frames
// together, three ways: uncompressed, as I-frames only, as they are.
//
// Two sliders: the frames (a jog: each shows its finished frame), and the
// steps of the frame (a P-frame: its reference, the blocks moved by their
// vectors, the residual added; a B-frame: the pieces from the earlier
// frame, those from the later one, where a block has both their mean, the
// residual added; an I-frame stands alone). »Weiter«, the keys and play go
// through every step of every frame while »Zwischenschritte« is on, and
// from finished frame to finished frame while it is off (at first), as the jog does
// and a click on a frame of the strip. Moving forward one step animates it:
// the blocks slide from where they lie in the reference to their place,
// the residual fades in; one step back in the same frame plays it
// backwards.
//
//   <div class='gop-stepper' data-stepper data-src='/video/gop-steps'></div>
//
// data-src: the base name of gop-steps.json, which names the sheet
// (gop-steps-HASH.webp beside it), both made by
// src/content/video-compression/bilder/gop_steps.py (kinds, vectors and
// sizes are x264's; the blocks are drawn here from the decoded reference
// frames and the vectors, bilinear between pixels).
(function () {
    'use strict';
    // the colours of the kinds, as CSS variables with a step for dark mode (default.css)
    const COLOURS = { I: 'var(--gop-i)', P: 'var(--gop-p)', B: 'var(--gop-b)' };
    const SLIDE_MS = 1100;
    const FADE_MS = 700;
    const de = (n) => n.toLocaleString('de-DE');
    const html = (tag, attrs = {}, children = []) => {
        const node = document.createElement(tag);
        for (const [k, v] of Object.entries(attrs)) {
            if (k === 'text') node.textContent = v; else node.setAttribute(k, v);
        }
        for (const child of children) node.append(child);
        return node;
    };
    const ease = (t) => (t < 0.5 ? 2 * t * t : 1 - (-2 * t + 2) ** 2 / 2);
    const reduced = () => window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

    function image(src) {
        return new Promise((resolve, reject) => {
            const img = new Image();
            img.onload = () => resolve(img);
            img.onerror = reject;
            img.src = src;
        });
    }

    // a canvas at the device pixels of its box (its layout box: a card of
    // the strip that is turned keeps the size it has when it is flat)
    function fit(canvas) {
        const ratio = window.devicePixelRatio || 1;
        const w = Math.max(1, Math.round(canvas.offsetWidth * ratio));
        const h = Math.max(1, Math.round(canvas.offsetHeight * ratio));
        if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }
        return { ctx: canvas.getContext('2d'), w, h, ratio };
    }

    // the sheet decoded once and cut into a bitmap per frame (with its
    // margin) and one per thumbnail, kept ready to draw, so that no step
    // waits for the browser to decode the large picture again
    async function cut(data, sheet) {
        const { frames, width: W, height: H, margin: M, thumb: THUMB } = data;
        const rowHeight = H + 2 * M;
        if (sheet.decode) await sheet.decode().catch(() => {});   // an <img>; a bitmap is decoded
        const bitmap = (sx, sy, w, h) => {
            if (window.createImageBitmap) return createImageBitmap(sheet, sx, sy, w, h);
            const c = document.createElement('canvas');
            c.width = w;
            c.height = h;
            c.getContext('2d').drawImage(sheet, sx, sy, w, h, 0, 0, w, h);
            return Promise.resolve(c);
        };
        const pictures = await Promise.all(frames.map((_, k) => bitmap(0, k * rowHeight, W + 2 * M, H + 2 * M)));
        const thumbs = await Promise.all(frames.map((_, k) => bitmap(W + 2 * M, k * rowHeight, W / THUMB, H / THUMB)));
        return { pictures, thumbs };
    }

    function mount(el, data, { pictures, thumbs }) {
        const { frames, width: W, height: H, margin: M } = data;
        const count = frames.length;
        const plain = (k) => `${frames[k].kind}${k}`;
        const name = (k) => {
            const span = html('span', { class: 'gop-name' });
            span.style.color = COLOURS[frames[k].kind];
            span.append(frames[k].kind, html('sub', { text: String(k) }));
            return span;
        };
        // the vectors of each frame as blocks: rectangle and its pieces
        const blocksOf = frames.map((frame) => {
            const map = new Map();
            const v = frame.vectors;
            for (let i = 0; i < v.length; i += 7) {
                const key = `${v[i]} ${v[i + 1]} ${v[i + 2]} ${v[i + 3]}`;
                if (!map.has(key)) map.set(key, { x: v[i], y: v[i + 1], w: v[i + 2], h: v[i + 3], pieces: [] });
                map.get(key).pieces.push({ dx: v[i + 4] / 4, dy: v[i + 5] / 4, ref: v[i + 6] });
            }
            return [...map.values()];
        });
        // the steps of a frame
        const stepsOf = (k) => (frames[k].kind === 'I' ? 1 : 3);
        const flat = [];
        frames.forEach((_, k) => { for (let s = 0; s < stepsOf(k); s++) flat.push([k, s]); });

        // ---- the film strip
        const arcs = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
        arcs.setAttribute('class', 'gop-arcs');
        arcs.setAttribute('aria-hidden', 'true');
        const maxBytes = Math.max(...frames.map((f) => f.bytes));
        const slots = frames.map((frame, k) => {
            const thumb = html('canvas', { class: 'gop-thumb', 'aria-hidden': 'true' });
            const bar = html('span', { class: 'gop-bar' }, [html('span')]);
            bar.firstChild.style.width = `${Math.max(2, 100 * frame.bytes / maxBytes)}%`;
            bar.firstChild.style.background = COLOURS[frame.kind];
            const slot = html('figure', { class: 'gop-slot', role: 'button', tabindex: '0', 'aria-label': `Bild ${plain(k)}, ${de(frame.bytes)} Bytes` }, [
                thumb,
                html('figcaption', {}, [name(k), html('span', { class: 'gop-bytes', text: `${de(frame.bytes)} Bytes` })]),
                bar,
            ]);
            slot.style.setProperty('--gop-kind', COLOURS[frame.kind]);
            return { slot, thumb, painted: 0 };
        });
        const track = html('div', { class: 'gop-track' }, [arcs, ...slots.map((s) => s.slot)]);
        const strip = html('div', { class: 'gop-strip', role: 'img', 'aria-label': 'Was die Datei für jedes Bild speichert, mit seinen Bytes' }, [track]);
        const stripHead = html('div', { class: 'gop-strip-head', text: 'In der Datei, mit den Bytes: I-Frames als Bild, P- und B-Frames als Rest (grau ist 0)' });

        // ---- the frames together, three ways, to one scale: a segment per
        // frame in its kind's colour
        const raw = data.raw * count;
        const total = (label, segments) => {
            const bytes = segments.reduce((s, [b]) => s + b, 0);
            const bar = html('span', { class: 'gop-total-bar' }, segments.map(([b, colour]) => {
                const seg = html('span');
                seg.style.width = `${100 * b / raw}%`;
                seg.style.background = colour;
                return seg;
            }));
            return [html('span', { class: 'gop-total-label', text: label }), bar, html('span', { class: 'gop-total-bytes', text: `${de(bytes)} Bytes` })];
        };
        const totals = html('div', { class: 'gop-totals' }, [
            ...total('unkomprimiert', frames.map(() => [data.raw, 'var(--widget)'])),
            ...total('nur I-Frames', frames.map((f) => [f.intraBytes, COLOURS.I])),
            ...total('I, P und B', frames.map((f) => [f.bytes, COLOURS[f.kind]])),
        ]);
        totals.setAttribute('aria-label', `Alle ${count} Bilder zusammen`);

        // ---- the buttons, the jog, the steps of the frame
        const controls = html('div', { class: 'gop-controls' });
        controls.innerHTML = `
            <button class='btn btn-sm btn-success' data-go='first' aria-label='Zum Anfang'><i class='bi bi-chevron-bar-left' style='margin-right: 0;'></i></button>
            <button class='btn btn-sm btn-success' data-go='prev' aria-label='Einen Schritt zurück'><i class='bi bi-chevron-left' style='margin-right: 0;'></i></button>
            <button class='btn btn-sm btn-success' data-go='next' aria-label='Nächster Schritt'><i class='bi bi-chevron-right' style='margin-right: 0;'></i></button>
            <button class='btn btn-sm btn-success' data-go='last' aria-label='Zum Ende'><i class='bi bi-chevron-bar-right' style='margin-right: 0;'></i></button>
            <span class='gop-count'></span>
            <button type='button' class='btn btn-sm btn-outline-secondary gop-steps-toggle' aria-pressed='false'><i class='bi bi-toggle-off' aria-hidden='true'></i> Zwischenschritte</button>`;
        // the jog: the timeline of stepper_keys.js, one dot a frame
        const jog = html('div', { class: 'stepper-timeline gop-jog', role: 'slider', 'aria-label': 'Die Bilder', 'data-no-swipe': '' },
            frames.map(() => html('span')));
        const build = html('div', { class: 'gop-build', role: 'group', 'aria-label': 'Wie das Bild entsteht' });
        let step = 0;
        let animation = 0;
        // the steps in between: on, »Weiter« goes through every step of
        // every frame; off, from finished frame to finished frame
        let inBetween = false;
        const finished = (k) => flat.findIndex(([kk, ss]) => kk === k && ss === stepsOf(k) - 1);
        const buildButtons = [0, 1, 2].map((s) => {
            const b = html('button', { type: 'button', class: 'gop-build-step' });
            b.addEventListener('click', () => {
                const [k, from] = flat[step];
                if (s >= stepsOf(k)) return;
                to(flat.findIndex(([kk, ss]) => kk === k && ss === s), s === from + 1 ? true : s === from - 1 ? 'back' : false);
            });
            return b;
        });
        const alone = html('span', { class: 'gop-build-alone' });
        build.append(...buildButtons, alone);

        // ---- the frame being built
        const view = html('canvas', { class: 'gop-view', role: 'img' });
        const say = html('div', { class: 'gop-say', 'aria-live': 'polite' });
        el.replaceChildren(stripHead, strip, totals, controls, jog, build, view, say);
        const go = (which) => el.querySelector(`[data-go='${which}']`);
        const toggle = controls.querySelector('.gop-steps-toggle');
        toggle.addEventListener('click', () => {
            inBetween = !inBetween;
            toggle.setAttribute('aria-pressed', String(inBetween));
            toggle.querySelector('i').className = `bi ${inBetween ? 'bi-toggle-on' : 'bi-toggle-off'}`;
            // off in the middle of a frame: to its finished frame
            if (!inBetween) to(finished(flat[step][0]));
        });

        // ---- the strip, a cover flow: the frame being built and the frames
        // it looks to pop out, flat and large, with their bytes; the others
        // stand stacked between them, turned towards the nearest large one
        let centres = [];
        const layoutStrip = () => {
            const [k] = flat[step];
            const width = strip.clientWidth;
            const popped = new Set([k, ...frames[k].refs]);
            const card = Math.min(width * 0.2, 17 * 16);
            const stacked = (width - popped.size * card) / (count - popped.size);
            track.style.setProperty('--gop-card', `${card}px`);
            let x = 0;
            centres = frames.map((_, i) => {
                const w = popped.has(i) ? card : stacked;
                const c = x + w / 2;
                x += w;
                return c;
            });
            slots.forEach((s, i) => {
                const up = popped.has(i);
                const nearest = [...popped].reduce((a, b) => (Math.abs(b - i) < Math.abs(a - i) ? b : a));
                const turn = up ? 0 : (i < nearest ? 58 : -58);
                s.slot.style.transform = `translateX(${centres[i] - card / 2}px) perspective(${card * 3}px) rotateY(${turn}deg) scale(${up ? 1 : 0.86})`;
                s.slot.style.zIndex = String(up ? 100 : 50 - Math.abs(i - nearest));
                s.slot.classList.toggle('popped', up);
                s.slot.classList.toggle('current', i === k);
            });
            track.style.height = `${slots[k].slot.offsetTop + slots[k].slot.offsetHeight}px`;
        };
        const paintThumbs = () => {
            slots.forEach((s, k) => {
                const width = s.thumb.offsetWidth;
                if (s.painted === width) return;
                const { ctx, w, h } = fit(s.thumb);
                ctx.imageSmoothingQuality = 'high';
                ctx.drawImage(thumbs[k], 0, 0, w, h);
                s.painted = width;
            });
        };
        // An arrow from the frame being built to each frame it looks to,
        // all of them from the start: it leaves the top edge a little
        // towards its target, in the direction away from the frame's
        // middle, and arrives the same way, aimed at the middle of the
        // target, the head along the curve with its tip on the outline.
        // The reference the step uses is purple (current), one already used
        // grey, one still to come dashed and pale.
        const drawArcs = () => {
            const [k, s] = flat[step];
            const frame = frames[k];
            // a P-frame uses its reference from the first step on, a
            // B-frame the earlier one from step 0, the later from step 1
            const used = (i) => frame.kind === 'P' || i <= s;
            const now = frame.kind === 'P' ? (s < 2 ? 0 : -1) : (s < 2 ? s : -1);
            const slot = slots[k];
            const base = slot.slot.offsetTop - 2.5;          // the outline of a thumbnail
            const thumbWidth = slot.thumb.offsetWidth;
            const middleY = slot.slot.offsetTop + slot.thumb.offsetHeight / 2;
            const width = strip.clientWidth;
            arcs.setAttribute('viewBox', `0 0 ${width} ${slot.slot.offsetTop}`);
            arcs.style.width = `${width}px`;
            arcs.style.height = `${slot.slot.offsetTop}px`;
            const unit = (dx, dy) => { const n = Math.hypot(dx, dy); return [dx / n, dy / n]; };
            arcs.innerHTML = frame.refs.map((ref, i) => {
                const side = ref > k ? 1 : -1;
                const sx = centres[k] + side * 0.22 * thumbWidth;
                const ex = centres[ref] - side * 0.22 * thumbWidth;
                const [dsx, dsy] = unit(sx - centres[k], base - middleY);
                const [dex, dey] = unit(ex - centres[ref], base - middleY);
                // how far the curve goes out along those directions: so far
                // that it stays inside the room above the cards
                const reach = Math.min(0.5 * Math.abs(ex - sx), (base - 6) / (0.75 * Math.max(Math.abs(dsy), Math.abs(dey))));
                const head = 9, half = 4.5;
                const p1 = [sx + dsx * reach, base + dsy * reach];
                const p2 = [ex + dex * reach, base + dey * reach];
                // the line stops under the head; the head points along -de
                const lx = ex + dex * (head - 1), ly = base + dey * (head - 1);
                const bx = ex + dex * head, by = base + dey * head;
                const colour = i === now ? 'var(--stepper-current)' : 'var(--widget-ink)';
                const pending = !used(i);
                return `<g${pending ? " opacity='0.55'" : ''}>` +
                    `<path d='M ${sx} ${base} C ${p1[0]} ${p1[1]} ${p2[0]} ${p2[1]} ${lx} ${ly}' fill='none' stroke='${colour}' stroke-width='1.8' stroke-linecap='round'${pending ? " stroke-dasharray='4 4'" : ''}/>` +
                    `<path d='M ${ex} ${base} L ${bx - dey * half} ${by + dex * half} L ${bx + dey * half} ${by - dex * half} z' fill='${colour}'/></g>`;
            }).join('');
        };

        // ---- the frame being built
        const hatchIntra = (ctx, sx, sy, k, ratio) => {
            const list = frames[k].intra;
            for (let i = 0; i < list.length; i += 2) {
                const x0 = Math.round(list[i] * 16 * sx), y0 = Math.round(list[i + 1] * 16 * sy);
                const x1 = Math.round((list[i] + 1) * 16 * sx), y1 = Math.round((list[i + 1] + 1) * 16 * sy);
                ctx.fillStyle = '#5f5f5f';
                ctx.fillRect(x0, y0, x1 - x0, y1 - y0);
                ctx.save();
                ctx.beginPath();
                ctx.rect(x0, y0, x1 - x0, y1 - y0);
                ctx.clip();
                ctx.strokeStyle = 'rgba(255, 255, 255, 0.45)';
                ctx.lineWidth = Math.max(1, Math.round(ratio));
                const gap = Math.max(4, Math.round(4 * ratio));
                for (let d = -(y1 - y0); d < x1 - x0; d += gap) {
                    ctx.beginPath();
                    ctx.moveTo(x0 + d, y1);
                    ctx.lineTo(x0 + d + (y1 - y0), y0);
                    ctx.stroke();
                }
                ctx.restore();
            }
        };
        // frame k at step s, the step's movement at t (0 … 1)
        // the prediction is drawn at the frame's own size, the pieces 1:1,
        // into a buffer that is then drawn onto the view in one go; the
        // state after a step (t = 1) is kept, so that an animation only
        // draws the pieces that move
        const work = html('canvas', { width: W, height: H });
        const wctx = work.getContext('2d');
        wctx.imageSmoothingEnabled = true;
        const still = new Map();
        // frame k after step s (0: the pieces from the first reference, 1:
        // from both), the moving ones at t of their way
        const prediction = (k, s, t) => {
            const key = `${k}:${s}`;
            if (t >= 1 && still.has(key)) return still.get(key);
            const frame = frames[k];
            const second = frame.kind === 'B' && s >= 1;
            const before = second ? prediction(k, 0, 1) : null;
            wctx.globalAlpha = 1;
            if (before) wctx.drawImage(before, 0, 0);
            else { wctx.fillStyle = '#808080'; wctx.fillRect(0, 0, W, H); }
            // the pieces from one reference, at t of their way from where
            // they lie there to their place; a second piece at alpha 0.5
            // makes the mean of the two
            const ref = frame.refs[second ? 1 : 0];
            const f = 1 - ease(t);
            for (const b of blocksOf[k]) {
                const p = b.pieces.find((q) => q.ref === ref);
                if (!p) continue;
                wctx.globalAlpha = second && b.pieces.length > 1 ? 0.5 : 1;
                wctx.drawImage(pictures[ref], M + b.x + p.dx, M + b.y + p.dy, b.w, b.h, b.x + p.dx * f, b.y + p.dy * f, b.w, b.h);
            }
            wctx.globalAlpha = 1;
            if (t < 1) return work;
            const kept = html('canvas', { width: W, height: H });
            kept.getContext('2d').drawImage(work, 0, 0);
            still.set(key, kept);
            return kept;
        };
        // frame k at step s, the step's movement at t (0 … 1)
        const paintView = (k, s, t) => {
            const { ctx, w, h, ratio } = fit(view);
            const sx = w / W, sy = h / H;
            ctx.imageSmoothingEnabled = true;
            ctx.imageSmoothingQuality = 'high';
            const frame = frames[k];
            const decoded = (i, alpha = 1) => {
                ctx.globalAlpha = alpha;
                ctx.drawImage(pictures[i], M, M, W, H, 0, 0, w, h);
                ctx.globalAlpha = 1;
            };
            if (frame.kind === 'I') { decoded(k); return; }
            if (frame.kind === 'P' && s === 0) { decoded(frame.refs[0]); return; }
            // P: step 1 moves, step 2 shows its result; B: steps 0 and 1
            // move, step 2 shows the result of step 1
            const moving = frame.kind === 'P' ? (s === 1 ? 0 : null) : (s < 2 ? s : null);
            const picture = moving === null ? prediction(k, frame.kind === 'P' ? 0 : 1, 1) : prediction(k, moving, t);
            ctx.drawImage(picture, 0, 0, w, h);
            hatchIntra(ctx, sx, sy, k, ratio);
            // while blocks slide: a thin frame round each that moves (half a
            // pixel or more), at device pixels, so that the squares are seen
            // as squares; gone when they arrive
            if (moving !== null && t < 1) {
                const ref = frame.refs[moving];
                const f = 1 - ease(t);
                const line = Math.max(1, Math.round(ratio));
                ctx.save();
                // faint: they show the squares, not a grid over the picture
                ctx.globalAlpha = 0.3 * Math.min(1, 4 * (1 - t));
                for (const b of blocksOf[k]) {
                    const p = b.pieces.find((q) => q.ref === ref);
                    if (!p || Math.hypot(p.dx, p.dy) < 0.5) continue;
                    const x0 = Math.round((b.x + p.dx * f) * sx), y0 = Math.round((b.y + p.dy * f) * sy);
                    const x1 = Math.round((b.x + b.w + p.dx * f) * sx), y1 = Math.round((b.y + b.h + p.dy * f) * sy);
                    ctx.fillStyle = 'rgba(255, 255, 255, 0.85)';
                    ctx.fillRect(x0, y0, x1 - x0, line);
                    ctx.fillRect(x0, y0, line, y1 - y0);
                    ctx.fillStyle = 'rgba(0, 0, 0, 0.55)';
                    ctx.fillRect(x0, y1 - line, x1 - x0, line);
                    ctx.fillRect(x1 - line, y0, line, y1 - y0);
                }
                ctx.restore();
            }
            if (s === 2) decoded(k, ease(t));
        };

        const buildLabels = (k) => {
            const frame = frames[k];
            if (frame.kind === 'I') return null;
            if (frame.kind === 'P') return [['Vorlage ', name(frame.refs[0])], ['Blöcke verschoben'], ['+ Rest']];
            return [['aus ', name(frame.refs[0])], ['aus ', name(frame.refs[1]), ' dazu'], ['+ Rest']];
        };

        // a frame's name in the message, its number as an index
        const sub = (k) => `${frames[k].kind}<sub>${k}</sub>`;
        const describe = (k, s) => {
            const frame = frames[k];
            const n = sub(k);
            const blocks = frame.blocks;
            const all = Object.values(blocks).reduce((sum, b) => sum + b, 0);
            const from = (ref) => de(blocks[String(ref)] || 0);
            const usedBy = frames.map((f, i) => (f.refs.includes(k) ? sub(i) : null)).filter(Boolean);
            const size = `${n} braucht ${de(frame.bytes)} Bytes; als I-Frame wären es ${de(frame.intraBytes)}.`;
            if (frame.kind === 'I') {
                const where = k === 0 ? 'Das erste Bild ist ein I-Frame' : 'Alle 8 Bilder kommt ein I-Frame';
                return `${where}: Es steht für sich, jeder Block wird aus seinen Nachbarn im selben Bild vorhergesagt. ${de(frame.bytes)} Bytes; unkomprimiert wären es ${de(data.raw)}.`;
            }
            const intra = blocks.intra ? ` ${de(blocks.intra)} ${blocks.intra === 1 ? 'Block sagt' : 'Blöcke sagt'} der Encoder im Bild vorher, weil er nichts Passendes fand; ${blocks.intra === 1 ? 'er fehlt' : 'sie fehlen'} noch (schraffiert).` : '';
            if (frame.kind === 'P') {
                const ref = sub(frame.refs[0]);
                if (s === 0) return `${n} baut auf ${ref} auf, das der Decoder schon hat. Gespeichert sind für jeden Block nur ein Vektor, wo in ${ref} sein Stück liegt, und der Rest, der danach noch fehlt.`;
                if (s === 1) return `Jeder Block holt sich sein Stück aus ${ref}, um seinen Vektor verschoben: ${from(frame.refs[0])} der ${de(all)} Blöcke aus 16 × 16 Pixeln.${intra}`;
                return `Dazu kommt der Rest von ${n} aus der Leiste oben, und das Bild ist fertig. ${size}${usedBy.length ? ` ${n} ist selbst Vorlage für ${usedBy.join(', ')}.` : ''}`;
            }
            const [a, b] = frame.refs.map(sub);
            if (s === 0) {
                const early = ` ${b} kommt erst nach ${n}, steht in der Datei aber davor; der Decoder hat es schon.`;
                return `${n} schaut auf ${a} und ${b}.${early} Zuerst kommen die Stücke aus ${a}: ${from(frame.refs[0])} Blöcke holen alles von dort, ${de(blocks.both || 0)} ein Stück von beiden. Grau bleiben noch die ${from(frame.refs[1])} Blöcke, die alles aus ${b} holen.${intra}`;
            }
            if (s === 1) return `Dann die Stücke aus ${b}: ${from(frame.refs[1])} Blöcke holen alles von dort. Wo ein Block zwei Stücke hat, gilt der Mittelwert der beiden.`;
            const rest = usedBy.length ? ` ${n} ist selbst Vorlage für ${usedBy.join(' und ')}.` : ` Kein Bild schaut auf ${n}, darum darf der Encoder es gröber runden.`;
            return `Dazu kommt der Rest von ${n}, fast überall grau, also fast nichts. ${size}${rest}`;
        };

        const draw = (animate, left) => {
            cancelAnimationFrame(animation);
            const [k, s] = flat[step];
            layoutStrip();
            paintThumbs();
            drawArcs();
            // the steps of the frame: three buttons, or one label for an I-frame
            const labels = buildLabels(k);
            build.classList.toggle('alone', !labels);
            alone.innerHTML = labels ? '' : `<span>${sub(k)} steht für sich: keine Vorlage, kein Rest</span>`;
            buildButtons.forEach((b, i) => {
                b.replaceChildren(...(labels ? labels[i] : []));
                b.classList.toggle('done', !!labels && i < s);
                b.classList.toggle('current', !!labels && i === s);
                b.setAttribute('aria-pressed', String(!!labels && i === s));
                b.tabIndex = labels ? 0 : -1;
            });
            [...jog.children].forEach((dot, i) => { dot.className = i < k ? 'done' : i === k ? 'current' : ''; });
            jog.setAttribute('aria-valuemax', String(count - 1));
            jog.setAttribute('aria-valuenow', String(k));
            view.setAttribute('aria-label', `Bild ${plain(k)}: ${labels ? labels[s].map((p) => (typeof p === 'string' ? p : p.textContent)).join('') : 'das I-Frame'}`);
            say.innerHTML = describe(k, s);
            el.querySelector('.gop-count').innerHTML = `Bild ${k + 1} von ${count}: ${sub(k)}`;
            go('first').disabled = go('prev').disabled = step <= 0;
            go('next').disabled = go('last').disabled = step >= flat.length - 1;
            el.dataset.step = step;
            el.dataset.steps = flat.length;
            // forward: the step plays; one step back: the step it leaves
            // (left, possibly the first step of the next frame) plays
            // backwards, the residual fades out, the blocks slide back to
            // where they lie in the reference; then the step it went to
            const moves = (kk, ss) => frames[kk].kind !== 'I' && !(frames[kk].kind === 'P' && ss === 0);
            const back = animate === 'back';
            const [pk, ps] = back ? left : [k, s];
            if (!animate || !moves(pk, ps) || reduced()) { paintView(k, s, 1); return; }
            const duration = ps === 2 ? FADE_MS : SLIDE_MS;
            const start = performance.now();
            const at = (t) => paintView(pk, ps, back ? 1 - t : t);
            const tick = (now) => {
                const t = Math.min(1, (now - start) / duration);
                if (t < 1) { at(t); animation = requestAnimationFrame(tick); } else paintView(k, s, 1);
            };
            at(0);
            animation = requestAnimationFrame(tick);
        };
        // animate: true plays the step forward, 'back' the step left
        // behind backwards, when the step goes back by exactly one
        const to = (i, animate = false) => {
            const was = step;
            step = Math.max(0, Math.min(flat.length - 1, i));
            draw(animate === 'back' ? (step === was - 1 ? 'back' : false) : animate, flat[was]);
        };
        el.stepperGo = (i) => to(i, false);
        go('first').addEventListener('click', () => to(0));
        go('prev').addEventListener('click', () => {
            if (inBetween) to(step - 1, 'back');
            else to(finished(Math.max(0, flat[step][0] - (flat[step][1] === stepsOf(flat[step][0]) - 1 ? 1 : 0))));
        });
        go('next').addEventListener('click', () => {
            if (inBetween) to(step + 1, true);
            else to(finished(Math.min(count - 1, flat[step][0] + (flat[step][1] === stepsOf(flat[step][0]) - 1 ? 1 : 0))));
        });
        go('last').addEventListener('click', () => to(flat.length - 1));
        // a frame, finished: the jog, a click in the strip, and »Weiter« and
        // »Zurück« while the steps in between are switched off
        const jogTo = (event) => {
            const r = jog.getBoundingClientRect();
            const k = Math.max(0, Math.min(count - 1, Math.floor((event.clientX - r.left) / r.width * count)));
            const i = finished(k);
            if (i !== step) to(i);
        };
        // a click on a frame of the strip: that frame, finished
        slots.forEach((s, k) => {
            s.slot.addEventListener('click', () => to(finished(k)));
            s.slot.addEventListener('keydown', (event) => {
                if (event.key !== 'Enter' && event.key !== ' ') return;
                event.preventDefault();
                to(finished(k));
            });
        });
        let jogging = false;
        jog.addEventListener('pointerdown', (event) => { jogging = true; jog.setPointerCapture(event.pointerId); jogTo(event); });
        jog.addEventListener('pointermove', (event) => { if (jogging) jogTo(event); });
        jog.addEventListener('pointerup', () => { jogging = false; });
        jog.addEventListener('pointercancel', () => { jogging = false; });
        let width = 0;
        new ResizeObserver(() => {
            const now = strip.getBoundingClientRect().width;
            if (now === width) return;
            width = now;
            draw(false);
        }).observe(strip);
        draw(false);
        document.dispatchEvent(new CustomEvent('stepper-added', { detail: el }));
        // every step's result ready before it is needed, one frame at a time
        // while the browser has nothing else to do, so that no step waits
        const idle = window.requestIdleCallback || ((f) => setTimeout(f, 50));
        let next = 0;
        const prepare = () => {
            while (next < count && frames[next].kind === 'I') next++;
            if (next >= count) return;
            prediction(next, 0, 1);
            if (frames[next].kind === 'B') prediction(next, 1, 1);
            next++;
            idle(prepare);
        };
        idle(prepare);
    }

    // the sheet with a progress bar over a light preview of the first
    // frame, which stands where the frame being built will be; the sheet
    // read as a stream, so that the bar shows how much has come
    async function load(base, data, progress) {
        const response = await fetch(base.replace(/[^/]*$/, data.sheet));
        if (!response.ok) throw new Error(String(response.status));
        const total = Number(response.headers.get('Content-Length')) || 0;
        let blob;
        if (total && response.body && response.body.getReader) {
            const reader = response.body.getReader();
            const chunks = [];
            let have = 0;
            for (;;) {
                const { done, value } = await reader.read();
                if (done) break;
                chunks.push(value);
                have += value.length;
                progress(have / total);
            }
            blob = new Blob(chunks, { type: 'image/webp' });
        } else {
            blob = await response.blob();
        }
        progress(1);
        // decoded away from the page where the browser can
        if (window.createImageBitmap) return createImageBitmap(blob);
        const url = URL.createObjectURL(blob);
        try { return await image(url); } finally { URL.revokeObjectURL(url); }
    }

    function start(el) {
        if (el.dataset.gopStarted) return;
        el.dataset.gopStarted = '1';
        const base = el.dataset.src;
        const bar = html('span', { class: 'gop-progress-bar' });
        const label = html('span', { class: 'gop-progress-label', text: 'Die Bilder werden geladen …' });
        const preview = html('div', { class: 'gop-preview' }, [
            html('div', { class: 'gop-progress', role: 'progressbar', 'aria-valuemin': '0', 'aria-valuemax': '100' }, [bar]), label]);
        const box = html('div', { class: 'gop-loading' }, [preview]);
        el.replaceChildren(box);
        const progress = (share) => {
            const percent = Math.round(100 * share);
            bar.style.width = `${percent}%`;
            bar.parentElement.setAttribute('aria-valuenow', String(percent));
            label.textContent = share < 1 ? `Die Bilder werden geladen … ${percent} %` : 'Die Bilder werden vorbereitet …';
        };
        // the JSON is asked for anew each time; it names the sheet, whose
        // name changes with its bytes (browsers keep pictures for years)
        fetch(`${base}.json`, { cache: 'no-cache' }).then((r) => r.json())
            .then((data) => {
                if (data.preview) preview.style.backgroundImage = `url('${base.replace(/[^/]*$/, data.preview)}')`;
                return load(base, data, progress).then((sheet) => cut(data, sheet)).then((cuts) => mount(el, data, cuts));
            })
            .catch(() => { label.textContent = 'Die Bilder ließen sich nicht laden. Lade die Seite neu, um es noch einmal zu versuchen.'; });
    }

    // the data is a few megabytes: loaded once the page has loaded and the
    // browser is idle, or when the widget comes near, whichever is first
    const all = () => document.querySelectorAll('.gop-stepper[data-src]').forEach((el) => {
        if (!('IntersectionObserver' in window)) { start(el); return; }
        const watch = new IntersectionObserver((entries) => {
            if (!entries.some((e) => e.isIntersecting)) return;
            watch.disconnect();
            start(el);
        }, { rootMargin: '1200px 0px' });
        watch.observe(el);
        const soon = () => (window.requestIdleCallback ? requestIdleCallback(() => start(el), { timeout: 4000 }) : setTimeout(() => start(el), 1500));
        if (document.readyState === 'complete') soon(); else window.addEventListener('load', soon, { once: true });
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', all);
    else all();
})();
