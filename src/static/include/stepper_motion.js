// Movement between two steps of a stepper (algo_stepper.js and others): the
// widget draws the next step into a detached SVG, and morph() turns the
// visible SVG into it.
//
//   StepperMotion.morph(liveSvg, nextSvg, { duration: 500 });
//
// Elements are matched by data-key. A matched element stays the same DOM
// node, and every attribute that differs only in its numbers (x, cy, d,
// points, opacity, transform, …) moves from the old to the new value; other
// attributes and the text change at once (colours fade through the CSS
// transition of the widget). A new element starts at the element of the old
// step named by its data-from, or fades in; an element that is gone moves to
// the element of the new step named by its data-to and fades out, or just
// fades out. A line with data-ends='keyA keyB' joins two elements and has its
// own data-key (in a tree: the parent and the side of its pointer). Its ends
// sit on the centres of their elements in every frame, so it never comes
// loose: a line that stays keeps its end on keyA, and its other end leaves the
// old element and snaps onto the new one; a new line grows out of keyA to
// keyB; a line that is gone shrinks back into keyA. data-inset='20' ends the
// line 20 units before the centre of keyB (an arrowhead on a node's border).
// Such a line may be a path: then it bends while it moves and straightens when
// its ends rest. data-follow='key' keeps an element that comes or goes on the
// centre of that element while it fades. data-pulse='key' makes an element a
// light that runs once along that line during the movement: a path becomes a
// bulge of the line that travels along it (wider by data-swell, in the line's
// colour unless it has data-fill), a circle a point on it. data-rigid on a
// line: when its target changes, it slides over with its owner instead of
// letting go (a label that moves along with the node it points to).
// data-solid: a new element is there at once, opaque, instead of fading in
// (the box of a variable, which hides the start of its arrow). data-arc
// on a group that moves by translate: it goes on a small arc to the side. Elements without data-key
// are replaced at once, so a widget that sets no keys looks as before. With
// prefers-reduced-motion, or duration 0, the new step is shown at once. A new
// morph first finishes the running one.
(function (root) {
    'use strict';

    const NUMBER = /-?(?:\d+\.?\d*|\.\d+)(?:e[-+]?\d+)?/gi;
    const shape = (text) => text.replace(NUMBER, '#');

    // The value between from and to at t (0 to 1), or null when the two do
    // not differ only in their numbers
    function mix(from, to, t) {
        if (from === to) return to;
        if (shape(from) !== shape(to)) return null;
        const start = from.match(NUMBER);
        let i = 0;
        return to.replace(NUMBER, (end) => {
            const a = Number(start[i++]);
            const value = a + (Number(end) - a) * t;
            return String(Math.round(value * 100) / 100);
        });
    }

    const ease = (t) => (t < 0.5 ? 2 * t * t : 1 - (2 - 2 * t) ** 2 / 2);
    const reduced = () => typeof matchMedia === 'function' && matchMedia('(prefers-reduced-motion: reduce)').matches;
    const movable = (name) => !name.startsWith('data-') && name !== 'class' && name !== 'id';
    const attributes = (node) => Object.fromEntries([...node.attributes].map((a) => [a.name, a.value]));
    const running = new WeakMap();
    // ?motion=slow: every movement six times slower, to look at it closely
    const slow = typeof location !== 'undefined' && new URLSearchParams(location.search).get('motion') === 'slow' ? 6 : 1;

    function morph(live, next, options = {}) {
        running.get(live)?.();
        const duration = options.duration === 0 || reduced() ? 0 : (options.duration || 500) * slow;
        const old = new Map();
        for (const child of live.children) if (child.dataset.key && !child.dataset.ends && !old.has(child.dataset.key)) old.set(child.dataset.key, child);
        const goal = new Map();
        for (const child of next.children) if (child.dataset.key) goal.set(child.dataset.key, attributes(child));
        const tracks = [];
        const move = (node, name, from, to) => {
            if (duration && from !== undefined && from !== to && movable(name) && mix(from, to, 0) !== null) {
                tracks.push({ node, name, from, to });
                node.setAttribute(name, from);
            } else node.setAttribute(name, to);
        };
        const copy = (before, target, skip = []) => {
            const from = attributes(before);
            for (const name of Object.keys(from)) if (!target.hasAttribute(name)) before.removeAttribute(name);
            for (const [name, value] of Object.entries(attributes(target))) if (!skip.includes(name)) move(before, name, from[name], value);
        };
        const ENDS = ['x1', 'y1', 'x2', 'y2', 'd'];
        const edges = new Map();
        const ride = [];
        if (duration) {
            // an edge is matched by its data-key; the first of its ends is
            // the node it belongs to (in a tree: the parent with this pointer)
            const ends = (line) => line.dataset.ends.split(' ');
            const oldLines = new Map();
            for (const line of live.children) if (line.dataset.ends) oldLines.set(line.dataset.key, line);
            for (const target of next.children) {
                if (!target.dataset.ends) continue;
                const before = oldLines.get(target.dataset.key);
                const [a, b] = ends(target);
                if (before) {
                    // the pointer stays: its other end leaves the old node
                    // and snaps onto the new one (the old ends noted before
                    // copy() gives the line the new data-ends)
                    oldLines.delete(target.dataset.key);
                    const from = ends(before);
                    copy(before, target, ENDS);
                    edges.set(target, before);
                    ride.push({ line: before, from, to: [a, b] });
                } else ride.push({ line: target, from: [a, a], to: [a, b] });   // grows out of its node
            }
            for (const line of oldLines.values()) {
                const [a, b] = ends(line);
                ride.push({ line, from: [a, b], to: [a, a], leaving: true });   // shrinks back into it
            }
        }
        const kept = new Set();
        const shown = [];
        for (const target of [...next.children]) {
            if (target.dataset.ends) { shown.push(edges.get(target) || target); continue; }
            const key = target.dataset.key;
            const before = key && old.get(key);
            if (before && before.tagName === target.tagName && !kept.has(before)) {
                kept.add(before);
                copy(before, target);
                if (target.children.length) before.replaceChildren(...target.childNodes);
                else if (before.textContent !== target.textContent) before.textContent = target.textContent;
                shown.push(before);
                continue;
            }
            const anchor = target.dataset.from && old.get(target.dataset.from);
            if (anchor) {
                const from = attributes(anchor);
                for (const [name, value] of Object.entries(attributes(target))) move(target, name, from[name], value);
            } else if (duration && !('solid' in target.dataset)) move(target, 'opacity', '0', target.getAttribute('opacity') || '1');
            shown.push(target);
        }
        const leaving = ride.filter((r) => r.leaving).map((r) => r.line);
        if (duration) {
            for (const node of old.values()) {
                if (kept.has(node)) continue;
                const to = node.dataset.to && goal.get(node.dataset.to);
                if (to) for (const [name, value] of Object.entries(to)) if (node.hasAttribute(name) && name !== 'opacity') move(node, name, node.getAttribute(name), value);
                move(node, 'opacity', node.getAttribute('opacity') || '1', '0');
                leaving.push(node);
            }
        }
        const viewBox = next.getAttribute('viewBox');
        if (viewBox) move(live, 'viewBox', live.getAttribute('viewBox') || undefined, viewBox);
        live.replaceChildren(...leaving, ...shown);
        // data-pulse='key': a light (a circle) that runs once along the line
        // with that key, from its first end to its tip, starting at
        // data-delay and taking data-length (parts of the movement, 0 to 1);
        // at rest it is invisible
        const pulses = shown.filter((node) => node.dataset.pulse);
        if (!tracks.length && !ride.length && !pulses.length) return;
        const lineByKey = new Map([...leaving, ...shown].filter((node) => node.dataset.ends).map((node) => [node.dataset.key, node]));

        // where the ends of the edges are: on the centres of their nodes as
        // they move; an end that changes its node slides from the old one to
        // the new one
        const present = new Map();
        for (const node of [...leaving, ...shown]) if (node.dataset.key && !node.dataset.ends) present.set(node.dataset.key, node);
        const centre = (key) => {
            const node = present.get(key);
            if (!node) return null;
            return node.hasAttribute('cx')
                ? [Number(node.getAttribute('cx')), Number(node.getAttribute('cy'))]
                : [Number(node.getAttribute('x')) + Number(node.getAttribute('width') || 0) / 2, Number(node.getAttribute('y')) + Number(node.getAttribute('height') || 0) / 2];
        };
        // An edge is a pointer of its first node. Over the movement (t from 0
        // to 1): a new one sprouts from its node once the nodes are moving,
        // its tip follows the target at a distance and snaps on near the end;
        // one that breaks lets go with a jerk and shrinks back into its node;
        // one that points elsewhere lets go, swings across and snaps onto the
        // new node. The line is briefly thicker when it snaps on.
        const toward = (p, q, s) => [p[0] + (q[0] - p[0]) * s, p[1] + (q[1] - p[1]) * s];
        const part = (t, a, b) => Math.min(1, Math.max(0, (t - a) / (b - a)));
        const out = (t) => 1 - (1 - t) ** 2;
        const NEAR = 0.82;
        for (const r of ride) {
            r.width = Number(r.line.getAttribute('stroke-width') || 1);
            // data-rigid: a line that travels with its new target (a label
            // that moves along) slides over instead of letting go
            r.kind = r.leaving ? 'cut' : r.from[0] === r.from[1] ? 'grow'
                : r.from[1] === r.to[1] ? 'stay' : r.from[0] === r.to[0] && !('rigid' in r.line.dataset) ? 'swing' : 'slide';
        }
        // A path bends while it moves: its middle is where the middle of the
        // straight line was a moment ago (a fifth of the movement), so it
        // trails behind its ends like a rubber band and is straight again when
        // they rest; but only as far as its ends move differently. A line
        // element stays straight.
        const LAG = duration / 5;
        const round = (v) => Math.round(v * 100) / 100;
        const draw = (r, base, tip, now, last) => {
            const mid = [(base[0] + tip[0]) / 2, (base[1] + tip[1]) / 2];
            r.trail = (r.trail || []).filter((p) => p.at >= now - 4 * LAG);
            r.trail.push({ at: now, mid });
            const earlier = r.trail.find((p) => p.at >= now - LAG) || r.trail[0];
            // only as far as the two ends move differently: a line whose
            // ends shift alike (the whole picture making room, a change of
            // view) stays straight
            r.start = r.start || { base, tip };
            const apart = Math.hypot(base[0] - r.start.base[0] - (tip[0] - r.start.tip[0]), base[1] - r.start.base[1] - (tip[1] - r.start.tip[1]));
            const k = Math.min(1, apart / 24);
            const bend = last ? mid : [mid[0] + (earlier.mid[0] - mid[0]) * k, mid[1] + (earlier.mid[1] - mid[1]) * k];
            // data-inset: the line stops that far before its end (an arrowhead
            // on the border of a node), and is hidden while shorter than that
            const inset = Number(r.line.dataset.inset || 0);
            let end = tip;
            let control = bend;
            if (inset) {
                const d = Math.hypot(tip[0] - bend[0], tip[1] - bend[1]) || 1;
                if (Math.hypot(tip[0] - base[0], tip[1] - base[1]) <= inset + 1) {
                    r.line.setAttribute('visibility', 'hidden');
                    end = base;
                } else {
                    // inset before the tip, in the direction the curve comes in;
                    // when that would be before the bend, a straight line
                    // (else the curve turns back and the arrowhead with it)
                    r.line.removeAttribute('visibility');
                    if (d > inset + 2) end = [tip[0] - ((tip[0] - bend[0]) / d) * inset, tip[1] - ((tip[1] - bend[1]) / d) * inset];
                    else {
                        const total = Math.hypot(tip[0] - base[0], tip[1] - base[1]);
                        end = toward(base, tip, (total - inset) / total);
                        control = [(base[0] + end[0]) / 2, (base[1] + end[1]) / 2];
                    }
                }
            }
            // the curve must arrive in the direction of the arrow, else its
            // arrowhead turns over: then this frame is straight
            const heading = [tip[0] - base[0], tip[1] - base[1]];
            if ((end[0] - control[0]) * heading[0] + (end[1] - control[1]) * heading[1] <= 0.05 * (heading[0] ** 2 + heading[1] ** 2)) {
                control = [(base[0] + end[0]) / 2, (base[1] + end[1]) / 2];
            }
            if (r.line.tagName === 'path') r.line.setAttribute('d', `M${round(base[0])} ${round(base[1])} Q${round(control[0])} ${round(control[1])} ${round(end[0])} ${round(end[1])}`);
            else {
                r.line.setAttribute('x1', round(base[0]));
                r.line.setAttribute('y1', round(base[1]));
                r.line.setAttribute('x2', round(end[0]));
                r.line.setAttribute('y2', round(end[1]));
            }
        };
        // A step that assigns a pointer moves in two phases: first the pointer
        // sprouts (or swings over) and snaps onto its target where it stands
        // (t up to PHASE), only then does everything else move, the pointer
        // staying on its target. Such a step takes STRETCH times as long.
        const PHASE = 0.45;
        const STRETCH = 1.7;
        const assigning = ride.some((r) => (r.kind === 'grow' || r.kind === 'swing') && !('rigid' in r.line.dataset));
        const later = (t) => (assigning ? part(t, PHASE, 1) : t);
        const attach = (t, now) => {
            const u = assigning ? Math.min(1, t / PHASE) : t;
            for (const r of ride) {
                const { line, from, to, kind } = r;
                const base = centre(to[0]) || centre(from[0]);
                if (!base) continue;
                let tip;
                let snap = null;
                if (kind === 'stay') tip = centre(to[1]);
                else if (kind === 'grow') {
                    const target = centre(to[1]);
                    tip = u < 0.72 ? toward(base, target, NEAR * out(part(u, 0.15, 0.72))) : toward(base, target, NEAR + (1 - NEAR) * part(u, 0.72, 0.84));
                    snap = part(u, 0.8, 1);
                } else if (kind === 'cut') {
                    const target = centre(from[1]);
                    tip = u < 0.15 ? toward(base, target, 1 - (1 - NEAR) * out(part(u, 0, 0.15))) : toward(base, target, NEAR * (1 - part(u, 0.15, 0.75) ** 2));
                } else if (kind === 'swing') {
                    const before = centre(from[1]);
                    const after = centre(to[1]);
                    if (u < 0.2) tip = toward(base, before, 1 - (1 - NEAR) * out(part(u, 0, 0.2)));
                    else if (u < 0.74) tip = toward(toward(base, before, NEAR), toward(base, after, NEAR), ease(part(u, 0.2, 0.74)));
                    else tip = toward(base, after, NEAR + (1 - NEAR) * part(u, 0.74, 0.86));
                    snap = part(u, 0.82, 1);
                } else {
                    const before = centre(from[1]);
                    const after = centre(to[1]);
                    tip = toward(before, after, ease(later(t)));
                }
                if (!tip) continue;
                if (t >= 1) tip = centre(to[1]) || tip;
                draw(r, base, tip, now, t >= 1);
                if (snap !== null) line.setAttribute('stroke-width', t >= 1 || snap <= 0 ? r.width : Math.round(r.width * (1 + 0.9 * Math.sin(Math.PI * snap)) * 100) / 100);
            }
        };

        let frameId = null;
        let start = null;
        // data-follow='key': an element that comes or goes sits on the centre
        // of that element in every frame (a ring that fades on a moving node)
        const follow = [...leaving, ...shown].filter((node) => node.dataset.follow && !kept.has(node));
        const arcs = tracks.filter((track) => track.name === 'transform' && track.node.dataset.arc && /^translate/.test(track.to));
        const paint = (t, now) => {
            for (const track of tracks) track.node.setAttribute(track.name, t >= 1 ? track.to : mix(track.from, track.to, ease(later(t))));
            // data-arc: a group that moves by its translate goes on a small
            // arc (that many units to the side at the middle), so that two
            // that swap pass each other
            for (const track of arcs) {
                const [x0, y0] = track.from.match(NUMBER).map(Number);
                const [x1, y1] = track.to.match(NUMBER).map(Number);
                const e = t >= 1 ? 1 : ease(later(t));
                const len = Math.hypot(x1 - x0, y1 - y0) || 1;
                const bow = Number(track.node.dataset.arc) * Math.sin(Math.PI * e);
                track.node.setAttribute('transform', `translate(${round(x0 + (x1 - x0) * e - ((y1 - y0) / len) * bow)} ${round(y0 + (y1 - y0) * e + ((x1 - x0) / len) * bow)})`);
            }
            for (const node of follow) {
                const c = centre(node.dataset.follow);
                if (!c) continue;
                if (node.hasAttribute('cx')) { node.setAttribute('cx', round(c[0])); node.setAttribute('cy', round(c[1])); }
            }
            attach(Math.min(1, t), now);
            for (const node of pulses) {
                const line = lineByKey.get(node.dataset.pulse);
                const delay = Number(node.dataset.delay || 0);
                const length = Number(node.dataset.length || 0.4);
                const f = Math.min(1, Math.max(0, (t - delay) / length));
                if (!line || !line.getTotalLength || f <= 0 || f >= 1 || line.getAttribute('visibility') === 'hidden') {
                    node.setAttribute('opacity', 0);
                    continue;
                }
                const total = line.getTotalLength();
                const strength = Number(node.dataset.strength || 1);
                if (node.tagName === 'path') {
                    // a bulge of the line itself that travels from the owner
                    // to the tip: the outline of the curve, wider by up to
                    // data-swell around the place of the pulse, tapering into
                    // the line on both sides (a Gaussian, sigma SPREAD)
                    const SPREAD = 9;
                    // it sets off slowly, runs, and comes to rest at the tip
                    // (ease in and out, cubic), and swells up as it sets off
                    // and down as it arrives
                    const along = f < 0.5 ? 4 * f ** 3 : 1 - (2 - 2 * f) ** 3 / 2;
                    const swell = Number(node.dataset.swell || 4) * Math.sin(Math.PI * f) ** 0.6;
                    const half = Number(line.getAttribute('stroke-width') || 2) / 2;
                    const at = along * total;
                    const from = Math.max(0, at - 3 * SPREAD);
                    const to = Math.min(total, at + 3 * SPREAD);
                    const left = [];
                    const right = [];
                    for (let s = from; s <= to + 0.01; s += 1.5) {
                        const p = line.getPointAtLength(Math.min(s, total));
                        const q = line.getPointAtLength(Math.min(total, s + 0.5));
                        const r = line.getPointAtLength(Math.max(0, s - 0.5));
                        const tx = q.x - r.x;
                        const ty = q.y - r.y;
                        const length = Math.hypot(tx, ty) || 1;
                        const w = half + swell * Math.exp(-(((s - at) / SPREAD) ** 2));
                        left.push(`${round(p.x - (ty / length) * w)} ${round(p.y + (tx / length) * w)}`);
                        right.push(`${round(p.x + (ty / length) * w)} ${round(p.y - (tx / length) * w)}`);
                    }
                    node.setAttribute('d', `M${left.join('L')}L${right.reverse().join('L')}Z`);
                    if (!node.dataset.fill) node.setAttribute('fill', line.getAttribute('stroke') || 'currentColor');
                    node.setAttribute('opacity', strength);
                } else {
                    const point = line.getPointAtLength(f * total);
                    node.setAttribute('cx', round(point.x));
                    node.setAttribute('cy', round(point.y));
                    node.setAttribute('opacity', round(strength * Math.sin(Math.PI * f)));
                }
            }
        };
        // data-moving on the picture while it moves (for tests)
        const finish = () => {
            cancelAnimationFrame(frameId);
            paint(1, performance.now());
            for (const node of leaving) node.remove();
            running.delete(live);
            live.removeAttribute('data-moving');
        };
        running.set(live, finish);
        live.setAttribute('data-moving', '');
        paint(0, performance.now());
        const frame = (now) => {
            if (start === null) start = now;
            const t = (now - start) / (assigning ? duration * STRETCH : duration);
            if (t >= 1) return finish();
            paint(t, now);
            frameId = requestAnimationFrame(frame);
        };
        frameId = requestAnimationFrame(frame);
    }

    const api = { morph, mix };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    else root.StepperMotion = api;
})(typeof window !== 'undefined' ? window : globalThis);
