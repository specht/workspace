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
// centre of that element while it fades. data-grow: an element that covers
// what lies behind it comes and goes by its size (a circle's radius, a group's
// scale), opaque, instead of fading; with data-inset='auto' a line stops at
// the current radius of the circle it points to. data-merge with data-to: an
// element that goes into the one it moves to (a token into its node) stays
// opaque under it and is removed at the end. data-pulse='key' makes an element a
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
//
// data-along='key' on a circle that moves: it travels along that line (an
// automaton's arrow) instead of straight, in time with the pulse on it, from
// its old centre to its new one (a loop: once around it), a fifth smaller
// on its way. With data-along-from='key' a new element comes out of that
// element instead (a ring reached by ε out of the ring that has just
// arrived before it), unseen until it sets off.
//
// reverse: true, for a step back: the movement forward from the step shown
// by next to the one shown now, played backwards, so that a step back looks
// exactly like the step forward run in reverse (a pulse runs back along its
// arrow, what came goes back where it came from, what grew shrinks).
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
        // backwards: the step now shown is where the forward movement ends;
        // it starts at the step shown by next, which is also where this one
        // comes to rest
        const backwards = !!options.reverse && !!duration;
        const goal = next;
        if (backwards) {
            const after = live.cloneNode(true);
            if (goal.getAttribute('viewBox')) live.setAttribute('viewBox', goal.getAttribute('viewBox'));
            live.replaceChildren(...goal.cloneNode(true).childNodes);
            next = after;
        }
        // at rest on the goal: backwards, the goal's own drawing
        const rest = () => {
            if (!backwards) return;
            if (goal.getAttribute('viewBox')) live.setAttribute('viewBox', goal.getAttribute('viewBox'));
            live.replaceChildren(...goal.childNodes);
        };
        const old = new Map();
        for (const child of live.children) if (child.dataset.key && !child.dataset.ends && !old.has(child.dataset.key)) old.set(child.dataset.key, child);
        const goals = new Map();
        for (const child of next.children) if (child.dataset.key) goals.set(child.dataset.key, attributes(child));
        const tracks = [];
        // final: what the attribute is at the end, when it differs from to
        // only in its form (a group that grew: its transform without the scale)
        // early: it runs at the start, before pointers sprout (an element
        // that grows in: its pointers sprout from it)
        const move = (node, name, from, to, final = to, early = false) => {
            if (duration && from !== undefined && from !== to && movable(name) && mix(from, to, 0) !== null) {
                tracks.push({ node, name, from, to, final, early });
                node.setAttribute(name, from);
            } else node.setAttribute(name, final);
        };
        // data-grow: an element that covers what lies behind it (a node, a
        // token over lines) comes and goes by its size, opaque, not by fading:
        // a circle by its radius, a group by a scale about its own origin or
        // about the point data-grow names ('22 21', in its own units: a cell
        // drawn from its corner grows from its middle); a group that came
        // ends on its own transform, so that its next move tweens
        const grow = (node, coming) => {
            if (node.tagName === 'circle') {
                const r = node.getAttribute('r');
                if (coming) move(node, 'r', '0', r, r, true);
                else move(node, 'r', r, '0');
            } else if (node.tagName === 'g') {
                const base = node.getAttribute('transform') || '';
                const [cx, cy] = (node.dataset.grow || '0 0').split(/\s+/).map(Number);
                const about = (s) => `${base} translate(${cx} ${cy}) scale(${s}) translate(${-cx} ${-cy})`;
                if (coming) move(node, 'transform', about(0), about(1), base, true);
                else move(node, 'transform', about(1), about(0));
            } else if (coming) move(node, 'opacity', '0', node.getAttribute('opacity') || '1', node.getAttribute('opacity') || '1', true);
            else move(node, 'opacity', node.getAttribute('opacity') || '1', '0');
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
            } else if (duration && 'grow' in target.dataset) grow(target, true);
            else if (duration && !('solid' in target.dataset)) move(target, 'opacity', '0', target.getAttribute('opacity') || '1');
            shown.push(target);
        }
        const leaving = ride.filter((r) => r.leaving).map((r) => r.line);
        if (duration) {
            for (const node of old.values()) {
                if (kept.has(node)) continue;
                const to = node.dataset.to && goals.get(node.dataset.to);
                // only what can move: it keeps its own key and class
                if (to) for (const [name, value] of Object.entries(to)) if (node.hasAttribute(name) && name !== 'opacity' && movable(name)) move(node, name, node.getAttribute(name), value);
                // data-merge: it goes into the element it moves to, which
                // covers it; it does not fade (two half-transparent shapes on
                // top of each other), it is removed at the end
                if (to && 'merge' in node.dataset) { /* stays as it is */ }
                else if (!to && 'grow' in node.dataset) grow(node, false);
                else move(node, 'opacity', node.getAttribute('opacity') || '1', '0');
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
        // options.done: called when the movement has ended by itself (not
        // when the next one cut it short), for a step made of two movements
        if (!duration || (!tracks.length && !ride.length && !pulses.length)) { rest(); if (options.done) options.done(); return; }
        // a pulse runs along a pointer or along any keyed path (a curved
        // edge of an automaton that does not move)
        const lineByKey = new Map([...leaving, ...shown].filter((node) => node.dataset.ends || (node.tagName === 'path' && node.dataset.key)).map((node) => [node.dataset.key, node]));

        // where the ends of the edges are: on the centres of their nodes as
        // they move; an end that changes its node slides from the old one to
        // the new one
        const present = new Map();
        for (const node of [...leaving, ...shown]) if (node.dataset.key && !node.dataset.ends) present.set(node.dataset.key, node);
        const rings = new Map(shown.filter((node) => node.dataset.ring).map((node) => [node.dataset.ring, node]));
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
            // and only as far as the line is long: a short arrow (an index
            // under its column) has no room for a bend, its head would tilt
            const length = Math.hypot(tip[0] - base[0], tip[1] - base[1]);
            const k = Math.min(1, apart / 24) * Math.min(1, Math.max(0, (length - 40) / 80));
            const bend = last ? mid : [mid[0] + (earlier.mid[0] - mid[0]) * k, mid[1] + (earlier.mid[1] - mid[1]) * k];
            // data-inset: the line stops that far before its end (an arrowhead
            // on the border of a node), and is hidden while shorter than that
            // data-inset='auto': as far as the current radius of the circle it
            // points to, so the tip stays on the border of a node that grows
            let inset = Number(r.line.dataset.inset || 0);
            // a node that wears a ring (data-ring='node-…'): the tip stops
            // outside the ring, never on it
            if (r.line.dataset.inset === 'auto') {
                const key = r.kind === 'cut' ? r.from[1] : r.to[1];
                const target = present.get(key);
                const ring = rings.get(key);
                inset = ring ? Number(ring.getAttribute('r')) + Number(ring.getAttribute('stroke-width') || 2) / 2 + 2
                    : target && target.hasAttribute('r') ? Number(target.getAttribute('r')) + 2 : 20;
            }
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
        // what grows in: at once, within the first third of the sprouting
        const early = (t) => (assigning ? part(t, 0, PHASE / 3) : t);
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
                    if (!target) continue;
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
        // data-lift='h': a circle or a text that rises by up to h on its way
        // and comes down at its target (a tree that passes over others);
        // the lines to it follow. Its height is the moving one or, if it
        // only moves sideways, its own
        const lifted = shown.filter((node) => node.dataset.lift && (node.hasAttribute('cy') || node.tagName === 'text')).map((node) => {
            const name = node.hasAttribute('cy') ? 'cy' : 'y';
            const track = tracks.find((k) => k.node === node && k.name === name);
            return { node, name, track, base: Number(node.getAttribute(name)), lift: Number(node.dataset.lift) };
        });
        // data-along: a circle that travels along a line (its pulse's timing)
        // its old and new centre, fixed before the first frame (a value
        // without a track is the same at both ends)
        const along = shown.filter((node) => node.dataset.along && node.hasAttribute('cx')).map((node) => {
            const ends = (name) => {
                const track = tracks.find((k) => k.node === node && k.name === name);
                return track ? [Number(track.from), Number(track.final)] : [Number(node.getAttribute(name)), Number(node.getAttribute(name))];
            };
            const [x, y] = [ends('cx'), ends('cy')];
            // data-along-from='key': it comes out of that element (a ring
            // that arrives in the same step), unseen until it sets off
            const source = node.dataset.alongFrom && centre(node.dataset.alongFrom);
            const fixed = !tracks.some((k) => k.node === node && k.name === 'cx');
            return { node, from: source && fixed ? source : [x[0], y[0]], to: [x[1], y[1]], r: Number(node.getAttribute('r')),
                hide: !!(source && fixed), opacity: node.getAttribute('opacity') };
        });
        const travel = (t) => {
            for (const { node, from, to, r, hide, opacity } of along) {
                const line = lineByKey.get(node.dataset.along);
                if (t >= 1 && r && !tracks.some((k) => k.node === node && k.name === 'r')) node.setAttribute('r', r);
                if (t >= 1 && hide) { if (opacity === null) node.removeAttribute('opacity'); else node.setAttribute('opacity', opacity); }
                // at the end exactly at its place
                if (t >= 1) { node.setAttribute('cx', to[0]); node.setAttribute('cy', to[1]); }
                if (t >= 1 || !line || !line.getTotalLength) continue;
                const pulse = pulses.find((p) => p.dataset.pulse === node.dataset.along);
                const delay = pulse ? Number(pulse.dataset.delay || 0) : 0;
                const length = pulse ? Number(pulse.dataset.length || 0.4) : 1;
                const f = Math.min(1, Math.max(0, (t - delay) / length));
                const e = f < 0.5 ? 4 * f ** 3 : 1 - (2 - 2 * f) ** 3 / 2;
                if (hide) node.setAttribute('opacity', f > 0 ? (opacity === null ? 1 : opacity) : 0);
                // on the way: from the old centre straight to where the line
                // starts, along the line, straight into the new centre, so
                // that the centre is on the line all the way
                const total = line.getTotalLength();
                const first = line.getPointAtLength(0);
                const last = line.getPointAtLength(total);
                const into = Math.hypot(first.x - from[0], first.y - from[1]);
                const out = Math.hypot(to[0] - last.x, to[1] - last.y);
                const way = e * (into + total + out);
                let point;
                if (way <= into) point = [from[0] + (first.x - from[0]) * (into ? way / into : 1), from[1] + (first.y - from[1]) * (into ? way / into : 1)];
                else if (way <= into + total) { const p = line.getPointAtLength(way - into); point = [p.x, p.y]; }
                else { const k = out ? (way - into - total) / out : 1; point = [last.x + (to[0] - last.x) * k, last.y + (to[1] - last.y) * k]; }
                node.setAttribute('cx', round(point[0]));
                node.setAttribute('cy', round(point[1]));
                // smaller on its way, its own size again at the state
                if (r && !tracks.some((k) => k.node === node && k.name === 'r')) node.setAttribute('r', round(r * (1 - 0.2 * Math.sin(Math.PI * e))));
            }
        };
        const paint = (t, now) => {
            for (const track of tracks) track.node.setAttribute(track.name, t >= 1 ? track.final : mix(track.from, track.to, ease(track.early ? early(t) : later(t))));
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
            if (lifted.length) {
                const e = t >= 1 ? 1 : ease(later(t));
                for (const l of lifted) {
                    const at = l.track ? Number(l.node.getAttribute(l.name)) : l.base;
                    l.node.setAttribute(l.name, round(at - l.lift * Math.sin(Math.PI * e)));
                }
            }
            for (const node of follow) {
                const c = centre(node.dataset.follow);
                if (!c) continue;
                if (node.hasAttribute('cx')) { node.setAttribute('cx', round(c[0])); node.setAttribute('cy', round(c[1])); }
                else if (node.tagName === 'text') { node.setAttribute('x', round(c[0])); node.setAttribute('y', round(c[1])); }
            }
            attach(Math.min(1, t), now);
            travel(t);
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
                    // up to the tip: the arrowhead (HEAD long) stays as it is
                    const HEAD = 9;
                    const reach = Math.max(1, total - HEAD);
                    const at = along * reach;
                    const from = Math.max(0, at - 3 * SPREAD);
                    const to = Math.min(reach, at + 3 * SPREAD);
                    const left = [];
                    const right = [];
                    for (let s = from; s <= to + 0.01; s += 1.5) {
                        const p = line.getPointAtLength(Math.min(s, total));
                        const q = line.getPointAtLength(Math.min(total, s + 0.5));
                        const r = line.getPointAtLength(Math.max(0, s - 0.5));
                        const tx = q.x - r.x;
                        const ty = q.y - r.y;
                        const length = Math.hypot(tx, ty) || 1;
                        // tapering to the line's own width at the arrowhead
                        const w = half + swell * Math.exp(-(((s - at) / SPREAD) ** 2)) * Math.min(1, (reach - s) / SPREAD);
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
            paint(backwards ? 0 : 1, performance.now());
            for (const node of leaving) node.remove();
            rest();
            running.delete(live);
            live.removeAttribute('data-moving');
        };
        running.set(live, finish);
        live.setAttribute('data-moving', '');
        paint(backwards ? 1 : 0, performance.now());
        const frame = (now) => {
            if (start === null) start = now;
            const t = (now - start) / (assigning ? duration * STRETCH : duration);
            if (t >= 1) { finish(); if (options.done) options.done(); return; }
            paint(backwards ? 1 - t : t, now);
            frameId = requestAnimationFrame(frame);
        };
        frameId = requestAnimationFrame(frame);
    }

    // slow: how much slower every movement is (?motion=slow), for widgets
    // that time CSS animations to a movement
    const api = { morph, mix, slow };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    else root.StepperMotion = api;
})(typeof window !== 'undefined' ? window : globalThis);
