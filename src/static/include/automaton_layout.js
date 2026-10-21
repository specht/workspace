// A layout for finite automata, as an alternative to Graphviz.
//
// The states in columns by their distance from the start (BFS), so that a
// word is read from left to right; inside a column ordered by where they
// are reached from, and moved towards it (a chain stays on one line).
// Every arrow bends to the left of its direction: an arrow forward a
// little above, an arrow back below, two arrows between the same states
// apart. An arrow that would pass a state bends further until it keeps
// clear. Loops sit on top of their state, or below when a state is right
// above. Labels stand outside their arrow, where they keep clear of every
// arrow, state and label.
//
//   AutomatonLayout.svg({ states: [{ id, label }], start, accept: [ids],
//                         edges: [{ from, to, label }] })  → SVG string
//   AutomatonLayout.layout(a, { ring })  → the places, for the step widget:
//       ring is how far a ring around a state reaches beyond its border,
//       labels keep clear of it; every path has end(inset), the same arrow
//       with its tip inset from the centre of its target
//
// Where an automaton is too big for a drawing, svg() gives its transition
// table. Static drawings in a page: <div class='automaton-drawing'
// role='img' aria-label='…' data-states='q0 q1' data-start='q0'
// data-accept='q1' data-transitions='q0 münze q1, q1 drücken q0, …'>;
// a symbol a-c stands for a, b and c, the labels between two states are
// joined.
(function (root) {
    'use strict';
    const R = 22;
    const BASE_GAPX = 116;
    const BASE_GAPY = 92;
    // a state's name: a letter and its index, q0, r12, qF (the error state)
    const NAME = /^([A-Za-z])(\d+|F)$/;
    const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;');

    // Several starts for the order inside the columns (the sweeps settle
    // in the nearest good order, not always in the best), the drawing that
    // reads best kept: fewest crossings, no arrows on top of each other or
    // past a state, no label on an arrow
    function layout(a, options = {}) {
        const tries = a.states.length <= 12 ? 16 : a.states.length <= 30 ? 8 : 3;
        // the tries without their labels (placing those takes the time),
        // the best one again with them
        // a NEA numbered along its arrows tries both kinds of columns: the
        // longest way reads best for most, the distance from the start where
        // choices nest (the lock's ten digits)
        const modes = a.columns === 'order' ? ['order', 'distance'] : ['distance'];
        let best = null;
        for (const mode of modes) for (let variant = 0; variant < tries; variant++) {
            const L = layoutOnce(a, options, variant, false, mode);
            if (!best || L.score < best.score) best = { score: L.score, variant, mode };
            if (best.score === 0) break;
        }
        // labels hard to tell apart (as near to another arrow as to their
        // own): the same drawing with more room between its rows, if that
        // leaves fewer of them
        const L = layoutOnce(a, options, best.variant, true, best.mode);
        if (!L.ambiguous) return L;
        const roomy = layoutOnce(a, { ...options, spread: 1.4 }, best.variant, true, best.mode);
        return roomy.ambiguous < L.ambiguous && roomy.score <= L.score + 2 ? roomy : L;
    }

    // how well a drawing reads: crossings, arrows running along each other
    // or past a state, labels on arrows (lower is better)
    // the box around some points, grown by m
    const boundsOf = (pts, m = 0) => {
        let [x0, y0, x1, y1] = [Infinity, Infinity, -Infinity, -Infinity];
        for (const [x, y] of pts) { x0 = Math.min(x0, x); y0 = Math.min(y0, y); x1 = Math.max(x1, x); y1 = Math.max(y1, y); }
        return { x0: x0 - m, y0: y0 - m, x1: x1 + m, y1: y1 + m };
    };
    const meet = (a, b) => a.x0 <= b.x1 && b.x0 <= a.x1 && a.y0 <= b.y1 && b.y0 <= a.y1;
    const inside = (b, [x, y]) => x >= b.x0 && x <= b.x1 && y >= b.y0 && y <= b.y1;

    function badness(paths, pos) {
        const segX = (a, b, c, d) => { const o = (p, q, r) => Math.sign((q[0] - p[0]) * (r[1] - p[1]) - (q[1] - p[1]) * (r[0] - p[0])); return o(a, b, c) !== o(a, b, d) && o(c, d, a) !== o(c, d, b); };
        const states = [...pos.values()];
        const free = (pt) => states.every((st) => Math.hypot(pt[0] - st[0], pt[1] - st[1]) >= R + 10);
        const lines = paths.map((p) => p.own);
        const boxes = lines.map((l) => boundsOf(l, 6));
        let score = 0;
        for (let i = 0; i < lines.length; i++) for (let j = i + 1; j < lines.length; j++) {
            if (!meet(boxes[i], boxes[j])) continue;
            const P = lines[i];
            const Q = lines[j];
            for (let m = 0; m < P.length - 1; m++) for (let n = 0; n < Q.length - 1; n++) if (segX(P[m], P[m + 1], Q[n], Q[n + 1]) && free(P[m])) score += 1;
            let along = 0;
            for (const pt of P) if (free(pt) && Q.some((q) => Math.hypot(q[0] - pt[0], q[1] - pt[1]) < 6)) along++;
            if (along >= 4) score += 10;
        }
        for (const path of paths) {
            if (path.loop) continue;
            const box = boundsOf(path.own, R + 6);
            for (const [id, st] of pos) if (id !== path.e.from && id !== path.e.to && inside(box, st) && path.own.some((pt) => Math.hypot(pt[0] - st[0], pt[1] - st[1]) < R + 6)) score += 20;
            const b = path.box;
            if (!b) continue;
            for (const o of paths) if (o !== path && o.own.some((pt) => pt[0] > b.x && pt[0] < b.x + b.w && pt[1] > b.y && pt[1] < b.y + b.h)) score += 5;
        }
        return score;
    }

    function layoutOnce(a, options, variant, labels, mode) {
        const ring = options.ring || 0;
        const ids = a.states.map((s) => s.id);
        // the arrows between the same two states as one, their labels joined
        const pairs = new Map();
        for (const e of a.edges) {
            const k = `${e.from}\u0000${e.to}`;
            if (!pairs.has(k)) pairs.set(k, { from: e.from, to: e.to, labels: [] });
            pairs.get(k).labels.push(e.label);
        }
        const edges = [...pairs.values()].map((e) => ({ ...e, label: joinLabels(e.labels) }));
        // long labels (0, 3, 6, 9) need room between the states: the gaps
        // grow with the longest
        const longest = Math.max(0, ...edges.map((e) => 7.8 * e.label.length + 4));
        let GAPX = Math.max(BASE_GAPX, longest + 64);
        let GAPY = Math.max(BASE_GAPY, longest + 40) * (options.spread || 1);
        const out = new Map(ids.map((id) => [id, []]));
        for (const e of edges) out.get(e.from).push(e);
        for (const list of out.values()) list.sort((x, y) => (x.label < y.label ? -1 : 1));
        // columns: the distance from the start (a DEA: the shortest word
        // that gets there); with columns: 'order' (Thompson's NEA, numbered
        // along its arrows) the longest way along the arrows that go forward
        // in the numbering, so that the main path is one row and a skip (the
        // ε past a star) an arc over it, not a short cut to the end
        const depth = new Map([[a.start, 0]]);
        let order = [a.start];
        if (mode === 'order') {
            const index = new Map(ids.map((id, i) => [id, i]));
            for (const id of ids) if (!depth.has(id)) depth.set(id, 0);
            for (const id of ids) for (const e of out.get(id)) if (index.get(e.to) > index.get(id)) depth.set(e.to, Math.max(depth.get(e.to), depth.get(id) + 1));
            order = [...ids].sort((x, y) => depth.get(x) - depth.get(y) || index.get(x) - index.get(y));
        } else {
            for (let i = 0; i < order.length; i++) {
                for (const e of out.get(order[i]) || []) if (!depth.has(e.to)) { depth.set(e.to, depth.get(order[i]) + 1); order.push(e.to); }
            }
            let last = Math.max(0, ...depth.values());
            for (const id of ids) if (!depth.has(id)) { depth.set(id, ++last); order.push(id); }
        }
        const columns = [];
        for (const id of order) (columns[depth.get(id)] = columns[depth.get(id)] || []).push(id);
        // y: a column centred, then every state moved towards the states it
        // is reached from (a few sweeps), keeping GAPY apart
        // arrows within a column put their labels between the columns,
        // among the labels of the arrows to and from the column: more room
        // both ways
        if (edges.some((e) => e.from !== e.to && depth.get(e.from) === depth.get(e.to))) {
            GAPX = Math.max(GAPX, 2 * longest + 30);
            GAPY = Math.max(GAPY, 2 * longest + 20);
        }
        const y = new Map();
        // the first try as found, the others from a shuffled order
        let seed = variant * 7919 + 1;
        const random = () => { seed = (seed * 16807) % 2147483647; return seed / 2147483647; };
        if (variant > 0) for (const col of columns) for (let k = col.length - 1; k > 0; k--) { const j = Math.floor(random() * (k + 1)); [col[k], col[j]] = [col[j], col[k]]; }
        columns.forEach((col) => col.forEach((id, k) => y.set(id, (k - (col.length - 1) / 2) * GAPY)));
        // sweeps from the left (towards the states a state is reached from)
        // and from the right (towards the states it leads to), so that
        // arrows cross as little as they can
        const preds = new Map(ids.map((id) => [id, []]));
        const succs = new Map(ids.map((id) => [id, []]));
        for (const e of edges) if (e.from !== e.to && depth.get(e.from) < depth.get(e.to)) { preds.get(e.to).push(e.from); succs.get(e.from).push(e.to); }
        for (let sweep = 0; sweep < 6; sweep++) {
            const fromLeft = sweep % 2 === 0;
            for (const col of fromLeft ? columns.slice(1) : columns.slice(0, -1).reverse()) {
                const near = fromLeft ? preds : succs;
                const want = col.map((id) => (near.get(id).length ? near.get(id).reduce((s, p) => s + y.get(p), 0) / near.get(id).length : y.get(id)));
                const sorted = col.map((id, k) => ({ id, want: want[k] })).sort((p, q) => p.want - q.want);
                // spaced: from the wanted places, pushed apart where too close
                for (let k = 1; k < sorted.length; k++) sorted[k].want = Math.max(sorted[k].want, sorted[k - 1].want + GAPY);
                const shift = (sorted.reduce((s, p) => s + p.want, 0) - want.reduce((s, w) => s + w, 0)) / sorted.length;
                sorted.forEach((p) => y.set(p.id, p.want - shift));
                col.splice(0, col.length, ...sorted.map((p) => p.id));
            }
        }
        const pos = new Map(ids.map((id) => [id, [depth.get(id) * GAPX, y.get(id)]]));

        // the arrows; every one bends to its left (in screen coordinates the
        // left of (dx, dy) is (dy, -dx)): forward above, back below
        const near = (p, q) => Math.hypot(p[0] - q[0], p[1] - q[1]);
        const quad = (p, c, q, t) => [(1 - t) ** 2 * p[0] + 2 * (1 - t) * t * c[0] + t * t * q[0], (1 - t) ** 2 * p[1] + 2 * (1 - t) * t * c[1] + t * t * q[1]];
        const routed = [];
        for (const e of edges) {
            const p = pos.get(e.from);
            const q = pos.get(e.to);
            if (e.from === e.to) { routed.push({ ...e, loop: true }); continue; }
            const [dx, dy] = [q[0] - p[0], q[1] - p[1]];
            const len = Math.hypot(dx, dy);
            const n = [dy / len, -dx / len];
            const back = pairs.has(`${e.to}\u0000${e.from}`);
            const forwardNeighbour = depth.get(e.to) === depth.get(e.from) + 1;
            // the further an arrow reaches over the columns, the more it
            // bends, so that arrows back to the same state nest
            const span = Math.abs(depth.get(e.to) - depth.get(e.from));
            let bend = forwardNeighbour && !back ? 0 : back && span <= 1 ? 16 : 18 + 16 * Math.max(0, span - 1) + (depth.get(e.to) < depth.get(e.from) ? 10 : 0);
            // arrows back of the same reach graded by where they start, the
            // one starting further right outside, so that they nest
            if (depth.get(e.to) < depth.get(e.from) && span > 1) bend += 9 * depth.get(e.from);
            // further out while the curve passes a state
            const clear = (b) => {
                const c = [(p[0] + q[0]) / 2 + n[0] * 2 * b, (p[1] + q[1]) / 2 + n[1] * 2 * b];
                for (let t = 0.08; t <= 0.92; t += 0.04) {
                    const s = quad(p, c, q, t);
                    for (const [id, o] of pos) if (id !== e.from && id !== e.to && near(s, o) < R + 10) return false;
                }
                return true;
            };
            while (!clear(bend) && bend < 220) bend += 12;
            routed.push({ ...e, n, bend, c: [(p[0] + q[0]) / 2 + n[0] * 2 * bend, (p[1] + q[1]) / 2 + n[1] * 2 * bend] });
        }
        // Arrows within one row are arcs above or below it, as in an arc
        // diagram: two arcs on one side cross when their spans overlap
        // without one holding the other, so each goes to the side where it
        // crosses fewer, the widest first; forward above, back below unless
        // that costs crossings. A pair between neighbours keeps its bend
        const col = (id) => depth.get(id);
        const arcs = routed.filter((e) => !e.loop && Math.abs(pos.get(e.from)[1] - pos.get(e.to)[1]) < 1
            && (Math.abs(col(e.to) - col(e.from)) > 1 || pairs.has(`${e.to}\u0000${e.from}`)));
        const span = (e) => [Math.min(col(e.from), col(e.to)), Math.max(col(e.from), col(e.to))];
        const cross = (a, b) => { const [a0, a1] = span(a); const [b0, b1] = span(b); return (a0 < b0 && b0 < a1 && a1 < b1) || (b0 < a0 && a0 < b1 && b1 < a1); };
        const sides = { [-1]: [], [1]: [] };
        for (const e of [...arcs].sort((x, y) => (span(y)[1] - span(y)[0]) - (span(x)[1] - span(x)[0]))) {
            const preferred = col(e.to) > col(e.from) ? -1 : 1;
            const neighbours = span(e)[1] - span(e)[0] === 1;
            const cost = (side) => sides[side].filter((o) => cross(o, e)).length;
            const side = neighbours ? preferred : cost(-preferred) < cost(preferred) ? -preferred : preferred;
            sides[side].push(e);
            e.arc = side;
            const reach = span(e)[1] - span(e)[0];
            // its height grows with what it spans, and nests inside the
            // arcs that hold it
            e.height = neighbours ? 20 : 24 + 22 * (reach - 1);
            e.n = [0, side];
        }
        // Arrows within a column are arcs beside it, all on the side where
        // fewer arrows leave the column (the right one when even), nested by
        // what they span: their labels stand outside them, not among the
        // labels of the arrows to and from the column
        const inColumn = routed.filter((e) => !e.loop && Math.abs(pos.get(e.from)[0] - pos.get(e.to)[0]) < 1);
        const rowOf = (id) => pos.get(id)[1];
        for (const c of new Set(inColumn.map((e) => depth.get(e.from)))) {
            const mine = inColumn.filter((e) => depth.get(e.from) === c);
            const leaving = (dir) => routed.filter((e) => !e.loop && ((depth.get(e.from) === c && Math.sign(depth.get(e.to) - c) === dir) || (depth.get(e.to) === c && Math.sign(depth.get(e.from) - c) === dir))).length;
            const side = leaving(-1) < leaving(1) ? -1 : 1;
            // the narrower inside: by span, then the one going down; an arc
            // goes outside those it overlaps (and their labels), at least a
            // third as high as it is long, so that it rounds out
            const byLength = [...mine].sort((x, y) => Math.abs(rowOf(x.to) - rowOf(x.from)) - Math.abs(rowOf(y.to) - rowOf(y.from)) || rowOf(y.to) - rowOf(x.to));
            const interval = (e) => [Math.min(rowOf(e.from), rowOf(e.to)), Math.max(rowOf(e.from), rowOf(e.to))];
            const done = [];
            for (const e of byLength) {
                const [lo, hi] = interval(e);
                const under = done.filter((o) => { const [olo, ohi] = interval(o); return olo < hi - 1 && lo < ohi - 1; });
                e.column = side;
                e.height = Math.max(26, (hi - lo) * 0.3, ...under.map((o) => o.height + longest + 28));
                e.n = [side, 0];
                done.push(e);
            }
        }
        // loops: on top, or below when a state is right above
        for (const e of routed.filter((r) => r.loop)) {
            const [x, yy] = pos.get(e.from);
            const above = [...pos.values()].some(([ox, oy]) => Math.abs(ox - x) < 1 && oy < yy && yy - oy < GAPY * 1.2);
            e.side = above ? 1 : -1;
        }
        // Where an arrow leaves and enters its states: aimed at its control
        // point; on every state, ends closer than 22° are pushed apart (a
        // loop's ends stay where they are), so that heads do not pile up
        const angleOf = (o, to) => Math.atan2(to[1] - o[1], to[0] - o[0]);
        let points;
        let paths;
        const shape = () => {
        const ends = new Map(ids.map((id) => [id, []]));
        // the arrow into the start comes from the left
        ends.get(a.start).push({ fixed: true, angle: Math.PI });
        for (const e of routed) {
            const p = pos.get(e.from);
            const q = pos.get(e.to);
            if (e.loop) {
                const s = e.side;
                ends.get(e.from).push({ fixed: true, angle: Math.atan2(s * (R - 2), -10) }, { fixed: true, angle: Math.atan2(s * (R - 2), 10) });
                continue;
            }
            if (e.column) {
                // beside the column: out sideways, turned towards the other end
                const towards = Math.sign(q[1] - p[1]);
                const base = e.column > 0 ? 0 : Math.PI;
                // the wider, the more sideways it leaves, so that it rounds out
                const tilt = (Math.max(8, 30 - e.height * 0.15) * Math.PI) / 180;
                e.out = { angle: base + e.column * towards * tilt };
                e.in = { angle: base - e.column * towards * tilt };
            } else if (e.arc) {
                // an arc leaves and enters diagonally, below or above,
                // turned towards the other end
                const towards = Math.sign(q[0] - p[0]);
                // the wider an arc, the steeper it leaves, so that of two arcs
                // from one state the wider runs outside the narrower
                const reachOf = Math.abs(depth.get(e.to) - depth.get(e.from));
                const tilt = (Math.max(12, 48 - 12 * (reachOf - 1)) * Math.PI) / 180;
                e.out = { angle: e.arc * (Math.PI / 2) - e.arc * towards * tilt };
                e.in = { angle: e.arc * (Math.PI / 2) + e.arc * towards * tilt };
            } else {
                // base: the straight direction, which keeps the order
                e.out = { angle: e.bend === 0 ? angleOf(p, q) : angleOf(p, e.c), base: angleOf(p, q) };
                e.in = { angle: e.bend === 0 ? angleOf(q, p) : angleOf(q, e.c), base: angleOf(q, p) };
            }
            ends.get(e.from).push(e.out);
            ends.get(e.to).push(e.in);
        }
        const MIN = (22 * Math.PI) / 180;
        const wrap = (a) => Math.atan2(Math.sin(a), Math.cos(a));
        for (const end of [...ends.values()].flat()) if (end.base === undefined) end.base = end.angle;
        for (const list of ends.values()) {
            // Ends towards different states keep the order of those states:
            // two arrows that bend towards each other (from a state to two
            // states above and below it) would otherwise swap and cross, so
            // such a pair is put back, MIN apart around its middle
            for (let round = 0; round < 10; round++) {
                let swapped = false;
                for (let i = 0; i < list.length; i++) for (let j = i + 1; j < list.length; j++) {
                    const a = list[i];
                    const b = list[j];
                    const order = wrap(b.base - a.base);
                    // only neighbouring directions: around the circle every
                    // pair has an order, but only these can swap
                    if (Math.abs(order) < 0.05 || Math.abs(order) > Math.PI / 2) continue;
                    const now = wrap(b.angle - a.angle);
                    if (Math.abs(now) > Math.PI / 2 || Math.sign(now) === Math.sign(order)) continue;
                    const dir = Math.sign(order);
                    if (a.fixed && b.fixed) continue;
                    if (a.fixed) b.angle = a.angle + dir * MIN;
                    else if (b.fixed) a.angle = b.angle - dir * MIN;
                    else {
                        const mid = a.angle + now / 2;
                        a.angle = mid - dir * MIN / 2;
                        b.angle = mid + dir * MIN / 2;
                    }
                    swapped = true;
                }
                if (!swapped) break;
            }
            for (let round = 0; round < 30; round++) {
                let moved = false;
                for (let i = 0; i < list.length; i++) for (let j = i + 1; j < list.length; j++) {
                    const a = list[i];
                    const b = list[j];
                    const gap = wrap(b.angle - a.angle);
                    if (Math.abs(gap) >= MIN) continue;
                    const push = (MIN - Math.abs(gap)) / 2 * (gap >= 0 ? 1 : -1) + (gap === 0 ? 0.01 : 0);
                    if (!a.fixed) a.angle -= b.fixed ? 2 * push : push;
                    if (!b.fixed) b.angle += a.fixed ? 2 * push : push;
                    moved = true;
                }
                if (!moved) break;
            }
        }
        // the points of every arrow, for the labels to keep clear of
        points = [];
        paths = routed.map((e) => {
            const p = pos.get(e.from);
            const q = pos.get(e.to);
            if (e.loop) {
                const s = e.side;
                const a0 = [p[0] - 10, p[1] + s * (R - 2)];
                const a1 = [p[0] + 10, p[1] + s * (R - 2)];
                const c0 = [p[0] - 34, p[1] + s * 72];
                const c1 = [p[0] + 34, p[1] + s * 72];
                const own = [];
                for (let t = 0; t <= 1; t += 0.05) {
                    const u = 1 - t;
                    own.push([u ** 3 * a0[0] + 3 * u * u * t * c0[0] + 3 * u * t * t * c1[0] + t ** 3 * a1[0], u ** 3 * a0[1] + 3 * u * u * t * c0[1] + 3 * u * t * t * c1[1] + t ** 3 * a1[1]]);
                }
                points.push(...own);
                const angle = Math.atan2(a1[1] - p[1], a1[0] - p[0]);
                const end = (inset) => `M${a0[0]} ${a0[1]} C${c0[0]} ${c0[1]} ${c1[0]} ${c1[1]} ${p[0] + Math.cos(angle) * inset} ${p[1] + Math.sin(angle) * inset}`;
                return { e, own, d: end(R + 0.4), end, at: () => [p[0], p[1] + s * 64], normal: [0, s], loop: true };
            }
            // a cubic curve that leaves and enters radially at its two
            // angles: the head points along the curve into the centre
            const u0 = [Math.cos(e.out.angle), Math.sin(e.out.angle)];
            const u1 = [Math.cos(e.in.angle), Math.sin(e.in.angle)];
            const s0 = [p[0] + u0[0] * R, p[1] + u0[1] * R];
            const s1 = [q[0] + u1[0] * (R + 2), q[1] + u1[1] * (R + 2)];
            const reach = e.column ? e.height * 1.33 / Math.cos(Math.abs(e.out.angle - (e.column > 0 ? 0 : Math.PI))) : e.arc ? e.height * 1.3 : Math.hypot(s1[0] - s0[0], s1[1] - s0[1]) * (e.bend === 0 ? 0.33 : 0.45) + Math.abs(e.bend) * 0.6;
            const c0 = [s0[0] + u0[0] * reach, s0[1] + u0[1] * reach];
            const c1 = [s1[0] + u1[0] * reach, s1[1] + u1[1] * reach];
            const at = (t) => { const v = 1 - t; return [v ** 3 * s0[0] + 3 * v * v * t * c0[0] + 3 * v * t * t * c1[0] + t ** 3 * s1[0], v ** 3 * s0[1] + 3 * v * v * t * c0[1] + 3 * v * t * t * c1[1] + t ** 3 * s1[1]]; };
            const own = [];
            for (let t = 0; t <= 1; t += 0.03) own.push(at(t));
            points.push(...own);
            const end = (inset) => `M${s0[0]} ${s0[1]} C${c0[0]} ${c0[1]} ${c1[0]} ${c1[1]} ${q[0] + u1[0] * inset} ${q[1] + u1[1] * inset}`;
            return { e, own, d: end(R + 2), end, at, normal: e.n };
        });
        };
        // The curves as they are drawn (after the ends were spread) keep
        // clear of the states they do not join: one that passes a state
        // bends further, or as an arc rises higher, and all are shaped again
        for (let round = 0; ; round++) {
            shape();
            const passing = paths.filter((path) => {
                if (path.loop) return false;
                const box = boundsOf(path.own, R + 8);
                return [...pos].some(([id, o]) => id !== path.e.from && id !== path.e.to && inside(box, o) && path.own.some((pt) => near(pt, o) < R + 8));
            });
            if (!passing.length || round === 12) break;
            for (const { e } of passing) {
                if (e.arc || e.column) e.height += 16;
                else {
                    e.bend += 14;
                    const p = pos.get(e.from);
                    const q = pos.get(e.to);
                    e.c = [(p[0] + q[0]) / 2 + e.n[0] * 2 * e.bend, (p[1] + q[1]) / 2 + e.n[1] * 2 * e.bend];
                }
            }
        }
        if (!labels) return { score: badness(paths, pos) };
        // labels: outside their arrow (to its left), moved out until clear;
        // else on the other side
        const placed = [];
        // each arrow's bounding box: an arrow far from a label is not
        // measured point by point
        for (const path of paths) {
            const xs = path.own.map((pt) => pt[0]);
            const ys = path.own.map((pt) => pt[1]);
            path.bounds = { x: Math.min(...xs), y: Math.min(...ys), w: Math.max(...xs) - Math.min(...xs), h: Math.max(...ys) - Math.min(...ys) };
        }
        const gap = (b, o) => Math.hypot(Math.max(0, o.x - b.x - b.w, b.x - o.x - o.w), Math.max(0, o.y - b.y - b.h, b.y - o.y - o.h));
        const nearestOther = (path, b, enough) => {
            let least = Infinity;
            for (const o of paths) {
                if (o === path || gap(b, o.bounds) >= Math.min(least, enough)) continue;
                for (const pt of o.own) least = Math.min(least, distance(b, pt));
            }
            return least;
        };
        const box = (cx, cy, w) => ({ x: cx - w / 2, y: cy - 8, w, h: 15 });
        const distance = (b, [px, py]) => Math.hypot(Math.max(b.x - px, 0, px - b.x - b.w), Math.max(b.y - py, 0, py - b.y - b.h));
        // side by side, two labels would read as one: more room sideways
        const overlap = (b, o) => !(b.x + b.w + 10 < o.x || o.x + o.w + 10 < b.x || b.y + b.h + 3 < o.y || o.y + o.h + 3 < b.y);
        // a label stays at its own arrow: somewhere along its middle part,
        // at most 25 units out; where nothing is clear, at its middle
        for (const path of paths) {
            const w = 7.8 * path.e.label.length + 4;
            const h = 15;
            let best = null;
            let margin = -Infinity;
            const ts = path.loop ? [0.5] : [0.5, 0.42, 0.58, 0.34, 0.66, 0.26, 0.74];
            for (const side of [1, -1]) {
                for (const t of ts) {
                    for (let off = 9; off <= 25; off += 4) {
                        const [mx, my] = path.at(t);
                        // out along the normal by the box's own extent too
                        const ext = Math.abs(path.normal[0]) * w / 2 + Math.abs(path.normal[1]) * h / 2;
                        const b = box(mx + path.normal[0] * (off + ext - 6) * side, my + path.normal[1] * (off + ext - 6) * side, w);
                        // clear of everything, and clearly nearer to its own
                        // arrow than to any other, so that it is read as its
                        const mine = Math.min(...path.own.map((pt) => distance(b, pt)));
                        const others = nearestOther(path, b, mine + 46);
                        const ok = mine >= 4 && others >= Math.max(4, mine + 6) && [...pos.values()].every((o) => distance(b, o) >= R + ring + 4) && placed.every((o) => !overlap(b, o));
                        // of the clear places the least ambiguous: furthest
                        // from other arrows compared with its own, near the
                        // middle, on the left (the first side)
                        const value = Math.min(others - mine, 40) - Math.abs(t - 0.5) * 8 - off * 0.15 - (side < 0 ? 2 : 0);
                        if (ok && value > margin) { margin = value; best = b; }
                    }
                }
            }
            // nowhere clear: the place with the most room, never on a state
            // or another label, as near its own arrow as it can be
            if (!best) {
                let score = -Infinity;
                for (const side of [1, -1]) for (let t = 0.2; t <= 0.81; t += 0.05) for (let off = 4; off <= 40; off += 3) {
                    const [mx, my] = path.at(path.loop ? 0.5 : t);
                    const ext = Math.abs(path.normal[0]) * w / 2 + Math.abs(path.normal[1]) * h / 2;
                    const b = box(mx + path.normal[0] * (off + ext - 6) * side, my + path.normal[1] * (off + ext - 6) * side, w);
                    const mine = Math.min(...path.own.map((pt) => distance(b, pt)));
                    const others = nearestOther(path, b, 13);
                    const states = Math.min(...[...pos.values()].map((o) => distance(b, o) - R - ring));
                    const free = placed.every((o) => !overlap(b, o));
                    const value = (states < 4 ? -1000 + states : 0) + (free ? 0 : -500) + Math.min(others, 12) * 2 + Math.min(mine, 4) - Math.max(0, mine - 8) - off * 0.1;
                    if (value > score) { score = value; best = b; }
                }
            }
            placed.push(best);
            path.box = best;
        }
        // the whole picture
        const xs = [...[...pos.values()].map((p) => p[0] - R - 44), ...[...pos.values()].map((p) => p[0] + R + 6), ...points.map((p) => p[0]), ...placed.flatMap((b) => [b.x, b.x + b.w])];
        const ys = [...[...pos.values()].flatMap((p) => [p[1] - R - 6, p[1] + R + 6]), ...points.map((p) => p[1]), ...placed.flatMap((b) => [b.y, b.y + b.h])];
        const view = [Math.min(...xs) - 8, Math.min(...ys) - 8, Math.max(...xs) - Math.min(...xs) + 16, Math.max(...ys) - Math.min(...ys) + 16];
        // labels a reader could take for another arrow's
        let ambiguous = 0;
        for (const path of paths) {
            const mine = Math.min(...path.own.map((pt) => distance(path.box, pt)));
            if (nearestOther(path, path.box, mine + 12) < mine + 10) ambiguous++;
        }
        return { pos, paths, view, accept: new Set(a.accept), start: a.start, states: a.states, score: badness(paths, pos), ambiguous };
    }

    // ε stays ε; several symbols in a run: a–c, 0–9
    function joinLabels(labels) {
        const list = [...new Set(labels)].sort();
        const singles = list.filter((l) => l.length === 1 && l !== 'ε');
        const rest = list.filter((l) => !(l.length === 1 && l !== 'ε'));
        const runs = [];
        for (const s of singles) {
            const r = runs[runs.length - 1];
            if (r && s.charCodeAt(0) === r[1].charCodeAt(0) + 1) r[1] = s; else runs.push([s, s]);
        }
        return [...runs.map(([x, y]) => (x === y ? x : y.charCodeAt(0) === x.charCodeAt(0) + 1 ? `${x}, ${y}` : `${x}–${y}`)), ...rest].join(', ');
    }

    // Where no drawing can be read (many states or many arrows): the
    // transition table instead, a row per state, a column per symbol
    // A sparse automaton (Thompson's NEA, almost a chain) reads well as a
    // drawing even with many states, a dense one (a DEA with ten digits)
    // only as a table
    const drawable = (states, arrows) => states <= 50 && arrows <= Math.max(45, 1.4 * states);
    function table(a) {
        const symbols = [...new Set(a.edges.map((e) => e.label))].sort();
        const name = new Map(a.states.map((s) => [s.id, s.label]));
        // r0 as r with a small 0, as in the drawings
        const named = (label) => { const m = String(label).match(NAME); return m ? `<i>${esc(m[1])}<sub>${m[2]}</sub></i>` : esc(label); };
        const rows = a.states.map((s) => `<tr><th>${s.id === a.start ? '<span title="Start">→</span> ' : ''}${named(s.label)}${a.accept.includes(s.id) ? ' <span title="akzeptiert">✱</span>' : ''}</th>${symbols.map((sym) => `<td>${a.edges.filter((e) => e.from === s.id && e.label === sym).map((e) => named(name.get(e.to))).join(', ') || '–'}</td>`).join('')}</tr>`);
        return `<table class="automaton-table"><thead><tr><th>Zustand</th>${symbols.map((s) => `<th>${esc(s)}</th>`).join('')}</tr></thead><tbody>${rows.join('')}</tbody></table>`;
    }

    // the colours come from the page (.automaton-drawn in default.css)
    let drawings = 0;
    const MONO = "'0xProto Nerd Font Mono', monospace";
    function svg(a) {
        const pairs = new Set(a.edges.map((e) => `${e.from} ${e.to}`));
        if (!drawable(a.states.length, pairs.size)) return table(a);
        const L = layout(a);
        const head = `automaton-head-${++drawings}`;
        const parts = [];
        parts.push(`<svg xmlns="http://www.w3.org/2000/svg" viewBox="${L.view.map((v) => Math.round(v * 10) / 10).join(' ')}" style="max-width:${Math.round(L.view[2] * 1.2)}px;min-width:${Math.round(Math.min(L.view[2] * 0.8, 600))}px" class="automaton-drawn" aria-hidden="true">`);
        const marker = (id, cls) => `<marker id="${id}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="9" markerHeight="9" markerUnits="userSpaceOnUse" orient="auto-start-reverse"><path d="M0 0L10 5L0 10z" class="${cls}"/></marker>`;
        parts.push(`<defs>${marker(head, 'automaton-head')}${marker(`${head}-on`, 'automaton-head-on')}</defs>`);
        const round = (d) => d.replace(/-?\d+\.\d+/g, (n) => String(Math.round(Number(n) * 10) / 10));
        // every arrow, its wide invisible line to point at and its label
        // share data-edge: pointing at one marks all of them (hover)
        const edge = (p) => esc(`${p.e.from}>${p.e.to}`).replace(/"/g, '&quot;');
        for (const p of L.paths) parts.push(`<path d="${round(p.d)}" class="automaton-arrow" data-edge="${edge(p)}" data-head-on="url(#${head}-on)" marker-end="url(#${head})"/>`);
        for (const p of L.paths) parts.push(`<path d="${round(p.d)}" class="automaton-pick" data-edge="${edge(p)}"/>`);
        for (const p of L.paths) parts.push(`<text x="${Math.round(p.box.x + p.box.w / 2)}" y="${Math.round(p.box.y + 12)}" text-anchor="middle" font-size="13" font-family="${MONO}" class="automaton-label" data-edge="${edge(p)}">${esc(p.e.label)}</text>`);
        const [sx, sy] = L.pos.get(L.start);
        parts.push(`<path d="M${sx - R - 36} ${sy} L${sx - R - 2} ${sy}" class="automaton-arrow" marker-end="url(#${head})"/>`);
        for (const s of L.states) {
            const [x, y] = L.pos.get(s.id);
            parts.push(`<circle cx="${x}" cy="${y}" r="${R}" class="automaton-state"/>`);
            if (L.accept.has(s.id)) parts.push(`<circle cx="${x}" cy="${y}" r="${R - 4}" class="automaton-accepting"/>`);
            const m = String(s.label).match(NAME);
            parts.push(m ? `<text x="${x}" y="${y + 5}" text-anchor="middle" font-size="15" font-family="${MONO}" font-style="italic" class="automaton-name">${esc(m[1])}<tspan dy="0.3em" font-size="0.7em">${m[2]}</tspan></text>`
                : `<text x="${x}" y="${y + 5}" text-anchor="middle" font-size="15" font-family="${MONO}" class="automaton-name">${esc(s.label)}</text>`);
        }
        parts.push('</svg>');
        return parts.join('');
    }

    // The data attributes of a drawing or a step widget: states, start,
    // accepting states, transitions 'q0 a-c q1' (one arrow, a run of
    // symbols) separated by commas
    function fromData(data) {
        const ids = data.states.trim().split(/\s+/);
        const edges = [];
        for (const part of (data.transitions || '').split(',').map((t) => t.trim()).filter(Boolean)) {
            const [from, symbols, to] = part.split(/\s+/);
            const run = symbols.match(/^(.)-(.)$/);
            if (run) for (let c = run[1].charCodeAt(0); c <= run[2].charCodeAt(0); c++) edges.push({ from, to, label: String.fromCharCode(c) });
            else edges.push({ from, to, label: symbols });
        }
        return { states: ids.map((id) => ({ id, label: id })), start: data.start || ids[0], accept: (data.accept || '').trim().split(/\s+/).filter(Boolean), edges };
    }

    // Pointing at an arrow (or its wide invisible line) or at its label marks
    // both, the arrowhead too: in a crowded drawing it shows which label
    // belongs to which arrow. Elements of one arrow share data-edge; an
    // arrow names its marked head in data-head-on
    function hover(svg) {
        if (svg.dataset.hover) return;
        svg.dataset.hover = '1';
        let current = null;
        const mark = (key, on) => {
            for (const node of svg.querySelectorAll('[data-edge]')) {
                if (node.dataset.edge !== key) continue;
                node.classList.toggle('edge-hover', on);
                if (!node.dataset.headOn) continue;
                if (on) { node.dataset.headOff = node.getAttribute('marker-end') || ''; node.setAttribute('marker-end', node.dataset.headOn); }
                else if (node.dataset.headOff !== undefined) { node.setAttribute('marker-end', node.dataset.headOff); delete node.dataset.headOff; }
            }
        };
        const to = (key) => {
            if (key === current) return;
            if (current !== null) mark(current, false);
            current = key;
            if (key !== null) mark(key, true);
        };
        // only a pointer that moves: content that appears under one at rest
        // (a page that scrolls, a screenshot) marks nothing
        svg.addEventListener('pointermove', (event) => { const node = event.target.closest && event.target.closest('[data-edge]'); to(node ? node.dataset.edge : null); });
        svg.addEventListener('pointerleave', () => to(null));
    }

    root.AutomatonLayout = { R, layout, svg, table, fromData, joinLabels, hover };

    // the drawings in the page
    if (typeof document === 'undefined') return;
    const start = () => document.querySelectorAll('.automaton-drawing').forEach((el) => {
        // a box with data-regex is Automatron's (automatron.js draws it)
        if (el.dataset.drawn || !el.dataset.states) return;
        el.dataset.drawn = '1';
        el.innerHTML = svg(fromData(el.dataset));
        if (el.querySelector('svg')) hover(el.querySelector('svg'));
    });
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})(typeof window !== 'undefined' ? window : globalThis);
