// Colour classes in 3D: the RGB cube cut into cubes and the colours of RGB
// in CIELab cut into boxes, both drawn with WebGL, shaded by the normals of
// their faces. Drag turns both views, a slider pulls the classes apart, a
// quarter can be taken out to look inside. A click picks a colour: its class
// stays solid in both views (the rest turns to glass), and two swatches show
// which colours the class in RGB and the class in Lab lump together.
//
// <div class='color-classes' data-split='lab'></div>
//   data-split  the division shown first: rgb (cubes) or lab (boxes)
//   data-views  the views, 'rgb lab' (default) or one of them
//   data-lock   no switch between the divisions
//   data-plain  no swatches
//
// The division level k gives k × k × k cubes in RGB; Lab gets boxes in the
// proportions 78 : 48 : 46 of the activity, as many that a colour lies in
// a class as big as its cube in RGB, on average. (Counting the boxes would
// be unfair to Lab: many of them hold only a sliver at the edge.)

(function (root) {
    'use strict';

    // --- Colour maths, as on the page -------------------------------------

    const LINEAR = Array.from({ length: 256 }, (_, v) => {
        const c = v / 255;
        return c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4;
    });
    const f = (t) => t > 216 / 24389 ? Math.cbrt(t) : (24389 / 27 * t + 16) / 116;
    const A_MIN = -86.2, A_MAX = 98.3, B_MIN = -107.9, B_MAX = 94.5;

    function lab(color) {
        const r = LINEAR[color >> 16], g = LINEAR[(color >> 8) & 255], b = LINEAR[color & 255];
        const x = (0.4124 * r + 0.3576 * g + 0.1805 * b) / 0.95047;
        const y = 0.2126 * r + 0.7152 * g + 0.0722 * b;
        const z = (0.0193 * r + 0.1192 * g + 0.9505 * b) / 1.08883;
        const fx = f(x), fy = f(y), fz = f(z);
        return [116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)];
    }

    // Back from Lab: the colour 0xRRGGBB, or -1 if no screen can show it
    const finv = (t) => t ** 3 > 216 / 24389 ? t ** 3 : (116 * t - 16) / (24389 / 27);
    const encode = (c) => 255 * (c <= 0.0031308 ? 12.92 * c : 1.055 * c ** (1 / 2.4) - 0.055);
    function fromLab(L, a, b) {
        const fy = (L + 16) / 116;
        const x = finv(fy + a / 500) * 0.95047, y = finv(fy), z = finv(fy - b / 200) * 1.08883;
        const rgb = [
            3.2406 * x - 1.5372 * y - 0.4986 * z,
            -0.9689 * x + 1.8758 * y + 0.0415 * z,
            0.0557 * x - 0.2040 * y + 1.0570 * z,
        ];
        if (rgb.some((c) => c < -0.002 || c > 1.002)) return -1;
        const [r, g, bl] = rgb.map((c) => Math.max(0, Math.min(255, Math.round(encode(Math.max(0, c))))));
        return (r << 16) | (g << 8) | bl;
    }

    // --- The two divisions ----------------------------------------------------

    // k cubes along each channel: the class of a value v is v * k / 256
    function boxCell(color, k) {
        const r = color >> 16, g = (color >> 8) & 255, b = color & 255;
        return ((Math.floor(r * k / 256) * k) + Math.floor(g * k / 256)) * k + Math.floor(b * k / 256);
    }
    function boxRange(index, k) {
        return [Math.ceil(index * 256 / k), Math.ceil((index + 1) * 256 / k) - 1];
    }

    const level = (value, low, high, n) => Math.min(n - 1, Math.max(0, Math.floor((value - low) / (high - low) * n)));
    function labCell(L, a, b, n) {
        return (level(L, 0, 100, n[0]) * n[1] + level(a, A_MIN, A_MAX, n[1])) * n[2] + level(b, B_MIN, B_MAX, n[2]);
    }
    function labRange(cell, n) {
        const k = cell % n[2], j = Math.floor(cell / n[2]) % n[1], i = Math.floor(cell / n[2] / n[1]);
        const part = (x, low, high, m) => [low + (high - low) * x / m, low + (high - low) * (x + 1) / m];
        return [part(i, 0, 100, n[0]), part(j, A_MIN, A_MAX, n[1]), part(k, B_MIN, B_MAX, n[2])];
    }

    // Every colour 2 + 4i in each channel, 262.144 of them, standing for 64
    // colours each: enough to count the colours of a class.
    let sampleCache = null;
    function samples() {
        if (sampleCache) return sampleCache;
        const count = 64 ** 3;
        const color = new Int32Array(count), L = new Float32Array(count), a = new Float32Array(count), b = new Float32Array(count);
        for (let i = 0; i < count; i++) {
            const c = (((i >> 12) * 4 + 2) << 16) | ((((i >> 6) & 63) * 4 + 2) << 8) | ((i & 63) * 4 + 2);
            const l = lab(c);
            color[i] = c; L[i] = l[0]; a[i] = l[1]; b[i] = l[2];
        }
        sampleCache = { count, color, L, a, b };
        return sampleCache;
    }
    const countCache = new Map();
    function labCounts(n) {
        const key = n.join(' ');
        if (countCache.has(key)) return countCache.get(key);
        const s = samples();
        const counts = new Map();
        for (let i = 0; i < s.count; i++) {
            const cell = labCell(s.L[i], s.a[i], s.b[i], n);
            counts.set(cell, (counts.get(cell) || 0) + 64);
        }
        countCache.set(key, counts);
        return counts;
    }

    // How many colours the class of a random colour holds, on average:
    // a class with s colours is hit s times, so it is Σ s² / Σ s.
    function feltSize(counts) {
        let squares = 0, total = 0;
        for (const size of counts.values()) { squares += size * size; total += size; }
        return squares / total;
    }

    // The Lab boxes for division level k: classes as big as the cubes of 256 / k
    const levelCache = new Map();
    function labLevels(k) {
        if (levelCache.has(k)) return levelCache.get(k);
        const target = 16777216 / k ** 3;
        let low = 0.01, high = 4, best = null;
        for (let step = 0; step < 20; step++) {
            const middle = (low + high) / 2;
            const n = [78, 48, 46].map((x) => Math.max(1, Math.round(x * middle)));
            const counts = labCounts(n), size = feltSize(counts);
            if (!best || Math.abs(Math.log(size / target)) < Math.abs(Math.log(best.size / target))) best = { n, size, filled: counts.size };
            if (size > target) low = middle; else high = middle;
        }
        levelCache.set(k, best);
        return best;
    }

    // How far apart two colours of a class are, on average, in Lab units:
    // all in all and in lightness alone
    function meanDistance(colors) {
        const labs = colors.map(lab);
        let sum = 0, light = 0, pairs = 0;
        for (let i = 0; i < labs.length; i++) for (let j = i + 1; j < labs.length; j++) {
            sum += Math.hypot(labs[i][0] - labs[j][0], labs[i][1] - labs[j][1], labs[i][2] - labs[j][2]);
            light += Math.abs(labs[i][0] - labs[j][0]);
            pairs++;
        }
        return pairs ? [sum / pairs, light / pairs] : [0, 0];
    }

    // --- Geometry ---------------------------------------------------------------
    //
    // Every class is drawn as what it is, a solid with smooth faces. A point
    // of its surface carries both its Lab and its RGB values, [L, a, b, r, g,
    // b] with r, g, b from 0 to 1 as stored, so the same triangles go into
    // either view. A cube of RGB is its six faces, a box of Lab is its six
    // faces cut off where the colours of RGB end, plus the part of the outside
    // of those colours (the faces of the RGB cube, bent) that lies in the box.

    const toLinear = (v) => v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4;
    const toStored = (c) => { c = Math.max(0, Math.min(1, c)); return c <= 0.0031308 ? 12.92 * c : 1.055 * c ** (1 / 2.4) - 0.055; };

    function labOfUnit(r, g, b) {
        const lr = toLinear(r), lg = toLinear(g), lb = toLinear(b);
        const x = (0.4124 * lr + 0.3576 * lg + 0.1805 * lb) / 0.95047;
        const y = 0.2126 * lr + 0.7152 * lg + 0.0722 * lb;
        const z = (0.0193 * lr + 0.1192 * lg + 0.9505 * lb) / 1.08883;
        const fx = f(x), fy = f(y), fz = f(z);
        return [116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)];
    }
    // The light of a Lab colour in R, G and B, not cut off at 0 and 1
    function linearOfLab(L, a, b) {
        const fy = (L + 16) / 116;
        const x = finv(fy + a / 500) * 0.95047, y = finv(fy), z = finv(fy - b / 200) * 1.08883;
        return [3.2406 * x - 1.5372 * y - 0.4986 * z, -0.9689 * x + 1.8758 * y + 0.0415 * z, 0.0557 * x - 0.2040 * y + 1.0570 * z];
    }
    // How far inside the colours of RGB a Lab colour lies: below 0 it is outside
    function inside(L, a, b) {
        const c = linearOfLab(L, a, b);
        return Math.min(c[0], 1 - c[0], c[1], 1 - c[1], c[2], 1 - c[2]);
    }
    const pointOfUnit = (r, g, b) => [...labOfUnit(r, g, b), r, g, b];
    const pointOfLab = (L, a, b) => [L, a, b, ...linearOfLab(L, a, b).map(toStored)];

    // The part of a polygon where `values` (one per corner) is at least 0; new
    // corners lie where the values, taken as linear along an edge, are 0.
    function clip(polygon, values) {
        const out = [];
        for (let i = 0; i < polygon.length; i++) {
            const j = (i + 1) % polygon.length, p = polygon[i], q = polygon[j], vp = values[i], vq = values[j];
            if (vp >= 0) out.push(p);
            if ((vp >= 0) !== (vq >= 0)) {
                const t = vp / (vp - vq);
                out.push(p.map((x, m) => x + (q[m] - x) * t));
            }
        }
        return out;
    }

    // Cut polygons along the planes `axis` = value (sorted), into pieces
    function splitAlong(polygons, axis, planes) {
        const out = [];
        for (const polygon of polygons) {
            let rest = polygon;
            const coords = polygon.map((p) => p[axis]);
            const low = Math.min(...coords), high = Math.max(...coords);
            for (const plane of planes) {
                if (plane <= low || rest.length < 3) continue;
                if (plane >= high) break;
                const values = rest.map((p) => p[axis] - plane);
                const below = clip(rest, values.map((v) => -v));
                rest = clip(rest, values);
                if (below.length >= 3) out.push(below);
            }
            if (rest.length >= 3) out.push(rest);
        }
        return out;
    }

    // A grid of m × n squares over a face, each as two triangles; corner(u, v) for u, v from 0 to 1
    function grid(corner, m, n) {
        const out = [];
        for (let i = 0; i < m; i++) for (let j = 0; j < n; j++) {
            const p = corner(i / m, j / n), q = corner((i + 1) / m, j / n), r = corner((i + 1) / m, (j + 1) / n), s = corner(i / m, (j + 1) / n);
            out.push([p, q, r], [p, r, s]);
        }
        return out;
    }

    // The six faces of a box [low, high] in three coordinates, each a grid of
    // m × m squares; point(x, y, z) gives the point of the surface
    function boxFaces(low, high, m, point) {
        const out = [];
        for (let axis = 0; axis < 3; axis++) {
            const u = (axis + 1) % 3, w = (axis + 2) % 3;
            for (const side of [low[axis], high[axis]]) {
                out.push(...grid((s, t) => {
                    const c = [0, 0, 0];
                    c[axis] = side;
                    c[u] = low[u] + (high[u] - low[u]) * s;
                    c[w] = low[w] + (high[w] - low[w]) * t;
                    return point(c[0], c[1], c[2]);
                }, m, m));
            }
        }
        return out;
    }

    // Triangles as one array of floats, 18 per triangle
    function pack(polygons) {
        const out = [];
        for (const polygon of polygons) {
            for (let i = 1; i + 1 < polygon.length; i++) out.push(...polygon[0], ...polygon[i], ...polygon[i + 1]);
        }
        return out;
    }

    // The cubes of RGB: k × k × k of them
    function cubeClasses(k) {
        const m = Math.max(1, Math.round(24 / k));
        const classes = new Map();
        for (let i = 0; i < k; i++) for (let j = 0; j < k; j++) for (let l = 0; l < k; l++) {
            const low = [i / k, j / k, l / k], high = [(i + 1) / k, (j + 1) / k, (l + 1) / k];
            classes.set((i * k + j) * k + l, pack(boxFaces(low, high, m, pointOfUnit)));
        }
        return classes;
    }

    // The boxes of Lab, n[0] × n[1] × n[2] of them, cut to the colours of RGB
    function boxClasses(n) {
        const lows = [0, A_MIN, B_MIN], sizes = [100 / n[0], (A_MAX - A_MIN) / n[1], (B_MAX - B_MIN) / n[2]];
        const planes = [0, 1, 2].map((axis) => Array.from({ length: n[axis] - 1 }, (_, i) => lows[axis] + (i + 1) * sizes[axis]));
        const cellAt = (p) => labCell(p[0], p[1], p[2], n);
        const pieces = new Map();
        const add = (cell, polygons) => {
            if (!pieces.has(cell)) pieces.set(cell, []);
            pieces.get(cell).push(...polygons);
        };
        // the outside of the colours, cut along the planes between the boxes
        const surface = shellPolygons(40);
        for (const piece of splitAlong(splitAlong(splitAlong(surface, 0, planes[0]), 1, planes[1]), 2, planes[2])) {
            const centre = [0, 1, 2].map((x) => piece.reduce((s, p) => s + p[x], 0) / piece.length);
            add(cellAt(centre), [piece]);
        }
        // the faces of every box that holds colours, where they lie inside
        const cells = new Set([...labCounts(n).keys(), ...pieces.keys()]);
        const steps = sizes.map((size, axis) => Math.max(1, Math.ceil(size / [4, 6, 6][axis])));
        for (const cell of cells) {
            const [[l0, l1], [a0, a1], [b0, b1]] = labRange(cell, n);
            const faces = boxFaces([l0, a0, b0], [l1, a1, b1], Math.max(...steps), (L, a, b) => [L, a, b, inside(L, a, b)]);
            const kept = [];
            for (const triangle of faces) {
                const part = clip(triangle, triangle.map((p) => p[3]));
                if (part.length >= 3) kept.push(part.map((p) => pointOfLab(p[0], p[1], p[2])));
            }
            add(cell, kept);
        }
        const classes = new Map();
        for (const [cell, polygons] of pieces) classes.set(cell, pack(polygons));
        return classes;
    }

    // The outside of the RGB cube: its six faces, s × s squares each
    function shellPolygons(s) {
        return boxFaces([0, 0, 0], [1, 1, 1], s, pointOfUnit);
    }

    const geometryCache = new Map();
    function geometry(split, k) {
        const key = `${split} ${k}`;
        if (!geometryCache.has(key)) {
            if (geometryCache.size > 3) geometryCache.delete(geometryCache.keys().next().value);
            geometryCache.set(key, split === 'rgb' ? cubeClasses(k) : boxClasses(labLevels(k).n));
        }
        return geometryCache.get(key);
    }
    let shellCache = null;
    const shell = () => shellCache || (shellCache = new Map([[-7, pack(shellPolygons(24))]]));

    // --- The mesh -------------------------------------------------------------
    //
    // The triangles of the classes in one view: the RGB view puts red to the
    // right, blue up and green to the front, from -0.5 to 0.5; the Lab view a
    // to the right, L up and -b to the front, in Lab units / 100. Every
    // triangle gets its normal for the light; pulled apart, each class moves
    // away from the centre by its centroid times the uniform uExplode.

    const VIEWS = {
        rgb: (t, i) => [t[i + 3] - 0.5, t[i + 5] - 0.5, t[i + 4] - 0.5],
        lab: (t, i) => [t[i + 1] / 100, (t[i] - 50) / 100, -t[i + 2] / 100],
    };

    // Each class in one view, once: its corners, normals and colours, and its
    // centroid, weighted by the area of its triangles
    const prepared = new WeakMap();
    function prepare(classes, space) {
        if (!prepared.has(classes)) prepared.set(classes, {});
        const cache = prepared.get(classes);
        if (cache[space]) return cache[space];
        const view = VIEWS[space];
        const out = new Map();
        for (const [cell, t] of classes) {
            const count = t.length / 18;
            const pos = new Float32Array(count * 9), normal = new Float32Array(count * 9), rgba = new Uint8Array(count * 12);
            const sum = [0, 0, 0];
            let area = 0;
            for (let q = 0; q < count; q++) {
                const i = q * 18;
                const p = view(t, i), u = view(t, i + 6), w = view(t, i + 12);
                const n = cross(sub(u, p), sub(w, p)), length = Math.hypot(n[0], n[1], n[2]);
                pos.set(p, q * 9); pos.set(u, q * 9 + 3); pos.set(w, q * 9 + 6);
                for (let m = 0; m < 3; m++) {
                    for (let x = 0; x < 3; x++) normal[q * 9 + m * 3 + x] = length ? n[x] / length : 0;
                    const o = i + m * 6;
                    rgba[q * 12 + m * 4] = Math.round(t[o + 3] * 255);
                    rgba[q * 12 + m * 4 + 1] = Math.round(t[o + 4] * 255);
                    rgba[q * 12 + m * 4 + 2] = Math.round(t[o + 5] * 255);
                    rgba[q * 12 + m * 4 + 3] = 255;
                }
                for (let x = 0; x < 3; x++) sum[x] += (p[x] + u[x] + w[x]) / 3 * length;
                area += length;
            }
            out.set(cell, { count, pos, normal, rgba, centroid: area > 0 ? sum.map((v) => v / area) : view(t, 0) });
        }
        return (cache[space] = out);
    }

    function buildMesh(classes, space, options = {}) {
        const { cut = false, keep = -1 } = options;
        const parts = prepare(classes, space);
        // the quarter in front, towards +x and +z, comes out (all but the kept class)
        const shown = ([cell, part]) => cell === keep || !cut || !(part.centroid[0] > 0 && part.centroid[2] > 0);
        const chosen = [...parts].filter(shown);
        const total = chosen.reduce((sum, [, part]) => sum + part.count, 0);
        const pos = new Float32Array(total * 9), shift = new Float32Array(total * 9), normal = new Float32Array(total * 9);
        const rgba = new Uint8Array(total * 12), info = new Float32Array(total * 3);
        let at = 0, radius = 0, reach = 0;
        for (const [cell, part] of chosen) {
            pos.set(part.pos, at * 9);
            normal.set(part.normal, at * 9);
            rgba.set(part.rgba, at * 12);
            const c = part.centroid;
            for (let v = at * 3; v < (at + part.count) * 3; v++) { shift[v * 3] = c[0]; shift[v * 3 + 1] = c[1]; shift[v * 3 + 2] = c[2]; }
            info.fill(cell, at * 3, (at + part.count) * 3);
            for (let v = 0; v < part.pos.length; v += 3) radius = Math.max(radius, part.pos[v] ** 2 + part.pos[v + 1] ** 2 + part.pos[v + 2] ** 2);
            reach = Math.max(reach, Math.hypot(c[0], c[1], c[2]));
            at += part.count;
        }
        // pulled apart by e, nothing is further out than radius + e * reach
        return { triangles: total, pos, shift, normal, rgba, info, radius: Math.sqrt(radius), reach };
    }
    const sub = (p, q) => [p[0] - q[0], p[1] - q[1], p[2] - q[2]];
    const cross = (p, q) => [p[1] * q[2] - p[2] * q[1], p[2] * q[0] - p[0] * q[2], p[0] * q[1] - p[1] * q[0]];

    // --- The swatches -----------------------------------------------------------
    //
    // A flat area of the colour after hiding something in it: every pixel
    // takes a random colour of its class. Random, but the same every time.

    const MOSAIC = 4096;
    function random(seed) {
        let x = seed >>> 0 || 1;
        return () => { x ^= x << 13; x >>>= 0; x ^= x >> 17; x ^= x << 5; x >>>= 0; return x / 4294967296; };
    }

    function boxSwatch(color, k, count = MOSAIC) {
        const ranges = [color >> 16, (color >> 8) & 255, color & 255].map((v) => boxRange(Math.floor(v * k / 256), k));
        const next = random(color * 31 + k);
        const pick = ([low, high]) => low + Math.floor(next() * (high - low + 1));
        const colors = Array.from({ length: count }, () => (pick(ranges[0]) << 16) | (pick(ranges[1]) << 8) | pick(ranges[2]));
        return { colors, ranges, size: ranges.reduce((p, [low, high]) => p * (high - low + 1), 1) };
    }

    function labSwatch(color, n, count = MOSAIC) {
        const [L, a, b] = lab(color);
        const cell = labCell(L, a, b, n);
        // the sampled colours of the class, each moved a little within its 4 × 4 × 4 colours
        const s = samples();
        const members = [];
        for (let i = 0; i < s.count; i++) if (labCell(s.L[i], s.a[i], s.b[i], n) === cell) members.push(s.color[i]);
        const next = random(color * 37 + n[0] * 7 + n[1]);
        const colors = [];
        for (let tries = 0; colors.length < count && tries < count * 20; tries++) {
            const base = members.length ? members[Math.floor(next() * members.length)] : color;
            const c = base - 0x020202 + ((Math.floor(next() * 4) << 16) | (Math.floor(next() * 4) << 8) | Math.floor(next() * 4));
            const l = lab(c);
            if (labCell(l[0], l[1], l[2], n) === cell) colors.push(c);
        }
        while (colors.length < count) colors.push(color);
        return { colors, ranges: labRange(cell, n), size: labCounts(n).get(cell) || 64 };
    }

    // --- WebGL ----------------------------------------------------------------

    const VERTEX = `
        attribute vec3 aPos;
        attribute vec3 aShift;
        attribute vec3 aNormal;
        attribute vec4 aColor;
        attribute float aCell;
        uniform mat3 uRot;
        uniform vec3 uScale;
        uniform float uKeep;
        uniform float uExplode;
        uniform int uMode;     // 0 all, 1 only the kept class, 2 all but it (glass), 3 picking
        varying vec4 vColor;
        void main() {
            bool kept = abs(aCell - uKeep) < 0.5;
            if ((uMode == 1 && !kept) || (uMode == 2 && kept)) { gl_Position = vec4(2.0, 2.0, 2.0, 1.0); return; }
            vec3 p = uRot * (aPos + aShift * uExplode);
            gl_Position = vec4(p.x * uScale.x, p.y * uScale.y, -p.z * uScale.z, 1.0);
            // picking: the colour itself, unlit
            if (uMode == 3) { vColor = vec4(aColor.rgb, 1.0); return; }
            // lit from both sides: a face seen from behind turns its normal around
            vec3 n = uRot * aNormal;
            if (n.z < 0.0) n = -n;
            vec3 light = normalize(vec3(-0.45, 0.75, 0.55));
            float shade = 0.42 + 0.48 * max(dot(n, light), 0.0) + 0.16 * max(n.z, 0.0);
            vColor = uMode == 2 ? vec4(aColor.rgb * shade, 0.22) : vec4(aColor.rgb * shade, 1.0);
        }`;
    const FRAGMENT = `
        precision mediump float;
        varying vec4 vColor;
        void main() { gl_FragColor = vColor; }`;

    function makeRenderer(canvas) {
        const gl = canvas.getContext('webgl', { antialias: true, alpha: true, preserveDrawingBuffer: true });
        if (!gl) return null;
        const shader = (type, source) => {
            const s = gl.createShader(type);
            gl.shaderSource(s, source);
            gl.compileShader(s);
            if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
            return s;
        };
        const program = gl.createProgram();
        gl.attachShader(program, shader(gl.VERTEX_SHADER, VERTEX));
        gl.attachShader(program, shader(gl.FRAGMENT_SHADER, FRAGMENT));
        gl.linkProgram(program);
        gl.useProgram(program);
        const attr = (name) => gl.getAttribLocation(program, name);
        const uni = (name) => gl.getUniformLocation(program, name);
        const meshes = {};
        let mesh = null;
        const pickTarget = { fb: null, w: 0, h: 0 };

        const LAYOUT = [['pos', 'aPos', 3, gl.FLOAT, false], ['shift', 'aShift', 3, gl.FLOAT, false],
            ['normal', 'aNormal', 3, gl.FLOAT, false], ['rgba', 'aColor', 4, gl.UNSIGNED_BYTE, true], ['info', 'aCell', 1, gl.FLOAT, false]];
        // name: 'classes' or 'shell'
        function upload(name, m) {
            const entry = meshes[name] || (meshes[name] = { buffers: {} });
            entry.mesh = m;
            for (const [key] of LAYOUT) {
                const buffer = entry.buffers[key] || (entry.buffers[key] = gl.createBuffer());
                gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
                gl.bufferData(gl.ARRAY_BUFFER, m[key], gl.STATIC_DRAW);
            }
        }
        function use(name) {
            const entry = meshes[name];
            mesh = entry.mesh;
            for (const [key, attribute, size, type, normalized] of LAYOUT) {
                gl.bindBuffer(gl.ARRAY_BUFFER, entry.buffers[key]);
                const location = attr(attribute);
                gl.enableVertexAttribArray(location);
                gl.vertexAttribPointer(location, size, type, normalized, 0, 0);
            }
        }

        function setView(view) {
            const { yaw, pitch } = view;
            const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
            // first about the vertical axis, then tilted towards the viewer; column-major
            const rot = [cy, sp * sy, -cp * sy, 0, cp, sp, sy, -sp * cy, cp * cy];
            const fit = meshes.shell ? meshes.shell.mesh : mesh;
            const s = 0.92 / Math.max(0.1, fit.radius + view.explode * meshes.classes.mesh.reach);
            const aspect = canvas.width / canvas.height;
            const scale = aspect > 1 ? [s / aspect, s, s * 0.5] : [s, s * aspect, s * 0.5];
            gl.uniformMatrix3fv(uni('uRot'), false, new Float32Array(rot));
            gl.uniform1f(uni('uExplode'), view.keep >= 0 ? 0 : view.explode);
            gl.uniform3fv(uni('uScale'), new Float32Array(scale));
            return { rot, scale };
        }

        function pass(mode) {
            gl.uniform1i(uni('uMode'), mode);
            gl.drawArrays(gl.TRIANGLES, 0, mesh.triangles * 3);
        }

        // With a chosen class (view.keep), it stays solid in its place and the
        // outside of the whole solid is glass around it.
        function draw(view) {
            gl.bindFramebuffer(gl.FRAMEBUFFER, null);
            gl.viewport(0, 0, canvas.width, canvas.height);
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
            if (!meshes.classes) return null;
            use('classes');
            const projection = setView(view);
            gl.enable(gl.DEPTH_TEST);
            gl.disable(gl.BLEND);
            gl.depthMask(true);
            gl.uniform1f(uni('uKeep'), view.keep);
            if (view.keep < 0) { pass(0); return projection; }
            pass(1);
            use('shell');
            gl.enable(gl.BLEND);
            gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
            gl.depthMask(false);
            pass(2);
            gl.depthMask(true);
            gl.disable(gl.BLEND);
            return projection;
        }

        // The colour 0xRRGGBB under a point of the canvas (in canvas pixels), or -1
        function pick(view, x, y) {
            if (!meshes.classes) return -1;
            if (pickTarget.w !== canvas.width || pickTarget.h !== canvas.height) {
                pickTarget.fb = gl.createFramebuffer();
                gl.bindFramebuffer(gl.FRAMEBUFFER, pickTarget.fb);
                const texture = gl.createTexture();
                gl.bindTexture(gl.TEXTURE_2D, texture);
                gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, canvas.width, canvas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
                gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
                gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
                const depth = gl.createRenderbuffer();
                gl.bindRenderbuffer(gl.RENDERBUFFER, depth);
                gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, canvas.width, canvas.height);
                gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, depth);
                pickTarget.w = canvas.width;
                pickTarget.h = canvas.height;
            }
            gl.bindFramebuffer(gl.FRAMEBUFFER, pickTarget.fb);
            gl.viewport(0, 0, canvas.width, canvas.height);
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
            use('classes');
            setView(view);
            gl.enable(gl.DEPTH_TEST);
            gl.disable(gl.BLEND);
            gl.uniform1f(uni('uKeep'), -1);
            pass(3);
            const pixel = new Uint8Array(4);
            gl.readPixels(Math.round(x), Math.round(canvas.height - y), 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel);
            gl.bindFramebuffer(gl.FRAMEBUFFER, null);
            return pixel[3] ? (pixel[0] << 16) | (pixel[1] << 8) | pixel[2] : -1;
        }

        return { upload, draw, pick };
    }

    // --- The widget -------------------------------------------------------------

    const hex = (c) => '#' + c.toString(16).padStart(6, '0');
    const triple = (c) => `${c >> 16}, ${(c >> 8) & 255}, ${c & 255}`;
    const number = (x) => Math.round(x).toLocaleString('de-DE');
    const decimal = (x) => x.toLocaleString('de-DE', { minimumFractionDigits: 1, maximumFractionDigits: 1 }).replace('-', '−');

    const LABELS = {
        rgb: [['Schwarz', [-0.5, -0.5, -0.5]], ['Weiß', [0.5, 0.5, 0.5]], ['Rot', [0.5, -0.5, -0.5]],
            ['Grün', [-0.5, -0.5, 0.5]], ['Blau', [-0.5, 0.5, -0.5]]],
        lab: [['L = 0', [0, -0.5, 0]], ['L = 100', [0, 0.5, 0]], ['+a, rot', [0.98, 0, 0]], ['−a, grün', [-0.86, 0, 0]],
            ['+b, gelb', [0, 0, -0.95]], ['−b, blau', [0, 0, 1.08]]],
    };

    function mount(element) {
        const views = (element.dataset.views || 'rgb lab').split(/\s+/);
        const state = {
            split: element.dataset.split === 'rgb' ? 'rgb' : 'lab',
            k: Number(element.dataset.k || 6),
            explode: 0.25, cut: true, yaw: -Math.PI / 4, pitch: 0.5,
            color: 0x44a8e0,   // the sky above the beach huts, 68, 168, 224
            keep: false,
        };
        const lock = 'lock' in element.dataset, plain = 'plain' in element.dataset;
        element.innerHTML = `
            <div class='cc-controls'>
                <div class='btn-group btn-group-sm cc-split' role='group' aria-label='Einteilung' ${lock ? 'hidden' : ''}>
                    <button type='button' class='btn btn-outline-secondary' data-split='rgb'>Würfel in RGB</button>
                    <button type='button' class='btn btn-outline-secondary' data-split='lab'>Quader in Lab</button>
                </div>
                <label class='cc-slider'>Einteilung <input type='range' min='2' max='10' step='1' data-k aria-label='Wie fein geteilt wird'> <span class='cc-k'></span></label>
                <label class='cc-slider'>Auseinander <input type='range' min='0.03' max='0.9' step='0.01' data-explode aria-label='Wie weit die Klassen auseinandergezogen sind'></label>
                <label class='cc-check'><input type='checkbox' data-cut> Ein Viertel herausnehmen</label>
            </div>
            <div class='cc-views'>${views.map((space) => `
                <figure class='cc-view' data-space='${space}'>
                    <div class='cc-stage'><canvas tabindex='0' aria-label='${space === 'rgb' ? 'Der RGB-Würfel' : 'Die Farben des RGB-Würfels in CIELab'}, in Klassen geteilt. Ziehen dreht ihn, ein Klick wählt eine Farbe.'></canvas><div class='cc-labels'></div></div>
                    <figcaption>${space === 'rgb' ? 'Im RGB-Würfel' : 'Im Lab-Raum'}</figcaption>
                </figure>`).join('')}
            </div>
            <div class='cc-say' aria-live='polite'></div>
            <div class='cc-swatches' ${plain ? 'hidden' : ''}>
                <figure><canvas class='cc-swatch' data-swatch='rgb' width='128' height='32'></canvas><figcaption data-caption='rgb'></figcaption></figure>
                <figure><canvas class='cc-swatch' data-swatch='lab' width='128' height='32'></canvas><figcaption data-caption='lab'></figcaption></figure>
            </div>`;
        const q = (selector) => element.querySelector(selector);
        const kInput = q('[data-k]'), explodeInput = q('[data-explode]'), cutInput = q('[data-cut]');
        kInput.value = state.k;
        explodeInput.value = state.explode;
        cutInput.checked = state.cut;

        const panes = [...element.querySelectorAll('.cc-view')].map((figure) => {
            const canvas = figure.querySelector('canvas');
            return { space: figure.dataset.space, figure, canvas, labels: figure.querySelector('.cc-labels'), renderer: makeRenderer(canvas), radius: 1 };
        });
        if (panes.some((p) => !p.renderer)) {
            q('.cc-views').innerHTML = `<p class='cc-say'>Dein Browser zeigt hier kein WebGL. Ohne WebGL lässt sich der Würfel leider nicht zeichnen.</p>`;
            return;
        }

        const cellOf = (color) => {
            if (state.split === 'rgb') return boxCell(color, state.k);
            const [L, a, b] = lab(color);
            return labCell(L, a, b, labLevels(state.k).n);
        };
        const currentKeep = () => state.keep ? cellOf(state.color) : -1;
        function rebuildMeshes() {
            const classes = geometry(state.split, state.k), keep = currentKeep();
            for (const pane of panes) {
                pane.renderer.upload('classes', buildMesh(classes, pane.space, { cut: state.cut, keep }));
                if (!pane.shell) pane.renderer.upload('shell', pane.shell = buildMesh(shell(), pane.space));
            }
        }

        function resize() {
            for (const pane of panes) {
                const rect = pane.canvas.getBoundingClientRect();
                const ratio = Math.min(2, window.devicePixelRatio || 1);
                pane.canvas.width = Math.max(1, Math.round(rect.width * ratio));
                pane.canvas.height = Math.max(1, Math.round(rect.height * ratio));
            }
        }

        function draw() {
            for (const pane of panes) {
                const projection = pane.renderer.draw({ yaw: state.yaw, pitch: state.pitch, explode: state.explode, keep: currentKeep() });
                if (!projection) continue;
                // the labels, at the corners (pulled out a little with the classes)
                const { rot, scale } = projection;
                const w = pane.canvas.clientWidth, h = pane.canvas.clientHeight;
                const together = state.keep;
                pane.labels.innerHTML = LABELS[pane.space].map(([text, p]) => {
                    const grow = 1 + (together ? 0 : state.explode * 0.85) + 0.06;
                    const world = p.map((x) => x * grow);
                    const v = [0, 1, 2].map((r) => rot[r] * world[0] + rot[r + 3] * world[1] + rot[r + 6] * world[2]);
                    // a corner of the cube gets a label if one of its faces looks this way
                    // and it was not taken out
                    const seen = [0, 1, 2].some((i) => rot[i * 3 + 2] * Math.sign(p[i]) > 0);
                    if (pane.space === 'rgb' && (!seen || (state.cut && !together && p[0] > 0 && p[2] > 0))) return '';
                    const x = (v[0] * scale[0] + 1) / 2 * w, y = (1 - v[1] * scale[1]) / 2 * h;
                    return `<span style='left: ${x.toFixed(1)}px; top: ${y.toFixed(1)}px'>${text}</span>`;
                }).join('');
            }
        }

        function describe() {
            const k = state.k;
            // Lab only where it is shown
            const { n, filled, size } = state.split === 'lab' || !plain ? labLevels(k) : { n: [] };
            const cubes = k ** 3;
            q('.cc-k').textContent = state.split === 'rgb' ? `${k} × ${k} × ${k}` : n.join(' × ');
            const per = 16777216 / cubes;
            const rgbLine = `${number(cubes)} Würfel in RGB, ${k} × ${k} × ${k}, mit je etwa ${number(per)} Farben`;
            const labLine = `${number(n[0] * n[1] * n[2])} Quader in Lab, ${n.join(' × ')}, davon ${number(filled)} mit Farben. Viele am Rand haben nur wenige; im Schnitt liegt eine Farbe in einem Quader mit etwa ${number(size)} Farben, so viele wie in einem Würfel in RGB`;
            const hint = state.keep ? 'Die Klasse der gewählten Farbe steht an ihrem Platz, der Rest ist Glas; ein Klick zeigt wieder alle Klassen.' : `Ziehen dreht ${panes.length > 1 ? 'beide Ansichten' : 'den Würfel'}, ein Klick wählt eine Farbe und hebt ihre Klasse hervor.`;
            q('.cc-say').textContent = `${state.split === 'rgb' ? rgbLine : labLine}. ${hint}`;
            element.querySelectorAll('[data-split]').forEach((b) => {
                const on = b.dataset.split === state.split;
                b.classList.toggle('active', on);
                b.setAttribute('aria-pressed', on);
            });
            if (plain) return;
            // 300 colours of each class are plenty to measure them
            const box = boxSwatch(state.color, k, 300), labBox = labSwatch(state.color, n, 300);
            // one colour per pixel of the screen, as in a photo
            for (const [name, make] of [['rgb', (count) => boxSwatch(state.color, k, count)], ['lab', (count) => labSwatch(state.color, n, count)]]) {
                const canvas = q(`[data-swatch='${name}']`);
                const width = Math.max(1, Math.round(canvas.clientWidth * (window.devicePixelRatio || 1)));
                const height = Math.max(1, Math.round(canvas.clientHeight * (window.devicePixelRatio || 1)));
                canvas.width = width;
                canvas.height = height;
                // the colour itself on the left, for comparison
                const flat = Math.round(width / 5);
                const colors = make(width * height).colors;
                const context = canvas.getContext('2d');
                const picture = context.createImageData(width, height);
                colors.forEach((c, i) => {
                    const pixel = i % width < flat ? state.color : c;
                    picture.data.set([pixel >> 16, (pixel >> 8) & 255, pixel & 255, 255], i * 4);
                });
                context.putImageData(picture, 0, 0);
            }
            const channel = ([low, high], name) => `${name} ${low}–${high}`;
            const apart = (colors) => {
                const [all, light] = meanDistance(colors);
                return `zwei davon im Mittel ${decimal(all)} Lab-Einheiten auseinander, in der Helligkeit ${decimal(light)}`;
            };
            q("[data-caption='rgb']").textContent = `Ihr Würfel in RGB: ${box.ranges.map((r, i) => channel(r, ['Rot', 'Grün', 'Blau'][i])).join(', ')}; ${number(box.size)} Farben, ${apart(box.colors)}`;
            const span = ([low, high], name) => `${name} ${decimal(low)} bis ${decimal(high)}`;
            q("[data-caption='lab']").textContent = `Ihr Quader in Lab: ${labBox.ranges.map((r, i) => span(r, ['L', 'a', 'b'][i])).join(', ')}; etwa ${number(labBox.size)} Farben, ${apart(labBox.colors)}`;
            q('.cc-swatches').style.setProperty('--cc-picked', hex(state.color));
            q('.cc-swatches').dataset.picked = triple(state.color);
        }

        let pending = null;
        function update(what) {
            pending = pending ? new Set([...pending, ...what]) : new Set(what);
            requestAnimationFrame(() => {
                if (!pending) return;
                const todo = pending;
                pending = null;
                                if (todo.has('mesh')) rebuildMeshes();
                if (todo.has('size')) resize();
                describe();
                draw();
            });
        }

        element.querySelectorAll('[data-split]').forEach((b) => b.addEventListener('click', () => {
            state.split = b.dataset.split;
            update(['mesh']);
        }));
        kInput.addEventListener('input', () => { state.k = Number(kInput.value); update(['mesh']); });
        explodeInput.addEventListener('input', () => { state.explode = Number(explodeInput.value); update([]); });
        cutInput.addEventListener('change', () => { state.cut = cutInput.checked; update(['mesh']); });

        for (const pane of panes) {
            let drag = null;
            pane.canvas.addEventListener('pointerdown', (e) => {
                drag = { x: e.clientX, y: e.clientY, moved: 0 };
                pane.canvas.setPointerCapture(e.pointerId);
            });
            pane.canvas.addEventListener('pointermove', (e) => {
                if (!drag) return;
                const dx = e.clientX - drag.x, dy = e.clientY - drag.y;
                drag.moved += Math.abs(dx) + Math.abs(dy);
                drag.x = e.clientX; drag.y = e.clientY;
                state.yaw += dx * 0.01;
                state.pitch = Math.max(-1.45, Math.min(1.45, state.pitch + dy * 0.01));
                draw();
            });
            pane.canvas.addEventListener('pointerup', (e) => {
                if (drag && drag.moved < 5 && state.keep) {
                    state.keep = false;
                    update(['mesh']);
                } else if (drag && drag.moved < 5) {
                    const rect = pane.canvas.getBoundingClientRect();
                    const ratio = pane.canvas.width / rect.width;
                    const color = pane.renderer.pick({ yaw: state.yaw, pitch: state.pitch, explode: state.explode, keep: currentKeep() },
                        (e.clientX - rect.left) * ratio, (e.clientY - rect.top) * ratio);
                    if (color >= 0) {
                        state.color = color;
                        state.keep = true;
                        update(['mesh']);
                    }
                }
                drag = null;
            });
            pane.canvas.addEventListener('keydown', (e) => {
                const turn = { ArrowLeft: [-0.12, 0], ArrowRight: [0.12, 0], ArrowUp: [0, -0.12], ArrowDown: [0, 0.12] }[e.key];
                if (!turn) return;
                e.preventDefault();
                state.yaw += turn[0];
                state.pitch = Math.max(-1.45, Math.min(1.45, state.pitch + turn[1]));
                draw();
            });
        }
        if (typeof ResizeObserver !== 'undefined') new ResizeObserver(() => update(['size'])).observe(q('.cc-views'));
        update(['mesh', 'size']);
    }

    const api = { lab, fromLab, inside, feltSize, meanDistance, boxCell, boxRange, labCell, labRange, labLevels, labCounts, cubeClasses, boxClasses, geometry, shell, buildMesh, boxSwatch, labSwatch, mount };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.ColorClasses = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.color-classes').forEach(mount);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
