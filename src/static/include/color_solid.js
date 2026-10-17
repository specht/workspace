// A colour space in 3D, for the Handbuch »Farbräume«: all colours of RGB
// as a solid in the coordinates of one colour space, drawn with WebGL and
// shaded by the normals of its faces. Drag turns it; a slider cuts it
// horizontally, so the inside shows as a flat section; a quarter can be
// taken out. Split into the 8 × 8 × 8 cubes of RGB and pulled apart, it
// shows how the space bends equal steps of RGB. Brighter is up in the solid,
// and at the start also on the screen.
//
// <div class='color-solid' data-space='lab'></div>
//   data-space  rgb, hsv, ycbcr, lab or oklab
//
// The solid is the six faces of the RGB cube, taken into the colour space
// point by point, so they bend as the space bends them. Sections and the
// walls of the missing quarter are flat grids, kept where they lie inside:
// where the way back to RGB gives values from 0 to 1.

(function (root) {
    'use strict';

    // --- The colour spaces ----------------------------------------------------
    //
    // Each takes r, g, b as stored (0 to 1) to [x, y, z] on the screen, y up,
    // and back. `back` gives three values that are from 0 to 1 exactly for
    // the colours of RGB; `color` the colour there, as stored.

    const toLinear = (v) => v <= 0.04045 ? v / 12.92 : ((v + 0.055) / 1.055) ** 2.4;
    const toStored = (c) => { c = Math.max(0, Math.min(1, c)); return c <= 0.0031308 ? 12.92 * c : 1.055 * c ** (1 / 2.4) - 0.055; };
    const clamp = (c) => Math.max(0, Math.min(1, c));
    const turn = 2 * Math.PI;

    function hsv(r, g, b) {
        const max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
        let h = 0;
        if (d > 0) h = max === r ? ((g - b) / d) % 6 : max === g ? (b - r) / d + 2 : (r - g) / d + 4;
        return [((h / 6) % 1 + 1) % 1, max > 0 ? d / max : 0, max];
    }
    // also beyond S = 1 and V = 1: then a channel leaves 0 to 1
    function fromHsv(h, s, v) {
        const c = v * s, x = c * (1 - Math.abs((h * 6) % 2 - 1)), m = v - c;
        const [r, g, b] = [[c, x, 0], [x, c, 0], [0, c, x], [0, x, c], [x, 0, c], [c, 0, x]][Math.floor(h * 6) % 6];
        return [r + m, g + m, b + m];
    }

    const f = (t) => t > 216 / 24389 ? Math.cbrt(t) : (24389 / 27 * t + 16) / 116;
    const finv = (t) => t ** 3 > 216 / 24389 ? t ** 3 : (116 * t - 16) / (24389 / 27);

    const S2 = Math.SQRT2, S3 = Math.sqrt(3), S6 = Math.sqrt(6);
    const corner = (name, r, g, b) => [name, [r, g, b]];
    const CORNERS = [corner('Schwarz', 0, 0, 0), corner('Weiß', 1, 1, 1), corner('Rot', 1, 0, 0), corner('Grün', 0, 1, 0),
        corner('Blau', 0, 0, 1), corner('Gelb', 1, 1, 0), corner('Cyan', 0, 1, 1), corner('Magenta', 1, 0, 1)];

    const SPACES = {
        // the cube on its black corner: the grey diagonal is the vertical axis
        rgb: {
            name: 'RGB-Würfel',
            forward: (r, g, b) => {
                const u = r - 0.5, v = g - 0.5, w = b - 0.5;
                return [(u - v) / S2, (u + v + w) / S3, (u + v - 2 * w) / S6];
            },
            back: (x, y, z) => {
                const u = x / S2 + y / S3 + z / S6, v = -x / S2 + y / S3 + z / S6, w = y / S3 - 2 * z / S6;
                return [u + 0.5, v + 0.5, w + 0.5];
            },
            color: (c) => c.map(clamp),
            height: (y) => `(R + G + B) / 3 = ${Math.round((y / S3 + 0.5) * 255)}`,
            labels: CORNERS,
        },
        // a cone: V up, S times V as the distance from the axis, H around it
        hsv: {
            name: 'HSV-Kegel',
            forward: (r, g, b) => {
                const [h, s, v] = hsv(r, g, b);
                return [s * v * 0.5 * Math.cos(h * turn), v - 0.5, -s * v * 0.5 * Math.sin(h * turn)];
            },
            back: (x, y, z) => {
                const v = y + 0.5, radius = Math.hypot(x, z);
                if (v <= 0) return radius > 0 ? [-1, -1, -1] : [v, v, v];
                const h = ((Math.atan2(-z, x) / turn) % 1 + 1) % 1;
                return fromHsv(h, 2 * radius / v, v);
            },
            color: (c) => c.map(clamp),
            height: (y) => `V = ${decimal(y + 0.5, 2)}`,
            labels: CORNERS,
        },
        // Y up, Cr to the right, Cb to the front
        ycbcr: {
            name: 'YCbCr',
            forward: (r, g, b) => [0.5 * r - 0.418688 * g - 0.081312 * b, 0.299 * r + 0.587 * g + 0.114 * b - 0.5, -0.168736 * r - 0.331264 * g + 0.5 * b],
            back: (x, y, z) => {
                const Y = y + 0.5;
                return [Y + 1.402 * x, Y - 0.344136 * z - 0.714136 * x, Y + 1.772 * z];
            },
            color: (c) => c.map(clamp),
            height: (y) => `Y = ${Math.round((y + 0.5) * 255)}`,
            labels: CORNERS,
        },
        // L up, a to the right, b to the back (blue in front), in Lab units / 100
        lab: {
            name: 'CIELab',
            forward: (r, g, b) => {
                const lr = toLinear(r), lg = toLinear(g), lb = toLinear(b);
                const x = (0.4124 * lr + 0.3576 * lg + 0.1805 * lb) / 0.95047;
                const y = 0.2126 * lr + 0.7152 * lg + 0.0722 * lb;
                const z = (0.0193 * lr + 0.1192 * lg + 0.9505 * lb) / 1.08883;
                const fx = f(x), fy = f(y), fz = f(z);
                return [5 * (fx - fy), (116 * fy - 16) / 100 - 0.5, -2 * (fy - fz)];
            },
            back: (x, y, z) => {
                const L = (y + 0.5) * 100, a = x * 100, b = -z * 100;
                const fy = (L + 16) / 116;
                const X = finv(fy + a / 500) * 0.95047, Y = finv(fy), Z = finv(fy - b / 200) * 1.08883;
                return [3.2406 * X - 1.5372 * Y - 0.4986 * Z, -0.9689 * X + 1.8758 * Y + 0.0415 * Z, 0.0557 * X - 0.2040 * Y + 1.0570 * Z];
            },
            color: (c) => c.map(toStored),
            height: (y) => `L = ${Math.round((y + 0.5) * 100)}`,
            labels: [['L = 100', [0, 0.5, 0]], ['L = 0', [0, -0.5, 0]], ['+a, rot', [0.98, 0, 0]], ['−a, grün', [-0.86, 0, 0]],
                ['+b, gelb', [0, 0, -0.95]], ['−b, blau', [0, 0, 1.08]]].map(([text, p]) => [text, null, p]),
        },
        // the same for OKLab, in its own units: L from 0 to 1
        oklab: {
            name: 'OKLab',
            forward: (r, g, b) => {
                const lr = toLinear(r), lg = toLinear(g), lb = toLinear(b);
                const l = Math.cbrt(0.4122214708 * lr + 0.5363325363 * lg + 0.0514459929 * lb);
                const m = Math.cbrt(0.2119034982 * lr + 0.6806995451 * lg + 0.1073969566 * lb);
                const s = Math.cbrt(0.0883024619 * lr + 0.2817188376 * lg + 0.6299787005 * lb);
                return [1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
                    0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s - 0.5,
                    -(0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s)];
            },
            back: (x, y, z) => {
                const L = y + 0.5, a = x, b = -z;
                const l = (L + 0.3963377774 * a + 0.2158037573 * b) ** 3;
                const m = (L - 0.1055613458 * a - 0.0638541728 * b) ** 3;
                const s = (L - 0.0894841775 * a - 1.2914855480 * b) ** 3;
                return [4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
                    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
                    -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s];
            },
            color: (c) => c.map(toStored),
            height: (y) => `L = ${decimal(y + 0.5, 2)}`,
            labels: [['L = 1', [0, 0.5, 0]], ['L = 0', [0, -0.5, 0]], ['+a, rot', [0.28, 0, 0]], ['−a, grün', [-0.24, 0, 0]],
                ['+b, gelb', [0, 0, -0.2]], ['−b, blau', [0, 0, 0.32]]].map(([text, p]) => [text, null, p]),
        },
    };

    const decimal = (x, digits) => x.toLocaleString('de-DE', { minimumFractionDigits: digits, maximumFractionDigits: digits });
    // how far inside RGB a point of the space lies: below 0 it is outside
    const inside = (space, x, y, z) => {
        const c = space.back(x, y, z);
        return Math.min(c[0], 1 - c[0], c[1], 1 - c[1], c[2], 1 - c[2]);
    };

    // --- Geometry ---------------------------------------------------------------
    //
    // A point is [x, y, z, r, g, b]: where it is on the screen and its colour.

    // The part of a polygon where `values` (one per corner) is at least 0
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

    // A grid of m × m squares, each as two triangles; at(u, v) for u, v from 0 to 1
    function grid(at, m) {
        const out = [];
        for (let i = 0; i < m; i++) for (let j = 0; j < m; j++) {
            const p = at(i / m, j / m), q = at((i + 1) / m, j / m), r = at((i + 1) / m, (j + 1) / m), s = at(i / m, (j + 1) / m);
            out.push([p, q, r], [p, r, s]);
        }
        return out;
    }

    // The faces of the RGB cube in the space, once per space
    const surfaceCache = new Map();
    function surface(name) {
        if (surfaceCache.has(name)) return surfaceCache.get(name);
        const space = SPACES[name], out = [];
        for (let axis = 0; axis < 3; axis++) {
            const u = (axis + 1) % 3, w = (axis + 2) % 3;
            for (const side of [0, 1]) {
                out.push(...grid((s, t) => {
                    const c = [0, 0, 0];
                    c[axis] = side; c[u] = s; c[w] = t;
                    return [...space.forward(c[0], c[1], c[2]), c[0], c[1], c[2]];
                }, 40));
            }
        }
        // how far the solid reaches, sideways and up and down, and from the
        // centre in any direction: turned any way, it stays in that sphere
        let reach = 0, low = Infinity, high = -Infinity, sphere = 0;
        for (const triangle of out) for (const p of triangle) {
            reach = Math.max(reach, Math.abs(p[0]), Math.abs(p[2]));
            low = Math.min(low, p[1]);
            high = Math.max(high, p[1]);
            sphere = Math.max(sphere, Math.hypot(p[0], p[1], p[2]));
        }
        const result = { triangles: out, reach, low, high, sphere };
        surfaceCache.set(name, result);
        return result;
    }

    // A flat piece of the space, kept where it lies inside and below `top`;
    // at(u, v) gives [x, y, z]
    function flat(space, at, m, top) {
        const out = [];
        for (const triangle of grid((u, v) => {
            const [x, y, z] = at(u, v);
            return [x, y, z, inside(space, x, y, z), Math.min(top, 10) - y];
        }, m)) {
            let part = clip(triangle, triangle.map((p) => p[3]));
            if (part.length >= 3) part = clip(part, part.map((p) => p[4]));
            if (part.length >= 3) out.push(part.map(([x, y, z]) => [x, y, z, ...space.color(space.back(x, y, z))]));
        }
        return out;
    }

    // Leave out the quarter in front, x > 0 and z > 0
    function withoutQuarter(polygons) {
        const out = [];
        for (const polygon of polygons) {
            for (const left of [clip(polygon, polygon.map((p) => -p[0])), clip(polygon, polygon.map((p) => p[0]))]) {
                if (left.length < 3) continue;
                if (left.reduce((s, p) => s + p[0], 0) <= 0) { out.push(left); continue; }
                const back = clip(left, left.map((p) => -p[2]));
                if (back.length >= 3) out.push(back);
            }
        }
        return out;
    }

    // The 8 × 8 × 8 cubes of RGB in the space, each with its centroid, once per space
    const cubeCache = new Map();
    function cubes(name) {
        if (cubeCache.has(name)) return cubeCache.get(name);
        const space = SPACES[name], out = [];
        for (let i = 0; i < 8; i++) for (let j = 0; j < 8; j++) for (let l = 0; l < 8; l++) {
            const low = [i / 8, j / 8, l / 8], polygons = [];
            for (let axis = 0; axis < 3; axis++) {
                const u = (axis + 1) % 3, w = (axis + 2) % 3;
                for (const side of [0, 1]) {
                    polygons.push(...grid((s, t) => {
                        const c = [...low];
                        c[axis] += side / 8; c[u] += s / 8; c[w] += t / 8;
                        return [...space.forward(c[0], c[1], c[2]), c[0], c[1], c[2]];
                    }, 4));
                }
            }
            const centre = space.forward(low[0] + 1 / 16, low[1] + 1 / 16, low[2] + 1 / 16);
            out.push({ polygons, centre });
        }
        cubeCache.set(name, out);
        return out;
    }

    // All triangles of the solid, cut at the height `top`, perhaps without a
    // quarter; split, the cubes whose centre lies below `top` and outside the quarter
    function solid(name, options = {}) {
        const space = SPACES[name], whole = surface(name);
        const top = options.top ?? Infinity, cut = !!options.cut;
        if (options.split) {
            const groups = cubes(name).filter(({ centre: [x, y, z] }) => y <= top && !(cut && x > 0 && z > 0))
                .map(({ polygons, centre }) => ({ polygons, shift: centre }));
            return pack(groups);
        }
        let polygons = [];
        for (const triangle of whole.triangles) {
            const part = top === Infinity ? triangle : clip(triangle, triangle.map((p) => top - p[1]));
            if (part.length >= 3) polygons.push(part);
        }
        const r = whole.reach * 1.05, high = Math.min(top, whole.high);
        // the section at the top
        if (top < whole.high) polygons.push(...flat(space, (u, v) => [-r + 2 * r * u, top, -r + 2 * r * v], 64, Infinity));
        if (cut) {
            polygons = withoutQuarter(polygons);
            // the two walls of the missing quarter
            const height = (v) => whole.low + (high - whole.low) * v;
            polygons.push(...flat(space, (u, v) => [0, height(v), r * u], 48, top + 1e-9));
            polygons.push(...flat(space, (u, v) => [r * u, height(v), 0], 48, top + 1e-9));
        }
        return pack([{ polygons, shift: [0, 0, 0] }]);
    }

    // Triangles as floats: for every corner x, y, z, its colour, the normal of
    // its triangle and the shift of its group when pulled apart
    function pack(groups) {
        const triangles = [];
        for (const { polygons, shift } of groups) {
            for (const polygon of polygons) {
                for (let i = 1; i + 1 < polygon.length; i++) triangles.push([polygon[0], polygon[i], polygon[i + 1], shift]);
            }
        }
        const count = triangles.length;
        const pos = new Float32Array(count * 9), normal = new Float32Array(count * 9), shifts = new Float32Array(count * 9), rgba = new Uint8Array(count * 12);
        let radius = 0, reach = 0;
        triangles.forEach((t, q) => {
            const u = [0, 1, 2].map((x) => t[1][x] - t[0][x]), w = [0, 1, 2].map((x) => t[2][x] - t[0][x]);
            const n = [u[1] * w[2] - u[2] * w[1], u[2] * w[0] - u[0] * w[2], u[0] * w[1] - u[1] * w[0]];
            const length = Math.hypot(...n) || 1;
            reach = Math.max(reach, Math.hypot(...t[3]));
            for (let m = 0; m < 3; m++) {
                const p = t[m], v = q * 3 + m;
                pos.set([p[0], p[1], p[2]], v * 3);
                normal.set(n.map((x) => x / length), v * 3);
                shifts.set(t[3], v * 3);
                rgba.set([p[3], p[4], p[5]].map((c) => Math.round(clamp(c) * 255)).concat(255), v * 4);
                radius = Math.max(radius, Math.hypot(p[0], p[1], p[2]));
            }
        });
        return { triangles: count, pos, normal, shift: shifts, rgba, radius, reach };
    }

    // --- WebGL ----------------------------------------------------------------

    const VERTEX = `
        attribute vec3 aPos;
        attribute vec3 aNormal;
        attribute vec3 aShift;
        attribute vec4 aColor;
        uniform mat3 uRot;
        uniform vec3 uScale;
        uniform float uExplode;
        varying vec4 vColor;
        void main() {
            vec3 p = uRot * (aPos + aShift * uExplode);
            gl_Position = vec4(p.x * uScale.x, p.y * uScale.y, -p.z * uScale.z, 1.0);
            // lit from both sides: a face seen from behind turns its normal around
            vec3 n = uRot * aNormal;
            if (n.z < 0.0) n = -n;
            vec3 light = normalize(vec3(-0.45, 0.75, 0.55));
            float shade = 0.42 + 0.48 * max(dot(n, light), 0.0) + 0.16 * max(n.z, 0.0);
            vColor = vec4(aColor.rgb * shade, 1.0);
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
        const uni = (name) => gl.getUniformLocation(program, name);
        const LAYOUT = [['pos', 'aPos', 3, gl.FLOAT, false], ['normal', 'aNormal', 3, gl.FLOAT, false], ['shift', 'aShift', 3, gl.FLOAT, false],
            ['rgba', 'aColor', 4, gl.UNSIGNED_BYTE, true]];
        const buffers = {};
        let mesh = null;

        function upload(m) {
            mesh = m;
            for (const [key, attribute, size, type, normalized] of LAYOUT) {
                const buffer = buffers[key] || (buffers[key] = gl.createBuffer());
                gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
                gl.bufferData(gl.ARRAY_BUFFER, m[key], gl.STATIC_DRAW);
                const location = gl.getAttribLocation(program, attribute);
                gl.enableVertexAttribArray(location);
                gl.vertexAttribPointer(location, size, type, normalized, 0, 0);
            }
        }

        // view: the turn (rot), how far apart (explode) and the radius to fit in
        function draw(view) {
            gl.viewport(0, 0, canvas.width, canvas.height);
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
            const rot = view.rot;
            const s = 0.9 / view.radius, aspect = canvas.width / canvas.height;
            const scale = aspect > 1 ? [s / aspect, s, s * 0.5] : [s, s * aspect, s * 0.5];
            if (mesh) {
                gl.uniformMatrix3fv(uni('uRot'), false, new Float32Array(rot));
                gl.uniform3fv(uni('uScale'), new Float32Array(scale));
                gl.uniform1f(uni('uExplode'), view.explode);
                gl.enable(gl.DEPTH_TEST);
                gl.drawArrays(gl.TRIANGLES, 0, mesh.triangles * 3);
            }
            return { rot, scale };
        }
        return { upload, draw };
    }

    // --- Turning -----------------------------------------------------------------
    //
    // A turn is a 3 × 3 matrix, column-major: its columns are where the axes
    // of the space point on the screen (x right, y up, z towards the viewer).

    // first about the vertical axis, then tilted towards the viewer
    function startTurn(yaw, pitch) {
        const cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
        return [cy, sp * sy, -cp * sy, 0, cp, sp, sy, -sp * cy, cp * cy];
    }

    // Turned further, about the screen's vertical axis by `right` (the front
    // goes right) and about its horizontal axis by `down` (the front goes down)
    function screenTurn(rot, right, down) {
        const cr = Math.cos(right), sr = Math.sin(right), cd = Math.cos(down), sd = Math.sin(down);
        const columns = [0, 3, 6].map((i) => {
            let [x, y, z] = rot.slice(i, i + 3);
            [x, z] = [x * cr + z * sr, -x * sr + z * cr];
            [y, z] = [y * cd - z * sd, y * sd + z * cd];
            return [x, y, z];
        });
        // straightened, so that rounding does not skew it over many turns
        const unit = (v) => { const l = Math.hypot(...v) || 1; return v.map((c) => c / l); };
        const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
        const a = unit(columns[0]);
        const b = unit(columns[1].map((c, k) => c - dot(columns[1], a) * a[k]));
        const c = [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
        return [...a, ...b, ...c];
    }

    // --- The widget -------------------------------------------------------------

    function mount(element) {
        const name = element.dataset.space in SPACES ? element.dataset.space : 'rgb';
        const space = SPACES[name], whole = surface(name);
        const state = { rot: startTurn(-Math.PI / 4, 0.45), cut: false, top: whole.high, split: false, explode: 0.3 };
        element.innerHTML = `
            <div class='cs-stage'>
                <canvas tabindex='0' aria-label='Die Farben des RGB-Würfels als Körper im Farbraum ${space.name}. Ziehen dreht ihn.'></canvas>
                <div class='cs-labels'></div>
                <div class='cs-hint' aria-hidden='true'><i class='bi bi-arrows-move'></i> Ziehen dreht</div>
            </div>
            <div class='cs-bar'>
                <label class='cs-field'>
                    <span class='cs-name'><i class='bi bi-layers-half'></i> Schnitt</span>
                    <input type='range' class='form-range' min='0' max='1000' value='1000' data-top>
                    <span class='cs-value cs-height'></span>
                </label>
                <div class='cs-toggles'>
                    <button type='button' class='cs-toggle' data-cut aria-pressed='false'><i class='bi bi-pie-chart'></i> Viertel herausnehmen</button>
                    <button type='button' class='cs-toggle' data-split aria-pressed='false'><i class='bi bi-boxes'></i> Zerteilen</button>
                </div>
                <label class='cs-field' data-apart hidden>
                    <span class='cs-name'><i class='bi bi-arrows-angle-expand'></i> Auseinander</span>
                    <input type='range' class='form-range' min='0' max='100' value='30' data-explode>
                    <span class='cs-value'></span>
                </label>
            </div>`;
        const canvas = element.querySelector('canvas'), labels = element.querySelector('.cs-labels');
        const topInput = element.querySelector('[data-top]'), cutInput = element.querySelector('[data-cut]');
        const splitInput = element.querySelector('[data-split]'), explodeInput = element.querySelector('[data-explode]');
        const renderer = makeRenderer(canvas);
        if (!renderer) {
            element.querySelector('.cs-stage').innerHTML = `<p>Dein Browser zeigt hier kein WebGL. Ohne WebGL lässt sich der Körper leider nicht zeichnen.</p>`;
            return;
        }
        // the corners of the RGB cube, or the ends of the axes, as labels
        const marks = space.labels.map(([text, rgb, p]) => [text, rgb ? space.forward(...rgb) : p]);
        // the whole sphere fits, so no turn pushes a part out of the picture
        const extent = whole.sphere * 1.05;

        function draw() {
            const explode = state.split ? state.explode : 0;
            const radius = extent * (1 + explode * 0.9);
            const { rot, scale } = renderer.draw({ rot: state.rot, explode, radius });
            const w = canvas.clientWidth, h = canvas.clientHeight;
            labels.innerHTML = marks.map(([text, p]) => {
                // none for what was cut away
                if (p[1] > state.top + 0.02 || (state.cut && p[0] > 0.01 && p[2] > 0.01)) return '';
                const out = p.map((x, i) => x * (1 + explode * 0.9) * (i === 1 ? 1.08 : 1.15));
                const v = [0, 1, 2].map((r) => rot[r] * out[0] + rot[r + 3] * out[1] + rot[r + 6] * out[2]);
                // a label at the back of the solid hides behind it
                if (v[2] < -0.12 * radius) return '';
                const x = (v[0] * scale[0] + 1) / 2 * w, y = (1 - v[1] * scale[1]) / 2 * h;
                return `<span style='left: ${x.toFixed(1)}px; top: ${y.toFixed(1)}px'>${text}</span>`;
            }).join('');
            element.querySelector('.cs-height').textContent = state.top >= whole.high ? 'nicht geschnitten' : `bis ${space.height(state.top)}`;
            element.querySelector('[data-apart] .cs-value').textContent = state.explode ? `${Math.round(state.explode * 100)} %` : 'zusammen';
        }

        let pending = null;
        function update(rebuild) {
            if (pending) { pending.rebuild ||= rebuild; return; }
            pending = { rebuild };
            requestAnimationFrame(() => {
                const job = pending;
                pending = null;
                const rect = canvas.getBoundingClientRect(), ratio = Math.min(2, window.devicePixelRatio || 1);
                canvas.width = Math.max(1, Math.round(rect.width * ratio));
                canvas.height = Math.max(1, Math.round(rect.height * ratio));
                if (job.rebuild) renderer.upload(solid(name, { top: state.top >= whole.high ? Infinity : state.top, cut: state.cut, split: state.split }));
                draw();
            });
        }

        topInput.addEventListener('input', () => {
            state.top = whole.low + (whole.high - whole.low) * Math.max(0.02, Number(topInput.value) / 1000);
            update(true);
        });
        const toggle = (button, on) => button.setAttribute('aria-pressed', on);
        cutInput.addEventListener('click', () => { state.cut = !state.cut; toggle(cutInput, state.cut); update(true); });
        splitInput.addEventListener('click', () => {
            state.split = !state.split;
            toggle(splitInput, state.split);
            element.querySelector('[data-apart]').hidden = !state.split;
            update(true);
        });
        explodeInput.addEventListener('input', () => { state.explode = Number(explodeInput.value) / 100; update(false); });
        // Dragging turns it about the axes of the screen, so that what is in
        // front follows the pointer however the solid lies: an angle per pixel
        // of the sphere's radius on the screen. Let go while moving, it turns
        // on and slows down, unless the reader wants less motion.
        const turnBy = (right, down) => {
            state.rot = screenTurn(state.rot, right, down);
            draw();
        };
        const perPixel = () => 1 / Math.max(1, 0.45 * Math.min(canvas.clientWidth, canvas.clientHeight));
        const still = window.matchMedia?.('(prefers-reduced-motion: reduce)');
        let drag = null, spin = null;
        canvas.addEventListener('pointerdown', (e) => {
            if (spin) cancelAnimationFrame(spin);
            spin = null;
            drag = { x: e.clientX, y: e.clientY, t: e.timeStamp, vx: 0, vy: 0 };
            canvas.setPointerCapture(e.pointerId);
            element.classList.add('cs-turned');
        });
        canvas.addEventListener('pointermove', (e) => {
            if (!drag) return;
            const k = perPixel(), dx = (e.clientX - drag.x) * k, dy = (e.clientY - drag.y) * k;
            // the speed, in radians per millisecond, smoothed over the last moves
            const dt = Math.max(1, e.timeStamp - drag.t);
            drag.vx = 0.6 * dx / dt + 0.4 * drag.vx;
            drag.vy = 0.6 * dy / dt + 0.4 * drag.vy;
            Object.assign(drag, { x: e.clientX, y: e.clientY, t: e.timeStamp });
            turnBy(dx, dy);
        });
        const release = (e) => {
            if (!drag) return;
            let { vx, vy, t } = drag;
            drag = null;
            // held still before letting go: it stays where it is
            if (e.type !== 'pointerup' || still?.matches || e.timeStamp - t > 80 || Math.hypot(vx, vy) < 0.0003) return;
            let last = performance.now();
            const step = (now) => {
                const dt = Math.min(50, now - last);
                last = now;
                turnBy(vx * dt, vy * dt);
                const slow = Math.exp(-dt / 280);
                vx *= slow;
                vy *= slow;
                spin = Math.hypot(vx, vy) > 0.00003 ? requestAnimationFrame(step) : null;
            };
            spin = requestAnimationFrame(step);
        };
        canvas.addEventListener('pointerup', release);
        canvas.addEventListener('pointercancel', release);
        canvas.addEventListener('keydown', (e) => {
            const step = { ArrowLeft: [-0.12, 0], ArrowRight: [0.12, 0], ArrowUp: [0, -0.12], ArrowDown: [0, 0.12] }[e.key];
            if (!step) return;
            e.preventDefault();
            turnBy(step[0], step[1]);
        });
        if (typeof ResizeObserver !== 'undefined') new ResizeObserver(() => update(false)).observe(canvas);
        update(true);
    }

    const api = { SPACES, inside, surface, cubes, solid, startTurn, screenTurn, mount };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.ColorSolid = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.color-solid').forEach(mount);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
