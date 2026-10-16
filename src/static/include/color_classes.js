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

    // --- Voxels -------------------------------------------------------------
    //
    // Each view is a grid of small boxes, voxels, each with the colour at its
    // centre. The RGB view: the cube, x red, y blue (up), z green, from -0.5
    // to 0.5. The Lab view: x is a, y is L (up), z is -b, in Lab units / 100.

    function rgbVoxels(k) {
        const n = k * Math.ceil(36 / k);
        const count = n ** 3;
        const set = { dims: [n, n, n], half: [0.5 / n, 0.5 / n, 0.5 / n], count,
            pos: new Float32Array(count * 3), color: new Int32Array(count), lab: new Float32Array(count * 3), grid: new Int32Array(count) };
        let v = 0;
        for (let i = 0; i < n; i++) for (let j = 0; j < n; j++) for (let l = 0; l < n; l++) {
            const value = (x) => Math.floor((x + 0.5) * 256 / n);
            const c = (value(i) << 16) | (value(l) << 8) | value(j);
            set.pos.set([(i + 0.5) / n - 0.5, (j + 0.5) / n - 0.5, (l + 0.5) / n - 0.5], v * 3);
            set.color[v] = c;
            set.lab.set(lab(c), v * 3);
            set.grid[(i * n + j) * n + l] = v;
            v++;
        }
        return set;
    }

    function labVoxels(n) {
        const dims = [Math.ceil(50 / n[1]) * n[1], Math.ceil(40 / n[0]) * n[0], Math.ceil(50 / n[2]) * n[2]];
        const step = [(A_MAX - A_MIN) / dims[0], 100 / dims[1], (B_MAX - B_MIN) / dims[2]];
        const grid = new Int32Array(dims[0] * dims[1] * dims[2]).fill(-1);
        const pos = [], color = [], labs = [];
        for (let i = 0; i < dims[0]; i++) for (let j = 0; j < dims[1]; j++) for (let l = 0; l < dims[2]; l++) {
            // l runs against b, as z does
            const a = A_MIN + (i + 0.5) * step[0], L = (j + 0.5) * step[1], b = B_MAX - (l + 0.5) * step[2];
            const c = fromLab(L, a, b);
            if (c < 0) continue;
            grid[(i * dims[1] + j) * dims[2] + l] = color.length;
            pos.push(a / 100, (L - 50) / 100, -b / 100);
            color.push(c);
            labs.push(L, a, b);
        }
        return { dims, half: step.map((s) => s / 200), count: color.length,
            pos: new Float32Array(pos), color: new Int32Array(color), lab: new Float32Array(labs), grid };
    }

    // --- The mesh -------------------------------------------------------------
    //
    // A face of a voxel is drawn where the next voxel is missing or belongs to
    // another class, so every class is a closed solid. Pulled apart, each
    // class moves away from the centre by its centroid times the uniform
    // uExplode; every vertex carries the centroid of its class for that.

    const DIRS = [[1, 0, 0], [-1, 0, 0], [0, 1, 0], [0, -1, 0], [0, 0, 1], [0, 0, -1]];
    // the four corners of the face towards each direction, in units of the half size
    const CORNERS = DIRS.map((d) => {
        const axis = d.findIndex((x) => x !== 0), u = (axis + 1) % 3, w = (axis + 2) % 3;
        return [[-1, -1], [1, -1], [1, 1], [-1, 1]].map(([p, q]) => {
            const c = [0, 0, 0];
            c[axis] = d[axis]; c[u] = p; c[w] = q;
            return c;
        });
    });

    function buildMesh(set, cells, options = {}) {
        const { cut = false, keep = -1 } = options;
        const [nx, ny, nz] = set.dims;
        // the centroid of every class
        const sums = new Map();
        for (let v = 0; v < set.count; v++) {
            let s = sums.get(cells[v]);
            if (!s) sums.set(cells[v], s = [0, 0, 0, 0]);
            s[0] += set.pos[v * 3]; s[1] += set.pos[v * 3 + 1]; s[2] += set.pos[v * 3 + 2]; s[3]++;
        }
        const centroid = new Map();
        for (const [cell, s] of sums) centroid.set(cell, [s[0] / s[3], s[1] / s[3], s[2] / s[3]]);
        // the quarter in front, towards +x and +z, comes out (all but the kept class)
        const shown = (cell) => cell === keep || !cut || !(centroid.get(cell)[0] > 0 && centroid.get(cell)[2] > 0);

        const voxelAt = (i, j, l) => i < 0 || j < 0 || l < 0 || i >= nx || j >= ny || l >= nz ? -1 : set.grid[(i * ny + j) * nz + l];
        const faces = [];
        for (let i = 0; i < nx; i++) for (let j = 0; j < ny; j++) for (let l = 0; l < nz; l++) {
            const v = voxelAt(i, j, l);
            if (v < 0 || !shown(cells[v])) continue;
            DIRS.forEach((d, dir) => {
                const w = voxelAt(i + d[0], j + d[1], l + d[2]);
                if (w < 0 || cells[w] !== cells[v] || !shown(cells[w])) faces.push(v, dir);
            });
        }

        const total = faces.length / 2;
        const pos = new Float32Array(total * 12), shift = new Float32Array(total * 12), rgba = new Uint8Array(total * 16), info = new Float32Array(total * 8);
        const index = new Uint32Array(total * 6);
        let radius = 0, reach = 0;
        for (let q = 0; q < total; q++) {
            const v = faces[q * 2], dir = faces[q * 2 + 1], cell = cells[v], c = centroid.get(cell), color = set.color[v];
            CORNERS[dir].forEach((corner, m) => {
                const at = q * 4 + m;
                let r2 = 0;
                for (let x = 0; x < 3; x++) {
                    const p = set.pos[v * 3 + x] + corner[x] * set.half[x];
                    pos[at * 3 + x] = p;
                    shift[at * 3 + x] = c[x];
                    r2 += p * p;
                }
                radius = Math.max(radius, r2);
                reach = Math.max(reach, c[0] ** 2 + c[1] ** 2 + c[2] ** 2);
                rgba.set([color >> 16, (color >> 8) & 255, color & 255, dir], at * 4);
                info[at * 2] = cell;
                info[at * 2 + 1] = v;
            });
            index.set([q * 4, q * 4 + 1, q * 4 + 2, q * 4, q * 4 + 2, q * 4 + 3], q * 6);
        }
        // pulled apart by e, nothing is further out than radius + e * reach
        return { faces: total, pos, shift, rgba, info, index, radius: Math.sqrt(radius), reach: Math.sqrt(reach) };
    }

    // The outside of the whole solid, for the glass around a chosen class
    function buildShell(set) {
        const all = new Int32Array(set.count).fill(-7);   // one class no real class number matches
        return buildMesh(set, all);
    }

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
        attribute vec4 aColor;
        attribute vec2 aInfo;
        uniform mat3 uRot;
        uniform vec3 uScale;
        uniform vec3 uNormals[6];
        uniform float uKeep;
        uniform float uExplode;
        uniform int uMode;     // 0 all, 1 only the kept class, 2 all but it (glass), 3 picking
        varying vec4 vColor;
        void main() {
            bool kept = abs(aInfo.x - uKeep) < 0.5;
            if ((uMode == 1 && !kept) || (uMode == 2 && kept)) { gl_Position = vec4(2.0, 2.0, 2.0, 1.0); return; }
            vec3 p = uRot * (aPos + aShift * uExplode);
            gl_Position = vec4(p.x * uScale.x, p.y * uScale.y, -p.z * uScale.z, 1.0);
            if (uMode == 3) {
                float id = aInfo.y + 1.0;
                vColor = vec4(mod(id, 256.0), mod(floor(id / 256.0), 256.0), floor(id / 65536.0), 255.0) / 255.0;
                return;
            }
            vec3 n = uRot * uNormals[int(aColor.a * 255.0 + 0.5)];
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
        if (!gl || !gl.getExtension('OES_element_index_uint')) return null;
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
        gl.uniform3fv(uni('uNormals'), new Float32Array(DIRS.flat()));
        const meshes = {};
        let mesh = null;
        const pickTarget = { fb: null, w: 0, h: 0 };

        const LAYOUT = [['pos', 'aPos', 3, gl.FLOAT, false], ['shift', 'aShift', 3, gl.FLOAT, false],
            ['rgba', 'aColor', 4, gl.UNSIGNED_BYTE, true], ['info', 'aInfo', 2, gl.FLOAT, false]];
        // name: 'classes' or 'shell'
        function upload(name, m) {
            const entry = meshes[name] || (meshes[name] = { buffers: {} });
            entry.mesh = m;
            for (const [key] of LAYOUT.concat([['index']])) {
                const buffer = entry.buffers[key] || (entry.buffers[key] = gl.createBuffer());
                const target = key === 'index' ? gl.ELEMENT_ARRAY_BUFFER : gl.ARRAY_BUFFER;
                gl.bindBuffer(target, buffer);
                gl.bufferData(target, m[key], gl.STATIC_DRAW);
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
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, entry.buffers.index);
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
            gl.drawElements(gl.TRIANGLES, mesh.faces * 6, gl.UNSIGNED_INT, 0);
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

        // The voxel under a point of the canvas (in canvas pixels), or -1
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
            return pixel[0] + pixel[1] * 256 + pixel[2] * 65536 - 1;
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
                <label class='cc-slider'>Einteilung <input type='range' min='2' max='12' step='1' data-k aria-label='Wie fein geteilt wird'> <span class='cc-k'></span></label>
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

        const cellOf = (color, labValue) => {
            if (state.split === 'rgb') return boxCell(color, state.k);
            return labCell(labValue[0], labValue[1], labValue[2], labLevels(state.k).n);
        };
        let voxels = {};
        function rebuildVoxels() {
            voxels = {};
            for (const pane of panes) voxels[pane.space] = pane.space === 'rgb' ? rgbVoxels(state.k) : labVoxels(labLevels(state.k).n);
        }
        const currentKeep = () => state.keep ? cellOf(state.color, lab(state.color)) : -1;
        function rebuildMeshes() {
            const keep = currentKeep();
            for (const pane of panes) {
                const set = voxels[pane.space];
                const cells = new Int32Array(set.count);
                for (let v = 0; v < set.count; v++) cells[v] = cellOf(set.color[v], set.lab.subarray(v * 3, v * 3 + 3));
                pane.renderer.upload('classes', buildMesh(set, cells, { cut: state.cut, keep }));
                if (pane.voxels !== set) pane.renderer.upload('shell', buildShell(set));
                pane.voxels = set;
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
                if (todo.has('voxels')) rebuildVoxels();
                if (todo.has('voxels') || todo.has('mesh')) rebuildMeshes();
                if (todo.has('size')) resize();
                describe();
                draw();
            });
        }

        element.querySelectorAll('[data-split]').forEach((b) => b.addEventListener('click', () => {
            state.split = b.dataset.split;
            update(['mesh']);
        }));
        kInput.addEventListener('input', () => { state.k = Number(kInput.value); update(['voxels']); });
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
                    const v = pane.renderer.pick({ yaw: state.yaw, pitch: state.pitch, explode: state.explode, keep: currentKeep() },
                        (e.clientX - rect.left) * ratio, (e.clientY - rect.top) * ratio);
                    if (v >= 0) {
                        state.color = pane.voxels.color[v];
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
        update(['voxels', 'size']);
    }

    const api = { lab, fromLab, feltSize, meanDistance, buildShell, boxCell, boxRange, labCell, labRange, labLevels, labCounts, rgbVoxels, labVoxels, buildMesh, boxSwatch, labSwatch, mount };
    if (typeof module !== 'undefined' && module.exports) module.exports = api;
    root.ColorClasses = api;
    if (typeof document !== 'undefined') {
        const start = () => document.querySelectorAll('.color-classes').forEach(mount);
        if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start); else start();
    }
})(typeof window !== 'undefined' ? window : globalThis);
