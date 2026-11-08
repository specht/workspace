// The splat viewer of »Gaussian Splatting« (src/content/gaussian-splatting):
// a scene of 3D Gaussians in WebGL2, drawn the way the page's own renderer
// draws it, only on the graphics chip: every splat becomes an ellipse on the
// screen (Σ' = J V Σ Vᵀ Jᵀ, in the vertex shader), the splats are sorted by
// their depth (here on the CPU, a counting sort) and blended back to front.
// That is the same picture as front to back, as the page's renderer does
// it, but the canvas keeps 8 bits a channel: front to back, the colour of
// the many faint splats behind rounds to nothing while their alpha still
// counts, and a street turns dark and hazy.
//
// <div class='splat-viewer' data-scenes='[{"name": "Maßband", "src": "…/measuring-tape.splat"}, …]'></div>
//
// A scene may name where the camera starts, "view": {"target": [x, y, z],
// "distance": d, "yaw": a, "pitch": b} (radians; yaw 0 looks along -z):
// a street scanned from inside is a cloud of stray splats from outside, so
// its camera starts in the street, close to the point it turns around.
// More in "view": "speed" (walking, units a second), "look": true (a place
// to walk through: turning keeps the eye where it is, the wheel goes
// forward; a switch turns the y axis of dragging round), "overfly": true
// (outdoors: a long flight to a spot rises over the houses). Beside "view",
// "spots": [{"name", "target", "distance", "yaw", "pitch", "roll"}, …]
// become buttons that fly the camera there.
//
// Reads .splat (32 bytes a splat: position and scale as float32, RGBA and
// the rotation as bytes, the format of antimatter15/splat), .csplat (16
// bytes a splat, werkzeug/prepare_street.py says how) and the .ply of a
// 3DGS trainer (the colour from f_dc only). The arrow keys or W, A, S, D
// walk (Q and E down and up), and on a touch screen two buttons walk
// forward and back: through a street that was scanned from inside. »Anzahl der Splats« draws only
// the first ones of a file: the files of the page come sorted with the most
// visible splats first (werkzeug/prepare_*.py), so a scene builds up from a
// few large blurry ones to all of them. »Eigene Datei öffnen« reads a
// file from the person's computer; nothing is sent anywhere. Drag to turn,
// the wheel or two fingers to zoom, the right button or Shift to move.
(function () {
    const C0 = 0.28209479177387814;

    function parseSplat(buffer) {
        const n = Math.floor(buffer.byteLength / 32);
        const f = new Float32Array(buffer, 0, n * 8);
        const u = new Uint8Array(buffer, 0, n * 32);
        const s = { count: n, position: new Float32Array(n * 3), scale: new Float32Array(n * 3),
            rotation: new Float32Array(n * 4), color: new Float32Array(n * 4) };
        for (let i = 0; i < n; i++) {
            for (let k = 0; k < 3; k++) {
                s.position[i * 3 + k] = f[i * 8 + k];
                s.scale[i * 3 + k] = f[i * 8 + 3 + k];
            }
            for (let k = 0; k < 4; k++) {
                s.color[i * 4 + k] = u[i * 32 + 24 + k] / 255;
                s.rotation[i * 4 + k] = (u[i * 32 + 28 + k] - 128) / 128;
            }
        }
        return s;
    }

    function parseCompact(buffer) {
        const head = new DataView(buffer, 0, 40);
        const n = head.getUint32(8, true);
        const low = [0, 1, 2].map((k) => head.getFloat32(12 + 4 * k, true));
        const high = [0, 1, 2].map((k) => head.getFloat32(24 + 4 * k, true));
        const u = new Uint8Array(buffer, 40, n * 16);
        const s = { count: n, position: new Float32Array(n * 3), scale: new Float32Array(n * 3),
            rotation: new Float32Array(n * 4), color: new Float32Array(n * 4) };
        const r = Math.SQRT1_2;
        const q = [0, 0, 0, 0];
        for (let i = 0; i < n; i++) {
            const o = i * 16;
            for (let k = 0; k < 3; k++) {
                s.position[i * 3 + k] = low[k] + (u[o + 2 * k] | (u[o + 2 * k + 1] << 8)) / 65535 * (high[k] - low[k]);
                s.scale[i * 3 + k] = Math.exp(u[o + 6 + k] / 255 * 14 - 12);
            }
            for (let k = 0; k < 4; k++) s.color[i * 4 + k] = u[o + 9 + k] / 255;
            const packed = (u[o + 13] << 16) | (u[o + 14] << 8) | u[o + 15];
            const largest = packed >> 22;
            const rest = [(packed >> 14) & 127, (packed >> 7) & 127, packed & 127].map((v) => (v / 127 * 2 - 1) * r);
            let sum = 0;
            for (let k = 0, j = 0; k < 4; k++) {
                if (k === largest) continue;
                q[k] = rest[j++];
                sum += q[k] * q[k];
            }
            q[largest] = Math.sqrt(Math.max(0, 1 - sum));
            for (let k = 0; k < 4; k++) s.rotation[i * 4 + k] = q[k];
        }
        return s;
    }

    function parse(buffer, name) {
        const magic = new TextDecoder().decode(new Uint8Array(buffer, 0, Math.min(8, buffer.byteLength)));
        if (magic === 'CSPLAT01') return parseCompact(buffer);
        if (magic.startsWith('ply')) return parsePly(buffer);
        if (/\.ply$/i.test(name)) throw new Error('Das ist keine PLY-Datei.');
        return parseSplat(buffer);
    }

    function parsePly(buffer) {
        const bytes = new Uint8Array(buffer);
        const head = new TextDecoder().decode(bytes.subarray(0, Math.min(bytes.length, 64 * 1024)));
        const end = head.indexOf('end_header\n');
        if (!head.startsWith('ply') || end < 0) throw new Error('Das ist keine PLY-Datei.');
        if (!/format binary_little_endian/.test(head)) throw new Error('Nur binäre PLY-Dateien gehen hier.');
        const lines = head.slice(0, end).split('\n');
        const props = [];
        let count = 0, inVertex = false, offset = 0;
        const sizes = { char: 1, uchar: 1, int8: 1, uint8: 1, short: 2, ushort: 2, int16: 2, uint16: 2, int: 4, uint: 4, int32: 4, uint32: 4, float: 4, float32: 4, double: 8, float64: 8 };
        for (const line of lines) {
            const w = line.trim().split(/\s+/);
            if (w[0] === 'element') { inVertex = w[1] === 'vertex'; if (inVertex) count = parseInt(w[2], 10); }
            else if (w[0] === 'property' && inVertex) {
                if (w[1] === 'list') throw new Error('Diese PLY-Datei ist keine Splat-Datei.');
                props.push({ name: w[2], type: w[1], offset }); offset += sizes[w[1]] || 4;
            }
        }
        const stride = offset;
        const at = Object.fromEntries(props.map((p) => [p.name, p]));
        for (const need of ['x', 'y', 'z', 'scale_0', 'rot_0', 'opacity', 'f_dc_0']) {
            if (!at[need]) throw new Error(at.packed_position ? 'Komprimierte PLY-Dateien kann dieser Viewer nicht lesen.' : `In der Datei fehlt »${need}«: keine Splat-Datei aus einem Training?`);
        }
        const view = new DataView(buffer, end + 'end_header\n'.length);
        const get = (i, p) => (p.type === 'double' || p.type === 'float64') ? view.getFloat64(i * stride + p.offset, true) : view.getFloat32(i * stride + p.offset, true);
        const s = { count, position: new Float32Array(count * 3), scale: new Float32Array(count * 3),
            rotation: new Float32Array(count * 4), color: new Float32Array(count * 4) };
        const pos = [at.x, at.y, at.z], sc = [at.scale_0, at.scale_1, at.scale_2],
            rot = [at.rot_0, at.rot_1, at.rot_2, at.rot_3], dc = [at.f_dc_0, at.f_dc_1, at.f_dc_2];
        for (let i = 0; i < count; i++) {
            for (let k = 0; k < 3; k++) {
                s.position[i * 3 + k] = get(i, pos[k]);
                s.scale[i * 3 + k] = Math.exp(get(i, sc[k]));
                s.color[i * 4 + k] = Math.min(1, Math.max(0, 0.5 + C0 * get(i, dc[k])));
            }
            s.color[i * 4 + 3] = 1 / (1 + Math.exp(-get(i, at.opacity)));
            let q = rot.map((p) => get(i, p));
            const len = Math.hypot(...q) || 1;
            for (let k = 0; k < 4; k++) s.rotation[i * 4 + k] = q[k] / len;
        }
        return s;
    }

    // the 3D covariance Σ = R S Sᵀ Rᵀ of every splat, as six numbers
    function covariances(s, size) {
        const out = new Float32Array(s.count * 6);
        for (let i = 0; i < s.count; i++) {
            const w = s.rotation[i * 4], x = s.rotation[i * 4 + 1], y = s.rotation[i * 4 + 2], z = s.rotation[i * 4 + 3];
            const R = [1 - 2 * (y * y + z * z), 2 * (x * y - w * z), 2 * (x * z + w * y),
                2 * (x * y + w * z), 1 - 2 * (x * x + z * z), 2 * (y * z - w * x),
                2 * (x * z - w * y), 2 * (y * z + w * x), 1 - 2 * (x * x + y * y)];
            const sx = s.scale[i * 3] * size, sy = s.scale[i * 3 + 1] * size, sz = s.scale[i * 3 + 2] * size;
            const M = [R[0] * sx, R[1] * sy, R[2] * sz, R[3] * sx, R[4] * sy, R[5] * sz, R[6] * sx, R[7] * sy, R[8] * sz];
            out[i * 6] = M[0] * M[0] + M[1] * M[1] + M[2] * M[2];
            out[i * 6 + 1] = M[0] * M[3] + M[1] * M[4] + M[2] * M[5];
            out[i * 6 + 2] = M[0] * M[6] + M[1] * M[7] + M[2] * M[8];
            out[i * 6 + 3] = M[3] * M[3] + M[4] * M[4] + M[5] * M[5];
            out[i * 6 + 4] = M[3] * M[6] + M[4] * M[7] + M[5] * M[8];
            out[i * 6 + 5] = M[6] * M[6] + M[7] * M[7] + M[8] * M[8];
        }
        return out;
    }

    const VERTEX = `#version 300 es
precision highp float;
precision highp int;
uniform highp sampler2D u_data;
uniform mat3 u_view;       // world -> camera: x right, y up, the camera looks along -z
uniform vec3 u_eye;
uniform vec2 u_focal;      // in pixels
uniform vec2 u_viewport;
in vec2 a_corner;          // a corner of the square, -3 ... 3 (three standard deviations)
in uint a_index;
out vec4 v_color;
out vec2 v_corner;
void main() {
    int i = int(a_index);
    ivec2 base = ivec2((i & 1023) * 4, i >> 10);
    vec4 t0 = texelFetch(u_data, base, 0);                 // x y z alpha
    vec4 t1 = texelFetch(u_data, base + ivec2(1, 0), 0);   // r g b σxx
    vec4 t2 = texelFetch(u_data, base + ivec2(2, 0), 0);   // σxy σxz σyy σyz
    vec4 t3 = texelFetch(u_data, base + ivec2(3, 0), 0);   // σzz
    vec3 cam = u_view * (t0.xyz - u_eye);
    float z = -cam.z;
    if (z < 0.01) { gl_Position = vec4(0.0, 0.0, 2.0, 1.0); return; }
    // splats whose middle lies outside the picture (with a margin) are left
    // out, as in the program of Kerbl: a splat of the wall just beside the
    // camera would otherwise become a huge ellipse across the whole picture
    vec2 center = vec2(u_focal.x * cam.x / z, u_focal.y * cam.y / z);
    if (any(greaterThan(abs(center), 0.65 * u_viewport))) { gl_Position = vec4(0.0, 0.0, 2.0, 1.0); return; }
    mat3 sigma = mat3(t1.w, t2.x, t2.y, t2.x, t2.z, t2.w, t2.y, t2.w, t3.x);
    mat3 J = mat3(u_focal.x / z, 0.0, 0.0,
                  0.0, u_focal.y / z, 0.0,
                  u_focal.x * cam.x / (z * z), u_focal.y * cam.y / (z * z), 0.0);
    mat3 T = J * u_view;
    mat3 cov = T * sigma * transpose(T);
    float a = cov[0][0] + 0.3, b = cov[0][1], c = cov[1][1] + 0.3;
    float mid = 0.5 * (a + c);
    float r = length(vec2(0.5 * (a - c), b));
    float l1 = mid + r, l2 = max(mid - r, 0.01);
    vec2 e1 = abs(b) > 1e-9 ? normalize(vec2(b, l1 - a)) : (a >= c ? vec2(1.0, 0.0) : vec2(0.0, 1.0));
    vec2 e2 = vec2(-e1.y, e1.x);
    vec2 offset = a_corner.x * min(sqrt(l1), 1024.0) * e1 + a_corner.y * min(sqrt(l2), 1024.0) * e2;
    gl_Position = vec4((center + offset) * 2.0 / u_viewport, 0.0, 1.0);
    v_color = vec4(t1.rgb, t0.w);
    v_corner = a_corner;
}`;

    const FRAGMENT = `#version 300 es
precision highp float;
uniform float u_sharpness;  // 1: the Gaussian; larger: towards a hard ellipse
in vec4 v_color;
in vec2 v_corner;
out vec4 fragColor;
void main() {
    float q = dot(v_corner, v_corner);
    if (q > 9.0) discard;
    // q^k with k = 1 is the bell of the Gaussian; the larger k, the flatter
    // it is inside one standard deviation (q < 1) and the steeper it falls
    // at its edge: an ellipse with a sharp border
    float alpha = min(0.99, v_color.a * exp(-0.5 * pow(q, u_sharpness)));
    if (alpha < 1.0 / 255.0) discard;
    fragColor = vec4(v_color.rgb * alpha, alpha);
}`;

    function compile(gl, type, source) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(shader));
        return shader;
    }

    // the value below which the share q of the values lies (q = 0.5: the
    // median), from a sample of 5000; values holds stride numbers a splat,
    // k picks one of them
    function quantile(values, stride, k, q) {
        const list = [];
        const step = Math.max(1, Math.floor(values.length / stride / 5000));
        for (let i = k; i < values.length; i += stride * step) list.push(values[i]);
        list.sort((a, b) => a - b);
        return list[Math.min(list.length - 1, Math.floor(list.length * q))] || 0;
    }

    class Viewer {
        constructor(root) {
            this.root = root;
            this.scenes = JSON.parse(root.dataset.scenes || '[]');
            this.size = 1;
            this.yaw = 0.6; this.pitch = 0.3; this.distance = 1; this.target = [0, 0, 0];
            this.build();
            if (!this.gl) return;
            window.addEventListener('resize', () => { if (this.splats) { this.fit(); this.redraw(); } });
            if (this.scenes.length) this.load(this.scenes[0]);
        }

        build() {
            const root = this.root;
            root.innerHTML = '';
            const bar = document.createElement('div');
            bar.className = 'splat-viewer-bar';
            if (this.scenes.length > 1) {
                this.select = document.createElement('select');
                this.select.className = 'form-select form-select-sm';
                this.select.setAttribute('aria-label', 'Szene');
                this.scenes.forEach((scene, i) => this.select.append(new Option(scene.name, i)));
                this.select.addEventListener('change', () => this.load(this.scenes[this.select.value]));
                bar.append(this.select);
            }
            const sliders = document.createElement('div');
            sliders.className = 'splat-viewer-sliders';
            const sizeLabel = document.createElement('label');
            sizeLabel.className = 'splat-viewer-size';
            sizeLabel.innerHTML = '<span>Größe der Splats</span>';
            this.slider = document.createElement('input');
            this.slider.type = 'range'; this.slider.className = 'form-range'; this.slider.min = '0.05'; this.slider.max = '1'; this.slider.step = '0.05'; this.slider.value = '1';
            this.slider.addEventListener('input', () => { this.size = parseFloat(this.slider.value); this.showValues(); this.upload(); });
            this.sizeValue = document.createElement('output');
            sizeLabel.append(this.slider, this.sizeValue);
            sliders.append(sizeLabel);
            const countLabel = document.createElement('label');
            countLabel.className = 'splat-viewer-size';
            countLabel.innerHTML = '<span>Anzahl der Splats</span>';
            this.counter = document.createElement('input');
            this.counter.type = 'range'; this.counter.className = 'form-range'; this.counter.min = '0'; this.counter.max = '1'; this.counter.step = '0.01'; this.counter.value = '1';
            this.counter.addEventListener('input', () => { this.lastSort = null; this.describe(); this.redraw(); });
            this.countValue = document.createElement('output');
            countLabel.append(this.counter, this.countValue);
            sliders.append(countLabel);
            const sharpLabel = document.createElement('label');
            sharpLabel.className = 'splat-viewer-size';
            sharpLabel.innerHTML = '<span>Weichheit der Splats</span>';
            this.sharpness = document.createElement('input');
            this.sharpness.type = 'range'; this.sharpness.className = 'form-range'; this.sharpness.min = '0'; this.sharpness.max = '1'; this.sharpness.step = '0.01'; this.sharpness.value = '1';
            this.sharpness.setAttribute('aria-valuetext', 'weich');
            this.sharpness.addEventListener('input', () => {
                this.sharpness.setAttribute('aria-valuetext', this.sharpness.value < 0.5 ? 'scharf' : 'weich');
                this.showValues();
                this.redraw();
            });
            // the exponent k of exp(-q^k / 2): 1 is the bell of Gauss, 32 an
            // ellipse with a sharp border (sharpness in paint() of the page)
            this.sharpValue = document.createElement('output');
            this.sharpValue.title = 'Der Exponent der Glocke: 1 ist die Glocke von Gauß, 32 eine Ellipse mit scharfem Rand';
            sharpLabel.append(this.sharpness, this.sharpValue);
            sliders.append(sharpLabel);
            const open = document.createElement('button');
            open.type = 'button';
            open.className = 'btn btn-sm btn-outline-secondary';
            open.innerHTML = "<i class='bi bi-folder2-open'></i><span>Eigene Datei öffnen</span>";
            this.file = document.createElement('input');
            this.file.type = 'file'; this.file.accept = '.ply,.splat'; this.file.hidden = true;
            open.addEventListener('click', () => this.file.click());
            this.file.addEventListener('change', () => { if (this.file.files[0]) this.loadFile(this.file.files[0]); this.file.value = ''; });
            bar.append(open, this.file);
            const turn = document.createElement('button');
            turn.type = 'button';
            turn.className = 'btn btn-sm btn-outline-secondary';
            // only for a file of one's own: a trainer often writes it with
            // another axis up, and the scene lies on its side
            turn.title = 'Die eigene Datei um eine Vierteldrehung drehen, wenn oben nicht oben ist';
            turn.innerHTML = "<i class='bi bi-arrow-clockwise'></i><span>Aufrichten</span>";
            turn.hidden = true;
            turn.addEventListener('click', () => this.tilt());
            this.turnButton = turn;
            bar.append(turn);
            // in a street: the y axis of dragging turned round; remembered in
            // this browser (only a convenience, so it may fail)
            const game = document.createElement('label');
            game.className = 'form-check form-switch splat-viewer-game';
            game.hidden = true;
            this.invertY = document.createElement('input');
            this.invertY.type = 'checkbox';
            this.invertY.className = 'form-check-input';
            this.invertY.setAttribute('role', 'switch');
            try { this.invertY.checked = localStorage.getItem('splat-viewer-invert-y') === '1'; } catch (e) { /* no storage */ }
            this.invertY.addEventListener('change', () => {
                try { localStorage.setItem('splat-viewer-invert-y', this.invertY.checked ? '1' : '0'); } catch (e) { /* no storage */ }
            });
            const gameText = document.createElement('span');
            gameText.className = 'form-check-label';
            gameText.textContent = 'Y-Achse umkehren';
            game.append(this.invertY, gameText);
            this.gameSwitch = game;
            bar.append(game);
            const swing = document.createElement('label');
            swing.className = 'form-check form-switch splat-viewer-game';
            this.swing = document.createElement('input');
            this.swing.type = 'checkbox';
            this.swing.className = 'form-check-input';
            this.swing.setAttribute('role', 'switch');
            this.swing.addEventListener('change', () => this.swingLoop());
            const swingText = document.createElement('span');
            swingText.className = 'form-check-label';
            swingText.textContent = 'Kamera pendelt';
            swing.title = 'Die Kamera bewegt sich langsam ein Stück zur Seite und zurück';
            swing.append(this.swing, swingText);
            bar.append(swing);
            this.canvas = document.createElement('canvas');
            this.canvas.className = 'splat-viewer-canvas';
            this.canvas.tabIndex = 0;
            this.canvas.setAttribute('aria-label', 'Szene: ziehen oder Pfeiltasten zum Drehen, W, A, S, D zum Gehen');
            this.status = document.createElement('div');
            this.status.className = 'splat-viewer-status';
            const stage = document.createElement('div');
            stage.className = 'splat-viewer-stage';
            this.stage = stage;
            const walk = document.createElement('div');
            walk.className = 'splat-viewer-walk';
            for (const [dir, icon, label] of [[1, 'bi-arrow-up', 'Vorwärts'], [-1, 'bi-arrow-down', 'Rückwärts']]) {
                const button = document.createElement('button');
                button.type = 'button';
                button.className = 'btn btn-light';
                button.setAttribute('aria-label', label);
                button.innerHTML = `<i class='bi ${icon}'></i>`;
                const stop = () => { this.held.delete(dir > 0 ? 'touch-forward' : 'touch-back'); };
                button.addEventListener('pointerdown', (e) => { e.preventDefault(); this.held.add(dir > 0 ? 'touch-forward' : 'touch-back'); this.walkLoop(); });
                button.addEventListener('pointerup', stop);
                button.addEventListener('pointerleave', stop);
                button.addEventListener('pointercancel', stop);
                walk.append(button);
            }
            stage.append(this.canvas, this.status, walk);
            this.info = document.createElement('div');
            this.info.className = 'splat-viewer-info';
            // the keys and what they do, for the scene shown (showHelp)
            const help = document.createElement('div');
            help.className = 'splat-viewer-help';
            this.help = help;
            // the spots of a scene (»spots« in its entry): buttons that fly
            // the camera there, filled in by show()
            this.spotBar = document.createElement('div');
            this.spotBar.className = 'splat-viewer-spots';
            this.spotBar.hidden = true;
            root.append(stage, bar, this.spotBar, sliders, this.info, help);

            const gl = this.canvas.getContext('webgl2', { antialias: false, premultipliedAlpha: true, alpha: true });
            if (!gl) { this.message('Dieser Browser kann kein WebGL2. Mit einem aktuellen Firefox, Chrome oder Safari geht es.'); return; }
            this.gl = gl;
            const program = gl.createProgram();
            gl.attachShader(program, compile(gl, gl.VERTEX_SHADER, VERTEX));
            gl.attachShader(program, compile(gl, gl.FRAGMENT_SHADER, FRAGMENT));
            gl.linkProgram(program);
            if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
            this.program = program;
            gl.useProgram(program);
            this.u = {};
            for (const name of ['u_data', 'u_view', 'u_eye', 'u_focal', 'u_viewport', 'u_sharpness']) this.u[name] = gl.getUniformLocation(program, name);
            this.vao = gl.createVertexArray();
            gl.bindVertexArray(this.vao);
            const corners = gl.createBuffer();
            gl.bindBuffer(gl.ARRAY_BUFFER, corners);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-3, -3, 3, -3, 3, 3, -3, 3]), gl.STATIC_DRAW);
            const aCorner = gl.getAttribLocation(program, 'a_corner');
            gl.enableVertexAttribArray(aCorner);
            gl.vertexAttribPointer(aCorner, 2, gl.FLOAT, false, 0, 0);
            const indices = gl.createBuffer();
            gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, indices);
            gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array([0, 1, 2, 0, 2, 3]), gl.STATIC_DRAW);
            this.orderBuffer = gl.createBuffer();
            gl.bindBuffer(gl.ARRAY_BUFFER, this.orderBuffer);
            const aIndex = gl.getAttribLocation(program, 'a_index');
            gl.enableVertexAttribArray(aIndex);
            gl.vertexAttribIPointer(aIndex, 1, gl.UNSIGNED_INT, 0, 0);
            gl.vertexAttribDivisor(aIndex, 1);
            this.texture = gl.createTexture();
            gl.bindTexture(gl.TEXTURE_2D, this.texture);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
            this.controls();
            new ResizeObserver(() => this.redraw()).observe(this.canvas);
        }

        message(text) {
            this.status.textContent = text;
            this.status.hidden = !text;
        }

        async load(scene) {
            const token = this.loading = {};
            this.progress(0, null);
            try {
                const response = await fetch(scene.src);
                if (!response.ok) throw new Error(`Die Szene ließ sich nicht laden (${response.status}).`);
                const buffer = await this.download(response, token);
                if (token !== this.loading) return;
                this.message('Bereitet die Splats vor …');
                await new Promise((r) => setTimeout(r, 30));
                this.view = scene.view || null;
                this.spots = scene.spots || [];
                this.turnButton.hidden = true;
                this.show(parse(buffer, scene.src), scene.credit || '');
                this.loaded = scene.name;
            } catch (e) {
                if (token === this.loading) this.message(e.message);
            }
        }

        // reads the response piece by piece and shows how much has come
        async download(response, token) {
            const total = parseInt(response.headers.get('Content-Length') || '0', 10);
            if (!response.body || !response.body.getReader) return response.arrayBuffer();
            const reader = response.body.getReader();
            const parts = [];
            let received = 0, shown = 0;
            for (;;) {
                const { done, value } = await reader.read();
                if (done) break;
                if (token !== this.loading) { reader.cancel(); return null; }
                parts.push(value);
                received += value.length;
                const now = performance.now();
                if (now - shown > 100) { shown = now; this.progress(received, total); }
            }
            const out = new Uint8Array(received);
            let at = 0;
            for (const part of parts) { out.set(part, at); at += part.length; }
            return out.buffer;
        }

        progress(received, total) {
            const mb = (v) => (v / 1e6).toLocaleString('de-DE', { maximumFractionDigits: 0 });
            this.status.hidden = false;
            this.status.textContent = '';
            const box = document.createElement('div');
            box.className = 'splat-viewer-progress';
            const text = document.createElement('div');
            const bar = document.createElement('div');
            bar.className = 'progress';
            bar.setAttribute('role', 'progressbar');
            const fill = document.createElement('div');
            fill.className = 'progress-bar';
            bar.append(fill);
            if (total) {
                const percent = Math.min(100, Math.round(100 * received / total));
                text.textContent = `Lädt … ${mb(received)} von ${mb(total)} MB`;
                fill.style.width = percent + '%';
                bar.setAttribute('aria-valuenow', String(percent));
                bar.setAttribute('aria-valuemin', '0');
                bar.setAttribute('aria-valuemax', '100');
            } else {
                text.textContent = received ? `Lädt … ${mb(received)} MB` : 'Lädt …';
                fill.classList.add('progress-bar-striped', 'progress-bar-animated');
                fill.style.width = '100%';
            }
            box.append(text, bar);
            this.status.append(box);
        }

        loadFile(file) {
            this.loading = {};
            this.message('Lädt …');
            file.arrayBuffer().then((buffer) => {
                this.view = null;
                this.spots = [];
                this.turnButton.hidden = false;
                this.show(parse(buffer, file.name), file.name);
            }).catch((e) => this.message(e.message));
        }

        show(splats, credit) {
            this.splats = splats;
            this.credit = credit;
            const n = splats.count;
            // the camera looks at the middle of the object's extent, not at
            // the median of its splats: a fire extinguisher has most of
            // them in its base. The outermost 1 % on each side are left out
            // (stray splats far away).
            this.target = [0, 1, 2].map((k) => (quantile(splats.position, 3, k, 0.01) + quantile(splats.position, 3, k, 0.99)) / 2);
            const d = [];
            const step = Math.max(1, Math.floor(n / 5000));
            for (let i = 0; i < n; i += step) d.push(Math.hypot(...[0, 1, 2].map((k) => splats.position[i * 3 + k] - this.target[k])));
            d.sort((a, b) => a - b);
            this.radius = d[Math.floor(d.length * 0.9)] || 1;
            this.distance = this.radius * 3;
            this.yaw = 0.6; this.pitch = 0.3; this.roll = 0;
            this.look = !!(this.view && this.view.look);
            this.gameSwitch.hidden = !this.look;
            this.showHelp();
            if (this.view) {
                if (this.view.target) this.target = this.view.target.slice();
                if (this.view.distance) this.distance = this.view.distance;
                if (this.view.yaw !== undefined) this.yaw = this.view.yaw;
                if (this.view.pitch !== undefined) this.pitch = this.view.pitch;
            }
            this.up = 0;
            this.counter.value = '1';
            this.showSpots();
            this.upload();
            this.fit();
            this.message('');
            this.describe();
        }

        showSpots() {
            const spots = this.spots || [];
            this.spotBar.replaceChildren();
            this.spotBar.hidden = spots.length === 0;
            if (!spots.length) return;
            const title = document.createElement('span');
            title.textContent = 'Standorte:';
            this.spotBar.append(title);
            for (const spot of spots) {
                const button = document.createElement('button');
                button.type = 'button';
                button.className = 'btn btn-sm btn-outline-secondary';
                button.textContent = spot.name;
                button.addEventListener('click', () => this.fly(spot));
                this.spotBar.append(button);
            }
        }

        // moves the camera to a spot in a short flight instead of a jump,
        // so that one sees where it lies in the scene
        fly(spot) {
            const from = { target: this.target.slice(), distance: this.distance, yaw: this.yaw, pitch: this.pitch, roll: this.roll || 0 };
            // the shorter way round
            let yaw = spot.yaw;
            while (yaw - from.yaw > Math.PI) yaw -= 2 * Math.PI;
            while (yaw - from.yaw < -Math.PI) yaw += 2 * Math.PI;
            // outdoors (»overfly« in the view, the street) a long flight
            // rises in an arc over the houses and comes down again, like a
            // drone: there are no walls to steer around, only splats, and
            // from above one sees where one lands. In a room it would go
            // through the ceiling, so there the flight goes straight.
            const travel = Math.hypot(...[0, 1, 2].map((k) => spot.target[k] - from.target[k]));
            const overfly = this.view && this.view.overfly;
            const hump = overfly && travel > 2 * this.walkSpeed() ? Math.min(0.4 * travel, 4 * this.walkSpeed()) : 0;
            const time = this.look ? Math.min(2600, 1200 + 40 * travel / this.walkSpeed() * 1.5) : 1200;
            const start = performance.now();
            const flight = this.flight = {};
            const step = (now) => {
                if (this.flight !== flight) return;
                const t = Math.min(1, (now - start) / time);
                const e = t * t * (3 - 2 * t);
                this.target = from.target.map((v, k) => v + (spot.target[k] - v) * e + (k === 1 ? hump * Math.sin(Math.PI * e) : 0));
                this.distance = from.distance + (spot.distance - from.distance) * e;
                this.yaw = from.yaw + (yaw - from.yaw) * e;
                this.pitch = from.pitch + (spot.pitch - from.pitch) * e;
                this.roll = from.roll + ((spot.roll || 0) - from.roll) * e;
                this.redraw();
                if (t < 1) requestAnimationFrame(step);
                else this.flight = null;
            };
            requestAnimationFrame(step);
        }

        // how many splats the slider asks for: from 100 to all, evenly on a
        // logarithmic scale
        drawn() {
            const n = this.splats ? this.splats.count : 0;
            const v = parseFloat(this.counter.value);
            if (n <= 100 || v >= 1) return n;
            return Math.max(1, Math.round(100 * Math.pow(n / 100, v)));
        }

        // the whole viewer fits into the window, the controls and the legend
        // below the picture included: the picture gets at most the height
        // that is left (it becomes wider than 16:9 then, not smaller), minus
        // what is fixed over the page (the footer in development)
        fit() {
            const below = this.root.getBoundingClientRect().bottom - this.stage.getBoundingClientRect().bottom;
            let covered = 0;
            for (const bar of document.querySelectorAll('footer, header, nav')) {
                const position = getComputedStyle(bar).position;
                if (position === 'fixed' || position === 'sticky') covered += bar.getBoundingClientRect().height;
            }
            const room = Math.floor(window.innerHeight - covered - below - 16);
            this.stage.style.maxHeight = room >= 240 ? `${room}px` : '';
        }

        // a legend of the controls: in a place to walk through other ones
        // than around an object
        showHelp() {
            const k = (...keys) => keys.map((key) => `<kbd>${key}</kbd>`).join('');
            const items = this.look ? [
                [k('W', 'A', 'S', 'D'), 'gehen'],
                [k('Q') + k('E'), 'nach unten, nach oben'],
                [k('←', '→', '↑', '↓') + ' oder ziehen', 'umschauen'],
                ['Mausrad', 'vorwärts, rückwärts'],
                ['Doppelklick', 'zurück zum Anfang'],
            ] : [
                ['Ziehen oder ' + k('←', '→', '↑', '↓'), 'drehen'],
                ['Mausrad oder zwei Finger', 'näher heran'],
                ['Rechte Maustaste oder ' + k('Shift') + ' und ziehen', 'verschieben'],
                [k('W', 'A', 'S', 'D') + ' ' + k('Q') + k('E'), 'bewegen'],
                ['Doppelklick', 'zurück zum Anfang'],
            ];
            this.help.innerHTML = items.map(([keys, what]) => `<span><span class='splat-viewer-keys'>${keys}</span>: ${what}</span>`).join('');
        }

        // the numbers beside the sliders
        exponent() {
            return Math.pow(32, 1 - parseFloat(this.sharpness.value));
        }

        showValues() {
            this.sizeValue.textContent = `${Math.round(parseFloat(this.slider.value) * 100)} %`;
            this.countValue.textContent = this.splats ? this.drawn().toLocaleString('de-DE') : '';
            const k = this.exponent();
            this.sharpValue.textContent = k < 9.95 ? k.toLocaleString('de-DE', { minimumFractionDigits: 1, maximumFractionDigits: 1 }) : String(Math.round(k));
        }

        describe() {
            if (!this.splats) return;
            this.showValues();
            const n = this.splats.count, k = this.drawn();
            const count = k < n ? `${k.toLocaleString('de-DE')} von ${n.toLocaleString('de-DE')} Splats` : `${n.toLocaleString('de-DE')} Splats`;
            this.info.textContent = count + (this.credit ? ', ' + this.credit : '');
        }

        tilt() {
            if (!this.splats) return;
            // a quarter turn around x: (x, y, z) -> (x, -z, y), and the rotation with it
            const s = this.splats;
            const c = Math.SQRT1_2;
            for (let i = 0; i < s.count; i++) {
                const y = s.position[i * 3 + 1], z = s.position[i * 3 + 2];
                s.position[i * 3 + 1] = -z; s.position[i * 3 + 2] = y;
                const w = s.rotation[i * 4], x = s.rotation[i * 4 + 1], qy = s.rotation[i * 4 + 2], qz = s.rotation[i * 4 + 3];
                s.rotation[i * 4] = c * (w - x); s.rotation[i * 4 + 1] = c * (x + w);
                s.rotation[i * 4 + 2] = c * (qy - qz); s.rotation[i * 4 + 3] = c * (qz + qy);
            }
            const t = this.target;
            this.target = [t[0], -t[2], t[1]];
            this.upload();
        }

        upload() {
            const gl = this.gl, s = this.splats;
            if (!gl || !s) return;
            const cov = covariances(s, this.size);
            const rows = Math.ceil(s.count / 1024);
            const data = new Float32Array(1024 * 4 * 4 * rows);
            for (let i = 0; i < s.count; i++) {
                const o = i * 16;
                data[o] = s.position[i * 3]; data[o + 1] = s.position[i * 3 + 1]; data[o + 2] = s.position[i * 3 + 2]; data[o + 3] = s.color[i * 4 + 3];
                data[o + 4] = s.color[i * 4]; data[o + 5] = s.color[i * 4 + 1]; data[o + 6] = s.color[i * 4 + 2]; data[o + 7] = cov[i * 6];
                data[o + 8] = cov[i * 6 + 1]; data[o + 9] = cov[i * 6 + 2]; data[o + 10] = cov[i * 6 + 3]; data[o + 11] = cov[i * 6 + 4];
                data[o + 12] = cov[i * 6 + 5];
            }
            gl.bindTexture(gl.TEXTURE_2D, this.texture);
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA32F, 4096, rows, 0, gl.RGBA, gl.FLOAT, data);
            this.order = new Uint32Array(s.count);
            this.depths = new Float32Array(s.count);
            this.keys = new Uint16Array(s.count);
            this.lastSort = null;
            this.redraw();
        }

        camera() {
            const cp = Math.cos(this.pitch), sp = Math.sin(this.pitch), cy = Math.cos(this.yaw), sy = Math.sin(this.yaw);
            const back = [sy * cp, sp, cy * cp];                      // from the target to the eye
            const flatRight = [cy, 0, -sy];
            // »Kamera pendelt«: the eye swings a little to the side and back, so that
            // what lies deeper (a reflection behind a window) moves against
            // what lies in front
            const sway = this.sway || 0;
            const eye = this.target.map((t, k) => t + back[k] * this.distance + flatRight[k] * sway);
            const flatUp = [-sy * sp, cp, -cy * sp];
            // tilted sideways by roll (a spot may want a slanted view)
            const cr = Math.cos(this.roll || 0), sr = Math.sin(this.roll || 0);
            const right = flatRight.map((v, k) => cr * v + sr * flatUp[k]);
            const up = flatUp.map((v, k) => -sr * flatRight[k] + cr * v);
            return { eye, right, up, back };
        }

        sort(cam) {
            const s = this.splats, n = this.drawn();
            const f = cam.back;
            let min = Infinity, max = -Infinity;
            for (let i = 0; i < n; i++) {
                const d = -((s.position[i * 3] - cam.eye[0]) * f[0] + (s.position[i * 3 + 1] - cam.eye[1]) * f[1] + (s.position[i * 3 + 2] - cam.eye[2]) * f[2]);
                this.depths[i] = d;
                if (d < min) min = d;
                if (d > max) max = d;
            }
            // a counting sort on 16 bits of depth: the farthest first
            const scale = 65535 / Math.max(max - min, 1e-9);
            const counts = new Uint32Array(65536);
            for (let i = 0; i < n; i++) { const k = ((max - this.depths[i]) * scale) | 0; this.keys[i] = k; counts[k]++; }
            for (let k = 1; k < 65536; k++) counts[k] += counts[k - 1];
            for (let i = n - 1; i >= 0; i--) this.order[--counts[this.keys[i]]] = i;
            const gl = this.gl;
            gl.bindBuffer(gl.ARRAY_BUFFER, this.orderBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, this.order.subarray(0, n), gl.DYNAMIC_DRAW);
            this.sortedCount = n;
        }

        redraw() {
            if (this.frame) return;
            this.frame = requestAnimationFrame(() => { this.frame = null; this.draw(); });
        }

        draw() {
            const gl = this.gl, s = this.splats;
            if (!gl) return;
            const ratio = Math.min(window.devicePixelRatio || 1, 2);
            const w = Math.max(1, Math.round(this.canvas.clientWidth * ratio)), h = Math.max(1, Math.round(this.canvas.clientHeight * ratio));
            if (this.canvas.width !== w || this.canvas.height !== h) { this.canvas.width = w; this.canvas.height = h; }
            gl.viewport(0, 0, w, h);
            gl.clearColor(0, 0, 0, 0);
            gl.clear(gl.COLOR_BUFFER_BIT);
            if (!s) return;
            const cam = this.camera();
            const key = cam.back.map((v) => v.toFixed(3)).join() + cam.eye.map((v) => v.toFixed(4)).join();
            if (key !== this.lastSort) { this.sort(cam); this.lastSort = key; }
            const focal = 0.5 * h / Math.tan(25 * Math.PI / 180);
            gl.useProgram(this.program);
            gl.bindVertexArray(this.vao);
            gl.activeTexture(gl.TEXTURE0);
            gl.bindTexture(gl.TEXTURE_2D, this.texture);
            gl.uniform1i(this.u.u_data, 0);
            // rows of the matrix: right, up, back; mat3 in GLSL takes columns
            gl.uniformMatrix3fv(this.u.u_view, false, [cam.right[0], cam.up[0], cam.back[0], cam.right[1], cam.up[1], cam.back[1], cam.right[2], cam.up[2], cam.back[2]]);
            gl.uniform3fv(this.u.u_eye, cam.eye);
            gl.uniform2f(this.u.u_focal, focal, focal);
            gl.uniform2f(this.u.u_viewport, w, h);
            // »Weichheit« from 1 (the Gaussian, k = 1) down to 0 (k = 32, an
            // ellipse with a sharp border)
            gl.uniform1f(this.u.u_sharpness, this.sharpness ? this.exponent() : 1);
            gl.enable(gl.BLEND);
            // back to front: each splat covers what is already there as far
            // as it is opaque (the colour comes premultiplied by alpha)
            gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
            gl.drawElementsInstanced(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0, this.sortedCount);
        }

        // walking: while a key or a button is held, the camera and the point
        // it turns around move together, along the ground and sideways
        // while »Kamera pendelt« is on: the eye swings sideways, a few centimetres in
        // a street, a few percent of the distance around an object, once in
        // three seconds
        swingLoop() {
            if (this.swinging) return;
            this.swinging = true;
            const start = performance.now();
            const step = (now) => {
                if (!this.swing.checked) {
                    this.sway = 0;
                    this.swinging = false;
                    this.redraw();
                    return;
                }
                const amplitude = this.look ? 0.06 * this.walkSpeed() : 0.04 * this.distance;
                this.sway = amplitude * Math.sin((now - start) / 3000 * 2 * Math.PI);
                this.draw();
                requestAnimationFrame(step);
            };
            requestAnimationFrame(step);
        }

        walkSpeed() {
            return (this.view && this.view.speed) || this.radius * 0.6;
        }

        walkLoop() {
            this.flight = null;
            if (this.walking) return;
            this.walking = true;
            let last = performance.now();
            const step = (now) => {
                const dt = Math.min(0.05, (now - last) / 1000);
                last = now;
                const h = this.held;
                const ahead = (h.has('w') || h.has('touch-forward') ? 1 : 0) - (h.has('s') || h.has('touch-back') ? 1 : 0);
                const side = (h.has('d') ? 1 : 0) - (h.has('a') ? 1 : 0);
                const up = (h.has('e') ? 1 : 0) - (h.has('q') ? 1 : 0);
                // the arrow keys turn and tilt, as dragging does
                const turnSide = (h.has('arrowleft') ? 1 : 0) - (h.has('arrowright') ? 1 : 0);
                const turnUp = (h.has('arrowup') ? 1 : 0) - (h.has('arrowdown') ? 1 : 0);
                if (!this.splats || (!ahead && !side && !up && !turnSide && !turnUp)) { this.walking = false; return; }
                if (turnSide || turnUp) this.turn(turnSide * 1.4 * dt, -turnUp * 1.0 * dt);
                const speed = this.walkSpeed();
                this.level();
                const sy = Math.sin(this.yaw), cy = Math.cos(this.yaw);
                const forward = [-sy, 0, -cy], right = [cy, 0, -sy];
                for (let i = 0; i < 3; i++) this.target[i] += (ahead * forward[i] + side * right[i]) * speed * dt;
                this.target[1] += up * speed * dt;
                this.draw();
                requestAnimationFrame(step);
            };
            requestAnimationFrame(step);
        }

        controls() {
            const c = this.canvas;
            this.held = new Set();
            const keys = ['w', 'a', 's', 'd', 'q', 'e', 'arrowup', 'arrowdown', 'arrowleft', 'arrowright'];
            // the keys work anywhere in the viewer, not only on the canvas:
            // after a slider W, A, S, D, Q and E still walk. The arrow keys
            // stay with a slider that has the focus (keyboard users move it
            // with them) and with the list of scenes, which also takes letters.
            const root = this.root;
            root.addEventListener('keydown', (e) => {
                const k = e.key.toLowerCase();
                if (!keys.includes(k) || e.ctrlKey || e.metaKey || e.altKey) return;
                const target = e.target;
                if (target.tagName === 'SELECT') return;
                if (target !== c && k.startsWith('arrow') && target.type === 'range') return;
                e.preventDefault();
                this.held.add(k);
                this.walkLoop();
            });
            root.addEventListener('keyup', (e) => this.held.delete(e.key.toLowerCase()));
            root.addEventListener('focusout', (e) => { if (!root.contains(e.relatedTarget)) this.held.clear(); });
            // a slider or button used with the mouse hands the focus back
            // to the scene, so that the arrow keys walk again
            root.addEventListener('pointerup', (e) => {
                const control = e.target.closest('input[type=range], button');
                if (control && !control.closest('.splat-viewer-walk')) c.focus({ preventScroll: true });
            });
            const pointers = new Map();
            let last = null;
            c.addEventListener('contextmenu', (e) => e.preventDefault());
            c.addEventListener('pointerdown', (e) => {
                this.flight = null;   // a drag stops a flight to a spot
                c.focus({ preventScroll: true });
                c.setPointerCapture(e.pointerId);
                pointers.set(e.pointerId, { x: e.clientX, y: e.clientY, button: e.button, shift: e.shiftKey });
                last = null;
            });
            const end = (e) => { pointers.delete(e.pointerId); last = null; };
            c.addEventListener('pointerup', end);
            c.addEventListener('pointercancel', end);
            c.addEventListener('pointermove', (e) => {
                const p = pointers.get(e.pointerId);
                if (!p) return;
                const dx = e.clientX - p.x, dy = e.clientY - p.y;
                p.x = e.clientX; p.y = e.clientY;
                if (pointers.size === 1) {
                    if (p.button === 2 || p.shift) this.pan(dx, dy);
                    else {
                        if (this.look) {
                            // in a street the view turns where the hand goes,
                            // as in a game, twice the angle a pixel covers;
                            // »Y-Achse umkehren« (remembered) makes dragging
                            // up look down, as in a flight game
                            const perPixel = 4 * Math.tan(25 * Math.PI / 180) / this.canvas.clientHeight;
                            const k = this.invertY.checked ? -1 : 1;
                            this.turn(-dx * perPixel, k * dy * perPixel);
                        } else {
                            // around an object the object turns with the hand
                            this.turn(-dx * 0.008, dy * 0.008);
                        }
                    }
                } else if (pointers.size === 2) {
                    const [a, b] = [...pointers.values()];
                    const now = { d: Math.hypot(a.x - b.x, a.y - b.y), x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 };
                    if (last) {
                        if (this.look) this.dolly(this.walkSpeed() * 0.004 * (now.d - last.d));
                        else this.distance *= last.d / Math.max(now.d, 1);
                        this.pan(now.x - last.x, now.y - last.y);
                    }
                    last = now;
                }
                this.redraw();
            });
            c.addEventListener('wheel', (e) => {
                e.preventDefault();
                this.flight = null;
                if (this.look) this.dolly(-this.walkSpeed() * 0.0015 * e.deltaY);
                else this.distance *= Math.exp(e.deltaY * 0.001);
                this.redraw();
            }, { passive: false });
            c.addEventListener('dblclick', () => { if (this.splats) { this.show(this.splats, this.credit); } });
        }

        // turns the camera: around the point it looks at for an object, around
        // the eye in a street (»look«), so that turning there is turning
        // the head, wherever one has walked to
        // after a slanted spot the horizon comes back level as soon as one
        // walks or turns again: a little with each step (about half a second)
        level() {
            if (this.flight || !this.roll) return;
            this.roll *= 0.88;
            if (Math.abs(this.roll) < 0.002) this.roll = 0;
        }

        turn(yaw, pitch) {
            this.level();
            // the eye without the swing of »Kamera pendelt«, else turning would drift
            const sway = this.sway;
            this.sway = 0;
            const eye = this.look ? this.camera().eye : null;
            this.sway = sway;
            this.yaw += yaw;
            this.pitch = Math.max(-1.5, Math.min(1.5, this.pitch + pitch));
            if (eye) {
                const back = this.camera().back;
                this.target = eye.map((v, k) => v - back[k] * this.distance);
            }
        }

        // in a street the wheel and two fingers go forward and back, along
        // where the camera looks, instead of changing the distance to the
        // point it turns around
        dolly(amount) {
            this.level();
            const back = this.camera().back;
            this.target = this.target.map((v, k) => v - back[k] * amount);
        }

        pan(dx, dy) {
            const cam = this.camera();
            const k = this.distance * 0.0015;
            for (let i = 0; i < 3; i++) this.target[i] += (-dx * cam.right[i] + dy * cam.up[i]) * k;
        }
    }

    function start() {
        document.querySelectorAll('.splat-viewer:not([data-started])').forEach((root) => {
            root.dataset.started = '1';
            try { root.splatViewer = new Viewer(root); } catch (e) { root.textContent = 'Der Viewer konnte nicht starten: ' + e.message; }
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
