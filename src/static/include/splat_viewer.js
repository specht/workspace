// The splat viewer of »Gaussian Splatting« (src/content/gaussian-splatting):
// a scene of 3D Gaussians in WebGL2, drawn the way the page's own renderer
// draws it, only on the graphics chip: every splat becomes an ellipse on the
// screen (Σ' = J V Σ Vᵀ Jᵀ, in the vertex shader), the splats are sorted by
// their depth (here on the CPU, a counting sort) and blended front to back.
//
// <div class='splat-viewer' data-scenes='[{"name": "Maßband", "src": "…/measuring-tape.splat"}, …]'></div>
//
// Reads .splat (32 bytes a splat: position and scale as float32, RGBA and
// the rotation as bytes, the format of antimatter15/splat) and the .ply of a
// 3DGS trainer (the colour from f_dc only). »Eigene Datei öffnen« reads a
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
    vec2 center = vec2(u_focal.x * cam.x / z, u_focal.y * cam.y / z);
    gl_Position = vec4((center + offset) * 2.0 / u_viewport, 0.0, 1.0);
    v_color = vec4(t1.rgb, t0.w);
    v_corner = a_corner;
}`;

    const FRAGMENT = `#version 300 es
precision highp float;
in vec4 v_color;
in vec2 v_corner;
out vec4 fragColor;
void main() {
    float q = dot(v_corner, v_corner);
    if (q > 9.0) discard;
    float alpha = min(0.99, v_color.a * exp(-0.5 * q));
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

    function median(values, stride, k) {
        const list = [];
        const step = Math.max(1, Math.floor(values.length / stride / 5000));
        for (let i = k; i < values.length; i += stride * step) list.push(values[i]);
        list.sort((a, b) => a - b);
        return list[Math.floor(list.length / 2)] || 0;
    }

    class Viewer {
        constructor(root) {
            this.root = root;
            this.scenes = JSON.parse(root.dataset.scenes || '[]');
            this.size = 1;
            this.yaw = 0.6; this.pitch = 0.3; this.distance = 1; this.target = [0, 0, 0];
            this.build();
            if (!this.gl) return;
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
            const sizeLabel = document.createElement('label');
            sizeLabel.className = 'splat-viewer-size';
            sizeLabel.innerHTML = '<span>Größe der Splats</span>';
            this.slider = document.createElement('input');
            this.slider.type = 'range'; this.slider.min = '0.05'; this.slider.max = '1'; this.slider.step = '0.05'; this.slider.value = '1';
            this.slider.addEventListener('input', () => { this.size = parseFloat(this.slider.value); this.upload(); });
            sizeLabel.append(this.slider);
            bar.append(sizeLabel);
            const open = document.createElement('button');
            open.type = 'button';
            open.className = 'btn btn-sm btn-outline-secondary';
            open.innerHTML = "<i class='bi bi-folder2-open'></i> Eigene Datei öffnen";
            this.file = document.createElement('input');
            this.file.type = 'file'; this.file.accept = '.ply,.splat'; this.file.hidden = true;
            open.addEventListener('click', () => this.file.click());
            this.file.addEventListener('change', () => { if (this.file.files[0]) this.loadFile(this.file.files[0]); this.file.value = ''; });
            bar.append(open, this.file);
            const turn = document.createElement('button');
            turn.type = 'button';
            turn.className = 'btn btn-sm btn-outline-secondary';
            turn.title = 'Die Szene um eine Vierteldrehung kippen, wenn oben nicht oben ist';
            turn.innerHTML = "<i class='bi bi-arrow-clockwise'></i> Kippen";
            turn.addEventListener('click', () => this.tilt());
            bar.append(turn);
            this.canvas = document.createElement('canvas');
            this.canvas.className = 'splat-viewer-canvas';
            this.status = document.createElement('div');
            this.status.className = 'splat-viewer-status';
            const stage = document.createElement('div');
            stage.className = 'splat-viewer-stage';
            stage.append(this.canvas, this.status);
            this.info = document.createElement('div');
            this.info.className = 'splat-viewer-info';
            root.append(stage, bar, this.info);

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
            for (const name of ['u_data', 'u_view', 'u_eye', 'u_focal', 'u_viewport']) this.u[name] = gl.getUniformLocation(program, name);
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
            this.message('Lädt …');
            try {
                const response = await fetch(scene.src);
                if (!response.ok) throw new Error(`Die Szene ließ sich nicht laden (${response.status}).`);
                const buffer = await response.arrayBuffer();
                this.show(scene.src.endsWith('.ply') ? parsePly(buffer) : parseSplat(buffer), scene.credit || '');
            } catch (e) {
                this.message(e.message);
            }
        }

        loadFile(file) {
            this.message('Lädt …');
            file.arrayBuffer().then((buffer) => {
                this.show(/\.splat$/i.test(file.name) ? parseSplat(buffer) : parsePly(buffer), file.name);
            }).catch((e) => this.message(e.message));
        }

        show(splats, credit) {
            this.splats = splats;
            this.credit = credit;
            const n = splats.count;
            this.target = [0, 1, 2].map((k) => median(splats.position, 3, k));
            const d = [];
            const step = Math.max(1, Math.floor(n / 5000));
            for (let i = 0; i < n; i += step) d.push(Math.hypot(...[0, 1, 2].map((k) => splats.position[i * 3 + k] - this.target[k])));
            d.sort((a, b) => a - b);
            this.radius = d[Math.floor(d.length * 0.9)] || 1;
            this.distance = this.radius * 3;
            this.yaw = 0.6; this.pitch = 0.3;
            this.up = 0;
            this.upload();
            this.message('');
            this.info.textContent = `${n.toLocaleString('de-DE')} Splats${credit ? ', ' + credit : ''}`;
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
            const eye = this.target.map((t, k) => t + back[k] * this.distance);
            const right = [cy, 0, -sy];
            const up = [-sy * sp, cp, -cy * sp];
            return { eye, right, up, back };
        }

        sort(cam) {
            const s = this.splats, n = s.count;
            const f = cam.back;
            let min = Infinity, max = -Infinity;
            for (let i = 0; i < n; i++) {
                const d = -((s.position[i * 3] - cam.eye[0]) * f[0] + (s.position[i * 3 + 1] - cam.eye[1]) * f[1] + (s.position[i * 3 + 2] - cam.eye[2]) * f[2]);
                this.depths[i] = d;
                if (d < min) min = d;
                if (d > max) max = d;
            }
            // a counting sort on 16 bits of depth: the nearest first
            const scale = 65535 / Math.max(max - min, 1e-9);
            const counts = new Uint32Array(65536);
            for (let i = 0; i < n; i++) { const k = ((this.depths[i] - min) * scale) | 0; this.keys[i] = k; counts[k]++; }
            for (let k = 1; k < 65536; k++) counts[k] += counts[k - 1];
            for (let i = n - 1; i >= 0; i--) this.order[--counts[this.keys[i]]] = i;
            const gl = this.gl;
            gl.bindBuffer(gl.ARRAY_BUFFER, this.orderBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, this.order, gl.DYNAMIC_DRAW);
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
            gl.enable(gl.BLEND);
            gl.blendFuncSeparate(gl.ONE_MINUS_DST_ALPHA, gl.ONE, gl.ONE_MINUS_DST_ALPHA, gl.ONE);
            gl.drawElementsInstanced(gl.TRIANGLES, 6, gl.UNSIGNED_SHORT, 0, s.count);
        }

        controls() {
            const c = this.canvas;
            const pointers = new Map();
            let last = null;
            c.addEventListener('contextmenu', (e) => e.preventDefault());
            c.addEventListener('pointerdown', (e) => {
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
                        this.yaw -= dx * 0.008;
                        this.pitch = Math.max(-1.5, Math.min(1.5, this.pitch + dy * 0.008));
                    }
                } else if (pointers.size === 2) {
                    const [a, b] = [...pointers.values()];
                    const now = { d: Math.hypot(a.x - b.x, a.y - b.y), x: (a.x + b.x) / 2, y: (a.y + b.y) / 2 };
                    if (last) {
                        this.distance *= last.d / Math.max(now.d, 1);
                        this.pan(now.x - last.x, now.y - last.y);
                    }
                    last = now;
                }
                this.redraw();
            });
            c.addEventListener('wheel', (e) => {
                e.preventDefault();
                this.distance *= Math.exp(e.deltaY * 0.001);
                this.redraw();
            }, { passive: false });
            c.addEventListener('dblclick', () => { if (this.splats) { this.show(this.splats, this.credit); } });
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
            try { new Viewer(root); } catch (e) { root.textContent = 'Der Viewer konnte nicht starten: ' + e.message; }
        });
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
