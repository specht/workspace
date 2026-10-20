// Shader examples on the page: every <div class='shader-lab'> holds a
// <textarea> with a GLSL function `vec3 color(vec2 p)`; p is the place on
// the surface in metres. The script draws it into a canvas next to it (WebGL 1,
// GLSL ES 1.00), again whenever the code changes, and shows compile errors
// below. A slider moves the surface further away: more metres per pixel.
(function () {
    'use strict';

    // shared by every example, shown on the page as well
    const LIBRARY = `
float hash(vec2 p) {
    vec3 q = fract(vec3(p.xyx) * 0.1031);
    q += dot(q, q.yzx + 33.33);
    return fract((q.x + q.y) * q.z);
}
float noise(vec2 p) {
    vec2 i = floor(p), f = fract(p);
    vec2 u = f * f * (3.0 - 2.0 * f);
    return mix(mix(hash(i), hash(i + vec2(1.0, 0.0)), u.x),
               mix(hash(i + vec2(0.0, 1.0)), hash(i + vec2(1.0, 1.0)), u.x), u.y);
}
float turbulence(vec2 p) {
    float sum = 0.0, size = 1.0;
    for (int i = 0; i < 4; i++) {
        sum += noise(p * size) / size;
        size *= 2.0;
    }
    return sum / 1.875;
}
`;

    const HEADER = `#extension GL_OES_standard_derivatives : enable
precision highp float;
uniform vec2 resolution;
uniform float metres;
`;

    const FOOTER = `
void main() {
    vec2 p = (gl_FragCoord.xy - 0.5 * resolution) / resolution.y * metres;
    gl_FragColor = vec4(clamp(color(p), 0.0, 1.0), 1.0);
}
`;

    const VERTEX = `attribute vec2 corner;
void main() { gl_Position = vec4(corner, 0.0, 1.0); }
`;

    // GLSL in the classes Rouge gives a ```glsl block (kt, nf, nb, k, mf, mi,
    // c1, cm, cp, n, p), so the page's colours apply, light and dark
    const KEYWORDS = new Set('return if else for while do break continue discard in out inout uniform varying attribute const precision highp mediump lowp struct true false'.split(' '));
    const TYPES = new Set('void bool int float vec2 vec3 vec4 bvec2 bvec3 bvec4 ivec2 ivec3 ivec4 mat2 mat3 mat4 sampler2D samplerCube'.split(' '));
    const BUILTINS = new Set(('radians degrees sin cos tan asin acos atan pow exp log exp2 log2 sqrt inversesqrt abs sign floor ceil fract mod min max clamp mix step smoothstep '
        + 'length distance dot cross normalize faceforward reflect refract matrixCompMult lessThan lessThanEqual greaterThan greaterThanEqual equal notEqual any all not '
        + 'texture2D textureCube dFdx dFdy fwidth').split(' '));
    const escape = (text) => text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    function highlight(text) {
        const rules = [
            [/\/\/.*/y, () => 'c1'],
            [/\/\*[\s\S]*?(?:\*\/|$)/y, () => 'cm'],
            [/#.*/y, () => 'cp'],
            [/(?:\d+\.\d*|\.\d+)(?:[eE][+-]?\d+)?|\d+[eE][+-]?\d+/y, () => 'mf'],
            [/\d+/y, () => 'mi'],
            [/[A-Za-z_]\w*/y, (word, after, before) => (KEYWORDS.has(word) ? 'k' : TYPES.has(word) ? 'kt' : BUILTINS.has(word) ? 'nb'
                : before === 'kt' && /^\s*\(/.test(after) ? 'nf' : 'n')],
            [/[^\sA-Za-z_0-9]/y, () => 'p'],
            [/\s+/y, () => null],
        ];
        let out = '', i = 0, last = null;
        while (i < text.length) {
            for (const [re, token] of rules) {
                re.lastIndex = i;
                const m = re.exec(text);
                if (!m) continue;
                const kind = token(m[0], text.slice(i + m[0].length, i + m[0].length + 40), last);
                out += kind ? `<span class="${kind}">${escape(m[0])}</span>` : escape(m[0]);
                if (kind) last = kind;
                i += m[0].length;
                break;
            }
        }
        return out + '\n';   // a last empty line keeps the height of the textarea
    }

    const style = document.createElement('style');
    style.textContent = `
.shader-lab { display: grid; grid-template-columns: minmax(0, 20em) minmax(0, 1fr); gap: 0.75em 1.25em; margin: 1em 0 1.5em; align-items: start; }
.shader-lab canvas { width: 100%; aspect-ratio: 1; border-radius: 4px; background: #222; display: block; }
.shader-lab .code-input { min-width: 0; }
.shader-lab .code-input .code-input-text { width: 100%; }
.shader-lab label { display: flex; gap: 0.6em; align-items: center; font-size: 0.9em; }
.shader-lab input[type=range] { flex: 1; }
.shader-lab .shader-error { color: var(--bs-danger-text-emphasis, #b00020); font-size: 0.8em; white-space: pre-wrap; margin: 0; min-height: 1em; }
@media (max-width: 720px) { .shader-lab { grid-template-columns: minmax(0, 1fr); } }
`;
    document.head.append(style);

    function compile(gl, type, source) {
        const shader = gl.createShader(type);
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            const log = gl.getShaderInfoLog(shader);
            gl.deleteShader(shader);
            throw new Error(log);
        }
        return shader;
    }

    // error lines count from the user's code, not from what is put around it
    const offset = (HEADER + LIBRARY).split('\n').length - 1;
    const tidy = (log) => log.replace(/ERROR: 0:(\d+)/g, (_, line) => `Zeile ${Number(line) - offset}`);

    function setUp(lab) {
        const area = lab.querySelector('textarea');
        // wie die Code-Felder der Aufgaben (code_input.js): darunter ein <pre>
        // mit Farben, darüber das Textfeld mit unsichtbarer Schrift
        const box = document.createElement('div');
        box.className = 'code-input';
        const view = document.createElement('pre');
        view.className = 'code-input-view';
        view.setAttribute('aria-hidden', 'true');
        area.classList.add('code-input-text');
        area.parentNode.insertBefore(box, area);
        box.append(view, area);
        const paint = () => {
            view.innerHTML = highlight(area.value);
            area.style.height = 'auto';
            area.style.height = `${area.scrollHeight + 2}px`;
        };
        area.addEventListener('input', paint);
        const canvas = document.createElement('canvas');
        const error = document.createElement('div');
        error.className = 'shader-error';
        const column = document.createElement('div');
        column.append(canvas);
        const max = Number(lab.dataset.far ?? 0);
        let range = null;
        if (max > 0) {
            const label = document.createElement('label');
            label.textContent = 'näher';
            range = document.createElement('input');
            range.type = 'range';
            range.min = '0';
            range.max = '1000';
            range.value = '0';
            label.append(range, document.createTextNode('weiter weg'));
            column.append(label);
        }
        lab.prepend(column);
        lab.append(error);

        const gl = canvas.getContext('webgl', { antialias: false, preserveDrawingBuffer: true });
        if (!gl) {
            error.textContent = 'Dieser Browser kann kein WebGL.';
            return;
        }
        gl.getExtension('OES_standard_derivatives');
        const buffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([-1, -1, 1, -1, -1, 1, 1, 1]), gl.STATIC_DRAW);
        const vertex = compile(gl, gl.VERTEX_SHADER, VERTEX);
        let program = null;

        const near = Number(lab.dataset.metres ?? 2);
        const metres = () => (range ? near * Math.pow(max / near, Number(range.value) / 1000) : near);

        function draw() {
            if (!program) return;
            const size = Math.round(canvas.clientWidth * (window.devicePixelRatio || 1));
            if (canvas.width !== size) canvas.width = canvas.height = size;
            gl.viewport(0, 0, size, size);
            gl.useProgram(program);
            gl.uniform2f(gl.getUniformLocation(program, 'resolution'), size, size);
            gl.uniform1f(gl.getUniformLocation(program, 'metres'), metres());
            const corner = gl.getAttribLocation(program, 'corner');
            gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
            gl.enableVertexAttribArray(corner);
            gl.vertexAttribPointer(corner, 2, gl.FLOAT, false, 0, 0);
            gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
        }

        function build() {
            try {
                const fragment = compile(gl, gl.FRAGMENT_SHADER, HEADER + LIBRARY + area.value + FOOTER);
                const next = gl.createProgram();
                gl.attachShader(next, vertex);
                gl.attachShader(next, fragment);
                gl.linkProgram(next);
                if (!gl.getProgramParameter(next, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(next));
                if (program) gl.deleteProgram(program);
                program = next;
                error.textContent = '';
                draw();
            } catch (problem) {
                error.textContent = tidy(String(problem.message || problem)).trim();
            }
        }

        let pending = null;
        area.addEventListener('input', () => {
            clearTimeout(pending);
            pending = setTimeout(build, 300);
        });
        area.addEventListener('keydown', (event) => {
            if (event.key !== 'Tab') return;
            event.preventDefault();
            area.setRangeText('    ', area.selectionStart, area.selectionEnd, 'end');
            paint();
        });
        range?.addEventListener('input', draw);
        new ResizeObserver(draw).observe(canvas);
        new ResizeObserver(paint).observe(box);
        paint();
        build();
    }

    document.querySelectorAll('.shader-lab').forEach(setUp);
})();
