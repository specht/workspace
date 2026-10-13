// scene-dump: what is where in an Anaglyph scene (check type scene,
// docs/aufgaben.md). The server runs this with `node -e` in the student's
// Workspace, as the student, so the student's scene and its expressions
// (t * 30, …) are only ever evaluated there.
//
// It loads the student's own viewer (anaglyph/include/script.js) with small
// stand-ins for the browser, so it reads a scene exactly like the viewer does,
// then follows the viewer's renderScene: push/pop and transforms, without
// drawing.
//
//   node -e "$(cat scene_dump.js)" <script.js> <scene.ini> <t1,t2,…>
//
// Prints JSON: { errors: [...], times: [...], objects: [{ line, kind, shape,
// name, model, text, at: [{ t, pos, scale, rot }] }] }
const fs = require('fs');
const vm = require('vm');

const [scriptPath, scenePath, timesArg] = process.argv.slice(1);
const times = (timesArg || '0').split(',').map(Number);
const out = { errors: [], times, objects: [] };

function finish() {
    process.stdout.write(JSON.stringify(out));
}

let source;
try {
    source = fs.readFileSync(scenePath, 'utf8');
} catch (e) {
    out.errors.push(`Die Datei ${scenePath} gibt es nicht.`);
    out.missing = true;
    finish();
    process.exit(0);
}

const noop = () => {};
const element = { addEventListener: noop, style: {}, appendChild: noop };
const sandbox = {
    console: { log: noop, error: noop, warn: noop },
    window: { addEventListener: noop, location: { search: '', pathname: '/' }, history: { replaceState: noop } },
    document: {
        addEventListener: noop,
        querySelector: () => null,
        getElementById: () => element,
        createElement: () => ({ ...element }),
    },
    // Colour names are only checked for error messages; accept them all.
    Option: function () { this.style = { set color(v) { this._c = v; }, get color() { return this._c || ''; } }; },
    URLSearchParams,
};
vm.createContext(sandbox);

try {
    vm.runInContext(fs.readFileSync(scriptPath, 'utf8'), sandbox, { timeout: 2000, filename: 'script.js' });
} catch (e) {
    out.errors.push(`Der Viewer konnte nicht geladen werden: ${e.message}`);
    finish();
    process.exit(0);
}

let parsed;
try {
    sandbox.__source = source;
    parsed = vm.runInContext('parseSceneINI(__source)', sandbox, { timeout: 2000 });
} catch (e) {
    out.errors.push(`Die Szene konnte nicht gelesen werden: ${e.message}`);
    finish();
    process.exit(0);
}
out.errors.push(...(parsed.errors || []));

// 4x4 matrices, row-major, column vectors: p' = M p. Transforms multiply on
// the right, like p5's translate/rotate/scale.
const I = () => [1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1];
function mul(a, b) {
    const r = new Array(16).fill(0);
    for (let i = 0; i < 4; i++)
        for (let j = 0; j < 4; j++)
            for (let k = 0; k < 4; k++) r[i * 4 + j] += a[i * 4 + k] * b[k * 4 + j];
    return r;
}
const T = (x, y, z) => [1, 0, 0, x, 0, 1, 0, y, 0, 0, 1, z, 0, 0, 0, 1];
const S = (x, y, z) => [x, 0, 0, 0, 0, y, 0, 0, 0, 0, z, 0, 0, 0, 0, 1];
function RX(a) { const c = Math.cos(a), s = Math.sin(a); return [1, 0, 0, 0, 0, c, -s, 0, 0, s, c, 0, 0, 0, 0, 1]; }
function RY(a) { const c = Math.cos(a), s = Math.sin(a); return [c, 0, s, 0, 0, 1, 0, 0, -s, 0, c, 0, 0, 0, 0, 1]; }
function RZ(a) { const c = Math.cos(a), s = Math.sin(a); return [c, -s, 0, 0, s, c, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1]; }

function value(expr, t) {
    sandbox.__expr = String(expr);
    sandbox.__t = t;
    return Number(vm.runInContext('evalSceneValue(__expr, __t)', sandbox, { timeout: 500 }));
}

function apply(m, transforms, t) {
    for (const tr of transforms || []) {
        const v = tr.value.map((x) => value(x, t));
        if (tr.type === 'move') m = mul(m, T(v[0], v[1], v[2]));
        else if (tr.type === 'rotate') {
            m = mul(m, RX(v[0] / 180 * Math.PI));
            m = mul(m, RY(v[1] / 180 * Math.PI));
            m = mul(m, RZ(v[2] / 180 * Math.PI));
        } else if (tr.type === 'scale') m = mul(m, S(v[0], v[1], v[2]));
    }
    return m;
}

const round = (x) => Math.round(x * 100) / 100;
const objects = [];
const reported = new Set();

for (const t of times) {
    let m = I();
    const stack = [];
    let index = 0;
    for (const entry of parsed.objects) {
        try {
            if (entry.command === 'push') {
                stack.push(m);
                m = apply(m, entry.transform, t);
            }
            if (entry.shape || entry.model || entry.text !== undefined) {
                const w = apply(m, entry.transform, t);
                const col = (j) => Math.hypot(w[j], w[4 + j], w[8 + j]);
                const at = {
                    t,
                    pos: [round(w[3]), round(w[7]), round(w[11])],
                    scale: [round(col(0)), round(col(1)), round(col(2))],
                    rot_z: round(Math.atan2(w[4], w[0]) / Math.PI * 180),
                };
                if (!objects[index]) {
                    objects[index] = {
                        line: entry._lineStart,
                        kind: entry.shape ? 'shape' : entry.model ? 'model' : 'text',
                        shape: entry.shape || null,
                        model: entry.model || null,
                        text: entry.text === undefined ? null : String(entry.text),
                        name: entry.name === undefined ? null : String(entry.name),
                        at: [],
                    };
                }
                objects[index].at.push(at);
                index++;
            }
            if (entry.command === 'pop') {
                m = stack.length > 0 ? stack.pop() : I();
            }
        } catch (e) {
            const message = `Zeile ${entry._lineStart}: ${e.message}`;
            if (!reported.has(message)) {
                reported.add(message);
                out.errors.push(message);
            }
        }
    }
}
out.objects = objects.filter(Boolean);
finish();
