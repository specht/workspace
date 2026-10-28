// Runs a turtle program in the browser (docs/plan-katas.md, »Turtle«): a
// module worker for one language, so that »Stopp« can end a program at
// once (terminate) and the page stays responsive. Python is Pyodide, Ruby
// is ruby.wasm (fetch-turtle-runtimes.sh), JavaScript runs as it is. The
// commands of each language (turtle.rb, turtle.py, turtle.js) hand every
// command to host() here.
//
// In: { type: 'prepare', language } to load a runtime ahead, and
// { type: 'run', language, code, runs, speed }. runs is a list: null
// runs the program, a text (»square(50)«) runs the program without drawing
// and then the call, each from the start. speed 0 sends everything at
// once; any other speed lets each line take as long as it takes at PACE,
// so that the page draws while the program runs, endless ones until they
// are stopped, at whichever speed it shows them.
//
// Out: { type: 'loading' } while a runtime loads, then per run
// { type: 'start', index }, { type: 'items', list } with commands
// [command, value, line] and output ['out', text] in their order,
// { type: 'error', index, message, line } or nothing, { type: 'end', index };
// at last { type: 'done' }. { type: 'limit' } and the worker closes itself
// after MAX_COMMANDS commands or MAX_OUTPUT characters of output.

const BASE = '/include/turtle/';
// the page's version of these files (turtle_canvas.js starts the worker
// with it), so that a cached turtle.rb never meets a newer worker
const VERSION = self.location.search;
const PYODIDE = BASE + 'runtime/pyodide-314.0.7/';
const RUBY = BASE + 'runtime/ruby-2.10.1/';
const MAX_COMMANDS = 200000;
const MAX_OUTPUT = 100000;
const MAX_WIDTH = 50;
// places remembered with push at once
const MAX_DEPTH = 10000;
// steps per second: faster than the page draws at »Schnell« even on paper
// zoomed far out (turtle_canvas.js), so that »Sofort« in the middle of a
// run draws a fern in a few seconds; an endless program still runs for a
// while before MAX_COMMANDS. Turns take no time, as on the page.
const PACE = 60000;

let colors = null;
let depth = 0;
let pace = null;
let due = 0;
let quiet = false;
let count = 0;
let printed = 0;
let batch = [];
let lastFlush = 0;
let runIndex = 0;

function flush() {
    if (batch.length > 0) self.postMessage({ type: 'items', list: batch });
    batch = [];
    lastFlush = performance.now();
}

function stopAll() {
    flush();
    self.postMessage({ type: 'limit', commands: count > MAX_COMMANDS, index: runIndex });
    self.close();
}

// A colour as #rrggbb: a name, #rgb or #rrggbb, a hue (»hue:200«, color
// 200) or a mix (»mix:gold:red:0.25«), as TurtleWorld.color_hex on the
// server; null when it is none of them.
function colorHex(name) {
    const text = String(name).trim().toLowerCase();
    const hue = /^hue:(.+)$/.exec(text);
    if (hue) return Number.isFinite(Number(hue[1])) ? hueHex(Number(hue[1])) : null;
    const mix = /^mix:([^:]+):([^:]+):(.+)$/.exec(text);
    if (mix) {
        const a = colorHex(mix[1]);
        const b = colorHex(mix[2]);
        const t = Number(mix[3]);
        return a && b && Number.isFinite(t) ? mixHex(a, b, t) : null;
    }
    if (/^#[0-9a-f]{6}$/.test(text)) return text;
    if (/^#[0-9a-f]{3}$/.test(text)) return '#' + text[1] + text[1] + text[2] + text[2] + text[3] + text[3];
    return colors[text] || null;
}

function hex(r, g, b) {
    return '#' + [r, g, b].map((c) => Math.round(Math.min(255, Math.max(0, c))).toString(16).padStart(2, '0')).join('');
}

// the hue on the colour wheel, as bright as it stays readable on the
// paper: saturation 0.8, lightness 0.47 (HSL)
function hueHex(degrees) {
    const h = ((degrees % 360) + 360) % 360;
    const c = (1 - Math.abs(2 * 0.47 - 1)) * 0.8;
    const x = c * (1 - Math.abs((h / 60) % 2 - 1));
    const m = 0.47 - c / 2;
    const [r, g, b] = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x] : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
    return hex((r + m) * 255, (g + m) * 255, (b + m) * 255);
}

// from a to b, t of the way (0 to 1)
function mixHex(a, b, t) {
    const amount = Math.min(1, Math.max(0, t));
    const part = (h, i) => parseInt(h.slice(1 + 2 * i, 3 + 2 * i), 16);
    return hex(...[0, 1, 2].map((i) => part(a, i) + (part(b, i) - part(a, i)) * amount));
}

// The one way in for every command of every language: null when it is
// done, otherwise why not. line: the student's line, or null.
function host(command, value, line) {
    if (quiet) return null;
    if (command === 'color') {
        const found = colorHex(value);
        if (!found) {
            // in a mix, the name it does not know
            const mix = /^mix:([^:]+):([^:]+):/.exec(String(value));
            const name = mix ? (colorHex(mix[1]) ? mix[2] : mix[1]) : value;
            return 'color kennt »' + name + '« nicht: Nimm einen englischen Farbnamen wie "red", "orange" oder "darkgreen" oder eine Farbe wie "#ff8800".';
        }
        value = found;
    }
    if (command === 'push') {
        if (depth >= MAX_DEPTH) return 'push: Die Schildkröte merkt sich schon ' + MAX_DEPTH + ' Stellen. Fehlt irgendwo ein pop?';
        depth += 1;
    }
    if (command === 'pop') {
        if (depth === 0) return 'pop: Die Schildkröte hat sich keine Stelle gemerkt. Vor jedem pop steht ein push.';
        depth -= 1;
    }
    if (command === 'width' && !(value > 0 && value <= MAX_WIDTH)) return 'width braucht eine Zahl über 0 und höchstens ' + MAX_WIDTH + ', nicht ' + value + '.';
    count += 1;
    if (count > MAX_COMMANDS) stopAll();
    if (pace) {
        // wait until the turtle is done with the line before; the page
        // has what came before it meanwhile
        if (performance.now() < due) flush();
        while (performance.now() < due) { /* the turtle is still on its way */ }
        batch.push([command, value, line === undefined ? null : line]);
        if (performance.now() - lastFlush > 15) flush();
        if (command === 'forward' || command === 'back') due = Math.max(due, performance.now()) + Math.abs(value) / pace * 1000;
    } else {
        batch.push([command, value, line === undefined ? null : line]);
        if (batch.length >= 4000 || performance.now() - lastFlush > 40) flush();
    }
    return null;
}

function out(text) {
    if (quiet) return;
    printed += text.length;
    batch.push(['out', text]);
    if (printed > MAX_OUTPUT) stopAll();
    if (pace && performance.now() - lastFlush > 15) flush();
}

async function text(path) {
    const response = await fetch(path + VERSION);
    if (!response.ok) throw new Error(path + ': ' + response.status);
    return response.text();
}

// ----- JavaScript -----

let jsCommands = null;
let jsOffset = 0;

async function prepareJs() {
    if (jsCommands) return;
    const turtleCommands = new Function(await text(BASE + 'turtle.js') + '\nreturn turtleCommands;')();
    // where a line of a new Function's body shows in a stack
    try {
        new Function('throw new Error("probe");\n//# sourceURL=probe.js')();
    } catch (e) {
        const m = String(e.stack).match(/probe\.js:(\d+)/);
        jsOffset = m ? Number(m[1]) - 1 : 0;
    }
    jsCommands = turtleCommands((command, value) => host(command, value, jsLine()));
}

let jsLines = 0;
function jsLine(stack) {
    const m = String(stack || new Error().stack).match(/kata\.js:(\d+)/);
    if (!m) return null;
    const line = Number(m[1]) - jsOffset;
    return line >= 1 && line <= jsLines ? line : null;
}

function runJs(code, call) {
    const names = Object.keys(jsCommands);
    jsLines = code.split('\n').length;
    const log = (...parts) => out(parts.map((p) => typeof p === 'string' ? p : JSON.stringify(p)).join(' ') + '\n');
    const body = code + '\n;' + (call === null ? '' : '__loaded();\n' + call + ';') + '\n//# sourceURL=kata.js';
    try {
        const fn = new Function(...names, 'console', '__loaded', body);
        fn(...names.map((n) => jsCommands[n]), { log, error: log, warn: log, info: log }, () => { quiet = false; });
        return null;
    } catch (e) {
        if (e instanceof RangeError && /call stack/i.test(e.message)) return { message: 'RangeError: Maximum call stack size exceeded', line: jsLine(e.stack) };
        return { message: (e && e.name ? e.name + ': ' : '') + (e && e.message !== undefined ? e.message : String(e)), line: jsLine(e && e.stack) };
    }
}

// ----- Python -----

let pyodide = null;
let pyRun = null;

async function preparePython() {
    if (pyodide) return;
    self.postMessage({ type: 'loading', language: 'python' });
    const { loadPyodide } = await import(PYODIDE + 'pyodide.mjs');
    pyodide = await loadPyodide({ indexURL: PYODIDE });
    pyodide.setStdout({ write: (bytes) => { out(new TextDecoder().decode(bytes)); return bytes.length; } });
    pyodide.setStderr({ write: (bytes) => { out(new TextDecoder().decode(bytes)); return bytes.length; } });
    // JavaScript's null is not None in Python (it is JsNull), undefined is
    pyodide.globals.set('_host', (command, value, line) => host(command, value, line === undefined ? null : line) || undefined);
    pyodide.globals.set('_loaded', () => { quiet = false; });
    pyodide.globals.set('_TURTLE', await text(BASE + 'turtle.py'));
    pyodide.runPython(`
import json, traceback

def _turtle_run(code, call):
    ns = {'__name__': '__main__', '_turtle_host': _host}
    exec(compile(_TURTLE, 'turtle.py', 'exec'), ns)
    try:
        exec(compile(code, 'kata.py', 'exec'), ns)
        if call is not None:
            _loaded()
            exec(compile(call, 'call', 'exec'), ns)
    except SystemExit:
        pass
    except SyntaxError as e:
        line = e.lineno if e.filename == 'kata.py' else None
        return json.dumps({'message': f'{type(e).__name__}: {e.msg}', 'line': line})
    except BaseException as e:
        line = None
        for frame, lineno in traceback.walk_tb(e.__traceback__):
            if frame.f_code.co_filename == 'kata.py':
                line = lineno
        return json.dumps({'message': f'{type(e).__name__}: {e}', 'line': line})
    return None
`);
    pyRun = pyodide.globals.get('_turtle_run');
}

function runPython(code, call) {
    const answer = pyRun(code, call === null ? undefined : call);
    return answer ? JSON.parse(answer) : null;
}

// ----- Ruby -----

let rubyModule = null;
let rubyParts = null;
let rubyTurtle = null;
let rubyError = null;

async function prepareRuby() {
    if (rubyModule) return;
    self.postMessage({ type: 'loading', language: 'ruby' });
    const [compiled, vm, console_, shim, source] = await Promise.all([
        WebAssembly.compileStreaming(fetch(RUBY + 'ruby.wasm')),
        import(RUBY + 'wasm-wasi/vm.js'),
        import(RUBY + 'wasm-wasi/console.js'),
        import(RUBY + 'browser_wasi_shim/index.js'),
        text(BASE + 'turtle.rb'),
    ]);
    rubyModule = compiled;
    rubyParts = { RubyVM: vm.RubyVM, consolePrinter: console_.consolePrinter, shim };
    rubyTurtle = source;
    self.__turtleHost = (command, value, line) => host(command, value, line === undefined ? null : line);
    self.__turtleLoaded = () => { quiet = false; };
    self.__turtleError = (message, line) => { rubyError = { message: String(message), line: line === undefined ? null : line }; };
}

// A new VM for every run: nothing of an earlier run, not even a method,
// is left over.
async function runRuby(code, call) {
    const { RubyVM, consolePrinter, shim } = rubyParts;
    const fds = [new shim.OpenFile(new shim.File([])), new shim.OpenFile(new shim.File([])), new shim.OpenFile(new shim.File([])),
        new shim.PreopenDirectory('/', new Map())];
    const wasi = new shim.WASI([], [], fds, { debug: false });
    const printer = consolePrinter({ stdout: out, stderr: out });
    const { vm } = await RubyVM.instantiateModule({
        module: rubyModule, wasip1: wasi,
        // without the standard library: no RubyGems and no error helpers,
        // which would otherwise say so on stderr at every start
        args: ['ruby.wasm', '--disable=gems,error_highlight,did_you_mean,syntax_suggest', '-EUTF-8', '-e_=0'],
        addToImports: (imports) => printer.addToImports(imports),
        setMemory: (memory) => printer.setMemory(memory),
    });
    rubyError = null;
    // the code as a JS value, not inside a Ruby string, where #{…} would run
    self.__turtleCode = code;
    self.__turtleCall = call;
    vm.eval(`
require "js"
def __turtle_host(command, value, line)
    answer = JS.global.call(:__turtleHost, command, value, line)
    answer.typeof == "string" ? answer.to_s : nil
end
${rubyTurtle}
`);
    vm.eval(`
__code = JS.global[:__turtleCode].to_s
__call = JS.global[:__turtleCall].typeof == "string" ? JS.global[:__turtleCall].to_s : nil
begin
    TOPLEVEL_BINDING.eval(__code, 'kata.rb', 1)
    if __call
        JS.global.call(:__turtleLoaded)
        TOPLEVEL_BINDING.eval(__call, 'call', 1)
    end
rescue SystemExit
rescue SyntaxError => e
    text = e.message.gsub(/\\e\\[[0-9;]*m/, '')
    line = text[/kata\\.rb:(\\d+)/, 1]
    reason = text[/\\^~*\\s+(.+)$/, 1] || text.lines.first.to_s.strip
    JS.global.call(:__turtleError, "SyntaxError: #{reason}", line ? line.to_i : nil)
rescue Exception => e
    line = e.backtrace_locations&.find { |l| l.path == 'kata.rb' }&.lineno
    JS.global.call(:__turtleError, "#{e.class}: #{e.message}", line)
end
`);
    return rubyError;
}

// ----- the runs -----

const PREPARE = { javascript: prepareJs, python: preparePython, ruby: prepareRuby };
const preparing = {};

// Each runtime loads once, even when »prepare« and a run ask at once.
function prepare(language) {
    if (!preparing[language]) {
        preparing[language] = (async () => {
            if (!colors) colors = JSON.parse(await text(BASE + 'colors.json'));
            await PREPARE[language]();
        })();
        preparing[language].catch(() => { delete preparing[language]; });
    }
    return preparing[language];
}
const RUN = { javascript: runJs, python: runPython, ruby: runRuby };

self.onmessage = async (event) => {
    const message = event.data;
    if (message.type === 'prepare') {
        // loaded before the first run, while the student is still typing
        try {
            await prepare(message.language);
            self.postMessage({ type: 'ready' });
        } catch (e) { /* the run will say so */ }
        return;
    }
    if (message.type !== 'run') return;
    try {
        await prepare(message.language);
    } catch (e) {
        self.postMessage({ type: 'error', index: 0, message: 'Die Laufzeit für ' + message.language + ' ließ sich nicht laden: ' + e.message, line: null, fatal: true });
        self.postMessage({ type: 'done' });
        return;
    }
    pace = message.speed > 0 ? PACE : null;
    count = 0;
    printed = 0;
    for (let i = 0; i < message.runs.length; i++) {
        runIndex = i;
        const call = message.runs[i];
        quiet = call !== null;
        depth = 0;
        due = performance.now();
        self.postMessage({ type: 'start', index: i });
        let error = null;
        try {
            error = await RUN[message.language](message.code, call);
        } catch (e) {
            // the runtime itself gave up (a wasm stack that overflowed)
            error = { message: /stack|recursion/i.test(String(e && e.message)) ? 'Zu tiefe Rekursion: Die Funktion ruft sich zu oft selbst auf.' : String(e && e.message || e), line: null };
        }
        flush();
        if (error) self.postMessage({ type: 'error', index: i, message: error.message, line: error.line === undefined ? null : error.line });
        self.postMessage({ type: 'end', index: i });
    }
    self.postMessage({ type: 'done' });
};
