// The Gesang clip tuned as the autotune widget does it, with its own code
// (track, tuneFactors and psola from effect_widget.js), for the card
// picture (teaser.py): reads 32-bit floats at 44.1 kHz, writes them tuned.
//
//   node autotune.js effect_widget.js in.f32 out.f32 flat|note|mirror
const fs = require('fs');
const [widget, input, output, target] = process.argv.slice(2);
const source = fs.readFileSync(widget, 'utf8');

// a function of the widget, from its name to its closing brace
function grab(name) {
    const start = source.indexOf(`    function ${name}(`);
    if (start < 0) throw new Error(`no function ${name}`);
    let depth = 0;
    for (let i = source.indexOf('{', start); i < source.length; i++) {
        if (source[i] === '{') depth++;
        else if (source[i] === '}' && --depth === 0) return source.slice(start, i + 1);
    }
    throw new Error(`${name} does not end`);
}
const code = ['const SIZE = 2048, HOP = 512;',
    'const blockAt = (c, n) => Math.min(n - 1, Math.max(0, Math.floor((c - SIZE / 2) / HOP)));',
    grab('track'), grab('bendLag'), grab('tuneFactors'), grab('psola'),
    'return { track, tuneFactors, psola };'].join('\n');
const { track, tuneFactors, psola } = new Function(code)();

const bytes = fs.readFileSync(input);
const x = new Float32Array(bytes.buffer.slice(bytes.byteOffset, bytes.byteOffset + bytes.length));
const rate = 44100;
const found = track(x, rate);
const y = psola(x, rate, tuneFactors(found, rate, { target, speed: 0, transpose: 0 }), found);
fs.writeFileSync(output, Buffer.from(Float32Array.from(y).buffer));
