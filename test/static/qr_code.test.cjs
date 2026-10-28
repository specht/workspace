// The parts of a QR code (src/static/include/qr_code.js) against codes from
// qrencode, read back with jsQR as the widget of »Prüfziffern« does: the
// layout counts the modules of every part, the format information and the
// codewords read back, the correction codewords are those Reed-Solomon
// computes, and damage() counts what a sticker turns.
const test = require('node:test');
const assert = require('node:assert');
const { execFileSync } = require('node:child_process');
const Q = require('../../src/static/include/qr_code.js');
const jsQR = require('../../src/static/include/jsqr/jsQR.js');

const TEXT = '4260111020434';

function hasQrencode() {
    try {
        execFileSync('qrencode', ['--version'], { stdio: 'ignore' });
        return true;
    } catch (e) {
        return false;
    }
}

function encode(level, version) {
    const out = execFileSync('qrencode', ['-v', String(version), '-l', level, '-m', '0', '-t', 'ASCII', TEXT]).toString();
    return out.trimEnd().split('\n').map((line) => {
        const row = [];
        for (let i = 0; i < line.length; i += 2) row.push(line[i] === '#' ? 1 : 0);
        return row;
    });
}

function decode(m) {
    const px = 8;
    const quiet = 4;
    const w = (m.length + 2 * quiet) * px;
    const data = new Uint8ClampedArray(w * w * 4).fill(255);
    m.forEach((row, r) => row.forEach((v, c) => {
        if (!v) return;
        for (let y = 0; y < px; y++) {
            for (let x = 0; x < px; x++) {
                const i = (((r + quiet) * px + y) * w + (c + quiet) * px + x) * 4;
                data[i] = data[i + 1] = data[i + 2] = 0;
            }
        }
    }));
    const result = jsQR(data, w, w, { inversionAttempts: 'dontInvert' });
    return result ? result.data : null;
}

const sticker = (m, r0, c0, h, w, value = 0) =>
    m.map((row, r) => row.map((v, c) => (r >= r0 && r < r0 + h && c >= c0 && c < c0 + w ? value : v)));

test('the layout of versions 1 and 2 has every part with its number of modules', () => {
    for (const [version, data, alignment] of [[1, 208, 0], [2, 359, 25]]) {
        const l = Q.layout(version);
        const counts = {};
        l.role.flat().forEach((role) => { counts[role] = (counts[role] || 0) + 1; });
        assert.strictEqual(counts.finder, 3 * 64);
        assert.strictEqual(counts.format, 31);
        assert.strictEqual(counts.data, data);
        assert.strictEqual(counts.alignment, alignment || undefined);
        const total = Q.BLOCKS[version].total;
        const bits = l.codeword.flat().filter((k) => k >= 0).length;
        assert.strictEqual(bits, total * 8, 'every codeword has eight modules');
        // codeword 0 starts at the bottom right, its highest bit first
        assert.strictEqual(l.codeword[l.size - 1][l.size - 1], 0);
        assert.strictEqual(l.bit[l.size - 1][l.size - 1], 7);
    }
});

test('format information, codewords and correction read back from qrencode', { skip: !hasQrencode() && 'no qrencode' }, () => {
    for (const version of [1, 2]) {
        for (const level of Q.LEVELS) {
            const m = encode(level, version);
            assert.strictEqual(m.length, Q.sizeOf(version));
            const format = Q.readFormat(m);
            assert.strictEqual(format.level, level);
            assert.strictEqual(format.distance, 0);
            const words = Q.codewords(m, format.mask);
            const cap = Q.capacity(version, level);
            assert.deepStrictEqual(Q.correction(words.slice(0, cap.data), cap.ec), words.slice(cap.data), `${version}-${level}`);
            // numeric mode (0001), 13 digits in ten bits: 0001 0000001101 …
            assert.strictEqual(words[0], 0x10);
            assert.strictEqual(words[1] >> 2, 0b001101);
            assert.strictEqual(decode(m), TEXT);
        }
    }
});

test('the levels of version 2 repair half their correction codewords', () => {
    assert.deepStrictEqual(Q.LEVELS.map((level) => Q.capacity(2, level).repairs), [5, 8, 11, 14]);
    assert.deepStrictEqual(Q.LEVELS.map((level) => Q.capacity(2, level).data), [34, 28, 22, 16]);
});

test('a format copy with up to three wrong bits still reads, the second copy stands in', { skip: !hasQrencode() && 'no qrencode' }, () => {
    const m = encode('Q', 2);
    const want = Q.readFormat(m);
    const places = Q.formatPlaces(m.length);
    const broken = m.map((row) => row.slice());
    for (const [r, c] of places.first) broken[r][c] = 0;   // the first copy gone
    assert.deepStrictEqual(Q.readFormat(broken), want);
});

test('the stickers of the page: A over a finder pattern, B over codewords at the right edge', { skip: !hasQrencode() && 'no qrencode' }, () => {
    const m = encode('M', 2);
    const a = sticker(m, 1, 1, 5, 5);
    const b = sticker(m, 9, 20, 5, 5);
    const da = Q.damage(m, a, 'M');
    assert.deepStrictEqual(da.codewords, []);
    assert.deepStrictEqual(da.finder, ['oben links']);
    assert.strictEqual(decode(a), null);
    const db = Q.damage(m, b, 'M');
    assert.strictEqual(db.codewords.length, 5);
    assert.deepStrictEqual([db.finder, db.timing, db.alignment, db.format], [[], false, false, []]);
    assert.strictEqual(decode(b), TEXT);
    // at L the same sticker damages six codewords, one more than L repairs
    const l = encode('L', 2);
    const dl = Q.damage(l, sticker(l, 9, 20, 5, 5), 'L');
    assert.strictEqual(dl.codewords.length, 6);
    assert.strictEqual(dl.capacity.repairs, 5);
    assert.strictEqual(decode(sticker(l, 9, 20, 5, 5)), null);
});

test('more damaged codewords than a level repairs do not read', { skip: !hasQrencode() && 'no qrencode' }, () => {
    for (const level of Q.LEVELS) {
        const m = encode(level, 2);
        const stripe = sticker(m, 11, 0, 3, 25);
        const d = Q.damage(m, stripe, level);
        assert.ok(d.codewords.length > d.capacity.repairs, level);
        assert.strictEqual(decode(stripe), null, level);
    }
});
