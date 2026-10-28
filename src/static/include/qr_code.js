// The parts of a small QR code (»Prüfziffern«, src/content/check-digits):
// which module is a finder pattern, a timing line, the alignment pattern,
// the format information, and which bit of which codeword every other
// module carries, as ISO/IEC 18004 places them. The page's widget
// (qr_damage.js) uses it to say what a painted spot has damaged, the
// generator of the page's pictures (src/scripts/qr-code.cjs) to colour the
// parts. Only versions 1 and 2 (21 and 25 modules) with their single block
// of codewords; the matrices come from qrencode, the decoding on the page
// from jsQR.
//
// A matrix is an array of rows, each an array of 0 (light) and 1 (dark),
// without the quiet zone. Runs without a page: test/static/qr_code.test.cjs.
(function (root, factory) {
    'use strict';
    const engine = factory();
    if (typeof module !== 'undefined' && module.exports) module.exports = engine;
    else root.QrCode = engine;
}(typeof self !== 'undefined' ? self : this, function () {
    'use strict';

    // codewords in all and correction codewords per level (ISO/IEC 18004,
    // table 9)
    const BLOCKS = {
        1: { total: 26, ec: { L: 7, M: 10, Q: 13, H: 17 } },
        2: { total: 44, ec: { L: 10, M: 16, Q: 22, H: 28 } },
    };
    const LEVEL_BITS = { L: 1, M: 0, Q: 3, H: 2 };
    const LEVELS = ['L', 'M', 'Q', 'H'];

    const sizeOf = (version) => 17 + 4 * version;

    // the 15 bits of the format information for a level and a mask: five bits
    // of data, ten of a BCH code over them, XORed with a fixed pattern
    function formatBits(level, mask) {
        const data = (LEVEL_BITS[level] << 3) | mask;
        let rem = data;
        for (let i = 0; i < 10; i++) rem = (rem << 1) ^ ((rem >>> 9) * 0x537);
        return ((data << 10) | (rem & 0x3ff)) ^ 0x5412;
    }

    // where bit i of the format information stands, in the two copies
    // ([row, column]; bit 0 is the lowest)
    function formatPlaces(size) {
        const first = [];
        const second = [];
        for (let i = 0; i < 15; i++) {
            if (i <= 5) first.push([i, 8]);
            else if (i === 6) first.push([7, 8]);
            else if (i === 7) first.push([8, 8]);
            else if (i === 8) first.push([8, 7]);
            else first.push([8, 14 - i]);
            if (i < 8) second.push([8, size - 1 - i]);
            else second.push([size - 15 + i, 8]);
        }
        return { first, second };
    }

    // whether a mask turns the module in row r and column c
    function maskHit(mask, r, c) {
        switch (mask) {
            case 0: return (r + c) % 2 === 0;
            case 1: return r % 2 === 0;
            case 2: return c % 3 === 0;
            case 3: return (r + c) % 3 === 0;
            case 4: return (Math.floor(r / 2) + Math.floor(c / 3)) % 2 === 0;
            case 5: return ((r * c) % 2) + ((r * c) % 3) === 0;
            case 6: return (((r * c) % 2) + ((r * c) % 3)) % 2 === 0;
            case 7: return (((r + c) % 2) + ((r * c) % 3)) % 2 === 0;
            default: throw new Error(`no mask ${mask}`);
        }
    }

    // The role of every module: 'finder' (the three squares with their light
    // border), 'timing', 'alignment', 'format' (with the one module that is
    // always dark), or 'data'; for 'data' the codeword (counted from 0) and
    // the bit in it (7 is the highest), codeword -1 for the remainder bits
    // that carry nothing.
    function layout(version) {
        if (!BLOCKS[version]) throw new Error(`version ${version} is not supported`);
        const size = sizeOf(version);
        const role = Array.from({ length: size }, () => new Array(size).fill(null));
        const set = (r, c, what) => { if (r >= 0 && c >= 0 && r < size && c < size) role[r][c] = what; };
        for (const [r0, c0] of [[0, 0], [0, size - 7], [size - 7, 0]]) {
            for (let r = r0 - 1; r <= r0 + 7; r++) for (let c = c0 - 1; c <= c0 + 7; c++) set(r, c, 'finder');
        }
        for (let i = 8; i < size - 8; i++) { set(6, i, 'timing'); set(i, 6, 'timing'); }
        if (version >= 2) {
            const at = size - 7;
            for (let r = at - 2; r <= at + 2; r++) for (let c = at - 2; c <= at + 2; c++) set(r, c, 'alignment');
        }
        const places = formatPlaces(size);
        for (const [r, c] of places.first.concat(places.second)) set(r, c, 'format');
        set(size - 8, 8, 'format');               // the module that is always dark
        const codeword = Array.from({ length: size }, () => new Array(size).fill(-1));
        const bit = Array.from({ length: size }, () => new Array(size).fill(-1));
        const order = [];                          // the data modules in the order they are filled
        const total = BLOCKS[version].total;
        let i = 0;
        for (let right = size - 1; right >= 1; right -= 2) {
            if (right === 6) right = 5;            // the vertical timing line is skipped
            const upward = ((right + 1) & 2) === 0;
            for (let v = 0; v < size; v++) {
                const r = upward ? size - 1 - v : v;
                for (let j = 0; j < 2; j++) {
                    const c = right - j;
                    if (role[r][c] !== null) continue;
                    role[r][c] = 'data';
                    if (i < total * 8) {
                        codeword[r][c] = i >> 3;
                        bit[r][c] = 7 - (i & 7);
                    }
                    order.push([r, c]);
                    i++;
                }
            }
        }
        return { version, size, role, codeword, bit, order, total };
    }

    const versionOf = (matrix) => (matrix.length - 17) / 4;

    // The level and mask the format information gives, from whichever copy
    // is nearer to a valid one (at most 3 bits off), or null.
    function readFormat(matrix) {
        const size = matrix.length;
        const places = formatPlaces(size);
        const read = (list) => list.reduce((bits, [r, c], i) => bits | (matrix[r][c] << i), 0);
        const copies = [read(places.first), read(places.second)];
        let best = null;
        for (const level of LEVELS) {
            for (let mask = 0; mask < 8; mask++) {
                const want = formatBits(level, mask);
                for (const got of copies) {
                    let x = want ^ got;
                    let distance = 0;
                    while (x) { distance += x & 1; x >>>= 1; }
                    if (!best || distance < best.distance) best = { level, mask, distance };
                }
            }
        }
        return best && best.distance <= 3 ? best : null;
    }

    // the codewords as bytes, with the mask taken off
    function codewords(matrix, mask, lay) {
        const l = lay || layout(versionOf(matrix));
        const bytes = new Array(l.total).fill(0);
        for (let r = 0; r < l.size; r++) {
            for (let c = 0; c < l.size; c++) {
                const k = l.codeword[r][c];
                if (k < 0) continue;
                const value = matrix[r][c] ^ (maskHit(mask, r, c) ? 1 : 0);
                bytes[k] |= value << l.bit[r][c];
            }
        }
        return bytes;
    }

    // How many codewords a level gives to data and to correction, and how
    // many damaged codewords Reed-Solomon can repair: half the correction
    // codewords. The norm advises decoders of the smallest codes to repair
    // a few less at L (p in table 9, against misreading); jsQR repairs all
    // it can, and so does the page.
    function capacity(version, level) {
        const b = BLOCKS[version];
        const ec = b.ec[level];
        return { total: b.total, data: b.total - ec, ec, repairs: ec >> 1 };
    }

    // What a painted copy has changed against the original: the codewords
    // with at least one module turned (data and correction apart), and which
    // fixed parts have a turned module.
    function damage(original, painted, level) {
        const version = versionOf(original);
        const l = layout(version);
        const cap = capacity(version, level);
        const words = new Set();
        const parts = { finder: new Set(), timing: false, alignment: false, format: new Set() };
        const places = formatPlaces(l.size);
        const formatCopy = (r, c) => {
            if (places.first.some(([a, b]) => a === r && b === c)) return 1;
            return 2;
        };
        for (let r = 0; r < l.size; r++) {
            for (let c = 0; c < l.size; c++) {
                if (original[r][c] === painted[r][c]) continue;
                const role = l.role[r][c];
                if (role === 'data' && l.codeword[r][c] >= 0) words.add(l.codeword[r][c]);
                else if (role === 'finder') parts.finder.add(r < 9 && c < 9 ? 'oben links' : (r < 9 ? 'oben rechts' : 'unten links'));
                else if (role === 'timing') parts.timing = true;
                else if (role === 'alignment') parts.alignment = true;
                else if (role === 'format') parts.format.add(formatCopy(r, c));
            }
        }
        const list = [...words].sort((a, b) => a - b);
        return {
            codewords: list,
            data: list.filter((k) => k < cap.data).length,
            ec: list.filter((k) => k >= cap.data).length,
            capacity: cap,
            finder: [...parts.finder],
            timing: parts.timing,
            alignment: parts.alignment,
            format: [...parts.format].sort(),
        };
    }

    // a matrix from rows of 0 and 1 (qrencode -t ASCII turned into digits),
    // and back
    const parse = (text) => text.trim().split(/\s+/).map((row) => [...row].map(Number));
    const format = (matrix) => matrix.map((row) => row.join('')).join(' ');

    // Reed-Solomon over GF(256) as in ISO/IEC 18004 (x^8 + x^4 + x^3 + x^2 + 1):
    // the correction codewords for data codewords. Only for the tests and
    // the picture's generator; the page decodes with jsQR.
    function correction(data, ec) {
        const exp = new Array(512);
        const log = new Array(256);
        let x = 1;
        for (let i = 0; i < 255; i++) {
            exp[i] = x;
            log[x] = i;
            x <<= 1;
            if (x & 0x100) x ^= 0x11d;
        }
        for (let i = 255; i < 512; i++) exp[i] = exp[i - 255];
        const mul = (a, b) => (a && b ? exp[log[a] + log[b]] : 0);
        let gen = [1];
        for (let i = 0; i < ec; i++) {
            const next = new Array(gen.length + 1).fill(0);
            gen.forEach((g, j) => { next[j] ^= g; next[j + 1] ^= mul(g, exp[i]); });
            gen = next;
        }
        const rem = new Array(ec).fill(0);
        for (const d of data) {
            const factor = d ^ rem.shift();
            rem.push(0);
            for (let j = 0; j < ec; j++) rem[j] ^= mul(gen[j + 1], factor);
        }
        return rem;
    }

    return { BLOCKS, LEVELS, sizeOf, formatBits, formatPlaces, maskHit, layout, readFormat, codewords, capacity, damage, parse, format, correction };
}));
