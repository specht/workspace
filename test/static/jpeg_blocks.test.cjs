// The Huffman stepper of »JPEG« reads one block of the photo again on its
// own (huffman_decode.js, from where jpeg_image.js says its bits are): for
// every block of 44-baseline.jpg it must find the coefficients the whole
// decoder found, with the stuffed bytes skipped and the DC before it.
const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const JI = require('../../src/static/include/jpeg_image.js');
const HD = require('../../src/static/include/huffman_decode.js');

const bytes = new Uint8Array(fs.readFileSync(path.join(__dirname, '../../src/content/jpeg/beispiele/44-baseline.jpg')));
const image = JI.decode(bytes);

function readAgain(c, mx, my, i) {
    const b = image.bits(c, mx, my, i);
    const reader = new HD.BitReader(b.bytes, { startBit: b.startBit, stuffed: true });
    const block = HD.decodeOneBlock(reader, HD.compileHuffmanTable(b.dc), HD.compileHuffmanTable(b.ac), b.previousDC);
    return { b, block, end: reader.bitPos };
}

test('the first Y block is the one on the page', () => {
    const { b, block } = readAgain(0, 0, 0, 0);
    assert.deepStrictEqual(b.bytes.slice(0, 4), [0xf0, 0x7b, 0x9d, 0x26]);
    assert.strictEqual(b.startBit, 0);
    assert.strictEqual(block.dc, -120);
    assert.strictEqual(block.steps[1].decodedValue, 9);
});

test('every block read alone gives the decoder\'s coefficients and ends where it ends', () => {
    let stuffed = 0;
    for (let my = 0; my < image.mcusY; my++) {
        for (let mx = 0; mx < image.mcusX; mx++) {
            image.comps.forEach((comp, c) => {
                for (let i = 0; i < comp.h * comp.v; i++) {
                    const { b, block, end } = readAgain(c, mx, my, i);
                    const dequantised = HD.ZIGZAG_TO_NATURAL.map((natural, k) => [natural, block.coefficientsNatural[natural] * b.quant[k]]);
                    const expected = image.block(c, mx, my, i);
                    for (const [natural, value] of dequantised) assert.strictEqual(value, expected[natural], `${comp.name}${i} in MCU ${mx}, ${my}`);
                    assert.strictEqual(end, b.endBit, `${comp.name}${i} in MCU ${mx}, ${my} ends elsewhere`);
                    for (let k = 1; k < b.bytes.length; k++) if (b.bytes[k - 1] === 0xff && b.bytes[k] === 0) stuffed++;
                }
            });
        }
    }
    assert.ok(stuffed > 0, 'no block had a stuffed byte, so skipping it was not tested');
});

test('a code across a stuffed byte is marked in two pieces around it', () => {
    // 16 bits of code: four in the first byte, eight in 0xFF, four after its 0x00
    const steps = [{ huffman: { startBit: 4, endBit: 28, length: 16 }, valueBits: { length: 0 } }];
    const html = HD.stepsToMarkedHexdump([0x0f, 0xff, 0x00, 0xf0], steps, Infinity, [{ start: 16, end: 24, className: 'hd-other' }]);
    assert.match(html, /0000 <span class="m0">111111111111<\/span> <span class="hd-other">00000000<\/span> <span class="m0">1111<\/span> 0000/);
});
