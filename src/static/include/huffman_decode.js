// ------------------------------------------------------------
// JPEG Huffman DCT step-by-step decoder
// ------------------------------------------------------------

// Standard JPEG zigzag order: zigzag index -> natural 8x8 index
const ZIGZAG_TO_NATURAL = [
    0, 1, 8, 16, 9, 2, 3, 10,
    17, 24, 32, 25, 18, 11, 4, 5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6, 7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63
];

// Convert a natural index 0..63 into row/col.
function naturalIndexToRowCol(naturalIndex) {
    return {
        row: Math.floor(naturalIndex / 8),
        col: naturalIndex % 8
    };
}

// Convert magnitude bits to signed JPEG value.
// Example:
// size=3, bits=101 => +5
// size=3, bits=010 => -5
function decodeJpegAmplitude(size, valueBitsAsNumber) {
    if (size === 0) return 0;

    const threshold = 1 << (size - 1);
    if (valueBitsAsNumber >= threshold) {
        return valueBitsAsNumber;
    }

    // JPEG negative reconstruction
    return valueBitsAsNumber - ((1 << size) - 1);
}

// Compile a Huffman table from { "bitstring": symbol, ... }
// into a Map for quick prefix matching.
function compileHuffmanTable(tableObj) {
    const map = new Map();
    for (const [bits, symbol] of Object.entries(tableObj)) {
        map.set(bits, symbol);
    }
    return map;
}

// Parse hex bytes string like "f0 7b 9d ..."
function parseHexBytes(hexString) {
    return hexString
        .trim()
        .split(/\s+/)
        .filter(Boolean)
        .map(h => parseInt(h, 16));
}

// Bit reader, MSB-first inside each byte.
class BitReader {
    constructor(bytes) {
        this.bytes = bytes;
        this.totalBits = bytes.length * 8;
        this.bitPos = 0;
    }

    eof() {
        return this.bitPos >= this.totalBits;
    }

    bitsRemaining() {
        return this.totalBits - this.bitPos;
    }

    readBit() {
        if (this.eof()) {
            throw new Error("Unexpected end of bitstream");
        }
        const byteIndex = this.bitPos >> 3;
        const bitIndexInByte = 7 - (this.bitPos & 7); // MSB first
        const bit = (this.bytes[byteIndex] >> bitIndexInByte) & 1;
        this.bitPos++;
        return bit;
    }

    readBits(n) {
        if (n < 0) throw new Error("Cannot read negative number of bits");
        if (this.bitsRemaining() < n) {
            throw new Error(`Not enough bits: wanted ${n}, have ${this.bitsRemaining()}`);
        }

        const start = this.bitPos;
        let value = 0;
        let bits = "";
        for (let i = 0; i < n; i++) {
            const b = this.readBit();
            value = (value << 1) | b;
            bits += b ? "1" : "0";
        }
        return {
            startBit: start,
            length: n,
            bits,
            value
        };
    }
}

// Decode one Huffman symbol by growing the prefix one bit at a time.
function readHuffmanSymbol(reader, compiledTable) {
    const startBit = reader.bitPos;
    let prefix = "";

    while (true) {
        if (reader.eof()) {
            throw new Error("Unexpected end of bitstream while reading Huffman code");
        }

        prefix += reader.readBit() ? "1" : "0";

        if (compiledTable.has(prefix)) {
            return {
                startBit,
                length: prefix.length,
                bits: prefix,
                symbol: compiledTable.get(prefix)
            };
        }
    }
}

// Convert a coefficient array in natural order into an 8x8 matrix.
function coefficientsToMatrix(coeffsNatural) {
    const m = [];
    for (let r = 0; r < 8; r++) {
        m.push(coeffsNatural.slice(r * 8, r * 8 + 8));
    }
    return m;
}

// Decode one 8x8 DCT block from the entropy-coded bitstream.
function decodeOneBlock(reader, dcTable, acTable, prevDC = 0) {
    const steps = [];
    const coeffs = new Array(64).fill(0);

    // -------------------------
    // DC coefficient
    // -------------------------
    const dcHuff = readHuffmanSymbol(reader, dcTable);
    const dcCategory = dcHuff.symbol;

    let dcValueInfo = {
        startBit: reader.bitPos,
        length: 0,
        bits: "",
        value: 0
    };

    let dcDiff = 0;
    if (dcCategory > 0) {
        dcValueInfo = reader.readBits(dcCategory);
        dcDiff = decodeJpegAmplitude(dcCategory, dcValueInfo.value);
    }

    const dcCoeff = prevDC + dcDiff;
    coeffs[0] = dcCoeff;

    steps.push({
        kind: "DC",
        huffman: {
            startBit: dcHuff.startBit,
            length: dcHuff.length,
            bits: dcHuff.bits,
            symbol: dcCategory
        },
        valueBits: {
            startBit: dcValueInfo.startBit,
            length: dcValueInfo.length,
            bits: dcValueInfo.bits,
            rawValue: dcValueInfo.value
        },
        decodedValue: dcDiff,
        coefficient: dcCoeff,
        coefficientIndex: 0,
        zigzagIndex: 0,
        naturalIndex: 0,
        matrixPosition: { row: 0, col: 0 },
        extra: {
            previousDC: prevDC,
            newDC: dcCoeff
        }
    });

    // -------------------------
    // AC coefficients
    // -------------------------
    let zigzagIndex = 1;

    while (zigzagIndex < 64) {
        const acHuff = readHuffmanSymbol(reader, acTable);
        const rs = acHuff.symbol;

        // EOB
        if (rs === 0x00) {
            steps.push({
                kind: "AC_EOB",
                huffman: {
                    startBit: acHuff.startBit,
                    length: acHuff.length,
                    bits: acHuff.bits,
                    symbol: rs
                },
                valueBits: {
                    startBit: reader.bitPos,
                    length: 0,
                    bits: "",
                    rawValue: 0
                },
                decodedValue: 0,
                coefficient: null,
                coefficientIndex: null,
                zigzagIndex: zigzagIndex,
                naturalIndex: null,
                matrixPosition: null,
                extra: {
                    run: 0,
                    size: 0,
                    meaning: "EOB"
                }
            });
            break;
        }

        // ZRL: 16 zeros
        if (rs === 0xF0) {
            const oldIndex = zigzagIndex;
            zigzagIndex += 16;
            if (zigzagIndex > 64) {
                throw new Error("ZRL moved beyond the end of block");
            }

            steps.push({
                kind: "AC_ZRL",
                huffman: {
                    startBit: acHuff.startBit,
                    length: acHuff.length,
                    bits: acHuff.bits,
                    symbol: rs
                },
                valueBits: {
                    startBit: reader.bitPos,
                    length: 0,
                    bits: "",
                    rawValue: 0
                },
                decodedValue: 0,
                coefficient: null,
                coefficientIndex: null,
                zigzagIndex: zigzagIndex,
                naturalIndex: null,
                matrixPosition: null,
                extra: {
                    run: 16,
                    size: 0,
                    rangeSkipped: [oldIndex, zigzagIndex - 1]
                }
            });
            continue;
        }

        const run = rs >> 4;
        const size = rs & 0x0F;

        zigzagIndex += run;
        if (zigzagIndex >= 64) {
            throw new Error(`AC run moved beyond block end: run=${run}, zigzagIndex=${zigzagIndex}`);
        }

        let acValueInfo = {
            startBit: reader.bitPos,
            length: 0,
            bits: "",
            value: 0
        };

        let coeffValue = 0;
        if (size > 0) {
            acValueInfo = reader.readBits(size);
            coeffValue = decodeJpegAmplitude(size, acValueInfo.value);
        }

        const naturalIndex = ZIGZAG_TO_NATURAL[zigzagIndex];
        const pos = naturalIndexToRowCol(naturalIndex);
        coeffs[naturalIndex] = coeffValue;

        steps.push({
            kind: "AC",
            huffman: {
                startBit: acHuff.startBit,
                length: acHuff.length,
                bits: acHuff.bits,
                symbol: rs
            },
            valueBits: {
                startBit: acValueInfo.startBit,
                length: acValueInfo.length,
                bits: acValueInfo.bits,
                rawValue: acValueInfo.value
            },
            decodedValue: coeffValue,
            coefficient: coeffValue,
            coefficientIndex: zigzagIndex,
            zigzagIndex: zigzagIndex,
            naturalIndex: naturalIndex,
            matrixPosition: pos,
            extra: {
                run,
                size
            }
        });

        zigzagIndex++;
    }

    return {
        steps,
        coefficientsNatural: coeffs,
        matrix: coefficientsToMatrix(coeffs),
        dc: dcCoeff
    };
}

// Decode blocks until the stream is exhausted or decoding fails.
// Useful for teaching and experimentation.
function decodeJpegDctStream(bytes, dcTableObj, acTableObj, options = {}) {
    const dcTable = compileHuffmanTable(dcTableObj);
    const acTable = compileHuffmanTable(acTableObj);
    const reader = new BitReader(bytes);

    let prevDC = options.initialDC || 0;
    const blocks = [];

    while (reader.bitsRemaining() > 0) {
        const startBit = reader.bitPos;

        try {
            const block = decodeOneBlock(reader, dcTable, acTable, prevDC);
            prevDC = block.dc;

            blocks.push({
                blockIndex: blocks.length,
                startBit,
                endBit: reader.bitPos,
                bitLength: reader.bitPos - startBit,
                steps: block.steps,
                coefficientsNatural: block.coefficientsNatural,
                matrix: block.matrix
            });
        } catch (err) {
            // Stop gracefully and return what was decoded so far.
            return {
                blocks,
                stoppedAtBit: reader.bitPos,
                bitsRemaining: reader.bitsRemaining(),
                error: err.message
            };
        }
    }

    return {
        blocks,
        stoppedAtBit: reader.bitPos,
        bitsRemaining: reader.bitsRemaining(),
        error: null
    };
}

function stepsToMarkedHexdump(bytes, steps, visibleMarkedCount = Infinity) {
    const bits = bytes
        .map(b => b.toString(2).padStart(8, "0"))
        .join("");

    // Build visible marked regions in decode order:
    // Huffman first, then value bits.
    const allRegions = [];
    let classIndex = 0;

    for (const step of steps) {
        if (step.huffman && step.huffman.length > 0) {
            allRegions.push({
                start: step.huffman.startBit,
                end: step.huffman.startBit + step.huffman.length,
                className: `m${classIndex % 9}`
            });
            classIndex++;
        }

        if (step.valueBits && step.valueBits.length > 0) {
            allRegions.push({
                start: step.valueBits.startBit,
                end: step.valueBits.startBit + step.valueBits.length,
                className: `m${classIndex % 9}`
            });
            classIndex++;
        }
    }

    const spans = allRegions.slice(0, visibleMarkedCount).sort((a, b) => a.start - b.start);

    function esc(s) {
        return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
    }

    // Build a set of hard boundaries:
    // - every byte boundary
    // - every visible span start/end
    // This preserves original byte grouping for all unmarked bits.
    const boundaries = new Set([0, bits.length]);

    for (let i = 0; i <= bits.length; i += 8) {
        boundaries.add(i);
    }

    for (const span of spans) {
        boundaries.add(span.start);
        boundaries.add(span.end);
    }

    const sortedBoundaries = [...boundaries]
        .filter(x => x >= 0 && x <= bits.length)
        .sort((a, b) => a - b);

    // Turn boundaries into segments.
    // Each segment is either fully marked or fully plain.
    const tokens = [];

    function findCoveringSpan(start, end) {
        for (const span of spans) {
            if (span.start <= start && span.end >= end) {
                return span;
            }
        }
        return null;
    }

    for (let i = 0; i < sortedBoundaries.length - 1; i++) {
        const start = sortedBoundaries[i];
        const end = sortedBoundaries[i + 1];
        if (start === end) continue;

        const text = bits.slice(start, end);
        const span = findCoveringSpan(start, end);

        if (span) {
            // Merge consecutive pieces of the same marked span so the span stays intact.
            const prev = tokens[tokens.length - 1];
            if (
                prev &&
                prev.type === "marked" &&
                prev.className === span.className &&
                prev.spanStart === span.start &&
                prev.spanEnd === span.end &&
                prev.end === start
            ) {
                prev.end = end;
                prev.text += text;
            } else {
                tokens.push({
                    type: "marked",
                    start,
                    end,
                    text,
                    className: span.className,
                    spanStart: span.start,
                    spanEnd: span.end
                });
            }
        } else {
            // Plain segments remain byte-aligned because byte boundaries are in the split set.
            tokens.push({
                type: "plain",
                start,
                end,
                text
            });
        }
    }

    // Render with newline after 7 bytes (= 56 bits),
    // but never split a marked token.
    let html = "<pre class='spec hexdump'>";
    let lineBits = 0;

    for (let i = 0; i < tokens.length; i++) {
        const t = tokens[i];
        const len = t.end - t.start;

        if (lineBits > 0 && lineBits + len > 56) {
            html += "\n";
            lineBits = 0;
        } else if (i > 0 && lineBits > 0) {
            html += " ";
        }

        if (t.type === "marked") {
            html += `<span class="${t.className}">${esc(t.text)}</span>`;
        } else {
            html += esc(t.text);
        }

        lineBits += len;

        if (lineBits === 56 && i !== tokens.length - 1) {
            html += "\n";
            lineBits = 0;
        }
    }

    html += "</pre>";
    return html;
}

function stepsToDctTable(steps, visibleMarkedCount = Infinity, colorize = true) {
    const cells = Array.from({ length: 8 }, () =>
        Array.from({ length: 8 }, () => ({ known: false, value: "", className: "" }))
    );

    function naturalToRowCol(naturalIndex) {
        return {
            row: Math.floor(naturalIndex / 8),
            col: naturalIndex % 8
        };
    }

    function setCellByZigzag(zigzagIndex, value, className = "") {
        const naturalIndex = ZIGZAG_TO_NATURAL[zigzagIndex];
        const { row, col } = naturalToRowCol(naturalIndex);

        cells[row][col] = {
            known: true,
            value,
            className: (colorize && value !== 0) ? className : ""
        };
    }

    function esc(s) {
        return String(s)
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;");
    }

    let regionIndex = 0;
    let acIndex = 1;

    for (const step of steps) {
        let huffmanVisible = false;
        let huffmanClass = "";
        let valueVisible = false;
        let valueClass = "";

        if (step.huffman && step.huffman.length > 0) {
            huffmanVisible = regionIndex < visibleMarkedCount;
            if (huffmanVisible) {
                huffmanClass = `m${regionIndex % 9}`;
            }
            regionIndex++;
        }

        if (step.valueBits && step.valueBits.length > 0) {
            valueVisible = regionIndex < visibleMarkedCount;
            if (valueVisible) {
                valueClass = `m${regionIndex % 9}`;
            }
            regionIndex++;
        }

        // -------------------------
        // DC
        // -------------------------
        if (step.kind === "DC") {
            const dcKnown =
                (step.valueBits && step.valueBits.length > 0 && valueVisible) ||
                ((!step.valueBits || step.valueBits.length === 0) && huffmanVisible);

            if (dcKnown) {
                const value = step.coefficient === 0 ? 0 : step.coefficient;

                const cls =
                    (!colorize || value === 0)
                        ? ""
                        : ((step.valueBits && step.valueBits.length > 0)
                            ? valueClass
                            : huffmanClass);

                cells[0][0] = {
                    known: true,
                    value,
                    className: cls
                };
            }

            continue;
        }

        // -------------------------
        // AC normal coefficient
        // -------------------------
        if (step.kind === "AC") {
            const run = step.extra?.run ?? 0;

            if (huffmanVisible) {
                for (let i = 0; i < run && acIndex < 64; i++, acIndex++) {
                    setCellByZigzag(acIndex, 0, "");
                }
            } else {
                break;
            }

            if (valueVisible && acIndex < 64) {
                setCellByZigzag(
                    acIndex,
                    step.coefficient === 0 ? 0 : step.coefficient,
                    valueClass
                );
            }

            acIndex++;
            continue;
        }

        // -------------------------
        // AC ZRL
        // -------------------------
        if (step.kind === "AC_ZRL") {
            if (!huffmanVisible) {
                break;
            }

            for (let i = 0; i < 16 && acIndex < 64; i++, acIndex++) {
                setCellByZigzag(acIndex, 0, "");
            }
            continue;
        }

        // -------------------------
        // AC EOB
        // -------------------------
        if (step.kind === "AC_EOB") {
            if (!huffmanVisible) {
                break;
            }

            while (acIndex < 64) {
                setCellByZigzag(acIndex, 0, "");
                acIndex++;
            }
            continue;
        }
    }

    let html = "<table class='table dct'>\n";

    for (let r = 0; r < 8; r++) {
        html += "<tr>\n";
        for (let c = 0; c < 8; c++) {
            const cell = cells[r][c];

            if (!cell.known) {
                html += "<td></td>";
            } else if (cell.className) {
                html += `<td class='${cell.className}'>${esc(cell.value)}</td>`;
            } else {
                html += `<td>${esc(cell.value)}</td>`;
            }
        }
        html += "\n</tr>\n";
    }

    html += "</table>";
    return html;
}

function fillQuantAndDequantTablesFromSteps(
    quantTarget,
    dequantTarget,
    steps,
    quantValuesZigZag,
    visibleMarkedCount = Infinity,
    colorize = true
) {
    const quantEl = typeof quantTarget === "string"
        ? document.querySelector(quantTarget)
        : quantTarget;

    const dequantEl = typeof dequantTarget === "string"
        ? document.querySelector(dequantTarget)
        : dequantTarget;

    if (!quantEl || !dequantEl) return;

    if (!Array.isArray(quantValuesZigZag) || quantValuesZigZag.length !== 64) {
        throw new Error("quantValuesZigZag must be an array of 64 numbers");
    }

    const cells = Array.from({ length: 64 }, () => ({
        known: false,
        value: "",
        className: ""
    }));

    function esc(s) {
        return String(s)
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;");
    }

    function naturalToRowCol(naturalIndex) {
        return {
            row: Math.floor(naturalIndex / 8),
            col: naturalIndex % 8
        };
    }

    function setCoeffByZigzag(zigzagIndex, value, className = "") {
        const naturalIndex = ZIGZAG_TO_NATURAL[zigzagIndex];
        cells[naturalIndex] = {
            known: true,
            value,
            className: (colorize && value !== 0) ? className : ""
        };
    }

    // Convert quantization values from zigzag order to natural order.
    const quantNatural = new Array(64);
    for (let zigzagIndex = 0; zigzagIndex < 64; zigzagIndex++) {
        const naturalIndex = ZIGZAG_TO_NATURAL[zigzagIndex];
        quantNatural[naturalIndex] = quantValuesZigZag[zigzagIndex];
    }

    let regionIndex = 0;
    let acIndex = 1;

    for (const step of steps) {
        let huffmanVisible = false;
        let huffmanClass = "";
        let valueVisible = false;
        let valueClass = "";

        if (step.huffman && step.huffman.length > 0) {
            huffmanVisible = regionIndex < visibleMarkedCount;
            if (huffmanVisible) {
                huffmanClass = `m${regionIndex % 9}`;
            }
            regionIndex++;
        }

        if (step.valueBits && step.valueBits.length > 0) {
            valueVisible = regionIndex < visibleMarkedCount;
            if (valueVisible) {
                valueClass = `m${regionIndex % 9}`;
            }
            regionIndex++;
        }

        // DC
        if (step.kind === "DC") {
            const dcKnown =
                (step.valueBits && step.valueBits.length > 0 && valueVisible) ||
                ((!step.valueBits || step.valueBits.length === 0) && huffmanVisible);

            if (dcKnown) {
                const value = step.coefficient === 0 ? 0 : step.coefficient;
                const cls =
                    (!colorize || value === 0)
                        ? ""
                        : ((step.valueBits && step.valueBits.length > 0) ? valueClass : huffmanClass);

                cells[0] = {
                    known: true,
                    value,
                    className: cls
                };
            }

            continue;
        }

        // Normal AC
        if (step.kind === "AC") {
            const run = step.extra?.run ?? 0;

            if (huffmanVisible) {
                for (let i = 0; i < run && acIndex < 64; i++, acIndex++) {
                    setCoeffByZigzag(acIndex, 0, "");
                }
            } else {
                break;
            }

            if (valueVisible && acIndex < 64) {
                setCoeffByZigzag(
                    acIndex,
                    step.coefficient === 0 ? 0 : step.coefficient,
                    valueClass
                );
            }

            acIndex++;
            continue;
        }

        // ZRL
        if (step.kind === "AC_ZRL") {
            if (!huffmanVisible) {
                break;
            }

            for (let i = 0; i < 16 && acIndex < 64; i++, acIndex++) {
                setCoeffByZigzag(acIndex, 0, "");
            }
            continue;
        }

        // EOB
        if (step.kind === "AC_EOB") {
            if (!huffmanVisible) {
                break;
            }

            while (acIndex < 64) {
                setCoeffByZigzag(acIndex, 0, "");
                acIndex++;
            }
            continue;
        }
    }

    function buildTable(values, classes = null, knownMask = null, multiplyWith = null) {
        let html = "<table class='table dct'>\n";

        for (let r = 0; r < 8; r++) {
            html += "<tr>\n";
            for (let c = 0; c < 8; c++) {
                const i = r * 8 + c;

                if (knownMask && !knownMask[i]) {
                    html += "<td></td>";
                    continue;
                }

                let value = values[i];
                if (multiplyWith) {
                    value = value * multiplyWith[i];
                }

                const className = classes && classes[i] ? classes[i] : "";

                if (className) {
                    html += `<td class='${className}'>${esc(value)}</td>`;
                } else {
                    html += `<td>${esc(value)}</td>`;
                }
            }
            html += "\n</tr>\n";
        }

        html += "</table>";
        return html;
    }

    const coeffValuesNatural = cells.map(cell => cell.value);
    const coeffKnownNatural = cells.map(cell => cell.known);
    const coeffClassesNatural = cells.map(cell => cell.className);

    quantEl.innerHTML = buildTable(
        quantNatural,
        coeffClassesNatural,
        coeffKnownNatural,
        null
    );

    dequantEl.innerHTML = buildTable(
        coeffValuesNatural,
        coeffClassesNatural,
        coeffKnownNatural,
        quantNatural
    );
}

function markTableCellsByContent(tableSelector, marks) {
    const table = typeof tableSelector === "string"
        ? document.querySelector(tableSelector)
        : tableSelector;

    if (!table) return;

    function escapeHtml(s) {
        return String(s)
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;");
    }

    // Remove existing mN spans but keep their text.
    table.querySelectorAll("span").forEach(span => {
        if ([...span.classList].some(c => /^m\d+$/.test(c)) || [...span.classList].some(c => /^mg$/.test(c))) {
            span.replaceWith(document.createTextNode(span.textContent));
        }
    });

    const markMap = new Map(marks.map(m => [String(m.content), m.className]));

    table.querySelectorAll("td").forEach(td => {
        const text = td.textContent.trim();
        const className = markMap.get(text);
        if (className) {
            td.innerHTML = `<span class="${className}">${escapeHtml(text)}</span>`;
        }
    });
}