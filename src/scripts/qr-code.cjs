#!/usr/bin/env node
// The QR codes of »Prüfziffern« (src/content/check-digits): the same 13
// digits as the noodles' barcode, as a QR code of version 2 (25 × 25
// modules) at the four levels L, M, Q and H, made by qrencode. Writes the
// pictures of the chapter and prints the widget's <div> for the page:
//
//   node src/scripts/qr-code.cjs src/content/check-digits
//
// then src/scripts/render-svg.rb for qr-aufbau.svg (1200) and qr-flecken.svg
// (800), light and dark. Before it writes anything, it checks every code with jsQR:
// the clean code reads, and the two stickers of the prediction do what the
// page says (A over a finder pattern: unreadable at every level; B over the
// right edge: readable at M, not at L). The parts of the code come from
// src/static/include/qr_code.js, which the page's widget uses too.
'use strict';
const fs = require('fs');
const path = require('path');
const { execFileSync } = require('child_process');
const Q = require('../static/include/qr_code.js');
const jsQR = require('../static/include/jsqr/jsQR.js');

const TEXT = '4260111020434';
const VERSION = 2;
const STICKERS = { A: [1, 1, 5, 5], B: [9, 20, 5, 5] };   // row, column, height, width

function encode(level) {
    const out = execFileSync('qrencode', ['-v', String(VERSION), '-l', level, '-m', '0', '-t', 'ASCII', TEXT]).toString();
    return out.trimEnd().split('\n').map((line) => {
        const row = [];
        for (let i = 0; i < line.length; i += 2) row.push(line[i] === '#' ? 1 : 0);
        return row;
    });
}

// what jsQR reads from the matrix drawn at 8 px a module with a quiet zone of 4
function decode(m, px = 8, quiet = 4) {
    const w = (m.length + 2 * quiet) * px;
    const data = new Uint8ClampedArray(w * w * 4).fill(255);
    for (let r = 0; r < m.length; r++) {
        for (let c = 0; c < m.length; c++) {
            if (!m[r][c]) continue;
            for (let y = 0; y < px; y++) {
                for (let x = 0; x < px; x++) {
                    const i = (((r + quiet) * px + y) * w + (c + quiet) * px + x) * 4;
                    data[i] = data[i + 1] = data[i + 2] = 0;
                }
            }
        }
    }
    const result = jsQR(data, w, w, { inversionAttempts: 'dontInvert' });
    return result ? result.data : null;
}

function sticker(m, [r0, c0, h, w]) {
    return m.map((row, r) => row.map((v, c) => (r >= r0 && r < r0 + h && c >= c0 && c < c0 + w ? 0 : v)));
}

const SANS = "'IBM Plex Sans', system-ui, sans-serif";
const LABEL = '#fdfdfd';       // a printed label: white in both modes
const BAR = '#0b0f19';
const INK = '#1f2937';
const GREY = '#6b7280';
const LINE = '#d1d5db';
const BLUE = '#2b5fb4';
const GREEN = '#16a34a';
const AMBER = '#d97706';
const PURPLE = '#7c3aed';
const STICKER = '#fde68a';     // a yellow sticker on the label: the same in both modes

const f1 = (x) => (Math.round(x * 10) / 10).toString();
const text = (x, y, content, colour = INK, anchor = 'middle', size = 15, weight = '400') =>
    `<text x="${f1(x)}" y="${f1(y)}" text-anchor="${anchor}" font-family="${SANS}" font-size="${size}" font-weight="${weight}" fill="${colour}">${content}</text>`;

// the modules as one path of dark squares
function modules(m, x0, y0, u) {
    let d = '';
    m.forEach((row, r) => row.forEach((v, c) => {
        if (v) d += `M${f1(x0 + c * u)} ${f1(y0 + r * u)}h${u}v${u}h-${u}z`;
    }));
    return `<path d="${d}" fill="${BAR}"/>`;
}

// The code with its parts: pale areas in the colour of each part, the edges
// between two codewords drawn, the names around it with a line to the part.
function aufbau(m, level) {
    const l = Q.layout(VERSION);
    const cap = Q.capacity(VERSION, level);
    const u = 13;
    const left = 240;
    const top = 62;
    const n = l.size;
    const x0 = left + 2 * u;
    const y0 = top + 2 * u;
    const width = left + (n + 4) * u + 240;
    const height = top + (n + 4) * u + 30;
    const colourOf = (r, c) => {
        const role = l.role[r][c];
        if (role === 'finder' || role === 'timing' || role === 'alignment') return AMBER;
        if (role === 'format') return PURPLE;
        if (l.codeword[r][c] < 0) return null;
        return l.codeword[r][c] < cap.data ? BLUE : GREEN;
    };
    const out = [`<rect width="${width}" height="${height}" fill="#ffffff"/>`,
        `<rect x="${left}" y="${top}" width="${(n + 4) * u}" height="${(n + 4) * u}" rx="6" fill="${LABEL}" stroke="${LINE}" stroke-width="1.5"/>`];
    const fills = {};
    for (let r = 0; r < n; r++) {
        for (let c = 0; c < n; c++) {
            const colour = colourOf(r, c);
            if (!colour) continue;
            fills[colour] = (fills[colour] || '') + `M${x0 + c * u} ${y0 + r * u}h${u}v${u}h-${u}z`;
        }
    }
    for (const [colour, d] of Object.entries(fills)) out.push(`<path d="${d}" fill="${colour}" fill-opacity="0.2"/>`);
    out.push(modules(m, x0, y0, u));
    // the edges of every codeword, in its colour
    const edges = {};
    const key = (r, c) => (r < 0 || c < 0 || r >= n || c >= n || l.role[r][c] !== 'data' ? 'x' : l.codeword[r][c]);
    for (let r = 0; r < n; r++) {
        for (let c = 0; c < n; c++) {
            const k = key(r, c);
            if (k === 'x' || k < 0) continue;
            const colour = colourOf(r, c);
            const x = x0 + c * u;
            const y = y0 + r * u;
            let d = '';
            if (key(r - 1, c) !== k) d += `M${x} ${y}h${u}`;
            if (key(r + 1, c) !== k) d += `M${x} ${y + u}h${u}`;
            if (key(r, c - 1) !== k) d += `M${x} ${y}v${u}`;
            if (key(r, c + 1) !== k) d += `M${x + u} ${y}v${u}`;
            edges[colour] = (edges[colour] || '') + d;
        }
    }
    for (const [colour, d] of Object.entries(edges)) {
        out.push(`<path d="${d}" fill="none" stroke="${colour}" stroke-width="2.5" stroke-linecap="square"/>`);
    }
    // the names, each with a line to a point of its part (row, column)
    const at = (r, c) => [x0 + (c + 0.5) * u, y0 + (r + 0.5) * u];
    const names = [
        { side: 'left', label: 'Suchmuster', colour: AMBER, to: at(3, 0) },
        { side: 'left', label: 'Formatinformation', colour: PURPLE, to: at(8, 2) },
        { side: 'left', label: `Korrektur: ${cap.ec} Codewörter`, colour: GREEN, to: at(14, 2) },
        { side: 'left', label: 'Suchmuster', colour: AMBER, to: at(21, 0) },
        { side: 'right', label: 'Suchmuster', colour: AMBER, to: at(3, n - 1) },
        { side: 'right', label: 'Formatinformation', colour: PURPLE, to: at(8, n - 3) },
        { side: 'right', label: `Daten: ${cap.data} Codewörter`, colour: BLUE, to: at(11, n - 1) },
        { side: 'right', label: 'Ausrichtungsmuster', colour: AMBER, to: at(18, 20) },
    ];
    for (const { side, label, colour, to } of names) {
        const edge = side === 'left' ? left - 12 : left + (n + 4) * u + 12;
        out.push(`<path d="M${edge} ${f1(to[1])} H${f1(to[0])}" fill="none" stroke="${colour}" stroke-width="1.5"/>`);
        out.push(`<circle cx="${f1(to[0])}" cy="${f1(to[1])}" r="3.5" fill="${colour}"/>`);
        out.push(text(side === 'left' ? edge - 8 : edge + 8, to[1] + 6, label, colour, side === 'left' ? 'end' : 'start', 17));
    }
    // the timing line: its name above the code, a line down to it
    const [tx, ty] = at(6, 12);
    out.push(`<path d="M${f1(tx)} ${top - 14} V${f1(ty)}" fill="none" stroke="${AMBER}" stroke-width="1.5"/>`);
    out.push(`<circle cx="${f1(tx)}" cy="${f1(ty)}" r="3.5" fill="${AMBER}"/>`);
    out.push(text(tx, top - 22, 'Taktlinie', AMBER, 'middle', 17));
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${width} ${height}" width="${width}" height="${height}">${out.join('')}</svg>\n`;
}

// the code with the two stickers of the prediction, A and B
function flecken(m) {
    const u = 12;
    const quiet = 3;
    const n = m.length;
    const w = (n + 2 * quiet) * u;
    const out = [`<rect width="${w}" height="${w}" fill="#ffffff"/>`,
        `<rect x="2" y="2" width="${w - 4}" height="${w - 4}" rx="6" fill="${LABEL}" stroke="${LINE}" stroke-width="1.5"/>`,
        modules(m, quiet * u, quiet * u, u)];
    for (const [name, [r, c, h, wd]] of Object.entries(STICKERS)) {
        const x = (quiet + c) * u;
        const y = (quiet + r) * u;
        out.push(`<rect x="${x}" y="${y}" width="${wd * u}" height="${h * u}" rx="3" fill="${STICKER}" stroke="${AMBER}" stroke-width="1.5"/>`);
        out.push(text(x + wd * u / 2, y + h * u / 2 + 9, name, INK, 'middle', 26, '600'));
    }
    return `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${w} ${w}" width="${w}" height="${w}">${out.join('')}</svg>\n`;
}

function main() {
    const dir = process.argv[2];
    if (!dir) {
        console.error('usage: node src/scripts/qr-code.cjs src/content/check-digits');
        process.exit(1);
    }
    const codes = {};
    for (const level of Q.LEVELS) {
        const m = encode(level);
        const format = Q.readFormat(m);
        if (!format || format.level !== level || format.distance !== 0) throw new Error(`${level}: format information not as expected`);
        const cap = Q.capacity(VERSION, level);
        const words = Q.codewords(m, format.mask);
        const ec = Q.correction(words.slice(0, cap.data), cap.ec);
        if (ec.join() !== words.slice(cap.data).join()) throw new Error(`${level}: correction codewords differ from qr_code.js`);
        if (decode(m) !== TEXT) throw new Error(`${level}: jsQR does not read the clean code`);
        if (decode(sticker(m, STICKERS.A)) !== null) throw new Error(`${level}: jsQR reads the code with sticker A`);
        codes[level] = { m, mask: format.mask };
    }
    const b = sticker(codes.M.m, STICKERS.B);
    if (decode(b) !== TEXT) throw new Error('M: jsQR does not read the code with sticker B');
    if (decode(sticker(codes.L.m, STICKERS.B)) !== null) throw new Error('L: jsQR reads the code with sticker B');
    const damaged = Q.damage(codes.M.m, b, 'M');
    const write = (name, content) => {
        fs.writeFileSync(path.join(dir, name), content);
        console.error(`wrote ${path.join(dir, name)}`);
    };
    write('qr-aufbau.svg', aufbau(codes.M.m, 'M'));
    write('qr-flecken.svg', flecken(codes.M.m));
    console.error(`M: mask ${codes.M.mask}; sticker B damages ${damaged.codewords.length} codewords (${damaged.codewords.join(', ')}), M repairs ${damaged.capacity.repairs}`);
    const attrs = Q.LEVELS.map((level) => `data-${level.toLowerCase()}='${Q.format(codes[level].m)}'`).join('\n    ');
    console.log(`<div class='qr-damage' data-text='${TEXT}' data-level='M'\n    ${attrs}></div>`);
}

main();
