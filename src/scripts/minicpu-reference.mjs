#!/usr/bin/env node
// The tables of the MiniCPU Handbuch pages (src/content/reference/minicpu*),
// generated from the CPU Sandbox's own tables, so bytes, cycles, opcodes,
// ports and palettes can never disagree with the machine:
//
//   node src/scripts/minicpu-reference.mjs ~/cpu-sandbox
//
// Rewrites every block between <!-- minicpu-table: NAME --> and
// <!-- /minicpu-table --> in those pages. The German texts are here; an
// instruction, port or pixel format without one stops the script, so a new
// one in the sandbox is noticed. Use the sandbox commit the screenshot
// recipes in sandbox-cpu.md pin.

import { readdirSync, readFileSync, writeFileSync, existsSync } from 'node:fs';
import { join, resolve, dirname } from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';

const sandbox = resolve(process.argv[2] || '');
if (!process.argv[2] || !existsSync(join(sandbox, 'src/opcodes.js'))) {
    console.error('Usage: node src/scripts/minicpu-reference.mjs PATH_TO_CPU_SANDBOX');
    process.exit(1);
}
const load = (file) => import(pathToFileURL(join(sandbox, 'src', file)).href);
const { ARCHITECTURES } = await load('architectures.js');
const { INSTRUCTIONS, WIDE_OPERATIONS, availableOn, sizeOf, cycleLabel } = await load('opcodes.js');
const { DEVICES } = await load('devices.js');
const { MODES } = await load('display.js');
const { PALETTES, defaultPalette } = await load('palettes.js');

const FAMILIES = {
    'Control': 'Steuerung',
    'Data movement': 'Daten bewegen',
    'Arithmetic': 'Rechnen',
    'Logic': 'Logik',
    'Comparison': 'Vergleichen',
    'Stack': 'Stack',
    'Jumps': 'Springen',
    'Input/output': 'Ein- und Ausgabe',
};

// What each instruction does, by its name in opcodes.js.
const EFFECT = {
    NOP: 'Tut nichts.',
    HLT: 'Hält die CPU an.',
    CLF: 'Löscht ZF und CF.',
    LDI: 'Lädt die Zahl 42 in A.',
    LDA: 'Lädt das Byte an Adresse 0x70 in A.',
    LDA_R: 'Lädt das Byte an der Adresse, die in B steht.',
    STA: 'Speichert A an Adresse 0x70.',
    STA_R: 'Speichert A an der Adresse, die in B steht.',
    MOV: 'Kopiert B nach A.',
    ADD: 'A = A + B. CF = Übertrag aus dem obersten Bit.',
    ADD_I: 'A = A + 5.',
    SUB: 'A = A − B. CF = geborgt (A war kleiner).',
    SUB_I: 'A = A − 5.',
    ADC: 'A = A + B + CF: addiert den Übertrag eines ADD davor.',
    ADC_I: 'A = A + 0 + CF.',
    SBC: 'A = A − B − CF: zieht das Geborgte eines SUB davor ab.',
    SBC_I: 'A = A − 0 − CF.',
    INC: 'A = A + 1.',
    DEC: 'A = A − 1.',
    SHL: 'Schiebt nach links; das oberste Bit kommt nach CF.',
    SHR: 'Schiebt nach rechts; Bit 0 kommt nach CF.',
    AND: 'Bitweises UND.',
    AND_I: 'Bitweises UND mit einer Zahl.',
    OR: 'Bitweises ODER.',
    OR_I: 'Bitweises ODER mit einer Zahl.',
    XOR: 'Bitweises exklusives ODER.',
    XOR_I: 'Bitweises exklusives ODER mit einer Zahl.',
    CMP: 'Rechnet A − B, nur für die Flags.',
    CMP_I: 'Rechnet A − 10, nur für die Flags.',
    PUSH: 'SP = SP − 1, dann A nach [SP].',
    POP: 'A aus [SP], dann SP = SP + 1.',
    CALL: 'Legt die Rücksprungadresse auf den Stack und springt.',
    RET: 'Nimmt die Rücksprungadresse vom Stack und springt dorthin.',
    LDA_SP: 'Lädt das Byte 2 Plätze über SP: ein Argument oder eine lokale Variable.',
    STA_SP: 'Speichert A 2 Plätze über SP.',
    ADD_SP: 'Gibt 3 Bytes auf dem Stack frei (lokale Variablen, oder Argumente nach dem Aufruf).',
    SUB_SP: 'Macht auf dem Stack Platz für 3 Bytes lokale Variablen.',
    JMP: 'Springt immer.',
    JZ: 'Springt, wenn ZF = 1 (null, nach CMP: gleich).',
    JNZ: 'Springt, wenn ZF = 0.',
    JC: 'Springt, wenn CF = 1 (Übertrag, nach CMP: kleiner).',
    JNC: 'Springt, wenn CF = 0.',
    IN: 'Liest ein Byte vom Gerät an Port 2.',
    OUT: 'Schickt A an das Gerät an Port 0x11.',
};

const DEVICE = {
    0x00: ['Joystick', 'die Tasten, die gerade gedrückt sind: Bit 0 ←, 1 →, 2 ↑, 3 ↓, 4 Leertaste'],
    0x01: ['Tastatur', 'das nächste getippte Zeichen (ASCII), oder 0, wenn nichts getippt wurde'],
    0x02: ['Timer', 'zählt 60-mal pro Sekunde Maschinenzeit hoch, 0–255, dann von vorn'],
    0x03: ['Zufall', 'bei jedem Lesen eine neue Zufallszahl 0–255'],
    0x04: ['Vsync', 'wartet, bis der Bildschirm sein nächstes Bild aufnimmt, und gibt dann den Timer zurück'],
    0x05: ['Gehaltene Taste', 'wo die zuletzt gedrückte, noch gehaltene Taste liegt: Reihe × 16 + Spalte, +128 mit Shift; 0, wenn keine'],
    0x10: ['Lautsprecher', 'spielt einen Ton; der Wert ist die MIDI-Notennummer (60 = c\', 69 = a\' mit 440 Hz)'],
    0x11: ['Zahlenanzeige', 'zeigt den Wert als Dezimalzahl'],
    0x12: ['Bildschirmseite', 'legt den Bildschirm an Adresse Wert × 256 (Seitenwechsel)'],
    0x13: ['Palettennummer', 'wählt die Farbe, die Port 0x14 als Nächstes ändert'],
    0x14: ['Palettenfarbe', 'Rot, Grün, Blau dieser Farbe (je 0–255); nach Blau ist die nächste Farbe dran'],
    0x15: ['Tonlänge', 'wie lange die folgenden Töne klingen, in 1/60 s (ohne Angabe 6)'],
};

const FORMAT = {
    mono: '1 Bit pro Pixel, 2 Farben aus der Palette, 8 Pixel pro Byte',
    indexed2: '2 Bit pro Pixel, 4 Farben aus der Palette, 4 Pixel pro Byte',
    indexed4: '4 Bit pro Pixel, 16 Farben aus der Palette, 2 Pixel pro Byte',
    indexed8: '8 Bit pro Pixel, 256 Farben aus der Palette',
    gray8: '8 Bit pro Pixel als Grauton: 0 schwarz, 255 weiß',
    rgb332: '8 Bit pro Pixel: 3 Bit Rot, 3 Bit Grün, 2 Bit Blau',
    rgb24: '24 Bit pro Pixel: je ein Byte Rot, Grün, Blau',
};

function need(table, key, what) {
    if (table[key] === undefined) {
        console.error(`No German text for ${what} ${typeof key === 'number' ? '0x' + key.toString(16) : key}. Add it to ${fileURLToPath(import.meta.url)}.`);
        process.exit(1);
    }
    return table[key];
}

const esc = (s) => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
const code = (s) => `<code>${esc(s)}</code>`;
const hex = (n, digits = 2) => '0x' + n.toString(16).toUpperCase().padStart(digits, '0');

// A check mark (Bootstrap Icons): the result sets the flag; 0 always
// cleared; – unchanged.
const SET = "<i class='bi bi-check-lg' role='img' aria-label='nach dem Ergebnis'></i>";
function flagCells(info) {
    if (info.name === 'CLF') return ['0', '0'];
    if (!info.flags) return ['–', '–'];
    return [SET, info.family === 'Logic' ? '0' : SET];
}

function table(head, rows) {
    return "<div class='scroll-x'><table class='table table-sm reference-table minicpu-table'>\n" +
        `<thead><tr>${head.map((h) => `<th>${h}</th>`).join('')}</tr></thead>\n<tbody>\n` +
        rows.join('\n') + '\n</tbody>\n</table>\n</div>';
}

function instructionTable(archId, only) {
    const arch = ARCHITECTURES[archId];
    const rows = [];
    let family = null;
    for (const info of INSTRUCTIONS.filter(only)) {
        if (info.family !== family) {
            family = info.family;
            rows.push(`<tr class='minicpu-family'><th colspan='7'>${need(FAMILIES, family, 'family')}</th></tr>`);
        }
        const [zf, cf] = flagCells(info);
        rows.push(`<tr><td>${code(info.syntax)}</td><td>${esc(need(EFFECT, info.name, 'instruction'))}</td>` +
            `<td class='minicpu-flag'>${zf}</td><td class='minicpu-flag'>${cf}</td>` +
            `<td class='minicpu-num'>${sizeOf(info, arch)}</td><td class='minicpu-num'>${cycleLabel(info, arch)}</td>` +
            `<td>${code(hex(info.code))}</td></tr>`);
    }
    return table(['Befehl', 'Wirkung', 'ZF', 'CF', 'Bytes', 'Takte', 'Opcode'], rows);
}

// On 8X: what changes against 8S, and what X or Y cost.
function costTable() {
    const s = ARCHITECTURES.cpu8s, x = ARCHITECTURES.cpu8x;
    const rows = INSTRUCTIONS.filter((info) => {
        const wide = WIDE_OPERATIONS.has(info.name);
        return wide || sizeOf(info, s) !== sizeOf(info, x) || cycleLabel(info, s) !== cycleLabel(info, x);
    }).map((info) => {
        const wide = WIDE_OPERATIONS.has(info.name);
        const cost = (arch, w = false) => `${sizeOf(info, arch, w)} Bytes, ${cycleLabel(info, arch, w)} Takte`;
        const same = cost(s) === cost(x);
        return `<tr><td>${code(info.syntax)}</td><td class='minicpu-num'>${cost(s)}</td>` +
            `<td class='minicpu-num'>${same ? cost(x) : `<strong>${cost(x)}</strong>`}</td>` +
            `<td class='minicpu-num'>${wide ? cost(x, true) : '–'}</td></tr>`;
    });
    return table(['Befehl', '8 / 8S', '8X', '8X mit X oder Y'], rows);
}

function portTable() {
    return table(['Port', 'Gerät', '', 'Das Byte'], DEVICES.map((d) => {
        const [name, text] = need(DEVICE, d.port, 'port');
        return `<tr><td>${code(hex(d.port))}</td><td>${esc(name)}</td><td>${code(d.direction === 'in' ? 'IN' : 'OUT')}</td><td>${esc(text)}</td></tr>`;
    }));
}

function formatTable() {
    return table(['Format', 'Pixel'], Object.keys(MODES).map((mode) =>
        `<tr><td>${code(mode)}</td><td>${esc(need(FORMAT, mode, 'pixel format'))}</td></tr>`));
}

function paletteList() {
    return "<div class='minicpu-palettes'>\n" + Object.entries(PALETTES).map(([name, colors]) => {
        const label = `${colors.length} Farben${defaultPalette(colors.length) === name ? ', Standard' : ''}`;
        const swatches = colors.map((c, i) => `<span style='background:${c}' title='${i}: ${c}'></span>`).join('');
        return `<div class='minicpu-palette'><div>${code(name)} <small>${label}</small></div>` +
            `<div class='minicpu-swatches${colors.length > 16 ? ' many' : ''}' role='img' aria-label='Palette ${name}, ${colors.length} Farben'>${swatches}</div></div>`;
    }).join('\n') + '\n</div>';
}

const BLOCKS = {
    'befehle-8': () => instructionTable('cpu8', (i) => availableOn(i, 'cpu8')),
    'befehle-8s': () => instructionTable('cpu8s', (i) => i.since === 'cpu8s'),
    'kosten-8x': costTable,
    'ports': portTable,
    'formate': formatTable,
    'paletten': paletteList,
};

// The ```minicpu syntax colours know the mnemonics by name.
const { MNEMONICS } = await load('assembler.js');
const lexer = readFileSync(join(dirname(fileURLToPath(import.meta.url)), '../ruby/include/minicpu_lexer.rb'), 'utf8');
const known = (lexer.match(/MNEMONICS = %w\[([^\]]*)\]/) || [])[1]?.split(/\s+/).filter(Boolean) || [];
const differ = [...MNEMONICS.filter((m) => !known.includes(m)), ...known.filter((m) => !MNEMONICS.includes(m))];
if (differ.length) {
    console.error(`MiniCPU::MNEMONICS in src/ruby/include/minicpu_lexer.rb differs from the sandbox: ${differ.join(', ')}`);
    process.exit(1);
}

const reference = join(dirname(fileURLToPath(import.meta.url)), '../content/reference');
const pages = readdirSync(reference).filter((d) => d.startsWith('minicpu')).map((d) => join(reference, d, `${d}.md`));
const seen = new Set();
for (const page of pages) {
    const before = readFileSync(page, 'utf8');
    // Blank lines around the HTML, or Markdown after it is not read as Markdown.
    const after = before.replace(/(<!-- minicpu-table: ([\w-]+) -->\n)[\s\S]*?(<!-- \/minicpu-table -->)/g, (_, open, name, close) => {
        if (!BLOCKS[name]) throw new Error(`${page}: unknown table ${name}`);
        seen.add(name);
        return `${open}\n${BLOCKS[name]()}\n\n${close}`;
    });
    if (after !== before) writeFileSync(page, after);
    console.log(`${after === before ? 'unchanged' : 'updated  '} ${page}`);
}
const missing = Object.keys(BLOCKS).filter((name) => !seen.has(name));
if (missing.length) console.log(`Not on any page: ${missing.join(', ')}`);
