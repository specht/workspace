#!/usr/bin/env node
// Draws an ER model (.er) in crow's foot notation as a card picture, light
// and dark: OUT.webp and OUT-dark.webp, which the catalog picks up with
// »image: OUT.webp:contain« (padded to 16:9).
//
//   node src/scripts/er-teaser.cjs MODEL.er OUT.webp [--row GAP | --grid ROWS GAPX GAPY]
//
// Default: the ER-Werkstatt's own layout. --row puts the boxes side by
// side, in the order the ER-Werkstatt stacks them, GAP pixels apart.
// --grid places them in rows, »a,b,c;d,,e« (an empty name leaves a cell
// free, N:name is a relationship with a table of its own), GAPX and GAPY
// apart. Needs rsvg-convert, ImageMagick and cwebp.
'use strict';
const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const core = require('../vscode-extensions/er/core/er-core.js');

const [model, out, flag, ...rest] = process.argv.slice(2);
if (!model || !out || !out.endsWith('.webp')) {
    process.stderr.write('node er-teaser.cjs MODEL.er OUT.webp [--row GAP | --grid ROWS GAPX GAPY]\n');
    process.exit(2);
}
const text = fs.readFileSync(model, 'utf8');

let positions = null;
if (flag === '--grid') {
    const [rows, gapX, gapY] = rest;
    positions = {};
    rows.split(';').forEach((row, y) => row.split(',').forEach((name, x) => {
        // an entity (C:) or the box of a relationship with its own table (N:)
        const id = name.trim().includes(':') ? name.trim() : `C:${name.trim()}`;
        if (name.trim()) positions[id] = { x: x * Number(gapX), y: y * Number(gapY) };
    }));
} else if (flag === '--row') {
    const gap = Number(rest[0] || 340);
    const stacked = core.diagram(text, 'crowfoot', {}).positions;
    positions = {};
    Object.keys(stacked)
        .sort((a, b) => stacked[a].y - stacked[b].y || stacked[a].x - stacked[b].x)
        .forEach((id, i) => { positions[id] = { x: i * gap, y: 0 }; });
}

// The colours of dark.css for ER diagrams (.er-playground-output)
const DARK = {
    '--er-ink': '#e7e6e1', '--er-paper': '#1c1c1b', '--er-shape': '#1c1c1b', '--er-muted': '#a3a29d',
    '--er-pen': '#7fbfe3', '--er-pen-soft': '#1d3346', '--er-red': '#f19aa1',
};

const tmp = fs.mkdtempSync(path.join(os.tmpdir(), 'er-teaser-'));
for (const dark of [false, true]) {
    let svg = core.diagram(text, 'crowfoot', { positions, background: dark ? DARK['--er-paper'] : '#ffffff' }).svg;
    // rsvg-convert does not take variables from outside: the dark colours
    // replace the fallbacks in the diagram's own style
    if (dark) svg = svg.replace(/var\((--er-[a-z-]+),[^)]*\)/g, (all, name) => DARK[name] || all);
    const svgPath = path.join(tmp, 'diagram.svg');
    const pngPath = path.join(tmp, 'diagram.png');
    fs.writeFileSync(svgPath, svg);
    execFileSync('rsvg-convert', ['-w', '1600', svgPath, '-o', pngPath]);
    // Only the diagram and an even margin: the catalog pads it to 16:9
    const background = dark ? DARK['--er-paper'] : '#ffffff';
    execFileSync('convert', [pngPath, '-trim', '+repage', '-bordercolor', background, '-border', '16', pngPath]);
    const target = dark ? out.replace(/\.webp$/, '-dark.webp') : out;
    execFileSync('cwebp', ['-quiet', '-lossless', pngPath, '-o', target]);
    process.stdout.write(`${target}\n`);
}
fs.rmSync(tmp, { recursive: true, force: true });
