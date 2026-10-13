// Small pure helpers of the extension, tested without VS Code.
'use strict';

const path = require('path');

const KINDS = ['chen', 'crowfoot', 'tables'];

// film.er -> .film.er.layout.json (hidden, next to the model)
function layoutPath(erPath) {
    return path.join(path.dirname(erPath), `.${path.basename(erPath)}.layout.json`);
}

// film.er -> film.sql
function sqlPath(erPath) {
    return erPath.replace(/\.er$/i, '') + '.sql';
}

// film.er -> film-chen.svg
function svgPath(erPath, kind) {
    return `${erPath.replace(/\.er$/i, '')}-${kind}.svg`;
}

// A file the ER-Werkstatt wrote itself may be overwritten; anything else is
// someone's own SQL and is only replaced after asking.
function isGenerated(text) {
    return /^-- Erzeugt( aus \S+)? mit der ER-Werkstatt/.test(String(text || ''));
}

// The layout file: { version: 1, chen: { id: {x, y} }, crowfoot: {...}, tables: {...} }.
// Anything broken counts as »no positions yet«.
function readLayout(text) {
    const empty = { version: 1, chen: {}, crowfoot: {}, tables: {} };
    let data;
    try {
        data = JSON.parse(String(text || ''));
    } catch (e) {
        return empty;
    }
    if (!data || typeof data !== 'object') return empty;
    for (const kind of KINDS) {
        const clean = {};
        const src = data[kind];
        if (src && typeof src === 'object') {
            for (const [id, p] of Object.entries(src)) {
                if (p && Number.isFinite(p.x) && Number.isFinite(p.y)) clean[id] = { x: p.x, y: p.y };
            }
        }
        empty[kind] = clean;
    }
    return empty;
}

function writeLayout(layout) {
    return JSON.stringify(layout, null, 1) + '\n';
}

module.exports = { KINDS, layoutPath, sqlPath, svgPath, isGenerated, readLayout, writeLayout };
