// Small pure helpers of the extension, tested without VS Code.
'use strict';

// supermarkt.uml -> supermarkt.svg
function svgPath(umlPath) {
    return umlPath.replace(/\.uml$/i, '') + '.svg';
}

// A file the UML-Werkstatt wrote itself may be overwritten; any other SVG
// is someone's own picture and is only replaced after asking.
function isGenerated(text) {
    return /^<svg [^>]*class="cd-diagram"/.test(String(text || ''));
}

// Where a problem goes in the editor: line (0-based) and columns, kept
// inside the document. lineLength(i) gives the length of line i.
function problemRange(p, lineCount, lineLength) {
    const line = Math.max(0, Math.min(lineCount - 1, (p.line || 1) - 1));
    const length = lineLength(line);
    const from = Math.min(p.from || 0, length);
    const to = Math.max(from + 1, Math.min(p.to || length, length));
    return { line, from, to };
}

module.exports = { svgPath, isGenerated, problemRange };
