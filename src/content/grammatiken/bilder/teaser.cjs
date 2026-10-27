// The card picture of »Grammatiken«: the syntax diagrams of the grammar for
// expressions, expression and term side by side, factor under them, the
// expression inside factor in purple: there the grammar calls itself. Drawn
// by the page's own module (src/static/include/syntax_diagram.js), so that the
// card looks like the diagrams on the page.
//
//   node src/content/grammatiken/bilder/teaser.cjs
//   src/scripts/render-svg.rb src/content/grammatiken/teaser.svg 1280
const fs = require('fs');
const path = require('path');
const SD = require('../../../static/include/syntax_diagram.js');

const GRAMMAR = `expression = term { "+" term }
term       = factor { "*" factor }
factor     = number | "(" expression ")"`;
const W = 640;
const H = 360;

// one rule as a nested picture at (x, y), scale times its size
function rule(name, x, y, scale) {
    const r = SD.svg(GRAMMAR, { only: [name], file: true, accent: ['expression'] });
    if (!r.svg) throw new Error(r.errors.join('\n'));
    const inner = r.svg.replace(/^<svg xmlns="http:\/\/www\.w3\.org\/2000\/svg" class="syntax-diagram-svg"/, '<svg')
        .replace(/width="\d+" height="\d+"/, `x="${x}" y="${y}" width="${Math.round(r.width * scale)}" height="${Math.round(r.height * scale)}"`);
    return { svg: inner, w: r.width * scale, h: r.height * scale };
}

const scale = 1.25;
const size = Object.fromEntries(['expression', 'term', 'factor'].map((name) => [name, rule(name, 0, 0, scale)]));
// expression and term side by side, factor centred under them
const gapX = 40;
const gapY = 18;
const rowW = size.expression.w + gapX + size.term.w;
const rowH = Math.max(size.expression.h, size.term.h);
const top = (H - rowH - gapY - size.factor.h) / 2;
const x0 = (W - rowW) / 2;
const placed = [
    rule('expression', Math.round(x0), Math.round(top), scale),
    rule('term', Math.round(x0 + size.expression.w + gapX), Math.round(top), scale),
    rule('factor', Math.round((W - size.factor.w) / 2), Math.round(top + rowH + gapY), scale),
];
const svg = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${W} ${H}" width="${W}" height="${H}">\n` +
    `  <!-- gemacht von bilder/teaser.cjs, als Bild: src/scripts/render-svg.rb src/content/grammatiken/teaser.svg 1280 -->\n` +
    `  <rect width="${W}" height="${H}" fill="#ffffff"/>\n  ${placed.map((p) => p.svg).join('\n  ')}\n</svg>\n`;
fs.writeFileSync(path.join(__dirname, '..', 'teaser.svg'), svg);
