// Parts of a Handbuch page that a page includes with _include(slug#part)
// and whose recipes depend on that page: a recipe that clones ${template}
// must show the page's own repository, not the one of the Handbuch page.
// Such a part is rendered once for every page that includes it: its text
// goes into the page at the include, in a tutorial-scope, with the page's
// values, and its pictures are written next to the page as
// included-<slug>-<name> (main.rb shows them there instead of the part's own,
// ContentParts.expand_includes). Kept free of Playwright so it can be tested
// on its own (test/includes.test.js).

export const INCLUDED_PREFIX = 'included-';

// The same as ContentParts::INCLUDE and PART_MARKER (src/ruby/include/content_parts.rb).
const NAME = '[a-z0-9][a-z0-9-]*';
const INCLUDE = new RegExp(`^_include\\(\\s*(${NAME})#(${NAME})\\s*(?:,\\s*done:\\s*${NAME}\\s*)?\\)[ \\t]*$`, 'gm');
const PART_START = new RegExp(`^<!--\\s*part:\\s*(${NAME})\\s*-->`);
const PART_END = /^<!--\s*\/part\s*-->/;
const RECIPE = /<!--\s*tutorial-(?:screenshot|step)\s*\n[\s\S]*?-->/g;
// ${template}, or ${template:-VALUE} with the value for a page that has
// none (the Handbuch page itself)
const VARIABLE = /\$\{template(?::-([^}]*))?\}/g;
const MEDIA_SRC = /(<(?:img|video|source)\b[^>]*\bsrc=)(['"])([^'"]+)\2/gi;

// The page's meta block (<div class='meta'> with »key: value« lines).
export function pageMeta(markdown) {
    const block = /<div class='meta'>\n([\s\S]*?)\n<\/div>/.exec(markdown);
    const meta = {};
    for (const line of (block ? block[1] : '').split('\n')) {
        const match = /^([a-z_]+):\s*(.*?)\s*$/.exec(line);
        if (match) meta[match[1]] = match[2];
    }
    return meta;
}

// The text of one part of a Handbuch page, or null.
export function partText(markdown, part) {
    const lines = [];
    let inside = false;
    for (const line of markdown.split('\n')) {
        const start = PART_START.exec(line);
        if (start) {
            if (inside) break;
            inside = start[1] === part;
            continue;
        }
        if (PART_END.test(line)) {
            if (inside) break;
            continue;
        }
        if (inside) lines.push(line);
    }
    return inside || lines.length > 0 ? lines.join('\n').trim() : null;
}

// ${template} in the recipes of markdown, from the page's meta. A
// clone-start of the page's repository takes the commit the page's own
// clone-start of it is pinned to, so both clone the same.
export function fillRecipeVariables(markdown, meta, pinsFrom = markdown) {
    const template = meta.template?.trim() || null;
    const pins = clonePins(pinsFrom);
    return markdown.replace(RECIPE, recipe => recipe
        .replace(VARIABLE, (whole, fallback) => template ?? (fallback !== undefined ? fallback.trim() : whole))
        .replace(/^(clone-start:\s*)(\S+)[ \t]*$/gm, (line, prefix, url) => (pins.has(url) ? `${prefix}${url} @ ${pins.get(url)}` : line)));
}

function clonePins(markdown) {
    const pins = new Map();
    for (const match of markdown.matchAll(/^clone-start:\s*(\S+)\s*@\s*([0-9a-f]{7,40})\s*$/gm)) {
        if (!pins.has(match[1])) pins.set(match[1], match[2]);
    }
    return pins;
}

// Does a part render differently on each page that includes it?
export function dependsOnPage(text) {
    return [...text.matchAll(RECIPE)].some(match => /\$\{template\b/.test(match[0]));
}

// The page's Markdown with every such part put in at its _include, and a map
// from lines of the result back to where they were written:
// { markdown, regions: [{ start, lines, path, line }] } — start is the line
// of the result where the part begins (1-based), lines how many it takes,
// path and line where its first line is written. readReference(slug) gives
// a Handbuch page's Markdown and its path, or null.
export function expandIncludedRecipes(markdown, markdownPath, readReference) {
    const meta = pageMeta(markdown);
    const regions = [];
    let shift = 0; // lines the result has more than the page so far
    const result = markdown.replace(INCLUDE, (whole, slug, part, offset) => {
        const reference = readReference(slug);
        const text = reference && partText(reference.markdown, part);
        if (!text || !dependsOnPage(text)) return whole;

        const filled = fillRecipeVariables(text, meta, markdown)
            .replace(MEDIA_SRC, (all, attr, quote, src) => (/^[a-z][a-z\d+.-]*:|^\//i.test(src) || src.includes('/')
                ? all : `${attr}${quote}${INCLUDED_PREFIX}${slug}-${src}${quote}`));
        const body = `<!-- tutorial-scope included -->\n${filled}\n<!-- /tutorial-scope -->`;
        const startLine = markdown.slice(0, offset).split('\n').length + shift;
        const lines = body.split('\n').length;
        const partLine = reference.markdown.split('\n').findIndex(line => PART_START.exec(line)?.[1] === part) + 2;
        regions.push({ start: startLine + 1, lines: lines - 2, path: reference.path, line: partLine });
        shift += lines - 1;
        return body;
    });
    return { markdown: result, regions, path: markdownPath };
}

// Where line (1-based) of an expanded page was written: { path, line }.
export function sourceLine(expansion, line) {
    let shift = 0;
    for (const region of expansion.regions) {
        if (line < region.start - 1) break;
        if (line < region.start) return { path: expansion.path, line: line - shift }; // the scope marker: the _include
        if (line < region.start + region.lines) {
            return { path: region.path, line: region.line + (line - region.start) };
        }
        if (line === region.start + region.lines) return { path: expansion.path, line: region.start - 1 - shift };
        shift += region.lines + 1;
    }
    return { path: expansion.path, line: line - shift };
}
