// Screenshot hooks for the HTML and CSS pages of the Handbuch (html, css,
// layout, shower), src/tutorial-screenshots/README.md »Tutorial-specific
// hooks«.
//
// Every example is shown twice: as a plain web page and as a Shower slide,
// because the same HTML and CSS work in both. A recipe writes the example
// into the screenshot Workspace's clone of the Shower template (beispiel.html
// and beispiel.css next to its index.html), then `hook: beispiel` turns the
// preview tab into a page that shows it. The variant `folie` shows it on a
// slide instead (media_variants.js puts a switch for it into the image's
// corner).
//
// The template is served by a small web server in the Workspace (the first
// recipe starts it, preview-port shows it). The preview stays on its
// address, so relative paths in an example (pictures/fuji.jpg, fonts/…)
// work as they do for students.

import { readFile } from 'node:fs/promises';

const readyTimeout = 60_000;

// A row of VS Code's Explorer by its name
function explorerRow(page, name) {
    return page.locator(`.explorer-folders-view .monaco-list-row[aria-label="${name}"]`).first();
}

// Drag events with a file from outside the browser, as the computer's file
// manager sends them: VS Code marks the folder the file would land in.
async function dragEvents(page, row, types) {
    const box = await row.boundingBox();
    const transfer = await page.evaluateHandle(() => {
        const data = new DataTransfer();
        data.items.add(new File([new Uint8Array(1)], 'classroom.jpg', { type: 'image/jpeg' }));
        return data;
    });
    for (const type of types) {
        await row.dispatchEvent(type, {
            dataTransfer: transfer, bubbles: true, cancelable: true,
            clientX: box.x + box.width / 2, clientY: box.y + box.height / 2,
        });
    }
}

// hook: beispiel css: the example has a stylesheet too (beispiel.css);
// without it, the one of an earlier example is not used. Kept for the
// variants, which show the same example.
let withCss = false;

// A plain page as a student's first page looks: the browser's own styles,
// a little larger, so that the picture can be read when it is small.
const PAGE = `<!DOCTYPE html>
<html lang="de">
<head>
<meta charset="utf-8">
<title>Beispiel</title>
<style>
html { font-size: 22px; }
body { margin: 0; padding: 1rem 2rem; font-family: system-ui, sans-serif; }
</style>
<style>%%CSS%%</style>
</head>
<body>
%%HTML%%
</body>
</html>`;

// One slide of the Shower template, with its theme, its styles.css and
// KaTeX, in full-screen mode.
const SLIDE = `<!DOCTYPE html>
<html lang="de">
<head>
<meta charset="utf-8">
<title>Beispiel</title>
<link rel="stylesheet" href="shower/themes/ribbon/styles/index.css">
<link rel="stylesheet" href="styles.css">
<link rel="stylesheet" href="include/katex/katex.min.css">
<style>%%CSS%%</style>
<script src="include/katex/katex.min.js"></script>
<script src="include/katex/auto-render.min.js"></script>
<script src="shower/index.js" defer></script>
</head>
<body class="shower full">
%%SLIDE%%
<script>renderMathInElement(document.body);</script>
</body>
</html>`;

async function render(page, mode, timeout) {
    const useCss = withCss;
    // Relative to the address the preview has: it sits below /proxy/PORT/.
    const url = new URL('index.html', page.url());
    url.search = mode === 'folie' ? '?full' : '';
    url.hash = mode === 'folie' ? '#beispiel' : '';
    await page.goto(url.href, { waitUntil: 'load', timeout });
    await page.evaluate(async ([mode, pageFrame, slideFrame, useCss]) => {
        const read = async (name) => {
            const response = await fetch(name, { cache: 'no-store' });
            return response.ok ? response.text() : '';
        };
        const [html, css] = await Promise.all([read('beispiel.html'), useCss ? read('beispiel.css') : '']);
        let source;
        if (mode === 'folie') {
            // An example that is a slide itself (the Shower page) keeps its
            // own <section>; everything else goes onto one.
            const slide = /<section\b[^>]*class="[^"]*\bslide\b/.test(html)
                ? html.replace(/<section\b/, '<section id="beispiel"')
                : `<section class="slide" id="beispiel">\n${html}\n</section>`;
            source = slideFrame.replace('%%SLIDE%%', () => slide);
        } else if (/<html\b/i.test(html)) {
            // A whole page (the skeleton of a page) is shown as it is, only
            // enlarged like the other examples (22 instead of 16 pixels), so
            // that it can be read in the small picture.
            const zoom = '<style>html { zoom: 1.375; }</style>';
            source = html.replace(/<\/head>/i, () => `${zoom}${css ? `<style>${css}</style>` : ''}</head>`);
        } else {
            source = pageFrame.replace('%%HTML%%', () => html);
        }
        source = source.replace('%%CSS%%', () => css);
        document.open();
        document.write(source);
        document.close();
    }, [mode, PAGE, SLIDE, useCss]);
    // Fonts and pictures are there, and Shower has laid out the slide.
    await page.waitForFunction((mode) => {
        if (document.readyState !== 'complete') return false;
        if ([...document.images].some((img) => !img.complete)) return false;
        if (mode === 'folie' && !document.querySelector('.shower.full .slide.active')) return false;
        return document.fonts.status === 'loaded';
    }, mode, { timeout, polling: 100 });
    await page.waitForTimeout(300);
}

export default {
    // The Shower template is cloned to /workspace/shower.js; the examples
    // are written next to its index.html.
    writeFileSubdirectory: 'shower.js',

    actions: {
        // hook: drag-over NAME: a file from outside is dragged over the
        // Explorer row NAME (a folder), and stays there for the picture
        async 'drag-over'({ page, args }) {
            const row = explorerRow(page, String(args).trim());
            await row.waitFor({ state: 'visible', timeout: readyTimeout });
            await dragEvents(page, row, ['dragenter', 'dragover']);
            await page.waitForTimeout(300);
        },

        // hook: drag-leave NAME: the file is taken away again, nothing dropped
        async 'drag-leave'({ page, args }) {
            await dragEvents(page, explorerRow(page, String(args).trim()), ['dragleave', 'dragend']);
        },

        // hook: page PATH: the preview shows the page PATH next to this file
        // (bilder/kopieren.html), its pictures embedded, without writing
        // anything into the Workspace
        async page({ page, args }) {
            const file = new URL(String(args).trim(), import.meta.url);
            let html = await readFile(file, 'utf8');
            for (const [, src] of html.matchAll(/src="([^"]+\.jpg)"/g)) {
                const bytes = await readFile(new URL(src, file));
                html = html.replace(`src="${src}"`, `src="data:image/jpeg;base64,${bytes.toString('base64')}"`);
            }
            await page.setContent(html, { waitUntil: 'load' });
            await page.waitForFunction(() => [...document.images].every((img) => img.complete), null, { timeout: readyTimeout });
        },

        // hook: clipboard-image PATH: a picture next to this file, on the
        // clipboard as PNG, the way »Copy image« in a browser puts it there
        async 'clipboard-image'({ page, args }) {
            const bytes = await readFile(new URL(String(args).trim(), import.meta.url));
            await page.context().grantPermissions(['clipboard-read', 'clipboard-write'], { origin: new URL(page.url()).origin });
            await page.bringToFront();
            await page.evaluate(async (base64) => {
                const raw = Uint8Array.from(atob(base64), (c) => c.charCodeAt(0));
                const bitmap = await createImageBitmap(new Blob([raw], { type: 'image/jpeg' }));
                const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
                canvas.getContext('2d').drawImage(bitmap, 0, 0);
                const png = await canvas.convertToBlob({ type: 'image/png' });
                await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })]);
            }, bytes.toString('base64'));
        },

        // hook: beispiel [css] [folie]: the preview shows beispiel.html (and
        // with css also beispiel.css) as a page, or with folie as a slide:
        // for the Shower page, whose examples only make sense on a slide
        // (no variant then).
        async beispiel({ page, args, timeout }) {
            const words = String(args || '').split(/\s+/).filter(Boolean);
            if (words.some((word) => !['css', 'folie'].includes(word))) {
                throw new Error(`hook: beispiel [css] [folie], got: ${args}`);
            }
            withCss = words.includes('css');
            await render(page, words.includes('folie') ? 'folie' : 'seite', timeout ?? readyTimeout);
        },
    },

    variants: {
        async folie({ page, on, timeout }) {
            await render(page, on ? 'folie' : 'seite', timeout ?? readyTimeout);
        },
    },
};
