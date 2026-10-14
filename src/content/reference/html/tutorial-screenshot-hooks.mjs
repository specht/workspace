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

const readyTimeout = 60_000;

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
