// Screenshot hooks for »Eine JPEG-Datei decodieren« (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«).
//
// The recipes write the reference decoder (loesung/jpeg.c) and the example
// photo into the cloned PixelRAM starter, run make in the Workspace terminal
// and look at the decoded picture in the Live Server preview. The decoder
// draws once and stops, so there is no clock to hold, unlike in the
// PixelRAM tutorial whose open hook this is.

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export default {
    writeFileSubdirectory: 'pixelram-starter',

    // Live Server opens the folder listing; the program is opened by hook.
    async waitForPreview({ page, timeout }) {
        await page.waitForLoadState('domcontentloaded', { timeout });
    },

    actions: {
        // hook: open FILE: shows FILE (relative to the opened folder) in the
        // preview and waits until the program has drawn. Live Server reloads
        // the page by itself once make has written a file, sometimes only
        // after this navigation: the page has to stay put for two seconds,
        // otherwise the reloaded page is waited for again.
        async open({ page, args, timeout }) {
            const file = args.trim();
            if (!/^[\w./-]+\.html$/.test(file) || file.includes('..')) throw new Error('hook: open FILE.html');
            const url = new URL(file, new URL('/', page.url())).href;
            // make may still be writing the file: Live Server answers 404
            // until it is there.
            const deadline = Date.now() + timeout;
            for (;;) {
                const response = await page.goto(url, { waitUntil: 'domcontentloaded', timeout });
                if (response?.status() !== 404) break;
                if (Date.now() > deadline) throw new Error(`hook: open ${file}: still 404`);
                await sleep(1000);
            }
            for (let attempt = 0; ; attempt += 1) {
                await page.waitForSelector('#canvas', { timeout });
                let navigated = false;
                const onNavigation = (frame) => {
                    if (frame === page.mainFrame()) navigated = true;
                };
                page.on('framenavigated', onNavigation);
                await sleep(2000);
                page.off('framenavigated', onNavigation);
                if (!navigated) break;
                if (attempt === 3) throw new Error(`hook: open ${file}: the page keeps reloading`);
                await page.waitForLoadState('domcontentloaded', { timeout });
            }
            // The display buttons appear while the mouse is over the screen.
            await page.mouse.move(0, 0);
        },
    },
};
