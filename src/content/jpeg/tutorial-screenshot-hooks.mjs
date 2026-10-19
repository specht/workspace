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
    writeFileSubdirectory: 'jpeg',

    // Live Server opens the folder listing; the program is opened by hook.
    async waitForPreview({ page, timeout }) {
        await page.waitForLoadState('domcontentloaded', { timeout });
    },

    actions: {
        // hook: knob LABEL VALUE: on the pages with knobs (/rgb, /dct), the
        // knob LABEL dragged up or down, as with the mouse, until it shows
        // VALUE (»knob Mode YCbCr«, »knob R 255«)
        async knob({ page, args }) {
            const [label, value] = args.trim().split(/\s+/);
            const svg = page.locator(`svg[aria-label="${label}"]`);
            const shown = async () => (await svg.locator('xpath=..').locator('.knob-value').textContent()).trim();
            const box = await svg.boundingBox();
            const x = box.x + box.width / 2;
            const y = box.y + box.height / 2;
            for (const direction of [1, -1]) {
                if (await shown() === value) return;
                await page.mouse.move(x, y);
                await page.mouse.down();
                for (let step = 1; step <= 600; step += 1) {
                    await page.mouse.move(x, y - direction * step);
                    if (await shown() === value) break;
                }
                await page.mouse.up();
            }
            if (await shown() !== value) throw new Error(`hook: knob ${label}: ${value} not reached, it shows ${await shown()}`);
        },

        // hook: only SELECTOR [WHEEL]: that panel alone fills the window,
        // without its title and its hint, drawn again at that size; with
        // WHEEL the mouse wheel turned over its middle (negative zooms in),
        // then the mouse out of the way
        async only({ page, args }) {
            const [selector, wheel] = args.trim().split(/\s+/);
            await page.addStyleTag({ content: `${selector} { position: fixed !important; left: 0 !important; top: 0 !important; width: 100vw !important; height: 100vh !important; z-index: 100; margin: 0 !important; border-radius: 0 !important; } ${selector} .panel-title, ${selector} .result-label { display: none !important; }` });
            await page.evaluate(() => window.dispatchEvent(new Event('resize')));
            await page.waitForTimeout(800);
            if (wheel) {
                const size = page.viewportSize();
                await page.mouse.move(size.width / 2, size.height / 2);
                await page.mouse.wheel(0, Number(wheel));
                await page.waitForTimeout(300);
            }
            await page.mouse.move(2, 2);
            await page.waitForTimeout(1200);
        },

        // hook: dct-select X Y [1d|2d] [COEFFICIENTS]: on the page »DCT«
        // (/dct), the mode and the number of coefficients set with their
        // knobs, and the eight pixels (1d) or the 8 x 8 block (2d) from X, Y of
        // the photo selected, as a click there selects them. A click cannot
        // aim that well: eight pixels of the photo are about one on the
        // screen.
        async 'dct-select'({ page, args }) {
            const [x, y, mode = '1d', count] = args.trim().split(/\s+/);
            await page.waitForFunction(() => typeof selectionState !== 'undefined' && width > 1 && coefficientsKnob);
            await page.evaluate(([x, y, mode, count]) => {
                knobs.find((knob) => knob.opts.label === 'Mode').setValue(mode === '2d' ? 2 : 1, true);
                if (count) coefficientsKnob.setValue(count, true);
                selectionState.hover = clampSelectionOrigin(x, y, 8, mode === '2d' ? 8 : 1);
                commitHoverSelection();
            }, [Number(x), Number(y), mode, count ? Number(count) : null]);
            await page.waitForTimeout(1500);
        },

        // hook: expect-value PREFIX EXPECTED TOLERANCE: the number the
        // program prints after PREFIX must be EXPECTED, give or take
        // TOLERANCE, as the task that asks for it accepts. PREFIX has no
        // spaces; write »Blau« for »… Blau 108.9«.
        async 'expect-value'({ page, args, timeout }) {
            const [prefix, expected, tolerance = '0'] = args.trim().split(/\s+/);
            const pattern = new RegExp(`${prefix.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\s+(-?\\d+(?:\\.\\d+)?)`);
            const deadline = Date.now() + timeout;
            for (;;) {
                const output = await page.evaluate(() => document.getElementById('log-output')?.textContent || '');
                const match = output.match(pattern);
                if (match) {
                    const value = Number(match[1]);
                    if (Math.abs(value - Number(expected)) <= Number(tolerance)) return;
                    throw new Error(`hook: expect-value: ${prefix} ${value}, the task expects ${expected} ± ${tolerance}`);
                }
                if (Date.now() > deadline) {
                    throw new Error(`hook: expect-value: no »${prefix}« printed; the console shows: ${output.slice(-400) || '(nothing)'}`);
                }
                await sleep(500);
            }
        },

        // hook: pixel-print X Y: writes the reference decoder with the line
        // the task »pixel« asks for: the colour of one pixel, printed after
        // decoding.
        async 'pixel-print'({ args, writeFile }) {
            const [x, y] = args.trim().split(/\s+/).map(Number);
            const fs = await import('node:fs/promises');
            const source = await fs.readFile(new URL('loesung/jpeg.c', import.meta.url), 'utf8');
            const anchor = '    print_average();\n    return 0;\n}';
            if (!source.includes(anchor)) throw new Error('hook: pixel-print: end of main not found in loesung/jpeg.c');
            await writeFile('jpeg.c', source.replace(anchor,
                `    uint8_t r, g, b;\n    get_pixel_rgb(${x}, ${y}, &r, &g, &b);\n    printf("Rot bei ${x}, ${y}: %d\\n", r);\n` + anchor));
        },

        // hook: run ARGS: clears the console and starts the program again
        // with these arguments, as the console's »run« does (the console may
        // be closed).
        async run({ page, args }) {
            await page.evaluate((value) => {
                // A clean console: what the next checks read is this run's.
                document.getElementById('clear-log')?.click();
                document.getElementById('args-input').value = value;
                document.getElementById('args-run').click();
            }, args.trim());
            await sleep(500);
        },

        // hook: expect-output TEXT: waits until the program has printed TEXT
        // into its console (#log-output, also while the console is closed),
        // and otherwise fails with what it did print.
        async 'expect-output'({ page, args, timeout }) {
            const text = args.trim();
            const deadline = Date.now() + timeout;
            for (;;) {
                const output = await page.evaluate(() => document.getElementById('log-output')?.textContent || '');
                if (output.includes(text)) return;
                if (Date.now() > deadline) {
                    throw new Error(`hook: expect-output: »${text}« not printed; the console shows: ${output.slice(-400) || '(nothing)'}`);
                }
                await sleep(500);
            }
        },
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
