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

    // variants: dark: the poster follows the colour scheme (hook: poster)
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },

    actions: {
        // hook: poster X Y: the picture for the card, over the whole window
        // of /jpeg: a part of the photo with the 8 × 8 Y block at X, Y
        // marked, the block enlarged with its values, an arrow »DCT« and
        // its coefficients (dequantised) with the zigzag up to the last one
        // that is not 0. Drawn with jpeg_image.js from 44-baseline.jpg, in
        // the colours of the scheme, again when the scheme changes.
        async poster({ page, args }) {
            const [x, y] = args.trim().split(/\s+/).map(Number);
            await page.waitForFunction(() => window.JpegImage);
            await page.evaluate(async ([bxPx, byPx]) => {
                const image = await JpegImage.load('/jpeg/44-baseline.jpg');
                await document.fonts.ready;
                const W = 1600, H = 900;
                const size = { w: window.innerWidth, h: window.innerHeight };
                const canvas = document.createElement('canvas');
                canvas.id = 'jpeg-poster';
                canvas.width = Math.round(size.w * 2);
                canvas.height = Math.round(size.w * 2 * H / W);
                canvas.style.cssText = `position:fixed;left:0;top:0;width:${size.w}px;height:${size.w * H / W}px;z-index:10000`;
                document.body.appendChild(canvas);
                const block = image.block(0, Math.floor(bxPx / 16), Math.floor(byPx / 16), (Math.floor(byPx / 8) % 2) * 2 + (Math.floor(bxPx / 8) % 2));
                const plane = image.comps[0];
                const pixels = Array.from({ length: 64 }, (_, i) => plane.plane[(byPx + Math.floor(i / 8)) * plane.planeWidth + bxPx + i % 8]);
                const minus = (v) => String(v).replace('-', '−');
                const font = (px, weight, mono) => `${weight} ${px}px ${mono ? "'0xProto Nerd Font Mono', monospace" : "'IBM Plex Sans', sans-serif"}`;
                function draw() {
                    const dark = matchMedia('(prefers-color-scheme: dark)').matches;
                    const c = dark
                        ? { bg: '#18181b', ink: '#e9e7e2', accent: [177, 151, 252], line: 'rgba(177, 151, 252, 0.6)', zero: 'rgba(255, 255, 255, 0.05)', zigzag: 'rgba(233, 231, 226, 0.22)', on: () => '#fff' }
                        : { bg: '#f6f5f2', ink: '#25232b', accent: [112, 72, 232], line: 'rgba(112, 72, 232, 0.55)', zero: 'rgba(0, 0, 0, 0.05)', zigzag: 'rgba(37, 35, 43, 0.2)', on: (a) => a > 0.5 ? '#fff' : '#25232b' };
                    const rgba = (a) => `rgba(${c.accent.join(', ')}, ${a})`;
                    const ctx = canvas.getContext('2d');
                    ctx.setTransform(canvas.width / W, 0, 0, canvas.width / W, 0, 0);
                    ctx.fillStyle = c.bg;
                    ctx.fillRect(0, 0, W, H);
                    // the photo around the block, four times larger
                    const zoom = 4, sw = 120, sh = 190;
                    const sx = Math.max(0, Math.min(image.width - sw, bxPx - 56)), sy = Math.max(0, Math.min(image.height - sh, byPx - 92));
                    const px = 70, py = (H - sh * zoom) / 2;
                    ctx.imageSmoothingEnabled = false;
                    ctx.drawImage(image.photo(), sx, sy, sw, sh, px, py, sw * zoom, sh * zoom);
                    const bx = px + (bxPx - sx) * zoom, by = py + (byPx - sy) * zoom;
                    // the block enlarged, with its values
                    const cell = 46, gx = 640, gy = (H - 8 * cell) / 2;
                    ctx.strokeStyle = c.line;
                    ctx.lineWidth = 1.5;
                    ctx.beginPath();
                    ctx.moveTo(bx + 8 * zoom, by); ctx.lineTo(gx, gy);
                    ctx.moveTo(bx + 8 * zoom, by + 8 * zoom); ctx.lineTo(gx, gy + 8 * cell);
                    ctx.stroke();
                    ctx.strokeStyle = rgba(1);
                    ctx.lineWidth = 3;
                    ctx.strokeRect(bx, by, 8 * zoom, 8 * zoom);
                    ctx.textAlign = 'center';
                    ctx.textBaseline = 'middle';
                    ctx.font = font(15, 500, true);
                    pixels.forEach((v, i) => {
                        const x0 = gx + (i % 8) * cell, y0 = gy + Math.floor(i / 8) * cell;
                        ctx.fillStyle = `rgb(${v}, ${v}, ${v})`;
                        ctx.fillRect(x0, y0, cell, cell);
                        ctx.fillStyle = v > 120 ? '#222' : '#eee';
                        ctx.fillText(v, x0 + cell / 2, y0 + cell / 2);
                    });
                    ctx.strokeRect(gx, gy, 8 * cell, 8 * cell);
                    // the arrow
                    const ax = gx + 8 * cell + 28, ay = H / 2;
                    ctx.strokeStyle = c.ink;
                    ctx.fillStyle = c.ink;
                    ctx.beginPath(); ctx.moveTo(ax, ay); ctx.lineTo(ax + 78, ay); ctx.stroke();
                    ctx.beginPath(); ctx.moveTo(ax + 92, ay); ctx.lineTo(ax + 74, ay - 10); ctx.lineTo(ax + 74, ay + 10); ctx.fill();
                    ctx.font = font(26, 600, false);
                    ctx.fillText('DCT', ax + 46, ay - 28);
                    // the coefficients, the zigzag through them
                    const cx = ax + 120, cy = gy;
                    const alpha = (v) => v ? Math.min(0.9, 0.25 + Math.abs(v) / 400) : 0;
                    block.forEach((v, i) => {
                        ctx.fillStyle = v ? rgba(alpha(v)) : c.zero;
                        ctx.fillRect(cx + (i % 8) * cell + 1, cy + Math.floor(i / 8) * cell + 1, cell - 2, cell - 2);
                    });
                    let last = 0;
                    JpegImage.ZIGZAG.forEach((n, k) => { if (block[n]) last = k; });
                    ctx.strokeStyle = c.zigzag;
                    ctx.lineWidth = 2;
                    ctx.beginPath();
                    JpegImage.ZIGZAG.slice(0, last + 1).forEach((n, k) => {
                        const x0 = cx + (n % 8 + 0.5) * cell, y0 = cy + (Math.floor(n / 8) + 0.5) * cell;
                        if (k) ctx.lineTo(x0, y0); else ctx.moveTo(x0, y0);
                    });
                    ctx.stroke();
                    ctx.font = font(15, 600, true);
                    block.forEach((v, i) => {
                        if (!v) return;
                        ctx.fillStyle = c.on(alpha(v));
                        ctx.fillText(minus(v), cx + (i % 8 + 0.5) * cell, cy + (Math.floor(i / 8) + 0.5) * cell);
                    });
                }
                draw();
                matchMedia('(prefers-color-scheme: dark)').addEventListener('change', draw);
            }, [x, y]);
            await page.mouse.move(0, 0);
            await page.waitForTimeout(500);
        },

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
