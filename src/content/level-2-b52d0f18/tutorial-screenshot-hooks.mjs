// Screenshot hooks for the Backrooms (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«).
//
// The preview is the starter's index.html (specht/backrooms-starter). It
// bakes the light on the graphics card: seconds on the host's GPU, a minute
// or two in software WebGL, so waiting for the light has its own, long
// limit. The Lobby brings its light baked (lobby/level.light).
//
// Every page is opened with ?sharp: the engine otherwise renders at most at
// twice the page's pixels, and the pictures (zoom 1.5) have almost five.

const lightTimeout = 15 * 60_000;

// Waits until the status line matches; says in the log every 30 s what it
// says meanwhile, so that a bake that hangs shows where.
async function waitForStatus(page, pattern, timeout) {
    const started = Date.now();
    const report = setInterval(async () => {
        const text = await page.evaluate(async () => {
            // frames in one second, and the card WebGL draws on
            let frames = 0;
            const count = () => { frames += 1; if (performance.now() - t0 < 1000) requestAnimationFrame(count); };
            const t0 = performance.now();
            requestAnimationFrame(count);
            await new Promise((resolve) => setTimeout(resolve, 1100));
            const gl = document.querySelector('canvas')?.getContext('webgl2');
            const info = gl?.getExtension('WEBGL_debug_renderer_info');
            const gpu = info ? gl.getParameter(info.UNMASKED_RENDERER_WEBGL) : '?';
            return `${document.getElementById('status')?.textContent ?? ''} | ${frames} frames/s | ${document.visibilityState} | ${gpu}`;
        }).catch((error) => `? ${error.message}`);
        console.log(`  waiting ${Math.round((Date.now() - started) / 1000)} s for /${pattern}/: ${text}`);
    }, 30_000);
    try {
        await page.waitForFunction(
            (source) => new RegExp(source).test(document.getElementById('status')?.textContent ?? ''),
            pattern,
            { timeout, polling: 250 },
        );
    } finally {
        clearInterval(report);
    }
}

export default {
    // 3D scenes: on the graphics card, if the server has one
    gpu: true,

    // The template is cloned to /workspace/backrooms-starter.
    writeFileSubdirectory: 'backrooms-starter',

    // The page has loaded its level: the status line says something. The
    // first load (go-live) comes without ?sharp; it is reloaded with it.
    async waitForPreview({ page, timeout }) {
        const url = new URL(page.url());
        if (!url.searchParams.has('sharp')) {
            url.search = url.search ? `${url.search}&sharp` : '?sharp';
            await page.goto(url.href);
        }
        await page.waitForSelector('#status', { timeout });
    },

    actions: {
        // hook: open QUERY loads index.html?QUERY in the preview, for another
        // place (view=x,z,yaw,pitch) or the Lobby (level=lobby/level.ini)
        // Live Server reloads the page by itself when a recipe has just
        // written a file (write-file); wait for that first, or it would come
        // in the middle of baking.
        async open({ page, args }) {
            await page.waitForTimeout(1200);
            const url = new URL(page.url());
            url.pathname = url.pathname.replace(/[^/]*$/, 'index.html');
            url.search = args ? `?sharp&${args}` : '?sharp';
            await page.goto(url.href);
            await page.waitForSelector('#status');
        },

        // hook: page FILE?QUERY opens another page of the starter in the
        // preview, such as textures.html?show=marble&bare
        async page({ page, args }) {
            await page.waitForTimeout(1200);
            const url = new URL(page.url());
            const [file, query = ''] = args.trim().split('?');
            url.pathname = url.pathname.replace(/[^/]*$/, file);
            url.search = query ? `?${query}` : '';
            await page.goto(url.href);
            await page.waitForSelector('canvas');
            await page.waitForTimeout(1000);
        },

        // hook: baking waits until the light is being baked (the first bounce,
        // the progress bar about a third full) and stops the page's animation
        // loop there: on a GPU the whole bake takes a few seconds, the picture
        // would show it finished. The frozen frame stays on screen; the next
        // shot opens the page again (hook: open). With ?rebake: light this
        // browser kept from an earlier picture would otherwise just be loaded.
        async baking({ page }) {
            const url = new URL(page.url());
            if (!url.searchParams.has('rebake')) {
                url.searchParams.set('rebake', '');
                await page.goto(url.href);
                await page.waitForSelector('#status');
            }
            // checked every frame and frozen in the same one: on a fast card
            // »bounce 1« stands there only for a few frames, and a check every
            // 250 ms (waitForStatus) missed it and waited until the timeout
            await page.waitForFunction(() => {
                if (!/Baking light: bounce 1/.test(document.getElementById('status')?.textContent ?? '')) return false;
                window.requestAnimationFrame = () => 0;
                return true;
            }, null, { timeout: lightTimeout, polling: 'raf' });
            await page.waitForTimeout(300);
        },

        // hook: light waits until the light is baked or loaded and every lamp is
        // on, then turns the flickering off: no picture catches a lamp dark.
        // hook: light flicker leaves it on, for a picture that shows the switches.
        // If the page is reloaded after the light was there (Live Server, late),
        // it bakes again: then wait for its light once more.
        async light({ page, args }) {
            for (let attempt = 0; attempt < 5; attempt++) {
                await waitForStatus(page, 'Light baked|Light loaded', lightTimeout);
                await page.evaluate(() => { window.lightSeen = true; });
                await page.waitForTimeout(3500);
                const same = await page.evaluate(() => window.lightSeen === true
                    && /Light baked|Light loaded/.test(document.getElementById('status')?.textContent ?? '')).catch(() => false);
                if (same) break;
            }
            if (args.trim() === 'flicker') return;
            await page.evaluate(() => {
                const flicker = [...document.querySelectorAll('#controls .flag')]
                    .find((button) => button.textContent.startsWith('Flicker'));
                if (flicker?.classList.contains('on')) flicker.click();
            });
            await page.waitForTimeout(500);
        },

        // hook: key KEY [KEY …] presses keys in the preview, as Playwright names them
        async key({ page, args }) {
            for (const key of args.split(/\s+/).filter(Boolean)) {
                await page.keyboard.press(key);
                await page.waitForTimeout(300);
            }
            await page.waitForTimeout(1500);   // the Automap flies there in a second
        },

        // hook: wheel N turns the mouse wheel over the picture: N > 0 zooms the
        // Automap out (distance times e^(N/1000))
        async wheel({ page, args }) {
            const size = page.viewportSize();
            await page.mouse.move(size.width / 2, size.height / 2);
            await page.mouse.wheel(0, Number(args));
            await page.waitForTimeout(500);
        },

        // hook: info off|on hides or shows the box at the top left
        async info({ page, args }) {
            await page.evaluate((show) => {
                document.getElementById('info').style.visibility = show ? '' : 'hidden';
            }, args.trim() !== 'off');
        },
    },
};
