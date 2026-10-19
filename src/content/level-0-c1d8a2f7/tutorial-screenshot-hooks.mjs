// Screenshot hooks for the Backrooms (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«).
//
// The preview is the starter's index.html (specht/backrooms-starter). It
// bakes the light on the graphics card; in the generator that is software
// WebGL and takes a minute or two for the small level, so waiting for the
// light has its own, long limit. The Lobby brings its light baked
// (lobby/level.light) and is there at once.

const lightTimeout = 15 * 60_000;

async function waitForStatus(page, pattern, timeout) {
    await page.waitForFunction(
        (source) => new RegExp(source).test(document.getElementById('status')?.textContent ?? ''),
        pattern,
        { timeout },
    );
}

export default {
    // The template is cloned to /workspace/backrooms-starter.
    writeFileSubdirectory: 'backrooms-starter',

    // The page has loaded its level: the status line says something.
    async waitForPreview({ page, timeout }) {
        await page.waitForSelector('#status', { timeout });
    },

    actions: {
        // hook: open QUERY loads index.html?QUERY in the preview, for another
        // place (view=x,z,yaw,pitch) or the Lobby (level=lobby/level.ini)
        async open({ page, args }) {
            const url = new URL(page.url());
            url.pathname = url.pathname.replace(/[^/]*$/, 'index.html');
            url.search = args ? `?${args}` : '';
            await page.goto(url.href);
            await page.waitForSelector('#status');
        },

        // hook: baking waits until the light is being baked (the progress bar runs)
        async baking({ page }) {
            await waitForStatus(page, 'Baking light: bounce', lightTimeout);
        },

        // hook: light waits until the light is baked or loaded and every lamp is
        // on, then turns the flickering off: no picture catches a lamp dark.
        // hook: light flicker leaves it on, for a picture that shows the switches.
        async light({ page, args }) {
            await waitForStatus(page, 'Light baked|Light loaded', lightTimeout);
            await page.waitForTimeout(3500);
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
