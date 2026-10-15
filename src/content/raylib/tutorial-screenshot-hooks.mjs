// Screenshot hooks for »Eine Klötzchenwelt in 3D« (src/tutorial-screenshots/
// README.md, »Tutorial-specific hooks«).
//
// The recipes write each stage of the program (stufen/N.c) into the cloned
// starter, run make in the Workspace terminal and look at the result in the
// Live Server preview. The page around the program is PixelRAM's shell, so
// opening a program works as in the PixelRAM tutorial. Unlike PixelRAM, the
// clock must never be stopped here (see hook: settle).

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

async function settleFrames(page, count) {
    await page.evaluate((n) => new Promise((resolve) => {
        const step = (left) => (left === 0 ? resolve() : requestAnimationFrame(() => step(left - 1)));
        step(n);
    }), count);
}

// A key that the program reads once per frame (IsKeyPressed) has to stay
// down for a few frames: frames come slower while the generator is busy.
async function tap(page, key) {
    await page.keyboard.down(key);
    await sleep(150);
    await page.keyboard.up(key);
    await sleep(100);
}

export default {
    writeFileSubdirectory: 'raylib-starter',

    // Live Server opens the folder listing; the program is opened by hook.
    async waitForPreview({ page, timeout }) {
        await page.waitForLoadState('domcontentloaded', { timeout });
    },

    // variants: dark: the same moment at night. N jumps between day and
    // night (from stage 7 on), so pressing it again brings the day back.
    variants: {
        async dark({ page, on }) {
            const night = await page.evaluate(() => window.__raylibNight === true);
            if (night === on) return;
            await page.evaluate((v) => { window.__raylibNight = v; }, on);
            await tap(page, 'KeyN');
            await settleFrames(page, 4);
        },
    },

    actions: {
        // hook: open FILE: shows FILE (relative to the opened folder) in the
        // preview and waits until the program has drawn for a moment. Live
        // Server reloads the page by itself once make has written a file,
        // sometimes only after this navigation: the page has to stay put for
        // two seconds, otherwise the reloaded page is waited for again.
        async open({ page, args, timeout }) {
            const file = args.trim();
            if (!/^[\w./-]+\.html$/.test(file) || file.includes('..')) throw new Error('hook: open FILE.html');
            const url = new URL(file, new URL('/', page.url())).href;
            const deadline = Date.now() + timeout;
            for (;;) {
                let response;
                try {
                    response = await page.goto(url, { waitUntil: 'domcontentloaded', timeout });
                } catch (error) {
                    // Live Server reloaded the page just as it was opened
                    // (make has written a file): open it again.
                    if (!/interrupted by another navigation/.test(error.message) || Date.now() > deadline) throw error;
                    await sleep(1000);
                    continue;
                }
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
            await page.evaluate(() => { window.__raylibNight = false; });
            // The display buttons appear while the mouse is over the screen.
            await page.mouse.move(0, 0);
        },

        // hook: play TIMELINE: tokens one after the other. »KeyW:1.5« holds a
        // key for 1.5 seconds, »Digit3« taps it, »Rechtsklick« clicks the
        // right mouse button in the middle of the picture (where the
        // crosshair is), a plain number waits that many seconds.
        async play({ page, args }) {
            const tokens = args.trim().split(/\s+/).filter(Boolean);
            if (tokens.length === 0) throw new Error('hook: play TOKENS');
            for (const token of tokens) {
                if (/^\d+(\.\d+)?$/.test(token)) {
                    const seconds = Number(token);
                    if (seconds > 30) throw new Error(`hook: play waits at most 30 s, not ${token}`);
                    await sleep(seconds * 1000);
                } else if (token === 'Rechtsklick') {
                    const box = await page.locator('#canvas').boundingBox();
                    if (!box) throw new Error('hook: play Rechtsklick: no #canvas');
                    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
                    await page.mouse.down({ button: 'right' });
                    await sleep(150);
                    await page.mouse.up({ button: 'right' });
                    await sleep(100);
                    await page.mouse.move(0, 0);
                } else {
                    const [key, hold] = token.split(':');
                    if (hold === undefined) {
                        await tap(page, key);
                    } else {
                        const seconds = Number(hold);
                        if (!(seconds > 0 && seconds <= 10)) throw new Error(`hook: play holds 0-10 s, not ${token}`);
                        await page.keyboard.down(key);
                        await sleep(seconds * 1000);
                        await page.keyboard.up(key);
                    }
                }
            }
        },

        // hook: studio FILE: the 2D Game Studio (2d.hackschule.de) in the
        // preview tab, with FILE (below src/content, e.g. the starter's
        // texturen.png) imported as the frames of a sprite »Kacheln«, the way
        // a student would have drawn them. Nothing is saved.
        async studio({ page, args, timeout }) {
            const file = args.trim();
            if (!/^[\w./-]+\.png$/.test(file) || file.includes('..')) throw new Error('hook: studio FILE.png');
            // The studio keeps unsaved work in this browser (rescue.js,
            // IndexedDB »2d-studio«) and asks about it on the next visit: the
            // import of the last render would block the menu. Start empty,
            // like a first visit.
            if (!page.__studioFresh) {
                await page.addInitScript(() => {
                    if (location.hostname === '2d.hackschule.de') indexedDB.deleteDatabase('2d-studio');
                });
                page.__studioFresh = true;
            }
            await page.goto('https://2d.hackschule.de/', { waitUntil: 'networkidle', timeout });
            await page.waitForSelector('#functions_dropdown .item', { timeout });
            await sleep(1000);
            await page.locator('#functions_dropdown .item', { hasText: 'Sprite' }).first().click();
            const [chooser] = await Promise.all([
                page.waitForEvent('filechooser', { timeout }),
                page.locator('.context-menu').getByText('Bilder öffnen …', { exact: true }).click(),
            ]);
            await chooser.setFiles(`/content/${file}`);
            await page.waitForSelector('#image_import_cols', { state: 'visible', timeout });
            // a strip of square tiles: one pixel per pixel, the tiles side by side
            const height = await page.evaluate(() => window.image_import_dialog.image.height);
            const width = await page.evaluate(() => window.image_import_dialog.image.width);
            for (const [field, value] of [['#image_import_scale', 1], ['#image_import_cols', width / height], ['#image_import_rows', 1]]) {
                await page.fill(field, String(value));
                await page.locator(field).dispatchEvent('change');
            }
            await sleep(500);
            await page.locator('.modal-footer button', { hasText: 'Einfügen' }).click();
            await sleep(800);
            const title = page.locator('input:visible').first();
            await title.fill('Kacheln');
            await title.press('Enter');
            await page.mouse.move(0, 0);
            await sleep(500);
        },

        // hook: studio-menu: Funktionen › Sprite › Als Bild speichern, open,
        // the pointer on »alle Frames dieses Zustands nebeneinander«.
        async 'studio-menu'({ page }) {
            await page.locator('#functions_dropdown .item', { hasText: 'Sprite' }).first().click();
            await sleep(300);
            await page.locator('.context-menu').getByText('Als Bild speichern', { exact: true }).hover();
            await sleep(400);
            await page.locator('.context-menu').getByText('alle Frames dieses Zustands nebeneinander', { exact: true }).hover();
            await sleep(600);
        },

        // hook: settle: lets the program draw a few more frames before the
        // shot. The clock keeps running: raylib waits for the next frame by
        // asking for the time (SetTargetFPS), so a stopped clock would hang the
        // page. Without input the picture stands still anyway, only the sun
        // moves (a day takes two minutes).
        async settle({ page }) {
            await sleep(500);
            await page.mouse.move(0, 0);
        },
    },
};
