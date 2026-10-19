// The fireworks page uses the same starter and the same preview as
// »Echtzeitgrafik mit C«, so it shares that page's hooks (open, freeze, play,
// at, the frozen clock for videos and the CRT variant). Only this file's
// hash reaches the screenshot state: after changing the PixelRAM hooks,
// render this page with --force.
//
// It adds one action. The fireworks are a simulation: every frame moves the
// sparks by the time since the last one, so »at« (one jump to the moment)
// would move them all at once. »run« goes there in steps of one frame.
import base from '../pixelram/tutorial-screenshot-hooks.mjs';

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

async function nextFrames(page, count) {
    await page.evaluate((n) => new Promise((resolve) => {
        const step = (left) => (left === 0 ? resolve() : requestAnimationFrame(() => step(left - 1)));
        step(n);
    }), count);
}

export default {
    ...base,
    // Its own clone of the starter (feuerwerk.md), not pixelram-starter.
    writeFileSubdirectory: 'feuerwerk',
    actions: {
        ...base.actions,

        // hook: run SECONDS: loads the program again with its clock stopped
        // from the first moment (only with ?pixelram-clock), then lets it run
        // SECONDS in steps of 1/60 s, two animation frames each, and stops.
        // rand() without srand() gives the same numbers every time, so the
        // picture is the same on every render.
        async run({ page, args, timeout }) {
            const seconds = Number(args);
            if (!(seconds > 0 && seconds <= 60)) throw new Error('hook: run SECONDS (up to 60)');
            if (!page.__fireworksClockScript) {
                await page.addInitScript(() => {
                    if (!new URLSearchParams(window.location.search).has('pixelram-clock')) return;
                    window.__pixelramNow = performance.now();
                    performance.now = () => window.__pixelramNow;
                });
                page.__fireworksClockScript = true;
            }
            const url = new URL(page.url());
            url.searchParams.set('pixelram-clock', '');
            await page.goto(url.href, { waitUntil: 'domcontentloaded', timeout });
            await page.waitForSelector('#canvas', { timeout });
            await sleep(1500);
            for (let frame = 0; frame < Math.round(seconds * 60); frame += 1) {
                await page.evaluate(() => { window.__pixelramNow += 1000 / 60; });
                await nextFrames(page, 2);
            }
            await page.mouse.move(0, 0);
        },
    },
};
