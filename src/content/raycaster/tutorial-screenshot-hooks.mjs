// Screenshot hooks for »Raycasting in C« (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«), taken from »Echtzeitgrafik mit C«.
//
// The recipes write a stage of the raycaster into ~/raycaster, run make in
// the Workspace terminal and look at the result in the Live Server preview.
// The raycaster moves with the time from seconds(); hook: hold walks with
// the keys held for so many seconds, as the stages' pictures were planned.

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// Two animation frames after the clock moved, the program has drawn and
// presented the frame for the new time.
async function settleFrames(page, count) {
    await page.evaluate((n) => new Promise((resolve) => {
        const step = (left) => (left === 0 ? resolve() : requestAnimationFrame(() => step(left - 1)));
        step(n);
    }), count);
}

export default {
    writeFileSubdirectory: 'raycaster',

    // Live Server opens the folder listing; the program is opened by hook.
    async waitForPreview({ page, timeout }) {
        await page.waitForLoadState('domcontentloaded', { timeout });
    },

    async setPreviewTime({ page, seconds }) {
        await page.evaluate((t) => {
            const clock = window.__pixelramClock || (window.__pixelramClock = {
                real: performance.now.bind(performance),
                start: null,
            });
            if (t === null) {
                performance.now = clock.real;
                clock.start = null;
                return;
            }
            if (clock.start === null) clock.start = clock.real();
            const frozen = clock.start + t * 1000;
            performance.now = () => frozen;
        }, seconds);
        await settleFrames(page, 3);
    },

    variants: {
        async crt({ page, on }) {
            const changed = await page.evaluate((wanted) => {
                if (document.body.classList.contains('crt') === wanted) return false;
                document.getElementById('crt-toggle').click();
                // The shell says »CRT ON« for a moment; not in the picture.
                document.getElementById('toast').style.visibility = 'hidden';
                return true;
            }, on);
            if (!changed) return;
            if (await page.evaluate(() => document.body.classList.contains('crt')) !== on) {
                throw new Error(`The CRT filter did not switch ${on ? 'on' : 'off'}`);
            }
            await page.mouse.move(0, 0);
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

        // hook: at SECONDS: shows the program exactly SECONDS after its
        // start and stops there, the same picture on every render. The page
        // is loaded again with the clock stopped from the first moment
        // (only with ?pixelram-clock, so later pages run normally); the
        // program draws its first picture and waits for time to pass. One
        // jump to SECONDS, then a step of one frame, and the picture for
        // SECONDS is on the screen.
        async at({ page, args, timeout }) {
            const seconds = Number(args);
            if (!(seconds >= 0 && seconds <= 600)) throw new Error('hook: at SECONDS (0-600)');
            if (!page.__pixelramClockScript) {
                await page.addInitScript(() => {
                    if (!new URLSearchParams(window.location.search).has('pixelram-clock')) return;
                    window.__pixelramNow = performance.now();
                    performance.now = () => window.__pixelramNow;
                });
                page.__pixelramClockScript = true;
            }
            const url = new URL(page.url());
            url.searchParams.set('pixelram-clock', '');
            await page.goto(url.href, { waitUntil: 'domcontentloaded', timeout });
            await page.waitForSelector('#canvas', { timeout });
            await sleep(1500);
            await page.evaluate((t) => { window.__pixelramNow += t * 1000; }, seconds);
            await settleFrames(page, 4);
            await page.evaluate(() => { window.__pixelramNow += 1000 / 60 + 1; });
            await settleFrames(page, 4);
            await page.mouse.move(0, 0);
        },

        // hook: aspect W H: the screen cut to W:H around its middle, so a
        // game with 320 x 200 pixels fits the row of 16:9 pictures beside it
        async aspect({ page, args }) {
            const [w, h] = String(args).trim().split(/\s+/).map(Number);
            if (!(w > 0 && h > 0)) throw new Error('hook: aspect W H');
            // The CRT filter draws on its own canvas over the same frame:
            // it gets the same cut, or the crt variant shows the whole screen.
            await page.addStyleTag({ content: `#canvas, #crt-canvas { aspect-ratio: ${w} / ${h} !important; height: auto !important; object-fit: cover !important; object-position: 50% 50% !important; }` });
            await settleFrames(page, 2);
        },

        // hook: freeze: stops the clock where it is. The program keeps its
        // last picture, so a variant shows the same moment as the shot.
        async freeze({ page }) {
            await page.evaluate(() => {
                const now = performance.now();
                performance.now = () => now;
            });
            await settleFrames(page, 4);
        },

        // hook: hold KEY SECONDS KEY SECONDS …: holds each key for so many
        // seconds, one after the other (Playwright key names); a key without
        // seconds is pressed once (Tab, KeyM). The raycaster moves with the
        // time, so this walks about as far as the same keys in the program.
        async hold({ page, args }) {
            const tokens = args.trim().split(/\s+/).filter(Boolean);
            if (tokens.length === 0) throw new Error('hook: hold KEY SECONDS …');
            for (let i = 0; i < tokens.length; i++) {
                const key = tokens[i];
                const next = tokens[i + 1];
                if (next !== undefined && /^\d+(\.\d+)?$/.test(next)) {
                    const seconds = Number(next);
                    if (seconds > 20) throw new Error(`hook: hold at most 20 s, not ${next}`);
                    await page.keyboard.down(key);
                    await sleep(seconds * 1000);
                    await page.keyboard.up(key);
                    i++;
                } else {
                    // held for a few frames: a press within one frame would
                    // never reach the program's key_pressed
                    await page.keyboard.down(key);
                    await sleep(120);
                    await page.keyboard.up(key);
                }
                await sleep(100);
            }
            await sleep(300);
        },

        // hook: play TIMELINE: numbers wait that many seconds, keys between
        // two numbers are pressed together (Playwright key names), e.g.
        // »Space 0.3 KeyW ArrowDown 0.8 KeyD«. A program reads a key once
        // per frame, and frames come slower while the generator is busy, so
        // each press lasts 250 ms.
        async play({ page, args }) {
            const tokens = args.trim().split(/\s+/).filter(Boolean);
            if (tokens.length === 0) throw new Error('hook: play KEYS SECONDS KEYS …');
            let keys = [];
            const flush = async () => {
                if (keys.length === 0) return;
                for (const key of keys) await page.keyboard.down(key);
                await sleep(250);
                for (const key of [...keys].reverse()) await page.keyboard.up(key);
                keys = [];
            };
            for (const token of tokens) {
                if (/^\d+(\.\d+)?$/.test(token)) {
                    const seconds = Number(token);
                    if (seconds > 30) throw new Error(`hook: play waits at most 30 s, not ${token}`);
                    await flush();
                    await sleep(seconds * 1000);
                } else {
                    keys.push(token);
                }
            }
            await flush();
        },
    },
};
