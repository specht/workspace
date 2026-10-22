// Screenshot hooks for »Shader in GLSL« (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«).
//
// The recipes write a stage into ~/glsl-starter/shader.frag; Live Server
// serves the folder, and its index.html (vorlage/index.html) draws the shader
// with WebGL. ?t= stops the clock, so every render shows the same picture,
// and ?scale= renders as many pixels as the screenshot has (the page itself
// renders at CSS pixels, and the pictures are taken at zoom 1.5).

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// Two animation frames after a change, the page has drawn and presented it.
async function settleFrames(page, count) {
    await page.evaluate((n) => new Promise((resolve) => {
        const step = (left) => (left === 0 ? resolve() : requestAnimationFrame(() => step(left - 1)));
        step(n);
    }), count);
}

export default {
    // 3D scenes: on the graphics card, if the server has one
    gpu: true,

    writeFileSubdirectory: 'glsl-starter',

    async waitForPreview({ page, timeout }) {
        await page.waitForLoadState('domcontentloaded', { timeout });
    },

    actions: {
        // hook: open [SECONDS] [error]: shows index.html with the clock
        // stopped at SECONDS (default 2.5). Without »error« a message in the
        // error box fails the render; with it, the box must have one. Live
        // Server reloads the page by itself once a file was written,
        // sometimes only after this navigation: the page has to stay put for
        // two seconds, otherwise the reloaded page is waited for again.
        //
        // hook: open SECONDS ss=N renders N × N times as many pixels and
        // scales them down for the picture: smooth edges (antialiasing) where
        // a pixel holds more than one hole of the sponge.
        async open({ page, args, timeout }) {
            const words = args.trim().split(/\s+/).filter(Boolean);
            const expectError = words.includes('error');
            const supersample = Number((words.find((w) => w.startsWith('ss=')) ?? 'ss=1').slice(3));
            const seconds = Number(words.find((w) => w !== 'error' && !w.startsWith('ss=')) ?? 2.5);
            if (!(seconds >= 0 && seconds <= 600)) throw new Error('hook: open [SECONDS] [error] [ss=N]');
            if (!(Number.isInteger(supersample) && supersample >= 1 && supersample <= 4)) throw new Error('hook: open: ss=N with N from 1 to 4');
            const scale = await page.evaluate(() => window.devicePixelRatio || 1) * supersample;
            const url = new URL(`/index.html?t=${seconds}&scale=${scale}`, page.url()).href;
            for (let attempt = 0; ; attempt += 1) {
                await page.goto(url, { waitUntil: 'domcontentloaded', timeout });
                await page.waitForSelector('#canvas', { timeout });
                let navigated = false;
                const onNavigation = (frame) => {
                    if (frame === page.mainFrame()) navigated = true;
                };
                page.on('framenavigated', onNavigation);
                await sleep(2000);
                page.off('framenavigated', onNavigation);
                if (!navigated) break;
                if (attempt === 3) throw new Error('hook: open: the page keeps reloading');
            }
            const error = await page.evaluate(() => document.getElementById('error').textContent);
            if (expectError && !error) throw new Error('hook: open error: the shader compiled without an error');
            if (!expectError && error) throw new Error(`hook: open: the shader reports an error:\n${error}`);
            await settleFrames(page, 3);
            if (supersample > 1) {
                // In the next animation frame, right after index.html has drawn
                // (its callback was queued first), the big picture goes scaled
                // down with the browser's best filter into a canvas of screen
                // size that lies on top of the WebGL canvas.
                await page.evaluate(() => new Promise((resolve) => {
                    requestAnimationFrame(() => {
                        const source = document.getElementById('canvas');
                        const box = source.getBoundingClientRect();
                        const small = document.createElement('canvas');
                        small.width = Math.round(box.width * devicePixelRatio);
                        small.height = Math.round(box.height * devicePixelRatio);
                        const context = small.getContext('2d');
                        context.imageSmoothingEnabled = true;
                        context.imageSmoothingQuality = 'high';
                        context.drawImage(source, 0, 0, small.width, small.height);
                        small.style.cssText = `position: fixed; left: ${box.left}px; top: ${box.top}px; ` +
                            `width: ${box.width}px; height: ${box.height}px; z-index: 10;`;
                        document.body.appendChild(small);
                        resolve();
                    });
                }));
                await settleFrames(page, 2);
            }
        },

        // hook: drag FX FY: presses the mouse in the middle of the picture
        // and drags it to FX, FY (fractions of the width and height, from the
        // top left), and holds it there for the picture.
        async drag({ page, args }) {
            const [fx, fy] = args.trim().split(/\s+/).map(Number);
            if (!(fx >= 0 && fx <= 1 && fy >= 0 && fy <= 1)) throw new Error('hook: drag FX FY (0-1)');
            const box = await page.locator('#canvas').boundingBox();
            await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
            await page.mouse.down();
            await page.mouse.move(box.x + fx * box.width, box.y + fy * box.height, { steps: 5 });
            await settleFrames(page, 3);
        },
    },
};
