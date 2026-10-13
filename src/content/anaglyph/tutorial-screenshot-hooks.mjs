// Screenshot and video hooks for the Anaglyph tutorial
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// The viewer (specht/anaglyph, include/script.js) is a p5 sketch in global
// mode. Its draw loop reads the scene time as millis() / 1000, so a video
// frame at time t is drawn by making millis() return t * 1000 and waiting
// until draw() has run again.

const readyTimeout = 60_000;

async function framesDrawn(page, count, timeout) {
    await page.waitForFunction(
        ([start, wanted]) => window.frameCount >= start + wanted,
        [await page.evaluate(() => window.frameCount), count],
        { timeout, polling: 'raf' },
    );
}

export default {
    // The template is cloned to /workspace/anaglyph; scene files live there.
    writeFileSubdirectory: 'anaglyph',

    // The scene file is loaded and drawn a few times.
    async waitForPreview({ page, timeout }) {
        await page.waitForFunction(
            () => typeof sceneDescription !== 'undefined' && window.frameCount > 3,
            null,
            { timeout: timeout ?? readyTimeout },
        );
    },

    // Recipe instructions only this viewer understands (hook: NAME ARGS).
    actions: {
        // hook: camera ROT_X ROT_Y DISTANCE [CENTER_X CENTER_Y CENTER_Z]
        // Angles in degrees. »Reset Camera« is 20 20 -300; a larger distance
        // looks from farther away. Set at once, without the orbit animation.
        async camera({ page, args, timeout }) {
            const values = args.split(/\s+/).filter(Boolean).map(Number);
            if (![3, 6].includes(values.length) || values.some(value => !Number.isFinite(value))) {
                throw new Error(`hook: camera ROT_X ROT_Y DISTANCE [CX CY CZ], got: ${args}`);
            }
            await page.evaluate(([rotX, rotY, distance, cx = 0, cy = 0, cz = 0]) => {
                camera.rotationX = camera.targetRotationX = radians(rotX);
                camera.rotationY = camera.targetRotationY = radians(rotY);
                camera.distance = camera.targetDistance = distance;
                camera.center = createVector(cx, cy, cz);
                camera.targetCenter = camera.center.copy();
            }, values);
            await framesDrawn(page, 2, timeout ?? readyTimeout);
        },

        // hook: anaglyph on|off: the »Anaglyph 3D« button, to a known state.
        async anaglyph({ page, args, timeout }) {
            if (!['on', 'off'].includes(args)) throw new Error(`hook: anaglyph on|off, got: ${args}`);
            const changed = await page.evaluate((on) => {
                if (camera.enableAnaglyph === on) return false;
                document.getElementById('bu-anaglyph').click();
                return true;
            }, args === 'on');
            if (changed) await framesDrawn(page, 2, timeout ?? readyTimeout);
        },

        // hook: hide-controls: the buttons and the scene field leave the
        // picture, for a video of the scene alone.
        async 'hide-controls'({ page }) {
            await page.addStyleTag({ content: '#controls { display: none !important; }' });
        },
    },

    // seconds: the frame's time; null hands the sketch back to real time.
    async setPreviewTime({ page, seconds, timeout }) {
        await page.evaluate((ms) => {
            if (!window.__tutorialRealMillis) window.__tutorialRealMillis = window.millis;
            if (ms === null) {
                window.millis = window.__tutorialRealMillis;
                return;
            }
            window.millis = () => ms;
        }, seconds === null ? null : seconds * 1000);
        if (seconds === null) return;
        // Two frames: the first may already have started with the old time.
        await framesDrawn(page, 2, timeout ?? readyTimeout);
    },
};
