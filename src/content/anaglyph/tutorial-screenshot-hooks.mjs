// Screenshot and video hooks for the Anaglyph tutorial
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// The viewer (specht/anaglyph, include/script.js) is a p5 sketch in global
// mode. Its draw loop reads the scene time as millis() / 1000, so a video
// frame at time t is drawn by making millis() return t * 1000 and waiting
// until draw() has run again.

const readyTimeout = 60_000;

// Stops p5's loop and draws exactly one frame, then waits until it is on
// screen. The loop drawing on in the background, all in software WebGL,
// took several seconds per frame in the generator, and every wait for two
// frames of it with it; a picture needs one frame, drawn when it is due.
// The next scene reloads the page, which starts the loop again.
async function drawOnce(page) {
    await page.evaluate(() => {
        noLoop();
        redraw();
    });
    await page.evaluate(() => new Promise((resolve) => requestAnimationFrame(() => resolve())));
}

// Clicks a viewer button if camera[flag] is not already on (or off).
async function toggleButton(page, id, flag, on, timeout) {
    const changed = await page.evaluate(([id, flag, on]) => {
        if (camera[flag] === on) return false;
        document.getElementById(id).click();
        return true;
    }, [id, flag, on]);
    if (changed) await drawOnce(page);
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
        // Angles in degrees. »Reset Camera« is 20 20 -300. The distance moves
        // the scene towards the camera: -450 is closer than -300, 0 farther.
        // Set at once, without the orbit animation.
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
            await drawOnce(page);
        },

        // hook: anaglyph on|off: the »Anaglyph 3D« button, to a known state.
        async anaglyph({ page, args, timeout }) {
            if (!['on', 'off'].includes(args)) throw new Error(`hook: anaglyph on|off, got: ${args}`);
            await toggleButton(page, 'bu-anaglyph', 'enableAnaglyph', args === 'on', timeout);
        },

        // hook: hide-controls: the buttons and the scene field leave the
        // picture, for a video of the scene alone.
        async 'hide-controls'({ page }) {
            await page.addStyleTag({ content: '#controls { display: none !important; }' });
        },
    },

    // variants: dark anaglyph (src/tutorial-screenshots/README.md): the
    // »Dunkel« and »Anaglyph 3D« buttons, each to a known state. The page
    // shows the variant that fits its theme and the reader's glasses button.
    variants: {
        async dark({ page, on, timeout }) {
            await toggleButton(page, 'bu-dark', 'enableDark', on, timeout);
        },
        async anaglyph({ page, on, timeout }) {
            await toggleButton(page, 'bu-anaglyph', 'enableAnaglyph', on, timeout);
        },
    },

    // seconds: the frame's time; null hands the sketch back to real time.
    // While a video is recorded, p5 does not run its loop: for each point in
    // time it draws exactly once (noLoop, redraw), and one animation frame
    // later the picture is on screen. With the loop running, every video
    // frame waited for two whole frames while the loop drew on in between,
    // all in software WebGL: 5 s per frame instead of a fraction of one.
    async setPreviewTime({ page, seconds, timeout }) {
        await page.evaluate((ms) => {
            if (!window.__tutorialRealMillis) window.__tutorialRealMillis = window.millis;
            if (ms === null) {
                window.millis = window.__tutorialRealMillis;
                loop();
                return;
            }
            noLoop();
            window.millis = () => ms;
            redraw();
        }, seconds === null ? null : seconds * 1000);
        if (seconds === null) return;
        await page.evaluate(() => new Promise((resolve) => requestAnimationFrame(() => resolve())));
    },
};
