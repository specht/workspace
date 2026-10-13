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
