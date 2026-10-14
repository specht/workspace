// Screenshot hooks for »Dateien und Verzeichnisse«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    actions: {
        // hook: seek-video SECONDS
        // VS Code's video preview is a webview (an iframe); its <video>
        // starts at 0:00, where jay.webm shows only the blurred garden.
        async 'seek-video'({ page, args, timeout }) {
            const seconds = Number(args);
            if (!Number.isFinite(seconds) || seconds < 0) {
                throw new Error(`hook: seek-video needs a number of seconds, got "${args}"`);
            }
            const deadline = Date.now() + timeout;
            while (Date.now() < deadline) {
                for (const frame of page.frames()) {
                    const done = await frame.evaluate(async (time) => {
                        const video = document.querySelector('video');
                        if (!video || video.readyState < 1) return false;
                        video.pause();
                        video.currentTime = time;
                        await new Promise((resolve) => video.addEventListener('seeked', resolve, { once: true }));
                        return true;
                    }, seconds).catch(() => false);
                    if (done) return;
                }
                await page.waitForTimeout(250);
            }
            throw new Error('hook: seek-video found no video to seek');
        },
    },
};
