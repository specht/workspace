// Screenshot hooks for the CPU Sandbox gallery
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// Programs read the keyboard through devices they poll once per frame, so
// a key has to stay down for a moment: Playwright's press is too short.

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

async function holdKeys(page, keys, ms) {
    await page.locator('#screen').click();
    for (const key of keys) await page.keyboard.down(key);
    await sleep(ms);
    for (const key of [...keys].reverse()) await page.keyboard.up(key);
}

export default {
    actions: {
        // hook: hold-key KEY[+KEY] [SECONDS]: clicks the screen (so keys go to
        // the program) and holds the keys, 0.4 s unless given.
        async 'hold-key'({ page, args }) {
            const [combo, seconds] = args.split(/\s+/);
            if (!combo) throw new Error('hook: hold-key KEY[+KEY] [SECONDS]');
            await holdKeys(page, combo.split('+'), Number(seconds || 0.4) * 1000);
        },

        // hook: pause: pauses the running program, if one runs. A program
        // that ended (HLT) leaves »Run« disabled, so a plain click would wait.
        async pause({ page }) {
            await page.evaluate(() => {
                const run = document.getElementById('run');
                if (run && !run.disabled && run.dataset.running === 'true') run.click();
            });
            await sleep(300);
        },

        // hook: play-breakout SECONDS: launches the ball again and again and
        // moves the paddle back and forth, so bricks go all over the wall.
        async 'play-breakout'({ page, args }) {
            const seconds = Number(args);
            if (!(seconds > 0 && seconds <= 120)) throw new Error('hook: play-breakout SECONDS (1-120)');
            const end = Date.now() + seconds * 1000;
            for (let step = 0; Date.now() < end; step += 1) {
                await holdKeys(page, ['Space'], 120);
                await holdKeys(page, [step % 2 ? 'ArrowLeft' : 'ArrowRight'], 250 + (step * 137) % 300);
            }
        },
    },
};
