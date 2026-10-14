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

        // hook: stop-at LABEL: sets a breakpoint on the first line that
        // starts with LABEL and waits until the program stops there. A
        // program that draws in place and waits for vsync at LABEL has a
        // finished frame in RAM at that moment, and the paused screen shows
        // RAM as it is. Running alone, it would only be clean if the browser
        // reached the clock: a busy one slows machine time down, and one
        // frame then spans several of the screen's pictures. Loading another
        // program clears the breakpoint.
        async 'stop-at'({ page, args }) {
            const label = args.trim();
            if (!label) throw new Error('hook: stop-at LABEL');
            const found = await page.evaluate((text) => {
                const cm = document.querySelector('.CodeMirror')?.CodeMirror;
                if (!cm) return 'no editor';
                for (let i = 0; i < cm.lineCount(); i += 1) {
                    if (cm.getLine(i).startsWith(text)) {
                        CodeMirror.signal(cm, 'gutterClick', cm, i, 'cm-breakpoints', new MouseEvent('mousedown'));
                        return null;
                    }
                }
                return `no line starts with ${text}`;
            }, label);
            if (found) throw new Error(`hook: stop-at: ${found}`);
            await page.waitForFunction(
                () => /Breakpoint in line \d+/.test(document.body.innerText),
                null, { timeout: 30000 },
            );
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
