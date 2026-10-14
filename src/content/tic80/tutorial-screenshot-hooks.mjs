// Screenshot hooks for »Ein Spiel in 64 KB« (src/tutorial-screenshots/
// README.md, »Tutorial-specific hooks«).
//
// The recipes open TIC-80 with open-site: /tic80/ and drive it like a
// person: TIC-80 reads the keyboard once per frame (60 times a second), so
// keys are typed with a pause between them, and arrow keys are held down
// for a while to move a figure.

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export default {
    // TIC-80 starts with a click on »- KLICKEN ZUM STARTEN -« and is ready
    // once its files from the Workspace are loaded and the console has
    // drawn its prompt.
    async waitForPreview({ page, timeout }) {
        await page.locator('#game-frame').click({ timeout });
        await page.waitForFunction(() => typeof window.Module?.hsTic80Root === 'string', null, { timeout });
        await sleep(3000);
        await page.locator('#canvas').click();
        // TIC-80 draws its own pointer where the mouse is: into the corner.
        const size = page.viewportSize();
        if (size) await page.mouse.move(size.width - 1, size.height - 1);
    },

    actions: {
        // hook: tic COMMAND: type a console command and press Enter.
        async tic({ page, args }) {
            await page.keyboard.type(args, { delay: 120 });
            await sleep(150);
            await page.keyboard.press('Enter');
            await sleep(700);
        },

        // hook: key KEY: one key or chord, as Playwright names it
        // (Escape, F2, Control+r).
        async key({ page, args }) {
            await page.keyboard.press(args.trim());
            await sleep(500);
        },

        // hook: hold KEY SECONDS: keep a key down, to move a figure.
        async hold({ page, args }) {
            const [key, seconds] = args.trim().split(/\s+/);
            const s = Number(seconds);
            if (!key || !(s > 0 && s <= 10)) throw new Error('hook: hold KEY SECONDS (up to 10)');
            await page.keyboard.down(key);
            await sleep(s * 1000);
            await page.keyboard.up(key);
        },
    },
};
