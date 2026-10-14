// Screenshot hooks for »Frontend-Entwicklung mit Svelte«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// The recipes write each step of the timer into the project that
// `npx sv create rubik-timer` made, and look at it through the dev server
// (preview-port: 5173). The timer listens for the space bar, which neither
// press (down and up at once) nor hold (the mouse) can keep down; these
// actions press and release it in the preview.

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export default {
    writeFileSubdirectory: 'rubik-timer',

    actions: {
        // hook: space-down: press the space bar and keep it down.
        async 'space-down'({ page }) {
            await page.keyboard.down('Space');
        },

        // hook: space-up: release it.
        async 'space-up'({ page }) {
            await page.keyboard.up('Space');
        },

        // hook: solve SECONDS: hold the space bar until the timer is ready,
        // let go, let it run SECONDS and stop it, like one solve.
        async solve({ page, args }) {
            const seconds = Number(args);
            if (!(seconds > 0 && seconds <= 30)) throw new Error('hook: solve SECONDS (up to 30)');
            await page.keyboard.down('Space');
            await sleep(800);
            await page.keyboard.up('Space');
            await sleep(seconds * 1000);
            await page.keyboard.down('Space');
            await sleep(100);
            await page.keyboard.up('Space');
            await page.mouse.move(0, 0);
        },
    },
};
