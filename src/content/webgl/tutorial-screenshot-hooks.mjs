// Screenshot hooks for »3D-Grafik mit WebGL«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    actions: {
        // hook: zoom FACTOR: brings the camera of main.js closer (below 1)
        // or further away, as the mouse wheel does: distance is a variable
        // of the page's script, the wheel multiplies it.
        async zoom({ page, args }) {
            const factor = Number(args.trim());
            if (!Number.isFinite(factor) || factor <= 0) throw new Error(`hook: zoom FACTOR, got: ${args}`);
            await page.evaluate((f) => { distance *= f; }, factor);
            await page.waitForTimeout(500);
        },
    },
};
