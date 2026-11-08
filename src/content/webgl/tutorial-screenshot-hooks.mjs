// Screenshot hooks for »3D-Grafik mit WebGL«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    // the students' programs in the preview on the graphics card (fast, and
    // the textures and mipmaps as a real card draws them); VS Code stays in
    // software
    gpu: true,

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

        // hook: view X Y Z DISTANCE YAW PITCH: the camera of main.js looks at
        // the point (X, Y, Z) from DISTANCE away, turned by YAW and PITCH
        // (radians), as dragging and the wheel would set it: center,
        // distance, yaw and pitch are variables of the page's script.
        async view({ page, args }) {
            const v = args.trim().split(/\s+/).map(Number);
            if (v.length !== 6 || v.some((x) => !Number.isFinite(x))) throw new Error(`hook: view X Y Z DISTANCE YAW PITCH, got: ${args}`);
            await page.evaluate(([x, y, z, d, a, b]) => {
                center = [x, y, z];
                distance = d;
                yaw = a;
                pitch = b;
            }, v);
            await page.waitForTimeout(500);
        },

        // hook: clear R G B: the background the program clears to, for the
        // card only (white like the other cards with an object): main.js
        // calls gl.clearColor in every frame, so the method itself is
        // replaced, and it reads the colour of the moment (the dark variant
        // below changes it).
        async clear({ page, args }) {
            const c = args.trim().split(/\s+/).map(Number);
            if (c.length !== 3 || c.some((x) => !Number.isFinite(x))) throw new Error(`hook: clear R G B, got: ${args}`);
            await page.evaluate(([r, g, b]) => {
                window.cardClear = { light: [r, g, b], now: [r, g, b] };
                const original = WebGL2RenderingContext.prototype.clearColor;
                WebGL2RenderingContext.prototype.clearColor = function () { original.call(this, ...window.cardClear.now, 1); };
            }, c);
            await page.waitForTimeout(500);
        },
    },

    // variants: dark: the card in the dark theme, on the dark ground of the
    // splat viewer's card (#2a2d31) instead of white
    variants: {
        async dark({ page, on }) {
            await page.evaluate((on) => {
                if (window.cardClear) window.cardClear.now = on ? [0.165, 0.176, 0.192] : window.cardClear.light;
            }, on);
            await page.waitForTimeout(800);
        },
    },
};
