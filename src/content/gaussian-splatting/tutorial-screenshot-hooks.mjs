// Screenshot hooks for »Gaussian Splatting«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    // the viewer draws up to 1.5 million splats: on the graphics card, not
    // in software (README, »WebGL renders on the host's NVIDIA graphics card«)
    gpu: true,

    actions: {
        // hook: hide-footer: the footer, fixed to the bottom in development,
        // and the bar of the page's chapters would lie over the viewer;
        // the stage loses its round corners, which the card has itself.
        async 'hide-footer'({ page }) {
            await page.addStyleTag({ content: 'footer, nav.autotoc { display: none !important; } .splat-viewer-stage { border-radius: 0 !important; }' });
            await page.waitForTimeout(200);
        },

        // hook: scene NAME COUNT: the viewer #szenen shows the scene NAME
        // (its name in the list) with only its first COUNT splats, from its
        // start view, and has drawn it.
        async scene({ page, args }) {
            const match = args.trim().match(/^(.*\S)\s+(\d+)$/);
            if (!match) throw new Error(`hook: scene NAME COUNT, got: ${args}`);
            await page.evaluate(async ([name, count]) => {
                const root = document.getElementById('szenen');
                const viewer = root.splatViewer;
                const select = root.querySelector('select');
                const option = [...select.options].find((o) => o.text === name);
                if (!option) throw new Error(`no scene ${name}`);
                if (select.value !== option.value) {
                    select.value = option.value;
                    select.dispatchEvent(new Event('change'));
                }
                for (let i = 0; i < 900 && viewer.loaded !== name; i++) {
                    await new Promise((r) => setTimeout(r, 100));
                }
                const n = viewer.splats.count;
                const k = Math.min(count, n);
                viewer.counter.value = String(k >= n ? 1 : Math.log(k / 100) / Math.log(n / 100));
                viewer.counter.dispatchEvent(new Event('input'));
            }, [match[1], Number(match[2])]);
            await page.waitForTimeout(4000);
        },

        // hook: camera ZOOM LIFT PITCH: the camera of #szenen, from the
        // scene's start view: ZOOM times its distance, the point it turns
        // around moved up on the screen by LIFT times the start distance,
        // and looking down at the angle PITCH (radians).
        async camera({ page, args }) {
            const v = args.trim().split(/\s+/).map(Number);
            if (v.length !== 3 || v.some((x) => !Number.isFinite(x)) || v[0] <= 0) throw new Error(`hook: camera ZOOM LIFT PITCH, got: ${args}`);
            await page.evaluate(([zoom, lift, pitch]) => {
                const viewer = document.getElementById('szenen').splatViewer;
                const start = viewer.distance;
                viewer.pitch = pitch;
                viewer.distance = start * zoom;
                const up = viewer.camera().up;
                viewer.target = viewer.target.map((t, k) => t + up[k] * lift * start);
                viewer.redraw();
            }, v);
            await page.waitForTimeout(3000);
        },

        // hook: softness VALUE: the slider »Weichheit der Splats« of
        // #szenen, from 0 (ellipses with a sharp border) to 1 (Gaussians).
        async softness({ page, args }) {
            const value = Number(args.trim());
            if (!(value >= 0 && value <= 1)) throw new Error(`hook: softness VALUE (0 to 1), got: ${args}`);
            await page.evaluate((v) => {
                const slider = document.getElementById('szenen').splatViewer.sharpness;
                slider.value = String(v);
                slider.dispatchEvent(new Event('input'));
            }, value);
            await page.waitForTimeout(2000);
        },
    },

    // variants: dark: the splat viewer's ground follows the site's colour
    // scheme (default.css, .splat-viewer), and the site follows the system's.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(800);
        },
    },
};
