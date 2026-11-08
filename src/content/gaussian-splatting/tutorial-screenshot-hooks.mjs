// Screenshot hooks for »Gaussian Splatting«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    // the viewer draws up to 5 million splats: its preview tabs on the
    // graphics card, not in software, where the street does not finish within
    // the capture's time (README, »WebGL renders on the host's NVIDIA
    // graphics card«); VS Code stays in software either way
    gpu: true,

    actions: {
        // hook: hide-footer: the footer, fixed to the bottom in development,
        // and the bar of the page's chapters would lie over the viewer, the
        // fullscreen button and a spot's caption over the picture;
        // the stage loses its round corners, which the card has itself.
        async 'hide-footer'({ page }) {
            await page.addStyleTag({ content: 'footer, nav.autotoc, .splat-viewer-full, .splat-viewer-note { display: none !important; } .splat-viewer-stage { border-radius: 0 !important; }' });
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
                // the number of the recipe, not the automatic one
                if (viewer.autoCount.checked) {
                    viewer.autoCount.checked = false;
                    viewer.autoCount.dispatchEvent(new Event('change'));
                }
                viewer.setLook({ count: k >= n ? 1 : Math.log(k / 100) / Math.log(n / 100) });
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

        // hook: white-ground: the viewer #szenen on white instead of its
        // beige, in the light theme only (the card picture, like the other
        // cards with an object on white; the dark variant keeps its ground)
        async 'white-ground'({ page }) {
            await page.addStyleTag({ content: 'html:not([data-bs-theme="dark"]) #szenen { --splat-ground: #ffffff; }' });
            await page.waitForTimeout(300);
        },

        // hook: softness VALUE: the slider »Weichheit« of #szenen, from 0
        // (ellipses with a sharp border) to 1 (Gaussians), without the wave
        // a change by hand runs.
        async softness({ page, args }) {
            const value = Number(args.trim());
            if (!(value >= 0 && value <= 1)) throw new Error(`hook: softness VALUE (0 to 1), got: ${args}`);
            await page.evaluate((v) => {
                document.getElementById('szenen').splatViewer.setLook({ softness: v });
            }, value);
            await page.waitForTimeout(1500);
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
