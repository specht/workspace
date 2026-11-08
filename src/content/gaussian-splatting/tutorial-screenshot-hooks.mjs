// Screenshot hooks for »Gaussian Splatting«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
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
