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
