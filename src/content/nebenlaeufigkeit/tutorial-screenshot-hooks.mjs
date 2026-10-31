// Screenshot hooks for »Nebenläufigkeit«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// variants: dark: the widget follows the site's colour scheme, and the site
// follows the system's.

export default {
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
