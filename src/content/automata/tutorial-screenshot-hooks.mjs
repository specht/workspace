// Screenshot hooks for »Endliche Automaten in Python«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    // variants: dark: Automatron's drawings follow the site's colour scheme,
    // and the site follows the system's.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
