// Screenshot hooks for »Ein Rechner für Ausdrücke«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    variants: {
        // variants: dark: the tree drawer and Automatron's drawings follow the
        // site's colour scheme, and the site follows the system's.
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
