// Screenshot hooks for »Filmdatenbank als Graph mit Neo4j«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).
//
// The Neo4j Browser's theme is »Automatic« by default and follows the
// system's colour scheme, so the dark variant only changes what the page is
// told about it.

export default {
    // variants: dark: the Neo4j Browser in its dark theme.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            // Let the Browser repaint in the new theme.
            await page.waitForTimeout(500);
        },
    },
};
