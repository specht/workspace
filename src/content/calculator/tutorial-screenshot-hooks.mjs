// Screenshot hooks for »Ein Rechner für Ausdrücke«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    actions: {
        // hook: plain-tree: the tree drawer without its band and scroll bar
        // and without the page's fixed footer, which at zoom 2 covers the
        // lower part of the small window; the tree in the middle of the
        // window, on the page's own background.
        async 'plain-tree'({ page }) {
            await page.addStyleTag({ content: `
                #calc-stepper { background: none !important; border: none !important; }
                #calc-stepper .calc-tree { overflow: visible !important; }
                footer.footer-fixed { display: none !important; }
            ` });
            // In the middle of the window, away from the page's scroll bar
            await page.evaluate(() => document.querySelector('#calc-stepper .calc-tree svg')
                .scrollIntoView({ block: 'center', inline: 'center' }));
            await page.waitForTimeout(200);
        },
    },

    variants: {
        // variants: dark: the tree drawer and Automatron's drawings follow the
        // site's colour scheme, and the site follows the system's.
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
