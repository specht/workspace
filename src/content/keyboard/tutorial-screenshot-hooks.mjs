// Screenshot hooks for the Keyboard-Tutorial page
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

export default {
    actions: {
        // hook: scroll-to TEXT
        // Scrolls the tutorial panel so that the element with TEXT is at
        // its top, as a person would scroll there.
        async 'scroll-to'({ page, args }) {
            for (const frame of page.frames()) {
                const found = await frame.evaluate((text) => {
                    const element = [...document.querySelectorAll('h1, h2, h3, p, li')]
                        .find((node) => node.textContent.trim().startsWith(text));
                    if (!element) return false;
                    element.scrollIntoView({ block: 'start' });
                    return true;
                }, args).catch(() => false);
                if (found) return;
            }
            throw new Error(`hook: scroll-to found no »${args}« in the page or its frames`);
        },

        // hook: scroll-top
        // The tutorial's webview scrolls its first open step into view; for
        // the overview the panel should start at its heading.
        async 'scroll-top'({ page }) {
            for (const frame of page.frames()) {
                await frame.evaluate(() => {
                    window.scrollTo(0, 0);
                    for (const element of document.querySelectorAll('*')) {
                        if (element.scrollTop > 0) element.scrollTop = 0;
                    }
                }).catch(() => {});
            }
        },
    },
};
