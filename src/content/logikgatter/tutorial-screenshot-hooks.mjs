// Screenshot hooks for the picture on the card (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«): one of the page's circuits, opened with
// open-site, with some inputs switched on, light and dark.

export default {
    // variants: dark: the site in its dark theme, as a reader who chose it
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.evaluate((on) => document.documentElement.setAttribute('data-bs-theme', on ? 'dark' : 'light'), on);
            await page.waitForTimeout(500);
        },
    },

    actions: {
        // hook: circuit NAME INPUT…: the circuit data-circuit='NAME' shown
        // (also where it waits for an answer), scrolled into view, and the
        // inputs named switched to 1, one after another
        async circuit({ page, args }) {
            const [name, ...inputs] = args.trim().split(/\s+/);
            const selector = `.logic-circuit[data-circuit='${name}']`;
            await page.evaluate((selector) => {
                const keep = document.querySelector(selector);
                const after = keep.closest('.after-task');
                if (after) after.classList.add('after-task-shown');
                // on the page background, white or dark, not the tinted box
                keep.style.setProperty('background', 'var(--bs-body-bg)', 'important');
                keep.style.setProperty('border-color', 'transparent', 'important');
                // the page's fixed bars (contents, footer) would cover it
                for (const element of document.querySelectorAll('body *')) {
                    if (keep.contains(element) || element.contains(keep)) continue;
                    const { position } = getComputedStyle(element);
                    if (position === 'fixed' || position === 'sticky') element.style.opacity = '0';
                }
            }, selector);
            const widget = page.locator(selector).first();
            await widget.scrollIntoViewIfNeeded();
            for (const input of inputs) {
                await widget.locator(`svg .lc-switch[aria-label^='Eingang ${input}:']`).click();
                await page.waitForTimeout(1500);
            }
            await page.mouse.move(1, 1);
            await page.waitForTimeout(500);
        },
    },
};
