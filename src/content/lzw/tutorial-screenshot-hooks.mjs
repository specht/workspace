// Screenshot hooks for the picture on the card (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«): the page's own widget, opened with open-site,
// in the state the picture shows, light and dark.

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
        // hook: widget SELECTOR [STEPS [TEXT]]: the widget scrolled into
        // view; TEXT typed into its field, as a reader types it; then its
        // »next« button pressed STEPS times (»last«: its last step)
        async widget({ page, args }) {
            const [selector, steps = '0', ...words] = args.trim().split(/\s+/);
            const widget = page.locator(selector).first();
            // the page's fixed bars (contents, footer) would cover it
            await page.evaluate((selector) => {
                const keep = document.querySelector(selector);
                // on the page background, white or dark, not the tinted box
                keep.style.setProperty('background', 'var(--bs-body-bg)', 'important');
                keep.style.setProperty('border-color', 'transparent', 'important');
                for (const element of document.querySelectorAll('body *')) {
                    if (keep.contains(element) || element.contains(keep)) continue;
                    const { position } = getComputedStyle(element);
                    if (position === 'fixed' || position === 'sticky') element.style.opacity = '0';
                }
            }, selector);
            await widget.scrollIntoViewIfNeeded();
            if (words.length) {
                const field = widget.locator('input, textarea').first();
                await field.fill(words.join(' '));
                await field.dispatchEvent('input');
                await page.waitForTimeout(300);
            }
            if (steps === 'last') {
                await widget.locator('[data-go="last"]').click();
            } else {
                for (let i = 0; i < Number(steps); i += 1) await widget.locator('[data-go="next"]').click();
            }
            await page.mouse.move(1, 1);
            // until the widget has stopped changing (a step may animate)
            await page.evaluate((selector) => new Promise((resolve) => {
                let timer = setTimeout(resolve, 600);
                new MutationObserver(() => {
                    clearTimeout(timer);
                    timer = setTimeout(resolve, 600);
                }).observe(document.querySelector(selector), { childList: true, subtree: true, attributes: true });
            }), selector);
        },
    },
};
