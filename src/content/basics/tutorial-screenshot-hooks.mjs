// Screenshot hooks for »Die Basics« (src/tutorial-screenshots/README.md,
// »Tutorial-specific hooks«).

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

export default {
    actions: {
        // hook: install PUBLISHER.NAME: installs a colour theme from the
        // marketplace the way a student does it, in the Extensions view, and
        // waits until it is there. VS Code may first ask whether to trust
        // the publisher; the hook says yes. Afterwards the Explorer is back.
        async install({ page, args, timeout }) {
            const id = args.trim();
            if (!/^[\w-]+\.[\w-]+$/.test(id)) throw new Error('hook: install PUBLISHER.NAME');
            await page.keyboard.press('Control+Shift+X');
            const search = page.locator('.extensions-viewlet .suggest-input-container');
            await search.waitFor({ state: 'visible', timeout });
            await search.click();
            await page.keyboard.press('Control+A');
            await page.keyboard.type(`@id:${id}`);
            await page.keyboard.press('Enter');
            // The search narrows the list down to this one extension.
            const rows = page.locator('.extensions-viewlet .monaco-list-row');
            const deadline = Date.now() + timeout;
            while ((await rows.count()) !== 1) {
                if (Date.now() > deadline) throw new Error(`hook: install ${id}: the search did not find exactly one extension`);
                await sleep(500);
            }
            // The button itself; the drop-down arrow beside it and the hidden
            // »Installing« carry the same classes
            const install = rows.first().getByRole('button', { name: 'Install', exact: true });
            await install.waitFor({ state: 'visible', timeout });
            await install.click();
            // The row says »Installing« at once, but VS Code first asks
            // whether to trust a publisher it has not installed from yet:
            // answer yes, and wait until the row says neither »Install« nor
            // »Installing« any more.
            const trust = page.locator('.monaco-dialog-box .monaco-button', { hasText: 'Trust Publisher' });
            for (;;) {
                if (await trust.first().isVisible().catch(() => false)) await trust.first().click();
                // An extension with code of its own may not run in the browser.
                const refusal = page.locator('.monaco-dialog-box', { hasText: 'is not available in' });
                if (await refusal.first().isVisible().catch(() => false)) {
                    throw new Error(`hook: install ${id}: VS Code in the browser cannot install it (no web version)`);
                }
                const text = (await rows.first().innerText().catch(() => '')).replace(/\s+/g, ' ').trim();
                if (!/(?:^| )Install(?:ing)?$/.test(text)) break;
                if (Date.now() > deadline) throw new Error(`hook: install ${id}: still not installed (»${text}«)`);
                await sleep(500);
            }
            // A theme extension may open the theme picker by itself, and VS
            // Code may show the extension's page: neither belongs in the
            // picture, which shows the code in the new theme.
            await sleep(1500);
            await page.keyboard.press('Escape');
            const pages = page.locator('.tabs-container .tab', { hasText: 'Extension:' });
            // A middle click closes a tab, without its hover-only close button.
            while (await pages.count()) {
                await pages.first().click({ button: 'middle' });
                await sleep(300);
                if (Date.now() > deadline) throw new Error(`hook: install ${id}: the extension page does not close`);
            }
            await page.keyboard.press('Control+Shift+E');
            await sleep(500);
        },
    },
};
