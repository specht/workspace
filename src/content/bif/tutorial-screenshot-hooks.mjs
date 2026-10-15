export default {
    writeFileSubdirectory: 'bif',

    async waitForPreview({ page, timeout }) {
        await page.waitForSelector('#content', {
            state: 'attached',
            timeout,
        });
        await page.waitForFunction(
            () => (document.querySelector('#content')?.innerText || '').trim().length > 0,
            null,
            { timeout },
        );
    },

    // variants: dark: BIF's default theme and its development view follow
    // the system's colour scheme, so the dark variant only changes what the
    // preview is told about it.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
