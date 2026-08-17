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
};
