// Screenshot hooks for the overview of the Katas
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

import http from 'node:http';

// GET url without the browser's cookies. Like the screenshot browser, it goes
// to the Compose nginx service and keeps the site's host name (as in the
// hooks of »Was ist der Hackschule Workspace?«).
function fetchAsGuest(url) {
    const { host, pathname, search } = new URL(url);
    return new Promise((resolve, reject) => {
        http.get({ host: 'nginx', port: 80, path: pathname + search, headers: { host } }, (response) => {
            let body = '';
            response.setEncoding('utf8');
            response.on('data', (chunk) => { body += chunk; });
            response.on('end', () => resolve({ status: response.statusCode, type: response.headers['content-type'], body }));
        }).on('error', reject);
    });
}

export default {
    actions: {
        // hook: guest: the map as a visitor sees it, without the progress of
        // the screenshot account.
        async guest({ page, timeout }) {
            const url = page.url();
            await page.route(url, async (route) => {
                try {
                    const { status, type, body } = await fetchAsGuest(url);
                    await route.fulfill({ status, contentType: type || 'text/html', body });
                } catch (error) {
                    console.error(`hook: guest could not fetch ${url}: ${error.message}`);
                    await route.abort();
                }
            });
            await page.reload({ waitUntil: 'networkidle', timeout });
            // the development site's footer is fixed to the window
            await page.addStyleTag({ content: 'footer { display: none !important; }' });
        },
    },

    // variants: dark: the site follows the system's colour scheme.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            // the page's colours move over to the new scheme
            await page.waitForTimeout(800);
        },
    },
};
