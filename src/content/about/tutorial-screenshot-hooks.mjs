// Screenshot hooks for »Was ist der Hackschule Workspace?«
// (src/tutorial-screenshots/README.md, »Tutorial-specific hooks«).

import http from 'node:http';

// Only the development site has the alt text review in its footer; the
// sponsor line is left out of the picture.
const hidden = '.alt-review-footer-left, .sponsor { visibility: hidden; }';

// GET url without the browser's cookies. Like the screenshot browser, it goes
// to the Compose nginx service and keeps the site's host name.
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
    async waitForPreview({ page }) {
        await page.addStyleTag({ content: hidden });
    },

    actions: {
        // hook: guest: the page as someone sees it who is not logged in. The
        // page itself is fetched without cookies; the screenshot account stays
        // logged in for everything else.
        async guest({ page, timeout }) {
            const url = page.url();
            await page.route(url, async (route) => {
                // An error here would end the screenshot service; a failed
                // request fails the reload below instead.
                try {
                    const { status, type, body } = await fetchAsGuest(url);
                    await route.fulfill({ status, contentType: type || 'text/html', body });
                } catch (error) {
                    console.error(`hook: guest could not fetch ${url}: ${error.message}`);
                    await route.abort();
                }
            });
            await page.reload({ waitUntil: 'networkidle', timeout });
            await page.addStyleTag({ content: hidden });
        },
    },

    // variants: dark: the site follows the system's colour scheme.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },
};
