// Screenshot hooks for »Ein Routenplaner für Berlin«,
// src/tutorial-screenshots/README.md »Tutorial-specific hooks«.

// A click on a button of the panel, as the page receives it. Playwright's own
// click waits until nothing moves, and while a search with a hundred thousand
// nodes replays, the page draws without a pause.
async function press(page, selector) {
    await page.waitForSelector(selector, { state: 'attached' });
    await page.evaluate((selector) => document.querySelector(selector).click(), selector);
}

export default {
    // The template is cloned to /workspace/routenplaner-starter.
    writeFileSubdirectory: 'routenplaner-starter',

    // The page is ready once the map has come: the panel counts its ways.
    async waitForPreview({ page, timeout }) {
        await page.waitForFunction(() => /Wege/.test(document.getElementById('area')?.textContent || ''), null, { timeout });
        await page.waitForTimeout(500);
    },

    // variants: dark: the map page follows the system's colour scheme, like
    // the site; the Workspace's own pictures (the terminal) have no variant.
    variants: {
        async dark({ page, on }) {
            await page.emulateMedia({ colorScheme: on ? 'dark' : 'light' });
            await page.waitForTimeout(500);
        },
    },

    actions: {
        // hook: teaser: the map alone, for the card of the activity: the panel,
        // zoom and scale hidden and the route fitted into the whole picture
        async teaser({ page }) {
            await page.evaluate(() => {
                for (const id of ['panel', 'zoom', 'scale', 'suggestions']) document.getElementById(id).style.display = 'none';
                if (fitted) fit(...fitted); else draw();
            });
            await page.waitForTimeout(300);
        },

        // hook: show LON LAT SCALE [graph]: the map there, SCALE pixels per
        // degree of latitude, and a click on that spot, as a student would
        // click; with »graph« the view »Graph« first
        async show({ page, args }) {
            const [lon, lat, scale, graph] = String(args).trim().split(/\s+/);
            if (graph === 'graph') {
                await press(page, '#layers [data-layer="graph"]');
                await page.waitForFunction(() => typeof graph !== 'undefined' && graph);
            }
            await page.evaluate(async ([lon, lat, scale]) => {
                fitted = null;
                view.lon = lon;
                view.lat = lat;
                view.scale = scale;
                draw();
                await clicked(lon, lat);
            }, [Number(lon), Number(lat), Number(scale)]);
            await page.waitForTimeout(300);
        },

        // hook: route START | GOAL | SEARCH [| MODE]: typed into the fields like a
        // student would (the first place whose name contains the text), the
        // search (bfs, dijkstra) clicked, and waited for until it has played
        // to the end and the way is drawn
        async route({ page, args }) {
            const [start, goal, search, mode = 'length'] = String(args).split('|').map((part) => part.trim());
            if (await page.getAttribute('#tabs [data-tab="route"]', 'aria-selected') !== 'true') await press(page, '#tabs [data-tab="route"]');
            await press(page, `#modes [data-mode="${mode}"]`);
            // die Ansicht »Graph« eines Bildes davor bleibt nach dem Neuladen in der Adresse
            await press(page, '#layers [data-layer="map"]');
            await press(page, `#searches [data-search="${search}"]`);
            for (const [field, text] of [['#start', start], ['#goal', goal]]) {
                await page.fill(field, text);
                await page.locator(field).dispatchEvent('change');
            }
            // die Suche ist einmal abgelaufen, der Weg gezeichnet
            await page.waitForFunction(() => route && route.shown >= route.visited.length / 2, null, { timeout: 60_000 });
            await page.locator(':focus').evaluate((element) => element.blur()).catch(() => {});
            await page.waitForTimeout(300);
        },

        // hook: tour SEARCH | MODE | METHOD | PLACE | PLACE | …: the tab
        // »Rundfahrt«, the places typed into its field one after another
        // (the first that contains the text), and waited for the tour
        async tour({ page, args }) {
            const [search, mode, method, ...places] = String(args).split('|').map((part) => part.trim());
            // die Ansicht »Graph« eines Bildes davor bleibt nach dem Neuladen in der Adresse
            await press(page, '#layers [data-layer="map"]');
            await press(page, '#tabs [data-tab="tour"]');
            await page.evaluate(() => { stops = []; tour = null; showStops(); });
            await press(page, `#searches [data-search="${search}"]`);
            await press(page, `#modes [data-mode="${mode}"]`);
            await press(page, `#methods [data-method="${method}"]`);
            for (const place of places) {
                await page.focus('#stop');
                await page.keyboard.type(place);
                await page.keyboard.press('Enter');
            }
            // die neue Rundfahrt, nicht die von vorher, die bis dahin stehen bleibt
            await page.waitForFunction(([n, wanted]) => tour && tour.order.length === n && method === wanted
                && document.getElementById('info').textContent.startsWith(`Rundfahrt: ${METHOD_NAMES[wanted]}`), [places.length, method], { timeout: 180_000 });
            await page.locator(':focus').evaluate((element) => element.blur()).catch(() => {});
            await page.waitForTimeout(300);
        },
    },
};
