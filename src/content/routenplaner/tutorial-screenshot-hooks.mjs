// Screenshot hooks for »Ein Routenplaner für Berlin«,
// src/tutorial-screenshots/README.md »Tutorial-specific hooks«.

export default {
    // The template is cloned to /workspace/routenplaner-starter.
    writeFileSubdirectory: 'routenplaner-starter',

    // The page is ready once the map has come: the panel counts its ways.
    async waitForPreview({ page, timeout }) {
        await page.waitForFunction(() => /Wege/.test(document.getElementById('area')?.textContent || ''), null, { timeout });
        await page.waitForTimeout(500);
    },

    actions: {
        // hook: show LON LAT SCALE: the map there, SCALE pixels per degree of
        // latitude, and a click on that spot, as a student would click
        async show({ page, args }) {
            const [lon, lat, scale] = String(args).trim().split(/\s+/).map(Number);
            await page.evaluate(async ([lon, lat, scale]) => {
                view.lon = lon;
                view.lat = lat;
                view.scale = scale;
                draw();
                await clicked(lon, lat);
            }, [lon, lat, scale]);
            await page.waitForTimeout(300);
        },
    },
};
