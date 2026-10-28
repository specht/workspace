// Screenshot hooks for »Eine Pixelwand für alle« (src/tutorial-screenshots/
// README.md, »Tutorial-specific hooks«).

export default {
    actions: {
        // hook: paint: paints a small scene (sky, sun, meadow, house, tree)
        // through a WebSocket connection of its own, as if somebody else in
        // the class were painting. The page in the preview receives every
        // pixel from the server like any other.
        async paint({ page }) {
            await page.evaluate(() => new Promise((resolve, reject) => {
                const url = (location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host + '/ws';
                const ws = new WebSocket(url);
                const pixels = [];
                const rect = (x0, y0, x1, y1, color) => {
                    for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) pixels.push({ x, y, color });
                };
                rect(0, 0, 63, 27, 9);        // sky
                rect(0, 28, 63, 39, 6);       // meadow
                for (let y = -3; y <= 3; y++) for (let x = -3; x <= 3; x++)
                    if (x * x + y * y <= 10) pixels.push({ x: 52 + x, y: 7 + y, color: 4 });   // sun
                rect(12, 20, 25, 32, 3);      // house
                for (let i = 0; i <= 8; i++) rect(10 + i, 19 - i, 27 - i, 19 - i, 2);    // roof
                rect(17, 27, 20, 32, 0);      // door
                rect(14, 23, 16, 25, 11);     // windows
                rect(22, 23, 24, 25, 11);
                rect(39, 24, 40, 32, 15);     // trunk
                for (let y = -5; y <= 5; y++) for (let x = -5; x <= 5; x++)
                    if (x * x + y * y <= 26) pixels.push({ x: 40 + x, y: 19 + y, color: 6 });   // crown
                ws.onopen = () => {
                    for (const p of pixels) ws.send(JSON.stringify(p));
                    // Wait until the server has sent the last pixel back.
                    let seen = 0;
                    ws.onmessage = (e) => {
                        if (JSON.parse(e.data).type === 'pixel' && ++seen === pixels.length) { ws.close(); resolve(); }
                    };
                };
                ws.onerror = () => reject(new Error('hook: paint: no WebSocket connection'));
            }));
            await page.waitForTimeout(500);
        },

        // hook: paint-teaser: the scene of »paint« with more going on, as if
        // the class had been at it for a while: a rainbow, clouds, flowers.
        // For the picture on the activity's card, which shows rows 2 to 37
        // of the 40 (hook: band): the clouds start at row 3, the flowers
        // end at row 37.
        async 'paint-teaser'({ page }) {
            await page.evaluate(() => new Promise((resolve, reject) => {
                const url = (location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host + '/ws';
                const ws = new WebSocket(url);
                const pixels = [];
                const dot = (x, y, color) => { if (x >= 0 && x < 64 && y >= 0 && y < 40) pixels.push({ x, y, color }); };
                const rect = (x0, y0, x1, y1, color) => {
                    for (let y = y0; y <= y1; y++) for (let x = x0; x <= x1; x++) dot(x, y, color);
                };
                rect(0, 0, 63, 27, 9);                                           // sky
                [2, 3, 4, 5, 11, 1].forEach((color, i) => {                      // rainbow
                    const r = 22 - i;
                    for (let y = 0; y <= 27; y++) for (let x = 20; x <= 63; x++) {
                        const d = Math.hypot(x - 44, y - 30);
                        if (d >= r - 0.5 && d < r + 0.5) dot(x, y, color);
                    }
                });
                [[9, 5, 4], [26, 9, 3], [56, 4, 3]].forEach(([cx, cy, w]) => {   // clouds
                    rect(cx - w, cy, cx + w, cy + 1, 12);
                    rect(cx - w + 2, cy - 1, cx + w - 2, cy - 1, 12);
                });
                rect(0, 28, 63, 39, 6);                                          // meadow
                for (let y = -2; y <= 2; y++) for (let x = -2; x <= 2; x++)       // sun
                    if (x * x + y * y <= 5) dot(5 + x, 13 + y, 4);
                rect(12, 20, 25, 32, 3);                                         // house
                for (let i = 0; i <= 8; i++) rect(10 + i, 19 - i, 27 - i, 19 - i, 2);
                rect(17, 27, 20, 32, 0);
                rect(14, 23, 16, 25, 11);
                rect(22, 23, 24, 25, 11);
                rect(39, 24, 40, 32, 15);                                        // tree
                for (let y = -5; y <= 5; y++) for (let x = -5; x <= 5; x++)
                    if (x * x + y * y <= 26) dot(40 + x, 19 + y, 6);
                [[4, 32, 2], [8, 35, 4], [30, 33, 12], [34, 36, 1], [47, 32, 2],  // flowers
                 [52, 35, 4], [58, 33, 12], [61, 36, 1], [2, 36, 4], [44, 35, 12]].forEach(([x, y, color]) => {
                    dot(x, y, color); dot(x, y + 1, 7);
                });
                ws.onopen = () => {
                    for (const p of pixels) ws.send(JSON.stringify(p));
                    let seen = 0;
                    ws.onmessage = (e) => {
                        if (JSON.parse(e.data).type === 'pixel' && ++seen === pixels.length) { ws.close(); resolve(); }
                    };
                };
                ws.onerror = () => reject(new Error('hook: paint-teaser: no WebSocket connection'));
            }));
            await page.waitForTimeout(500);
        },

        // hook: band: an empty box over rows 2 to 37 of the wall, 64 by 36
        // pixels, 16:9, for crop-element: #teaser-band. The wall is 64 by
        // 40 (8:5), and its frame has rounded corners; the band leaves both
        // out, so the card shows the picture and nothing around it.
        async band({ page }) {
            await page.evaluate(() => {
                document.getElementById('teaser-band')?.remove();
                const band = document.createElement('div');
                band.id = 'teaser-band';
                band.style.cssText = 'position: absolute; left: 0; right: 0; top: calc(100% * 2 / 40); '
                    + 'height: calc(100% * 36 / 40); pointer-events: none;';
                document.getElementById('grid').appendChild(band);
            });
        },
    },
};
