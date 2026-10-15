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
    },
};
