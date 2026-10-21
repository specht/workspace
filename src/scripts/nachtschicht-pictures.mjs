// The pictures of the Nachtschicht at the end of Backrooms Level 3, taken in
// the exported level (export.sh in specht/backrooms-starter, with its baked
// light), not from recipes: the generator would have to bake the level in
// every run. Runs inside the generator's image, which has Playwright and the
// graphics card; nachtschicht-pictures.sh starts it and turns the PNGs into
// WebP next to the page.
//
//   node nachtschicht-pictures.mjs SITE OUT
//
// SITE: the exported folder (served here), OUT: where the PNGs go. Every view
// is x, z (fields of the building), heading (0 north, -90 east, 180 south)
// and pitch in degrees, as in ?view=.
import { chromium } from 'playwright';
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';

const VIEWS = [
    ['nachtschicht-flucht3.png', '50.00,8.00,-70,4'],
    ['nachtschicht-flucht1.png', '15.00,6.00,-92,2'],
    ['nachtschicht-nachtbad.png', '119.50,42.00,180,-12'],
    ['nachtschicht-abfahrt.png', '145.50,86.00,180,-14'],
    ['nachtschicht-parkdeck.png', '144.00,107.00,-128,2'],
    ['nachtschicht-notgang.png', '174.00,118.50,-90,14'],
];

const [site, out] = process.argv.slice(2);
const types = { '.js': 'text/javascript', '.html': 'text/html', '.css': 'text/css', '.gltf': 'model/gltf+json', '.jpg': 'image/jpeg', '.png': 'image/png' };
const server = http.createServer((req, res) => {
    const file = path.join(site, decodeURIComponent(new URL(req.url, 'http://x').pathname));
    fs.readFile(file, (err, data) => {
        if (err) { res.writeHead(404); res.end(); return; }
        res.writeHead(200, { 'content-type': types[path.extname(file)] ?? 'application/octet-stream' });
        res.end(data);
    });
}).listen(9000);

const browser = await chromium.launch({ headless: true, channel: 'chromium', args: ['--use-gl=angle', '--use-angle=vulkan', '--enable-features=Vulkan', '--enable-gpu', '--ignore-gpu-blocklist'] });
const page = await browser.newPage({ viewport: { width: 1536, height: 864 }, deviceScaleFactor: 1.8 });
page.on('pageerror', (error) => console.log('pageerror', error.message));
for (const [name, view] of VIEWS) {
    await page.goto(`http://localhost:9000/index.html?sharp&view=${view}`);
    await page.waitForFunction(() => /Light loaded|Light baked|went wrong|gave up/.test(document.getElementById('status')?.textContent ?? ''), null, { timeout: 120000 });
    const status = await page.locator('#status').textContent();
    if (!/Light loaded/.test(status)) throw new Error(`${name}: ${status} (the export needs its level.light)`);
    // the lamps on, flickering ones steady, the panel hidden
    await page.waitForTimeout(3000);
    await page.evaluate(() => {
        const flicker = [...document.querySelectorAll('#controls .flag')].find((button) => button.textContent.startsWith('Flicker'));
        if (flicker?.classList.contains('on')) flicker.click();
    });
    await page.keyboard.press('KeyP');
    await page.addStyleTag({ content: '#panel-button, #fullscreen-button { display: none !important; }' });
    await page.waitForTimeout(1500);
    await page.screenshot({ path: path.join(out, name) });
    console.log(name, status);
}
await browser.close();
server.close();
