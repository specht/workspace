// The card picture of »Datenübertragung per Kabel« (src/content/cable), from
// the running development site:
//
//   cd e2e && HS_PICTURES=1 npx playwright test --project=pictures cable.pictures
//
// One browser holds the keys 1 and 3 on 42.7 kHz; a second one on the same
// frequency is photographed at /ether, light and dark (teaser.webp,
// teaser-dark.webp): the device on its own colour, 16:9, scaled to
// 1280 x 720. Nothing is stored anywhere; the ether forgets both browsers when
// they close.
import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import type { Browser, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

const ACTIVITY = path.resolve(__dirname, '../../src/content/cable');
const REPO = path.resolve(__dirname, '../..');
// What the picture shows comes from these files: when one changes,
// src/scripts/recreate-screenshots.rb --status calls it stale.
const INPUTS = [
  'e2e/tests/cable.pictures.ts',
  'src/static/include/cable.js', 'src/static/include/cable.css', 'src/static/ether.html',
];
const FREQ = 427;

function inputsHash(): string {
  const hash = crypto.createHash('sha256');
  for (const file of [...INPUTS].sort()) {
    hash.update(file + '\n');
    hash.update(fs.readFileSync(path.join(REPO, file)));
    hash.update('\n');
  }
  return hash.digest('hex');
}

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string') throw new Error('Playwright baseURL must be configured');
  return value;
}

async function ether(browser: Browser, testInfo: TestInfo, scheme: 'light' | 'dark') {
  const context = await browser.newContext({
    baseURL: baseUrl(testInfo), colorScheme: scheme, viewport: { width: 900, height: 800 }, deviceScaleFactor: 2,
  });
  const page = await context.newPage();
  // while the activity is parked, the ether is open to accounts only
  // (cable_page.rb)
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.addInitScript((f) => window.localStorage.setItem('cable-freq', String(f)), FREQ);
  await page.goto('/ether');
  await expect(page.locator('.cable-status')).toHaveText('verbunden');
  return page;
}

test('the card picture of the cable', async ({ browser }, testInfo) => {
  const out = testInfo.outputDir;
  fs.mkdirSync(out, { recursive: true });
  const sender = await ether(browser, testInfo, 'light');
  const taken: string[] = [];
  try {
    await sender.keyboard.down('1');
    await sender.keyboard.down('3');
    for (const scheme of ['light', 'dark'] as const) {
      const page = await ether(browser, testInfo, scheme);
      await expect(page.locator(".cable-channel[data-lamp='3'] .cable-lamp")).toHaveClass(/\bon\b/);
      await expect(page.locator(".cable-channel[data-lamp='1'] .cable-lamp")).toHaveClass(/\bon\b/);
      // The device without the hint under the buttons, in a 16:9 frame.
      await page.addStyleTag({ content: '.cable-keys-hint { display: none; } .ether-page h2, .ether-lead { visibility: hidden; } footer { display: none !important; }' });
      await page.waitForTimeout(300);
      // No frame within the card's own frame: the page around the device in
      // the device's colour, its edge and shadow gone, so the device fills
      // the picture with its margin and nothing of it is cut.
      // (the device's gradient, --cable-panel to --cable-panel-2, as its
      // main colour, flat, since the page cannot continue a gradient)
      await page.locator('.cable-device').evaluate((device) => {
        const bg = getComputedStyle(device).getPropertyValue('--cable-panel').trim();
        for (let e = device; e; e = e.parentElement) e.style.setProperty('background', bg, 'important');
        device.style.setProperty('box-shadow', 'none', 'important');
        device.style.setProperty('border-color', 'transparent', 'important');
      });
      await page.waitForTimeout(300);
      const box = (await page.locator('.cable-device').boundingBox())!;
      // The whole device in 16:9; its own padding is the margin.
      let width = box.width;
      let height = (width * 9) / 16;
      if (height < box.height) { height = box.height; width = (height * 16) / 9; }
      const clip = { x: box.x + box.width / 2 - width / 2, y: box.y + box.height / 2 - height / 2, width, height };
      const name = scheme === 'light' ? 'teaser' : 'teaser-dark';
      const png = path.join(out, name + '.png');
      await page.screenshot({ path: png, clip });
      execFileSync('convert', [png, '-resize', '1280x720!', '-quality', '85', '-define', 'webp:method=6', path.join(ACTIVITY, name + '.webp')]);
      taken.push(name + '.webp');
      await page.context().close();
    }
    fs.writeFileSync(path.join(ACTIVITY, '.pictures.json'), JSON.stringify({
      script: 'e2e/tests/cable.pictures.ts',
      command: 'cd e2e && HS_PICTURES=1 npx playwright test --project=pictures cable.pictures',
      inputs: INPUTS, hash: inputsHash(), pictures: taken.sort(),
    }, null, 2) + '\n');
  } finally {
    await sender.context().close();
  }
});
