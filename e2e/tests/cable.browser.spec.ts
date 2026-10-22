import { expect, test } from './fixtures';
import type { Browser, Page, TestInfo } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// »Datenübertragung per Kabel« (/cable, /ether): one public ether without
// login. Two people on the same frequency see each other's lamps, someone on
// another frequency does not; a teacher changes the number of lamps for
// everyone (src/ruby/include/cable_ether.rb, src/static/include/cable.js).
//
// Parked for now (sections.yaml, »parked«): the ether then answers 403 to
// visitors without a login, so these tests wait until it is back.
test.skip(true, 'Datenübertragung per Kabel ist zurückgestellt');

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string') throw new Error('Playwright baseURL must be configured');
  return value;
}

// A frequency nobody else in this run uses (10.0 to 99.8 kHz, in tenths).
function someFrequency(): number {
  return 100 + Math.floor(Math.random() * 899);
}

async function openEther(browser: Browser, testInfo: TestInfo, freq: number, path = '/ether'): Promise<Page> {
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const page = await context.newPage();
  await page.addInitScript((f) => window.localStorage.setItem('cable-freq', String(f)), freq);
  await page.goto(path);
  await expect(page.locator('.cable-status')).toHaveText('verbunden');
  await expect(page.locator('.cable-lcd')).toContainText((freq / 10).toFixed(1));
  return page;
}

function lamp(page: Page, i: number) {
  return page.locator(`.cable-channel[data-lamp='${i}'] .cable-lamp`);
}

async function hold(page: Page, i: number) {
  await page.locator(`.cable-channel[data-lamp='${i}'] .cable-key`).hover();
  await page.mouse.down();
}

test('two people on one frequency see each other\'s lamps, another frequency does not', async ({ browser }, testInfo) => {
  const freq = someFrequency();
  const ada = await openEther(browser, testInfo, freq);
  const bob = await openEther(browser, testInfo, freq);
  const eve = await openEther(browser, testInfo, freq + 1);
  try {
    await hold(ada, 3);
    await expect(lamp(bob, 3)).toHaveClass(/\bon\b/);
    await expect(lamp(ada, 3)).toHaveClass(/\bon\b/);
    await expect(lamp(bob, 2)).not.toHaveClass(/\bon\b/);
    // Give a wrong delivery time to arrive before saying it did not.
    await ada.waitForTimeout(500);
    await expect(lamp(eve, 3)).not.toHaveClass(/\bon\b/);

    await ada.mouse.up();
    await expect(lamp(bob, 3)).not.toHaveClass(/\bon\b/);

    // Bob sends too, with the key 1, while the page is on screen.
    await bob.keyboard.down('1');
    await expect(lamp(ada, 1)).toHaveClass(/\bon\b/);
    await bob.keyboard.up('1');
    await expect(lamp(ada, 1)).not.toHaveClass(/\bon\b/);
  } finally {
    for (const page of [ada, bob, eve]) await page.context().close();
  }
});

test('nobody else can change the lamps; a teacher sets four for everyone', async ({ browser }, testInfo) => {
  const freq = someFrequency();
  const ada = await openEther(browser, testInfo, freq);
  const bob = await openEther(browser, testInfo, freq);
  const teacherContext = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const teacher = await teacherContext.newPage();
  try {
    await expect(ada.locator('.cable-teacher')).toBeHidden();
    await expect(ada.locator('.cable-key:visible')).toHaveCount(5);

    await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
    await teacher.goto('/cable');
    const panel = teacher.locator('.cable-teacher');
    await expect(panel).toBeVisible();
    await expect(panel).toContainText('auch für andere Klassen');

    // A countdown first, then cancelled.
    await panel.getByLabel('Wartezeit bis zur Änderung').fill('60');
    await panel.getByRole('button', { name: '3 Lampen' }).click();
    await expect(ada.locator('.cable-banner')).toContainText('dann gibt es nur noch 3 Lampen');
    await expect(ada.locator('.cable-channel.leaving')).toHaveCount(2);
    await panel.getByRole('button', { name: 'Abbrechen' }).click();
    await expect(ada.locator('.cable-banner')).toBeHidden();
    await expect(ada.locator('.cable-key:visible')).toHaveCount(5);

    // At once: four lamps.
    await panel.getByLabel('Wartezeit bis zur Änderung').fill('0');
    await panel.getByRole('button', { name: '4 Lampen' }).click();
    await expect(ada.locator('.cable-key:visible')).toHaveCount(4);
    await expect(bob.locator('.cable-key:visible')).toHaveCount(4);
    await expect(bob.locator('.cable-banner')).toContainText('nur noch 4 Lampen');

    // The teacher sees the frequency with two people on it (and the words).
    await expect(panel.locator('.cable-freqs tr', { hasText: (freq / 10).toFixed(1) })).toContainText('2');
    await panel.getByRole('button', { name: 'Nächstes Wort' }).click();
    await expect(panel.locator('.cable-word')).toHaveText(/^[A-Z]{5,11}$/);
    await expect(panel.locator('.cable-word-count')).toHaveText(/^1 von \d+$/);
  } finally {
    // Back to five lamps for everyone.
    const panel = teacher.locator('.cable-teacher');
    if (await panel.isVisible()) {
      await panel.getByLabel('Wartezeit bis zur Änderung').fill('0');
      await panel.getByRole('button', { name: '5 Lampen' }).click();
      await expect(ada.locator('.cable-key:visible')).toHaveCount(5);
    }
    for (const page of [ada, bob, teacher]) await page.context().close();
  }
});

test('after a dropped connection the page says so and reconnects on the same frequency', async ({ browser }, testInfo) => {
  const freq = someFrequency();
  const context = await browser.newContext({ baseURL: baseUrl(testInfo) });
  const page = await context.newPage();
  const tuned: number[] = [];
  let first = true;
  // Pass the websocket through, and end the first one after it is open.
  await page.routeWebSocket(/\/ws_cable$/, (ws) => {
    const server = ws.connectToServer();
    ws.onMessage((message) => {
      const data = JSON.parse(String(message));
      if (data.t === 'tune') tuned.push(data.f);
      server.send(message);
    });
    if (first) {
      first = false;
      setTimeout(() => ws.close(), 1500);
    }
  });
  await page.addInitScript((f) => window.localStorage.setItem('cable-freq', String(f)), freq);
  await page.goto('/ether');
  await expect(page.locator('.cable-notice')).toContainText('Die Verbindung ist unterbrochen');
  await expect(page.locator('.cable-status')).toHaveText('verbunden');
  await expect(page.locator('.cable-notice')).toBeHidden();
  expect(tuned).toEqual([freq, freq]);
  await context.close();
});

test('the websocket refuses other origins', async ({ request }) => {
  const response = await request.get('/ws_cable', {
    headers: {
      Connection: 'Upgrade', Upgrade: 'websocket', 'Sec-WebSocket-Version': '13',
      'Sec-WebSocket-Key': 'dGhlIHNhbXBsZSBub25jZQ==', Origin: 'http://example.org',
    },
  });
  expect(response.status()).toBe(403);
});
