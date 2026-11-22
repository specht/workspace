import { expect, test } from './fixtures';
import type { Page, Locator } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// After the lightbox the picture it showed last is visible on the page too
// (tutorial-preview.js, showOnPage): on the desktop the page scrolls to it,
// on a phone the gallery that swipes turns to it. The gallery: the thirteen
// walls and floors of the hall in Level 0 of the Backrooms.
const PAGE = '/level-0-c1d8a2f7';
const FIRST = 'Der leere Saal mit gelber Tapete, Teppich und flacher Decke';

async function gallery(page: Page): Promise<Locator> {
  const first = page.locator(`img[alt^="${FIRST}"]`);
  await first.scrollIntoViewIfNeeded();
  return page.locator('.gallery').filter({ has: first });
}

async function openFirstAndGoToLast(page: Page, figures: Locator) {
  const count = await figures.count();
  expect(count).toBeGreaterThan(3);
  await figures.first().locator('img').click();
  await expect(page.locator('.tutorial-preview-lightbox')).toHaveClass(/is-visible/);
  for (let i = 1; i < count; i++) await page.keyboard.press('ArrowRight');
  await page.keyboard.press('Escape');
  await expect(page.locator('.tutorial-preview-lightbox')).not.toHaveAttribute('open', '');
  await page.waitForTimeout(600);
  return figures.last();
}

test('the last picture of the lightbox is on screen afterwards (desktop)', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto(PAGE);
  const figures = (await gallery(page)).locator('figure');
  // the first row at the top of the window: the last one is below it
  await figures.first().evaluate((el) => window.scrollTo(0, el.getBoundingClientRect().top + window.scrollY - 80));
  const last = await openFirstAndGoToLast(page, figures);
  await expect(last).toBeInViewport({ ratio: 0.9 });
});

test('the last picture of the lightbox is on screen afterwards (phone)', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto(PAGE);
  const row = await gallery(page);
  await expect(row).toHaveClass(/swipe/);
  const figures = row.locator('figure');
  const last = await openFirstAndGoToLast(page, figures);
  await expect(last).toBeInViewport({ ratio: 0.9 });
  // and its dot is the one lit
  const dots = page.locator('.gallery-dots').filter({ has: page.locator('span') }).nth(await page.evaluate((first) => {
    const all = [...document.querySelectorAll('.gallery.swipe')];
    return all.findIndex((g) => g.querySelector(`img[alt^="${first}"]`));
  }, FIRST));
  await expect(dots.locator('span').last()).toHaveClass(/active/);
});

// A single picture opens on a click, not at the end of a drag; on a tablet,
// where a wide screenshot already scrolls sideways in its box, it does not
// open at all (tutorial-preview.js, zoomable and dragged). The neighbours
// of a node on the map of »Routenplaner«.
const SHOT = 'img[alt^="Der Graph über der blassen Karte, nah herangezoomt"]';

test('a picture opens on a click, not after a drag', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 1440, height: 900 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/routenplaner');
  const shot = page.locator(SHOT);
  await shot.scrollIntoViewIfNeeded();
  await expect(shot).toHaveClass(/gallery-zoom/);
  const box = (await shot.boundingBox())!;
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 - 60, box.y + box.height / 2, { steps: 6 });
  await page.mouse.up();
  await page.waitForTimeout(400);
  await expect(page.locator('.tutorial-preview-lightbox')).not.toHaveAttribute('open', '');
  await shot.click();
  await expect(page.locator('.tutorial-preview-lightbox')).toHaveClass(/is-visible/);
});

test('a picture that scrolls sideways on a tablet does not open', async ({ browser, e2eEmail }, testInfo) => {
  const context = await browser.newContext({ baseURL: testInfo.project.use.baseURL, viewport: { width: 820, height: 1180 }, isMobile: true, hasTouch: true });
  const page = await context.newPage();
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/routenplaner');
  const shot = page.locator(SHOT);
  await shot.scrollIntoViewIfNeeded();
  // wider than its box: it scrolls
  expect(await shot.evaluate((img) => img.parentElement!.scrollWidth > img.parentElement!.clientWidth)).toBe(true);
  await expect(shot).not.toHaveClass(/gallery-zoom/);
  await shot.tap();
  await page.waitForTimeout(400);
  await expect(page.locator('.tutorial-preview-lightbox')).not.toHaveAttribute('open', '');
  await context.close();
});
