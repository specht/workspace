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
