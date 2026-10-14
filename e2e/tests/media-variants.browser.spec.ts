import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// On a tablet a wide image scrolls sideways; its switches (CRT, anaglyph,
// dark) stay in the visible top right corner while it moves beneath them.
test.use({ viewport: { width: 820, height: 1180 }, hasTouch: true, isMobile: true });

test('the switches on a scrolling image stay in place', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pixelram');
  const box = page.locator('.media-variant-box-scroll').first();
  await box.scrollIntoViewIfNeeded();
  const right = () => page.evaluate(() =>
    document.querySelector('.media-variant-box-scroll .media-variant-toggles')!.getBoundingClientRect().right);
  const before = await right();
  expect(before).toBeLessThanOrEqual(820);
  await box.locator('.scroll-x').evaluate((el) => { el.scrollLeft = el.scrollWidth; });
  expect(await right()).toBe(before);
});
