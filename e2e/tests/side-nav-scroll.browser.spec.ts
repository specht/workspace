import { expect, test } from './fixtures';

// The wheel over the side navigation (»Auf dieser Seite«): a list that
// does not fit scrolls first, then the page; over a short list, and
// anywhere else, the page scrolls at once.

test.use({ viewport: { width: 1400, height: 900 } });

const scrollY = (page) => page.evaluate(() => window.scrollY);

test('over a short side navigation the page scrolls', async ({ page }) => {
  await page.goto('/crypto');
  const list = page.locator('.autotoc-secondary-items');
  await expect(list).toBeVisible();
  const box = (await list.boundingBox())!;
  await page.mouse.move(box.x + box.width / 2, box.y + 10);
  await page.mouse.wheel(0, 600);
  await expect.poll(() => scrollY(page)).toBeGreaterThan(100);
  // left of the navigation too
  const before = await scrollY(page);
  await page.mouse.move(5, 500);
  await page.mouse.wheel(0, 600);
  await expect.poll(() => scrollY(page)).toBeGreaterThan(before + 100);
});

test('over a long side navigation the list scrolls first, then the page', async ({ page }) => {
  await page.goto('/reference/personen');
  const list = page.locator('.autotoc-secondary-items');
  await expect(list).toBeVisible();
  expect(await list.evaluate((el) => el.scrollHeight > el.clientHeight)).toBe(true);
  const box = (await list.boundingBox())!;
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.wheel(0, 300);
  await expect.poll(() => list.evaluate((el) => el.scrollTop)).toBeGreaterThan(100);
  expect(await scrollY(page)).toBe(0);
  // at its end the page takes over
  for (let i = 0; i < 40 && await scrollY(page) === 0; i++) {
    await page.mouse.wheel(0, 800);
    await page.waitForTimeout(50);
  }
  expect(await scrollY(page)).toBeGreaterThan(0);
});
