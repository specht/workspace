import { expect, test } from './fixtures';
import type { Page } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// Pages with chapters show a bar once the header has scrolled away: the
// chapters, and on its left edge »Nach oben«, back to the header with the
// way home and the whole menu. While hidden, the bar is out of the Tab
// order. Pages without chapters (the Handbuch overview) have no bar.

async function scrollDown(page: Page) {
  await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight * 0.6));
  await page.waitForTimeout(500);
}

test('pages with chapters have a bar with »Nach oben«', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const path of ['/', '/feuerwerk', '/reference/farben']) {
    await page.goto(path);
    const bar = page.locator('nav.autotoc');
    await expect(bar).toHaveJSProperty('inert', true);
    await scrollDown(page);
    await expect(bar).not.toHaveClass(/hidden/);
    await expect(bar).toHaveJSProperty('inert', false);
    await bar.getByRole('button', { name: 'Nach oben' }).click();
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBe(0);
  }

  await page.goto('/reference');
  await scrollDown(page);
  await expect(page.locator('nav.autotoc')).toHaveClass(/hidden/);
});
