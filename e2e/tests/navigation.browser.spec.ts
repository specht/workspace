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

// Below 1400 px a page with chapters shows a bar once the header has
// scrolled away; from 1400 px the chapters stand in a column beside the
// text instead, with its own »Nach oben«, and there is no bar beside it.
test('pages with chapters have a bar with »Nach oben«', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.setViewportSize({ width: 1200, height: 1000 });
  for (const path of ['/', '/prolog', '/reference/farben']) {
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

test('on a wide screen the chapters stand beside the text, with »Nach oben«', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.setViewportSize({ width: 1600, height: 1000 });
  for (const path of ['/prolog', '/reference/farben']) {
    await page.goto(path);
    await scrollDown(page);
    await expect(page.locator('nav.autotoc')).not.toBeVisible();
    await page.locator('.autotoc-secondary-up').click();
    await expect.poll(() => page.evaluate(() => window.scrollY)).toBe(0);
  }
});

// A Handbuch part shown in an activity is one box: its sub-headings are h4
// and stay out of the page's own table of contents.
test('included Handbuch parts stay out of the page TOC', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/static-website');
  const inBox = page.locator('.reference-include').getByRole('heading', { name: 'Bildformate' });
  await expect(inBox).toHaveCount(1);
  expect(await inBox.evaluate(h => h.tagName)).toBe('H4');
  const toc = await page.evaluate(() =>
    ((window as any).autotoc_secondary_headings as HTMLElement[]).map(h => h.textContent!.trim()));
  expect(toc).toContain('Von der URL zum Dateinamen');
  expect(toc).not.toContain('Bildformate');
});
