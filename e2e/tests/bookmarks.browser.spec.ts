import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Bookmarks in the table of contents on the left (bookmarks.js,
// include/bookmarks.rb): set beside an entry, kept on the server with the
// account, so that they are there again after a reload, and removed again.

test('a bookmark in the table of contents stays after a reload', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 1500, height: 900 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/video-compression');

  const entries = page.locator('.autotoc-secondary .toc-entry');
  await expect(entries.first()).toBeVisible();
  const entry = entries.nth(2);
  const button = entry.locator('.toc-bookmark');
  // start from no bookmark here, whatever an earlier run left
  if ((await button.getAttribute('aria-pressed')) === 'true') {
    await entry.hover();
    await button.click();
    await expect(button).toHaveAttribute('aria-pressed', 'false');
  }

  await entry.hover();
  await button.click();
  await expect(button).toHaveAttribute('aria-pressed', 'true');
  // still under the mouse: the sign of what the next click does
  await expect(button.locator('i')).toHaveClass(/bi-bookmark-x-fill/);
  await page.mouse.move(0, 0);
  await expect(button.locator('i')).toHaveClass(/bi-bookmark-fill/);

  await page.reload();
  const again = page.locator('.autotoc-secondary .toc-entry').nth(2).locator('.toc-bookmark');
  await expect(again).toHaveAttribute('aria-pressed', 'true');
  // set ones show without hovering
  await expect(again).toHaveCSS('opacity', '1');

  await page.locator('.autotoc-secondary .toc-entry').nth(2).hover();
  await again.click();
  await expect(again).toHaveAttribute('aria-pressed', 'false');
  await page.reload();
  await expect(page.locator('.autotoc-secondary .toc-entry').nth(2).locator('.toc-bookmark')).toHaveAttribute('aria-pressed', 'false');
});
