import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// On a long page the way back to the start page is in the bar that appears
// once the header has scrolled away; while hidden, the bar is out of the Tab
// order.
test('the bar at the top leads back to the start page', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const path of ['/feuerwerk', '/reference']) {
    await page.goto(path);
    const bar = page.locator('nav.autotoc');
    await expect(bar).toHaveJSProperty('inert', true);
    await page.evaluate(() => window.scrollTo(0, document.body.scrollHeight * 0.6));
    await expect(bar).not.toHaveClass(/hidden/);
    await expect(bar).toHaveJSProperty('inert', false);
    await bar.getByRole('link', { name: 'Startseite' }).click();
    await page.waitForURL((url) => url.pathname === '/');
  }
  // Not on the start page itself.
  await expect(page.locator('nav.autotoc .autotoc-home')).toHaveCount(0);
});
