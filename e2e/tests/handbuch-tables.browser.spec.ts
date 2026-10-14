import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// On a phone a Handbuch table is a list: rows one after another, values
// labelled with their column, nothing wider than the screen.
test('Handbuch tables read as lists on a phone', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const path of ['/reference/minicpu', '/reference/minicpu-io', '/reference/er-modelle']) {
    await page.goto(path);
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), path).toBe(true);
  }
  await page.goto('/reference/minicpu');
  const row = page.locator('table.minicpu-table tr', { hasText: 'ADD A, B' }).first();
  expect(await row.evaluate((tr) => getComputedStyle(tr).display)).toBe('block');
  await expect(row.locator('td[data-label="Bytes"]')).toHaveText('2');
  await expect(row.locator('td[data-label="Opcode"]')).toHaveText('0x20');

  await page.setViewportSize({ width: 1200, height: 900 });
  expect(await row.evaluate((tr) => getComputedStyle(tr).display)).toBe('table-row');
});
