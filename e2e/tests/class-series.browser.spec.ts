import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A series of activities (sections.yaml series:, the Backrooms) as a whole on
// the class page: a column per activity, a row per student, each cell how
// many of its tasks are solved (class_series_html).
test('a teacher sees a whole series at once', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?tab=fortschritt&activity=series:level-0-c1d8a2f7');
  const table = page.locator('.class-series-table');
  await expect(table).toBeVisible();
  await expect(table.locator('thead th')).toHaveCount(4 + 2);
  await expect(table.locator('thead th a').first()).toHaveAttribute('href', /activity=level-0-c1d8a2f7/);
  await expect(page.locator('.class-activity-choice option[value="series:level-0-c1d8a2f7"]')).toHaveCount(1);
  // a column leads to the activity's own board
  await table.locator('thead th a').nth(1).click();
  await expect(page.locator('.class-series-table')).toHaveCount(0);
  await expect(page).toHaveURL(/activity=level-1-7e3a91c4/);
});
