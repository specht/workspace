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
  // name, last active, four sheets, finished
  await expect(table.locator('thead th')).toHaveCount(4 + 3);
  await expect(table.locator('thead th a').first()).toHaveAttribute('href', /activity=level-0-c1d8a2f7/);
  await expect(page.locator('.class-activity-choice option[value="series:level-0-c1d8a2f7"]')).toHaveCount(1);
  // nothing started is a dash, not »0/n«
  await expect(table.locator('td.class-series-none').first()).toHaveText('noch nichts–');
  // a column leads to the activity's own board
  await table.locator('thead th a').nth(1).click();
  await expect(page.locator('.class-series-table')).toHaveCount(0);
  await expect(page).toHaveURL(/activity=level-1-7e3a91c4/);
});

// The Office series: a sheet counts its tasks without the extras, so it can
// be finished; solved extras stand apart (+n). SERIES_SHOTS=dir keeps a picture.
test('a series counts a sheet without its extras', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?tab=fortschritt&activity=series:formatting-marks');
  const table = page.locator('.class-series-table');
  await expect(table).toBeVisible();
  await expect(table.locator('thead .class-series-sheet').first()).toContainText('Extras');
  if (process.env.SERIES_SHOTS) await page.screenshot({ path: `${process.env.SERIES_SHOTS}/series.png`, fullPage: true });
});
