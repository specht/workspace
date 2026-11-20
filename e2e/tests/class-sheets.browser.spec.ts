import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// On a sheet of a series the class page shows the series' sheets by number
// (class_sheet_strip_html), and the list of activities opens at the chosen
// one instead of at the top (searchable_select).
test('a sheet of a series leads to the others', async ({ page }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.setViewportSize({ width: 1400, height: 900 });
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?group=E2E&tab=fortschritt&activity=character-formatting');
  const strip = page.locator('nav.class-sheets');
  await expect(strip.locator('a[aria-current=page]')).toHaveText('Blatt 2');
  await expect(strip.locator('.class-sheets-all')).toHaveAttribute('href', /activity=series%3Aformatting-marks/);

  // the list opens at the chosen sheet, in view
  const field = page.locator('.class-activity-choice input');
  await field.click();
  const active = page.locator('.class-activity-choice .suggest-menu .dropdown-item.active');
  await expect(active).toHaveText(await field.inputValue());
  await expect(active).toBeInViewport();
  await page.screenshot({ path: testInfo.outputPath('class-sheets-menu.png') });
  await page.keyboard.press('Escape');
  await page.screenshot({ path: testInfo.outputPath('class-sheets.png') });

  // the next sheet by its title
  await strip.locator('.class-sheets-next').click();
  await expect(page).toHaveURL(/activity=/);
  await expect(page.locator('nav.class-sheets a[aria-current=page]')).toHaveText('Blatt 3');
  for (const width of [390]) {
    await page.setViewportSize({ width, height: 844 });
    expect(await page.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(width);
  }
  await page.screenshot({ path: testInfo.outputPath('class-sheets-phone.png'), fullPage: false });
  expect(errors).toEqual([]);
});
