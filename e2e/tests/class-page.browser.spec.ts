import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The class page has three tabs with one job each; the chosen tab stays in
// the address. »Lernende« shows the server's cards and each Workspace live.
test('the class page: tabs, menu and the Workspaces of the group', async ({ page }, testInfo) => {
  test.setTimeout(90_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/klasse?group=E2E&activity=bif');

  await expect(page.getByRole('tab', { name: 'Fortschritt' })).toHaveAttribute('aria-selected', 'true');
  await expect(page.locator('table.class-board')).toBeVisible();
  await page.getByRole('button', { name: 'Jetzt nachsehen' }).click();
  await expect(page.locator('.class-live-menu .class-live-check').first()).toBeVisible();
  await page.keyboard.press('Escape');

  await page.getByRole('tab', { name: /Lernende/ }).click();
  await expect(page).toHaveURL(/tab=lernende/);
  await expect(page.locator('#class-panel-fortschritt')).toBeHidden();
  await expect(page.locator('.class-people-table')).toBeVisible();
  await expect(page.locator('.class-server-cards')).toBeVisible({ timeout: 20_000 });
  await expect(page.locator('.class-server-cpu p').first()).toContainText('% of');

  await page.reload();
  await expect(page.getByRole('tab', { name: /Lernende/ })).toHaveAttribute('aria-selected', 'true');
  await page.getByRole('tab', { name: /Lernende/ }).press('ArrowRight');
  await expect(page.getByRole('tab', { name: 'Startseite' })).toHaveAttribute('aria-selected', 'true');
  await expect(page.locator('.class-pins')).toBeVisible();
  expect(errors).toEqual([]);
});

// The class page opens where the teacher left it: the last group, and in
// each group the last activity.
test('the class page remembers the group and each group\'s activity', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  const testGroup = 'Testgruppe (E2E Teacher)';
  await page.goto('/klasse?group=E2E&activity=bif');
  await page.goto('/klasse?group=' + encodeURIComponent(testGroup) + '&activity=tcpip');
  await page.goto('/klasse');
  await expect(page.locator('.class-board-select select')).toHaveValue(testGroup);
  await expect(page.locator('.class-activity-select select')).toHaveValue('tcpip');
  await page.locator('.class-board-select select').selectOption('E2E');
  await expect(page.locator('.class-activity-select select')).toHaveValue('bif');
  await page.goto('/klasse');
  await expect(page.locator('.class-board-select select')).toHaveValue('E2E');
});
