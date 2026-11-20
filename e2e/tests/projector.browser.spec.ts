import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The projector (docs/teacher-access.md, »Am Beamer«): the teacher opens it
// from the class board; each student is a dot at the first task still open
// for them, without names until the teacher shows them. A solved task moves
// the dot on without a reload.

test('the projector shows how far the class is and follows a solved task', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  const errors: string[] = [];

  // the student, with the first task of crypto open again
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/crypto');
  await page.evaluate(() => fetch('/api/task/delete', {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ task: 'crypto/lawine' }),
  }));
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.reload();
  await states;

  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL, viewport: { width: 1600, height: 900 } });
  const teacher = await teacherContext.newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=crypto');
  const link = teacher.locator('a.class-projector-link', { hasText: 'Am Beamer zeigen' });
  await expect(link).toHaveAttribute('target', '_blank');
  await teacher.goto((await link.getAttribute('href'))!);

  const projector = teacher.locator('.projector');
  await expect(projector.locator('h1')).toHaveText('Kryptographie im Terminal');
  const first = projector.locator('.projector-column').first();
  await expect(first).toHaveAttribute('aria-label', /^Aufgabe 1, /);
  await expect(projector.locator('.projector-column.is-done')).toHaveCount(1);
  // without names: dots only, no address anywhere in the page
  await expect(projector.locator('.projector-chip')).toHaveCount(0);
  expect(await projector.locator('.projector-board').innerHTML()).not.toContain('@');
  const before = await first.locator('.projector-dot').count();
  expect(before).toBeGreaterThan(0);
  await expect(projector.locator('.projector-total')).toContainText(/\d+ von \d+ fertig/);

  // solved: one dot fewer at the first task, without a reload
  const card = page.locator(".task-card[data-task='crypto/lawine']");
  await card.getByText('Fast alle', { exact: true }).click();
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(first.locator('.projector-dot')).toHaveCount(before - 1, { timeout: 10_000 });

  // names: short ones, kept in the address for a reload
  await projector.locator('.projector-names').click();
  await expect(projector.locator('.projector-names')).toHaveAttribute('aria-pressed', 'true');
  await expect(teacher).toHaveURL(/names=1/);
  await expect(projector.locator('.projector-dot')).toHaveCount(0);
  await expect(projector.locator('.projector-chip').first()).toBeVisible();
  await teacher.reload();
  await expect(projector.locator('.projector-chip').first()).toBeVisible();
  await projector.locator('.projector-names').click();
  await expect(teacher).not.toHaveURL(/names=1/);
  await teacher.screenshot({ path: testInfo.outputPath('projector.png') });

  // as wide as the screen, on a phone too
  for (const width of [1600, 390]) {
    await teacher.setViewportSize({ width, height: 844 });
    expect(await teacher.evaluate(() => document.documentElement.scrollWidth)).toBeLessThanOrEqual(width);
  }

  // a group that is not the teacher's
  const other = await teacher.goto('/beamer?group=nicht-da&activity=crypto');
  expect(other!.status()).toBe(403);
  await teacherContext.close();
  expect(errors).toEqual([]);
});
