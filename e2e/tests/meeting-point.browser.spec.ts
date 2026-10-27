import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The meeting point (docs/plan-treffpunkt.md): the teacher sets it on the
// class board after a task; the student's open page shows it without a
// reload, quiet until the student has arrived; lifting it removes it.

test('a meeting point reaches the open page, counts the student as there and goes again', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));

  // the student, on the page, with the first task not yet solved
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/crypto');
  await page.evaluate(() => fetch('/api/task/delete', {
    method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ task: 'crypto/lawine' }),
  }));
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.reload();
  await states;
  const box = page.locator('aside.meeting-point');

  // the teacher sets the point after that task
  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=crypto');
  const choice = teacher.locator('.class-meeting-choice');
  await choice.selectOption('');
  await teacher.waitForLoadState('load');
  await choice.selectOption('lawine');
  await expect(teacher.locator('.class-meeting-status')).toContainText('Treffpunkt nach Aufgabe 1');
  await expect(teacher.locator('th.cb-meeting')).toHaveCount(1);

  // the student's page shows it, quiet, right after the card
  await expect(box).toBeVisible({ timeout: 10_000 });
  await expect(box).toContainText('Hier trifft sich deine Klasse');
  await expect(box).not.toHaveClass(/meeting-arrived/);

  // solving the task: the student is there
  const card = page.locator(".task-card[data-task='crypto/lawine']");
  await card.getByText('Fast alle', { exact: true }).click();
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(box).toHaveClass(/meeting-arrived/);
  await expect(box).toContainText('Du bist da.');
  await expect(box).toContainText('jemandem aus deiner Klasse helfen');

  // the board counts the student
  await teacher.reload();
  await expect(teacher.locator('.class-meeting-status')).toContainText(/[1-9]\d* von \d+ da/);

  // lifted: gone from the open page
  await teacher.locator('.class-meeting-choice').selectOption('');
  await expect(box).toHaveCount(0, { timeout: 10_000 });
  await teacherContext.close();
  expect(errors).toEqual([]);
});
