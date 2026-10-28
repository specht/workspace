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
  // what follows is dimmed, not hidden
  await expect(page.locator('.meeting-later').first()).toBeVisible();
  await expect(page.locator('h2', { hasText: 'Passwörter speichern' }).locator('xpath=ancestor-or-self::*[contains(@class, "meeting-later")]')).toHaveCount(1);
  const before = await teacher.locator('.class-meeting-count').textContent();

  // solving the task: the student is there
  const card = page.locator(".task-card[data-task='crypto/lawine']");
  await card.getByText('Fast alle', { exact: true }).click();
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(box).toHaveClass(/meeting-arrived/);
  await expect(box).toContainText('Du bist da.');
  await expect(box).toContainText('jemandem aus deiner Klasse helfen');

  // the board counts the student, without a reload
  const arrivedBefore = parseInt(before!, 10);
  await expect(teacher.locator('.class-meeting-count')).toHaveText(new RegExp('^' + (arrivedBefore + 1) + ' von \\d+ da$'), { timeout: 10_000 });

  // everyone else absent today: everyone present is there
  const line = teacher.locator('.class-meeting-status');
  await line.locator('summary', { hasText: 'Noch nicht da' }).click();
  for (let i = 0; i < 60 && await line.locator("button.meeting-away[data-away='1']").count() > 0; i++) {
    await line.locator("button.meeting-away[data-away='1']").first().click();
  }
  await expect(line).toHaveClass(/meeting-all/);
  await expect(line).toContainText('alle da');
  await expect(line).toContainText(/Fehlen heute: \d+/);

  // lifted: gone from the open page
  await teacher.locator('.class-meeting-choice').selectOption('');
  await expect(box).toHaveCount(0, { timeout: 10_000 });
  await teacherContext.close();
  expect(errors).toEqual([]);
});

// The teacher sets and lifts it right on the activity page, for the group
// chosen on the class page.
test('a teacher places the meeting point on the activity page', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/crypto');
  const box = page.locator('aside.meeting-point');

  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=crypto');
  await teacher.goto('/crypto');
  const slot = teacher.locator('.meeting-slot:not([hidden])').filter({ hasText: 'Treffpunkt für E2E' });
  await expect(slot.first()).toBeVisible();
  // after the second task
  await teacher.locator('.meeting-slot').nth(1).getByRole('button').click();
  await expect(teacher.locator('aside.meeting-point')).toContainText(/von E2E: \d+ von \d+ da/);
  await expect(teacher.locator('.meeting-slot').nth(1)).toBeHidden();
  await expect(teacher.locator('.meeting-slot').first()).toContainText('hierher verschieben');
  await expect(box).toBeVisible({ timeout: 10_000 });
  await expect(box).toHaveCount(1);

  await teacher.locator('aside.meeting-point .meeting-lift').click();
  await expect(teacher.locator('aside.meeting-point')).toHaveCount(0);
  await expect(box).toHaveCount(0, { timeout: 10_000 });
  await teacherContext.close();
});

// A task inside the after block of a prediction (kata-bits: »Ein Byte
// addieren« waits for »200 + 100 in einem Byte«) keeps its slot inside that
// block, hidden with it; the prediction's slot goes before the block, so the
// two never stand side by side, not even with the block open.
test('a task inside an after block keeps its meeting slot inside it', async ({ browser }, testInfo) => {
  test.setTimeout(60_000);
  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=kata-bits');
  await teacher.goto('/kata-bits');
  const after = teacher.locator(".after-task[data-after='byte-overflow']");
  await expect(after).toHaveCount(1);
  await expect(teacher.locator('.meeting-slot').first()).toBeAttached();
  // the prediction's slot right before the block, the task's inside it at its end
  await expect(teacher.locator(".meeting-slot + .after-task[data-after='byte-overflow']")).toHaveCount(1);
  await expect(after.locator('.after-task-body .meeting-slot')).toHaveCount(1);
  await expect(teacher.locator(".after-task[data-after='byte-overflow'] + .meeting-slot")).toHaveCount(0);
  await teacherContext.close();
});
