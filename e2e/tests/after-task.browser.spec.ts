import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// What gives away a task's answer waits for it (AfterTask, docs/didaktik.md):
// after an ordinary task until it is solved, so that a wrong answer does not
// show the right one; for teachers too, who open it with a button; for
// guests too.

test('an explanation waits until the task above is solved', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  // the cards answer once tasks.js has their states
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/crypto');
  await states;
  const card = page.locator(".task-card[data-task='crypto/fingerabdruck']");
  const after = page.locator(".after-task[data-after='fingerabdruck']");
  const answer = async (label: string, status: string) => {
    await card.getByText(label, { exact: true }).click();
    await card.locator('.task-submit').click();
    await expect(card).toHaveAttribute('data-status', status);
  };
  await answer('Ein Hash des privaten Schlüssels', 'wrong');
  await expect(after.locator('.after-task-wait')).toBeVisible();
  await expect(after.locator('.after-task-wait')).toContainText('sobald du die Aufgabe oben gelöst hast');
  await expect(after.locator('.after-task-body')).toBeHidden();
  await answer('Ein Hash des öffentlichen Schlüssels', 'correct');
  await expect(after.locator('.after-task-body')).toBeVisible();
});

test('a teacher sees the page as the class does and can open it', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/pip-1');
  const after = page.locator(".after-task[data-after='predict']");
  const card = page.locator(".task-card[data-task='pip-1/predict']");
  test.skip(await card.getAttribute('data-status') !== null && ['correct', 'wrong'].includes((await card.getAttribute('data-status'))!),
    'the teacher account has answered this prediction already');
  await expect(after.locator('.after-task-body')).toBeHidden();
  await after.getByRole('button', { name: /Für Lehrkräfte: jetzt zeigen/ }).click();
  await expect(after.locator('.after-task-body')).toBeVisible();
});

// A guest waits as a student does: otherwise a student could read the
// answers in a private window before guessing.
test('a guest waits too and is told to log in', async ({ page }) => {
  await page.goto('/crypto');
  const after = page.locator(".after-task[data-after='fingerabdruck']");
  await expect(after.locator('.after-task-wait')).toContainText('sobald du angemeldet bist');
  await expect(after.locator('.after-task-body')).toBeHidden();
  await expect(after.getByRole('button', { name: /jetzt zeigen/ })).toHaveCount(0);
});
