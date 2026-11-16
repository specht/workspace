import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Below the class board, each prediction lists what the class predicted
// first, how often, and what a wrong prediction is told, without names
// (ClassBoard.predictions, docs/teacher-access.md »Was die Klasse vermutet
// hat«). The first answer counts, not the last.
test('the class page counts what the class predicted first', async ({ browser }, testInfo) => {
  test.setTimeout(120_000);
  const task = 'calculator/hoch-vorhersage';

  const student = await (await browser.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-peer@example.com', testInfo);
  await student.request.post('/api/task/delete', { data: { task } });
  const first = await (await student.request.post('/api/task/check', { data: { task, answer: '64' } })).json();
  expect(first.status).toBe('wrong');
  const second = await (await student.request.post('/api/task/check', { data: { task, answer: '512' } })).json();
  expect(second.status).toBe('correct');

  const errors: string[] = [];
  const teacher = await (await browser.newContext()).newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&activity=calculator');
  const list = teacher.locator('details.class-predictions', { hasText: 'Hoch und noch mal hoch' });
  await expect(list).toHaveCount(1);
  await list.locator('summary').click();
  const wrong = list.locator('.cb-prediction.cb-wrong', { hasText: '64' });
  await expect(wrong).toBeVisible();
  await expect(wrong.locator('.cb-prediction-count')).toHaveText(/^[1-9]\d*×$/);
  await expect(wrong.locator('.cb-prediction-message')).toContainText('Das wäre');
  await expect(wrong.locator('.cb-prediction-message code').first()).toHaveText('(2 ^ 3) ^ 2');
  // no names in the list
  await expect(list).not.toContainText('E2E Peer');
  expect(errors).toEqual([]);

  await student.request.post('/api/task/delete', { data: { task } });
  await student.context().close();
  await teacher.context().close();
});

// A card »Vom Beispiel zur Regel« (a choice with prediction: true): once
// answered, the rule after it shows; »Meine Antwort löschen« unticks the
// choice and hides the rule again.
test('deleting the answer of a rule card resets it and hides the rule again', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  const task = 'calculator/baum-regel';
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task } });
  await page.goto('/calculator');
  const card = page.locator(`.task-card[data-task="${task}"]:visible`);
  const after = page.locator('.after-task[data-after="baum-regel"]');
  await expect(after).not.toHaveClass(/after-task-shown/);
  await card.locator('label', { hasText: 'von links nach rechts' }).click();
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'wrong');
  await expect(after).toHaveClass(/after-task-shown/);

  await card.locator('.task-delete').click();
  await page.locator('#__template_modal .btn-danger').click();
  await expect(card.locator('.task-delete')).toBeHidden();
  await expect(card.locator('.form-check-input:checked')).toHaveCount(0);
  await expect(after).not.toHaveClass(/after-task-shown/);
  await expect(card).not.toHaveAttribute('data-status', /./);
});
