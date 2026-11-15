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
