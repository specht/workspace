import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Under an open task (reflection), teachers see the example answers from
// tasks.yaml or the answers of their current group, switchable; the choice
// is stored with the account. Students see neither.
test('teachers see example answers and the answers of their group under open tasks', async ({ browser }, testInfo) => {
  test.setTimeout(120_000);
  const task = 'tcpip/traceroute-erklaeren';
  const card = '#aufgabe-traceroute-erklaeren';
  const answer = `Jeder Router zieht von der TTL eins ab und meldet sich bei null; so findet traceroute einen nach dem anderen (${Date.now()}).`;

  const student = await (await browser.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-peer@example.com', testInfo);
  const saved = await (await student.request.post('/api/task/check', { data: { task, answer } })).json();
  expect(saved.status).toBe('sent');
  await student.goto('/tcpip');
  await expect(student.locator(card)).toBeVisible();
  await expect(student.locator('.task-teacher')).toHaveCount(0);
  const refused = await student.request.post('/api/task/group_answers', { data: { activity: 'tcpip', group: 'E2E' } });
  expect(refused.ok()).toBe(false);

  const errors: string[] = [];
  const teacher = await (await browser.newContext()).newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/tcpip');
  const panel = teacher.locator(`${card} .task-teacher`);
  await expect(panel).toBeVisible();
  await panel.locator(".task-teacher-view[data-view='group']").click();
  await panel.locator('.task-teacher-group').selectOption('E2E');
  await expect(panel.getByRole('button', { name: 'Antworten aus E2E' })).toHaveAttribute('aria-pressed', 'true');
  const mine = panel.locator('.task-teacher-answer', { hasText: 'E2E Peer' });
  await expect(mine).toContainText(answer);

  // The choice holds for the next page.
  await teacher.reload();
  await expect(panel.getByRole('button', { name: 'Antworten aus E2E' })).toHaveAttribute('aria-pressed', 'true');
  await expect(mine).toContainText(answer);

  await panel.getByRole('button', { name: 'Beispielantworten' }).click();
  await expect(panel.locator('.task-teacher-example').first()).toContainText('TTL');
  await teacher.reload();
  await expect(panel.getByRole('button', { name: 'Beispielantworten' })).toHaveAttribute('aria-pressed', 'true');
  await expect(panel.locator('.task-teacher-example')).toHaveCount(2);

  await panel.getByRole('button', { name: 'Ausblenden' }).click();
  await expect(panel.locator('.task-teacher-body')).toBeHidden();
  await teacher.reload();
  await expect(panel.locator('.task-teacher-body')).toBeHidden();
  // Back to the examples for whoever runs this next.
  await panel.getByRole('button', { name: 'Beispielantworten' }).click();
  expect(errors).toEqual([]);

  // A teacher with teaching materials but no group: the examples only.
  const materials = await (await browser.newContext()).newPage();
  await loginAsE2eUser(materials, 'e2e-materials-teacher@example.com', testInfo);
  await materials.goto('/tcpip');
  const theirs = materials.locator(`${card} .task-teacher`);
  await expect(theirs.locator('.task-teacher-example')).toHaveCount(2);
  await expect(theirs.locator('.task-teacher-view')).toHaveText(['Beispielantworten', 'Ausblenden']);

  await student.request.post('/api/task/delete', { data: { task } });
});
