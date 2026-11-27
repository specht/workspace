import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE, loginAsE2eUser } from './workspace';

// Cases for the development (docs/teacher-access.md, »Für die Entwicklung
// sichern«, cases_page.rb): a teacher clicks a student's cell on the class
// board and saves it with a note; an administrator finds it at /faelle,
// without the student's name, downloads it and deletes it.

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  return credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)![1];
}

test('a teacher saves a student solution for the development, an administrator finds it', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  const task = 'crypto/lawine';
  // the student, with an answer the check did not accept
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task } });
  // any stored result will do: the case keeps what the check knew
  await page.request.post('/api/task/check', { data: { task, answer: '2' } });

  // the teacher, on the class board
  const errors: string[] = [];
  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=crypto');
  const cell = teacher.locator(`table.class-board tr[data-email="${e2eEmail}"] td.cb-cell[data-task="${task}"]`);
  await cell.click();
  const modal = teacher.locator('#__template_modal');
  // visible, not only filled: Bootstrap fades the dialog in
  await expect(modal).toBeVisible();
  await expect(modal.locator('#case-note')).toBeVisible();
  await expect(modal).toContainText('Für die Entwicklung sichern');
  await expect(modal).toContainText('ohne Namen');
  const note = `Sieht richtig aus, ${Date.now()}`;
  await modal.locator('#case-note').fill(note);
  const saved = teacher.waitForResponse((r) => r.url().endsWith('/api/class/save_case'));
  await modal.getByRole('button', { name: 'Sichern' }).click();
  const answer = await (await saved).json();
  expect(answer.id).toMatch(/^\d{8}-\d{6}-[0-9a-f]{6}$/);
  await expect(cell).toHaveClass(/cb-case-saved/);

  // the administrator: the case with its note, without the student's address
  const admin = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  try {
    const requested = await (await admin.request.post('/api/request_login', { data: { email: adminEmail() } })).json();
    await admin.request.post('/api/complete_login', { data: { tag: requested.tag, code: E2E_LOGIN_CODE } });
    const list = await admin.newPage();
    await list.goto('/faelle');
    const card = list.locator(`.case-card[data-id="${answer.id}"]`);
    await expect(card).toContainText(task);
    await expect(card).toContainText(note);
    await expect(card).toContainText('die Antwort');
    expect(await card.innerText()).not.toContain(e2eEmail);
    const download = await admin.request.get(`/api/faelle/${answer.id}`);
    expect(download.status()).toBe(200);
    expect(download.headers()['content-type']).toContain('gzip');
    list.on('dialog', (d) => d.accept());
    await card.locator('.case-delete').click();
    await expect(card).toHaveCount(0);
    // a teacher does not see the page
    const forbidden = await teacher.request.get('/faelle');
    expect(await forbidden.text()).not.toContain(note);
  } finally {
    await admin.close();
    await teacherContext.close();
  }
  expect(errors).toEqual([]);
});
