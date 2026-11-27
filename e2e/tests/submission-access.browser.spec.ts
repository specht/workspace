import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE, loginAsE2eUser } from './workspace';

// Who opens a student's stored document (/abgabe/dokument/…, the version
// last checked, submission_owner_for): teachers of the student's groups and
// administrators of every group, so that a teacher can send an
// administrator the link when a check does not accept what looks right
// (docs/teacher-access.md, »Wer was sieht«); another student cannot.

const repo = path.resolve(__dirname, '..', '..');
const content = path.join(repo, 'src/content/formatting-marks/stufen');

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  return credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)![1];
}

test('an administrator opens a student document a teacher sends, another student cannot', async ({ page, browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/formatting-marks');
  const card = page.locator(".task-card[data-task='formatting-marks/einladung']:visible");
  const checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await card.locator('.document-input').setInputFiles(path.join(content, 'einladung.docx'));
  await checked;
  const link = `/abgabe/dokument/formatting-marks/einladung?email=${encodeURIComponent(e2eEmail)}`;
  const file = `/api/task/document_file/formatting-marks/einladung?email=${encodeURIComponent(e2eEmail)}`;

  const signIn = async (email: string) => {
    const context = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
    const requested = await (await context.request.post('/api/request_login', { data: { email } })).json();
    await context.request.post('/api/complete_login', { data: { tag: requested.tag, code: E2E_LOGIN_CODE } });
    return context;
  };
  const admin = await signIn(adminEmail());
  const other = await signIn('e2e-peer@example.com');
  try {
    const view = await admin.newPage();
    const response = await view.goto(link);
    expect(response!.status()).toBe(200);
    await expect(view.locator('body')).toContainText('einladung.docx');
    expect((await admin.request.get(file)).status()).toBe(200);

    expect((await other.request.get(link)).status()).toBe(403);
    expect((await other.request.get(file)).status()).toBe(403);
  } finally {
    await admin.close();
    await other.close();
  }
});
