import fs from 'node:fs';
import path from 'node:path';
import type { Page } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Feedback of teachers on an activity (src/ruby/include/activity_feedback.rb):
// the teacher answers on the class page (»Rückmeldung geben«, always there;
// the card that asks by itself needs a week without results, which the
// Ruby tests cover), changes the answer later without a second one, the
// administration reads it on /rueckmeldungen, a student gets 403 there and
// at the API. HS_SHOTS=dir keeps pictures of the form and the page, desktop
// and phone, light and dark.

const shots = process.env.HS_SHOTS;

async function keep(page: Page, name: string, selector: string) {
  if (!shots) return;
  fs.mkdirSync(shots, { recursive: true });
  for (const theme of ['light', 'dark']) {
    await page.evaluate((t) => document.documentElement.setAttribute('data-bs-theme', t), theme);
    await page.waitForTimeout(200);
    await page.locator(selector).first().screenshot({ path: path.join(shots, `${name}-${theme}.png`) });
  }
  await page.evaluate(() => document.documentElement.setAttribute('data-bs-theme', 'light'));
}

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email) throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

const TEACHER = 'e2e-teacher@example.com';
const BOARD = '/klasse?group=E2E&tab=fortschritt&activity=crypto';

test('a teacher gives feedback on an activity, changes it, and the administration reads it', async ({ browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  const marker = `Probe ${Date.now().toString(36)}: mehr Zeit für die Passwörter.`;
  const errors: string[] = [];

  const teacherContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const teacher = await teacherContext.newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, TEACHER, testInfo);
  await teacher.goto(BOARD);

  // the button is always in the toolbar; the form opens only on a click
  const open = teacher.locator('.class-toolbar .feedback-open');
  await expect(open).toBeVisible();
  const form = teacher.locator('form.feedback-form');
  await expect(form).toBeHidden();
  await open.click();
  await expect(form).toBeVisible();
  await expect(form).toContainText('Rückmeldung zu »');
  await expect(form).toContainText('Angesetzt: 2 Doppelstunden');

  // nothing answered: not sent (an earlier run's answers are cleared first)
  await form.evaluate((f: HTMLFormElement) => {
    f.querySelectorAll('input[type=radio], input[type=checkbox]').forEach((i) => { (i as HTMLInputElement).checked = false; (i as HTMLInputElement).dataset.was = '0'; });
    f.querySelectorAll('input[type=text], textarea').forEach((i) => { (i as HTMLInputElement).value = ''; });
  });
  await form.locator("button[type='submit']").click();
  await expect(form.locator('.feedback-form-error')).toBeVisible();

  // a click on the chosen answer takes it back
  await form.getByText('teils, teils', { exact: true }).click();
  await expect(form.locator('#feedback-overall-mixed')).toBeChecked();
  await form.getByText('teils, teils', { exact: true }).click();
  await expect(form.locator('#feedback-overall-mixed')).not.toBeChecked();

  await form.getByText('gut', { exact: true }).click();
  await form.getByText('länger', { exact: true }).click();
  await form.getByText('Python', { exact: true }).click();
  await form.locator("textarea[name='text']").fill(marker);
  await form.locator("input[name='contact']").check();
  await keep(teacher, 'feedback-form-desktop', 'form.feedback-form');
  const saved = teacher.waitForResponse((r) => r.url().endsWith('/api/activity_feedback/save'));
  await form.locator("button[type='submit']").click();
  expect((await saved).status()).toBe(200);
  await expect(teacher.locator('.api_messages')).toContainText('Danke für deine Rückmeldung');
  await expect(form).toBeHidden();
  await expect(open).toContainText('Deine Rückmeldung');

  // after a reload: the same answers, to change; no second feedback
  await teacher.goto(BOARD);
  await open.click();
  await expect(form.locator('#feedback-overall-good')).toBeChecked();
  await expect(form.locator('#feedback-language-python')).toBeChecked();
  await expect(form.locator("textarea[name='text']")).toHaveValue(marker);
  await expect(form).toContainText('Gespeichert am');
  await form.getByText('zu schwer', { exact: true }).click();
  const changed = teacher.waitForResponse((r) => r.url().endsWith('/api/activity_feedback/save'));
  await form.locator("button[type='submit']").click();
  expect((await changed).status()).toBe(200);

  if (shots) {
    await teacher.setViewportSize({ width: 390, height: 844 });
    await teacher.goto(BOARD);
    await open.click();
    await keep(teacher, 'feedback-form-phone', 'form.feedback-form');
  }

  // the administration: counted, with the text, the address because the box is ticked
  const adminContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const admin = await adminContext.newPage();
  admin.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(admin, adminEmail(), testInfo);
  await admin.goto('/rueckmeldungen');
  await expect(admin.locator('h1')).toHaveText('Rückmeldungen der Lehrkräfte');
  const entry = admin.locator('.fb-entry', { hasText: marker });
  await expect(entry).toHaveCount(1);
  await expect(entry).toContainText('E2E');
  await expect(entry).toContainText('Niveau: zu schwer');
  await expect(entry).toContainText('Dauer: länger');
  // the overview card of the activity, with the planned duration
  await expect(admin.locator('.fb-card', { hasText: 'Kryptographie' })).toContainText('angesetzt: 2 Doppelstunden');
  // a filter: only answers with a text; this one has one
  await admin.locator("label[for='fb-only-text']").click();
  await expect(entry).toBeVisible();
  await admin.locator('.fb-search').fill('gibt es nicht');
  await expect(admin.locator('.fb-none-found')).toBeVisible();
  await admin.locator('.fb-search').fill('');
  await expect(entry.locator(`a[href='mailto:${TEACHER}']`)).toHaveCount(1);
  await keep(admin, 'feedback-admin-desktop', '.fb-page');
  if (shots) {
    await admin.setViewportSize({ width: 390, height: 844 });
    await admin.reload();
    await keep(admin, 'feedback-admin-phone', '.fb-page');
  }
  // the menu links the page
  await admin.goto('/');
  await admin.getByRole('button', { name: 'Administration' }).click();
  await expect(admin.locator(".dropdown-menu a[href='/rueckmeldungen']")).toBeVisible();

  // a student: neither the page nor the API
  const studentContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const student = await studentContext.newPage();
  await loginAsE2eUser(student, e2eEmail, testInfo);
  const page = await student.goto('/rueckmeldungen');
  expect(page?.status()).toBe(403);
  const api = await student.request.post('/api/activity_feedback/save', {
    data: { group: 'E2E', activity: 'crypto', answers: { overall: 'bad' } },
  });
  expect(api.status()).toBe(403);
  const dismiss = await student.request.post('/api/activity_feedback/dismiss', {
    data: { group: 'E2E', activity: 'crypto', mode: 'never' },
  });
  expect(dismiss.status()).toBe(403);
  // and the class page offers nothing of it to a student
  await student.goto('/crypto');
  await expect(student.locator('.feedback-open, .feedback-form')).toHaveCount(0);

  await teacherContext.close();
  await adminContext.close();
  await studentContext.close();
  expect(errors).toEqual([]);
});
