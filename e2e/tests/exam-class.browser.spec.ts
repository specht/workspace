import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Leistungsüberprüfungen on the class page (exams.js, class_exams_html): a
// package uploaded in the tab belongs to the class, and its students take
// part by themselves (not the teacher, not test accounts); the dialog of who
// takes part opens with the class, where one is taken out and back in. The
// exam is deleted at the end.

const TEACHER = 'e2e-teacher@example.com';

function examPackage(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'exam-class-'));
  fs.mkdirSync(path.join(dir, '.workspace'));
  fs.writeFileSync(path.join(dir, '.workspace', 'config.yaml'), 'workspace_package: 1\ngit:\n  mode: none\n');
  fs.writeFileSync(path.join(dir, 'AUFGABE.md'), '# Klasse\n');
  const archive = path.join(os.tmpdir(), `e2e-klasse-${Date.now()}.tar.gz`);
  execFileSync('tar', ['-czf', archive, '-C', dir, '.']);
  fs.rmSync(dir, { recursive: true, force: true });
  return archive;
}

test('an exam uploaded for a class: its students take part, one taken out and back', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, TEACHER, testInfo);
  await page.goto('/klasse?tab=pruefungen');
  const group = await page.locator('select.class-group-choice').inputValue();
  const members = await page.locator('#class-panel-lernende tr[data-email]').evaluateAll(
    (rows) => rows.map((r) => r.getAttribute('data-email')!));
  test.skip(members.length < 2, 'the class needs two students');
  // the hint: try it with »Ausprobieren«
  await expect(page.locator('.class-exams-tip')).toContainText('Mit Ausprobieren unter Vorschau');

  const archive = examPackage();
  const name = path.basename(archive);
  let tag = '';
  try {
    const chooser = page.waitForEvent('filechooser');
    await page.locator('#bu_upload_test_archive').click();
    await (await chooser).setFiles(archive);
    await expect(page.locator('#test_archive_upload_status')).toContainText('hochgeladen', { timeout: 60_000 });
    const listed = (await (await page.request.post('/api/get_my_test_archives', { data: {} })).json()).entries;
    const entry = listed.find((e: any) => e.filename === name);
    tag = entry.tag;
    expect(entry.group).toBe(group);
    const taking = (await (await page.request.post('/api/get_assigned_users_for_test', { data: { tag } })).json()).emails;
    expect(taking.length).toBeGreaterThan(0);
    expect(taking).not.toContain(TEACHER);
    expect(taking.every((e: string) => members.includes(e))).toBe(true);
    await expect(page.locator('#test_archive_upload_status')).toContainText(`${taking.length === 1 ? 'eine Person nimmt' : `${taking.length} Personen nehmen`} teil`);

    // the dialog: the class open and first, everyone in it chosen
    const row = page.locator('#table_my_test_archives tr', { hasText: name.replace(/\.tar\.gz$/, '') });
    await row.getByRole('button', { name: /Teilnehmende von/ }).click();
    const first = page.locator('#test_modal .exam-people-group').first();
    await expect(first).toHaveAttribute('open', '');
    await expect(first.locator('.exam-people-group-name')).toHaveText(group);
    const box = first.locator(`input[data-email="${taking[0]}"]`);
    await expect(box).toBeChecked();
    await box.uncheck();
    await expect(page.locator('.exam-people-count')).toHaveText(`${taking.length - 1} ausgewählt`);
    await box.check();
    await expect(page.locator('.exam-people-count')).toHaveText(`${taking.length} ausgewählt`);
    await page.locator('#test_modal').getByRole('button', { name: 'Fertig' }).click();

    // »Ausprobieren«: the start page as the class sees it, the menu hidden,
    // a bar back; the button carries on in the preview, »Von vorn
    // beginnen« starts it again; spinning while the Workspace boots
    const asked: string[] = [];
    await page.route('**/api/start_test_demo', async (route) => {
      asked.push(route.request().postDataJSON().keep);
      await route.fulfill({ json: { success: true, status: 'started', server_tag: 'e2etryboot' } });
    });
    await page.route('**/w/e2etryboot/', (route) => route.fulfill({ status: 502, body: '' }));
    await row.getByRole('link', { name: /ausprobieren$/ }).click();
    await expect(page).toHaveURL(/\/pruefung-ansehen\?tag=/);
    await expect(page.locator('.look-in-bar')).toContainText('Du siehst die Startseite wie die Klasse');
    await expect(page.locator('.menu').first()).toBeHidden();
    const launch = page.locator('.bu-launch-test-demo');
    await expect(launch).toContainText('Leistungsüberprüfung bei');
    await launch.click();
    await expect.poll(() => asked).toEqual(['yes']);
    await page.waitForResponse('**/w/e2etryboot/');
    await expect(launch.locator('.bi')).toHaveClass(/bi-spin/);
    await page.reload();
    await page.locator('#bu_exam_try_fresh').click();
    await expect.poll(() => asked).toEqual(['yes', 'no']);
    await page.unroute('**/api/start_test_demo');
    await page.getByRole('link', { name: 'Zurück zur Klasse' }).click();
    await expect(page).toHaveURL(/\/klasse\?group=.*tab=pruefungen/);

    // renamed: the new name in the row, the file name stays
    const renamed = `Probe ${Date.now()}`;
    await page.locator('#table_my_test_archives tr', { hasText: name.replace(/\.tar\.gz$/, '') })
      .getByRole('button', { name: /umbenennen$/ }).click();
    await page.locator('#ti_input_dialog').fill(renamed);
    await page.locator('#ti_input_dialog').press('Enter');
    await expect(page.locator('#table_my_test_archives tr', { hasText: renamed })).toHaveCount(1);
    const after = (await (await page.request.post('/api/get_my_test_archives', { data: {} })).json()).entries
      .find((e: any) => e.tag === tag);
    expect(after.title).toBe(renamed);
    expect(after.filename).toBe(name);
  } finally {
    fs.rmSync(archive, { force: true });
    if (tag) await page.request.post('/api/delete_test', { data: { tag } });
  }
});

// An exam uploaded in the profile before exams belonged to a class: shown
// only in the classes its participants are in, which it names.
test('an exam without a class shows in the class of its participants', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, TEACHER, testInfo);
  await page.goto('/klasse?tab=pruefungen');
  const group = await page.locator('select.class-group-choice').inputValue();
  const members = await page.locator('#class-panel-lernende tr[data-email]').evaluateAll(
    (rows) => rows.map((r) => r.getAttribute('data-email')!));
  test.skip(members.length < 1, 'the class needs a student');
  const legacy = (filename: string, emails: string[]) => ({
    tag: filename, running: false, size: '1 kB', filename, ts: 0, count: emails.length, emails,
  });
  await page.route('**/api/get_my_test_archives', (route) => route.fulfill({
    json: { success: true, entries: [legacy('hier.tar.gz', [members[0]]), legacy('woanders.tar.gz', ['nobody@example.com'])] },
  }));
  await page.reload();
  const rows = page.locator('#table_my_test_archives tbody tr');
  await expect(rows).toHaveCount(1);
  await expect(rows.first()).toContainText('hier');
  await expect(rows.first().locator('.exam-unassigned')).toHaveText(`Noch keiner Klasse zugeordnet. Teilnehmende aus: ${group} (1)`);
  await expect(rows.first().locator('.exam-assign')).toHaveText('Dieser Klasse zuordnen');
});
