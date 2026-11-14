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
  } finally {
    fs.rmSync(archive, { force: true });
    if (tag) await page.request.post('/api/delete_test', { data: { tag } });
  }
});
