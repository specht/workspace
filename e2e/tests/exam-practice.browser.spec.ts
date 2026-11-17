import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import type { APIRequestContext } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A mock exam (»Probelauf«, exams_page.rb): while it runs, who takes part
// may go to the normal site and the own Workspace (the button on the exam's
// start page), a yellow bar under the menu leads back, and the own Workspace
// is reachable meanwhile. A real exam has no such way; switching the mock
// exam back to a real one brings everyone back into it.

const TEACHER = 'e2e-teacher@example.com';

function examPackage(name: string): Buffer {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'exam-practice-'));
  try {
    fs.mkdirSync(path.join(dir, '.workspace'));
    fs.writeFileSync(path.join(dir, '.workspace', 'config.yaml'), 'workspace_package: 1\ngit:\n  mode: none\n');
    fs.writeFileSync(path.join(dir, 'AUFGABE.md'), '# Probe\n');
    const archive = path.join(os.tmpdir(), `${name}.tar.gz`);
    execFileSync('tar', ['-czf', archive, '-C', dir, '.']);
    const bytes = fs.readFileSync(archive);
    fs.rmSync(archive);
    return bytes;
  } finally {
    fs.rmSync(dir, { recursive: true, force: true });
  }
}

async function post(api: APIRequestContext, url: string, data: object = {}) {
  const response = await api.post(url, { data });
  return { status: response.status(), body: await response.json().catch(() => ({})) };
}

test('a mock exam: to the normal site and the own Workspace, and back', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(300_000);
  const name = `e2e-probe-${Date.now()}`;
  const teacherContext = await browser.newContext();
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, TEACHER, testInfo);
  await loginAsE2eUser(page, e2eEmail, testInfo);

  let tag = '';
  try {
    await test.step('A real exam runs: no way to the normal site', async () => {
      const upload = await teacher.request.post('/api/upload_test_archive', {
        multipart: { file: { name: `${name}.tar.gz`, mimeType: 'application/gzip', buffer: examPackage(name) } },
      });
      expect(upload.ok(), await upload.text()).toBe(true);
      tag = (await post(teacher.request, '/api/get_my_test_archives')).body.entries.find((e: any) => e.filename === `${name}.tar.gz`)?.tag;
      expect(tag).toBeTruthy();
      await post(teacher.request, '/api/test_set_users', { tag, emails: [e2eEmail], flag: true });
      expect((await post(teacher.request, '/api/start_test', { tag })).status).toBe(200);

      await page.goto('/');
      await expect(page.locator('.bu-launch-test')).toContainText('Leistungsüberprüfung bei');
      await expect(page.locator('.bu-practice-exam-away')).toHaveCount(0);
      await expect(page.locator('.menu').first()).toBeHidden();
      expect((await post(page.request, '/api/practice_exam_away', { tag, away: true })).status).toBe(404);
    });

    await test.step('As a mock exam: away to the normal site, the bar leads back', async () => {
      expect((await post(teacher.request, '/api/test_set_practice', { tag, practice: true })).status).toBe(200);
      const listed = (await post(teacher.request, '/api/get_my_test_archives')).body.entries.find((e: any) => e.tag === tag);
      expect(listed.practice).toBe(true);

      await page.goto('/');
      await expect(page.locator('.bu-launch-test')).toContainText('Probe-Leistungsüberprüfung bei');
      await page.locator('.bu-practice-exam-away').click();
      await expect(page.locator('.look-in-bar')).toContainText('Die Probe-Leistungsüberprüfung');
      await expect(page.locator('.menu').first()).toBeVisible();
      await expect(page.locator('.bu-launch-test')).toHaveCount(0);
      // on every page
      await page.goto('/profil');
      await expect(page.locator('.bu-practice-exam-back')).toBeVisible();
    });

    await test.step('The own Workspace is reachable meanwhile', async () => {
      const started = await post(page.request, '/api/start_server');
      expect(started.status).toBe(200);
      const serverTag = started.body.server_tag;
      expect(serverTag).toBeTruthy();
      expect(serverTag).not.toContain(tag);
      await expect.poll(async () => (await page.request.get(`/w/${serverTag}/`)).status(),
        { timeout: 120_000, intervals: [2_000] }).toBe(200);
    });

    await test.step('Back into the exam by the bar', async () => {
      await page.locator('.bu-practice-exam-back').click();
      await expect(page).toHaveURL(/\/$/);
      await expect(page.locator('.bu-launch-test')).toContainText('Probe-Leistungsüberprüfung bei');
      await expect(page.locator('.menu').first()).toBeHidden();
      await expect(page.locator('.look-in-bar')).toHaveCount(0);
    });

    await test.step('A real exam again: who was away is back in it', async () => {
      expect((await post(page.request, '/api/practice_exam_away', { tag, away: true })).status).toBe(200);
      await page.goto('/');
      await expect(page.locator('.bu-practice-exam-back')).toBeVisible();
      expect((await post(teacher.request, '/api/test_set_practice', { tag, practice: false })).status).toBe(200);
      await page.goto('/');
      await expect(page.locator('.bu-launch-test')).toContainText('Leistungsüberprüfung bei');
      await expect(page.locator('.bu-practice-exam-away')).toHaveCount(0);
      await expect(page.locator('.look-in-bar')).toHaveCount(0);
    });
  } finally {
    if (tag) await post(teacher.request, '/api/delete_test', { tag });
    await teacherContext.close();
  }
});
