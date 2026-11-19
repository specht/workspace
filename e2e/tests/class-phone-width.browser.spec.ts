import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import type { APIRequestContext } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The class page on a phone: never wider than the screen. The tabs are one
// row that scrolls sideways; the names of the tabs not chosen are hidden
// (absolute), and once they lay outside that row, so that with a long
// chosen tab (»Leistungsüberprüfungen«) the whole page was wider. The
// chosen tab is scrolled into view, and the notes under an exam's name
// wrap instead of making the first column as wide as their sentence.

const TEACHER = 'e2e-teacher@example.com';

function examPackage(name: string): Buffer {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'class-phone-'));
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

test('the class page on a phone is not wider than the screen', async ({ page }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, TEACHER, testInfo);
  const name = `e2e-phone-${Date.now()}`;
  let tag = '';
  try {
    const upload = await page.request.post('/api/upload_test_archive', {
      multipart: { file: { name: `${name}.tar.gz`, mimeType: 'application/gzip', buffer: examPackage(name) } },
    });
    expect(upload.ok(), await upload.text()).toBe(true);
    tag = (await post(page.request, '/api/get_my_test_archives')).body.entries.find((e: any) => e.filename === `${name}.tar.gz`)?.tag;
    expect(tag).toBeTruthy();
    await page.setViewportSize({ width: 390, height: 844 });
    for (const tab of ['pruefungen', 'einstellungen', 'fortschritt']) {
      await page.goto(`/klasse?group=E2E&tab=${tab}`);
      const selected = page.locator(`#class-tab-${tab}`);
      await expect(selected).toHaveAttribute('aria-selected', 'true');
      expect(await page.evaluate(() => document.documentElement.scrollWidth), tab).toBeLessThanOrEqual(390);
      // the chosen tab lies inside the row
      const [t, bar] = await Promise.all([selected.boundingBox(), page.locator('.class-tabs').boundingBox()]);
      expect(t!.x + t!.width, tab).toBeLessThanOrEqual(bar!.x + bar!.width + 1);
      expect(t!.x, tab).toBeGreaterThanOrEqual(bar!.x - 1);
    }
    await page.goto('/klasse?group=E2E&tab=pruefungen');
    const practice = page.locator('#table_my_test_archives .exam-practice').first();
    await expect(practice).toBeVisible();
    expect(await practice.evaluate((el) => getComputedStyle(el).whiteSpace)).toBe('normal');
    expect((await practice.boundingBox())!.width).toBeLessThanOrEqual(18 * 16 + 1);
    await page.screenshot({ path: testInfo.outputPath('class-exams-phone.png'), fullPage: true });
  } finally {
    if (tag) await post(page.request, '/api/delete_test', { tag });
  }
});
