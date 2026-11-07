import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import type { APIRequestContext } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

// All results of a Leistungsüberprüfung as one ZIP (/api/zip_for_test,
// exams_page.rb): a folder per person, named as on the printout, with what
// the person wrote and the package's files, without code-server's hidden
// folders; whoever never opened the exam Workspace is listed in
// nicht-geoeffnet.txt. The dialog »Ergebnisse« links it.

const TEACHER = 'e2e-teacher@example.com';

function examPackage(): Buffer {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'exam-zip-'));
  try {
    fs.mkdirSync(path.join(dir, '.workspace'));
    fs.writeFileSync(path.join(dir, '.workspace', 'config.yaml'), 'workspace_package: 1\ngit:\n  mode: none\n');
    fs.writeFileSync(path.join(dir, 'AUFGABE.md'), '# Summe\n');
    const archive = path.join(os.tmpdir(), `exam-zip-${Date.now()}.tar.gz`);
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

function zipEntries(bytes: Buffer): { names: string[]; read: (name: string) => string } {
  const file = path.join(os.tmpdir(), `results-${Date.now()}.zip`);
  fs.writeFileSync(file, bytes);
  const names = execFileSync('python3', ['-c', 'import sys, zipfile; print("\\n".join(zipfile.ZipFile(sys.argv[1]).namelist()))', file])
    .toString().split('\n').filter((n) => n && !n.endsWith('/'));
  const read = (name: string) => execFileSync('python3', ['-c', 'import sys, zipfile; sys.stdout.write(zipfile.ZipFile(sys.argv[1]).read(sys.argv[2]).decode())', file, name]).toString();
  return { names, read };
}

test('the results of an exam download as one ZIP, a folder per person', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(300_000);
  const absent = e2eEmail === 'e2e-7@example.com' ? 'e2e-6@example.com' : 'e2e-7@example.com';
  const teacherContext = await browser.newContext();
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, TEACHER, testInfo);
  await loginAsE2eUser(page, e2eEmail, testInfo);

  let tag = '';
  try {
    const name = `e2e-ergebnisse-${Date.now()}`;
    const upload = await teacher.request.post('/api/upload_test_archive', {
      multipart: { file: { name: `${name}.tar.gz`, mimeType: 'application/gzip', buffer: examPackage() } },
    });
    expect(upload.ok(), await upload.text()).toBe(true);
    tag = (await post(teacher.request, '/api/get_my_test_archives')).body.entries.find((e: any) => e.filename === `${name}.tar.gz`)?.tag;
    expect(tag).toBeTruthy();
    const chosen = await post(teacher.request, '/api/test_set_users', { tag, emails: [e2eEmail, absent], flag: true });
    expect(chosen.body.emails.sort()).toEqual([e2eEmail, absent].sort());
    expect((await post(teacher.request, '/api/start_test', { tag })).status).toBe(200);

    // the student writes a solution in the exam Workspace
    expect((await post(page.request, '/api/start_server', { test_tag: tag })).status).toBe(200);
    const exam = new WorkspaceContainer(`${e2eEmail}${tag}`);
    await exam.waitUntilRunning(180_000);
    const written = await exam.exec("printf 'int sum(int n) => n * (n + 1) ~/ 2;\\n' > summe.dart && mkdir -p .local/state && echo x > .local/state/x",
      { workdir: '/workspace' });
    expect(written.exitCode, written.stderr).toBe(0);

    // the dialog links the download
    await teacher.goto('/profil');
    const row = teacher.locator('tr', { hasText: name });
    await row.getByRole('button', { name: 'Ergebnisse', exact: true }).click();
    await expect(teacher.locator('#test_results_zip')).toHaveAttribute('href', `/api/zip_for_test/${tag}`);

    const response = await teacher.request.get(`/api/zip_for_test/${tag}`);
    expect(response.status()).toBe(200);
    expect(response.headers()['content-disposition']).toContain(`${name}-ergebnisse.zip`);
    const { names, read } = zipEntries(await response.body());
    const folder = names.find((n) => n.endsWith('/summe.dart'))!.split('/')[0];
    expect(folder).not.toContain('@');
    expect(names).toContain(`${folder}/AUFGABE.md`);
    expect(names).toContain(`${folder}/.workspace/config.yaml`);
    expect(names.some((n) => n.startsWith(`${folder}/.local/`))).toBe(false);
    expect(read(`${folder}/summe.dart`)).toContain('n * (n + 1)');
    expect(read('nicht-geoeffnet.txt')).toContain('E2E Worker');

    // only the exam's own teacher
    expect((await page.request.get(`/api/zip_for_test/${tag}`)).status()).not.toBe(200);
  } finally {
    if (tag) {
      await post(teacher.request, '/api/stop_test', { tag });
      await post(teacher.request, '/api/delete_test', { tag });
    }
    await teacherContext.close();
  }
});
