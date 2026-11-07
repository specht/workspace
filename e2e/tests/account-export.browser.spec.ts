import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// »Meine Daten herunterladen« (/api/export_my_data, accounts_page.rb): a
// ZIP with the account's data and the Workspace. From 31 October to
// 7 November 2026 the page looked for tools/zip_stream.py in the wrong
// folder and every export came out empty; this catches that.

test('one\'s own data download as a ZIP with the account and its files', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const response = await page.request.get('/api/export_my_data');
  expect(response.status()).toBe(200);
  expect(response.headers()['content-type']).toContain('application/zip');
  const file = path.join(os.tmpdir(), `export-${Date.now()}.zip`);
  fs.writeFileSync(file, await response.body());
  try {
    const names = execFileSync('python3', ['-c', 'import sys, zipfile; print("\\n".join(zipfile.ZipFile(sys.argv[1]).namelist()))', file])
      .toString().split('\n');
    expect(names).toContain('konto.json');
    expect(names).toContain('LIESMICH.txt');
    const account = execFileSync('python3', ['-c', 'import sys, zipfile; sys.stdout.write(zipfile.ZipFile(sys.argv[1]).read("konto.json").decode())', file]).toString();
    expect(account).toContain(e2eEmail);
  } finally {
    fs.rmSync(file, { force: true });
  }
});
