import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { E2E_LOGIN_CODE } from './workspace';

// »Laufende Workspaces« on /admin: the button »Prozesse« of a running
// Workspace lists what keeps it busy (WorkspaceProcesses), program and file
// of each process, VS Code by name, commands in mono; what the person last
// did above it; »Beenden« ends one program with a reason, the rest keeps
// running. Starts the program with docker exec on the host. Logs in as the first ADMIN_USERS address
// (src/ruby/credentials.rb) or E2E_ADMIN_EMAIL, as access-rules does.

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  return credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)![1];
}

function fsTag(email: string): string {
  const hex = crypto.createHash('sha256').update(email).digest('hex');
  return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

test('an administrator sees what keeps a running Workspace busy', async ({ browser }, testInfo) => {
  test.setTimeout(300_000);
  const base = testInfo.project.use.baseURL as string;
  const signIn = async (email: string) => {
    const context = await browser.newContext({ baseURL: base });
    const requested = await (await context.request.post('/api/request_login', { data: { email } })).json();
    await context.request.post('/api/complete_login', { data: { tag: requested.tag, code: E2E_LOGIN_CODE } });
    await context.request.get('/');
    return context;
  };
  const student = await signIn('e2e-1@example.com');
  const admin = await signIn(adminEmail());
  try {
    const started = await (await student.request.post('/api/start_server', { data: {} })).json();
    const url = `${new URL(base).protocol}//${started.server_tag}.${new URL(base).host}/`;
    await expect.poll(async () => (await student.request.get(url, { maxRedirects: 0 }).catch(() => null))?.status() ?? 0,
      { timeout: 180_000, intervals: [2_000] }).toBeLessThan(400);
    // a program the student left running (as the student, in the background)
    const container = `hs_code_${fsTag('e2e-1@example.com')}`;
    execFileSync('docker', ['exec', '-u', '1000', container, 'sh', '-c', 'printf "import time\\ntime.sleep(987)\\n" > /tmp/e2e_wait.py'], { stdio: 'pipe' });
    execFileSync('docker', ['exec', '-d', '-u', '1000', container, 'python3', '/tmp/e2e_wait.py'], { stdio: 'pipe' });
    const page = await admin.newPage();
    await page.goto('/admin');
    const row = page.locator(`tr[data-tag="${fsTag('e2e-1@example.com')}"]`);
    await row.locator('.bu-workspace-processes').click();
    const modal = page.locator('#__template_modal');
    await expect(modal.locator('.modal-title')).toContainText('Prozesse von');
    await expect(modal.locator('table')).toContainText('VS Code', { timeout: 30_000 });
    await expect(modal.locator('th', { hasText: 'läuft seit' })).toBeVisible();
    await expect(modal.locator('.modal-body ul')).toContainText(/Browser/);
    const sleeping = modal.locator('tr', { hasText: 'e2e_wait.py' });
    await expect(sleeping.locator('td').first()).toHaveClass(/font-monospace/);
    await expect(modal.locator('tr', { hasText: 'VS Code' }).first().locator('.bu-end-process')).toHaveCount(0);
    await expect(modal.locator('.bu-stop-workspace')).toBeVisible();
    await sleeping.locator('.bu-end-process').click();
    await modal.locator('#ti_admin_reason').fill('Ein Test beendet ein Programm.');
    await modal.locator('#cb_admin_notify').uncheck();
    await modal.getByRole('button', { name: 'Beenden' }).click();
    await expect(page.getByText('Das Programm ist beendet.')).toBeVisible();
    await expect(modal.locator('table')).toContainText('VS Code', { timeout: 30_000 });
    await expect(modal.locator('table')).not.toContainText('e2e_wait.py');
    await modal.getByRole('button', { name: 'Neu lesen' }).click();
    await expect(modal.locator('table')).toContainText('VS Code');
  } finally {
    await student.close();
    await admin.close();
  }
});
