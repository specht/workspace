import { expect, test } from './fixtures';
import type { Page } from '@playwright/test';
import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import { loginAsE2eUser } from './workspace';

// TIC-80 keeps its files in the person's Workspace under /workspace/TIC-80
// (docker/tic80/hackschule-persistence.js, src/ruby/include/tic80_storage.rb).
// The browser is the thing under test here: the bridge runs inside TIC-80's
// page. Changes »from elsewhere« (VS Code, git pull) are written straight
// into the folder, as the Workspace would.

function fsTag(email: string): string {
  const hex = crypto.createHash('sha256').update(email).digest('hex');
  return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

function inFolder(email: string, script: string): string {
  const dir = `/user/${fsTag(email)}/workspace/TIC-80`;
  return execFileSync('docker', ['exec', 'workspace_ruby_1', 'sh', '-c', `cd ${dir} && ${script}`]).toString();
}

async function startTic80(page: Page) {
  await page.goto('/tic80/');
  await page.locator('#game-frame').click();
  await page.waitForFunction(() => typeof (window as any).Module?.hsTic80Root === 'string', null, { timeout: 60_000 });
  // The console draws its prompt a moment after the files are there.
  await page.waitForTimeout(2500);
  await page.locator('#canvas').click();
}

// TIC-80 reads the keyboard once per frame; instant typing loses keys.
async function ticType(page: Page, text: string) {
  await page.keyboard.type(text, { delay: 120 });
  await page.waitForTimeout(150);
  await page.keyboard.press('Enter');
}

async function serverFiles(page: Page): Promise<string[]> {
  return await page.evaluate(async () => {
    const tree = await (await fetch('/api/tic80/fs?meta=1', { cache: 'no-store' })).json();
    return tree.files.map((f: { path: string }) => f.path);
  });
}

async function inTic80(page: Page, name: string): Promise<string | null> {
  return await page.evaluate((file) => {
    const w = window as any;
    try {
      return new TextDecoder().decode(w.FS.readFile(`${w.Module.hsTic80Root}/${file}`));
    } catch {
      return null;
    }
  }, name);
}

test('TIC-80 keeps its files in the Workspace, and never overwrites a change from elsewhere', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(240_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  // A clean folder for this test (TIC-80 is not open yet).
  execFileSync('docker', ['exec', 'workspace_ruby_1', 'rm', '-rf', `/user/${fsTag(e2eEmail)}/workspace/TIC-80`]);

  await test.step('a save reaches the Workspace and survives a reload', async () => {
    await startTic80(page);
    await ticType(page, 'new lua');
    await ticType(page, 'save spiel');
    await expect.poll(() => serverFiles(page), { timeout: 15_000 }).toContain('spiel.tic');
    await page.reload();
    await startTic80(page);
    expect(await inTic80(page, 'spiel.tic')).not.toBeNull();
    await ticType(page, 'load spiel');
    await page.waitForTimeout(1000);
  });

  await test.step('a file changed elsewhere does not block saving', async () => {
    inFolder(e2eEmail, 'echo hallo > notiz.txt && chown 1000:1000 notiz.txt');
    await ticType(page, 'save anderes');
    await expect.poll(() => serverFiles(page), { timeout: 15_000 }).toContain('anderes.tic');
    await expect(page.locator('#hs-tic80-storage-status')).toHaveCount(0);
    expect(inFolder(e2eEmail, 'cat notiz.txt').trim()).toBe('hallo');
  });

  await test.step('a clash keeps both versions and says so', async () => {
    // spiel.tic is loaded; it changes elsewhere, then TIC-80 saves a
    // different cart under the same name. (Saving it unchanged sends
    // nothing at all, so the change from elsewhere simply stays.)
    await ticType(page, 'load spiel');
    await page.waitForTimeout(500);
    inFolder(e2eEmail, 'printf VSCODE > spiel.tic && chown 1000:1000 spiel.tic');
    await ticType(page, 'new js');
    await ticType(page, 'save spiel');
    // »The cart already exists. Do you want to overwrite it?«
    await page.waitForTimeout(500);
    await page.keyboard.press('y');
    await expect.poll(() => serverFiles(page), { timeout: 15_000 }).toContain('spiel (TIC-80).tic');
    expect(inFolder(e2eEmail, 'cat spiel.tic')).toBe('VSCODE');
    const status = page.locator('#hs-tic80-storage-status');
    await expect(status).toContainText('spiel.tic wurde außerhalb von TIC-80 geändert');
    await expect(status).toContainText('spiel (TIC-80).tic');
    // TIC-80 now has the Workspace's version at the path, its own next to it.
    expect(await inTic80(page, 'spiel.tic')).toBe('VSCODE');
    expect(await inTic80(page, 'spiel (TIC-80).tic')).not.toBeNull();
    await page.screenshot({ path: testInfo.outputPath('clash.png') });
  });

  await test.step('an edit from elsewhere reaches TIC-80 when the window gets the focus', async () => {
    inFolder(e2eEmail, 'printf "print(1)" > neu.lua && chown 1000:1000 neu.lua');
    await page.evaluate(() => window.dispatchEvent(new Event('focus')));
    await expect.poll(() => inTic80(page, 'neu.lua'), { timeout: 10_000 }).toBe('print(1)');
  });

  execFileSync('docker', ['exec', 'workspace_ruby_1', 'rm', '-rf', `/user/${fsTag(e2eEmail)}/workspace/TIC-80`]);
});
