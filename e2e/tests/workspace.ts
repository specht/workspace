import type { Page, TestInfo } from '@playwright/test';
import { E2E_LOGIN_CODE, expect, test } from './fixtures';

async function attachScreenshot(
  page: Page,
  testInfo: TestInfo,
  name: string,
) {
  await testInfo.attach(name, {
    body: await page.screenshot({ fullPage: true }),
    contentType: 'image/png',
  });
}

export async function loginAsE2eUser(
  page: Page,
  email: string,
  testInfo: TestInfo,
) {
  await test.step(`Log in as ${email}`, async () => {
    await test.step('Open login page', async () => {
      await page.goto('/login');
      await expect(page.locator('#ti_email')).toBeVisible();
    });

    await test.step('Enter email address', async () => {
        const emailInput = page.locator('#ti_email');

        await emailInput.fill(email);
        await emailInput.press('Tab');

        await expect(page.locator('#bu_submit_email')).toBeEnabled();
    });

    await test.step('Request login code', async () => {
        await page.locator('#bu_submit_email').click();
        await expect(page.locator('#ti_code')).toBeVisible();
    });

    await test.step('Enter development login code', async () => {
        const codeInput = page.locator('#ti_code');

        await codeInput.fill(E2E_LOGIN_CODE);
        await codeInput.press('Tab');

        await expect(page.locator('#bu_submit_code')).toBeEnabled();
    });

    await test.step('Submit login', async () => {
      await page.locator('#bu_submit_code').click();
      await page.waitForURL(url => url.pathname === '/');
    });

    await test.step('Verify workspace dashboard', async () => {
      await expect(page.locator('#bu_launch')).toBeVisible();
      await attachScreenshot(page, testInfo, '01-logged-in');
    });
  }, { box: true });
}

export async function resetWorkspace(
  page: Page,
  testInfo: TestInfo,
) {
  await test.step('Reset workspace state', async () => {
    const result = await page.evaluate(async () => {
      const response = await fetch('/api/reset_server', {
        method: 'POST',
        credentials: 'include',
        headers: {
          'content-type': 'application/json',
        },
        body: '{}',
      });

      return {
        status: response.status,
        body: await response.text(),
      };
    });

    expect(
      result.status,
      `reset_server returned ${result.status}: ${result.body}`,
    ).toBe(200);

    // reset_server changes server_tag/server_sid. Reload to receive the
    // updated server cookie before launching the fresh workspace.
    await page.reload();
    await expect(page.locator('#bu_launch')).toBeVisible();
    await attachScreenshot(page, testInfo, '02-workspace-reset');
  }, { box: true });
}

export async function launchWorkspace(
  page: Page,
  testInfo: TestInfo,
): Promise<Page> {
  return await test.step('Launch workspace', async () => {
    const popupPromise = page.waitForEvent('popup', {
      timeout: 180_000,
    });

    await page.locator('#bu_launch').click();
    const workspace = await popupPromise;
    await workspace.waitForLoadState('domcontentloaded');
    await attachScreenshot(workspace, testInfo, '03-workspace-opened');
    return workspace;
  }, { box: true });
}

export async function expectVsCodeReady(
  workspace: Page,
  testInfo: TestInfo,
) {
  await test.step('Verify VS Code is ready', async () => {
    await expect(workspace.locator('.monaco-workbench')).toBeVisible({
      timeout: 120_000,
    });
    await attachScreenshot(workspace, testInfo, '04-vscode-ready');
  }, { box: true });
}
