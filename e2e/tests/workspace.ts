import { expect, test } from '@playwright/test';
import type { Page, TestInfo } from '@playwright/test';
import { attachScreenshot } from './reporting';

export const E2E_LOGIN_CODE = '123456';

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
      const codeSection = page.locator('#part1');
      await expect(page.locator('#ti_code')).toBeVisible();
      // jQuery's slideDown makes the controls "visible" as soon as the
      // animation starts. Wait until it stops moving before clicking a button
      // whose hit target is otherwise able to slide away under suite load.
      await expect(codeSection).not.toHaveAttribute('style', /overflow:\s*hidden/);
    });

    await test.step('Enter development login code', async () => {
      const codeInput = page.locator('#ti_code');
      await codeInput.fill(E2E_LOGIN_CODE);
      await codeInput.press('Tab');
      await expect(page.locator('#bu_submit_code')).toBeEnabled();
    });

    await test.step('Submit login', async () => {
      const responsePromise = page.waitForResponse(response =>
        response.url().endsWith('/api/complete_login')
        && response.request().method() === 'POST',
      );
      await page.locator('#bu_submit_code').click();
      const response = await responsePromise;
      expect(response.status()).toBe(200);
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
  email: string,
  testInfo: TestInfo,
) {
  await test.step('Reset workspace state', async () => {
    const postReset = async (confirmation?: string) => page.evaluate(async value => {
      const response = await fetch('/api/reset_server', {
        method: 'POST',
        credentials: 'include',
        headers: {
          'content-type': 'application/json',
        },
        body: JSON.stringify(value === undefined ? {} : {
          confirmation: value,
        }),
      });

      return {
        status: response.status,
        body: await response.text(),
      };
    }, confirmation);

    const missingConfirmation = await postReset();
    expect(missingConfirmation.status).not.toBe(200);

    const wrongConfirmation = await postReset(`wrong-${email}`);
    expect(wrongConfirmation.status).not.toBe(200);

    await page.goto('/profil');
    await page.locator('#bu_reset_workspace').click();

    const modal = page.locator('#__template_modal');
    const input = modal.locator('#ti_reset_workspace_confirmation');
    const confirmButton = modal.getByRole('button', {
      name: 'Workspace endgültig zurücksetzen',
    });

    await expect(modal.getByText(
      'Dieser Vorgang kann nicht rückgängig gemacht werden.',
    )).toBeVisible();
    await expect(modal.locator('code')).toHaveText(email);
    await expect(modal.getByText(
      'Deine Daten in MySQL und Neo4j werden dabei nicht gelöscht.',
    )).toBeVisible();
    await expect(confirmButton).toBeDisabled();

    await input.fill(`wrong-${email}`);
    await expect(confirmButton).toBeDisabled();

    await input.fill(email);
    await expect(confirmButton).toBeEnabled();

    const responsePromise = page.waitForResponse(response =>
      response.url().endsWith('/api/reset_server')
      && response.request().method() === 'POST',
    );
    await confirmButton.click();
    await expect(input).toBeDisabled();
    await expect(confirmButton).toBeDisabled();

    const response = await responsePromise;
    const result = {
      status: response.status(),
      body: await response.text(),
    };

    expect(
      result.status,
      `reset_server returned ${result.status}: ${result.body}`,
    ).toBe(200);

    await expect(modal.locator('#div_reset_workspace_success')).toBeVisible();

    // reset_server changes server_tag/server_sid. Return to the dashboard to
    // receive the updated server cookie before launching the fresh workspace.
    await page.goto('/');
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
