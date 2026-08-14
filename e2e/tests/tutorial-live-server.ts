import type { BrowserContext, Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { attachScreenshot } from './reporting';
import { WorkspaceContainer } from './workspace-container';
import { expectVsCodeReady } from './workspace';

export const LIVE_SERVER_PORT = 5500;

export function expectCommandSuccess(
  result: {
    stdout: string;
    stderr: string;
    exitCode: number;
  },
  description: string,
) {
  expect(
    result.exitCode,
    `${description} failed\n` +
      `stdout:\n${result.stdout}\n` +
      `stderr:\n${result.stderr}`,
  ).toBe(0);
}

export async function openWorkspaceFolder(
  workspace: Page,
  folder: string,
  testInfo: TestInfo,
) {
  await test.step(`Open ${folder} in VS Code`, async () => {
    const url = new URL(workspace.url());
    url.searchParams.delete('workspace');
    url.searchParams.set('folder', folder);

    await workspace.goto(url.toString(), {
      waitUntil: 'domcontentloaded',
    });
    await expectVsCodeReady(workspace, testInfo);
  }, { box: true });
}

async function runVsCodeCommand(
  workspace: Page,
  query: string,
  expectedText: string,
) {
  await test.step(`Run VS Code command: ${expectedText}`, async () => {
    const workbench = workspace.locator('.monaco-workbench');
    await expect(workbench).toBeVisible();
    await workbench.click({ position: { x: 500, y: 100 } });
    await workspace.keyboard.press('Control+Shift+P');

    const widget = workspace.locator('.quick-input-widget:visible');
    await expect(widget).toBeVisible();

    const input = widget.locator('input').last();
    await input.fill(`>${query}`);

    const row = widget
      .locator('.monaco-list-row')
      .filter({ hasText: expectedText })
      .first();
    await expect(row).toBeVisible({ timeout: 30_000 });
    await row.click();
  }, { box: true });
}

function newlyOpenedPage(
  context: BrowserContext,
  existingPages: Set<Page>,
): Page | undefined {
  return context.pages().find(page => !existingPages.has(page));
}

export async function startLiveServer(
  workspace: Page,
  container: WorkspaceContainer,
  projectDir: string,
  testInfo: TestInfo,
): Promise<Page> {
  return await test.step('Start the project with Live Server', async () => {
    const context = workspace.context();
    const existingPages = new Set(context.pages());
    let openedPage: Page | undefined;
    const rememberPage = (page: Page) => {
      openedPage ??= page;
    };
    context.on('page', rememberPage);

    try {
      await runVsCodeCommand(
        workspace,
        'Live Server: Open with Live Server',
        'Open with Live Server',
      );

      await expect.poll(async () => {
        const result = await container.exec(
          `curl -fsS http://127.0.0.1:${LIVE_SERVER_PORT}/ >/dev/null`,
          { workdir: projectDir },
        );
        return result.exitCode;
      }, {
        timeout: 30_000,
        intervals: [250, 500, 1_000],
        message: `waiting for Live Server on port ${LIVE_SERVER_PORT}`,
      }).toBe(0);

      await expect(
        workspace.locator('.statusbar-item:visible').filter({
          hasText: `Port : ${LIVE_SERVER_PORT}`,
        }).first(),
      ).toBeVisible({ timeout: 30_000 });

      await attachScreenshot(workspace, testInfo, 'live-server-running');
      return openedPage ?? newlyOpenedPage(context, existingPages) ??
        await context.newPage();
    } finally {
      context.off('page', rememberPage);
    }
  }, { box: true });
}

export function liveServerUrl(
  workspace: Page,
  relativePath: string,
): string {
  const url = new URL(workspace.url());
  const path = relativePath.replace(/^\/+/, '');
  url.pathname = `/proxy/${LIVE_SERVER_PORT}/${path}`;
  url.search = '';
  url.hash = '';
  return url.toString();
}

export async function stopLiveServer(
  workspace: Page,
  container: WorkspaceContainer,
  projectDir: string,
) {
  const listening = await container.exec(
    `curl -fsS http://127.0.0.1:${LIVE_SERVER_PORT}/ >/dev/null`,
    { workdir: projectDir },
  );
  if (listening.exitCode !== 0)
    return;

  await runVsCodeCommand(
    workspace,
    'Live Server: Stop Live Server',
    'Stop Live Server',
  );

  await expect.poll(async () => {
    const result = await container.exec(
      `curl -fsS http://127.0.0.1:${LIVE_SERVER_PORT}/ >/dev/null`,
      { workdir: projectDir },
    );
    return result.exitCode;
  }, {
    timeout: 15_000,
    intervals: [250, 500, 1_000],
    message: `waiting for Live Server on port ${LIVE_SERVER_PORT} to stop`,
  }).not.toBe(0);
}
