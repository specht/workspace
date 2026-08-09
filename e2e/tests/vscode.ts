import type { Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';

async function attachScreenshot(
  page: Page,
  testInfo: TestInfo,
  name: string,
) {
  await testInfo.attach(name, {
    body: await page.screenshot(),
    contentType: 'image/png',
  });
}

function visibleQuickInput(page: Page) {
  return page.locator('.quick-input-widget:visible input').last();
}

function visibleEditor(page: Page) {
  return page.locator('.monaco-editor:visible').last();
}

function visibleTerminal(page: Page) {
  return page.locator('.terminal-wrapper:visible').last();
}

export async function createTutorialTextFile(
  page: Page,
  filename: string,
  contents: string,
  testInfo: TestInfo,
) {
  await test.step(`Create ${filename}`, async () => {
    await test.step('Click New File and choose Text File', async () => {
      const newFile = page.getByText(/^New File(?:\.\.\.|…)?$/).first();
      await expect(newFile).toBeVisible();
      await newFile.click();

      const textFile = page.getByText('Text File', { exact: true }).first();
      await expect(textFile).toBeVisible();
      await textFile.click();
    });

    await test.step('Enter the Fortran source code', async () => {
      // Monaco keeps its real textarea tiny/hidden. locator.focus() waits for
      // actionability there and can stall. Clicking the visible editor surface
      // focuses Monaco exactly as a user would.
      const editor = visibleEditor(page);
      await expect(editor).toBeVisible();
      await editor.click({ position: { x: 160, y: 40 } });
      await page.keyboard.insertText(contents);

      // Make sure the editor actually received the text before continuing.
      await expect(editor.locator('.view-lines')).toContainText('program HelloWorld');
      await attachScreenshot(page, testInfo, `fortran-${filename}-source`);
    });

    await test.step(`Save as ${filename}`, async () => {
      await page.keyboard.press('Control+S');

      const saveInput = visibleQuickInput(page);
      await expect(saveInput).toBeVisible();
      await saveInput.fill(filename);
      await saveInput.press('Enter');

      // The file name appears in the editor tab once the save has completed.
      await expect(page.getByText(filename, { exact: true }).first()).toBeVisible();
      await attachScreenshot(page, testInfo, `fortran-${filename}-saved`);
    });
  }, { box: true });
}

export async function openTutorialTerminal(
  page: Page,
  testInfo: TestInfo,
) {
  await test.step('Open the integrated terminal with Ctrl+J', async () => {
    await page.keyboard.press('Control+J');

    const terminal = visibleTerminal(page);
    await expect(terminal).toBeVisible({ timeout: 30_000 });
    await expect(terminal.locator('textarea.xterm-helper-textarea')).toBeAttached();
    await attachScreenshot(page, testInfo, 'fortran-terminal-open');
  }, { box: true });
}

export async function runTerminalCommand(
  page: Page,
  command: string,
  testInfo: TestInfo,
  screenshotName: string,
) {
  await test.step(`Run: ${command}`, async () => {
    const terminal = visibleTerminal(page);
    const input = terminal.locator('textarea.xterm-helper-textarea');

    await expect(input).toBeAttached();
    await input.focus();
    await page.keyboard.insertText(command);
    await page.keyboard.press('Enter');

    // Wait until the terminal has at least rendered the command we just sent.
    await expect.poll(async () => terminalText(terminal), {
      timeout: 30_000,
      message: `waiting for terminal to render: ${command}`,
    }).toContain(command);

    await attachScreenshot(page, testInfo, screenshotName);
  }, { box: true });
}

async function terminalText(terminal: ReturnType<Page['locator']>) {
  // With GPU disabled, VS Code uses its DOM terminal renderer. Different
  // xterm.js versions have used both of these containers.
  const rows = terminal.locator('.xterm-rows, .xterm-accessibility-tree');
  if (await rows.count() === 0)
    return '';
  return (await rows.allTextContents()).join('\n');
}

export async function expectTerminalText(
  page: Page,
  expected: string,
  testInfo: TestInfo,
  screenshotName: string,
) {
  await test.step(`Verify terminal contains “${expected}”`, async () => {
    const terminal = visibleTerminal(page);

    await expect.poll(async () => terminalText(terminal), {
      timeout: 30_000,
      message: `waiting for terminal output: ${expected}`,
    }).toContain(expected);

    await attachScreenshot(page, testInfo, screenshotName);
  }, { box: true });
}
