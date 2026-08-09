import { expect, test } from '@playwright/test';
import type { Locator, Page, TestInfo } from '@playwright/test';
import { attachScreenshot } from './reporting';

export type NewFileMethod = 'button' | 'shortcut';

export type CreateTextFileOptions = {
  method?: NewFileMethod;
  screenshotPrefix?: string;
};

export type RunTerminalCommandOptions = {
  timeout?: number;
};

export type TerminalInteraction = {
  waitFor: string | RegExp;
  send: string;
};

function visibleQuickInput(page: Page): Locator {
  return page.locator('.quick-input-widget:visible input').last();
}

function visibleEditor(page: Page): Locator {
  return page.locator('.monaco-editor:visible').last();
}

function untitledTab(page: Page): Locator {
  return page.getByText(/^Untitled-\d+$/).first();
}

function visibleTerminal(page: Page): Locator {
  return page.locator('.terminal-wrapper:visible').last();
}

function safeArtifactName(value: string): string {
  return value.replace(/[^a-zA-Z0-9._-]+/g, '-');
}

function compactForEcho(value: string): string {
  return value.replace(/\s+/g, '');
}

async function waitForUntitledEditor(page: Page) {
  await expect(untitledTab(page)).toBeVisible({
    timeout: 30_000,
  });
  await expect(visibleEditor(page)).toBeVisible({
    timeout: 30_000,
  });
}

/**
 * Click the "New File" action shown by an empty VS Code window.
 *
 * Depending on the exact code-server state this either creates an untitled
 * text file immediately or first opens a file-type picker. Both behaviours are
 * valid; if the picker appears we choose Text File just like the tutorial.
 */
async function openUntitledTextFileFromButton(page: Page) {
  const newFile = page.getByText(/^New File(?:\.\.\.|…)?$/).first();

  await expect(newFile).toBeVisible();
  await newFile.click();

  const picker = page.locator('.quick-input-widget:visible');

  await expect.poll(
    async () => (
      await untitledTab(page).isVisible() ||
      await picker.isVisible()
    ),
    {
      timeout: 30_000,
      message:
        'waiting for VS Code to create an untitled file or show the file-type picker',
    },
  ).toBe(true);

  if (await picker.isVisible()) {
    const textFile = picker
      .locator('.monaco-list-row')
      .filter({ hasText: /Text File/i })
      .first();

    await expect(textFile).toBeVisible();
    await textFile.click();
  }

  await waitForUntitledEditor(page);
}

/**
 * The tutorial uses Ctrl+Alt+N for later examples. This shortcut should create
 * an untitled text editor directly.
 */
async function openUntitledTextFileFromShortcut(page: Page) {
  await page.keyboard.press('Control+Alt+N');
  await waitForUntitledEditor(page);
}

async function saveUntitledFile(
  page: Page,
  filename: string,
) {
  await page.keyboard.press('Control+S');

  const saveInput = visibleQuickInput(page);
  await expect(saveInput).toBeVisible({
    timeout: 30_000,
  });
  await expect(saveInput).toBeFocused();

  // Even with no folder opened, Workspace should suggest /workspace.
  await expect.poll(
    async () => saveInput.inputValue(),
    {
      timeout: 10_000,
      message:
        'waiting for VS Code to suggest /workspace as the save location',
    },
  ).toMatch(/^\/workspace\//);

  const suggestedPath = await saveInput.inputValue();
  expect(
    suggestedPath,
    'fresh text files should be saved inside /workspace',
  ).toMatch(/^\/workspace\//);

  // VS Code selects just the suggested filename. Typing rather than fill()
  // preserves the /workspace/ prefix, matching what a student does.
  await page.keyboard.insertText(filename);

  await expect(saveInput).toHaveValue(`/workspace/${filename}`);
  await saveInput.press('Enter');
  await expect(saveInput).toBeHidden({
    timeout: 30_000,
  });

  await expect(
    page.getByText(filename, { exact: true }).first(),
  ).toBeVisible({
    timeout: 30_000,
  });
}

export async function closeFolder(
  page: Page,
  testInfo: TestInfo,
) {
  await test.step('Close the current folder with Ctrl+K, F', async () => {
    await page.keyboard.press('Control+K');
    await page.keyboard.press('F');

    await expect(
      page.getByText(/NO FOLDER OPENED/i).first(),
    ).toBeVisible({ timeout: 30_000 });
    await attachScreenshot(page, testInfo, 'no-folder-opened');
  }, { box: true });
}

export async function createTextFile(
  page: Page,
  filename: string,
  contents: string,
  testInfo: TestInfo,
  options: CreateTextFileOptions = {},
) {
  const method = options.method ?? 'button';
  const prefix = safeArtifactName(options.screenshotPrefix ?? filename);
  const firstContentLine = contents
    .split('\n')
    .map(line => line.trim())
    .find(Boolean);

  if (!firstContentLine)
    throw new Error(`Cannot create empty tutorial file: ${filename}`);

  await test.step(`Create ${filename}`, async () => {
    await test.step(
      method === 'shortcut'
        ? 'Create a new text file with Ctrl+Alt+N'
        : 'Create a new text file',
      async () => {
        if (method === 'shortcut')
          await openUntitledTextFileFromShortcut(page);
        else
          await openUntitledTextFileFromButton(page);

        await attachScreenshot(page, testInfo, `${prefix}-untitled`);
      },
    );

    await test.step('Enter source code', async () => {
      const editor = visibleEditor(page);
      await expect(editor).toBeVisible();

      await editor.click({
        position: { x: 160, y: 40 },
      });
      await page.keyboard.insertText(contents);

      await expect(editor.locator('.view-lines')).toContainText(
        firstContentLine,
      );
      await attachScreenshot(page, testInfo, `${prefix}-source`);
    });

    await test.step(`Save as ${filename}`, async () => {
      await saveUntitledFile(page, filename);
      await attachScreenshot(page, testInfo, `${prefix}-saved`);
    });
  }, { box: true });
}

export async function activateEditorTab(
  page: Page,
  filename: string,
  testInfo?: TestInfo,
) {
  await test.step(`Activate ${filename}`, async () => {
    const tab = page
      .locator('.tab:visible')
      .filter({ hasText: filename })
      .first();

    await expect(tab).toBeVisible({ timeout: 30_000 });
    await tab.click();
    await expect(visibleEditor(page)).toBeVisible({ timeout: 30_000 });

    if (testInfo)
      await attachScreenshot(
        page,
        testInfo,
        `editor-${safeArtifactName(filename)}-active`,
      );
  });
}

export async function replaceActiveEditorContents(
  page: Page,
  contents: string,
  expectedText: string,
  testInfo: TestInfo,
  screenshotName: string,
) {
  await test.step('Edit and save the current file', async () => {
    const editor = visibleEditor(page);
    await expect(editor).toBeVisible();

    await editor.click({
      position: { x: 160, y: 40 },
    });
    await page.keyboard.press('Control+A');
    await page.keyboard.insertText(contents);

    await expect(editor.locator('.view-lines')).toContainText(expectedText);
    await page.keyboard.press('Control+S');

    // Saving an already-named file should not open a Save As input.
    await expect(visibleQuickInput(page)).toBeHidden();
    await attachScreenshot(page, testInfo, screenshotName);
  }, { box: true });
}

/**
 * Follow the tutorial's Ctrl+P -> "ext install publisher.extension" path.
 * Installation itself is asynchronous; use expectVsCodeExtensionInstalled()
 * after opening the terminal to wait for and verify completion.
 */
export async function installVsCodeExtensionFromQuickOpen(
  page: Page,
  extensionId: string,
  displayName: string,
  testInfo: TestInfo,
) {
  await test.step(`Install the ${displayName} extension`, async () => {
    await page.keyboard.press('Control+P');

    const input = visibleQuickInput(page);
    await expect(input).toBeVisible();
    await input.fill(`ext install ${extensionId}`);
    await input.press('Enter');

    await expect(input).toBeHidden({
      timeout: 30_000,
    });
    await attachScreenshot(
      page,
      testInfo,
      `extension-${safeArtifactName(extensionId)}-requested`,
    );
  }, { box: true });
}

/**
 * Read the integrated terminal when VS Code uses its DOM renderer.
 * Local E2E users are configured with terminal.integrated.gpuAcceleration=off.
 */
export async function terminalText(page: Page): Promise<string> {
  const terminal = visibleTerminal(page);
  const rows = terminal.locator('.xterm-rows > div');

  if (await rows.count() > 0) {
    const contents = await rows.allTextContents();
    return contents.join('\n');
  }

  const rowContainer = terminal.locator('.xterm-rows');
  if (await rowContainer.count() > 0)
    return (await rowContainer.textContent()) ?? '';

  return '';
}

function countShellPrompts(text: string): number {
  // Current Workspace prompts end in "$ " (or "# " for a root shell).
  return (text.match(/[$#][ \u00a0]/g) ?? []).length;
}

async function terminalInput(page: Page): Promise<Locator> {
  const terminal = visibleTerminal(page);
  await expect(terminal).toBeVisible();

  const input = terminal.locator('textarea.xterm-helper-textarea');
  await expect(input).toBeAttached();
  return input;
}

async function waitForTerminalCommandEcho(
  page: Page,
  command: string,
  timeout = 30_000,
) {
  const compactCommand = compactForEcho(command);

  await expect.poll(
    async () => compactForEcho(await terminalText(page)),
    {
      timeout,
      message: `waiting for terminal to show command: ${command}`,
    },
  ).toContain(compactCommand);
}

async function waitForTerminalMatch(
  page: Page,
  expected: string | RegExp,
  timeout = 30_000,
) {
  if (typeof expected === 'string') {
    await expect.poll(
      async () => terminalText(page),
      {
        timeout,
        message: `waiting for terminal output: ${expected}`,
      },
    ).toContain(expected);
  } else {
    await expect.poll(
      async () => terminalText(page),
      {
        timeout,
        message: `waiting for terminal output matching: ${expected}`,
      },
    ).toMatch(expected);
  }
}

export async function openTerminal(
  page: Page,
  testInfo: TestInfo,
) {
  await test.step('Open the integrated terminal with Ctrl+J', async () => {
    let terminal = visibleTerminal(page);

    if (!await terminal.isVisible()) {
      await page.keyboard.press('Control+J');
      terminal = visibleTerminal(page);
    }

    await expect(terminal).toBeVisible({
      timeout: 30_000,
    });
    await terminalInput(page);

    // Fail here if the E2E-only DOM terminal renderer is not active.
    await expect.poll(
      async () => terminalText(page),
      {
        timeout: 15_000,
        message:
          'terminal contains no readable DOM text; E2E users need terminal.integrated.gpuAcceleration="off"',
      },
    ).not.toBe('');

    await attachScreenshot(page, testInfo, 'terminal-open');
  }, { box: true });
}

export async function runTerminalCommand(
  page: Page,
  command: string,
  testInfo: TestInfo,
  screenshotName: string,
  options: RunTerminalCommandOptions = {},
): Promise<string> {
  return await test.step(`Run: ${command}`, async () => {
    const timeout = options.timeout ?? 60_000;
    const before = await terminalText(page);
    const promptsBefore = countShellPrompts(before);
    const input = await terminalInput(page);

    await input.focus();
    await page.keyboard.insertText(command);
    await page.keyboard.press('Enter');

    await waitForTerminalCommandEcho(page, command, Math.min(timeout, 30_000));

    await expect.poll(
      async () => countShellPrompts(await terminalText(page)),
      {
        timeout,
        message: `waiting for command to finish: ${command}`,
      },
    ).toBeGreaterThan(promptsBefore);

    await attachScreenshot(page, testInfo, screenshotName);
    return await terminalText(page);
  }, { box: true });
}

export async function runInteractiveTerminalCommand(
  page: Page,
  command: string,
  interactions: TerminalInteraction[],
  testInfo: TestInfo,
  screenshotName: string,
  options: RunTerminalCommandOptions = {},
): Promise<string> {
  return await test.step(`Run interactively: ${command}`, async () => {
    const timeout = options.timeout ?? 60_000;
    const before = await terminalText(page);
    const promptsBefore = countShellPrompts(before);
    const input = await terminalInput(page);

    await input.focus();
    await page.keyboard.insertText(command);
    await page.keyboard.press('Enter');
    await waitForTerminalCommandEcho(page, command, Math.min(timeout, 30_000));

    for (const interaction of interactions) {
      await waitForTerminalMatch(page, interaction.waitFor, timeout);
      await input.focus();
      await page.keyboard.insertText(interaction.send);
      await page.keyboard.press('Enter');
    }

    await expect.poll(
      async () => countShellPrompts(await terminalText(page)),
      {
        timeout,
        message: `waiting for interactive command to finish: ${command}`,
      },
    ).toBeGreaterThan(promptsBefore);

    await attachScreenshot(page, testInfo, screenshotName);
    return await terminalText(page);
  }, { box: true });
}

export async function expectTerminalText(
  page: Page,
  expected: string | RegExp,
  testInfo: TestInfo,
  screenshotName: string,
  timeout = 30_000,
) {
  const label = typeof expected === 'string' ? expected : expected.toString();

  await test.step(`Verify terminal contains ${label}`, async () => {
    await waitForTerminalMatch(page, expected, timeout);
    await attachScreenshot(page, testInfo, screenshotName);
  }, { box: true });
}

/**
 * Wait until code-server's configured extension directory contains the given
 * extension. The shell loop tolerates the asynchronous Marketplace install
 * triggered by Ctrl+P.
 */
export async function expectVsCodeExtensionInstalled(
  page: Page,
  extensionId: string,
  testInfo: TestInfo,
  screenshotName: string,
) {
  const marker = `__E2E_EXTENSION_INSTALLED_${safeArtifactName(extensionId)}__`;
  const escapedId = extensionId.replace(/'/g, `'"'"'`);
  const command = [
    'for i in $(seq 1 90); do',
    '/app/code-server/bin/code-server --extensions-dir /workspace/.extensions --list-extensions 2>/dev/null',
    `| grep -Fxi '${escapedId}' >/dev/null`,
    `&& { echo '${marker}'; break; };`,
    'sleep 1;',
    'done',
  ].join(' ');

  await runTerminalCommand(
    page,
    command,
    testInfo,
    screenshotName,
    { timeout: 120_000 },
  );
  await expectTerminalText(
    page,
    marker,
    testInfo,
    `${screenshotName}-verified`,
  );
}
