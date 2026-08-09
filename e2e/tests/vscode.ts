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
  waitFor?: string | RegExp;
};

export type RunInteractiveTerminalCommandOptions = {
  timeout?: number;
  completion?: string | RegExp;
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
 * The tutorial uses Ctrl+Alt+N for later examples. The preceding step may have
 * left focus in the terminal, so explicitly move focus back to the editor
 * before sending a VS Code shortcut.
 */
async function openUntitledTextFileFromShortcut(page: Page) {
  const editor = visibleEditor(page);
  await expect(editor).toBeVisible({ timeout: 30_000 });
  await editor.click({ position: { x: 160, y: 40 } });

  await page.keyboard.press('Control+Alt+N');
  await waitForUntitledEditor(page);
}

/**
 * Insert a block of text into Monaco as a paste, not as a stream of typed
 * characters. Multi-line keyboard.insertText() interacts with Monaco's
 * language auto-indentation: every newline may add indentation and then the
 * indentation already present in the tutorial source is added again.
 *
 * Dispatching a paste event mirrors copying the tutorial's source into VS Code
 * and preserves the source text exactly, independent of the active language's
 * auto-indent rules. This deliberately avoids the system clipboard so the E2E
 * suite also works on plain HTTP local development origins.
 */
async function pasteIntoActiveEditor(
  page: Page,
  contents: string,
) {
  const editor = visibleEditor(page);
  await expect(editor).toBeVisible({ timeout: 30_000 });

  await editor.click({ position: { x: 160, y: 40 } });

  const input = editor.locator('textarea.inputarea').first();
  await expect(input).toBeAttached({ timeout: 30_000 });
  await input.focus();

  await input.evaluate((element, text) => {
    const transfer = new DataTransfer();
    transfer.setData('text/plain', text);

    const event = new ClipboardEvent('paste', {
      bubbles: true,
      cancelable: true,
      clipboardData: transfer,
    });

    element.dispatchEvent(event);
  }, contents);
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

      await pasteIntoActiveEditor(page, contents);

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

    await editor.click({ position: { x: 160, y: 40 } });
    await page.keyboard.press('Control+A');
    await pasteIntoActiveEditor(page, contents);

    await expect(editor.locator('.view-lines')).toContainText(expectedText);
    await page.keyboard.press('Control+S');

    // Saving an already-named file should not open a Save As input.
    await expect(visibleQuickInput(page)).toBeHidden();
    await attachScreenshot(page, testInfo, screenshotName);
  }, { box: true });
}

/**
 * Follow the tutorial's Ctrl+P -> "ext install publisher.extension" path.
 *
 * VS Code 1.97+ asks the user to trust a third-party publisher on the first
 * install. Handle that dialog instead of dismissing it with Escape, then wait
 * for the active editor's language mode to change. This verifies the visible
 * effect the student needs from the extension and avoids shell polling.
 */
export async function installVsCodeExtensionFromQuickOpen(
  page: Page,
  extensionId: string,
  displayName: string,
  testInfo: TestInfo,
  languageName?: string,
) {
  await test.step(`Install the ${displayName} extension`, async () => {
    await page.keyboard.press('Control+P');

    const input = visibleQuickInput(page);
    await expect(input).toBeVisible();
    await input.fill(`ext install ${extensionId}`);

    // Quick Open resolves the typed command asynchronously. Pressing Enter
    // immediately after fill() can arrive before the install action exists and
    // is then ignored. Wait for the actual "Press Enter to install..." row
    // that a student sees, then press Enter.
    const installAction = page
      .locator('.quick-input-widget:visible .monaco-list-row')
      .filter({
        hasText: new RegExp(
          `Press Enter to install extension.*${escapeRegExp(extensionId)}`,
          'i',
        ),
      })
      .first();

    await expect(installAction).toBeVisible({ timeout: 30_000 });
    await input.focus();
    await page.keyboard.press('Enter');

    const trustButton = page
      .getByRole('button', {
        name: /Trust.*(?:Publisher)?.*Install|Install.*Trust/i,
      })
      .first();

    if (languageName) {
      const languageMode = page
        .locator('.statusbar-item:visible')
        .filter({
          hasText: new RegExp(`\\b${escapeRegExp(languageName)}\\b`, 'i'),
        })
        .first();

      // VS Code 1.97+ may stop the first third-party extension installation at
      // a publisher-trust dialog. Keep watching the real UI state: accept the
      // trust prompt when it appears and finish only when the editor visibly
      // reports the requested language mode.
      await expect.poll(
        async () => {
          if (await trustButton.isVisible())
            await trustButton.click();

          return await languageMode.isVisible();
        },
        {
          timeout: 60_000,
          message:
            `waiting for ${displayName} to install and activate ${languageName} language mode`,
        },
      ).toBe(true);
    } else {
      // Generic fallback for extensions where the caller has no visible
      // activation state to assert. Handle a publisher-trust prompt if it
      // appears, but do not invent a filesystem-level verification here.
      try {
        await trustButton.waitFor({ state: 'visible', timeout: 10_000 });
        await trustButton.click();
      } catch {
        // No prompt is normal when the publisher is already trusted.
      }
    }

    // Close any remaining Quick Open only after installation/activation has
    // been dealt with; pressing Escape earlier can cancel the user's flow.
    const quickOpen = page.locator('.quick-input-widget:visible');
    if (await quickOpen.isVisible())
      await page.keyboard.press('Escape');

    await attachScreenshot(
      page,
      testInfo,
      `extension-${safeArtifactName(extensionId)}-installed`,
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

let terminalMarkerSequence = 0;

function escapeRegExp(value: string): string {
  return value.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'"'"'`)}'`;
}

async function terminalInput(page: Page): Promise<Locator> {
  const terminal = visibleTerminal(page);
  await expect(terminal).toBeVisible();

  const input = terminal.locator('textarea.xterm-helper-textarea');
  await expect(input).toBeAttached();
  return input;
}

async function typeTerminalCommand(
  page: Page,
  command: string,
) {
  const input = await terminalInput(page);
  await input.focus();
  await page.keyboard.insertText(command);
  await page.keyboard.press('Enter');
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

    await expect(terminal).toBeVisible({ timeout: 30_000 });
    await terminalInput(page);

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

/**
 * Type a command into the real integrated terminal.
 *
 * If waitFor is supplied, wait for an observable result from that command.
 * Otherwise this deliberately does not try to infer shell completion from the
 * rendered prompt. Build tests should follow this with expectExecutableUpToDate().
 */
export async function runTerminalCommand(
  page: Page,
  command: string,
  testInfo: TestInfo,
  screenshotName: string,
  options: RunTerminalCommandOptions = {},
): Promise<string> {
  return await test.step(`Run: ${command}`, async () => {
    await typeTerminalCommand(page, command);

    if (options.waitFor)
      await waitForTerminalMatch(
        page,
        options.waitFor,
        options.timeout ?? 30_000,
      );

    await attachScreenshot(page, testInfo, screenshotName);
    return await terminalText(page);
  }, { box: true });
}

/**
 * Verify a build by observable filesystem state instead of prompt rendering.
 *
 * The check command is typed immediately after the compiler command. The shell
 * executes it only after the compiler returns, so the unique marker proves the
 * executable exists, is executable, and is not older than its source.
 */
export async function expectExecutableUpToDate(
  page: Page,
  executable: string,
  source: string,
  testInfo: TestInfo,
  screenshotName: string,
  timeout = 30_000,
) {
  await test.step(
    `Verify ${executable} exists and is up to date`,
    async () => {
      const marker = `__E2E_EXECUTABLE_OK_${++terminalMarkerSequence}__`;
      const command = [
        `test -f ${shellQuote(source)}`,
        `test -x ${shellQuote(executable)}`,
        `! test ${shellQuote(executable)} -ot ${shellQuote(source)}`,
        `printf '${marker}\\n'`,
      ].join(' && ');

      await typeTerminalCommand(page, command);
      await waitForTerminalMatch(page, marker, timeout);
      await attachScreenshot(page, testInfo, screenshotName);
    },
    { box: true },
  );
}

export async function runInteractiveTerminalCommand(
  page: Page,
  command: string,
  interactions: TerminalInteraction[],
  testInfo: TestInfo,
  screenshotName: string,
  options: RunInteractiveTerminalCommandOptions = {},
): Promise<string> {
  return await test.step(`Run interactively: ${command}`, async () => {
    const timeout = options.timeout ?? 60_000;
    const input = await terminalInput(page);

    await input.focus();
    await page.keyboard.insertText(command);
    await page.keyboard.press('Enter');

    for (const interaction of interactions) {
      await waitForTerminalMatch(page, interaction.waitFor, timeout);
      await input.focus();
      await page.keyboard.insertText(interaction.send);
      await page.keyboard.press('Enter');
    }

    if (options.completion)
      await waitForTerminalMatch(page, options.completion, timeout);

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
