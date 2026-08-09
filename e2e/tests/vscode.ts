import type { Locator, Page, TestInfo } from '@playwright/test';
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

/**
 * Click "New File".
 *
 * Depending on the exact VS Code/code-server state, this may either:
 *
 *   1. immediately create Untitled-1, or
 *   2. show the "New File..." picker first.
 *
 * Both are valid. If the picker appears, choose Text File.
 */
async function openUntitledTextFile(page: Page) {
    const newFile = page.getByText(/^New File(?:\.\.\.|…)?$/).first();

    await expect(newFile).toBeVisible();
    await newFile.click();

    const picker = page.locator('.quick-input-widget:visible');

    // Wait until VS Code has done one of the two things we know it can do.
    //
    // locator.isVisible() deliberately returns immediately, which makes it
    // useful inside expect.poll().
    await expect.poll(
        async () => {
            return (
                await untitledTab(page).isVisible() ||
                await picker.isVisible()
            );
        },
        {
            timeout: 30_000,
            message:
                'waiting for VS Code to create an untitled file or show the file-type picker',
        },
    ).toBe(true);

    // Some VS Code/code-server versions/states show a type picker.
    if (await picker.isVisible()) {
        const textFile = picker
            .locator('.monaco-list-row')
            .filter({ hasText: /Text File/i })
            .first();

        await expect(textFile).toBeVisible();
        await textFile.click();
    }

    // In either case we must now have a real untitled editor.
    await expect(untitledTab(page)).toBeVisible({
        timeout: 30_000,
    });

    await expect(visibleEditor(page)).toBeVisible({
        timeout: 30_000,
    });
}

export async function createTutorialTextFile(
    page: Page,
    filename: string,
    contents: string,
    testInfo: TestInfo,
) {
    await test.step(`Create ${filename}`, async () => {
        await test.step('Create a new text file', async () => {
            await openUntitledTextFile(page);

            await attachScreenshot(
                page,
                testInfo,
                `fortran-${filename}-untitled`,
            );
        });

        await test.step('Enter the Fortran source code', async () => {
            const editor = visibleEditor(page);

            await expect(editor).toBeVisible();

            // Click inside the visible editor just like a user would.
            await editor.click({
                position: {
                    x: 160,
                    y: 40,
                },
            });

            await page.keyboard.insertText(contents);

            // Don't merely assume that keyboard focus landed in Monaco.
            await expect(
                editor.locator('.view-lines'),
            ).toContainText('program HelloWorld');

            await attachScreenshot(
                page,
                testInfo,
                `fortran-${filename}-source`,
            );
        });

        await test.step(`Save as ${filename}`, async () => {
            await page.keyboard.press('Control+S');

            const saveInput = visibleQuickInput(page);

            await expect(saveInput).toBeVisible({
                timeout: 30_000,
            });

            await expect(saveInput).toBeFocused();

            /*
             * This is an important part of the tutorial contract:
             *
             * even though no folder is opened in VS Code, Save should suggest
             * /workspace as the destination.
             */
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

            /*
             * Do NOT use fill(filename) here.
             *
             * VS Code has selected the suggested filename portion. Typing like
             * a real user replaces that selected filename while preserving
             * "/workspace/".
             */
            await page.keyboard.insertText(filename);

            await expect(saveInput).toHaveValue(
                `/workspace/${filename}`,
            );

            await saveInput.press('Enter');

            await expect(saveInput).toBeHidden({
                timeout: 30_000,
            });

            await expect(
                page.getByText(filename, { exact: true }).first(),
            ).toBeVisible({
                timeout: 30_000,
            });

            await attachScreenshot(
                page,
                testInfo,
                `fortran-${filename}-saved`,
            );
        });
    }, { box: true });
}

/**
 * Read the terminal when VS Code is using its DOM renderer.
 *
 * The E2E user should have:
 *
 *   "terminal.integrated.gpuAcceleration": "off"
 */
async function terminalText(page: Page): Promise<string> {
    const terminal = visibleTerminal(page);

    const rows = terminal.locator('.xterm-rows > div');

    if (await rows.count() > 0) {
        const contents = await rows.allTextContents();
        return contents.join('\n');
    }

    // Fallback for slightly different xterm DOM layouts.
    const rowContainer = terminal.locator('.xterm-rows');

    if (await rowContainer.count() > 0) {
        return (await rowContainer.textContent()) ?? '';
    }

    return '';
}

function countShellPrompts(text: string): number {
    /*
     * Current workspace prompt looks approximately like:
     *
     *   abc@container:~$
     *
     * We count "$ " / "# " occurrences. A returned prompt means the previous
     * command has finished.
     */
    return (
        text.match(/[$#][ \u00a0]/g) ?? []
    ).length;
}

export async function openTutorialTerminal(
    page: Page,
    testInfo: TestInfo,
) {
    await test.step(
        'Open the integrated terminal with Ctrl+J',
        async () => {
            let terminal = visibleTerminal(page);

            if (!await terminal.isVisible()) {
                await page.keyboard.press('Control+J');
                terminal = visibleTerminal(page);
            }

            await expect(terminal).toBeVisible({
                timeout: 30_000,
            });

            const input = terminal.locator(
                'textarea.xterm-helper-textarea',
            );

            await expect(input).toBeAttached({
                timeout: 30_000,
            });

            /*
             * Fail here if the DOM terminal renderer isn't actually active.
             * Otherwise every subsequent output assertion would mysteriously
             * return an empty string.
             */
            await expect.poll(
                async () => terminalText(page),
                {
                    timeout: 15_000,
                    message:
                        'terminal contains no readable DOM text; make sure the E2E user has terminal.integrated.gpuAcceleration = "off"',
                },
            ).not.toBe('');

            await attachScreenshot(
                page,
                testInfo,
                'fortran-terminal-open',
            );
        },
        { box: true },
    );
}

export async function runTerminalCommand(
    page: Page,
    command: string,
    testInfo: TestInfo,
    screenshotName: string,
) {
    await test.step(`Run: ${command}`, async () => {
        const terminal = visibleTerminal(page);

        await expect(terminal).toBeVisible();

        const input = terminal.locator(
            'textarea.xterm-helper-textarea',
        );

        await expect(input).toBeAttached();

        /*
         * Count prompts before running the command. Waiting for the command
         * text itself isn't enough: it appears as soon as we press Enter,
         * before gfortran has necessarily finished.
         */
        const before = await terminalText(page);
        const promptsBefore = countShellPrompts(before);

        await input.focus();
        await page.keyboard.insertText(command);
        await page.keyboard.press('Enter');

        // First make sure the terminal received exactly what we typed.
        await expect.poll(
            async () => terminalText(page),
            {
                timeout: 30_000,
                message:
                    `waiting for terminal to show command: ${command}`,
            },
        ).toContain(command);

        // Then wait until bash displays another prompt: command completed.
        await expect.poll(
            async () => {
                const text = await terminalText(page);
                return countShellPrompts(text);
            },
            {
                timeout: 60_000,
                message:
                    `waiting for command to finish: ${command}`,
            },
        ).toBeGreaterThan(promptsBefore);

        await attachScreenshot(
            page,
            testInfo,
            screenshotName,
        );
    }, { box: true });
}

export async function expectTerminalText(
    page: Page,
    expected: string,
    testInfo: TestInfo,
    screenshotName: string,
) {
    await test.step(
        `Verify terminal contains “${expected}”`,
        async () => {
            await expect.poll(
                async () => terminalText(page),
                {
                    timeout: 30_000,
                    message:
                        `waiting for terminal output: ${expected}`,
                },
            ).toContain(expected);

            await attachScreenshot(
                page,
                testInfo,
                screenshotName,
            );
        },
        { box: true },
    );
}