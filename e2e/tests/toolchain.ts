import { test } from '@playwright/test';
import type { Page, TestInfo } from '@playwright/test';
import { runTerminalCommand } from './vscode';

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'"'"'`)}'`;
}

function safeArtifactName(value: string): string {
  return value.replace(/[^a-zA-Z0-9._-]+/g, '-');
}

/**
 * Put a tutorial fixture into /workspace without exercising Monaco editing.
 * The source remains the real file from src/content; base64 only transports it
 * safely through the terminal regardless of quotes, indentation, or newlines.
 */
export async function writeWorkspaceFile(
  page: Page,
  filename: string,
  contents: string,
  testInfo: TestInfo,
) {
  await test.step(`Prepare /workspace/${filename}`, async () => {
    const encoded = Buffer.from(contents, 'utf8').toString('base64');
    const destination = `/workspace/${filename}`;

    await runTerminalCommand(
      page,
      `printf '%s' ${shellQuote(encoded)} | base64 -d > ${shellQuote(destination)}`,
      testInfo,
      `file-${safeArtifactName(filename)}-prepared`,
    );
  }, { box: true });
}

export type BuildExecutableOptions = {
  source: string;
  executable: string;
  command: string;
  screenshotName?: string;
  timeout?: number;
};

/**
 * Run a compiler and verify the resulting native executable exists, is
 * executable, and is not older than its source. This is language-independent.
 */
export async function buildExecutable(
  page: Page,
  options: BuildExecutableOptions,
  testInfo: TestInfo,
) {
  const source = shellQuote(options.source);
  const executable = shellQuote(options.executable);

  await test.step(`Build ${options.executable}`, async () => {
    await runTerminalCommand(
      page,
      [
        options.command,
        `test -f ${source}`,
        `test -x ${executable}`,
        `! test ${executable} -ot ${source}`,
      ].join(' && '),
      testInfo,
      options.screenshotName ?? `build-${safeArtifactName(options.executable)}`,
      { timeout: options.timeout ?? 30_000 },
    );
  }, { box: true });
}
