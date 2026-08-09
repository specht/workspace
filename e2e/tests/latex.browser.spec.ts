import type { Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { WorkspaceContainer } from './workspace-container';
import { expectVsCodeReady } from './workspace';

const LATEX_WORKSHOP_EXTENSION =
  'James-Yu.latex-workshop';

const TUTORIAL_REPOSITORY =
  'https://github.com/specht/latex-tutorial.git';

const TUTORIAL_DIR =
  '/workspace/latex-tutorial';

function expectSuccess(
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

async function runVsCodeCommand(
  page: Page,
  query: string,
  expectedText = query,
) {
  await test.step(
    `Run VS Code command: ${expectedText}`,
    async () => {
      const workbench = page.locator(
        '.monaco-workbench',
      );

      await expect(workbench).toBeVisible();

      /*
       * Establish workbench focus before opening the command palette.
       * This avoids depending on whichever control happened to have focus.
       */
      await workbench.click({
        position: {
          x: 500,
          y: 100,
        },
      });

      await page.keyboard.press(
        'Control+Shift+P',
      );

      const widget = page.locator(
        '.quick-input-widget:visible',
      );

      await expect(widget).toBeVisible();

      const input = widget
        .locator('input')
        .last();

      await expect(input).toBeVisible();

      await input.fill(`>${query}`);

      const row = widget
        .locator('.monaco-list-row')
        .filter({
          hasText: expectedText,
        })
        .first();

      await expect(
        row,
        `waiting for VS Code command "${expectedText}"`,
      ).toBeVisible({
        timeout: 30_000,
      });

      /*
       * Clicking the resolved command is less racy than pressing Enter while
       * Quick Input is still updating its result list.
       */
      await row.click();
    },
    {
      box: true,
    },
  );
}

async function openWorkspaceFile(
  page: Page,
  filename: string,
) {
  await test.step(
    `Open ${filename} in VS Code`,
    async () => {
      const workbench = page.locator(
        '.monaco-workbench',
      );

      await expect(workbench).toBeVisible();

      await workbench.click({
        position: {
          x: 500,
          y: 100,
        },
      });

      /*
       * The tutorial repository is already the open VS Code folder at this
       * point, so Quick Open can resolve the file without automating the
       * Explorer tree.
       */
      await page.keyboard.press('Control+P');

      const widget = page.locator(
        '.quick-input-widget:visible',
      );

      await expect(widget).toBeVisible();

      const input = widget
        .locator('input')
        .last();

      await input.fill(filename);

      const row = widget
        .locator('.monaco-list-row')
        .filter({
          hasText: filename,
        })
        .first();

      await expect(
        row,
        `waiting for ${filename} in VS Code Quick Open`,
      ).toBeVisible({
        timeout: 30_000,
      });

      await row.click();

      await expect(
        page
          .locator('[role="tab"]')
          .filter({
            hasText: filename,
          })
          .first(),
      ).toBeVisible({
        timeout: 30_000,
      });
    },
    {
      box: true,
    },
  );
}

async function attachBuildDiagnostics(
  container: WorkspaceContainer,
  workspace: Page,
  testInfo: TestInfo,
) {
  /*
   * These attachments are useful if LaTeX Workshop invoked the wrong engine
   * or if TeX itself reported an error.  Missing files are fine.
   */
  const log = await container.exec(
    [
      'printf "%s\\n" "=== hello.log ==="',
      'cat hello.log 2>/dev/null || true',
      'printf "%s\\n" "=== hello.fdb_latexmk ==="',
      'cat hello.fdb_latexmk 2>/dev/null || true',
    ].join(' && '),
    {
      workdir: TUTORIAL_DIR,
    },
  );

  await testInfo.attach(
    'latex-workshop-build-diagnostics',
    {
      body: Buffer.from(
        log.stdout + log.stderr,
        'utf8',
      ),
      contentType: 'text/plain',
    },
  );

  await testInfo.attach(
    'latex-workshop-workbench',
    {
      body: await workspace.screenshot(),
      contentType: 'image/png',
    },
  );
}

test(
  'LaTeX Workshop builds the tutorial hello.tex document',
  async ({
    freshWorkspace: workspace,
    e2eEmail,
  }, testInfo) => {
    test.setTimeout(300_000);

    const container =
      new WorkspaceContainer(e2eEmail);

    await container.waitUntilRunning();

    try {
      await test.step(
        'Clone the LaTeX tutorial repository',
        async () => {
          const result = await container.exec(
            [
              `rm -rf ${TUTORIAL_DIR}`,
              [
                'git clone --depth 1',
                TUTORIAL_REPOSITORY,
                TUTORIAL_DIR,
              ].join(' '),
            ].join(' && '),
            {
              workdir: '/workspace',
              timeoutMs: 90_000,
            },
          );

          expectSuccess(
            result,
            'LaTeX tutorial clone',
          );

          const hello = await container.exec(
            'test -s hello.tex',
            {
              workdir: TUTORIAL_DIR,
            },
          );

          expectSuccess(
            hello,
            'hello.tex check',
          );
        },
      );

      await test.step(
        'Install LaTeX Workshop into the running Workspace',
        async () => {
          /*
           * Install into the same persistent extension directory used by the
           * running code-server instance.  We reload VS Code afterwards so its
           * extension host discovers the newly installed extension.
           */
          const result = await container.exec(
            [
              [
                '/app/code-server/bin/code-server',
                '--extensions-dir /workspace/.extensions',
                `--install-extension ${LATEX_WORKSHOP_EXTENSION}`,
                '--force',
              ].join(' '),
              [
                '/app/code-server/bin/code-server',
                '--extensions-dir /workspace/.extensions',
                '--list-extensions',
                '|',
                `grep -Fxi '${LATEX_WORKSHOP_EXTENSION}'`,
              ].join(' '),
            ].join(' && '),
            {
              workdir: '/',
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            result,
            'LaTeX Workshop installation',
          );
        },
      );

      await test.step(
        'Open the tutorial repository in VS Code',
        async () => {
          /*
           * code-server gives the folder query parameter precedence when
           * deciding which folder to open.  This reproduces the state after a
           * student clones the repository and answers "Open".
           */
          const url =
            new URL(workspace.url());

          url.searchParams.delete(
            'workspace',
          );

          url.searchParams.set(
            'folder',
            TUTORIAL_DIR,
          );

          await workspace.goto(
            url.toString(),
            {
              waitUntil:
                'domcontentloaded',
            },
          );

          await expectVsCodeReady(
            workspace,
            testInfo,
          );
        },
      );

      await openWorkspaceFile(
        workspace,
        'hello.tex',
      );

      await test.step(
        'Remove any previously generated hello.pdf',
        async () => {
          const result = await container.exec(
            [
              'rm -f',
              'hello.pdf',
              'hello.aux',
              'hello.log',
              'hello.fls',
              'hello.fdb_latexmk',
              'hello.synctex.gz',
            ].join(' '),
            {
              workdir: TUTORIAL_DIR,
            },
          );

          expectSuccess(
            result,
            'cleaning hello.tex build products',
          );
        },
      );

      /*
       * This is the actual extension behavior taught by the tutorial.
       *
       * LaTeX Workshop contributes the command
       * "LaTeX Workshop: Build LaTeX project".
       */
      await runVsCodeCommand(
        workspace,
        'LaTeX Workshop: Build LaTeX project',
        'Build LaTeX project',
      );

      await test.step(
        'Wait for LaTeX Workshop to create hello.pdf',
        async () => {
          await expect.poll(
            async () => {
              const result =
                await container.exec(
                  'test -s hello.pdf',
                  {
                    workdir:
                      TUTORIAL_DIR,
                  },
                );

              return result.exitCode;
            },
            {
              timeout: 90_000,
              intervals: [
                500,
                1_000,
                2_000,
              ],
              message:
                'waiting for LaTeX Workshop to create hello.pdf',
            },
          ).toBe(0);
        },
      );

      await test.step(
        'Verify and attach the generated PDF',
        async () => {
          const result =
            await container.exec(
              [
                'file hello.pdf',
                '&&',
                'base64 -w0 hello.pdf',
              ].join(' '),
              {
                workdir:
                  TUTORIAL_DIR,
              },
            );

          expectSuccess(
            result,
            'reading hello.pdf',
          );

          const newline =
            result.stdout.indexOf('\n');

          expect(
            newline,
            'file output should precede the encoded PDF',
          ).toBeGreaterThanOrEqual(0);

          const description =
            result.stdout
              .slice(0, newline);

          expect(
            description,
          ).toMatch(/PDF document/i);

          const encoded =
            result.stdout
              .slice(newline + 1)
              .trim();

          const pdf = Buffer.from(
            encoded,
            'base64',
          );

          expect(
            pdf.subarray(0, 5).toString(
              'ascii',
            ),
            'hello.pdf should have a PDF header',
          ).toBe('%PDF-');

          expect(
            pdf.length,
            'hello.pdf should contain document data',
          ).toBeGreaterThan(500);

          await testInfo.attach(
            'latex-workshop-hello.pdf',
            {
              body: pdf,
              contentType:
                'application/pdf',
            },
          );

          await testInfo.attach(
            'latex-workshop-success',
            {
              body:
                await workspace.screenshot(),
              contentType:
                'image/png',
            },
          );
        },
      );
    }
    finally {
      await attachBuildDiagnostics(
        container,
        workspace,
        testInfo,
      );
    }
  },
);