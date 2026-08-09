import type { Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { WorkspaceContainer } from './workspace-container';
import { expectVsCodeReady } from './workspace';

const PIXELFLOW_EXTENSION =
  'gymnasiumsteglitz.pixelflow-canvas';

const DRAWING_SOURCE = String.raw`require 'pixelflow_canvas'

Pixelflow::Canvas.new(32, 18, :rgb) do
    set_color(16, 32, 64)
    fill_rect(0, 0, 31, 17)

    set_color(255, 0, 0)
    fill_rect(4, 4, 11, 9)

    set_color(0, 255, 0)
    fill_circle(22, 9, 4)

    save_as_png('drawing.png')
end
`;

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
  command: string,
) {
  await test.step(
    `Run VS Code command: ${command}`,
    async () => {
      const workbench = page.locator(
        '.monaco-workbench',
      );

      await expect(workbench).toBeVisible();

      /*
       * Establish workbench focus before opening the command palette.
       * Do not rely on whatever had focus after the previous step.
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

      /*
       * Keep the ">" prefix so Quick Input remains in command-palette
       * mode while the command is resolved.
       */
      await input.fill(`>${command}`);

      const row = widget
        .locator('.monaco-list-row')
        .filter({
          hasText: command,
        })
        .first();

      await expect(row).toBeVisible({
        timeout: 30_000,
      });

      /*
       * Clicking the resolved command is more deterministic than racing
       * an Enter key against Quick Input's asynchronous result list.
       */
      await row.click();
    },
    {
      box: true,
    },
  );
}

function pixelflowOuterSelector() {
  /*
   * The outer VS Code webview iframe identifies its owning extension in
   * its src once the webview has initialized.
   */
  return (
    `iframe.webview` +
    `[src*="${PIXELFLOW_EXTENSION}"]`
  );
}

function pixelflowCanvas(page: Page) {
  /*
   * VS Code webviews are nested:
   *
   *   iframe.webview
   *     iframe#active-frame
   *       Pixelflow extension HTML
   *         #pixelstream_canvas
   */
  return page
    .frameLocator(
      pixelflowOuterSelector(),
    )
    .last()
    .frameLocator(
      'iframe#active-frame' +
      '[title="Pixelflow Canvas"]',
    )
    .locator('#pixelstream_canvas');
}

async function attachWebviewDiagnostics(
  page: Page,
  testInfo: TestInfo,
) {
  const webviews = await page
    .locator('iframe.webview')
    .evaluateAll(elements =>
      elements.map(element => {
        const iframe =
          element as HTMLIFrameElement;

        const rect =
          iframe.getBoundingClientRect();

        return {
          src:
            iframe.getAttribute('src') ??
            '',
          title:
            iframe.getAttribute('title') ??
            '',
          name:
            iframe.getAttribute('name') ??
            '',
          id: iframe.id,
          className: iframe.className,
          visible:
            rect.width > 0 &&
            rect.height > 0,
        };
      }),
    );

  await testInfo.attach(
    'vscode-webviews',
    {
      body: Buffer.from(
        JSON.stringify(
          webviews,
          null,
          2,
        ),
        'utf8',
      ),
      contentType:
        'application/json',
    },
  );
}

async function waitForPixelflowCanvas(
  page: Page,
  testInfo: TestInfo,
) {
  await test.step(
    'Wait for the Pixelflow Canvas webview',
    async () => {
      /*
       * First verify that the extension command actually opened its tab.
       */
      await expect(
        page.locator(
          '[role="tab"]' +
          '[aria-label*="Pixelflow Canvas"]',
        ),
      ).toBeVisible({
        timeout: 30_000,
      });

      const outerElement = page
        .locator(
          pixelflowOuterSelector(),
        )
        .last();

      await expect(
        outerElement,
        `waiting for VS Code webview owned by ${PIXELFLOW_EXTENSION}`,
      ).toBeAttached({
        timeout: 30_000,
      });

      const outer = page
        .frameLocator(
          pixelflowOuterSelector(),
        )
        .last();

      await expect(
        outer.locator(
          'iframe#active-frame' +
          '[title="Pixelflow Canvas"]',
        ),
      ).toBeAttached({
        timeout: 30_000,
      });

      await expect(
        pixelflowCanvas(page),
      ).toBeVisible({
        timeout: 30_000,
      });

      await attachWebviewDiagnostics(
        page,
        testInfo,
      );
    },
    {
      box: true,
    },
  );
}

function checkPng(
  png: Buffer,
  width: number,
  height: number,
) {
  /*
   * PNG signature:
   *
   * 89 50 4e 47 0d 0a 1a 0a
   */
  expect(
    [...png.subarray(0, 8)],
    'drawing.png should have a valid PNG signature',
  ).toEqual([
    0x89,
    0x50,
    0x4e,
    0x47,
    0x0d,
    0x0a,
    0x1a,
    0x0a,
  ]);

  /*
   * In a normal PNG the IHDR chunk immediately follows the signature.
   * Width and height are big-endian uint32 values at bytes 16–23.
   */
  expect(
    png.toString('ascii', 12, 16),
    'first PNG chunk should be IHDR',
  ).toBe('IHDR');

  expect(
    png.readUInt32BE(16),
    'saved PNG width',
  ).toBe(width);

  expect(
    png.readUInt32BE(20),
    'saved PNG height',
  ).toBe(height);

  /*
   * This is intentionally only a smoke check, not image comparison.
   * A non-empty 32x18 PNG should comfortably exceed the bare header.
   */
  expect(
    png.length,
    'drawing.png should contain image data',
  ).toBeGreaterThan(50);
}

test(
  'Pixelflow Canvas renders a Ruby drawing and saves a PNG',
  async ({
    freshWorkspace: workspace,
    e2eEmail,
  }, testInfo) => {
    /*
     * freshWorkspace has already exercised the real application path:
     *
     * login -> reset -> launch -> code-server ready
     *
     * Reuse exactly the container that Workspace launched.
     */
    const container =
      new WorkspaceContainer(
        e2eEmail,
      );

    await container.waitUntilRunning();
    await container.resetSandbox();

    await test.step(
      'Install the Pixelflow Canvas VS Code extension',
      async () => {
        const result =
          await container.exec(
            [
              '/app/code-server/bin/code-server',
              '--extensions-dir /workspace/.extensions',
              `--install-extension ${PIXELFLOW_EXTENSION}`,
              '--force',
              '&&',
              '/app/code-server/bin/code-server',
              '--extensions-dir /workspace/.extensions',
              '--list-extensions',
              '|',
              `grep -Fxi '${PIXELFLOW_EXTENSION}'`,
            ].join(' '),
            {
              timeoutMs: 120_000,
              workdir: '/',
            },
          );

        expectSuccess(
          result,
          'Pixelflow Canvas extension installation',
        );
      },
    );

    await test.step(
      'Install the pixelflow_canvas Ruby gem',
      async () => {
        const result =
          await container.exec(
            [
              'export GEM_HOME="$HOME/.gem"',
              'export GEM_PATH="$GEM_HOME"',
              'gem install pixelflow_canvas --no-document',
            ].join(' && '),
            {
              timeoutMs: 120_000,
              workdir: '/',
            },
          );

        expectSuccess(
          result,
          'pixelflow_canvas gem installation',
        );
      },
    );

    await test.step(
      'Prepare the Ruby drawing program',
      async () => {
        await container.writeFile(
          'drawing.rb',
          DRAWING_SOURCE,
        );
      },
    );

    /*
     * The extension was installed after code-server had already started.
     * Reload the workbench so the extension host discovers it.
     */
    await test.step(
      'Reload VS Code with the installed extension',
      async () => {
        await workspace.reload({
          waitUntil:
            'domcontentloaded',
        });

        await expectVsCodeReady(
          workspace,
          testInfo,
        );
      },
    );

    await runVsCodeCommand(
      workspace,
      'Show Pixelflow Canvas',
    );

    await waitForPixelflowCanvas(
      workspace,
      testInfo,
    );

    await test.step(
      'Run the Ruby drawing program',
      async () => {
        const result =
          await container.exec(
            [
              'export GEM_HOME="$HOME/.gem"',
              'export GEM_PATH="$GEM_HOME"',
              'ruby drawing.rb',
            ].join(' && '),
            {
              timeoutMs: 30_000,
            },
          );

        expectSuccess(
          result,
          'Ruby drawing program',
        );
      },
    );

    await test.step(
      'Verify the live Pixelflow canvas is visible',
      async () => {
        const canvas =
          pixelflowCanvas(workspace);

        await expect(canvas).toBeVisible({
          timeout: 15_000,
        });

        /*
         * This is deliberately a visual diagnostic rather than a
         * pixel-perfect assertion. Canvas getImageData() proved unreliable
         * in the VS Code webview even while the rendered image was visibly
         * correct.
         */
        await testInfo.attach(
          'pixelflow-live-canvas',
          {
            body:
              await canvas.screenshot(),
            contentType:
              'image/png',
          },
        );

        await testInfo.attach(
          'pixelflow-workbench',
          {
            body:
              await workspace.screenshot(),
            contentType:
              'image/png',
          },
        );
      },
    );

    await test.step(
      'Verify drawing.png was saved',
      async () => {
        /*
         * The WorkspaceContainer command runs in /workspace/.e2e, which is
         * also where drawing.rb was written and executed.
         *
         * Transfer the PNG as base64 so the test does not require another
         * host-volume mapping or Docker copy helper.
         */
        const result =
          await container.exec(
            'base64 -w0 drawing.png',
          );

        expectSuccess(
          result,
          'reading drawing.png',
        );

        const encoded =
          result.stdout.trim();

        expect(
          encoded,
          'drawing.png should not be empty',
        ).not.toBe('');

        const png = Buffer.from(
          encoded,
          'base64',
        );

        checkPng(
          png,
          32,
          18,
        );

        /*
         * Playwright's HTML report can display this attachment directly.
         * This gives us a second visual result independent of the VS Code
         * webview screenshot.
         */
        await testInfo.attach(
          'pixelflow-drawing.png',
          {
            body: png,
            contentType:
              'image/png',
          },
        );
      },
    );
  },
);