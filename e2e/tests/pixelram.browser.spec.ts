import type { ConsoleMessage, Page } from '@playwright/test';
import { expect, test } from './fixtures';
import { readTutorialFile } from './tutorial';
import {
  expectCommandSuccess,
  liveServerUrl,
  openWorkspaceFolder,
  startLiveServer,
  stopLiveServer,
} from './tutorial-live-server';
import { WorkspaceContainer } from './workspace-container';

const TUTORIAL_REPOSITORY =
  'https://github.com/specht/pixelram-starter.git';
const PROJECT_DIR = '/workspace/pixelram-starter';

function firstCBlockContaining(tutorial: string, text: string): string {
  const block = [...tutorial.matchAll(/^```c\n([\s\S]*?)^```\s*$/gm)]
    .map(match => match[1].trimEnd())
    .find(source => source.includes(text));

  expect(
    block,
    `expected a C tutorial block containing ${JSON.stringify(text)}`,
  ).toBeDefined();
  return block!;
}

test('a student can build and run the PixelRAM starter program', async ({
  freshWorkspace: workspace,
  e2eEmail,
}, testInfo) => {
  test.setTimeout(360_000);

  const container = new WorkspaceContainer(e2eEmail);
  const tutorial = readTutorialFile('pixelram', 'pixelram.md');
  const tutorialStarter = firstCBlockContaining(
    tutorial,
    'set_pixel(160, 90, 10);',
  );
  let app: Page | undefined;

  await container.waitUntilRunning();

  try {
    await test.step('Clone the PixelRAM starter from the tutorial', async () => {
      const clone = await container.exec(
        `git clone --depth 1 ${TUTORIAL_REPOSITORY} ${PROJECT_DIR}`,
        { workdir: '/workspace', timeoutMs: 90_000 },
      );
      expectCommandSuccess(clone, 'PixelRAM starter clone');

      const source = await container.exec('cat main.c', {
        workdir: PROJECT_DIR,
      });
      expectCommandSuccess(source, 'PixelRAM starter source read');
      expect(source.stdout.trimEnd()).toBe(tutorialStarter);
    });

    await openWorkspaceFolder(workspace, PROJECT_DIR, testInfo);

    await test.step('Build the tutorial program with make', async () => {
      const build = await container.exec('make', {
        workdir: PROJECT_DIR,
        timeoutMs: 180_000,
      });
      expectCommandSuccess(build, 'PixelRAM make');

      const output = await container.exec(
        "test -s main.html && grep -Fq 'PixelRAM' main.html",
        { workdir: PROJECT_DIR },
      );
      expectCommandSuccess(output, 'PixelRAM generated page check');
    });

    app = await startLiveServer(
      workspace,
      container,
      PROJECT_DIR,
      testInfo,
    );

    const pageErrors: string[] = [];
    const consoleErrors: string[] = [];
    app.on('pageerror', error => pageErrors.push(error.message));
    app.on('console', (message: ConsoleMessage) => {
      if (message.type() === 'error')
        consoleErrors.push(message.text());
    });

    await test.step('Open the generated PixelRAM application', async () => {
      const response = await app!.goto(
        liveServerUrl(workspace, 'main.html'),
        { waitUntil: 'domcontentloaded' },
      );
      expect(
        response?.ok(),
        `PixelRAM returned HTTP ${response?.status()}`,
      ).toBe(true);
      await expect(app!.locator('#canvas')).toBeVisible();
      await expect(app!).toHaveTitle('PixelRAM');
    });

    await test.step('Verify the C program rendered its green pixel', async () => {
      await expect.poll(async () => app!.evaluate(() => {
        const pixelramWindow = window as typeof window & {
          PIXELRAM_FRAME_CANVAS?: HTMLCanvasElement;
        };
        const frame = pixelramWindow.PIXELRAM_FRAME_CANVAS;
        const context = frame?.getContext('2d');
        if (!frame || !context)
          return null;

        const center = [...context.getImageData(160, 90, 1, 1).data];
        const corner = [...context.getImageData(0, 0, 1, 1).data];
        return {
          width: frame.width,
          height: frame.height,
          centerIsGreen:
            center[1] > center[0] + 80 &&
            center[1] > center[2] + 80,
          cornerIsBlack:
            corner[0] < 10 && corner[1] < 10 && corner[2] < 10,
          centerDiffersFromBackground:
            center.slice(0, 3).some((value, index) => value !== corner[index]),
        };
      }), {
        timeout: 30_000,
        intervals: [100, 250, 500],
        message: 'waiting for the PixelRAM framebuffer to contain the tutorial pixel',
      }).toEqual({
        width: 320,
        height: 180,
        centerIsGreen: true,
        cornerIsBlack: true,
        centerDiffersFromBackground: true,
      });
    });

    await expect(app.locator('#log-output .stderr')).toHaveCount(0);
    expect(pageErrors, 'PixelRAM page errors').toEqual([]);
    expect(consoleErrors, 'PixelRAM console errors').toEqual([]);
  } finally {
    if (app && !app.isClosed())
      await app.close();

    await stopLiveServer(workspace, container, PROJECT_DIR);

    const cleanup = await container.exec(
      'rm -rf /workspace/pixelram-starter',
      { workdir: '/workspace' },
    );
    expectCommandSuccess(cleanup, 'PixelRAM tutorial cleanup');
  }
});
