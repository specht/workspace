import type { Page } from '@playwright/test';
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

const TUTORIAL_REPOSITORY = 'https://github.com/specht/bif.git';
const PROJECT_DIR = '/workspace/bif';

function shortestFencedBlockContaining(
  tutorial: string,
  texts: string[],
): string {
  const blocks = [...tutorial.matchAll(
    /^```[^\n]*\n([\s\S]*?)^```\s*$/gm,
  )]
    .map(match => match[1].trimEnd())
    .filter(block => texts.every(text => block.includes(text)))
    .sort((left, right) => left.length - right.length);

  expect(
    blocks,
    `expected a tutorial code block containing ${JSON.stringify(texts)}`,
  ).not.toHaveLength(0);
  return blocks[0];
}

test('a student can author and play the BIF starter story', async ({
  freshWorkspace: workspace,
  e2eEmail,
}, testInfo) => {
  test.setTimeout(360_000);

  const container = new WorkspaceContainer(e2eEmail);
  const tutorial = readTutorialFile('bif', 'bif.md');
  const firstPage = shortestFencedBlockContaining(
    tutorial,
    [
      '# Nach Schulschluss',
      '- [Gehe in den Flur.](2)',
    ],
  );
  const secondPage = shortestFencedBlockContaining(
    tutorial,
    ['Du stehst in einem leeren Flur.'],
  );
  let app: Page | undefined;

  await container.waitUntilRunning();

  try {
    await test.step('Clone and edit the tutorial BIF project', async () => {
      const clone = await container.exec(
        `git clone --depth 1 ${TUTORIAL_REPOSITORY} ${PROJECT_DIR}`,
        { workdir: '/workspace', timeoutMs: 90_000 },
      );
      expectCommandSuccess(clone, 'BIF tutorial clone');

      const starter = await container.exec('cat pages-starter/1.md', {
        workdir: PROJECT_DIR,
      });
      expectCommandSuccess(starter, 'BIF starter page read');
      expect(starter.stdout).toContain('# Nach Schulschluss');

      const writeFirstPage = await container.exec(
        'cat > pages-starter/1.md',
        { workdir: PROJECT_DIR, input: `${firstPage}\n` },
      );
      expectCommandSuccess(writeFirstPage, 'BIF first tutorial page write');

      const writeSecondPage = await container.exec(
        'cat > pages-starter/2.md',
        { workdir: PROJECT_DIR, input: `${secondPage}\n` },
      );
      expectCommandSuccess(writeSecondPage, 'BIF second tutorial page write');
    });

    await openWorkspaceFolder(workspace, PROJECT_DIR, testInfo);

    await test.step('Wait for the BIF authoring extension', async () => {
      await expect.poll(async () => {
        const result = await container.exec(
          [
            'test -s .story-tools/analysis.json',
            'test -d node_modules',
            "pgrep -f '[e]ngine/tools/publish-analysis.js --watch' >/dev/null",
          ].join(' && '),
          { workdir: PROJECT_DIR },
        );
        return result.exitCode;
      }, {
        timeout: 120_000,
        intervals: [500, 1_000, 2_000],
        message: 'waiting for BIF npm setup and story analysis',
      }).toBe(0);
    });

    app = await startLiveServer(
      workspace,
      container,
      PROJECT_DIR,
      testInfo,
    );

    const pageErrors: string[] = [];
    app.on('pageerror', error => pageErrors.push(error.message));

    await test.step('Open the generated BIF player', async () => {
      const response = await app!.goto(
        liveServerUrl(workspace, 'index.html'),
        { waitUntil: 'domcontentloaded' },
      );
      expect(response?.ok(), `BIF returned HTTP ${response?.status()}`).toBe(true);

      await expect(
        app!.getByRole('heading', { name: 'Nach Schulschluss' }),
      ).toBeVisible();
      await expect(app!.locator('#story-passage-1')).toContainText(
        'Deine Projektmappe liegt noch im Materialschrank.',
      );

      // The visible graph proves that the authoring extension's analysis is
      // being consumed by the development player, not just that Markdown loads.
      await expect(app!.locator('#graph-container svg')).toBeVisible({
        timeout: 30_000,
      });
      await expect(
        app!.locator('#graph-container').getByText('1', { exact: true }),
      ).toBeVisible();
      await expect(
        app!.locator('#graph-container').getByText('2', { exact: true }),
      ).toBeVisible();
    });

    await test.step('Choose the tutorial path to page 2', async () => {
      await app!.getByRole('link', { name: 'Gehe in den Flur.' }).click();
      await expect(app!.locator('.story-passage[data-page-id="2"]')).toContainText(
        'Du stehst in einem leeren Flur.',
      );
    });

    expect(pageErrors, 'BIF player page errors').toEqual([]);
  } finally {
    if (app && !app.isClosed())
      await app.close();

    await stopLiveServer(workspace, container, PROJECT_DIR);

    const stopWatcher = await container.exec(
      "pkill -TERM -f '[e]ngine/tools/publish-analysis.js --watch' || true",
      { workdir: '/workspace' },
    );
    expectCommandSuccess(stopWatcher, 'BIF authoring watcher stop');

    await expect.poll(async () => {
      const result = await container.exec(
        "pgrep -f '[e]ngine/tools/publish-analysis.js --watch'",
        { workdir: '/workspace' },
      );
      return result.exitCode;
    }, {
      timeout: 15_000,
      intervals: [250, 500, 1_000],
      message: 'waiting for the BIF authoring watcher to stop',
    }).not.toBe(0);

    const cleanup = await container.exec('rm -rf /workspace/bif', {
      workdir: '/workspace',
    });
    expectCommandSuccess(cleanup, 'BIF tutorial cleanup');
  }
});
