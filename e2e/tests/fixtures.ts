import { test as base, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import {
  expectVsCodeReady,
  launchWorkspace,
  loginAsE2eUser,
  resetWorkspace,
} from './workspace';
import { WorkspaceContainer } from './workspace-container';

type TestFixtures = {
  freshWorkspace: Page;

  /*
   * Reuses the real code-server container created by the workspace-smoke
   * project. This fixture does not depend on page/browser.
   */
  workspaceContainer: WorkspaceContainer;
};

type WorkerFixtures = {
  e2eEmail: string;
};

export const test = base.extend<TestFixtures, WorkerFixtures>({
  e2eEmail: [
    async ({}, use, workerInfo) => {
      // parallelIndex stays stable if Playwright has to restart a worker.
      await use(`e2e-${workerInfo.parallelIndex}@example.com`);
    },
    { scope: 'worker' },
  ],

  freshWorkspace: async ({ page, e2eEmail }, use, testInfo) => {
    await loginAsE2eUser(page, e2eEmail, testInfo);
    await resetWorkspace(page, testInfo);
    const workspace = await launchWorkspace(page, testInfo);
    await expectVsCodeReady(workspace, testInfo);

    // Deliberately do not stop the container in teardown. The dependent
    // toolchain project reuses this exact running Workspace.
    await use(workspace);
  },

  workspaceContainer: async ({ e2eEmail }, use) => {
    const container = new WorkspaceContainer(e2eEmail);

    await container.waitUntilRunning();

    // Every command-line test gets a clean build area while the actual
    // Workspace/code-server state remains intact.
    await container.resetSandbox();

    await use(container);
  },
});

export { expect };
