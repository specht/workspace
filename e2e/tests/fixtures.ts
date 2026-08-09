import { test as base, expect } from '@playwright/test';
import type { Page } from '@playwright/test';
import {
  expectVsCodeReady,
  launchWorkspace,
  loginAsE2eUser,
  resetWorkspace,
} from './workspace';

type TestFixtures = {
  freshWorkspace: Page;
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

    await use(workspace);
  },
});

export { expect };
