import { test as base, expect } from '@playwright/test';

type WorkerFixtures = {
  e2eEmail: string;
};

export const E2E_LOGIN_CODE = '123456';

export const test = base.extend<{}, WorkerFixtures>({
  e2eEmail: [
    async ({}, use, workerInfo) => {
      // parallelIndex stays stable if Playwright has to restart a worker.
      await use(`e2e-${workerInfo.parallelIndex}@example.com`);
    },
    { scope: 'worker' },
  ],
});

export { expect };
