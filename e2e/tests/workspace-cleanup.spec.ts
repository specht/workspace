import { test as teardown } from '@playwright/test';
import { WorkspaceContainer } from './workspace-container';

const e2eUserCount = Number.parseInt(
  process.env.E2E_USER_COUNT ?? '8',
  10,
);

if (!Number.isInteger(e2eUserCount) || e2eUserCount < 1)
  throw new Error('E2E_USER_COUNT must be a positive integer');

teardown('stop all E2E workspace containers', async () => {
  await Promise.all(
    Array.from({length: e2eUserCount}, async (_, index) => {
      const container = new WorkspaceContainer(
        `e2e-${index}@example.com`,
      );

      await container.stop();
    }),
  );
});
