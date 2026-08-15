import { test as teardown } from '@playwright/test';
import { WorkspaceContainer } from './workspace-container';

const e2eUserCount = Number.parseInt(
  process.env.E2E_USER_COUNT ?? '8',
  10,
);

if (!Number.isInteger(e2eUserCount) || e2eUserCount < 1)
  throw new Error('E2E_USER_COUNT must be a positive integer');

teardown('stop all E2E workspace containers', async () => {
  const emails = Array.from(
    {length: e2eUserCount},
    (_, index) => `e2e-${index}@example.com`,
  );
  emails.push('e2e-peer@example.com');

  await Promise.all(
    emails.map(async email => {
      const container = new WorkspaceContainer(email);
      await container.stop();
    }),
  );
});
