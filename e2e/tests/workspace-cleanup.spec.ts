import { test as teardown } from '@playwright/test';
import { WorkspaceContainer } from './workspace-container';

teardown('stop E2E workspace container', async () => {
  const container = new WorkspaceContainer(
    'e2e-0@example.com',
  );

  await container.stop();
});