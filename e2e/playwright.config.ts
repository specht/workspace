import { defineConfig } from '@playwright/test';

const workers = Number.parseInt(process.env.E2E_WORKERS ?? '1', 10);
const e2eUserCount = Number.parseInt(process.env.E2E_USER_COUNT ?? '8', 10);

if (!Number.isInteger(workers) || workers < 1)
  throw new Error('E2E_WORKERS must be a positive integer');

if (!Number.isInteger(e2eUserCount) || e2eUserCount < 1)
  throw new Error('E2E_USER_COUNT must be a positive integer');

if (workers > e2eUserCount)
  throw new Error(
    `E2E_WORKERS (${workers}) exceeds E2E_USER_COUNT (${e2eUserCount})`,
  );

export default defineConfig({
  testDir: './tests',
  globalSetup: './global-setup.ts',
  fullyParallel: true,
  workers,
  timeout: 240_000,
  expect: {
    timeout: 30_000,
  },
  outputDir: 'test-results',
  reporter: [
    ['list'],
    ['html', {
      outputFolder: 'playwright-report',
      open: 'never',
      title: 'Hackschule Workspace E2E',
    }],
  ],

  /*
   * The browser project creates a real fresh Workspace container.
   *
   * Toolchain tests depend on it and reuse that exact container via
   * docker exec. They do not request Playwright's page/browser fixtures,
   * therefore no Chrome instance is needed for the toolchain project.
   *
   * Toolchains intentionally use one worker: they all reuse e2e-0's
   * running container and reset /workspace/.e2e between tests.
   */
  projects: [
    {
      name: 'workspace-smoke',
      testMatch: /workspace-smoke\.spec\.ts/,
    },
    {
      name: 'toolchains',
      testMatch: /.*\.toolchain\.spec\.ts/,
      dependencies: ['workspace-smoke'],
      workers: 1,
    },
  ],

  use: {
    baseURL: process.env.E2E_BASE_URL ?? 'http://workspace.test:8025',
    viewport: { width: 1600, height: 1000 },
    trace: 'on',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
  },
});
