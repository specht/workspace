import { defineConfig } from '@playwright/test';

const workers = Number.parseInt(process.env.E2E_WORKERS ?? '1', 10);
const e2eUserCount = Number.parseInt(process.env.E2E_USER_COUNT ?? '8', 10);

if (!Number.isInteger(workers) || workers < 1)
  throw new Error('E2E_WORKERS must be a positive integer');

if (!Number.isInteger(e2eUserCount) || e2eUserCount < 1)
  throw new Error('E2E_USER_COUNT must be a positive integer');

if (workers > e2eUserCount)
  throw new Error(`E2E_WORKERS (${workers}) exceeds E2E_USER_COUNT (${e2eUserCount})`);

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
  use: {
    baseURL: process.env.E2E_BASE_URL ?? 'http://workspace.test:8025',
    viewport: { width: 1600, height: 1000 },
    trace: 'on',
    screenshot: 'only-on-failure',
    video: 'retain-on-failure',
    launchOptions: {
      // Keep the integrated terminal in VS Code's DOM renderer so Playwright
      // can assert terminal output text instead of inspecting painted pixels.
      args: ['--disable-gpu'],
    },
  },
});
