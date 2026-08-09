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
     * Browser-level tests create real fresh Workspace containers.
     *
     * Keep this project on one worker because the toolchain project deliberately
     * reuses e2e-0's running container after all browser tests have completed.
     *
     * Browser tests should be reserved for behavior that genuinely requires the
     * browser, such as the Pixelflow Canvas webview.
     */
    projects: [
        {
            name: 'workspace-smoke',
            testMatch: [
                /workspace-smoke\.spec\.ts/,
                /.*\.browser\.spec\.ts/,
            ],
            workers: 1,
            use: {
                launchOptions: {
                    args: [
                        '--unsafely-treat-insecure-origin-as-secure=http://workspace.test:8025,*.workspace.test',
                    ],
                },
            },
        },
        {
            name: 'toolchains',
            testMatch: /.*\.toolchain\.spec\.ts/,
            dependencies: ['workspace-smoke'],
            workers: 1,
        },
        {
            name: 'workspace-cleanup',
            testMatch: /workspace-cleanup\.spec\.ts/,
        },
    ],

    use: {
        baseURL:
            process.env.E2E_BASE_URL ??
            'http://workspace.test:8025',
        viewport: {
            width: 1600,
            height: 1000,
        },
        trace: 'on',
        screenshot: 'only-on-failure',
        video: 'retain-on-failure',
    },
});
