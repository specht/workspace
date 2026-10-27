import { expect, test } from './fixtures';
import type { ContainerCommandResult } from './workspace-container';

// Dart on its own (docker/code/Dockerfile, DART_VERSION), since Flutter left
// the image, and no usage reporting from the tools that have it: the
// environment variables the image sets, and what init-code-server-run
// switches off in the student's home at every start.
const DART_VERSION = '3.13.0';
const PROJECT_DIR = '/workspace/.e2e/dart-smoke';

function expectSuccess(
  result: ContainerCommandResult,
  description: string,
) {
  expect(
    result.exitCode,
    `${description} failed\n` +
    `stdout:\n${result.stdout}\n` +
    `stderr:\n${result.stderr}`,
  ).toBe(0);
}

test(
  'Dart runs and compiles a program, and no tool reports usage',
  async ({
    workspaceContainer: container,
  }) => {
    test.setTimeout(240_000);
    const run = (command: string) =>
      container.exec(command, { timeoutMs: 120_000, workdir: '/workspace' });

    await test.step('the standalone Dart SDK', async () => {
      const where = await run('command -v dart');
      expectSuccess(where, 'command -v dart');
      expect(where.stdout.trim()).toBe('/opt/dart-sdk/bin/dart');
      const version = await run('dart --version');
      expectSuccess(version, 'dart --version');
      expect(version.stdout + version.stderr).toContain(DART_VERSION);
    });

    await test.step('a program, run and compiled', async () => {
      const write = await run(
        `rm -rf ${PROJECT_DIR} && mkdir -p ${PROJECT_DIR} && ` +
        `printf 'void main() {\\n  print(6 * 7);\\n}\\n' > ${PROJECT_DIR}/main.dart`,
      );
      expectSuccess(write, 'writing main.dart');
      const ran = await run(`dart run ${PROJECT_DIR}/main.dart`);
      expectSuccess(ran, 'dart run');
      expect(ran.stdout.trim()).toBe('42');
      const compiled = await run(
        `dart compile exe ${PROJECT_DIR}/main.dart -o ${PROJECT_DIR}/main && ${PROJECT_DIR}/main`,
      );
      expectSuccess(compiled, 'dart compile exe');
      expect(compiled.stdout.trim().split('\n').pop()).toBe('42');
    });

    await test.step('no usage reporting', async () => {
      const environment = await run(
        'test "$DART_SUPPRESS_ANALYTICS" = true && ' +
        'test "$DOTNET_CLI_TELEMETRY_OPTOUT" = 1 && ' +
        'test "$POWERSHELL_TELEMETRY_OPTOUT" = 1 && ' +
        'test "$DO_NOT_TRACK" = 1',
      );
      expectSuccess(environment, 'the opt-out variables');
      const dart = await run('grep -x reporting=0 /workspace/.dart-tool/dart-flutter-telemetry.config');
      expectSuccess(dart, 'Dart reporting=0 in the home directory');
    });

    await run(`rm -rf ${PROJECT_DIR}`);
  },
);
