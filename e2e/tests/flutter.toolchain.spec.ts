import { expect, test } from './fixtures';
import type { ContainerCommandResult } from './workspace-container';

const PROJECT_NAME = 'flutter_android_smoke';
const PROJECT_DIR =
  '/workspace/.e2e/flutter-android-smoke';

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

function formatTranscript(
  command: string,
  result: ContainerCommandResult,
): string {
  return [
    `$ ${command}`,
    result.stdout.trimEnd(),
    result.stderr.trimEnd()
      ? `[stderr]\n${result.stderr.trimEnd()}`
      : '',
    `[exit ${result.exitCode}]`,
    '',
  ]
    .filter(line => line !== '')
    .join('\n');
}

test(
  'Flutter creates and builds a fresh Android debug APK',
  async ({
    workspaceContainer: container,
  }, testInfo) => {
    test.setTimeout(480_000);

    const transcript: string[] = [];

    const run = async (
      command: string,
      options: {
        timeoutMs?: number;
        workdir?: string;
      } = {},
    ) => {
      const result = await container.exec(
        command,
        {
          timeoutMs:
            options.timeoutMs ??
            60_000,
          workdir:
            options.workdir ??
            '/workspace/.e2e',
        },
      );

      transcript.push(
        formatTranscript(command, result),
      );

      return result;
    };

    try {
      await test.step(
        'Flutter and the managed Android SDK are available',
        async () => {
          const flutter = await run(
            'flutter --version',
          );

          expectSuccess(
            flutter,
            'Flutter version check',
          );
          expect(flutter.stdout).toContain(
            'Flutter',
          );

          const androidSdk = await run(
            '[ -n "$ANDROID_SDK_ROOT" ] && ' +
            '[ -d "$ANDROID_SDK_ROOT" ] && ' +
            '[ ! -w "$ANDROID_SDK_ROOT" ]',
          );

          expectSuccess(
            androidSdk,
            'managed Android SDK check',
          );
        },
      );

      await test.step(
        'Create a fresh Android-only Flutter app',
        async () => {
          const create = await run(
            [
              'flutter create',
              '--empty',
              '--platforms=android',
              `--project-name=${PROJECT_NAME}`,
              'flutter-android-smoke',
            ].join(' '),
            {
              timeoutMs: 180_000,
            },
          );

          expectSuccess(
            create,
            'Flutter Android project creation',
          );

          const scaffold = await run(
            [
              'test -f pubspec.yaml',
              'test -x android/gradlew',
              'test -f lib/main.dart',
            ].join(' && '),
            {
              workdir: PROJECT_DIR,
            },
          );

          expectSuccess(
            scaffold,
            'Flutter Android project scaffold check',
          );
        },
      );

      await test.step(
        'Build a debug APK in the real student container',
        async () => {
          const build = await run(
            [
              'flutter build apk --debug',
              'test -s build/app/outputs/flutter-apk/app-debug.apk',
            ].join(' && '),
            {
              workdir: PROJECT_DIR,
              timeoutMs: 240_000,
            },
          );

          expectSuccess(
            build,
            'Flutter Android debug APK build',
          );
        },
      );
    }
    finally {
      const cleanup = await container.exec(
        [
          'if [ -x flutter-android-smoke/android/gradlew ]; then',
          'cd flutter-android-smoke && ./android/gradlew --stop;',
          'fi',
        ].join(' '),
        {
          workdir: '/workspace/.e2e',
          timeoutMs: 60_000,
        },
      );

      transcript.push(
        formatTranscript(
          'stop Flutter Gradle daemons',
          cleanup,
        ),
      );

      await testInfo.attach(
        'flutter-android-container-transcript',
        {
          body: Buffer.from(
            transcript.join('\n'),
            'utf8',
          ),
          contentType: 'text/plain',
        },
      );
    }
  },
);
