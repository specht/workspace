import { expect, test } from './fixtures';
import { readTutorialFile } from './tutorial';
import type { ContainerCommandResult } from './workspace-container';

const HELLO_WORLD = readTutorialFile(
  'c',
  'hello.c',
);
const FACTOR = readTutorialFile(
  'c',
  'factor.c',
);
const BUBBLESORT = readTutorialFile(
  'c',
  'bubblesort.c',
);

const BROKEN_HELLO = HELLO_WORLD.replace(
  /\bprintf\b/,
  'prinft',
);

function combinedOutput(
  result: ContainerCommandResult,
): string {
  return [
    result.stdout,
    result.stderr,
  ]
    .filter(Boolean)
    .join('\n');
}

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

/**
 * Extract the numbers printed immediately after a label.
 *
 * The C Bubblesort tutorial prints each array on the same line:
 *
 *   Original array: 42 7 ...
 *   Sorted array:   7 42 ...
 */
function numbersAfterLabel(
  text: string,
  label: string,
): number[] {
  const line = text
    .split(/\r?\n/)
    .find(candidate => candidate.includes(label));

  if (!line)
    return [];

  const afterLabel = line.slice(
    line.indexOf(label) + label.length,
  );

  return (afterLabel.match(/-?\d+/g) ?? [])
    .map(Number);
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
  'C dependencies and tutorial programs work',
  async ({
    workspaceContainer: container,
  }, testInfo) => {
    const transcript: string[] = [];

    const run = async (
      command: string,
      options: {
        input?: string;
        timeoutMs?: number;
      } = {},
    ) => {
      const result = await container.exec(
        command,
        options,
      );

      transcript.push(
        formatTranscript(command, result),
      );

      return result;
    };

    try {
      await test.step(
        'Workspace container has the GNU C compiler',
        async () => {
          const result = await run(
            'gcc --version | head -n 1',
          );

          expectSuccess(
            result,
            'gcc version check',
          );

          expect(result.stdout).toMatch(
            /gcc/i,
          );
        },
      );

      await test.step(
        'hello.c compiles and runs',
        async () => {
          await container.writeFile(
            'hello.c',
            HELLO_WORLD,
          );

          const compile = await run(
            'gcc hello.c -o hello',
          );

          expectSuccess(
            compile,
            'hello.c compilation',
          );

          const executable = await run(
            'test -x hello && ! test hello -ot hello.c',
          );

          expectSuccess(
            executable,
            'hello executable freshness check',
          );

          const execute = await run('./hello');

          expectSuccess(
            execute,
            'hello execution',
          );

          expect(execute.stdout).toContain(
            'Hello, World!',
          );
        },
      );

      await test.step(
        'gcc rejects the deliberately broken hello.c',
        async () => {
          await container.writeFile(
            'broken-hello.c',
            BROKEN_HELLO,
          );

          const result = await run(
            'gcc broken-hello.c -o broken-hello',
          );

          expect(
            result.exitCode,
            'deliberately broken C unexpectedly compiled',
          ).not.toBe(0);

          /*
           * Do not couple the test to one GCC version's exact diagnostic.
           * With "prinft", GCC may diagnose an implicit declaration first
           * and then fail at link time with an undefined reference.
           */
          expect(
            combinedOutput(result),
          ).toMatch(
            /error:|undefined reference/i,
          );
        },
      );

      await test.step(
        'factor.c compiles and accepts input',
        async () => {
          await container.writeFile(
            'factor.c',
            FACTOR,
          );

          const compile = await run(
            'gcc factor.c -o factor',
          );

          expectSuccess(
            compile,
            'factor.c compilation',
          );

          const execute = await run(
            './factor',
            {
              input: '123\n',
            },
          );

          expectSuccess(
            execute,
            'factor execution',
          );

          expect(execute.stdout).toContain(
            'Prime factors of 123 are:',
          );

          expect(execute.stdout).toMatch(
            /(^|\s)3(\s|$)/,
          );

          expect(execute.stdout).toMatch(
            /(^|\s)41(\s|$)/,
          );
        },
      );

      await test.step(
        'bubblesort.c compiles and really sorts its values',
        async () => {
          await container.writeFile(
            'bubblesort.c',
            BUBBLESORT,
          );

          const compile = await run(
            'gcc bubblesort.c -o bubblesort',
          );

          expectSuccess(
            compile,
            'bubblesort.c compilation',
          );

          const execute = await run(
            './bubblesort',
          );

          expectSuccess(
            execute,
            'bubblesort execution',
          );

          const original = numbersAfterLabel(
            execute.stdout,
            'Original array:',
          );

          const sorted = numbersAfterLabel(
            execute.stdout,
            'Sorted array:',
          );

          expect(original).toHaveLength(10);
          expect(sorted).toHaveLength(10);

          expect(sorted).toEqual(
            [...original].sort((a, b) => a - b),
          );
        },
      );
    }
    finally {
      await testInfo.attach(
        'c-container-transcript',
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