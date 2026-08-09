import { expect, test } from './fixtures';
import { readTutorialFile } from './tutorial';
import type { ContainerCommandResult } from './workspace-container';

const HELLO_WORLD = readTutorialFile(
  'fortran',
  'hello.f90',
);
const FACTOR = readTutorialFile(
  'fortran',
  'factor.f90',
);
const BUBBLESORT = readTutorialFile(
  'fortran',
  'bubblesort.f90',
);

const BROKEN_HELLO = HELLO_WORLD.replace(
  /\bprint\b/,
  'prin',
);

const PHOTRAN_EXTENSION =
  'fiuba.photran-lsp-client-vscode';

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

function numbersOnLineAfter(
  text: string,
  label: string,
): number[] {
  const lines = text.split(/\r?\n/);

  for (let i = 0; i < lines.length; i++) {
    if (!lines[i].includes(label))
      continue;

    for (let j = i + 1; j < lines.length; j++) {
      const matches = lines[j].match(/-?\d+/g);

      if (matches)
        return matches.map(Number);
    }
  }

  return [];
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
  'FORTRAN dependencies and tutorial programs work',
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
        'Workspace container has the FORTRAN compiler',
        async () => {
          const result = await run(
            'gfortran --version | head -n 1',
          );

          expectSuccess(
            result,
            'gfortran version check',
          );

          expect(result.stdout).toMatch(
            /GNU Fortran/i,
          );
        },
      );

      await test.step(
        'Photran can still be installed by code-server',
        async () => {
          const command = [
            'rm -rf extensions user-data',
            'mkdir -p extensions user-data',
            [
              'timeout 90s /app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/user-data',
              '--extensions-dir /workspace/.e2e/extensions',
              `--install-extension ${PHOTRAN_EXTENSION}`,
            ].join(' '),
            [
              '/app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/user-data',
              '--extensions-dir /workspace/.e2e/extensions',
              '--list-extensions',
            ].join(' '),
          ].join(' && ');

          const result = await run(command, {
            timeoutMs: 110_000,
          });

          expectSuccess(
            result,
            'Photran installation',
          );

          expect(
            combinedOutput(result).toLowerCase(),
          ).toContain(
            PHOTRAN_EXTENSION.toLowerCase(),
          );
        },
      );

      await test.step(
        'hello.f90 compiles and runs',
        async () => {
          await container.writeFile(
            'hello.f90',
            HELLO_WORLD,
          );

          const compile = await run(
            'gfortran hello.f90 -o hello',
          );

          expectSuccess(
            compile,
            'hello.f90 compilation',
          );

          const executable = await run(
            'test -x hello && ! test hello -ot hello.f90',
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
        'gfortran rejects invalid FORTRAN',
        async () => {
          await container.writeFile(
            'broken-hello.f90',
            BROKEN_HELLO,
          );

          const result = await run(
            'gfortran broken-hello.f90 -o broken-hello',
          );

          expect(
            result.exitCode,
            'deliberately broken FORTRAN unexpectedly compiled',
          ).not.toBe(0);

          expect(
            combinedOutput(result),
          ).toMatch(/error:/i);
        },
      );

      await test.step(
        'factor.f90 compiles and accepts terminal input',
        async () => {
          await container.writeFile(
            'factor.f90',
            FACTOR,
          );

          const compile = await run(
            'gfortran factor.f90 -o factor',
          );

          expectSuccess(
            compile,
            'factor.f90 compilation',
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
            'Prime factors of',
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
        'bubblesort.f90 compiles and really sorts its values',
        async () => {
          await container.writeFile(
            'bubblesort.f90',
            BUBBLESORT,
          );

          const compile = await run(
            'gfortran bubblesort.f90 -o bubblesort',
          );

          expectSuccess(
            compile,
            'bubblesort.f90 compilation',
          );

          const execute = await run(
            './bubblesort',
          );

          expectSuccess(
            execute,
            'bubblesort execution',
          );

          const original = numbersOnLineAfter(
            execute.stdout,
            'Original array:',
          );

          const sorted = numbersOnLineAfter(
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
        'fortran-container-transcript',
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
