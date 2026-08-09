import type { TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import type {
  ContainerCommandResult,
  WorkspaceContainer,
} from './workspace-container';

const LATEX_REPOSITORY =
  'https://github.com/specht/latex-tutorial.git';

const LATEX_WORKSHOP_EXTENSION =
  'James-Yu.latex-workshop';

const REPOSITORY_DIR =
  '/workspace/.e2e/latex-tutorial';

const DOCUMENTS = [
  'hello.tex',
  'Bewerbung.tex',
  'CV.tex',
  'Ausarbeitung.tex',
  'Mathe.tex',
  'wpgtr.tex',
] as const;

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'\\''`)}'`;
}

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

function pdfNameFor(texFile: string): string {
  return texFile.replace(/\.tex$/i, '.pdf');
}

async function verifyPdf(
  container: WorkspaceContainer,
  pdfFile: string,
) {
  const result = await container.exec(
    [
      `test -s ${shellQuote(pdfFile)}`,
      `file ${shellQuote(pdfFile)}`,
    ].join(' && '),
    {
      workdir: REPOSITORY_DIR,
    },
  );

  expectSuccess(
    result,
    `${pdfFile} verification`,
  );

  expect(
    combinedOutput(result),
  ).toMatch(/PDF document/i);
}

test(
  'LaTeX tutorial dependencies and example documents work',
  async ({
    workspaceContainer: container,
  }, testInfo: TestInfo) => {
    /*
     * The book is considerably larger than the other tutorial examples.
     * Keep the suite's normal timeout for everything else but leave enough
     * headroom for a cold TeX run on slower development machines.
     */
    test.setTimeout(360_000);

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
        formatTranscript(
          command,
          result,
        ),
      );

      return result;
    };

    try {
      await test.step(
        'Workspace contains the LaTeX toolchain',
        async () => {
          const commands = [
            'latexmk -v | head -n 2',
            'lualatex --version | head -n 1',
            'biber --version',
          ];

          for (const command of commands) {
            const result = await run(
              command,
            );

            expectSuccess(
              result,
              command,
            );
          }
        },
      );

      await test.step(
        'LaTeX Workshop can still be installed by code-server',
        async () => {
          const command = [
            'rm -rf latex-workshop-extensions latex-workshop-user-data',
            'mkdir -p latex-workshop-extensions latex-workshop-user-data',
            [
              'timeout 90s /app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/latex-workshop-user-data',
              '--extensions-dir /workspace/.e2e/latex-workshop-extensions',
              `--install-extension ${LATEX_WORKSHOP_EXTENSION}`,
              '--force',
            ].join(' '),
            [
              '/app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/latex-workshop-user-data',
              '--extensions-dir /workspace/.e2e/latex-workshop-extensions',
              '--list-extensions',
            ].join(' '),
          ].join(' && ');

          const result = await run(
            command,
            {
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            result,
            'LaTeX Workshop installation',
          );

          expect(
            combinedOutput(result)
              .toLowerCase(),
          ).toContain(
            LATEX_WORKSHOP_EXTENSION
              .toLowerCase(),
          );
        },
      );

      await test.step(
        'Tutorial repository can still be cloned',
        async () => {
          const command = [
            'rm -rf latex-tutorial',
            [
              'git clone --depth 1',
              shellQuote(LATEX_REPOSITORY),
              'latex-tutorial',
            ].join(' '),
          ].join(' && ');

          const result = await run(
            command,
            {
              timeoutMs: 90_000,
            },
          );

          expectSuccess(
            result,
            'LaTeX tutorial clone',
          );

          const expectedFiles =
            await run(
              DOCUMENTS
                .map(
                  file =>
                    `test -f ${shellQuote(file)}`,
                )
                .join(' && '),
              {
                workdir:
                  REPOSITORY_DIR,
              },
            );

          expectSuccess(
            expectedFiles,
            'LaTeX tutorial file check',
          );
        },
      );

      for (const texFile of DOCUMENTS) {
        await test.step(
          `${texFile} compiles to PDF`,
          async () => {
            const pdfFile =
              pdfNameFor(texFile);

            /*
             * All current tutorial documents are intended for LuaLaTeX.
             * Explicitly select it here so this test exercises the TeX
             * toolchain and document dependencies rather than VS Code UI
             * recipe selection.
             *
             * latexmk also takes care of the repeated passes and invokes
             * bibliography tools such as Biber when the document requires
             * them.
             */
            const command = [
              `rm -f ${shellQuote(pdfFile)}`,
              'latexmk',
              '-lualatex',
              '-interaction=nonstopmode',
              '-halt-on-error',
              '-file-line-error',
              shellQuote(texFile),
            ].join(' && ');

            const result = await run(
              command,
              {
                timeoutMs:
                  texFile ===
                  'wpgtr.tex'
                    ? 180_000
                    : 90_000,
                workdir:
                  REPOSITORY_DIR,
              },
            );

            expectSuccess(
              result,
              `${texFile} compilation`,
            );

            await verifyPdf(
              container,
              pdfFile,
            );
          },
        );
      }

      await test.step(
        'Ausarbeitung bibliography was generated by Biber',
        async () => {
          const result = await run(
            [
              'test -s Ausarbeitung.bbl',
              'test -s Ausarbeitung.bcf',
              'grep -q',
              shellQuote('mueller2022'),
              'Ausarbeitung.bbl',
            ].join(' && '),
            {
              workdir:
                REPOSITORY_DIR,
            },
          );

          expectSuccess(
            result,
            'Ausarbeitung bibliography check',
          );
        },
      );

      await test.step(
        'All six generated PDFs are present',
        async () => {
          const result = await run(
            DOCUMENTS
              .map(
                texFile =>
                  `test -s ${shellQuote(pdfNameFor(texFile))}`,
              )
              .join(' && '),
            {
              workdir:
                REPOSITORY_DIR,
            },
          );

          expectSuccess(
            result,
            'generated PDF set check',
          );
        },
      );
    }
    finally {
      await testInfo.attach(
        'latex-container-transcript',
        {
          body: Buffer.from(
            transcript.join('\n'),
            'utf8',
          ),
          contentType:
            'text/plain',
        },
      );
    }
  },
);