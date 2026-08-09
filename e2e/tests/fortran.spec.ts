import { expect, test } from './fixtures';
import { readTutorialFile } from './tutorial';
import {
  activateEditorTab,
  closeFolder,
  createTextFile,
  expectExecutableUpToDate,
  expectTerminalText,
  installVsCodeExtensionFromQuickOpen,
  openTerminal,
  replaceActiveEditorContents,
  runInteractiveTerminalCommand,
  runTerminalCommand,
} from './vscode';

function numbersOnLineAfter(text: string, label: string): number[] {
  const lines = text.split('\n');
  let labelIndex = -1;

  for (let i = lines.length - 1; i >= 0; i--) {
    if (lines[i].includes(label)) {
      labelIndex = i;
      break;
    }
  }

  if (labelIndex < 0)
    return [];

  for (let i = labelIndex + 1; i < lines.length; i++) {
    const values = lines[i].match(/\b\d+\b/g);
    if (values)
      return values.map(Number);
  }

  return [];
}

const HELLO_WORLD = readTutorialFile('fortran', 'hello.f90');
const FACTOR = readTutorialFile('fortran', 'factor.f90');
const BUBBLESORT = readTutorialFile('fortran', 'bubblesort.f90');
const BROKEN_HELLO = HELLO_WORLD.replace(/\bprint\b/, 'prin');

const PHOTRAN_EXTENSION = 'fiuba.photran-lsp-client-vscode';

test.setTimeout(420_000);

test('Fortran tutorial works end to end', async ({
  freshWorkspace: workspace,
}, testInfo) => {
  await closeFolder(workspace, testInfo);

  await createTextFile(
    workspace,
    'hello.f90',
    HELLO_WORLD,
    testInfo,
    { screenshotPrefix: 'fortran-hello' },
  );

  await installVsCodeExtensionFromQuickOpen(
    workspace,
    PHOTRAN_EXTENSION,
    'Photran',
    testInfo,
    'Fortran',
  );

  await openTerminal(workspace, testInfo);

  await runTerminalCommand(
    workspace,
    'gfortran hello.f90 -o hello',
    testInfo,
    'fortran-hello-compiled',
  );
  await expectExecutableUpToDate(
    workspace,
    'hello',
    'hello.f90',
    testInfo,
    'fortran-hello-executable',
  );
  await runTerminalCommand(
    workspace,
    './hello',
    testInfo,
    'fortran-hello-ran',
    { waitFor: 'Hello, World!' },
  );
  await expectTerminalText(
    workspace,
    'Hello, World!',
    testInfo,
    'fortran-hello-output',
  );

  await test.step('Introduce and diagnose a compiler error', async () => {
    await activateEditorTab(workspace, 'hello.f90');
    await replaceActiveEditorContents(
      workspace,
      BROKEN_HELLO,
      'prin *, "Hello, World!"',
      testInfo,
      'fortran-hello-broken-source',
    );

    await runTerminalCommand(
      workspace,
      'gfortran hello.f90 -o hello',
      testInfo,
      'fortran-hello-compiler-error',
      {
        waitFor: /hello\.f90:2:[\s\S]*Error: Unclassifiable statement/i,
      },
    );
    await expectTerminalText(
      workspace,
      /hello\.f90:2:[\s\S]*Error: Unclassifiable statement/i,
      testInfo,
      'fortran-hello-error-message',
    );

    await replaceActiveEditorContents(
      workspace,
      HELLO_WORLD,
      'print *, "Hello, World!"',
      testInfo,
      'fortran-hello-fixed-source',
    );
    await runTerminalCommand(
      workspace,
      'gfortran hello.f90 -o hello',
      testInfo,
      'fortran-hello-fixed-compiled',
    );
    await expectExecutableUpToDate(
      workspace,
      'hello',
      'hello.f90',
      testInfo,
      'fortran-hello-fixed-executable',
    );
  }, { box: true });

  await createTextFile(
    workspace,
    'factor.f90',
    FACTOR,
    testInfo,
    {
      method: 'shortcut',
      screenshotPrefix: 'fortran-factor',
    },
  );
  await runTerminalCommand(
    workspace,
    'gfortran factor.f90 -o factor',
    testInfo,
    'fortran-factor-compiled',
  );
  await expectExecutableUpToDate(
    workspace,
    'factor',
    'factor.f90',
    testInfo,
    'fortran-factor-executable',
  );
  await runInteractiveTerminalCommand(
    workspace,
    './factor',
    [
      {
        waitFor: 'Enter a number:',
        send: '123',
      },
    ],
    testInfo,
    'fortran-factor-ran',
    {
      completion: /Prime factors of\s+123\s+:\s+3\s+41/i,
    },
  );
  await expectTerminalText(
    workspace,
    /Prime factors of\s+123\s+:\s+3\s+41/i,
    testInfo,
    'fortran-factor-output',
  );

  await createTextFile(
    workspace,
    'bubblesort.f90',
    BUBBLESORT,
    testInfo,
    {
      method: 'shortcut',
      screenshotPrefix: 'fortran-bubblesort',
    },
  );
  await runTerminalCommand(
    workspace,
    'gfortran bubblesort.f90 -o bubblesort',
    testInfo,
    'fortran-bubblesort-compiled',
  );
  await expectExecutableUpToDate(
    workspace,
    'bubblesort',
    'bubblesort.f90',
    testInfo,
    'fortran-bubblesort-executable',
  );
  const bubblesortOutput = await runTerminalCommand(
    workspace,
    './bubblesort',
    testInfo,
    'fortran-bubblesort-ran',
    { waitFor: /Sorted array:[\s\S]*\b\d+\b/i },
  );

  await test.step('Verify Bubblesort really sorted the ten numbers', async () => {
    const original = numbersOnLineAfter(
      bubblesortOutput,
      'Original array:',
    );
    const sorted = numbersOnLineAfter(
      bubblesortOutput,
      'Sorted array:',
    );

    expect(original).toHaveLength(10);
    expect(sorted).toHaveLength(10);
    expect(sorted).toEqual([...original].sort((a, b) => a - b));
  }, { box: true });
});
