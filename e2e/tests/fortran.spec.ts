import { test } from './fixtures';
import {
  expectVsCodeReady,
  launchWorkspace,
  loginAsE2eUser,
  resetWorkspace,
} from './workspace';
import {
  createTutorialTextFile,
  expectTerminalText,
  openTutorialTerminal,
  runTerminalCommand,
} from './vscode';

const HELLO_WORLD = `program HelloWorld
    print *, "Hello, World!"
end program HelloWorld`;

test('Fortran tutorial: Hello World compiles and runs', async ({
  page,
  e2eEmail,
}, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await resetWorkspace(page, testInfo);

  const workspace = await launchWorkspace(page, testInfo);
  await expectVsCodeReady(workspace, testInfo);

  await createTutorialTextFile(
    workspace,
    'hello.f90',
    HELLO_WORLD,
    testInfo,
  );

  await openTutorialTerminal(workspace, testInfo);
  await runTerminalCommand(
    workspace,
    'gfortran hello.f90 -o hello',
    testInfo,
    'fortran-hello-compiled',
  );
  await runTerminalCommand(
    workspace,
    './hello',
    testInfo,
    'fortran-hello-ran',
  );
  await expectTerminalText(
    workspace,
    'Hello, World!',
    testInfo,
    'fortran-hello-output',
  );
});
