import { test } from './fixtures';
import {
  expectVsCodeReady,
  launchWorkspace,
  loginAsE2eUser,
  resetWorkspace,
} from './workspace';

test('a fresh student workspace opens successfully', async ({
  page,
  e2eEmail,
}, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await resetWorkspace(page, testInfo);

  const workspace = await launchWorkspace(page, testInfo);
  await expectVsCodeReady(workspace, testInfo);
});
