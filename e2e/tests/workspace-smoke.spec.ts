import { expect, test } from './fixtures';

test('a fresh student workspace opens successfully', async ({
  freshWorkspace,
}) => {
  await expect(freshWorkspace.locator('.monaco-workbench')).toBeVisible();
});
