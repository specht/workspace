import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The profile's MySQL databases: a person can delete only their own. The
// names below that are not the person's must never exist, so a broken
// check could not destroy anything.
test('only your own MySQL databases can be deleted', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const api = (path: string, data: object = {}) => page.request.post(path, { data });
  const databases = async (): Promise<string[]> => (await (await api('/api/get_my_mysql_databases')).json()).result.databases;

  const before = await databases();
  expect((await api('/api/create_mysql_database')).ok()).toBe(true);
  const created = (await databases()).filter((name) => !before.includes(name));
  expect(created).toHaveLength(1);

  for (const name of ['db_e2enotmine0', 'db_e2enotmine0`; SELECT 1; --', '']) {
    const response = await api('/api/delete_mysql_database', { database: name });
    expect(response.ok(), `deleting ${JSON.stringify(name)}`).toBe(false);
  }
  expect(await databases()).toContain(created[0]);

  expect((await api('/api/delete_mysql_database', { database: created[0] })).ok()).toBe(true);
  expect(await databases()).not.toContain(created[0]);
});
