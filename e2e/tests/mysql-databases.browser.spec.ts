import { execFileSync } from 'node:child_process';
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

  // The extra database has a login of its own that reaches it and nothing else.
  const listing = await (await api('/api/get_my_mysql_databases')).json();
  const access = listing.result.logins[created[0]];
  expect(access.login).toBe(created[0]);
  const mysql = (sql: string) => {
    try {
      return execFileSync('docker', ['exec', 'workspace_mysql_1', 'mysql', '-N', '-u', access.login, `-p${access.password}`, '-e', sql], { stdio: 'pipe' }).toString();
    } catch (error: any) {
      return 'ERROR ' + String(error.stderr);
    }
  };
  expect(mysql(`CREATE TABLE \`${created[0]}\`.entries (id INT); SELECT COUNT(*) FROM \`${created[0]}\`.entries;`).trim()).toBe('0');
  expect(mysql('SHOW DATABASES;').split('\n').filter((l) => l.startsWith('db_') || l === before[0])).toEqual([created[0]]);
  expect(mysql(`SELECT 1 FROM \`${before[0]}\`.x LIMIT 1;`)).toContain('ERROR');

  for (const name of ['db_e2enotmine0', 'db_e2enotmine0`; SELECT 1; --', '']) {
    const response = await api('/api/delete_mysql_database', { database: name });
    expect(response.ok(), `deleting ${JSON.stringify(name)}`).toBe(false);
  }
  expect(await databases()).toContain(created[0]);

  expect((await api('/api/delete_mysql_database', { database: created[0] })).ok()).toBe(true);
  expect(await databases()).not.toContain(created[0]);
  // and its login is gone with it
  expect(mysql('SELECT 1;')).toContain('ERROR');
});
