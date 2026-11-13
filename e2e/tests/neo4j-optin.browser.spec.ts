import { execFileSync } from 'node:child_process';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A Neo4j database of one's own only on request: switching Neo4j on in the
// profile makes it and shows its login data; switching it off hides only
// the button in the menu, the database stays.
test('switching Neo4j on in the profile makes the database', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/profil');
  const toggle = page.locator("#neo4j-database .bu-toggle-module[data-module='neo4j']");
  const isOn = async () => (await toggle.getAttribute('class'))?.includes('btn-success');

  // start from off
  if (await isOn()) {
    await toggle.click();
    await expect(toggle).toHaveClass(/btn-outline-secondary/);
  }
  await expect(page.locator('#neo4j-off')).toBeVisible();
  await expect(page.locator('#neo4j-on')).toBeHidden();
  await expect(page.locator('#bu_neo4j')).toBeHidden();

  await toggle.click();
  await expect(toggle).toHaveClass(/btn-success/);
  await expect(page.locator('#neo4j-on')).toBeVisible();
  await expect(page.locator('#neo4j-off')).toBeHidden();

  const login = await page.locator('#neo4j_username').inputValue();
  const password = await page.locator('#neo4j_password').inputValue();
  const neo4j = () => {
    try {
      return execFileSync('docker', ['exec', 'workspace_neo4j_1', 'cypher-shell', '-a', 'neo4j://localhost:7687', '-u', login, '-p', password, '-d', login, 'RETURN 41 + 1 AS answer;'], { stdio: 'pipe' }).toString();
    } catch (error: any) {
      return 'ERROR ' + String(error.stderr);
    }
  };
  expect(neo4j()).toContain('42');

  // off again: the login data go from the page, the database stays
  await toggle.click();
  await expect(toggle).toHaveClass(/btn-outline-secondary/);
  await expect(page.locator('#neo4j-on')).toBeHidden();
  expect(neo4j()).toContain('42');
  await page.reload();
  await expect(page.locator('#neo4j-off')).toBeVisible();
});
