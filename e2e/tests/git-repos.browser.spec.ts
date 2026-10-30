import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// New repositories on /repos (docs/teacher-access.md, »Freie Repositorys
// einer Gruppe«, »Eigene Repositorys zum Üben«): a teacher makes one for the
// group and deletes it again; a student makes one of their own, which only
// they may read, not the teacher of their group and not another student;
// the profile puts Git into the menu. Needs GIT_SERVER = true.
test('repositories made on /repos: one of the group, and one of a student that stays private', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(180_000);
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const first = await teacher.goto('/repos');
  test.skip(first?.status() === 404, 'GIT_SERVER is off in this installation');
  let ownUrl = '';

  try {
    // The teacher: a free repository of the group, with a README.md.
    await teacher.getByRole('button', { name: 'Neues Repository' }).first().click();
    await teacher.locator('#git-new-group').selectOption('E2E');
    await teacher.locator('#git-new-name-group').fill('e2e-projekt');
    await teacher.locator('#git-new-description-group').fill('Ein Projekt der Klasse');
    await teacher.locator("[data-git-new='group'] [type=submit]").click();
    await expect(teacher).toHaveURL(/\/git\/[a-z0-9]{10}\/e2e-projekt$/);
    await expect(teacher.locator('.git-readme h1')).toHaveText('e2e-projekt');
    await teacher.goto('/repos');
    const row = teacher.locator('table.git-overview tbody tr', { hasText: 'e2e-projekt' });
    await expect(row).toContainText('frei');
    await expect(row).toContainText('Ein Projekt der Klasse');
    // the same name twice: refused with a word on why
    await teacher.getByRole('button', { name: 'Neues Repository' }).first().click();
    await teacher.locator('#git-new-name-group').fill('e2e-projekt');
    await teacher.locator("[data-git-new='group'] [type=submit]").click();
    await expect(teacher.locator("[data-git-new='group'] .invalid-feedback")).toContainText('gibt es hier schon');

    // The student: an own repository, the page of it.
    await loginAsE2eUser(page, e2eEmail, testInfo);
    await page.goto('/repos');
    await expect(page.locator('table.git-overview tbody tr', { hasText: 'e2e-projekt' })).toBeVisible();
    await expect(page.locator('table.git-overview').first()).not.toContainText('Einstellungen');
    await page.locator('#meine').getByRole('button', { name: 'Neues Repository' }).click();
    await page.locator('#git-new-name-mine').fill('e2e-uebung');
    await page.locator("[data-git-new='mine'] [type=submit]").click();
    await expect(page).toHaveURL(/\/git\/[a-z0-9]{10}\/e2e-uebung$/);
    ownUrl = new URL(page.url()).pathname;
    await expect(page.locator('.git-crumbs')).toContainText('Meine Repositorys');
    await expect(page.locator('.git-files')).toContainText('README.md');

    // Private: not the teacher of the student's group, not another student.
    expect((await teacher.request.get(ownUrl)).status()).toBe(403);
    expect((await teacher.request.get(ownUrl + '/raw/main/README.md')).status()).toBe(403);
    const other = await (await browser.newContext()).newPage();
    await loginAsE2eUser(other, 'e2e-materials-teacher+test@example.com', testInfo);
    expect((await other.request.get(ownUrl)).status()).toBe(403);
    await teacher.goto('/repos');
    await expect(teacher.locator('body')).not.toContainText('e2e-uebung');

    // The profile: Git in the menu, and back.
    await page.goto('/profil');
    const toggle = page.locator(".bu-toggle-module[data-module='git']");
    await expect(page.locator('#git')).toContainText('e2e-uebung');
    const menu = page.locator("[data-menu-module='git']");
    const shown = await menu.isVisible();
    await toggle.click();
    await expect(menu).toBeVisible({ visible: !shown });
    await toggle.click();
    await expect(menu).toBeVisible({ visible: shown });

    // Deleting after typing the name: the student's, then the teacher's.
    await page.goto('/repos');
    await page.locator("[data-git-delete='mine'][data-name='e2e-uebung']").click();
    const dialog = page.getByRole('dialog');
    await expect(dialog.getByRole('button', { name: 'Endgültig löschen' })).toBeDisabled();
    await dialog.getByLabel('Zum Bestätigen den Namen eintippen').fill('e2e-uebung');
    await dialog.getByRole('button', { name: 'Endgültig löschen' }).click();
    await expect(page.locator('#meine')).not.toContainText('e2e-uebung');
    ownUrl = '';
    await teacher.goto('/repos');
    await teacher.locator("[data-git-delete='group'][data-name='e2e-projekt']").click();
    await teacher.getByRole('dialog').getByLabel('Zum Bestätigen den Namen eintippen').fill('e2e-projekt');
    await teacher.getByRole('dialog').getByRole('button', { name: 'Endgültig löschen' }).click();
    await expect(teacher.locator('.git-overview-section').first()).not.toContainText('e2e-projekt');
  } finally {
    if (ownUrl)
      await page.request.post('/api/git/delete_my_repo', { data: { name: 'e2e-uebung', confirmation: 'e2e-uebung' } });
    await teacher.request.post('/api/git/delete_group_repo', { data: { group: 'E2E', name: 'e2e-projekt', confirmation: 'e2e-projekt' } });
  }
});
