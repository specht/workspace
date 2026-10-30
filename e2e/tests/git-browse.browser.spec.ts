import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Reading a class repository on the site (main.rb, »Reading a group's
// repository in the browser«; docs/teacher-access.md): a student of the
// group finds it from the activity's page, opens a folder of the tree in
// place, a file with its highlighted code, and the history; the teacher
// finds it on the class board; a student of another group and a visitor
// get nothing of it. Needs GIT_SERVER = true (skipped otherwise).
test('a student of the group browses the class repository, others may not', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(180_000);
  const group = 'E2E';
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const response = await teacher.request.post('/api/git/create_repo', { data: { group, slug: 'class-quiz' } });
  test.skip(response.status() === 404, 'GIT_SERVER is off in this installation');
  const created = await response.json();
  expect(created.ok).toBe(true);
  const browsePath = '/git/' + (created.address as string).replace(/^git@git\.workspace:/, '').replace(/\.git$/, '');
  const at = (rest: string) => new RegExp(browsePath.replace(/[/.]/g, '\\$&') + rest + '$');

  try {
    // The activity's page: a button under the clone command.
    await loginAsE2eUser(page, e2eEmail, testInfo);
    await page.goto('/class-quiz');
    await page.getByRole('link', { name: 'Das Repository im Browser ansehen' }).first().click();
    await expect(page).toHaveURL(at(''));

    // A folder of the tree opens in place, without a page load.
    const tree = page.getByRole('tree', { name: 'Dateien' });
    await tree.getByRole('treeitem', { name: 'questions', exact: true }).click();
    await expect(tree.getByRole('treeitem', { name: 'beispiel.txt' })).toBeVisible();
    await expect(page).toHaveURL(at(''));

    // A file: highlighted, with a link per line; the folder stays open.
    await tree.getByRole('treeitem', { name: 'quiz.rb' }).click();
    await expect(page).toHaveURL(at('/blob/main/quiz.rb'));
    await expect(page.locator('pre.git-code .git-lc .k').first()).toBeVisible();
    await expect(page.locator('#L1 .git-ln')).toHaveAttribute('href', '#L1');
    await expect(tree.getByRole('treeitem', { name: 'quiz.rb' })).toHaveAttribute('aria-current', 'page');
    await expect(tree.getByRole('treeitem', { name: 'beispiel.txt' })).toBeVisible();

    // t finds a file by its name.
    await page.locator('body').press('t');
    await page.keyboard.type('beisp');
    await page.keyboard.press('Enter');
    await expect(page).toHaveURL(at('/blob/main/questions/beispiel.txt'));

    // The history.
    await page.locator('.git-tab', { hasText: 'Verlauf' }).click();
    await expect(page).toHaveURL(at('/commits/main'));
    await expect(page.locator('.git-log-subject').first()).toBeVisible();

    // The sidebar's folders as JSON, a file on its own as text that runs nothing.
    const entries = await page.request.get(browsePath + '/entries/main/questions');
    expect(entries.status()).toBe(200);
    expect(((await entries.json()).entries as { name: string }[]).map(e => e.name)).toContain('beispiel.txt');
    const raw = await page.request.get(browsePath + '/raw/main/quiz.rb');
    expect(raw.headers()['content-type']).toContain('text/plain');
    expect(raw.headers()['content-security-policy']).toBe("sandbox; default-src 'none'");
    expect(raw.headers()['x-content-type-options']).toBe('nosniff');

    // The teacher: on the class board beside the activity.
    await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=class-quiz');
    await expect(teacher.locator('.class-activity-link', { hasText: 'Repository' })).toHaveAttribute('href', at(''));

    // A student of another group: refused, the pages and the JSON alike.
    const other = await (await browser.newContext()).newPage();
    await loginAsE2eUser(other, 'e2e-materials-teacher+test@example.com', testInfo);
    for (const path of ['', '/blob/main/quiz.rb', '/entries/main/questions', '/raw/main/quiz.rb'])
      expect((await other.request.get(browsePath + path)).status(), path).toBe(403);

    // Without a login, nothing.
    const anonymous = await (await browser.newContext()).request.get(browsePath + '/entries/main/questions', { maxRedirects: 0 });
    expect(anonymous.status()).not.toBe(200);
  } finally {
    await teacher.request.post('/api/git/delete_repo', { data: { group, slug: 'class-quiz' } });
  }
});
