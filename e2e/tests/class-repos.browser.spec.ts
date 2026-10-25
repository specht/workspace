import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The sandboxes' class repository is set per group on the class page
// (docs/teacher-access.md, »Klassen-Repository«). A student of a group with
// an address sees it in the clone command and gets the Git tasks; a student
// of a group without one sees the page without Git: clone only the app,
// create the folder, no Git tasks, and the class board of that group does
// not count them either. An https:// address leaves out the SSH key.
test('a teacher sets the class repository of a group; its students see it, others work without Git', async ({ browser }, testInfo) => {
  test.setTimeout(150_000);
  const testGroup = 'Testgruppe (E2E Teacher)';
  const address = `git@git.example.org:e2e/sorting-${Date.now()}.git`;
  const errors: string[] = [];

  const teacher = await (await browser.newContext()).newPage();
  teacher.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  // The group E2E has none for the sorting sandbox (e2e-peer is in it).
  const cleared = await (await teacher.request.post('/api/set_group_class_repos', { data: { group: 'E2E', repos: { 'sandbox-sortieren': '' } } })).json();
  expect(cleared.ok).toBe(true);

  try {
    await teacher.goto('/klasse?group=' + encodeURIComponent(testGroup) + '&tab=fortschritt');
    await teacher.getByRole('button', { name: /Klassen-Repositorys/ }).click();
    const dialog = teacher.locator('#__template_modal');
    await expect(dialog.locator('.modal-title')).toContainText(`Klassen-Repositorys für ${testGroup}`);
    const field = dialog.getByRole('textbox', { name: 'Sortieralgorithmen erforschen' });
    await expect(dialog.locator('.class-repo-input')).toHaveCount(3);

    // Not an address: named, nothing saved.
    await field.fill('git@git.example.org:e2e/sorting.git; rm -rf ~');
    await dialog.getByRole('button', { name: 'Speichern' }).click();
    await expect(field).toHaveClass(/is-invalid/);
    await expect(dialog.locator('.is-invalid + .invalid-feedback')).toBeVisible();
    await expect(dialog).toBeVisible();

    await field.fill(address);
    await dialog.getByRole('button', { name: 'Speichern' }).click();
    await expect(dialog).toBeHidden();
    await expect(teacher.locator('.class-repos-count')).toHaveText(/^· [1-3]$/);
    await expect(teacher.locator('.class-repos-open')).toHaveClass(/has-repos/);

    // A teacher of a group it does not manage: refused.
    const refused = await teacher.request.post('/api/set_group_class_repos', { data: { group: 'E2E Teachers', repos: { 'sandbox-sortieren': address } } });
    expect(refused.status()).toBe(403);

    // The activity page names the groups that have an address, leaves out
    // the others and leads to the dialog.
    await teacher.goto('/sandbox-sortieren');
    const line = teacher.locator('.class-repo-teacher');
    await expect(line).toContainText(address);
    await expect(line).not.toContainText('E2E:');
    await expect(line).toContainText('Deine anderen Gruppen sehen die Seite ohne Git.');
    await expect(teacher.locator('pre', { hasText: 'git clone [Adresse des Klassen-Repositorys]' })).toHaveCount(1);
    await line.getByRole('link', { name: testGroup }).click();
    await expect(teacher.locator('#__template_modal .modal-title')).toContainText(testGroup);
    await expect(teacher.locator('#__template_modal').getByRole('textbox', { name: 'Sortieralgorithmen erforschen' })).toBeFocused();
    await expect(teacher.locator('#__template_modal').getByRole('textbox', { name: 'Sortieralgorithmen erforschen' })).toHaveValue(address);
    await teacher.keyboard.press('Escape');

    // A student of that group: the address, the server, the Git tasks.
    const student = await (await browser.newContext()).newPage();
    student.on('pageerror', (e) => errors.push(e.message));
    await loginAsE2eUser(student, 'e2e-teacher+test@example.com', testInfo);
    await student.goto('/sandbox-sortieren');
    await expect(student.locator('pre', { hasText: `git clone ${address} ~/sorting-sandbox/algorithms` })).toHaveCount(1);
    await expect(student.getByText('Das Klassen-Repository liegt auf dem Git-Server git.example.org.')).toBeVisible();
    await expect(student.locator('pre', { hasText: 'ssh -T git@git.example.org' })).toHaveCount(1);
    for (const id of ['ssh-schluessel', 'git-name', 'geklont', 'klassen-repository', 'fehlstaende', 'nur-dein-ordner', 'veroeffentlicht'])
      await expect(student.locator(`#aufgabe-${id}`)).toHaveCount(1);
    await expect(student.locator('pre', { hasText: 'mkdir ~/sorting-sandbox/algorithms' })).toHaveCount(0);

    // A student of a group without one: the page without Git.
    const peer = await (await browser.newContext()).newPage();
    peer.on('pageerror', (e) => errors.push(e.message));
    await loginAsE2eUser(peer, 'e2e-peer@example.com', testInfo);
    await peer.goto('/sandbox-sortieren');
    await expect(peer.locator('pre', { hasText: 'mkdir ~/sorting-sandbox/algorithms' })).toHaveCount(1);
    await expect(peer.locator('pre', { hasText: 'git clone https://github.com/specht/sorting-sandbox.git ~/sorting-sandbox' })).toHaveCount(1);
    await expect(peer.getByText('Adresse des Klassen-Repositorys')).toHaveCount(0);
    await expect(peer.getByText(address)).toHaveCount(0);
    for (const id of ['ssh-schluessel', 'git-name', 'klassen-repository', 'nur-dein-ordner', 'veroeffentlicht'])
      await expect(peer.locator(`#aufgabe-${id}`)).toHaveCount(0);
    for (const id of ['geklont', 'fehlstaende', 'rennen-sortiert', 'verdoppeln', 'stabilitaet'])
      await expect(peer.locator(`#aufgabe-${id}`)).toHaveCount(1);
    await expect(peer.getByRole('heading', { name: 'Veröffentlichen' })).toHaveCount(0);

    // An https:// address: the Git tasks without the SSH key, signing in
    // with a token instead.
    const httpsAddress = `https://git.example.org/e2e/sorting-${Date.now()}.git`;
    const set = await (await teacher.request.post('/api/set_group_class_repos', { data: { group: 'E2E', repos: { 'sandbox-sortieren': httpsAddress } } })).json();
    expect(set.ok).toBe(true);
    await peer.goto('/sandbox-sortieren');
    await expect(peer.locator('pre', { hasText: `git clone ${httpsAddress} ~/sorting-sandbox/algorithms` })).toHaveCount(1);
    await expect(peer.getByText('Statt deines Passworts wollen die meisten Server ein Zugangstoken', { exact: false })).toBeVisible();
    await expect(peer.locator('pre', { hasText: "git config --global credential.helper 'cache --timeout=28800'" })).toHaveCount(1);
    await expect(peer.locator('pre', { hasText: 'ssh-keygen' })).toHaveCount(0);
    await expect(peer.locator('pre', { hasText: 'ssh -T' })).toHaveCount(0);
    await expect(peer.locator('#aufgabe-ssh-schluessel')).toHaveCount(0);
    for (const id of ['git-name', 'klassen-repository', 'nur-dein-ordner', 'veroeffentlicht', 'geklont', 'fehlstaende'])
      await expect(peer.locator(`#aufgabe-${id}`)).toHaveCount(1);
    await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=sandbox-sortieren');
    await expect(teacher.locator('table.class-board thead')).toContainText('Committet und gepusht');
    await expect(teacher.locator('table.class-board thead')).not.toContainText('Dein Schlüssel');
    await teacher.request.post('/api/set_group_class_repos', { data: { group: 'E2E', repos: { 'sandbox-sortieren': '' } } });

    // The class board counts what each group sees.
    await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=sandbox-sortieren');
    await expect(teacher.locator('table.class-board')).toBeVisible();
    await expect(teacher.locator('table.class-board thead')).toContainText('Die Sandbox ist da');
    await expect(teacher.locator('table.class-board thead')).not.toContainText('Committet und gepusht');
    await teacher.goto('/klasse?group=' + encodeURIComponent(testGroup) + '&tab=fortschritt&activity=sandbox-sortieren');
    await expect(teacher.locator('table.class-board thead')).toContainText('Committet und gepusht');
    await expect(teacher.locator('table.class-board thead')).toContainText('Dein Schlüssel');

    // No group with an address: no box, a hint where to set one.
    await teacher.request.post('/api/set_group_class_repos', { data: { group: testGroup, repos: { 'sandbox-sortieren': '' } } });
    await teacher.goto('/sandbox-sortieren');
    await expect(teacher.locator('.class-repo-teacher')).toHaveCount(0);
    await expect(teacher.locator('.class-repo-hint')).toContainText('Lernende sehen diese Seite ohne Git.');
    await expect(teacher.locator('.class-repo-hint a', { hasText: 'Klassenseite' })).toHaveCount(1);
    expect(errors).toEqual([]);
  } finally {
    await teacher.request.post('/api/set_group_class_repos', { data: { group: testGroup, repos: { 'sandbox-sortieren': '' } } });
    await teacher.request.post('/api/set_group_class_repos', { data: { group: 'E2E', repos: { 'sandbox-sortieren': '' } } });
  }
});
