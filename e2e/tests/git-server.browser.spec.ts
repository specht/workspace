import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

// The Git server of the groups (docs/teacher-access.md, »Repositorys im
// Workspace«), the whole way: a teacher has the Workspace set up keys for
// the group and creates the class quiz's repository from its starter
// files; a student's Workspace greets the server, clones, pushes a round
// and is refused a force push; the student's page shows the variant for
// the Workspace's server; student and teacher read the commit on the site.
// Needs GIT_SERVER = true (skipped otherwise).
test('a repository in the Workspace: created by a teacher, pushed to by a student, read on the site', async ({ freshWorkspace: workspace, e2eEmail, browser }, testInfo) => {
  test.setTimeout(240_000);
  const group = 'E2E';
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);

  const keys = await teacher.request.post('/api/git/set_keys', { data: { group, mode: 'auto' } });
  test.skip(keys.status() === 404, 'GIT_SERVER is off in this installation');
  expect((await keys.json()).ok).toBe(true);
  const created = await (await teacher.request.post('/api/git/create_repo', { data: { group, slug: 'class-quiz' } })).json();
  expect(created.ok).toBe(true);
  const address: string = created.address;
  expect(address).toMatch(/^git@git\.workspace:[a-z0-9]{10}\/class-quiz\.git$/);
  const browsePath = '/git/' + address.replace(/^git@git\.workspace:/, '').replace(/\.git$/, '');

  const container = new WorkspaceContainer(e2eEmail);
  await container.waitUntilRunning();
  const run = (command: string) => container.exec(command, { workdir: '/workspace', timeoutMs: 60_000 });
  try {
    // The automatic key is in the Workspace, with the server's key in known_hosts.
    const hello = await run('ssh -T git@git.workspace');
    expect(hello.exitCode, hello.stderr).toBe(0);
    expect(hello.stdout).toContain('Dein Schlüssel ist eingetragen');
    expect(hello.stdout).toContain(address);

    const push = await run(
      `rm -rf e2e-quiz && git clone -q ${address} e2e-quiz && cd e2e-quiz `
      + `&& git config user.name "E2E Worker" && git config user.email e2e-worker@example.com `
      + `&& printf '? Wie viele Bits hat ein Byte?\\n= 8\\n' > questions/e2e.txt && echo questions/e2e.txt >> rounds.txt `
      + `&& ruby quiz.rb --check && git add -A && git commit -qm "Runde von E2E" && git push -q`,
    );
    expect(push.exitCode, `${push.stdout}\n${push.stderr}`).toBe(0);
    const force = await run('cd e2e-quiz && git commit -q --amend -m "Anders" && git push --force 2>&1');
    expect(force.exitCode).not.toBe(0);
    expect(force.stdout + force.stderr).toMatch(/non-fast-forward/);

    // The student's page: the address and the box for the automatic key.
    await workspace.goto('/class-quiz');
    await expect(workspace.locator('pre', { hasText: `git clone ${address}` })).toHaveCount(1);
    await expect(workspace.locator('[data-git-key]')).toContainText('workspace_git');

    // The student reads the history, the teacher the commit.
    await workspace.goto(browsePath + '/commits/main');
    await expect(workspace.locator('.git-log-subject').first()).toHaveText('Runde von E2E');
    await teacher.goto(browsePath + '/commits/main');
    await teacher.locator('.git-log-subject', { hasText: 'Runde von E2E' }).click();
    await expect(teacher.locator('.git-diff-path', { hasText: 'questions/e2e.txt' })).toBeVisible();
    await expect(teacher.locator('.git-add', { hasText: 'Wie viele Bits' })).toBeVisible();

    // The teacher's overview of the group's repositories: the row with the
    // address, the last commit's author (no e-mail) and the way to look at
    // it. Students have the page too, with their groups' repositories and
    // their own (CHANGELOG, »Mit GIT_SERVER«).
    await teacher.goto('/repos');
    const row = teacher.locator('table.git-overview tbody tr', { hasText: address });
    await expect(row).toHaveCount(1);
    await expect(row).toContainText(group);
    await expect(row.locator('.git-overview-author')).toHaveText('E2E Worker');
    await expect(row).not.toContainText('e2e-worker@example.com');
    await row.getByRole('link', { name: 'Ansehen' }).click();
    await expect(teacher).toHaveURL(new RegExp(browsePath + '$'));
    await expect(teacher.locator('.git-crumbs a', { hasText: 'Repositorys' })).toHaveAttribute('href', /\/repos$/);
    expect((await workspace.request.get('/repos')).status()).toBe(200);

    // Without a login, nothing of it.
    const anonymous = await (await browser.newContext()).request.get(browsePath, { maxRedirects: 0 });
    expect(anonymous.status()).not.toBe(200);
  } finally {
    await run('rm -rf e2e-quiz');
    await teacher.request.post('/api/git/delete_repo', { data: { group, slug: 'class-quiz' } });
    await teacher.request.post('/api/git/set_keys', { data: { group, mode: 'handed_in' } });
  }
});

// The other way: the student creates a key pair, pastes the public key
// into the box in the Handbuch (a private key is refused), and gets in after
// accepting the server's key, whose fingerprint the box shows.
test('a student hands in a key and reaches the Git server with it', async ({ freshWorkspace: workspace, e2eEmail, browser }, testInfo) => {
  test.setTimeout(240_000);
  const group = 'E2E';
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const keys = await teacher.request.post('/api/git/set_keys', { data: { group, mode: 'handed_in' } });
  test.skip(keys.status() === 404, 'GIT_SERVER is off in this installation');
  const created = await (await teacher.request.post('/api/git/create_repo', { data: { group, slug: 'class-quiz' } })).json();
  expect(created.ok).toBe(true);

  const container = new WorkspaceContainer(e2eEmail);
  await container.waitUntilRunning();
  const run = (command: string) => container.exec(command, { workdir: '/workspace', timeoutMs: 60_000 });
  try {
    const keygen = await run('mkdir -p ~/.ssh && rm -f ~/.ssh/id_ed25519 ~/.ssh/id_ed25519.pub && ssh-keygen -q -t ed25519 -N "" -C "E2E Worker" -f ~/.ssh/id_ed25519');
    expect(keygen.exitCode, keygen.stderr).toBe(0);
    // Not handed in yet: the server refuses.
    const before = await run('ssh -o StrictHostKeyChecking=accept-new -o BatchMode=yes -T git@git.workspace');
    expect(before.stderr).toContain('Permission denied');

    await workspace.goto('/reference/gitlab');
    const box = workspace.locator('[data-git-key]').first();
    // the private key is refused, with a word on why
    await box.getByRole('textbox', { name: 'Dein öffentlicher Schlüssel' }).fill((await run('cat ~/.ssh/id_ed25519')).stdout);
    await box.getByRole('button', { name: 'Hinzufügen' }).click();
    await expect(box.locator('.invalid-feedback')).toContainText('privater');
    // the line of the public key, pasted as on any Git server
    await box.getByRole('textbox', { name: 'Dein öffentlicher Schlüssel' }).fill((await run('cat ~/.ssh/id_ed25519.pub')).stdout);
    await box.getByRole('button', { name: 'Hinzufügen' }).click();
    await expect(box).toContainText('Abgegeben');
    const fingerprint = (await run('ssh-keygen -l -f ~/.ssh/id_ed25519.pub')).stdout.split(' ')[1];
    await expect(box).toContainText(fingerprint);
    const host = (await run('ssh-keygen -l -F git.workspace')).stdout;
    const shown = (await box.locator('.git-key-host code').nth(1).innerText()).trim();
    expect(host).toContain(shown);

    const hello = await run('ssh -o BatchMode=yes -T git@git.workspace');
    expect(hello.exitCode, hello.stderr).toBe(0);
    expect(hello.stdout).toContain(created.address);
  } finally {
    await workspace.request.post('/api/git/remove_key');
    await teacher.request.post('/api/git/delete_repo', { data: { group, slug: 'class-quiz' } });
  }
});
