// The class page shows a hand-in in a viewer: the page runs sandboxed on
// abgaben.<host> (SubmissionView), the source is shown with line numbers,
// and Escape returns to the class page.
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

test('teacher views a hand-in on the class page', async ({ freshWorkspace: workspace, e2eEmail, browser }, testInfo) => {
  test.setTimeout(240_000);
  const container = new WorkspaceContainer(e2eEmail);
  await container.waitUntilRunning();
  const page = '<!DOCTYPE html>\n<html lang="de">\n<head><meta charset="utf-8"><title>Test</title><link rel="stylesheet" href="styles.css"></head>\n<body>\n<h1>Mein Feuerwerk</h1>\n<p id="out">noch nicht</p>\n<script src="script.js"></script>\n</body>\n</html>\n';
  await container.exec('mkdir -p /workspace/website-test && cat > /workspace/website-test/index.html', { workdir: '/workspace', input: page });
  await container.exec("echo 'h1 { color: crimson; }' > /workspace/website-test/styles.css", { workdir: '/workspace' });
  await container.exec("echo 'document.getElementById(\"out\").textContent = \"JavaScript läuft\"; try { document.cookie = \"x=1\"; if (document.cookie !== \"x=1\") throw new Error(\"cookie\"); localStorage.setItem(\"a\", \"1\"); } catch (e) { document.body.append(\" Fehler: \" + e.message); }' > /workspace/website-test/script.js", { workdir: '/workspace' });
  const handIn = await (await workspace.request.post('/api/task/check', { data: { task: 'static-website/abgabe', answer: '' } })).json();
  expect(handIn.status, handIn.message).toBe('sent');

  const teacherContext = await browser.newContext({ viewport: { width: 1300, height: 850 } });
  const teacher = await teacherContext.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&activity=static-website');
  const answers = teacher.locator('details.class-answers', { hasText: 'Abgabe' });
  await answers.locator('summary').click();
  await answers.locator('a.cb-submission').first().click();
  const dialog = teacher.getByRole('dialog');
  await expect(dialog).toBeVisible();
  const frame = teacher.frameLocator('.subview-frame');
  await expect(frame.locator('#out')).toHaveText('JavaScript läuft', { timeout: 15_000 });
  await expect(frame.locator('body')).not.toContainText('Fehler');
  await dialog.getByRole('button', { name: 'Quelltext' }).click();
  await expect(dialog.locator('.subview-code')).toContainText('Mein Feuerwerk');
  await dialog.locator('.subview-file', { hasText: 'script.js' }).click();
  await expect(dialog.locator('.subview-code')).toContainText('JavaScript läuft');
  await teacher.keyboard.press('Escape');
  await expect(dialog).toHaveCount(0);
  await expect(teacher).toHaveURL(/\/klasse/);

  // A new round: the teacher deletes the group's hand-ins, after a warning.
  await answers.getByRole('button', { name: 'Alle Abgaben löschen' }).click();
  const warning = teacher.getByRole('dialog').filter({ hasText: 'Alle Abgaben löschen?' });
  await expect(warning).toContainText('nicht rückgängig');
  await warning.getByRole('button', { name: /Abgabe(n)? löschen/ }).click();
  await expect(teacher.locator('a.cb-submission')).toHaveCount(0);
  const states = await (await workspace.request.post('/api/task/states', { data: { activity: 'static-website' } })).json();
  expect(states.states.abgabe).toBeUndefined();
  await container.exec('rm -rf /workspace/website-test', { workdir: '/workspace' });
  await teacherContext.close();
});
