import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// »Nextcloud der Schule« (NextcloudLink): a teacher names the school's
// Nextcloud on the class page, the Workspace asks it for status.php and keeps
// it only if one answers; the document cards of the group's students then
// take a share link from there, and say what is wrong with one that does not
// work. Uses cloud.nextcloud.com as a real Nextcloud that answers.
test('a class with a Nextcloud checks documents behind a share link', async ({ browser }, testInfo) => {
  const teacher = await (await browser.newContext()).newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  const student = await (await browser.newContext()).newPage();
  await loginAsE2eUser(student, 'e2e-0@example.com', testInfo);

  try {
    await teacher.goto('/klasse?group=E2E&tab=einstellungen');
    const field = teacher.getByLabel('Nextcloud der Schule');
    const state = teacher.locator('.class-nextcloud-state');
    // only https, and only a Nextcloud that answers
    await field.fill('http://cloud.nextcloud.com');
    await teacher.getByRole('button', { name: 'Prüfen und speichern' }).click();
    await expect(state).toContainText('https://');
    await field.fill('https://example.org');
    await teacher.getByRole('button', { name: 'Prüfen und speichern' }).click();
    await expect(state).toContainText('antwortet keine Nextcloud', { timeout: 30000 });
    await field.fill('cloud.nextcloud.com/apps/files/');
    await teacher.getByRole('button', { name: 'Prüfen und speichern' }).click();
    await expect(state).toContainText('Eingetragen: https://cloud.nextcloud.com (Nextcloud', { timeout: 30000 });
    await expect(field).toHaveValue('https://cloud.nextcloud.com');
    await teacher.screenshot({ path: testInfo.outputPath('class-nextcloud.png'), fullPage: false });

    await student.goto('/formatting-marks');
    const card = student.locator(".task-card[data-type='document']").first();
    const link = card.getByLabel(/Link zum Teilen aus eurer Nextcloud \(cloud\.nextcloud\.com\)/);
    await expect(link).toBeVisible();
    await card.scrollIntoViewIfNeeded();
    // a link of another server: refused before anything is fetched
    await link.fill('https://andere.example.org/s/AbCdEfGh1234567');
    await card.getByRole('button', { name: 'Prüfen', exact: true }).click();
    await expect(card.locator('.task-feedback')).toContainText('kein Link aus eurer Nextcloud');
    // a share that does not exist (any more)
    await link.fill('https://cloud.nextcloud.com/s/AbCdEfGh1234567');
    await card.getByRole('button', { name: 'Prüfen', exact: true }).click();
    await expect(card.locator('.task-feedback')).toContainText('gibt es in eurer Nextcloud nicht', { timeout: 30000 });
    await card.screenshot({ path: testInfo.outputPath('card-nextcloud.png') });

    // taken away: the cards take files only
    await teacher.getByRole('button', { name: 'Nextcloud entfernen' }).click();
    await expect(state).toContainText('Keine Nextcloud eingetragen.');
    await student.reload();
    await expect(student.locator(".task-card[data-type='document']").first().locator('.document-drop')).toBeVisible();
    await expect(student.locator('.document-link')).toHaveCount(0);
  } finally {
    await teacher.request.post('/api/set_group_nextcloud', { data: { group: 'E2E', url: '' } });
    await teacher.context().close();
    await student.context().close();
  }
});
