import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The start page for students (StudentHome, docs/aufgaben.md »Startseite«):
// after a task is solved on its page, »Weiter mit« leads back to the first
// open task of that activity, and its catalog card shows how many are
// solved. Teacher accounts get neither.
test('a solved task shows up in »Weiter mit« and on the catalog card', async ({ page, e2eEmail }, testInfo) => {
  const task = 'rle/von-hand';
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task } });
  try {
    await page.goto('/rle');
    const card = page.locator('#aufgabe-von-hand');
    await card.locator("[name='answer']").fill('4A3B2C1D');
    await card.getByRole('button', { name: 'Prüfen' }).click();
    await expect(card.locator('.task-status')).toHaveText('gelöst');

    await page.goto('/');
    const next = page.locator('.student-continue-list a').first();
    await expect(next).toContainText('Komprimieren mit Lauflängen');
    await expect(next).toContainText(/\d von \d+ gelöst/);
    await expect(next).toHaveAttribute('href', /^\/rle#aufgabe-(?!von-hand)/);
    await expect(page.locator('.student-howto')).toHaveCount(0);

    // The catalog cards show no progress, also for a started activity.
    await expect(page.locator("a.tutorial_card3[href='/rle'] .card-progress")).toHaveCount(0);
  } finally {
    await page.request.post('/api/task/delete', { data: { task } });
  }
});

test('activities name what they build on, on the page and on the card', async ({ page }) => {
  await page.goto('/raytracing');
  const strip = page.locator('.page-kind-requires');
  await expect(strip).toContainText('Baut auf Echtzeitgrafik mit C auf');
  await expect(strip.getByRole('link', { name: 'Echtzeitgrafik mit C' })).toHaveAttribute('href', '/pixelram');
  // How long it takes is for teachers.
  await expect(page.locator('.page-kind-duration')).toHaveCount(0);

  await page.goto('/');
  await expect(page.locator("a.tutorial_card3[href='/terminal-mystery'] .card-requires"))
    .toHaveText('Baut auf Dateien und Verzeichnisse und Versionsverwaltung mit Git auf');
  await expect(page.locator("a.tutorial_card3[href='/pixelram'] .card-requires")).toHaveCount(0);
});

test('teacher accounts get no student parts on the start page', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/');
  await expect(page.locator('.tutorial_card3').first()).toBeVisible();
  await expect(page.locator('.student-continue, .student-howto, .card-progress')).toHaveCount(0);
});
