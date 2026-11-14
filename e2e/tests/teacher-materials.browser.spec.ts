import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Lehrmaterial as a list of teacher notes is gone (October 2026); teacher
// notes stand on top of each activity. /lehrmaterial shows the guides for
// teachers as cards (November 2026), only to teachers.

test('the profile has no list of teacher notes', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/profil');
  await expect(page.getByRole('heading', { name: 'Notizen für Lehrkräfte' })).toHaveCount(0);
});

test('designated teacher finds the guides as cards on /lehrmaterial', async ({ page, browser }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/lehrmaterial');
  expect(new URL(page.url()).pathname).toBe('/lehrmaterial');
  await expect(page.getByRole('heading', { name: 'Für Lehrkräfte', level: 1 })).toBeVisible();
  for (const slug of ['einstieg', 'klassenseite', 'lehrplan', 'git', 'leistungsueberpruefung', 'datenschutz']) {
    await expect(page.locator(`a.tutorial_card3[href="/lehrmaterial-${slug}"]`)).toBeVisible();
  }
  const broken: string[] = [];
  page.on('response', (r) => { if (r.status() >= 400 && /\.(webp|png|jpg)/.test(r.url())) broken.push(r.status() + ' ' + r.url()); });
  for (const slug of ['klassenseite', 'lehrplan', 'git', 'datenschutz']) {
    await page.goto(`/lehrmaterial-${slug}`);
    for (const img of await page.locator('img').all()) { await img.scrollIntoViewIfNeeded(); }
  }
  expect(broken).toEqual([]);
  // students get neither the cards nor a guide
  const s = await (await browser.newContext()).newPage();
  await loginAsE2eUser(s, 'e2e-0@example.com', testInfo);
  expect((await s.request.get('/lehrmaterial')).status()).toBe(403);
  expect((await s.request.get('/lehrmaterial-git')).status()).toBe(403);
  await s.context().close();
  await expect(page.getByRole('link', { name: 'Administration' })).toHaveCount(0);
  // A 403 without a body makes page.goto throw in Chromium; ask directly.
  const response = await page.request.get('/admin');
  expect(response.status()).toBe(403);
});

// Group administrators find login codes and their students' workspaces on
// the class page; the admin page is for administrators (October 2026).
test('group administrator keeps the class page, not the admin page', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/profil');
  await expect(page.locator('.menu').getByRole('link', { name: 'Klassen', exact: true })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Administration' })).toHaveCount(0);
  const response = await page.request.get('/admin');
  expect(response.status()).toBe(403);
});

// The guides at the foot of a teacher's start page, and »Neu hier?« at the
// top until dismissed (in this browser); students see neither (November 2026).
test('teachers find the guides on the start page', async ({ page, browser }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/');
  await page.evaluate(() => localStorage.removeItem('hackschule-teacher-guides-hint-hidden'));
  await page.reload();
  const hint = page.locator('.teacher-guides-hint');
  await expect(hint).toBeVisible();
  await expect(page.locator('#fuer-lehrkraefte a.tutorial_card3[href="/lehrmaterial-einstieg"]')).toBeAttached();
  await expect(page.locator('#fuer-lehrkraefte a.tutorial_card3')).toHaveCount(6);
  await page.screenshot({ path: testInfo.outputPath('start-top.png') });
  await page.locator('#fuer-lehrkraefte').scrollIntoViewIfNeeded();
  await page.screenshot({ path: process.env.E2E_SHOT || testInfo.outputPath('start-guides.png') });
  await hint.locator('.teacher-guides-hint-close').click();
  await page.reload();
  await expect(hint).toBeHidden();
  await expect(page.locator('#fuer-lehrkraefte')).toBeAttached();
  const s = await (await browser.newContext()).newPage();
  await loginAsE2eUser(s, 'e2e-0@example.com', testInfo);
  await s.goto('/');
  await expect(s.locator('.teacher-guides-hint, #fuer-lehrkraefte')).toHaveCount(0);
  await s.context().close();
});
