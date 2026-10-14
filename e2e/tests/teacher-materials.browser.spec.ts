import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Lehrmaterial is gone (October 2026): the list of activities with a teacher
// note is in the profile, /lehrmaterial forwards there.

test('a student sees no teacher notes in the profile', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/profil');
  await expect(page.locator('#profile-teaching-materials')).toBeHidden();
});

test('designated teacher finds the teacher notes in the profile', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/lehrmaterial');
  expect(new URL(page.url()).pathname).toBe('/profil');
  const section = page.locator('#profile-teaching-materials');
  await expect(section).toBeVisible();
  await expect(section.locator('a[href="/sandbox-sortieren"]')).toBeVisible();
  await expect(page.getByRole('link', { name: 'Administration' })).toHaveCount(0);
  // A 403 without a body makes page.goto throw in Chromium; ask directly.
  const response = await page.request.get('/admin');
  expect(response.status()).toBe(403);
});

// Group administrators find login codes and their students' workspaces on
// the class page; the admin page is for administrators (October 2026).
test('group administrator keeps teacher notes and the class page, not the admin page', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/profil');
  await expect(page.locator('#profile-teaching-materials')).toBeVisible();
  await expect(page.getByRole('link', { name: 'Klassen', exact: true })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Administration' })).toHaveCount(0);
  const response = await page.request.get('/admin');
  expect(response.status()).toBe(403);
});
