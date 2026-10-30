import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Lehrmaterial is gone (October 2026). Teacher notes stand on top of each
// activity; the profile no longer lists them, and /lehrmaterial forwards to
// the onboarding for teachers.

test('the profile has no list of teacher notes', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/profil');
  await expect(page.getByRole('heading', { name: 'Notizen für Lehrkräfte' })).toHaveCount(0);
});

test('designated teacher reaches the onboarding from /lehrmaterial', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/lehrmaterial');
  expect(new URL(page.url()).pathname).toBe('/lehrmaterial-einstieg');
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
