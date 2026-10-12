import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

test('a student cannot see or open teacher material', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await expect(page.locator('.menu a[href="/lehrmaterial"]')).toHaveCount(0);
  await page.goto('/profil');
  await expect(page.getByRole('link', { name: 'Lehrmaterial öffnen' })).toHaveCount(0);
  const response = await page.goto('/lehrmaterial');
  expect(response?.status()).toBe(403);
});

test('designated teacher sees materials but cannot administer a group', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await expect(page.locator('.menu a[href="/lehrmaterial"]')).toHaveCount(0);
  await page.goto('/profil');
  await expect(page.getByRole('link', { name: 'Lehrmaterial öffnen' })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Administration' })).toHaveCount(0);
  await page.goto('/lehrmaterial');
  for (const name of ['Sorting Sandbox', 'Data Structures Sandbox', 'CPU Sandbox']) {
    await expect(page.getByRole('heading', { name })).toBeVisible();
  }
  await expect(page.getByRole('link', { name: 'Repository und Anleitung' })).toHaveCount(3);
  const response = await page.goto('/admin');
  expect(response?.status()).toBe(403);
});

test('group administrator retains teaching materials and administration', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await expect(page.locator('.menu a[href="/lehrmaterial"]')).toHaveCount(0);
  await page.goto('/profil');
  await expect(page.getByRole('link', { name: 'Lehrmaterial öffnen' })).toBeVisible();
  await expect(page.getByRole('link', { name: 'Administration' })).toBeVisible();
  const response = await page.goto('/lehrmaterial');
  expect(response?.status()).toBe(200);
});
