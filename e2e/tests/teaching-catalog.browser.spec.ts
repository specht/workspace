import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

test('students cannot access teacher catalog or articles', async ({
  page, e2eEmail,
}, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/');
  await expect(page.locator('a[href="/lehrmaterial"]')).toBeHidden();
  await expect(page.locator('a[href="/lehrmaterial-sortieren"]')).toHaveCount(0);

  for (const path of ['/lehrmaterial', '/lehrmaterial.html', '/lehrmaterial-sortieren',
                      '/lehrmaterial-datenstrukturen', '/lehrmaterial-cpu']) {
    const response = await page.goto(path);
    expect(response?.status(), path).toBe(403);
  }
});

test('designated teacher sees separate catalog without group admin access', async ({
  page,
}, testInfo) => {
  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/');
  await expect(page.locator('a[href="/lehrmaterial"]')).toBeVisible();
  await expect(page.locator('a[href="/admin"]')).toBeHidden();
  await expect(page.locator('a[href="/lehrmaterial-sortieren"]')).toHaveCount(0);

  await page.goto('/lehrmaterial');
  await expect(page.getByRole('heading', { name: 'Material für Lehrkräfte' })).toBeVisible();
  for (const slug of ['lehrmaterial-sortieren', 'lehrmaterial-datenstrukturen',
                      'lehrmaterial-cpu']) {
    await expect(page.locator(`a.tutorial_card3[href="/${slug}"]`)).toBeVisible();
    const response = await page.goto(`/${slug}`);
    expect(response?.status(), slug).toBe(200);
  }
});
