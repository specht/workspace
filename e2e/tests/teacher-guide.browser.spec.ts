// »Neu hier?« on the class page leads to the guide for teachers (and names
// the Lehrplan); once hidden it stays hidden, the links next to the heading
// stay.
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

test('new teachers find the guide from the class page; students do not get it', async ({ page, browser }, testInfo) => {
  const broken: string[] = [];
  page.on('response', (r) => { if (r.status() >= 400 && /\.(webp|png|jpg)/.test(r.url())) broken.push(r.status() + ' ' + r.url()); });
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.setViewportSize({ width: 1100, height: 900 });
  await page.goto('/klasse?group=E2E');
  await page.evaluate(() => localStorage.removeItem('hackschule-class-welcome-hidden'));
  await page.reload();
  await expect(page.locator('.class-welcome')).toBeVisible();
  // the box links the guide and the Lehrplan
  await expect(page.locator('.class-welcome').getByRole('link', { name: 'Lehrplan' })).toHaveAttribute('href', '/lehrplan');
  await page.locator('.class-welcome').getByRole('link', { name: 'Einstieg für Lehrkräfte' }).click();
  await expect(page.getByRole('heading', { name: 'Einstieg für Lehrkräfte', level: 1 })).toBeVisible();
  for (const img of await page.locator('img').all()) { await img.scrollIntoViewIfNeeded(); }
  expect(broken).toEqual([]);
  await page.goto('/klasse?group=E2E');
  await page.locator('.class-welcome-close').click();
  await page.reload();
  await expect(page.locator('.class-welcome')).toBeHidden();
  await expect(page.locator('.class-help-link[href="/lehrmaterial-einstieg"]')).toBeVisible();
  await expect(page.locator('.class-help-link[href="/lehrplan"]')).toBeVisible();
  // Students do not get the guide.
  const s = await (await browser.newContext()).newPage();
  await loginAsE2eUser(s, 'e2e-0@example.com', testInfo);
  const r = await s.request.get('/lehrmaterial-einstieg');
  expect(r.status()).toBe(403);
});
