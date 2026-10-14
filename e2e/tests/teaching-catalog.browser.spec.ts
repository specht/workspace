import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The teacher pages under Lehrmaterial became part of their sandbox pages
// (October 2026); their old addresses forward, for everyone.
const MOVED = {
  '/lehrmaterial-sortieren': '/sandbox-sortieren',
  '/lehrmaterial-datenstrukturen': '/sandbox-datenstrukturen',
  '/lehrmaterial-cpu': '/sandbox-cpu',
};

test('old teacher pages forward to their sandbox pages', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const [from, to] of Object.entries(MOVED)) {
    const response = await page.goto(from);
    expect(response?.status(), from).toBe(200);
    expect(new URL(page.url()).pathname, from).toBe(to);
  }
});

test('the teacher note on a sandbox page is for teachers only', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/sandbox-sortieren');
  await expect(page.locator('details.teacher-note')).toHaveCount(0);

  await loginAsE2eUser(page, 'e2e-materials-teacher@example.com', testInfo);
  await page.goto('/sandbox-sortieren');
  await expect(page.locator('details.teacher-note')).toHaveCount(1);
});
