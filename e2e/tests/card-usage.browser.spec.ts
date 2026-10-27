import fs from 'fs';
import path from 'path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The band at the foot of the catalog's cards (CardUsage, Main#usage_html):
// a student sees their own tasks on the cards they began, a teacher how
// many of their students solved each task. HS_SHOTS=dir also keeps
// screenshots of both, light and dark.

const shots = process.env.HS_SHOTS;

async function keep(page, name: string) {
  if (!shots) return;
  fs.mkdirSync(shots, { recursive: true });
  for (const theme of ['light', 'dark']) {
    await page.evaluate((t) => document.documentElement.setAttribute('data-bs-theme', t), theme);
    await page.waitForTimeout(200);
    await page.locator("a.tutorial_card3[href='/crypto']").screenshot({ path: path.join(shots, `${name}-${theme}.png`) });
  }
  await page.evaluate(() => document.documentElement.setAttribute('data-bs-theme', 'light'));
}

test('a student sees their own tasks on the card of an activity they began', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/crypto');
  await states;
  const card = page.locator(".task-card[data-task='crypto/fingerabdruck']");
  await card.getByText('Ein Hash des öffentlichen Schlüssels', { exact: true }).click();
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct');

  await page.goto('/');
  const band = page.locator("a.tutorial_card3[href='/crypto'] .usage-own");
  await expect(band).toBeVisible();
  await expect(band.locator('.shc-mark.shc-done').first()).toHaveAttribute('title', /: gelöst$/);
  await expect(band.locator('.usage-own-bar')).toHaveAttribute('aria-label', /^Du hast \d+ von \d+ Aufgaben gelöst$/);
  // no admin or teacher numbers for a student
  await expect(page.locator('.usage-strip')).toHaveCount(0);
  await keep(page, 'usage-student');
});

test('a teacher sees how many of their students solved each task', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/');
  const band = page.locator("a.tutorial_card3[href='/crypto'] .usage-row");
  await expect(band).toBeVisible();
  await expect(band.locator('.usage-bar').first()).toHaveAttribute('title', /^1\. .+: \d+ von \d+ deiner Lernenden gelöst$/);
  await expect(band.locator('.usage-strip')).toHaveAttribute('aria-label', /^Deine Lernenden: \d+ von \d+ haben hier etwas gelöst/);
  await expect(page.locator('.usage-own')).toHaveCount(0);
  await keep(page, 'usage-teacher');
});

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email) throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

test('an admin sees everybody\'s numbers, as before', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, adminEmail(), testInfo);
  await page.goto('/');
  const band = page.locator("a.tutorial_card3[href='/crypto'] .usage-row");
  await expect(band).toBeVisible();
  await expect(band.locator('.usage-bar').first()).toHaveAttribute('title', /^1\. .+: \d+-mal gelöst$/);
  await expect(band.locator('.usage')).toHaveAttribute('title', /ohne Lehrkräfte, Admins und Testkonten$/);
  await keep(page, 'usage-admin');
});
