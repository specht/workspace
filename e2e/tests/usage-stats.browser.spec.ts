import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// What is used at which school, without names (src/ruby/include/usage_stats.rb):
// the administration reads /statistik with its bars and the weekly grid
// and finds it in its menu, a student gets 403. The numbers and the threshold of five are tested in Ruby
// (usage_stats_test.rb). HS_SHOTS=dir keeps pictures, desktop and phone.

const shots = process.env.HS_SHOTS;

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email) throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

test('the administration sees what is used, without names; a student does not', async ({ browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  const errors: string[] = [];
  const adminContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const admin = await adminContext.newPage();
  admin.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(admin, adminEmail(), testInfo);
  await admin.goto('/statistik');
  await expect(admin.locator('h1')).toHaveText('Nutzung');
  for (const heading of ['Verlauf', 'Wochenrhythmus', 'Klassen', 'Stolpersteine', 'Schulen', 'Aktivitäten', 'Server']) {
    await expect(admin.locator('h2', { hasText: heading })).toBeVisible();
  }
  // the students' strip of the course: a bar per day, its number above it
  // under the pointer; the weekly grid of the students: 7 x 24 hours
  const columns = admin.locator('.usage-stack-row').first().locator('.usage-col');
  expect(await columns.count()).toBe(30);
  await columns.last().hover();
  await expect(admin.locator('.usage-tip')).toBeVisible();
  await expect(admin.locator('.usage-tip')).toHaveText(/\d+\.\d+\.: /);
  await expect(admin.locator('.usage-rhythms section').first().locator('.usage-cell')).toHaveCount(7 * 24);
  // another window
  await admin.locator('.usage-windows a', { hasText: '7 Tage' }).click();
  await expect(admin).toHaveURL(/tage=7/);
  await expect(admin.locator(".usage-windows a[aria-current='page']")).toHaveText('7 Tage');
  // no address anywhere on the page
  expect(await admin.locator('main, body').first().innerText()).not.toMatch(/@/);
  if (shots) {
    fs.mkdirSync(shots, { recursive: true });
    await admin.goto('/statistik?tage=90');
    await admin.screenshot({ path: path.join(shots, 'statistik-desktop.png'), fullPage: true });
    await admin.setViewportSize({ width: 400, height: 900 });
    await admin.screenshot({ path: path.join(shots, 'statistik-phone.png'), fullPage: true });
  }
  // the menu links the page
  await admin.goto('/');
  await admin.getByRole('button', { name: 'Administration' }).click();
  await expect(admin.locator(".dropdown-menu a[href='/statistik']")).toBeVisible();
  expect(errors).toEqual([]);

  const studentContext = await browser.newContext({ baseURL: testInfo.project.use.baseURL });
  const student = await studentContext.newPage();
  await loginAsE2eUser(student, e2eEmail, testInfo);
  const page = await student.goto('/statistik');
  expect(page?.status()).toBe(403);
});
