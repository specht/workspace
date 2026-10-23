import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The development page /person-sources (src/ruby/include/person_sources.rb):
// every sentence of the person boxes with its sources, for administrators.
// No E2E account is an administrator, so the first test logs in as the first
// address in ADMIN_USERS (src/ruby/credentials.rb), or E2E_ADMIN_EMAIL.

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL)
    return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(
    path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email)
    throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

test('an administrator sees every person box with its sources', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, adminEmail(), testInfo);
  await page.goto('/person-sources');
  await expect(page.locator('h1')).toHaveText('Belege für die Personen');
  // The Administration menu leads there, in development
  await expect(page.locator(".dropdown-menu a.dropdown-item[href='/person-sources']")).toHaveClass(/active/);
  expect(await page.locator('.ps-person').count()).toBeGreaterThan(50);
  const snellius = page.locator('.ps-person', { has: page.locator("a[href='/reference/personen#snellius']") });
  await expect(snellius.locator('.ps-sourced').first()).toBeVisible();
  await expect(snellius.locator('.ps-sources a').first()).toHaveAttribute('href', /^https:\/\//);

  // A filter keeps only the sentences in that state.
  await page.locator(".ps-filters button[data-filter='sourced']").click();
  await expect(page.locator('.ps-sentence:visible').first()).toHaveAttribute('data-state', 'sourced');
  expect(await page.locator(".ps-sentence:visible:not([data-state='sourced'])").count()).toBe(0);
});

test('a student does not get the page', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-0@example.com', testInfo);
  const response = await page.goto('/person-sources');
  expect(response?.status()).toBe(403);
});

// Readers get the sources too, behind a small info icon in each box
// (»Mehr dazu«, person_boxes.rb); a click elsewhere closes the list.
test('a reader opens the sources of a person box', async ({ page }) => {
  await page.goto('/raytracing');
  const box = page.locator('.person', { hasText: 'Willebrord Snellius' }).first();
  const list = box.locator('.person-sources-list');
  await expect(list).toBeHidden();
  await box.getByRole('group').locator('summary').click();
  await expect(list).toBeVisible();
  await expect(list.getByRole('link', { name: 'MacTutor: Snell' })).toHaveAttribute('href', /mathshistory\.st-andrews\.ac\.uk/);
  await page.locator('h1').click();
  await expect(list).toBeHidden();
});
