import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The Telefonbuch's management (/telefon-freigabe): a published game's title
// and subtitle edited, the game hidden from the book (a part of another
// game) and shown again. Needs a published game on the site (the
// development site has one); everything is put back at the end.

function adminEmail(): string {
  if (process.env.E2E_ADMIN_EMAIL) return process.env.E2E_ADMIN_EMAIL;
  const credentials = fs.readFileSync(path.resolve(process.cwd(), '..', 'src', 'ruby', 'credentials.rb'), 'utf8');
  const email = credentials.match(/^ADMIN_USERS\s*=\s*\[\s*['"]([^'"]+)['"]/m)?.[1];
  if (!email) throw new Error('No ADMIN_USERS in credentials.rb; set E2E_ADMIN_EMAIL');
  return email;
}

const book = async (page: import('@playwright/test').Page) => {
  await page.goto('/telefonbuch');
  return page.evaluate(() => ((window as any).PHONE_BOOK || []).map((g: any) => `${g.number} ${g.title}`));
};

test('the Telefonbuch is managed: titles edited, games hidden and shown', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, adminEmail(), testInfo);
  await page.goto('/telefon-freigabe');
  await expect(page.locator('h1')).toHaveText('Telefonbuch');
  const row = page.locator('.phone-manage-row').first();
  test.skip(await row.count() === 0, 'no published game on this site');
  const number = (await row.locator('.phone-manage-number b').textContent())!.trim();
  const title = row.locator('.phone-manage-title');
  const original = await title.inputValue();

  // a new title, saved
  await expect(row.locator('.phone-manage-save')).toBeDisabled();
  await title.fill(original + ' (neu)');
  await row.locator('.phone-manage-save').click();
  await expect(row.locator('.phone-manage-state')).toHaveText('Gespeichert.');
  expect(await book(page)).toContain(`${number} ${original} (neu)`);

  // hidden: not in the book any more
  await page.goto('/telefon-freigabe');
  await row.locator('.phone-manage-shown').uncheck();
  await expect(row.locator('.phone-manage-state')).toContainText('Ausgeblendet');
  expect((await book(page)).some((e: string) => e.startsWith(`${number} `))).toBe(false);

  // back as it was
  await page.goto('/telefon-freigabe');
  await row.locator('.phone-manage-shown').check();
  await expect(row.locator('.phone-manage-state')).toHaveText('Steht im Telefonbuch.');
  await title.fill(original);
  await row.locator('.phone-manage-save').click();
  await expect(row.locator('.phone-manage-state')).toHaveText('Gespeichert.');
  expect(await book(page)).toContain(`${number} ${original}`);
});
