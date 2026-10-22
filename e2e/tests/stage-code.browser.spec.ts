import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// At each step of an activity, teachers get a small button that copies the
// whole code of that step (src/ruby/include/stage_code.rb); students and
// visitors get neither the button nor the code.

test('a teacher copies the whole program of a step', async ({ page, context }, testInfo) => {
  await context.grantPermissions(['clipboard-read', 'clipboard-write']);
  await loginAsE2eUser(page, 'e2e-teacher@example.com', testInfo);
  await page.goto('/raytracing');
  const buttons = page.locator('.stage-code-copy');
  expect(await buttons.count()).toBeGreaterThanOrEqual(15);
  const first = buttons.first();
  await first.scrollIntoViewIfNeeded();
  await expect(first).toBeVisible();
  await expect(first).toHaveText('Code bis hier');
  await first.click();
  await expect(first).toHaveClass(/copied/);
  const copied = await page.evaluate(() => navigator.clipboard.readText());
  expect(copied).toContain('#include "pixelram.h"');
  expect(copied).toContain('int main(void)');
});

test('a student gets neither the buttons nor the code', async ({ page }, testInfo) => {
  await loginAsE2eUser(page, 'e2e-0@example.com', testInfo);
  await page.goto('/raytracing');
  await expect(page.locator('h1')).toBeVisible();
  await expect(page.locator('.stage-code-copy')).toHaveCount(0);
  const html = await page.content();
  expect(html).not.toContain('class="stage-code"');
  expect(html).not.toContain('Code bis hier');
});

test('a visitor gets neither the buttons nor the code', async ({ page }) => {
  await page.goto('/raytracing');
  await expect(page.locator('h1')).toBeVisible();
  await expect(page.locator('.stage-code-copy')).toHaveCount(0);
  const html = await page.content();
  expect(html).not.toContain('class="stage-code"');
  expect(html).not.toContain('Code bis hier');
});
