import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The two widgets of »Laufzeit« and »Prolog«: the growth table
// (growth_widget.js) follows its slider and quick buttons, and the Prolog
// search (prolog_search.js) steps with its buttons and the arrow keys,
// marking the clause of the step in the program. The search engine itself
// has tests of its own (test/static/prolog_search.test.cjs).

test('the growth table follows the quick buttons', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/laufzeit');

  const widget = page.locator('.growth-widget');
  await expect(widget.locator('.growth-name')).toHaveCount(7);
  await widget.locator('.growth-quick button', { hasText: /^20$/ }).click();
  await expect(widget.locator('.growth-n')).toHaveText('20');
  await widget.locator('.growth-quick button', { hasText: /^100$/ }).click();
  await expect(widget.locator('.growth-n')).toHaveText('100');
  // 2ⁿ and n! at n = 100 take longer than a year: their bars turn red
  await expect(widget.locator('.growth-too-long')).toHaveCount(2);
  expect(errors).toEqual([]);
});

test('the Prolog search steps through the grandparents of Wilhelm Friedemann', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/prolog');

  const search = page.locator('.prolog-search');
  const say = search.locator('.prolog-search-say');
  await expect(say).toContainText('Die Anfrage ist das erste Ziel');
  await expect(search.locator('.prolog-search-program li.current')).toHaveCount(0);

  await search.locator("[data-go='next']").click();
  await expect(say).toContainText('passt zur Regel');
  await expect(search.locator('.prolog-search-program li.current')).toContainText('grandparent(G, C) :-');
  await expect(search.locator('.prolog-goal')).toHaveText(['parent(G₁, P₁)', 'parent(P₁, wilhelm_friedemann)']);

  // The arrow keys step it, as every stepper on the site
  await search.locator('.prolog-search-title').click();
  await page.keyboard.press('ArrowRight');
  await expect(say).toContainText('passt zum Fakt in Zeile 1');
  await expect(search.locator('.prolog-choice')).not.toHaveCount(0);

  await search.locator("[data-go='last']").click();
  await expect(say).toContainText('Kein Merkpunkt mehr');
  await expect(search.locator('.prolog-answer')).toHaveText(['G = johann_ambrosius', 'G = maria_elisabeth', 'G = johann_michael']);
  await expect(search.locator("[data-go='next']")).toBeDisabled();
  expect(errors).toEqual([]);
});
