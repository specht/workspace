import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The query stepper (query_stepper.js) on its tool page: it loads the
// tables, shows the clauses in the order the database works through them,
// steps with the arrow keys and explains a mistake instead of a result.
// The engine itself has tests of its own (test/static/query_stepper.test.cjs).

test('the query stepper works through a query clause by clause', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/query-stepper');

  const stepper = page.locator('.query-stepper');
  const say = stepper.locator('.qs-say');
  await expect(say).toContainText('FROM land');
  await expect(stepper.locator('.qs-clause')).toHaveText(['FROM', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'ORDER BY']);
  await expect(stepper.locator('.qs-clause.qs-current')).toHaveText('FROM');
  await expect(stepper.locator('.qs-count')).toHaveText('Schritt 1 von 6');

  // The arrow keys step it, as every stepper on the site
  await page.locator('h2').click();
  await page.keyboard.press('ArrowRight');
  await expect(stepper.locator('.qs-clause.qs-current')).toHaveText('WHERE');
  await expect(stepper.locator('tr.qs-drop').first()).toBeVisible();
  await page.keyboard.press('ArrowRight');
  await expect(stepper.locator('.qs-clause.qs-current')).toHaveText('GROUP BY');
  await expect(stepper.locator('tr.qs-group0').first()).toBeVisible();

  await stepper.locator("[data-go='last']").click();
  await expect(stepper.locator('.qs-clause.qs-current')).toHaveText('ORDER BY');
  await expect(stepper.locator('.qs-table thead th')).toHaveText(['kontinent', 'laender']);
  const counts = await stepper.locator('.qs-table tbody tr td:nth-child(2)').allTextContents();
  expect(counts.length).toBeGreaterThan(1);
  expect(counts.map(Number)).toEqual([...counts.map(Number)].sort((a, b) => b - a));

  // An example with a mistake: the alias from SELECT does not exist in WHERE yet
  await page.locator('.query-stepper-examples button', { hasText: 'Ein Fehler' }).click();
  await expect(say.locator('.qs-error')).toContainText('SELECT kommt nach WHERE');
  await expect(stepper.locator("[data-go='next']")).toBeDisabled();

  // Typing a query of one's own
  await stepper.locator('.qs-input').fill("SELECT name FROM land WHERE name LIKE 'D%' ORDER BY name LIMIT 2");
  await expect(stepper.locator('.qs-clause')).toHaveText(['FROM', 'WHERE', 'SELECT', 'ORDER BY', 'LIMIT']);
  await stepper.locator("[data-go='last']").click();
  await expect(stepper.locator('.qs-table tbody tr:not(.qs-drop) td')).toHaveText(['Dänemark', 'Deutschland']);

  expect(errors).toEqual([]);
});
