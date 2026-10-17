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
  await expect(say).toContainText('FROM country');
  await expect(stepper.locator('.qs-clause')).toHaveText(['FROM', 'WHERE', 'GROUP BY', 'HAVING', 'SELECT', 'ORDER BY']);
  await expect(stepper.locator('.qs-clause.qs-current')).toHaveText('FROM');
  await expect(stepper.locator('.qs-count')).toHaveText('Schritt 1 von 6');

  // Above it, the tables as schema lines: the key underlined, a foreign key with ↑
  const schema = stepper.locator('.qs-schema > div');
  await expect(schema).toHaveText(['continent(name, area)',
    'country(code, name, population, area, ↑continent, ↑currency_code, tld, calling_code, driving_side)',
    'city(id, name, ↑country_code, population, latitude, longitude, is_capital)', 'border(↑country_code, ↑neighbor_code)',
    'currency(code, name)', 'language(code, name)', 'country_language(↑country_code, ↑language_code)']);
  await expect(schema.first().locator('u')).toHaveText('name');
  await expect(schema.nth(1).locator('.schema-fk')).toHaveAttribute('title', 'verweist auf continent.name');

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
  await expect(stepper.locator('.qs-table thead th')).toHaveText(['continent', 'countries']);
  const counts = await stepper.locator('.qs-table tbody tr td:nth-child(2)').allTextContents();
  expect(counts.length).toBeGreaterThan(1);
  expect(counts.map(Number)).toEqual([...counts.map(Number)].sort((a, b) => b - a));

  // An example with a mistake: the alias from SELECT does not exist in WHERE yet
  await page.locator('.query-stepper-examples button', { hasText: 'Ein Fehler' }).click();
  await expect(say.locator('.qs-error')).toContainText('SELECT kommt nach WHERE');
  await expect(stepper.locator("[data-go='next']")).toBeDisabled();

  // Typing a query of one's own
  await stepper.locator('.qs-input').fill("SELECT name FROM country WHERE name LIKE 'D%' ORDER BY name LIMIT 2");
  await expect(stepper.locator('.qs-clause')).toHaveText(['FROM', 'WHERE', 'SELECT', 'ORDER BY', 'LIMIT']);
  await stepper.locator("[data-go='last']").click();
  await expect(stepper.locator('.qs-table tbody tr:not(.qs-drop) td')).toHaveText(['Dänemark', 'Demokratische Republik Kongo']);

  // An example on the movie tables switches the tables
  await page.locator('.query-stepper-examples button', { hasText: 'Filme: zwei JOINs' }).click();
  await expect(stepper.locator('.qs-dataset select')).toHaveValue('movies');
  await expect(schema).toHaveCount(6);
  await expect(schema.nth(2)).toHaveText('movie_genre(↑movie_id, ↑genre_id)');
  await expect(stepper.locator('.qs-clause')).toHaveText(['FROM', 'JOIN', 'WHERE', 'SELECT']);
  await stepper.locator("[data-go='last']").click();
  await expect(stepper.locator('.qs-table tbody tr td:nth-child(2)')).toHaveText(['Adventure', 'Comedy', 'Sci-Fi']);

  expect(errors).toEqual([]);
});
