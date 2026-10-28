import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The turtle (TurtleCheck, docs/plan-katas.md, »Turtle«): the program runs in
// the browser, Ruby and Python as WebAssembly (fetch-turtle-runtimes.sh), and
// the turtle draws while it runs. A program that ends is checked on the
// server; one that is stopped keeps its code and the card's state.

const PROGRAMS = {
  ruby: ['4.times do\n  forward 100\n  left 90\nend\n',
    'def square(size)\n  4.times do\n    forward size\n    left 90\n  end\nend\n\nsquare(150)\n'],
  python: ['for i in range(4):\n    forward(100)\n    left(90)\n',
    'def square(size):\n    for i in range(4):\n        forward(size)\n        left(90)\n\nsquare(150)\n'],
  javascript: ['for (let i = 0; i < 4; i++) {\n  forward(100);\n  left(90);\n}\n',
    'function square(size) {\n  for (let i = 0; i < 4; i++) {\n    forward(size);\n    left(90);\n  }\n}\n\nsquare(150);\n'],
};

test('the turtle draws and is checked in Ruby, Python and JavaScript', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(180_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/turtle-1');
  for (const language of ['ruby', 'python', 'javascript'] as const) {
    await page.locator(`.code-language-option[data-set-language="${language}"]`).click();
    const square = page.locator('.task-card[data-task="turtle-1/square"]:visible');
    await expect(square.locator('.task-code')).toHaveAttribute('data-code-language', language);
    await square.locator('.turtle-speeds button', { hasText: 'Sofort' }).click();
    await square.locator('.task-code').fill(PROGRAMS[language][0]);
    await square.locator('.task-submit').click();
    await expect(square.locator('.task-feedback')).toContainText('Dein Bild stimmt.', { timeout: 60_000 });
    // solved: what it draws now is the student's own experiment, not »wrong«
    await square.locator('.task-code').fill(PROGRAMS[language][0].replace('100', '60'));
    await square.locator('.task-submit').click();
    await expect(square.locator('.task-feedback')).toContainText('frei ausprobieren', { timeout: 60_000 });
    await expect(square.locator('.task-feedback')).not.toContainText('fehlt');
    await expect(square).toHaveAttribute('data-status', 'correct');
    // nothing printed, nothing in the console (no messages of the runtime's start)
    await expect(square.locator('.task-console')).toBeHidden();
    // the commands at hand, as this language writes them
    await expect(square.locator('.turtle-commands')).toContainText(language === 'ruby' ? 'left 90' : 'left(90)');
    const dashes = page.locator('.task-card[data-task="turtle-1/dashes"]:visible');
    await expect(dashes.locator('.turtle-commands')).toContainText(language === 'javascript' ? 'penUp()' : language === 'python' ? 'pen_up()' : 'pen_up');

    // a function, called twice from the start: before it is solved (in the
    // first language), a triangle instead shows what differs
    const fn = page.locator('.task-card[data-task="turtle-1/square-function"]:visible');
    if (language === 'ruby') {
      // solved in an earlier run of this test: a solved card only says
      // »frei ausprobieren«, so the answer goes first
      if (await fn.locator('.task-delete').isVisible()) {
        await fn.locator('.task-delete').click();
        await page.locator('#__template_modal').getByRole('button', { name: 'Löschen', exact: true }).click();
        await expect(fn.locator('.task-feedback')).toContainText('Gelöscht.');
      }
      await fn.locator('.task-code').fill(PROGRAMS[language][1].replace('4', '3').replace('90', '120'));
      await fn.locator('.task-submit').click();
      // a picture on its way: one quiet sentence, the differences on request
      await expect(fn.locator('.task-feedback')).toContainText('Bleistiftlinie', { timeout: 60_000 });
      await expect(fn.locator('.task-feedback')).not.toContainText('fehlt');
      await fn.locator('.turtle-compare').click();
      await expect(fn.locator('.task-feedback')).toContainText('fehlt ein Stück');
      await expect(fn.locator('.task-feedback')).toContainText('zu viel (rot)');
    }
    await fn.locator('.task-code').fill(PROGRAMS[language][1]);
    await fn.locator('.task-submit').click();
    await expect(fn.locator('.task-feedback')).toContainText('Alle 2 Aufrufe zeichnen genau das Richtige.', { timeout: 60_000 });
  }
  expect(errors).toEqual([]);
});

test('an endless program draws until it is stopped, and its code stays', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/turtle-1');
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  const card = page.locator('.task-card[data-task="turtle-1/square-loop"]:visible');
  await card.locator('.turtle-speeds button', { hasText: 'Normal' }).click();
  const endless = 'let n = 0;\nwhile (true) {\n  forward(n);\n  left(59);\n  n = (n + 3) % 240;\n}\n';
  await card.locator('.task-code').fill(endless);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-submit')).toContainText('Stopp');
  // the line the turtle is on is marked in the editor while it draws
  await expect(card.locator('.code-input-gutter .current, .code-input-mark:visible').first()).toBeVisible({ timeout: 15_000 });
  // the speed changes while it runs, on every turtle card of the page
  await card.locator('.turtle-speeds button', { hasText: 'Langsam' }).click();
  await expect(page.locator('.task-card[data-task="turtle-1/square"]:visible .turtle-speeds button.active')).toHaveText('Langsam');
  await page.waitForTimeout(800);
  await card.locator('.turtle-speeds button', { hasText: 'Sofort' }).click();
  await page.waitForTimeout(800);
  await expect(card.locator('.task-submit')).toContainText('Stopp');
  await card.locator('.task-submit').click();
  await expect(card.locator('.turtle-state')).toHaveText('Angehalten.');
  await expect(card.locator('.task-submit')).toContainText('Ausführen');
  await page.reload();
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  await expect(page.locator('.task-card[data-task="turtle-1/square-loop"]:visible .task-code')).toHaveValue(endless);
});

test('a free card keeps several drawings by name, hands them in, renames and deletes them', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(180_000);
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  page.on('dialog', (d) => { errors.push('native dialog: ' + d.message()); d.dismiss(); });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/turtle-3');
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  const card = page.locator('.task-card[data-task="turtle-3/finale"]:visible');
  await card.scrollIntoViewIfNeeded();
  const select = card.locator('.turtle-sketches select');
  // from an earlier run of this test: back to one empty drawing
  await expect(select.locator('option').first()).toBeAttached();
  while (await select.locator('option').count() > 1 || !(await select.inputValue()).startsWith('Bild')) {
    await card.locator(".turtle-sketches [data-do='delete']").click();
    await page.locator('#__template_modal').getByRole('button', { name: 'Löschen', exact: true }).click();
    await page.waitForTimeout(400);
  }
  await card.locator('.turtle-speeds button', { hasText: 'Sofort' }).click();
  const sun = 'for (let i = 0; i < 36; i++) {\n  color(i * 10);\n  forward(100);\n  back(100);\n  left(10);\n}\n';
  await card.locator('.task-code').fill(sun);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText(/Fertig|frei ausprobieren/, { timeout: 60_000 });

  // hand it in: the picture of the last run
  const first = await select.inputValue();
  await card.locator('.task-hand-in').click();
  await expect(card.locator('.task-hand-in')).toHaveText('Eingereicht, zurücknehmen');
  await expect(select.locator('option', { hasText: '(eingereicht)' })).toHaveCount(1);

  // a second drawing by name, with an empty program
  await card.locator(".turtle-sketches [data-do='new']").click();
  await page.locator('#ti_input_dialog').fill('Wald');
  await page.locator('#__template_modal').getByRole('button', { name: 'Anlegen' }).click();
  await expect(select).toHaveValue('Wald');
  await expect(card.locator('.task-code')).not.toHaveValue(sun);
  await expect(card.locator('.task-hand-in')).toHaveText('Einreichen');
  await card.locator('.task-code').fill('forward(50);\n');
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText(/Fertig|frei ausprobieren/, { timeout: 60_000 });

  // back to the first, renamed; the hand-in follows the name
  await select.selectOption(first);
  await expect(card.locator('.task-code')).toHaveValue(sun);
  await card.locator(".turtle-sketches [data-do='rename']").click();
  await page.locator('#ti_input_dialog').fill('Sonne');
  await page.locator('#__template_modal').getByRole('button', { name: 'Umbenennen' }).click();
  await expect(select.locator('option', { hasText: 'Sonne (eingereicht)' })).toHaveCount(1);

  // deleted, it is no longer handed in; Wald stays, also after a reload
  await card.locator(".turtle-sketches [data-do='delete']").click();
  await page.locator('#__template_modal').getByRole('button', { name: 'Löschen', exact: true }).click();
  await expect(select.locator('option')).toHaveText(['Wald']);
  await page.reload();
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  const again = page.locator('.task-card[data-task="turtle-3/finale"]:visible');
  await expect(again.locator('.turtle-sketches select')).toHaveValue('Wald');
  await expect(again.locator('.task-code')).toHaveValue('forward(50);\n');
  expect(errors).toEqual([]);
});
