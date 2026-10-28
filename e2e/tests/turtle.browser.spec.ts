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
  await page.goto('/turtle');
  for (const language of ['ruby', 'python', 'javascript'] as const) {
    await page.locator(`.code-language-option[data-set-language="${language}"]`).click();
    const square = page.locator('.task-card[data-task="turtle/square"]:visible');
    await expect(square.locator('.task-code')).toHaveAttribute('data-code-language', language);
    await square.locator('.turtle-speeds button', { hasText: 'Sofort' }).click();
    await square.locator('.task-code').fill(PROGRAMS[language][0]);
    await square.locator('.task-submit').click();
    await expect(square.locator('.task-feedback')).toContainText('Dein Bild stimmt.', { timeout: 60_000 });

    // a function, called twice from the start; then a triangle instead
    const fn = page.locator('.task-card[data-task="turtle/square-function"]:visible');
    await fn.locator('.task-code').fill(PROGRAMS[language][1]);
    await fn.locator('.task-submit').click();
    await expect(fn.locator('.task-feedback')).toContainText('Alle 2 Aufrufe zeichnen genau das Richtige.', { timeout: 60_000 });
    await fn.locator('.task-code').fill(PROGRAMS[language][1].replace('4', '3').replace('90', '120'));
    await fn.locator('.task-submit').click();
    await expect(fn.locator('.task-feedback')).toContainText('fehlt ein Stück', { timeout: 60_000 });
    await expect(fn.locator('.task-feedback')).toContainText('zu viel (rot)');
  }
  expect(errors).toEqual([]);
});

test('an endless program draws until it is stopped, and its code stays', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/turtle');
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  const card = page.locator('.task-card[data-task="turtle/square-loop"]:visible');
  await card.locator('.turtle-speeds button', { hasText: 'Normal' }).click();
  const endless = 'let n = 0;\nwhile (true) {\n  forward(n);\n  left(59);\n  n = (n + 3) % 240;\n}\n';
  await card.locator('.task-code').fill(endless);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-submit')).toContainText('Stopp');
  // the line the turtle is on is marked in the editor while it draws
  await expect(card.locator('.code-input-gutter .current, .code-input-mark:visible').first()).toBeVisible({ timeout: 15_000 });
  await page.waitForTimeout(1500);
  await card.locator('.task-submit').click();
  await expect(card.locator('.turtle-state')).toHaveText('Angehalten.');
  await expect(card.locator('.task-submit')).toContainText('Ausführen');
  await page.reload();
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  await expect(page.locator('.task-card[data-task="turtle/square-loop"]:visible .task-code')).toHaveValue(endless);
});
