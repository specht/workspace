import { expect, test } from './fixtures';
import type { Locator } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// A Kata (KataCheck, docs/plan-katas.md): code typed into the card, run in a
// container of its own. »Ausführen« shows what the program prints,
// »Prüfen« calls the functions and compares; the code comes back after a
// reload. The page /kata-demo exists in development only. The visible card
// is in the person's language (Ruby or Python), so each test writes in that.

async function languageOf(card: Locator): Promise<'ruby' | 'python'> {
  return (await card.locator('.task-code').getAttribute('data-code-language')) as 'ruby' | 'python';
}

test('a Kata runs, checks and keeps its code', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-demo');
  const card = page.locator('.task-card[data-task="kata-demo/greet"]:visible');
  const field = card.locator('.task-code');
  await expect(field).toHaveValue(/def greet\(name\)/);
  await expect(card.locator('.task-level .bi')).toHaveCount(1);

  // Enter keeps the indentation and adds one after def (or a colon);
  // Shift+Tab takes one back.
  const ruby = (await languageOf(card)) === 'ruby';
  await field.fill('');
  await field.focus();
  await page.keyboard.type(ruby ? 'def greet(name)' : 'def greet(name):');
  await page.keyboard.press('Enter');
  await page.keyboard.type(ruby ? '"Hallo, #{name}!"' : "return f'Hallo, {name}!'");
  await page.keyboard.press('Enter');
  await page.keyboard.press('Shift+Tab');
  if (ruby) {
    await page.keyboard.type('end');
    await page.keyboard.press('Enter');
  }
  await page.keyboard.type(ruby ? "puts greet('Ada')" : "print(greet('Ada'))");
  await expect(field).toHaveValue(ruby
    ? 'def greet(name)\n  "Hallo, #{name}!"\nend\nputs greet(\'Ada\')'
    : "def greet(name):\n    return f'Hallo, {name}!'\nprint(greet('Ada'))");
  const view = card.locator('.code-input-view');
  await expect(view.locator('.nf')).toHaveText('greet');

  await card.locator('.task-run').click();
  await expect(card.locator('.task-console')).toHaveText('Hallo, Ada!\n', { timeout: 30_000 });

  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Alle 3 Aufrufe liefern, was sie sollen.', { timeout: 30_000 });
  await expect(card).toHaveAttribute('data-status', 'correct');

  await page.reload();
  await expect(card.locator('.task-code')).toHaveValue(ruby ? /"Hallo, #\{name\}!"/ : /f'Hallo, \{name\}!'/);
});

test('a typical wrong Kata gets its own feedback', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-demo');
  const card = page.locator('.task-card[data-task="kata-demo/ggt"]:visible');
  await card.locator('.task-code').fill((await languageOf(card)) === 'ruby' ? 'def gcd(a, b) = [a, b].min\n' : 'def gcd(a, b):\n    return min(a, b)\n');
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Die kleinere Zahl ist nur manchmal ein Teiler der größeren.', { timeout: 30_000 });
});

test('an Extra is marked and counts for nobody', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-demo');
  const card = page.locator('.task-card[data-task="kata-demo/ziffern"]:visible');
  await expect(card.locator('.task-extra')).toHaveText('Extra');
  for (const scheme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: scheme });
    await card.locator('.task-head').screenshot({ path: testInfo.outputPath(`extra-${scheme}.png`) });
  }
});

test('JavaScript on a Kata page, Ruby or Python elsewhere', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/automata');
  const before = await page.locator('[data-code-language]').first().getAttribute('data-code-language');
  await page.goto('/kata-functions');
  await page.locator('.code-language-option[data-set-language="javascript"]').click();
  await page.locator('.code-language-switch').screenshot({ path: testInfo.outputPath('switch.png') });
  const card = page.locator('.task-card[data-task="kata-functions/greet"]:visible');
  await expect(card.locator('.task-code')).toHaveAttribute('data-code-language', 'javascript');
  await card.locator('.task-code').fill("function greet(name) {\n  return `Hallo, ${name}!`;\n}\n\nconsole.log(greet('Ada'));\n");
  await card.locator('.task-run').click();
  await expect(card.locator('.task-console')).toHaveText('Hallo, Ada!\n', { timeout: 30_000 });
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Alle 3 Aufrufe liefern, was sie sollen.', { timeout: 30_000 });
  // a page without JavaScript keeps the language from before
  await page.goto('/automata');
  await expect(page.locator('[data-code-language]').first()).toHaveAttribute('data-code-language', before!);
  // back to it on the Kata page for the other tests
  await page.goto('/kata-functions');
  await page.locator(`.code-language-option[data-set-language="${before}"]`).click();
});

test('a solved Kata is handed in and shown in class without the name', async ({ browser, page, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-functions');
  const card = page.locator('.task-card[data-task="kata-functions/double"]:visible');
  const handIn = card.locator('.task-hand-in');
  const ruby = (await languageOf(card)) === 'ruby';
  await card.locator('.task-code').fill(ruby ? 'def double(n)\n  n * 2\nend\n' : 'def double(n):\n    return n * 2\n');
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct', { timeout: 30_000 });
  await expect(handIn).toHaveText('Einreichen');
  await handIn.click();
  await expect(handIn).toHaveText('Eingereicht · Zurücknehmen');
  await page.reload();
  await expect(card.locator('.task-hand-in')).toHaveText('Eingereicht · Zurücknehmen');

  const teacher = await browser.newPage();
  await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
  await teacher.goto('/klasse?group=E2E&tab=fortschritt&activity=kata-functions');
  const box = teacher.locator('.kata-hand-ins');
  await expect(box.locator('summary')).toContainText('eingereicht');
  await box.locator('summary').click();
  await expect(box.locator('.kata-hand-in-name').first()).toBeHidden();
  await box.locator('.kata-hand-in').first().click();
  const viewer = teacher.locator('dialog.kata-viewer');
  await expect(viewer.locator('.kata-viewer-title')).toHaveText(/^Lösung \d+$/);
  await expect(viewer.locator('.kata-viewer-code')).toContainText('double');
  await viewer.screenshot({ path: testInfo.outputPath('viewer.png') });
  await teacher.keyboard.press('Escape');
  await box.locator('.kata-hand-ins-names input').check();
  await expect(box.locator('.kata-hand-in-name').first()).toBeVisible();
  await teacher.close();

  await card.locator('.task-hand-in').click();
  await expect(card.locator('.task-hand-in')).toHaveText('Einreichen');
});

test('pictures of a Kata card', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const [scheme, width] of [['light', 1100], ['dark', 1100], ['light', 390]] as const) {
    await page.emulateMedia({ colorScheme: scheme });
    await page.setViewportSize({ width, height: 900 });
    await page.goto('/kata-demo');
    const card = page.locator('.task-card[data-task="kata-demo/ggt"]:visible');
    await card.locator('.task-code').fill((await languageOf(card)) === 'ruby'
      ? 'def gcd(a, b)\n  # Euklid: der Rest ist der neue Teiler\n  b.zero? ? a : gcd(b, a % b)\nend\n\nputs gcd(60, 48)\n'
      : 'def gcd(a, b):\n    # Euklid: der Rest ist der neue Teiler\n    while b:\n        a, b = b, a % b\n    return a\n\nprint(gcd(60, 48))\n');
    await card.locator('.task-run').click();
    await expect(card.locator('.task-console')).toContainText('12', { timeout: 30_000 });
    await card.screenshot({ path: testInfo.outputPath(`kata-${scheme}-${width}.png`) });
  }
});
