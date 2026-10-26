import { expect, test } from './fixtures';
import { devices } from '@playwright/test';
import type { Locator } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// A Kata (KataCheck, docs/plan-katas.md): code typed into the card, run in a
// container of its own. One button, »Ausführen«, shows what the program
// prints, then calls the functions and compares; the code comes back after
// a reload. The Kata pages exist in development only. The visible card is
// in the person's language (Ruby or Python), so each test writes in that.

async function languageOf(card: Locator): Promise<'ruby' | 'python'> {
  return (await card.locator('.task-code').getAttribute('data-code-language')) as 'ruby' | 'python';
}

test('a Kata runs, checks and keeps its code', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-functions');
  const card = page.locator('.task-card[data-task="kata-functions/greet"]:visible');
  const field = card.locator('.task-code');
  await expect(field).toHaveValue(/def greet\(name\)/);
  await expect(card.locator('.task-level')).toHaveText('Einstieg');
  await expect(card.locator('.task-level-bars .on')).toHaveCount(1);

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

  // one button: the program runs, what it prints shows, and the check says
  // whether the function works
  await expect(card.locator('.task-run')).toHaveCount(0);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Alle 3 Aufrufe liefern, was sie sollen.', { timeout: 30_000 });
  await expect(card.locator('.task-console')).toHaveText('Hallo, Ada!\n');
  await expect(card).toHaveAttribute('data-status', 'correct');

  await page.reload();
  await expect(card.locator('.task-code')).toHaveValue(ruby ? /"Hallo, #\{name\}!"/ : /f'Hallo, \{name\}!'/);
});

test('a typical wrong Kata gets its own feedback', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-loops');
  const card = page.locator('.task-card[data-task="kata-loops/gcd"]:visible');
  await card.locator('.task-code').fill((await languageOf(card)) === 'ruby' ? 'def gcd(a, b) = [a, b].min\n' : 'def gcd(a, b):\n    return min(a, b)\n');
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Die kleinere Zahl ist nur manchmal ein Teiler der größeren.', { timeout: 30_000 });
});

test('an Extra is marked and counts for nobody', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/kata-decisions');
  const card = page.locator('.task-card[data-task="kata-decisions/rock-paper-scissors"]:visible');
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
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Alle 3 Aufrufe liefern, was sie sollen.', { timeout: 30_000 });
  await expect(card.locator('.task-console')).toHaveText('Hallo, Ada!\n');
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
    await page.goto('/kata-loops');
    const card = page.locator('.task-card[data-task="kata-loops/gcd"]:visible');
    await card.locator('.task-code').fill((await languageOf(card)) === 'ruby'
      ? 'def gcd(a, b)\n  # Euklid: der Rest ist der neue Teiler\n  b.zero? ? a : gcd(b, a % b)\nend\n\nputs gcd(60, 48)\n'
      : 'def gcd(a, b):\n    # Euklid: der Rest ist der neue Teiler\n    while b:\n        a, b = b, a % b\n    return a\n\nprint(gcd(60, 48))\n');
    await card.locator('.task-submit').click();
    await expect(card.locator('.task-console')).toContainText('12', { timeout: 30_000 });
    await card.screenshot({ path: testInfo.outputPath(`kata-${scheme}-${width}.png`) });
  }
});

// Pip (PipCheck): the plan runs against the world on the server, the card
// plays the run back step by step and marks each step's line; a mistake
// names the line. The page /pip-1 exists in development only.
const PIP_SHAFT = {
  ruby: 'move\nmove\nturn_left\nturn_left\nturn_left\nmove\nmove\nmove\n',
  python: 'move()\nmove()\nturn_left()\nturn_left()\nturn_left()\nmove()\nmove()\nmove()\n',
};

test('Pip walks a plan, and a wall names the line', async ({ browser, page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pip-1');
  // the prediction's map is drawn from the text in the page
  await expect(page.locator('.pip-static .pip-label')).toHaveCount(3);
  await expect(page.locator('.pip-static .pip-pip')).toHaveCount(1);

  const card = page.locator('.task-card[data-task="pip-1/shaft"]:visible');
  const language = (await languageOf(card));
  await expect(card.locator('.pip-map .pip-pip')).toHaveAttribute('data-facing', 'east');
  await card.locator('.task-code').fill(language === 'ruby' ? 'move\nturn_left\nmove\n' : 'move()\nturn_left()\nmove()\n');
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Hier in Zeile 3 läuft Pip gegen die Wand.', { timeout: 30_000 });
  await expect(card.locator('.task-feedback')).toBeVisible();

  await card.locator('.task-code').fill(PIP_SHAFT[language]);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Pip schafft es.', { timeout: 30_000 });
  await expect(card.locator('.task-feedback')).toBeVisible();
  await expect(card).toHaveAttribute('data-status', 'correct');
  // the replay ends at the lamp, the last step on the last line
  await expect(card.locator('.pip-status')).toHaveText('Schritt 8 von 8 · Zeile 8', { timeout: 20_000 });
  await expect(card.locator('.pip-pip')).toHaveAttribute('data-facing', 'south');
  // Pip has lit the old lamp; back at the start it is dark again
  await expect(card.locator('.pip-lamp')).toHaveClass(/pip-lit/);
  // line numbers beside the plan, the replay's line brighter; the step
  // buttons and the timeline of every widget
  await expect(card.locator('.code-input-gutter > div')).toHaveCount(9);
  await expect(card.locator('.code-input-gutter > .current')).toHaveText('8');
  // the start and eight steps, the last one onto the lamp
  await expect(card.locator('.stepper-timeline > span')).toHaveCount(9);
  await card.locator("[data-go='first']").click();
  await expect(card.locator('.pip-status')).toHaveText('Schritt 0 von 8');
  await expect(card.locator('.pip-lamp')).not.toHaveClass(/pip-lit/);
  await card.locator("[data-go='next']").click();
  await expect(card.locator('.code-input-gutter > .current')).toHaveText('1');
  await card.locator("[data-go='last']").click();
  for (const [scheme, width] of [['light', 1100], ['dark', 1100], ['light', 390]] as const) {
    await page.emulateMedia({ colorScheme: scheme });
    await page.setViewportSize({ width, height: 900 });
    await page.waitForTimeout(500);
    await card.screenshot({ path: testInfo.outputPath(`pip-${scheme}-${width}.png`) });
    await page.locator('.pip-static').screenshot({ path: testInfo.outputPath(`pip-static-${scheme}-${width}.png`) });
  }
  // on a phone (a touch screen) the controls of every widget, for the thumb
  const phone = await browser.newContext({ ...devices['iPhone 13'], storageState: await page.context().storageState() });
  const mobile = await phone.newPage();
  await mobile.goto(page.url());
  const mobileCard = mobile.locator('.task-card[data-task="pip-1/shaft"]:visible');
  await mobileCard.locator('.task-submit').click();
  await expect(mobileCard.locator('.pip-status')).toHaveText('Schritt 8 von 8 · Zeile 8', { timeout: 30_000 });
  await expect(mobileCard.locator('.task-console')).toBeHidden();
  await mobileCard.locator('.code-input').scrollIntoViewIfNeeded();
  await mobileCard.locator('.pip-world').screenshot({ path: testInfo.outputPath('pip-phone.png') });
  await phone.close();
});

// The first chapter: the plan is clicked, one line a click, and only
// watched; a wall writes nothing, »Zurück« takes the last line back, the
// lamp checks the plan by itself (pip_world.js, PipButtons).
test('Pip is clicked to the first lamp', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pip-1');
  const card = page.locator('.task-card[data-task="pip-1/entrance"]:visible');
  const language = await languageOf(card);
  const turn = language === 'ruby' ? 'turn_left' : language === 'python' ? 'turn_left()' : 'turnLeft();';
  const forward = card.locator('.pip-button[data-command="move"]');
  const left = card.locator('.pip-button[data-command="turn_left"]');
  const undo = card.locator('.pip-undo');
  const pip = card.locator('.pip-map .pip-pip');
  await expect(card.locator('.task-submit')).toBeHidden();
  await expect(card.locator('.task-code')).toHaveAttribute('readonly', '');
  // a plan from an earlier run comes back; taken back to the start here
  while (await undo.isEnabled()) await undo.click();
  await expect(pip).toHaveAttribute('data-facing', 'south');

  await left.click();
  await expect(pip).toHaveAttribute('data-facing', 'east');
  await expect(card.locator('.task-code')).toHaveValue(turn + '\n');
  // east is the wall: Pip says so, the plan stays as it was
  await forward.click();
  await expect(card.locator('.pip-bubble')).toHaveText('Autsch!');
  await expect(card.locator('.task-code')).toHaveValue(turn + '\n');
  await undo.click();
  await expect(pip).toHaveAttribute('data-facing', 'south');
  await expect(card.locator('.task-code')).toHaveValue('');
  await expect(undo).toBeDisabled();

  for (const button of [forward, forward, left, forward, forward, forward]) await button.click();
  await expect(card.locator('.pip-status')).toHaveText('Schritt 6 von 6 · Zeile 6');
  await card.screenshot({ path: testInfo.outputPath('pip-buttons.png') });
  await forward.click();
  await expect(card.locator('.task-feedback')).toContainText('Pip schafft es.', { timeout: 30_000 });
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(card.locator('.pip-lamp')).toHaveClass(/pip-lit/);
  await expect(forward).toBeDisabled();
  await card.screenshot({ path: testInfo.outputPath('pip-buttons-done.png') });

  // after a reload the plan is clicked through again, up to the lamp
  await page.reload();
  await expect(card.locator('.pip-status')).toHaveText('Schritt 7 von 7 · Zeile 7');
  await expect(card.locator('.code-input-gutter > div')).toHaveCount(8);
  await expect(undo).toBeEnabled();
  await expect(card.locator("[data-go='next']")).toBeDisabled();
  await page.setViewportSize({ width: 390, height: 900 });
  await card.screenshot({ path: testInfo.outputPath('pip-buttons-390.png') });
});

// Teil 2: a function of one's own; the check calls it on its own maps and
// says where Pip looks afterwards.
test('Pip learns turn_right, and the check calls it', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pip-2');
  const card = page.locator('.task-card[data-task="pip-2/turn-right"]:visible');
  const language = await languageOf(card);
  const wrong = language === 'ruby' ? 'def turn_right\n  turn_left\nend\n' : 'def turn_right():\n    turn_left()\n';
  await card.locator('.task-code').fill(wrong);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('nach oben statt nach unten', { timeout: 30_000 });
  const right = language === 'ruby'
    ? 'def turn_right\n  3.times do\n    turn_left\n  end\nend\n\nmove\n'
    : 'def turn_right():\n    for i in range(3):\n        turn_left()\n\nmove()\n';
  await card.locator('.task-code').fill(right);
  await card.locator('.task-submit').click();
  await expect(card.locator('.task-feedback')).toContainText('Pip schafft alle 2 Karten.', { timeout: 30_000 });
  // the replay: three turns, the plan's own move left out
  await expect(card.locator('.pip-status')).toHaveText(/^Karte 1 von 2 · Schritt 3 von 3/, { timeout: 20_000 });
  await expect(card.locator('.pip-pip')).toHaveAttribute('data-facing', 'south');
  // what the plan says while it is loaded is said too, and the cursor is
  // back in the editor
  const said = language === 'ruby'
    ? 'def turn_right\n  3.times do\n    turn_left\n  end\nend\n\nsay "Rechts!"\nturn_right\n'
    : 'def turn_right():\n    for i in range(3):\n        turn_left()\n\nsay("Rechts!")\nturn_right()\n';
  await card.locator('.task-code').fill(said);
  await card.locator('.task-submit').click();
  await expect(card.locator('.pip-bubble')).toHaveText('Rechts!', { timeout: 30_000 });
  await expect(card.locator('.task-code')).toBeFocused();
});

// What Pip says comes sentence by sentence while the run plays, each long
// enough to read.
test('Pip says one sentence after the other', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pip-1');
  const card = page.locator('.task-card[data-task="pip-1/shaft"]:visible');
  const plan = (await languageOf(card)) === 'ruby'
    ? 'say "Hallo. Wo ist Bello?"\nmove\n'
    : 'say("Hallo. Wo ist Bello?")\nmove()\n';
  await card.locator('.task-code').fill(plan);
  await card.locator('.task-submit').click();
  const bubble = card.locator('.pip-bubble');
  await expect(bubble).toHaveText('Hallo.', { timeout: 30_000 });
  await expect(bubble).toHaveText('Wo ist Bello?', { timeout: 5_000 });
});

// The map in the tab runs first and stays; while a run plays, the tabs wait.
test('Pip runs on the map of the tab', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/pip-2');
  const card = page.locator('.task-card[data-task="pip-2/walk"]:visible');
  const language = await languageOf(card);
  await expect(card.locator('.pip-tab')).toHaveCount(3);
  await expect(card.locator('.pip-tab').nth(1)).toContainText('walk(5)');
  await card.locator('.pip-tab').nth(1).click();
  await expect(card.locator('.pip-pip')).toHaveAttribute('data-facing', 'south');
  await card.locator('.task-code').fill(language === 'ruby'
    ? 'def walk(n)\n  n.times do\n    move\n  end\nend\n'
    : 'def walk(n):\n    for i in range(n):\n        move()\n');
  await card.locator('.task-submit').click();
  await expect(card.locator('.pip-tab').first()).toBeDisabled({ timeout: 30_000 });
  await expect(card.locator('.task-feedback')).toContainText('Pip schafft alle 3 Karten.', { timeout: 30_000 });
  await expect(card.locator('.task-feedback')).toBeVisible();
  await expect(card.locator('.pip-status')).toHaveText(/^Karte 2 von 3 · Schritt 5 von 5/);
  await expect(card.locator('.pip-tab').nth(1)).toHaveClass(/active/);
  await expect(card.locator('.pip-tab').first()).toBeEnabled();
});
