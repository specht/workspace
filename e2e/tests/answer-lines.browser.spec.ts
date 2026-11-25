import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// An answer in a sentence or two (check type answer with lines: N, such as
// »Deine Idee« in the Routenplaner): a textarea across the card that grows
// with the text; Enter makes a new line, Ctrl+Enter checks
// (docs/aufgaben.md, »answer«).
test('a free-text answer is a growing textarea, Enter makes a new line, Ctrl+Enter checks', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  const task = 'routenplaner/idee';
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  let checks = 0;
  page.on('request', (r) => { if (r.url().endsWith('/api/task/check')) checks++; });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task } });

  for (const [width, scheme] of [[1280, 'light'], [390, 'dark']] as const) {
    await page.setViewportSize({ width, height: 900 });
    await page.emulateMedia({ colorScheme: scheme });
    await page.goto('/routenplaner');
    const card = page.locator(`.task-card[data-task="${task}"]:visible`);
    const field = card.locator('textarea.task-input');
    await expect(field).toBeVisible();
    await expect(field).toHaveAttribute('rows', '3');

    // as wide as the card's form
    const form = await card.locator('.task-form').boundingBox();
    const box = await field.boundingBox();
    expect(box!.width).toBeGreaterThan(form!.width - 2);

    // Enter makes a new line and checks nothing
    await field.fill('');
    await field.pressSequentially('Erst alle Nachbarn des Starts.');
    await field.press('Enter');
    await field.pressSequentially('Dann deren Nachbarn, Ring für Ring.');
    await expect(field).toHaveValue('Erst alle Nachbarn des Starts.\nDann deren Nachbarn, Ring für Ring.');
    await page.waitForTimeout(500);
    expect(checks).toBe(0);

    // it grows with the text and shrinks back
    const low = (await field.boundingBox())!.height;
    for (let i = 0; i < 4; i++) await field.press('Enter');
    await field.pressSequentially('Und so weiter.');
    expect((await field.boundingBox())!.height).toBeGreaterThan(low + 10);

    await page.screenshot({ path: testInfo.outputPath(`answer-lines-${width}-${scheme}.png`), fullPage: false });

    // Ctrl+Enter checks
    await field.press('Control+Enter');
    await expect(card).toHaveAttribute('data-status', 'correct');
    checks = 0;
    await page.request.post('/api/task/delete', { data: { task } });
  }
  expect(errors).toEqual([]);
});
