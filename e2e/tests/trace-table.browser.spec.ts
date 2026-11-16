import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A trace table (docs/aufgaben.md »trace«): the values of a loop's variables,
// pass by pass. Given cells show their value, the others are fields; Enter
// goes to the next field; the feedback names the first wrong cell and a
// typical mistake gets its own sentence; the values come back after a reload.
test('a trace table is filled in, checked cell by cell and kept', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(90_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task: 'kata-loops/trace-halving' } });
  await page.goto('/kata-loops');
  const card = page.locator('.task-card[data-task="kata-loops/trace-halving"]:visible');
  await expect(card.locator('.task-trace thead th')).toHaveText(['Durchlauf', 'n', 'steps']);
  await expect(card.locator('.task-trace-given')).toHaveText(['18', '1']);
  const fields = card.locator('.task-trace-input');
  await expect(fields).toHaveCount(8);

  // Enter goes on to the next field; 4,5 is the typical mistake in pass 3
  await fields.first().click();
  for (const value of ['9', '2', '4,5', '3', '2', '4', '1']) {
    await page.keyboard.type(value);
    await page.keyboard.press('Enter');
  }
  await expect(fields.nth(7)).toBeFocused();
  await page.keyboard.type('5');
  await page.keyboard.press('Enter');
  await expect(card.locator('.task-feedback')).toContainText('Durchlauf 3, n: Beim Halbieren fällt der Rest weg');
  await expect(card).toHaveAttribute('data-status', 'wrong');

  await fields.nth(2).fill('4');
  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct');

  await page.reload();
  await expect(card.locator('.task-trace-input').nth(2)).toHaveValue('4');
  await expect(card.locator('.task-trace-input').nth(7)).toHaveValue('5');

  // »Meine Antwort löschen« empties the fields; the given cells stay
  await card.locator('.task-delete').click();
  await page.locator('#__template_modal .btn-danger').click();
  await expect(card.locator('.task-delete')).toBeHidden();
  for (let i = 0; i < 8; i++) await expect(card.locator('.task-trace-input').nth(i)).toHaveValue('');
  await expect(card.locator('.task-trace-given')).toHaveText(['18', '1']);
  await page.reload();
  await expect(card.locator('.task-trace-input').nth(2)).toHaveValue('');
});
