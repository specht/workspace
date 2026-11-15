import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A Parsons puzzle (docs/aufgaben.md »Parsons-Puzzles«): a Kata whose code
// is put together from given lines. The card shows them sorted and without
// indentation; tasks.js shuffles them, the buttons move and indent them and
// leave out what does not belong, and the ordered lines are the code the
// check runs. After a reload the stored code comes back as the order.

const SOLUTION: Record<string, string[]> = {
  ruby: ['def sum_even(n)', '  total = 0', '  (1..n).each do |i|', '    if i.even?', '      total += i', '    end', '  end', '  total', 'end'],
  python: ['def sum_even(n):', '    total = 0', '    for i in range(1, n + 1):', '        if i % 2 == 0:', '            total += i', '    return total'],
};

test('a Parsons puzzle is ordered, indented, checked and kept', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task: 'kata-loops/sum-even' } });
  await page.goto('/kata-loops');
  const card = page.locator('.task-card[data-task="kata-loops/sum-even"]:visible');
  const language = (await card.locator('.parsons-answer').getAttribute('data-code-language')) as 'ruby' | 'python';
  const solution = SOLUTION[language];
  const unit = language === 'ruby' ? 2 : 4;
  const lines = card.locator('.parsons-line');
  // the solution and the two lines that do not belong
  await expect(lines).toHaveCount(solution.length + 2);
  await expect(card.locator('.parsons-answer')).toBeHidden();

  // bring each line of the solution to its place with »nach oben«, indent it
  for (let i = 0; i < solution.length; i++) {
    const text = solution[i].trim();
    const texts = await lines.evaluateAll((items) => items.map((li) => (li as HTMLElement).dataset.text));
    let at = texts.findIndex((t, j) => j >= i && t === text);
    expect(at).toBeGreaterThanOrEqual(0);
    while (at > i) {
      await lines.nth(at).locator("[data-move='up']").click();
      at--;
    }
    const level = (solution[i].length - solution[i].trimStart().length) / unit;
    for (let k = 0; k < level; k++) await lines.nth(i).locator("[data-move='right']").click();
  }
  // the two extra lines: left out
  for (let i = solution.length; i < solution.length + 2; i++) await lines.nth(i).locator("[data-move='skip']").click();
  await expect(lines.nth(solution.length)).toHaveClass(/parsons-skipped/);
  await expect(card.locator('.parsons-answer')).toHaveValue(solution.join('\n') + '\n');

  await card.locator('.task-submit').click();
  await expect(card).toHaveAttribute('data-status', 'correct', { timeout: 60_000 });

  // after a reload the stored code is the order again
  await page.reload();
  await expect(card.locator('.parsons-answer')).toHaveValue(solution.join('\n') + '\n');
  await expect(lines.first()).toHaveAttribute('data-text', solution[0]);
  await expect(lines.nth(solution.length)).toHaveClass(/parsons-skipped/);
  await page.request.post('/api/task/delete', { data: { task: 'kata-loops/sum-even' } });
});
