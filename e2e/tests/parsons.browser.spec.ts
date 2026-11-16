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
  const shuffled = await lines.evaluateAll((items) => items.map((li) => (li as HTMLElement).dataset.text));
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

  // »Meine Antwort löschen«: the puzzle as it was at the start, shuffled, nothing indented or left out
  await card.locator('.task-delete').click();
  await page.locator('#__template_modal .btn-danger').click();
  await expect(card.locator('.task-delete')).toBeHidden();
  expect(await lines.evaluateAll((items) => items.map((li) => (li as HTMLElement).dataset.text))).toEqual(shuffled);
  await expect(card.locator('.parsons-line.parsons-skipped')).toHaveCount(0);
  await expect(card.locator('.parsons-line:not([data-level="0"])')).toHaveCount(0);
  expect(await card.locator('.parsons-answer').inputValue()).toBe(shuffled.join('\n') + '\n');
});

// Dragging with the mouse moves a line; dragging it to the right indents it.
// With a finger (real touch events, as on a phone) only the grip drags.
test('Parsons lines can be dragged with the mouse and with a finger', async ({ browser, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  const texts = (card) => card.locator('.parsons-line').evaluateAll((items) => items.map((li) => (li as HTMLElement).dataset.text));

  const page = await (await browser.newContext()).newPage();
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task: 'kata-loops/sum-even' } });
  await page.goto('/kata-loops');
  const card = page.locator('.task-card[data-task="kata-loops/sum-even"]:visible');
  const lines = card.locator('.parsons-line');
  await lines.nth(4).scrollIntoViewIfNeeded();
  const first = (await texts(card))[0];
  // the first line, taken by its text, dragged below the fourth and one step to the right
  const from = (await lines.nth(0).locator('.parsons-code').boundingBox())!;
  const to = (await lines.nth(3).boundingBox())!;
  await page.mouse.move(from.x + 10, from.y + from.height / 2);
  await page.mouse.down();
  await page.mouse.move(from.x + 30, to.y + to.height * 0.8, { steps: 8 });
  await page.mouse.move(from.x + 10 + 34, to.y + to.height * 0.8, { steps: 4 });
  // while dragged the line is lifted out of the list and a slot shows where it lands
  await expect(card.locator('.parsons-slot')).toHaveCount(1);
  await expect(card.locator('.parsons-line.parsons-dragging')).toHaveCount(1);
  await card.screenshot({ path: testInfo.outputPath('dragging.png') });
  await page.mouse.up();
  await expect(card.locator('.parsons-slot')).toHaveCount(0);
  const after = await texts(card);
  expect(after.indexOf(first)).toBe(3);
  await expect(lines.nth(3)).toHaveAttribute('data-level', '1');
  expect(await card.locator('.parsons-answer').inputValue()).toContain(`\n${' '.repeat(Number(await card.locator('.parsons').getAttribute('data-indent')))}${first}\n`);
  await page.context().close();

  const phone = await (await browser.newContext({ viewport: { width: 400, height: 900 }, hasTouch: true, isMobile: true })).newPage();
  await loginAsE2eUser(phone, e2eEmail, testInfo);
  await phone.goto('/kata-loops');
  const pcard = phone.locator('.task-card[data-task="kata-loops/sum-even"]:visible');
  await pcard.scrollIntoViewIfNeeded();
  const plines = pcard.locator('.parsons-line');
  const cdp = await phone.context().newCDPSession(phone);
  const touch = async (type: string, x: number, y: number) =>
    cdp.send('Input.dispatchTouchEvent', { type, touchPoints: type === 'touchEnd' ? [] : [{ x, y }] });
  const swipe = async (x: number, y1: number, y2: number) => {
    await touch('touchStart', x, y1);
    for (let i = 1; i <= 8; i++) await touch('touchMove', x, y1 + (y2 - y1) * i / 8);
    await touch('touchEnd', x, y2);
  };
  // from the code: the page scrolls, the order stays
  const order = await texts(pcard);
  const code = (await plines.nth(0).locator('.parsons-code').boundingBox())!;
  const below = (await plines.nth(3).boundingBox())!;
  await swipe(code.x + 20, code.y + code.height / 2, below.y + below.height * 0.8);
  expect(await texts(pcard)).toEqual(order);
  // from the grip: the line moves (measured once the page has stopped scrolling)
  await phone.waitForTimeout(1000);
  await plines.nth(4).scrollIntoViewIfNeeded();
  await phone.waitForTimeout(500);
  const grip = (await plines.nth(0).locator('.parsons-grip').boundingBox())!;
  const target = (await plines.nth(3).boundingBox())!;
  await swipe(grip.x + grip.width / 2, grip.y + grip.height / 2, target.y + target.height * 0.8);
  expect((await texts(pcard)).indexOf(order[0])).toBe(3);
  await phone.context().close();
});

// Every Kata whose »Einstieg« starts with a puzzle shows it: its lines, two
// that do not belong, nothing indented, and the hidden code in that order.
const PUZZLES: [string, string, number][] = [
  ['kata-decisions', 'sign', 11], ['kata-loops', 'sum-even', 11], ['kata-texts', 'digits-only', 11],
  ['kata-lists', 'positives', 11], ['kata-dicts', 'missing', 11], ['kata-recursion', 'digit-sum', 9],
  ['kata-search', 'first-negative', 10],
];
test('the first card of each Kata\'s »Einstieg« is a puzzle', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(120_000);
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const [kata, id, count] of PUZZLES) {
    await page.request.post('/api/task/delete', { data: { task: `${kata}/${id}` } });
    await page.goto(`/${kata}`);
    const card = page.locator(`.task-card[data-task="${kata}/${id}"]:visible`);
    const lines = card.locator('.parsons-line');
    const language = await card.locator('.parsons-answer').getAttribute('data-code-language');
    // the counts are Ruby's lines; in Python, without »end«, there are fewer
    if (language === 'ruby') await expect(lines).toHaveCount(count);
    else expect(await lines.count()).toBeGreaterThan(5);
    await expect(card.locator('.parsons-line:not([data-level="0"])')).toHaveCount(0);
    const texts = await lines.evaluateAll((items) => items.map((li) => (li as HTMLElement).dataset.text));
    expect(await card.locator('.parsons-answer').inputValue()).toBe(texts.join('\n') + '\n');
    if (kata === 'kata-decisions') await card.screenshot({ path: testInfo.outputPath('sign.png') });
  }
});
