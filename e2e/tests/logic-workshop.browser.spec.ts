import { expect, test } from './fixtures';
import type { Locator, Page } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// The circuit editor of the Logik-Werkstatt (circuit_editor.js, on top of
// logic_circuit.js) and its check (CircuitCheck): gates placed from the
// palette, wires dragged from pin to pin (on a touch screen tapped one after
// the other, with the keyboard V and a digit), the circuit checked on the
// server, the first wrong row named, the circuit back after a reload and,
// without the browser's draft, from the stored answer. A solved half adder
// becomes a block that the next card's palette offers. The page is only in
// development (».logikwerkstatt« in sections.yaml), as the E2E site is.

const card = (page: Page, id: string) => page.locator(`.task-card[data-task="logikwerkstatt/${id}"]`);
const tool = (editor: Locator, kind: string) => editor.locator(`.ce-tool[data-tool='${kind}']`);
const part = (editor: Locator, id: string) => editor.locator(`[data-part='${id}']:not([data-pin])`);
const outPin = (editor: Locator, id: string, port = '') => editor.locator(`.ce-pin-hit[data-part='${id}'][data-pin='out']${port ? `[data-port='${port}']` : ''}`);
const inPin = (editor: Locator, id: string, index: number) => editor.locator(`.ce-pin-hit[data-part='${id}'][data-index='${index}']`);

async function center(locator: Locator): Promise<[number, number]> {
  const box = await locator.boundingBox();
  if (!box) throw new Error('not visible');
  return [box.x + box.width / 2, box.y + box.height / 2];
}

// a wire dragged with the mouse from one pin to another
async function wire(page: Page, from: Locator, to: Locator) {
  await from.scrollIntoViewIfNeeded();
  const [x0, y0] = await center(from);
  const [x1, y1] = await center(to);
  await page.mouse.move(x0, y0);
  await page.mouse.down();
  for (let i = 1; i <= 6; i++) await page.mouse.move(x0 + ((x1 - x0) * i) / 6, y0 + ((y1 - y0) * i) / 6);
  await page.mouse.up();
}

// no stored answers and no drafts or blocks in the browser: every run starts
// with empty editors
async function forget(page: Page, ids: string[]) {
  await page.evaluate(async (ids) => {
    for (const id of ids) {
      await new Promise((done) => (window as any).api_call('/api/task/delete', { task: `logikwerkstatt/${id}` }, done, { no_please_wait: true }));
    }
    Object.keys(localStorage).filter((k) => k.startsWith('circuit-')).forEach((k) => localStorage.removeItem(k));
  }, ids);
  await page.reload();
}

test('XOR from four NANDs: dragged together, checked, back after a reload', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/logikwerkstatt');
  await forget(page, ['xor-nand']);

  const task = card(page, 'xor-nand');
  const editor = task.locator('.circuit-editor');
  await editor.scrollIntoViewIfNeeded();
  // only NAND in the palette; the task's switches and lamp are there
  await expect(editor.locator('.ce-tool')).toHaveCount(1);
  await expect(part(editor, 'a')).toBeVisible();
  await expect(part(editor, 'y')).toBeVisible();

  await test.step('one NAND is not an XOR: the first wrong row', async () => {
    await tool(editor, 'nand').click();
    await wire(page, outPin(editor, 'a'), inPin(editor, 'g1', 0));
    await wire(page, outPin(editor, 'b'), inPin(editor, 'g1', 1));
    await wire(page, outPin(editor, 'g1'), inPin(editor, 'y', 0));
    await expect(editor.locator('.ce-count')).toContainText('1 Gatter');
    await task.locator('.task-submit').click();
    await expect(task.locator('.task-feedback')).toContainText('Bei a = 0 und b = 0 leuchtet die Lampe y, sie sollte aus sein.');
  });

  await test.step('four NANDs: correct', async () => {
    for (let i = 0; i < 3; i++) await tool(editor, 'nand').click();
    await wire(page, outPin(editor, 'a'), inPin(editor, 'g2', 0));
    await wire(page, outPin(editor, 'g1'), inPin(editor, 'g2', 1));
    await wire(page, outPin(editor, 'g1'), inPin(editor, 'g3', 0));
    await wire(page, outPin(editor, 'b'), inPin(editor, 'g3', 1));
    await wire(page, outPin(editor, 'g2'), inPin(editor, 'g4', 0));
    await wire(page, outPin(editor, 'g3'), inPin(editor, 'g4', 1));
    // a new wire into the lamp replaces the old one
    await wire(page, outPin(editor, 'g4'), inPin(editor, 'y', 0));
    await expect(editor.locator('.ce-table tbody tr td:last-child')).toHaveText(['0', '1', '1', '0']);
    // a switch toggles, the lamp follows
    await part(editor, 'a').click();
    await expect(part(editor, 'y').locator('text').first()).toHaveText('1');
    await editor.screenshot({ path: testInfo.outputPath('xor-nand-light.png') });
    await page.emulateMedia({ colorScheme: 'dark', reducedMotion: 'reduce' });
    await page.evaluate(() => document.documentElement.setAttribute('data-bs-theme', 'dark'));
    await editor.screenshot({ path: testInfo.outputPath('xor-nand-dark.png') });
    await page.emulateMedia({ colorScheme: 'light', reducedMotion: 'reduce' });
    await page.evaluate(() => document.documentElement.setAttribute('data-bs-theme', 'light'));
    await task.locator('.task-submit').click();
    await expect(task.locator('.task-feedback')).toContainText('Alle 4 Zeilen der Wahrheitstabelle stimmen.');
    await expect(task).toHaveAttribute('data-status', 'correct');
  });

  await test.step('undo takes a wire back, redo brings it again', async () => {
    // the last change was the wire from g4 into the lamp, which had come from g1
    await editor.locator('.ce-actions button', { hasText: 'Rückgängig' }).click();
    await expect(editor.locator('.ce-table tbody tr td:last-child')).toHaveText(['1', '1', '1', '0']);
    await editor.locator('.ce-actions button', { hasText: 'Wiederholen' }).click();
    await expect(editor.locator('.ce-table tbody tr td:last-child')).toHaveText(['0', '1', '1', '0']);
  });

  await test.step('back after a reload, from the draft and from the stored answer', async () => {
    await page.reload();
    await expect(editor.locator('.ce-count')).toContainText('4 Gatter');
    await expect(editor.locator('.ce-table tbody tr td:last-child')).toHaveText(['0', '1', '1', '0']);
    await page.evaluate(() => Object.keys(localStorage).filter((k) => k.startsWith('circuit-')).forEach((k) => localStorage.removeItem(k)));
    await page.reload();
    await expect(editor.locator('.ce-count')).toContainText('4 Gatter');
  });

  await test.step('deleting the answer empties the editor', async () => {
    await task.locator('.task-delete').click();
    await page.locator('#__template_modal .btn-danger').click();
    await expect(editor.locator('.ce-count')).toContainText('0 Gatter');
  });
  expect(errors).toEqual([]);
});

test('a half adder becomes a block, wired into a full adder with the keyboard', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/logikwerkstatt');
  await forget(page, ['halbaddierer', 'volladdierer']);

  const half = card(page, 'halbaddierer');
  const ha = half.locator('.circuit-editor');
  await ha.scrollIntoViewIfNeeded();
  await tool(ha, 'xor').click();
  await tool(ha, 'and').click();
  await wire(page, outPin(ha, 'a'), inPin(ha, 'g1', 0));
  await wire(page, outPin(ha, 'b'), inPin(ha, 'g1', 1));
  await wire(page, outPin(ha, 'a'), inPin(ha, 'g2', 0));
  await wire(page, outPin(ha, 'b'), inPin(ha, 'g2', 1));
  await wire(page, outPin(ha, 'g1'), inPin(ha, 's', 0));
  await wire(page, outPin(ha, 'g2'), inPin(ha, 'c', 0));
  await half.locator('.task-submit').click();
  await expect(half.locator('.task-feedback')).toContainText('Alle 4 Zeilen der Wahrheitstabelle stimmen.');

  // saved as a block: every editor of the page offers it
  await ha.locator('.ce-actions button', { hasText: 'Als Baustein speichern' }).click();
  await ha.locator('.ce-form input').fill('HA');
  await ha.locator('.ce-form input').press('Enter');
  await expect(ha.locator('.ce-message')).toContainText('Der Baustein HA steht jetzt oben');

  const full = card(page, 'volladdierer');
  const va = full.locator('.circuit-editor');
  await va.scrollIntoViewIfNeeded();
  await expect(tool(va, 'block:HA')).toBeVisible();
  await tool(va, 'block:HA').click();
  await tool(va, 'block:HA').click();
  await tool(va, 'or').click();
  await expect(va.locator('.ce-count')).toContainText('5 Gatter, 4 davon in Bausteinen');

  // with the keyboard: V on the source, then the input's digit on the target
  const key = async (from: string, to: string, digit: string, presses = 1) => {
    await part(va, from).focus();
    for (let i = 0; i < presses; i++) await page.keyboard.press('v');
    await part(va, to).focus();
    await page.keyboard.press(digit);
  };
  await key('a', 'ha1', '1');
  await key('b', 'ha1', '2');
  await key('ha1', 'ha2', '1');           // ha1.s
  await key('c_in', 'ha2', '2');
  await key('ha1', 'g1', '1', 2);         // the second V: ha1.c
  await key('ha2', 'g1', '2', 2);         // ha2.c
  await key('ha2', 's', '1');             // ha2.s
  await key('g1', 'c_out', '1');
  await expect(va.locator('.ce-table tbody tr td:nth-child(4)')).toHaveText(['0', '1', '1', '0', '1', '0', '0', '1']);
  await full.locator('.task-submit').click();
  await expect(full.locator('.task-feedback')).toContainText('Alle 8 Zeilen der Wahrheitstabelle stimmen.');
  await full.locator('.task-reads-toggle').click();
  await va.screenshot({ path: testInfo.outputPath('full-adder.png') });
  expect(errors).toEqual([]);
});

test.describe('on a tablet', () => {
  test.use({ hasTouch: true, viewport: { width: 820, height: 1180 } });

  test('wires by tapping one pin and then the other', async ({ page, e2eEmail }, testInfo) => {
    await page.emulateMedia({ reducedMotion: 'reduce' });
    await loginAsE2eUser(page, e2eEmail, testInfo);
    await page.goto('/logikwerkstatt');
    await forget(page, ['multiplexer']);
    const task = card(page, 'multiplexer');
    const editor = task.locator('.circuit-editor');
    await editor.scrollIntoViewIfNeeded();
    for (const kind of ['not', 'and', 'and', 'or']) await tool(editor, kind).tap();
    const tapWire = async (from: Locator, to: Locator) => {
      await from.tap();
      await to.tap();
    };
    await tapWire(outPin(editor, 'sel'), inPin(editor, 'g1', 0));
    await tapWire(outPin(editor, 'a'), inPin(editor, 'g2', 0));
    await tapWire(outPin(editor, 'g1'), inPin(editor, 'g2', 1));
    await tapWire(outPin(editor, 'b'), inPin(editor, 'g3', 0));
    await tapWire(outPin(editor, 'sel'), inPin(editor, 'g3', 1));
    await tapWire(outPin(editor, 'g2'), inPin(editor, 'g4', 0));
    await tapWire(outPin(editor, 'g3'), inPin(editor, 'g4', 1));
    await tapWire(outPin(editor, 'g4'), inPin(editor, 'y', 0));
    await task.locator('.task-submit').tap();
    await expect(task.locator('.task-feedback')).toContainText('Alle 8 Zeilen der Wahrheitstabelle stimmen.');
    expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
    await editor.screenshot({ path: testInfo.outputPath('multiplexer-tablet.png') });
  });
});

test('on a phone the circuits are to look at and to switch', async ({ page }, testInfo) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await page.goto('/logikwerkstatt');
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);
  const free = page.locator('.circuit-editor[data-free]');
  await expect(free.locator('.ce-phone-note')).toBeVisible();
  await expect(free.locator('.ce-palette')).toBeHidden();
  // a circuit to look at (shown as after answering, for a guest): its switch toggles
  const view = page.locator('.circuit-view').first();
  await page.evaluate(() => document.querySelectorAll('.after-task').forEach((a) => a.classList.add('after-task-shown')));
  await view.scrollIntoViewIfNeeded();
  await part(view, 'a').click();
  await expect(part(view, 'y').locator('text').first()).toHaveText('1');
  for (const scheme of ['light', 'dark'] as const) {
    await page.emulateMedia({ colorScheme: scheme, reducedMotion: 'reduce' });
    await page.evaluate((s) => document.documentElement.setAttribute('data-bs-theme', s), scheme);
    await view.screenshot({ path: testInfo.outputPath(`view-phone-${scheme}.png`) });
  }
});
