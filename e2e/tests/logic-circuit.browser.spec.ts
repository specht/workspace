import { expect, test } from './fixtures';
import type { Locator, Page } from '@playwright/test';

// The circuits of »Vom Gatter zum Rechenwerk« (logic_circuit.js): switches
// that a click or a key toggles, lamps and the truth table that follow, the
// RS flip-flop that holds its bit and says when it does not come to rest,
// and on a phone the inputs as buttons under the drawing. The engine has
// tests of its own (test/static/logic_circuit.test.cjs). The page is only in
// development (».logikgatter« in sections.yaml), as the E2E site is.

const circuit = (page: Page, name: string) => page.locator(`.logic-circuit[data-circuit~='${name}']`).first();
const input = (widget: Locator, name: string) => widget.locator(`svg .lc-switch[aria-label^='Eingang ${name}:']`);
const lamp = (widget: Locator, index: number) => widget.locator('svg .lc-lamp text').nth(index);

// what waits for an answer, shown as after answering, for a guest
async function showAll(page: Page) {
  await page.evaluate(() => document.querySelectorAll('.after-task').forEach((a) => a.classList.add('after-task-shown')));
}

test('gates, the flip-flops and the ALU switch as they should', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await page.goto('/logikgatter');
  await showAll(page);

  // a gate: clicks and keys toggle, the lamp and the row of the table follow
  const gates = circuit(page, 'gate-and');
  await expect(gates.locator('.lc-title')).toHaveText('Gatter');
  await expect(lamp(gates, 0)).toHaveText('0');
  await input(gates, 'a').click();
  await input(gates, 'b').focus();
  await page.keyboard.press('Space');
  await expect(input(gates, 'b')).toHaveAttribute('aria-pressed', 'true');
  await expect(lamp(gates, 0)).toHaveText('1');
  await expect(gates.locator('tr.lc-now td')).toHaveText(['1', '1', '1']);
  // another gate from the choice above; a row of the table sets the inputs
  await gates.locator('.lc-presets select').selectOption({ label: 'XOR (=1)' });
  await gates.locator('tbody tr').nth(2).click();
  await expect(lamp(gates, 0)).toHaveText('1');
  await expect(gates.locator('tr.lc-now td')).toHaveText(['1', '0', '1']);

  // the RS flip-flop holds the 1 after S is back at 0
  const rs = circuit(page, 'rs-latch');
  await input(rs, 'S').click();
  await input(rs, 'S').click();
  await expect(lamp(rs, 0)).toHaveText('1');
  await expect(rs.locator('.lc-say')).toContainText('hält sein Bit, Q = 1');
  // S and R released at once: it does not come to rest, and says so
  await expect(rs.locator('.lc-release')).toBeHidden();
  await input(rs, 'S').click();
  await input(rs, 'R').click();
  await rs.locator('.lc-release').click();
  await expect(rs.locator('.lc-say')).toContainText('kommt nicht zur Ruhe');

  // the D flip-flop follows D only while the clock is 1
  const d = circuit(page, 'd-latch');
  await input(d, 'Takt').click();
  await input(d, 'D').click();
  await expect(lamp(d, 0)).toHaveText('1');
  await input(d, 'Takt').click();
  await input(d, 'D').click();
  await expect(lamp(d, 0)).toHaveText('1');
  await expect(d.locator('.lc-say')).toContainText('Q behält 1');

  // the ALU: 3 − 5 with 4 bits is 1110, a borrow, CF = 1
  const alu = circuit(page, 'alu');
  await alu.locator('.lc-ops button', { hasText: 'SUB' }).click();
  for (const bit of ['a1', 'a0', 'b2', 'b0']) await input(alu, bit).click();
  await expect(alu.locator('.lc-readout')).toContainText('SUB 0011, 0101 ergibt 1110, ZF = 0, CF = 1');
  expect(errors).toEqual([]);
});

test('on a phone the circuits fit and the inputs are buttons for a finger', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await page.goto('/logikgatter');
  await showAll(page);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth)).toBe(true);

  const alu = circuit(page, 'alu');
  await expect(alu).toHaveClass(/lc-small/);
  const buttons = alu.locator('.lc-inputs .lc-input-button');
  await expect(buttons).toHaveCount(8);
  const box = await buttons.first().boundingBox();
  expect(box!.height).toBeGreaterThanOrEqual(44);
  // a3 is the first button; 1000 + 0000
  await buttons.first().click();
  await expect(alu.locator('.lc-readout')).toContainText('ADD 1000, 0000 ergibt 1000');
  await expect(input(alu, 'a3')).toHaveAttribute('aria-pressed', 'true');
});
