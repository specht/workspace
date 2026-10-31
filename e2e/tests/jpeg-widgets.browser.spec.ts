import { expect, test } from './fixtures';

// The demo photo of »JPEG« decoded in the browser (jpeg_image.js) for two
// widgets: the iDCT stepper, which shows any block of it, chosen on the
// photo, and the MCU taken apart (jpeg_mcu.js), whose pixels explain their
// colour. The numbers are the page's: the first Y block, its first row
// after the iDCT, the DC coefficient of the second block (−138 · 5).

test('the iDCT stepper and the MCU on the demo photo', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.goto('/jpeg');

  const stepper = page.locator('.idct-stepper');
  await expect(stepper.locator('.jpeg-picker-where')).toHaveText('MCU in Spalte 0, Reihe 0: x = 0 bis 15, y = 0 bis 15.');
  await expect(stepper.locator('.idct-count')).toHaveText('Schritt 1 von 16');
  await stepper.locator("[data-go='last']").click();
  await expect(stepper.locator('.idct-numbers span').first()).toHaveText('40');
  const row = await stepper.locator('.idct-numbers span').evaluateAll((spans) => spans.slice(0, 8).map((s) => s.textContent).join(' '));
  expect(row).toBe('40 41 43 46 46 43 41 42');

  await test.step('another block of the same MCU', async () => {
    await stepper.locator("button[data-block='1']").click();
    await expect(stepper.locator("button[data-block='1']")).toHaveAttribute('aria-pressed', 'true');
    await expect(stepper.locator('.idct-count')).toContainText('Schritt 1 von');
    await expect(stepper.locator('.idct-coef span').first()).toHaveText('-690');
  });

  await test.step('another MCU with the arrow keys, the stepper stays where it is', async () => {
    const photo = stepper.locator('.jpeg-picker-photo');
    await photo.focus();
    await page.keyboard.press('ArrowRight');
    await page.keyboard.press('ArrowDown');
    await expect(stepper.locator('.jpeg-picker-where')).toHaveText('MCU in Spalte 1, Reihe 1: x = 16 bis 31, y = 16 bis 31.');
    await expect(stepper.locator('.idct-count')).toContainText('Schritt 1 von');
  });

  await test.step('the MCU: a pixel and how its colour comes about', async () => {
    const mcu = page.locator('.jpeg-mcu');
    await expect(mcu.locator('.jpeg-picker-where')).toHaveText('MCU in Spalte 13, Reihe 17: x = 208 bis 223, y = 272 bis 287.');
    await expect(mcu.locator('.jpeg-mcu-pixel')).toContainText('Pixel x = 208, y = 272');
    await mcu.locator("canvas[data-show='rgb']").focus();
    await page.keyboard.press('ArrowRight');
    await page.keyboard.press('ArrowDown');
    await expect(mcu.locator('.jpeg-mcu-pixel')).toContainText('Pixel x = 209, y = 273');
    await expect(mcu.locator('.jpeg-mcu-readout')).toContainText('aus Block Y0');
    await expect(mcu.locator('.jpeg-mcu-readout')).toContainText('beide gemeinsam für 4 Pixel');
    await expect(mcu.locator('.jpeg-mcu-formulas code').first()).toHaveText('R = Y + 1.402 · (Cr − 128)');
  });

  await test.step('the last row of MCUs reaches beyond the photo', async () => {
    const mcu = page.locator('.jpeg-mcu');
    await mcu.locator('.jpeg-picker-photo').focus();
    for (let i = 0; i < 30; i++) await page.keyboard.press('ArrowDown');
    await expect(mcu.locator('.jpeg-picker-where')).toHaveText('MCU in Spalte 13, Reihe 41: x = 208 bis 223, y = 656 bis 666; ragt unten über das Bild hinaus.');
    await mcu.locator("canvas[data-show='y']").focus();
    for (let i = 0; i < 15; i++) await page.keyboard.press('ArrowDown');
    await expect(mcu.locator('.jpeg-mcu-pixel')).toContainText('liegt außerhalb des Bilds');
  });

  expect(errors).toEqual([]);
});
