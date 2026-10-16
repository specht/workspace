import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// Colour classes in 3D (color_classes.js) in »Geheime Botschaften in
// Bildern«: both views draw with WebGL, the division switches, a click picks
// a colour and its class, and the swatches show that class in RGB and Lab.
// The colour maths has tests of its own (test/static/color_classes.test.cjs).

// How many pixels of a canvas are not transparent
const drawn = (canvas) => canvas.evaluate((element: HTMLCanvasElement) => {
  const copy = document.createElement('canvas');
  copy.width = element.width;
  copy.height = element.height;
  const context = copy.getContext('2d')!;
  context.drawImage(element, 0, 0);
  const data = context.getImageData(0, 0, copy.width, copy.height).data;
  let count = 0;
  for (let i = 3; i < data.length; i += 4) if (data[i] > 0) count++;
  return count;
});

test('the colour classes turn, switch and show the class of a colour', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/steganography');

  // The cube in »Farbklassen«: one view, no switch, no swatches
  const cube = page.locator('.color-classes').first();
  await expect(cube.locator('.cc-view')).toHaveCount(1);
  await expect(cube.locator('.cc-split')).toBeHidden();
  await expect(cube.locator('.cc-say')).toContainText('216 Würfel in RGB');

  // The two views in »Farben, wie wir sie sehen«
  const widget = page.locator('.color-classes').nth(1);
  await widget.scrollIntoViewIfNeeded();
  const say = widget.locator('.cc-say');
  await expect(say).toContainText('Quader in Lab');
  const labCanvas = widget.locator(".cc-view[data-space='lab'] canvas");
  await expect.poll(() => drawn(widget.locator(".cc-view[data-space='rgb'] canvas"))).toBeGreaterThan(10000);
  await expect.poll(() => drawn(labCanvas)).toBeGreaterThan(10000);

  // The slider makes it finer, the switch shows the cubes
  await widget.locator('[data-k]').fill('8');
  await expect(widget.locator('.cc-k')).not.toHaveText('13 × 8 × 8');
  await widget.locator("button[data-split='rgb']").click();
  await expect(say).toContainText('512 Würfel in RGB');
  await expect(widget.locator('.cc-k')).toHaveText('8 × 8 × 8');

  // A click on the middle of the Lab solid picks a colour: its class alone
  await widget.locator('[data-cut]').uncheck();
  const box = (await labCanvas.boundingBox())!;
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2);
  await expect(say).toContainText('der Rest ist Glas');
  await expect(widget.locator("[data-caption='rgb']")).toContainText('Ihr Würfel in RGB');
  await expect(widget.locator("[data-caption='lab']")).toContainText('Ihr Quader in Lab');
  await page.mouse.click(box.x + box.width / 2, box.y + box.height / 2);
  await expect(say).not.toContainText('der Rest ist Glas');

  // Dragging turns the views
  const before = await labCanvas.screenshot();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 80, box.y + box.height / 2, { steps: 5 });
  await page.mouse.up();
  expect((await labCanvas.screenshot()).equals(before)).toBe(false);

  expect(errors).toEqual([]);
});
