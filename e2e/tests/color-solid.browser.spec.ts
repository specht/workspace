import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The colour spaces in 3D (color_solid.js) on the Handbuch page »Farbräume«:
// every space draws with WebGL, the section and the 8 × 8 × 8 cubes change
// the picture, dragging turns it. The geometry has tests of its own
// (test/static/color_solid.test.cjs).

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

test('the colour spaces turn, cut and fall apart into cubes', async ({ page, e2eEmail }, testInfo) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/reference/farbraeume');

  const solids = page.locator('.color-solid');
  await expect(solids).toHaveCount(5);
  for (const space of ['rgb', 'hsv', 'ycbcr', 'lab', 'oklab']) {
    const canvas = page.locator(`.color-solid[data-space='${space}'] canvas`);
    await canvas.scrollIntoViewIfNeeded();
    await expect.poll(() => drawn(canvas)).toBeGreaterThan(5000);
  }

  const lab = page.locator(".color-solid[data-space='lab']");
  const canvas = lab.locator('canvas');
  await lab.scrollIntoViewIfNeeded();
  const whole = await drawn(canvas);

  // The section: lower, less of the solid, and the height in L
  await lab.locator('[data-top]').fill('500');
  await expect(lab.locator('.cs-height')).toHaveText('bis L = 50');
  await expect.poll(() => drawn(canvas)).toBeLessThan(whole * 0.8);
  await lab.locator('[data-top]').fill('1000');
  await expect(lab.locator('.cs-height')).toHaveText('nicht geschnitten');

  // The cubes, pulled apart
  await expect(lab.locator('[data-apart]')).toBeHidden();
  await lab.locator('[data-split]').click();
  await expect(lab.locator('[data-split]')).toHaveAttribute('aria-pressed', 'true');
  await expect(lab.locator('[data-apart]')).toBeVisible();
  const before = await canvas.screenshot();
  await lab.locator('[data-explode]').fill('80');
  await expect.poll(async () => (await canvas.screenshot()).equals(before)).toBe(false);

  // Dragging turns it
  const box = (await canvas.boundingBox())!;
  const still = await canvas.screenshot();
  await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
  await page.mouse.down();
  await page.mouse.move(box.x + box.width / 2 + 80, box.y + box.height / 2, { steps: 5 });
  await page.mouse.up();
  expect((await canvas.screenshot()).equals(still)).toBe(false);

  expect(errors).toEqual([]);
});
