import { expect, test } from './fixtures';

// The block-matching widget of »Video komprimieren« (motion_widget.js): the
// search shows the best shift beside the reader's square and leaves the
// square where the reader put it, so that the two can be compared; a click
// on the map puts the square on that shift. The block is the head
// (data-block 208 80), whose best shift is (+10, −6).

test('the search leaves the square, a click on the map moves it', async ({ page }) => {
  const errors: string[] = [];
  page.on('pageerror', (e) => errors.push(e.message));
  await page.goto('/video-compression');

  const widget = page.locator('.motion-widget');
  await widget.scrollIntoViewIfNeeded();
  const cells = widget.locator('.motion-stats .motion-num');
  // the reader's vector, sum, then the best vector and sum
  await expect(cells.nth(0)).toHaveText('(0, 0)');
  await expect(cells.nth(2)).toHaveText('–');
  const mark = widget.locator('.motion-same');
  await expect(mark).toBeHidden();

  await widget.getByRole('button', { name: 'Beste Verschiebung suchen' }).click();
  await expect(cells.nth(2)).toHaveText('(+10, −6)');
  await expect(cells.nth(0)).toHaveText('(0, 0)');
  await expect(mark).toBeHidden();

  // the map: 33 × 33 cells, the shift (dx, dy) in column dx + 16, row dy + 16
  const map = widget.locator('canvas.motion-map');
  const box = await map.boundingBox();
  if (!box) throw new Error('no map');
  await page.mouse.click(box.x + (10 + 16.5) / 33 * box.width, box.y + (-6 + 16.5) / 33 * box.height);
  await expect(cells.nth(0)).toHaveText('(+10, −6)');
  await expect(cells.nth(1)).toHaveText(await cells.nth(3).innerText());
  await expect(mark).toBeVisible();
  expect(errors).toEqual([]);
});
