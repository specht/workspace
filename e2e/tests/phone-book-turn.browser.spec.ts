import { expect, test } from './fixtures';

// The Telefonbuch's pages turn smoothly when the dog-ear is clicked: the
// dog-ear grows under the pointer, and the end of that transition, bubbling
// up, once ended the turn after a fifth of a second (the page snapped). The
// turning page must still be there, rotating, until the turn is over. Uses
// the sample entries of the development site (?muster).
test('a click on the dog-ear turns the page smoothly', async ({ page }) => {
  await page.setViewportSize({ width: 1300, height: 950 });
  await page.goto('/telefonbuch?muster');
  const ear = page.locator('#phone-book .pb-ear.next');
  await ear.waitFor();
  const box = (await ear.boundingBox())!;
  await page.mouse.move(box.x + box.width / 3, box.y + box.height / 3);
  await page.waitForTimeout(300);
  await page.mouse.down();
  await page.mouse.up();
  const angles: string[] = [];
  for (const ms of [150, 150, 150]) {
    await page.waitForTimeout(ms);
    angles.push(await page.evaluate(() => {
      const s = document.querySelector('.phone-book-turning') as HTMLElement | null;
      return s ? getComputedStyle(s).transform : 'gone';
    }));
  }
  expect(angles[0]).not.toBe('gone');
  expect(angles[1]).not.toBe('gone');
  await page.waitForTimeout(600);
  expect(await page.locator('.phone-book-turning').count()).toBe(0);
  await expect(page.locator('#phone-book .phone-book-leaf.left .phone-book-head')).toContainText('H');
});
