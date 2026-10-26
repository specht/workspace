import fs from 'fs';
import path from 'path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// On a phone no page of the catalogs is wider than the screen
// (docs/checkliste.md, »Auf dem Handy«): every activity and every Handbuch
// page, logged in, so that task cards render with their editors and maps.
const CONTENT = path.resolve(__dirname, '../../src/content');

function entries(file: string): string[] {
  return fs.readFileSync(file, 'utf8').split('\n')
    .filter((line) => /^\s{4}- \.?[\w-]+\s*$/.test(line))
    .map((line) => line.trim().replace(/^- \.?/, ''));
}

const pages = [
  ...entries(path.join(CONTENT, 'sections.yaml')).map((slug) => `/${slug}`),
  ...entries(path.join(CONTENT, 'reference', 'sections.yaml')).map((slug) => `/reference/${slug}`),
];

test('no page is wider than a phone', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(15 * 60_000);
  await page.setViewportSize({ width: 390, height: 844 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const wide: string[] = [];
  for (const url of pages) {
    const response = await page.goto(url, { waitUntil: 'load' });
    if (!response || response.status() !== 200) continue;
    // what reaches past the right edge, for the message
    const over = await page.evaluate(() => {
      if (document.documentElement.scrollWidth <= innerWidth) return null;
      // not what a scrolling box (a code block, a table) holds: that box scrolls by itself
      const clipped = (el: Element) => {
        for (let a = el.parentElement; a && a !== document.body; a = a.parentElement) if (getComputedStyle(a).overflowX !== 'visible') return true;
        return false;
      };
      const culprit = Array.from(document.querySelectorAll('body *')).find((el) => {
        const box = el.getBoundingClientRect();
        return box.right > innerWidth + 1 && box.width > 0 && getComputedStyle(el).position !== 'fixed' && !clipped(el);
      });
      return `${document.documentElement.scrollWidth}px` + (culprit ? `, ${culprit.tagName.toLowerCase()}.${String(culprit.className).split(' ').join('.')}` : '');
    });
    if (over) wide.push(`${url}: ${over}`);
  }
  expect(wide, `${pages.length} pages at 390 px`).toEqual([]);
});

// The strip of Katas runs from edge to edge, its first set in line with the text.
test('the Kata strip on a phone', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/');
  const strip = page.locator('.kata-carousel');
  await strip.scrollIntoViewIfNeeded();
  const box = await strip.boundingBox();
  expect(box!.x).toBe(0);
  expect(box!.width).toBe(390);
  const first = await page.locator('.kata-set').first().boundingBox();
  const text = await page.locator('h2').first().boundingBox();
  expect(Math.abs(first!.x - text!.x)).toBeLessThanOrEqual(2);
});
