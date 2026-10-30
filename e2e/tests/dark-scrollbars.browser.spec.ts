import fs from 'fs';
import path from 'path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A box that stays dark in the light scheme scrolls with a scrollbar in its
// colours, never the browser's light one (docs/checkliste.md; the shared
// rule in default.css): every activity and every Handbuch page, light,
// logged in, so that task cards render.
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

test('a dark box scrolls with a dark scrollbar', async ({ page, e2eEmail }, testInfo) => {
  test.setTimeout(15 * 60_000);
  await page.emulateMedia({ colorScheme: 'light' });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const light: string[] = [];
  for (const url of pages) {
    const response = await page.goto(url, { waitUntil: 'load' });
    if (!response || response.status() !== 200) continue;
    const found = await page.evaluate(() => {
      // the brightness of the first background that is not transparent,
      // from the box outwards (the adventure's is on its frame)
      const brightness = (el: Element | null): number => {
        for (; el; el = el.parentElement) {
          const m = getComputedStyle(el).backgroundColor.match(/[\d.]+/g);
          if (!m || (m.length > 3 && Number(m[3]) === 0)) continue;
          const [r, g, b] = m.map(Number);
          return 0.2126 * r + 0.7152 * g + 0.0722 * b;
        }
        return 255;
      };
      const out = new Set<string>();
      for (const el of Array.from(document.querySelectorAll('body *'))) {
        const style = getComputedStyle(el);
        if (!/(auto|scroll)/.test(style.overflowX + style.overflowY)) continue;
        if (brightness(el) > 90 || style.scrollbarColor !== 'auto' || style.colorScheme.includes('dark')) continue;
        out.add(`${el.tagName.toLowerCase()}.${Array.from(el.classList).join('.')}`);
      }
      return Array.from(out);
    });
    for (const box of found) light.push(`${url}: ${box}`);
  }
  expect(light, `${pages.length} pages, light`).toEqual([]);
});
