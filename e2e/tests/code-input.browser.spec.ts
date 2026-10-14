import { expect, test } from './fixtures';
import type { Page } from '@playwright/test';
import { loginAsE2eUser } from './workspace';

// The SQL and Cypher answer fields colour their text like the page's code
// blocks (code_input.js): same token classes as Rouge, and the coloured
// <pre> lies exactly under the textarea.

// [class, text] of each coloured piece, neighbours of one class merged,
// whitespace left out.
const TOKENS = `(root) => {
  const out = [];
  root.childNodes.forEach((n) => {
    const cls = n.nodeType === 1 ? n.className : '';
    const text = n.textContent;
    if (!text.trim() || cls === 'w') return;
    const last = out[out.length - 1];
    if (last && last[0] === cls) last[1] += text; else out.push([cls, text]);
  });
  return out;
}`;

async function compareWithCodeBlocks(page: Page, selector: string, language: 'sql' | 'cypher') {
  const blocks = await page.locator(selector).evaluateAll((pres, args) => {
    const tokens = eval(args.TOKENS);
    return pres.filter((pre) => args.language === 'cypher' || /^\s*SELECT\b/.test(pre.textContent!)).map((pre) => {
      const box = document.createElement('pre');
      box.innerHTML = (window as any).CodeInput[args.language](pre.textContent);
      return { text: pre.textContent, server: tokens(pre), client: tokens(box) };
    });
  }, { TOKENS, language });
  expect(blocks.length).toBeGreaterThan(2);
  for (const block of blocks) expect(block.client, block.text).toEqual(block.server);
}

// The <pre> and the textarea wrap the same text into the same lines.
async function expectAligned(page: Page, field: ReturnType<Page['locator']>) {
  const sizes = await field.evaluate((ta: HTMLTextAreaElement) => {
    const view = ta.parentElement!.querySelector('.code-input-view') as HTMLElement;
    return { ta: [ta.clientWidth, ta.scrollHeight], view: [view.clientWidth, view.scrollHeight], color: getComputedStyle(ta).color };
  });
  expect(sizes.view).toEqual(sizes.ta);
  expect(sizes.color).toBe('rgba(0, 0, 0, 0)');
}

test('SQL answers are coloured like the SQL code blocks', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/filmdatenbank');
  await compareWithCodeBlocks(page, 'pre:not(.code-input-view):not(.code-cypher)', 'sql');

  const field = page.locator('.task-sql').first();
  await field.fill("SELECT titel, jahr FROM film -- Kommentar\nWHERE titel LIKE 'Der %' AND jahr >= 1994\nORDER BY jahr DESC LIMIT 10");
  const view = field.locator('xpath=preceding-sibling::pre[1]');
  await expect(view.locator('.k', { hasText: 'SELECT' })).toBeVisible();
  await expect(view.locator('.s1')).toHaveText("'Der %'");
  await expect(view.locator('.c1')).toHaveText('-- Kommentar');
  await expect(view.locator('.mi').first()).toHaveText('1994');
  await expectAligned(page, field);

  // A long line wraps the same way in both.
  await field.fill('SELECT ' + Array.from({ length: 30 }, (_, i) => `spalte_${i}`).join(', ') + ' FROM film');
  await expectAligned(page, field);
});

test('Cypher answers are coloured like the Cypher code blocks', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.goto('/neo4j-filmdatenbank');
  await compareWithCodeBlocks(page, 'pre.code-cypher:not(.code-input-view)', 'cypher');

  const field = page.locator('.task-cypher').first();
  await field.fill('MATCH (p:Person)-[:DIRECTOR]->(m:Movie {title: "Up"})\nWHERE m.year > 2000\nRETURN m.title, count(*) AS n');
  const view = field.locator('xpath=preceding-sibling::pre[1]');
  await expect(view).toHaveClass(/code-cypher/);
  await expect(view.locator('.nc')).toHaveText(['Person', 'Movie']);
  await expect(view.locator('.nf')).toHaveText('DIRECTOR');
  await expect(view.locator('.na')).toHaveText(['title', 'year', 'title']);
  await expect(view.locator('.nb')).toHaveText('count');
  await expectAligned(page, field);
});
