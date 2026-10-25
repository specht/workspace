// Pictures for »Was eine Aktivität ausmacht« on »Was ist der Hackschule
// Workspace?« (src/content/about), from the running development site:
//
//   cd e2e && HS_PICTURES=1 npx playwright test --project=pictures activity-tour.pictures
//
// One e2e student opens »Einführung in SQL«, »SQL: Gruppieren und
// Verbinden« and »Ein eigener Webserver« and is photographed at a few places, light and dark (NAME.webp,
// NAME-dark.webp):
//
// - tour-vorhersage: the prediction »reihenfolge« before an answer, with the
//   line that stands in for what would give the answer away (AfterTask);
// - tour-vorhersage-danach: the same card answered wrongly (the order in which
//   a query is written), with its feedback, and below it the explanation that
//   has appeared;
// - tour-rueckmeldung: »Was bedeutet die Zahl?« answered with square metres,
//   a typical mistake, and the feedback that names it;
// - tour-tipp: »Viele Nachbarn« with its first tip open; the next one is not
//   there yet (HintSteps);
// - tour-menschen: the box on Tim Berners-Lee, on »Ein eigener Webserver«.
//
// Each picture is that part of the page with a margin, at twice the pixels,
// written as .webp next to the page (needs ImageMagick's convert). The
// student's answers are deleted again afterwards; what the tips have opened
// stays in this browser only and goes with it. None of the cards is checked
// against a database, so the student needs no imported data.
import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import type { Locator, Page, TestInfo } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

const ABOUT = path.resolve(__dirname, '../../src/content/about');
const REPO = path.resolve(__dirname, '../..');
// What the pictures show comes from these files: when one changes,
// src/scripts/recreate-screenshots.rb --status calls the pictures stale.
const INPUTS = [
  'e2e/tests/activity-tour.pictures.ts',
  'src/content/mysql/mysql.md', 'src/content/mysql/tasks.yaml',
  'src/content/sql-joins/sql-joins.md', 'src/content/sql-joins/tasks.yaml',
  'src/content/webserver/webserver.md',
  'src/ruby/include/after_task.rb', 'src/ruby/include/hint_steps.rb', 'src/ruby/include/solution_blocks.rb',
  'src/ruby/include/task_card.rb', 'src/ruby/include/person_boxes.rb',
  'src/content/reference/personen/personen.md',
  'src/static/include/tasks.js', 'src/static/include/hint_steps.js',
  'src/static/include/default.css', 'src/static/include/dark.css', 'src/static/_template.html',
];
const STUDENT = 'e2e-auth-student-0@example.com';
const TASKS = ['mysql/reihenfolge', 'mysql/flaeche-einheit'];
const MARGIN = 16;

function inputsHash(): string {
  const hash = crypto.createHash('sha256');
  for (const file of [...INPUTS].sort()) {
    hash.update(file + '\n');
    hash.update(fs.readFileSync(path.join(REPO, file)));
    hash.update('\n');
  }
  return hash.digest('hex');
}

function baseUrl(testInfo: TestInfo): string {
  const value = testInfo.project.use.baseURL;
  if (typeof value !== 'string') throw new Error('Playwright baseURL must be configured');
  return value;
}

// The part of the page from the top of `from` to the bottom of `to`, as wide
// as the wider of the two, with a margin; in page coordinates, so that
// nothing fixed to the window lies over it.
async function shoot(page: Page, from: Locator, to: Locator, name: string, out: string, taken: string[]) {
  await from.scrollIntoViewIfNeeded();
  await page.waitForTimeout(400);
  const scroll = await page.evaluate(() => ({ x: window.scrollX, y: window.scrollY }));
  const a = (await from.boundingBox())!;
  const b = (await to.boundingBox())!;
  const left = Math.min(a.x, b.x) - MARGIN;
  const right = Math.max(a.x + a.width, b.x + b.width) + MARGIN;
  const top = a.y - MARGIN;
  const bottom = b.y + b.height + MARGIN;
  const clip = { x: left + scroll.x, y: top + scroll.y, width: right - left, height: bottom - top };
  const png = path.join(out, name + '.png');
  await page.screenshot({ path: png, clip, fullPage: true });
  execFileSync('convert', [png, '-quality', '85', '-define', 'webp:method=6', path.join(ABOUT, name + '.webp')]);
  taken.push(name + '.webp');
}

test('pictures for the activity tour', async ({ browser }, testInfo) => {
  test.setTimeout(300_000);
  const out = testInfo.outputDir;
  fs.mkdirSync(out, { recursive: true });
  const context = await browser.newContext({
    baseURL: baseUrl(testInfo), viewport: { width: 1280, height: 900 }, deviceScaleFactor: 2,
  });
  const page = await context.newPage();
  await loginAsE2eUser(page, STUDENT, testInfo);
  const forget = async () => {
    for (const task of TASKS) await page.request.post('/api/task/delete', { data: { task } });
  };
  const taken: string[] = [];
  try {
    for (const scheme of ['light', 'dark'] as const) {
      const suffix = scheme === 'dark' ? '-dark' : '';
      await forget();
      await page.emulateMedia({ colorScheme: scheme });
      await page.goto('/sql-joins');
      // a fresh start for the tips of every card
      await page.evaluate(() => Object.keys(localStorage).filter((k) => k.startsWith('hint-steps:')).forEach((k) => localStorage.removeItem(k)));
      await page.goto('/mysql');
      await page.addStyleTag({ content: 'footer { display: none !important; }' });

      // A prediction: what would give the answer away waits for it.
      const order = page.locator("section.task-card[data-task='mysql/reihenfolge']");
      const wait = page.locator(".after-task[data-after='reihenfolge'] > .after-task-wait");
      await expect(order).toBeVisible();
      await expect(wait).toBeVisible();
      await shoot(page, order, wait, 'tour-vorhersage' + suffix, out, taken);

      // Answered in the order a query is written: wrong, with the feedback
      // for exactly that answer, and the explanation is there now.
      await order.locator("input.form-check-input[value='1']").check();
      await order.locator('button.task-submit').click();
      await expect(order.locator('.task-feedback')).toContainText('So schreibt man die Abfrage', { timeout: 15_000 });
      const explained = page.locator(".after-task[data-after='reihenfolge'] > .after-task-body > p").first();
      await expect(explained).toBeVisible();
      await shoot(page, order, explained, 'tour-vorhersage-danach' + suffix, out, taken);

      // A typical mistake and the feedback that names it.
      const area = page.locator("section.task-card[data-task='mysql/flaeche-einheit']");
      await area.locator("input.form-check-input[value='1']").check();
      await area.locator('button.task-submit').click();
      await expect(area.locator('.task-feedback')).toContainText('Stadtviertel', { timeout: 15_000 });
      await shoot(page, area, area, 'tour-rueckmeldung' + suffix, out, taken);

      // Tips in steps: the first one open, the next one waits.
      await page.goto('/sql-joins');
      await page.addStyleTag({ content: 'footer { display: none !important; }' });
      const neighbours = page.locator("section.task-card[data-task='sql-joins/viele-nachbarn']");
      const steps = neighbours.locator('details.hint-step');
      await expect(steps.first()).toBeVisible();
      await steps.first().locator('summary').click();
      await expect(steps.first()).toHaveAttribute('open', '');
      await expect(steps.nth(1)).toBeHidden();
      await shoot(page, neighbours, neighbours, 'tour-tipp' + suffix, out, taken);

      // A person teachers know: Tim Berners-Lee on »Ein eigener Webserver«.
      await page.goto('/webserver');
      await page.addStyleTag({ content: 'footer { display: none !important; }' });
      const person = page.locator('div.person', { hasText: 'Berners-Lee' }).first();
      await expect(person.locator('img').first()).toHaveJSProperty('complete', true);
      await shoot(page, person, person, 'tour-menschen' + suffix, out, taken);
    }
    fs.writeFileSync(path.join(ABOUT, '.pictures.json'), JSON.stringify({
      script: 'e2e/tests/activity-tour.pictures.ts',
      command: 'cd e2e && HS_PICTURES=1 npx playwright test --project=pictures activity-tour.pictures',
      inputs: INPUTS, hash: inputsHash(), pictures: [...taken].sort(),
    }, null, 2) + '\n');
  } finally {
    await forget();
    await context.close();
  }
});
