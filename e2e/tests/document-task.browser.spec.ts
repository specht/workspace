import fs from 'fs';
import path from 'path';
import type { Locator } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A document task (DocumentCheck, document_task.js): the student's file goes
// to the office service, and the card answers with a checklist and the page
// as LibreOffice drew it, a numbered mark at each place a point fails. The
// starting file of »Was das Auge nicht sieht« fails seven of eight points,
// the built invitation passes all of them.

const repo = path.resolve(__dirname, '..', '..');
const starter = path.join(repo, 'src/static/formatting-marks/01 Einladung.docx');
const solution = path.join(repo, 'src/content/formatting-marks/stufen/einladung.docx');
const shots = process.env.DOCUMENT_TASK_SHOTS;

// »Seitenbreite« fits the text with its numbers in the margin, »Ganze Seite«
// the whole sheet, each without much room left over
async function checkFits(report: Locator, slack: number) {
  const viewport = report.locator('.document-viewport');
  await report.getByRole('button', { name: 'Seitenbreite' }).click();
  await expect.poll(async () => {
    const view = (await viewport.boundingBox())!;
    const badges = await report.locator('.document-badge').evaluateAll((all) => all.map((b) => b.getBoundingClientRect().left));
    const boxes = await report.locator('.document-mark-box').evaluateAll((all) => all.map((b) => b.getBoundingClientRect().right));
    const right = Math.max(...boxes);
    return Math.min(...badges) >= view.x - 1 && right <= view.x + view.width + 1 && right >= view.x + view.width - slack;
  }, { timeout: 3000 }).toBe(true);
  await report.getByRole('button', { name: 'Ganze Seite' }).click();
  await expect.poll(async () => {
    const view = (await viewport.boundingBox())!;
    const sheet = (await report.locator('.document-sheet').first().boundingBox())!;
    const inside = sheet.x >= view.x - 1 && sheet.y >= view.y - 1 &&
      sheet.x + sheet.width <= view.x + view.width + 1 && sheet.y + sheet.height <= view.y + view.height + 1;
    const fills = sheet.width >= view.width - 2 * slack || sheet.height >= view.height - 2 * slack;
    return inside && fills;
  }, { timeout: 3000 }).toBe(true);
}

test('a document is checked and its problems are marked on its page', async ({ page, e2eEmail }, testInfo) => {
  // half of a desktop beside Word is the size this is made for
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/formatting-marks');
  await states;
  const card = page.locator(".task-card[data-task='formatting-marks/einladung']");
  await expect(card.locator('.document-starter')).toHaveAttribute('href', '/formatting-marks/01%20Einladung.docx');

  // the starting file: everything but the text is open
  const checked = page.waitForResponse((r) => r.url().includes('/api/task/document'));
  await card.locator('.document-input').setInputFiles(starter);
  await checked;
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  // the verdict, the checklist and the page fit in the window, and the
  // card scrolls them there
  await expect.poll(async () => {
    const box = await report.boundingBox();
    return box!.y >= 0 && box!.y + box!.height <= page.viewportSize()!.height;
  }).toBe(true);
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(7);
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(1);
  await expect(report.locator('.document-sheet img')).toHaveCount(1);
  await expect(report.locator('.document-sheet img')).toHaveAttribute('src', /^data:image\/svg\+xml/);
  const marks = report.locator('.document-mark');
  expect(await marks.count()).toBeGreaterThan(10);
  // one number per open point; pointing at a point brings out its places
  await expect(report.locator('.document-chip')).toHaveCount(6);
  await report.locator(".document-open[data-n='1']").hover();
  await expect(report.locator(".document-mark[data-n='1']").first()).toHaveClass(/document-active/);
  await expect(report).toHaveAttribute('data-active', '1');
  await page.mouse.move(0, 0);
  await expect(card.locator('.document-name')).toContainText('01 Einladung.docx');
  if (shots) await page.screenshot({ path: path.join(shots, 'document-wrong.png') });
  await checkFits(report, 20);
  // a point's number moves the view to its place and zooms in
  const zoom = report.locator('.document-zoom');
  const before = await zoom.textContent();
  // the view travels to the point's place and keeps it for the next check
  await report.locator('.document-chip', { hasText: /^3$/ }).click();
  await expect(zoom).not.toHaveText(before!);
  await expect(report.locator(".document-mark.document-arrived[data-n='3']").first()).toBeAttached();
  await report.locator('.document-chip', { hasText: /^3$/ }).click();
  await expect(report.locator(".document-where[data-for='3']")).toHaveText('Stelle 2 von 2');
  await page.waitForTimeout(800);
  const kept = await zoom.textContent();
  const recheck = page.waitForResponse((r) => r.url().includes('/api/task/document'));
  await card.locator('.document-input').setInputFiles(starter);
  await recheck;
  await expect(report.locator('.document-zoom')).toHaveText(kept!);
  // the mouse wheel zooms at the pointer
  const viewport = report.locator('.document-viewport');
  await viewport.hover();
  await page.mouse.wheel(0, -300);
  await expect(report.locator('.document-zoom')).not.toHaveText(kept!);
  await expect(card.locator('.document-browser')).toContainText('genügt es, in Word oder LibreOffice zu speichern');
  if (shots) { await page.waitForTimeout(500); await page.screenshot({ path: path.join(shots, 'document-zoomed.png') }); }

  // the built invitation: everything ticked, no marks
  const again = page.waitForResponse((r) => r.url().includes('/api/task/document'));
  await card.locator('.document-input').setInputFiles(solution);
  await again;
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(8);
  await expect(report.locator('.document-mark')).toHaveCount(0);
  await expect(report.locator('.document-done')).toContainText('Sauber gebaut');
  await expect(report.locator('.document-stamp')).toHaveText('Alle 8 Punkte');
  if (shots) await page.waitForTimeout(2200);
  if (shots) await page.screenshot({ path: path.join(shots, 'document-correct.png') });
  // the last version stays on the server, to go on with at home
  const latest = card.locator('.document-latest');
  await expect(latest).toContainText('Deine letzte Fassung: einladung.docx');
  const href = await latest.locator('a').getAttribute('href');
  const version = await page.request.get(href!);
  expect(version.ok()).toBe(true);
  expect((await version.body()).equals(fs.readFileSync(solution))).toBe(true);
  expect(version.headers()['content-disposition']).toContain("filename*=UTF-8''einladung.docx");
  const reloaded = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.reload();
  await reloaded;
  await expect(card.locator('.document-latest')).toContainText('einladung.docx');

  // the whole width of a desktop works as well
  await page.setViewportSize({ width: 1920, height: 1040 });
  await card.locator('.document-input').setInputFiles(starter);
  await expect(card).toHaveAttribute('data-status', 'wrong');
  await expect.poll(async () => {
    const box = await report.boundingBox();
    return box!.y >= 0 && box!.y + box!.height <= 1040;
  }).toBe(true);
  if (shots) await page.screenshot({ path: path.join(shots, 'document-full.png') });
  // a phone: the page on top, the checklist below, in one screen
  await page.setViewportSize({ width: 390, height: 800 });
  await card.locator('.document-input').setInputFiles(starter);
  await expect(card).toHaveAttribute('data-status', 'wrong');
  await expect(report.locator('.document-mark').first()).toBeVisible();
  await checkFits(report, 12);
  if (shots) { await page.screenshot({ path: path.join(shots, 'document-phone-whole.png') }); }
  await report.getByRole('button', { name: 'Seitenbreite' }).click();
  // the tools in one row
  const tools = await report.locator('.document-tools').boundingBox();
  expect(tools!.height).toBeLessThan(60);
  if (shots) { await report.scrollIntoViewIfNeeded(); await page.screenshot({ path: path.join(shots, 'document-phone.png') }); }
  // full screen, closed again with the back button
  await report.getByRole('button', { name: 'Vollbild' }).click();
  const fullReport = page.locator('.document-report.document-fullscreen');
  await expect(fullReport).toBeVisible();
  const covers = await fullReport.boundingBox();
  expect(covers!.y).toBeLessThanOrEqual(1);
  expect(covers!.height).toBeGreaterThan(790);
  if (shots) await page.screenshot({ path: path.join(shots, 'document-phone-full.png') });
  await page.goBack();
  await expect(report).not.toHaveClass(/document-fullscreen/);
  await expect(page).toHaveURL(/formatting-marks/);
});

// »Schrift mit Absicht«: the flyer in seven fonts fails six points, each
// with its places marked; the flyer set with intent passes all seven.
test('a flyer is checked for its fonts, sizes and emphasis', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/character-formatting');
  await states;
  const card = page.locator(".task-card[data-task='character-formatting/aushang']");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/character-formatting/02 Aushang.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(6);
  await expect(report.locator('.document-mark-word').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'flyer-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/character-formatting/stufen/aushang.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(7);
});

// »Listen und Tabstopps«: typed dashes and prices pushed with spaces fail
// three points; a real list and a right-aligned tab stop pass all four.
test('a packing list is checked for its list and tab stops', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/lists-and-tabs');
  await states;
  const card = page.locator(".task-card[data-task='lists-and-tabs/packliste']");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/lists-and-tabs/03 Packliste.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(3);
  if (shots) await page.screenshot({ path: path.join(shots, 'packing-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/lists-and-tabs/stufen/packliste.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(4);
});

// »Tabellen«: the programme in tabs fails two points; as a table with a
// repeated header row it passes all three.
test('a programme is checked for its table', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/word-tables');
  await states;
  const card = page.locator(".task-card[data-task='word-tables/programm']");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/word-tables/04 Programm.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(2);
  if (shots) await page.screenshot({ path: path.join(shots, 'table-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/word-tables/stufen/programm.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(3);
});

test('an article is checked for its pictures', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/pictures-in-text');
  await states;
  const card = page.locator(".task-card[data-task='pictures-in-text/artikel']");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/pictures-in-text/05 Kraniche.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  // stretched, in the line, typed numbers, no alt text
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(4);
  // the marks of the pictures lie on the page: the photo carries three points
  await expect(report.locator('.document-mark')).not.toHaveCount(0);
  if (shots) await page.screenshot({ path: path.join(shots, 'pictures-wrong.png') });
  // on http (the test's site) Chrome cannot keep hold of the file: the
  // button beside the page asks for it again instead of sending the copy
  // taken when it was chosen, which Word's next save makes unreadable
  const recheck = report.locator('.document-recheck');
  await expect(recheck).toHaveAttribute('aria-label', 'Datei neu wählen und prüfen');
  const [chooser] = await Promise.all([page.waitForEvent('filechooser'), recheck.click()]);
  await chooser.setFiles(path.join(repo, 'src/content/pictures-in-text/stufen/kraniche.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(5);
  if (shots) await page.screenshot({ path: path.join(shots, 'pictures-correct.png') });
});
