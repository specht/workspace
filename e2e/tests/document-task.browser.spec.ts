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

// »Seitenbreite« fits the whole sheet from edge to edge with its numbers in
// the margin, »Ganze Seite« the whole sheet in the view, each without much
// room left over
async function checkFits(report: Locator, slack: number) {
  const viewport = report.locator('.document-viewport');
  await report.getByRole('button', { name: 'Seitenbreite' }).click();
  await expect.poll(async () => {
    const view = (await viewport.boundingBox())!;
    const sheet = (await report.locator('.document-sheet').first().boundingBox())!;
    const badges = await report.locator('.document-badge').evaluateAll((all) => all.map((b) => b.getBoundingClientRect().left));
    return Math.min(...badges) >= view.x - 1 && sheet.x >= view.x - 1 &&
      // where the numbers need more room than the sheet's margin (a phone),
      // the sheet is a little narrower than the view, in its middle
      sheet.x + sheet.width <= view.x + view.width + 1 && sheet.width >= Math.min(view.width - slack, 0.85 * view.width);
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
  const card = page.locator(".task-card[data-task='formatting-marks/einladung']:visible");
  // the starting file with today's date and the Elternabend a Tuesday
  // after it (DocumentDates), as a student downloads it
  await expect(card.locator('.document-starter')).toHaveAttribute('href', '/api/task/document_starter/formatting-marks/einladung');
  const download = await page.request.get('/api/task/document_starter/formatting-marks/einladung');
  expect(download.headers()['content-disposition']).toContain('01%20Einladung.docx');
  const dated = testInfo.outputPath('01 Einladung.docx');
  fs.writeFileSync(dated, await download.body());

  // the starting file: everything but the text is open
  const checked = page.waitForResponse((r) => r.url().includes('/api/task/document'));
  await card.locator('.document-input').setInputFiles(dated);
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
  // its other dates are its text all the same; its date is found
  await expect(report.locator('.document-checklist li.document-ok')).toContainText('Der Text ist noch vollständig');
  await expect(report.locator('.document-checklist li.document-open', { hasText: 'Das Datum ist rechtsbündig' })).toHaveCount(1);
  await expect(report.locator('.document-sheet img')).toHaveCount(1);
  await expect(report.locator('.document-sheet img')).toHaveAttribute('src', /^\/api\/document_page\/[0-9a-f]{40}\.svg$/);
  const marks = report.locator('.document-mark');
  expect(await marks.count()).toBeGreaterThan(10);
  // one number per open point; pointing at a point brings out its places
  await expect(report.locator('.document-chip')).toHaveCount(6);
  await report.locator(".document-open[data-n='1']").hover();
  await expect(report.locator(".document-mark[data-n='1']").first()).toHaveClass(/document-active/);
  await expect(report).toHaveAttribute('data-active', '1');
  await page.mouse.move(0, 0);
  // which document: at the top of the result, where full screen shows it too
  await expect(report.locator('.document-status')).toContainText('01 Einladung.docx');
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
  // the mouse wheel alone scrolls the website and says how to zoom;
  // Ctrl and the wheel zoom at the pointer
  const viewport = report.locator('.document-viewport');
  await viewport.hover();
  await page.mouse.wheel(0, -100);
  await expect(report.locator('.document-wheel-hint')).toHaveClass(/document-wheel-hint-on/);
  await expect(report.locator('.document-zoom')).toHaveText(kept!);
  // a wheel that sends large and small steps (as some mice do): the zoom
  // only grows, and the point under the pointer stays where it is
  const box = (await viewport.boundingBox())!;
  const px = box.x + box.width / 2, py = box.y + box.height / 2;
  await page.mouse.move(px, py);
  const under = () => report.locator('.document-stage').evaluate((stage, at) => {
    const m = /translate\(([-\d.]+)px, ([-\d.]+)px\) scale\(([\d.]+)\)/.exec((stage as HTMLElement).style.transform)!;
    const tx = +m[1], ty = +m[2], z = +m[3];
    const v = stage.parentElement!.getBoundingClientRect();
    return { z, x: (at[0] - v.left - tx) / z, y: (at[1] - v.top - ty) / z };
  }, [px, py]);
  const first = await under();
  const seen = [first.z];
  await page.keyboard.down('Control');
  for (const dy of [-100, -12, -12, -100, -8, -100]) {
    await page.mouse.wheel(0, dy);
    seen.push((await under()).z);
  }
  await page.keyboard.up('Control');
  await page.waitForTimeout(800);
  const last = await under();
  seen.push(last.z);
  for (let i = 1; i < seen.length; i++) expect(seen[i]).toBeGreaterThanOrEqual(seen[i - 1] - 1e-6);
  expect(last.z).toBeGreaterThan(first.z * 1.3);
  expect(last.z).toBeLessThan(first.z * 2.2);
  expect(Math.abs(last.x - first.x)).toBeLessThan(2);
  expect(Math.abs(last.y - first.y)).toBeLessThan(2);
  await expect(report.locator('.document-zoom')).not.toHaveText(kept!);
  await expect(card.locator('.document-browser')).toContainText('genügt es, in LibreOffice oder Word zu speichern');
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
  // a new check while full screen is open: its result takes over the full
  // screen, which says when it was checked
  // the »prüfen« button beside the page shows the check running
  const sent = page.waitForRequest((r) => r.url().includes('/api/task/document') && r.method() === 'POST');
  await card.locator('.document-input').setInputFiles(solution);
  await sent;
  await expect(page.locator('.document-report.document-fullscreen .document-recheck')).toContainText('Prüft');
  await expect(page.locator('.document-report.document-fullscreen .document-checklist li.document-ok')).toHaveCount(8);
  await expect(page.locator('.document-report')).toHaveCount(1);
  await expect(page.locator('.document-report.document-fullscreen .document-status')).toContainText('Geprüft um');
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
  const card = page.locator(".task-card[data-task='character-formatting/aushang']:visible");
  const download = await page.request.get('/api/task/document_starter/character-formatting/aushang');
  const dated = testInfo.outputPath('02 Aushang.docx');
  fs.writeFileSync(dated, await download.body());
  await card.locator('.document-input').setInputFiles(dated);
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
  const card = page.locator(".task-card[data-task='lists-and-tabs/packliste']:visible");
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
  const card = page.locator(".task-card[data-task='word-tables/programm']:visible");
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
  const card = page.locator(".task-card[data-task='pictures-in-text/artikel']:visible");
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

test('a diary is checked for its heading styles', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/paragraph-styles');
  await states;
  const card = page.locator(".task-card[data-task='paragraph-styles/tagebuch']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/paragraph-styles/06 Reisetagebuch.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  // the days and their parts are no headings, the title is by hand, the
  // pasted text keeps its font, both styles are as they were
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(6);
  if (shots) await page.screenshot({ path: path.join(shots, 'styles-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/paragraph-styles/stufen/noch-von-hand.docx'));
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(1);
  await expect(report.locator('.document-checklist li.document-open')).toContainText('nicht von Hand formatiert');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/paragraph-styles/stufen/reisetagebuch.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(9);
});

test('a report of six pages is checked for its breaks and contents', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/pages-and-contents');
  await states;
  const card = page.locator(".task-card[data-task='pages-and-contents/bericht']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/pages-and-contents/07 Reisebericht.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(3);
  // every page drawn, the page in view shown, the next one a click away
  await expect(report.locator('.document-sheet')).toHaveCount(6);
  const where = report.locator('.document-page');
  await expect(where).toHaveText('1 / 6');
  await report.getByRole('button', { name: 'Nächste Seite' }).click();
  await expect(where).toHaveText('2 / 6');
  await report.getByRole('button', { name: 'Ganze Seite' }).click();
  await report.getByRole('button', { name: 'Nächste Seite' }).click();
  await expect(where).toHaveText('3 / 6');
  // the empty paragraphs are marked, the runs of them before each day
  expect(await report.locator(".document-mark[data-n='1']").count()).toBeGreaterThanOrEqual(5);
  // the toolbar stays one row at half a Full HD screen
  const tools = (await report.locator('.document-tools').boundingBox())!;
  expect(tools.height).toBeLessThan(50);
  if (shots) await page.screenshot({ path: path.join(shots, 'pages-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/pages-and-contents/stufen/reisebericht.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-sheet')).toHaveCount(6);
  if (shots) await page.screenshot({ path: path.join(shots, 'pages-correct.png') });
});

test('an article of ones own is checked on every point, and the hand-in shows its marked pages', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/trip-newspaper');
  await states;
  const card = page.locator(".task-card[data-task='trip-newspaper/artikel']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-newspaper/stufen/artikel-eilig.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  // (the hurried article is ragged right, without hyphenation, too)
  await expect(card.locator('.document-report .document-checklist li.document-open')).toHaveCount(8);
  // the hand-in: the last checked version with its marks, as the teacher sees it
  await page.goto('/abgabe/dokument/trip-newspaper/artikel');
  await expect(page.locator('.document-handin-file')).toContainText('artikel-eilig.docx');
  const report = page.locator('.document-handin-card .document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(8);
  await expect(report.locator('.document-sheet img')).toHaveCount(1);
  await expect(report.locator('.document-recheck')).toHaveCount(0);
  if (shots) await page.screenshot({ path: path.join(shots, 'handin.png') });
  const href = await page.locator('.document-handin-file a[download]').getAttribute('href');
  const file = await page.request.get(href!);
  expect(file.ok()).toBe(true);
  expect((await file.body()).equals(fs.readFileSync(path.join(repo, 'src/content/trip-newspaper/stufen/artikel-eilig.docx')))).toBe(true);
});

// The article exported as PDF (accept: pdf): read by poppler, its pages
// shown, its fonts and words counted; the document itself is turned away
// there, and a PDF where the document is checked.
test('the article as PDF is checked, and each card says which file it wants', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/trip-newspaper');
  await states;
  const card = page.locator(".task-card[data-task='trip-newspaper/pdf']:visible");
  await expect(card.locator('.document-starter')).toHaveCount(0);
  await expect(card.locator('.document-input')).toHaveAttribute('accept', '.pdf');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-newspaper/stufen/artikel.docx'));
  // a message, no verdict
  await expect(card).toContainText('Hier gehört das PDF hin');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-newspaper/stufen/vorlage.pdf'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  await expect(card.locator('.document-report .document-checklist li.document-open')).toContainText('Wörter');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-newspaper/stufen/artikel.pdf'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(card.locator('.document-report .document-checklist li.document-ok')).toHaveCount(3);
  await expect(card.locator('.document-report .document-sheet img').first()).toBeVisible();
  if (shots) await page.screenshot({ path: path.join(shots, 'pdf-correct.png') });
  // a PDF where the document is checked
  const article = page.locator(".task-card[data-task='trip-newspaper/artikel']:visible");
  await article.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-newspaper/stufen/artikel.pdf'));
  await expect(article).toContainText('Das ist eine PDF-Datei');
});

// »Kopfzeile, Fußzeile, Fußnoten«: what is typed at the top and bottom of
// the pages goes into the header and footer as fields, the notes numbered
// by hand become footnotes.
test('a report is checked for its header, date, page number and footnotes', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/headers-and-footnotes');
  await states;
  const card = page.locator(".task-card[data-task='headers-and-footnotes/bericht']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/headers-and-footnotes/08 Reisebericht.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(4);
  await expect(report.locator('.document-checklist li.document-ok')).toContainText('Der Text ist noch vollständig');
  // the typed heads and the notes numbered by hand are marked on the pages
  await expect(report.locator('.document-mark').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'headers-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/headers-and-footnotes/stufen/datum-fest.docx'));
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(1);
  await expect(report.locator('.document-checklist li.document-open')).toContainText('Das Datum als Feld in der Kopfzeile');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/headers-and-footnotes/stufen/reisebericht.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(5);
});

// »Die Zeitungsseite«: two columns, narrow margins, and in »Standard«
// justified text with hyphenation, a first-line indent, single spacing.
test('an article is checked for its newspaper layout', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/newspaper-page');
  await states;
  const card = page.locator(".task-card[data-task='newspaper-page/seite']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/newspaper-page/09 Kraniche.docx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(6);
  if (shots) await page.screenshot({ path: path.join(shots, 'newspaper-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/newspaper-page/stufen/ohne-trennung.docx'));
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(1);
  await expect(report.locator('.document-checklist li.document-open')).toContainText('Silbentrennung');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/newspaper-page/stufen/zeitungsseite.docx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(7);
});

// A spreadsheet (»Zellen, die rechnen«): the service reads every cell,
// draws the sheet and changes a price in its copy; typed results do not
// follow, formulas do.
test('a spreadsheet is checked for its formulas and whether it calculates', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/spreadsheet-cells');
  await states;
  // the switch names Excel on a page of spreadsheets
  await expect(page.locator(".code-language-switch-office[data-office-kind='sheet']")).toContainText('Excel');
  const card = page.locator(".task-card[data-task='spreadsheet-cells/kosten']:visible");
  await expect(card.locator('.document-input')).toHaveAttribute('accept', '.ods,.xlsx,.xls');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/spreadsheet-cells/01 Kosten.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(3);
  await expect(report.locator('.document-checklist')).toContainText('Rechnet mit, wenn sich eine Zahl ändert');
  await expect(report.locator('.document-mark').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'sheet-wrong.png') });
  // a text document where a table belongs
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/formatting-marks/stufen/einladung.docx'));
  await expect(card).toContainText('Das ist ein Textdokument');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-cells/stufen/kosten.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(5);
  if (shots) await page.screenshot({ path: path.join(shots, 'sheet-correct.png') });
});

// »Zahlen in Form«: amounts typed as text, which SUMME leaves out, and
// number formats; the formats as Excel stores them pass too (its euro as
// text in the format).
test('a spreadsheet is checked for amounts typed as text and for its number formats', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/number-formats');
  await states;
  const card = page.locator(".task-card[data-task='number-formats/einzahlungen']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/number-formats/02 Einzahlungen.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(3);
  await expect(report).toContainText('als Text, nicht als Zahl');
  await expect(report.locator('.document-mark-cell').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'formats-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/number-formats/stufen/ohne-format.xlsx'));
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(1);
  await expect(report).toContainText('als Währung formatiert');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/number-formats/stufen/excel-format.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(6);
  if (shots) await page.screenshot({ path: path.join(shots, 'formats-correct.png') });
});

// »Feste Bezüge«: a formula filled down without $ divides by the wrong
// cells and shows #DIV/0!; with $B$2 it calculates, also when the check
// changes the number of children in its copy.
test('a spreadsheet is checked for absolute references and errors', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/absolute-references');
  await states;
  const card = page.locator(".task-card[data-task='absolute-references/anteile']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/absolute-references/stufen/ohne-dollar.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report).toContainText('Fester Bezug auf B2');
  await expect(report).toContainText('#DIV/0!');
  await expect(report.locator('.document-mark-cell').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'absolute-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/absolute-references/stufen/anteile.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(9);
});

// »Auswerten«: functions over the answers, ranges that miss the last rows
// count too few; WENN answers with a text, which the check compares as
// shown, also after its probe.
test('a spreadsheet is checked for functions and their ranges', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/spreadsheet-functions');
  await states;
  const card = page.locator(".task-card[data-task='spreadsheet-functions/umfrage']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-functions/stufen/zu-kurz.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report).toContainText('richtig wäre 26');
  if (shots) await page.screenshot({ path: path.join(shots, 'functions-wrong.png') });
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-functions/stufen/umfrage.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(7);
});

// »Sortieren, Filtern, Hervorheben«: a column of names sorted alone is
// caught by comparing the rows with the starting file; the solution has a
// filter and a conditional format, whose colour the drawn sheet shows.
test('a spreadsheet is checked for sorting whole rows, a filter and a conditional format', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/sort-and-filter');
  await states;
  const card = page.locator(".task-card[data-task='sort-and-filter/zahlungen']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/sort-and-filter/stufen/nur-namen.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report).toContainText('eine Spalte allein');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/sort-and-filter/stufen/zahlungen.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(5);
  if (shots) await page.screenshot({ path: path.join(shots, 'sort-correct.png') });
});

// »Diagramme«: the check reads each chart's type, title, axis titles and
// data; a pie of the stars without a title fails, the two charts pass.
test('a spreadsheet is checked for its charts', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/spreadsheet-charts');
  await states;
  const card = page.locator(".task-card[data-task='spreadsheet-charts/diagramme']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-charts/stufen/falsch.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report).toContainText('keinen Titel');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-charts/stufen/diagramme.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(3);
  if (shots) await page.screenshot({ path: path.join(shots, 'charts-correct.png') });
});

// »Die Abrechnung der Klassenfahrt«, the hand-in: the table with every
// point of the series, then the PDF exported from it.
test('the accounts of the trip are handed in as a table and a PDF', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/trip-accounts');
  await states;
  const card = page.locator(".task-card[data-task='trip-accounts/abrechnung']:visible");
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-accounts/stufen/getippt.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  await expect(card.locator('.document-report')).toContainText('ABRUNDEN');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-accounts/stufen/abrechnung.xlsx'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(card.locator('.document-report .document-checklist li.document-ok')).toHaveCount(11);
  if (shots) await page.screenshot({ path: path.join(shots, 'accounts-correct.png') });
  const pdf = page.locator(".task-card[data-task='trip-accounts/pdf']:visible");
  await expect(pdf.locator('.document-input')).toHaveAttribute('accept', '.pdf');
  await pdf.locator('.document-input').setInputFiles(path.join(repo, 'src/content/trip-accounts/stufen/abrechnung.pdf'));
  await expect(pdf).toHaveAttribute('data-status', 'correct');
});

// »Eine Folie, ein Gedanke«: a presentation, its slides read and drawn; the
// card offers the .odp to LibreOffice and the .pptx to PowerPoint, and
// checks both
// Presentations are shelved (docs/plan-praesentationen.md): /good-slides is in
// no catalog and answers 404. Back with `.good-slides` in sections.yaml.
test.skip('a presentation is checked for titles, words, sizes and notes', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 960, height: 1040 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/good-slides');
  await states;
  // the switch names PowerPoint on a page of slides
  await expect(page.locator(".code-language-switch-office[data-office-kind='slides']")).toContainText('PowerPoint');
  const card = page.locator(".task-card[data-task='good-slides/folien']:visible");
  await expect(card.locator('.document-input')).toHaveAttribute('accept', '.odp,.pptx,.ppt');
  await expect(card.locator('.document-starter')).toHaveAttribute('href', /01%20Kraniche\.(odp|pptx)$/);
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/static/good-slides/01 Kraniche.pptx'));
  await expect(card).toHaveAttribute('data-status', 'wrong');
  const report = card.locator('.document-report');
  await expect(report.locator('.document-checklist li.document-open')).toHaveCount(3);
  await expect(report.locator('.document-checklist')).toContainText('Jeder Titel sagt, worum es auf der Folie geht');
  await expect(report.locator('.document-mark').first()).toBeAttached();
  if (shots) await page.screenshot({ path: path.join(shots, 'slides-wrong.png') });
  // a table where a presentation belongs
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/spreadsheet-cells/stufen/kosten.xlsx'));
  await expect(card).toContainText('Das ist eine Tabelle. Hier gehört deine Präsentation hin');
  await card.locator('.document-input').setInputFiles(path.join(repo, 'src/content/good-slides/stufen/folien.odp'));
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-checklist li.document-ok')).toHaveCount(7);
  if (shots) await page.screenshot({ path: path.join(shots, 'slides-correct.png') });
});
