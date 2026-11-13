import path from 'path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// The extras of a document page (»Was das Auge nicht sieht«): a passed card
// offers another round after many checks and something harder after few
// (next: in tasks.yaml); a document full of traps (traps: true) says only
// how many are found, shows each once it is fixed, and offers tips for each
// hidden one, one by one.

const repo = path.resolve(__dirname, '..', '..');
const content = path.join(repo, 'src/content/formatting-marks/stufen');
const statics = path.join(repo, 'src/static/formatting-marks');

test('a document with traps hides them until they are found, with tips for each', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 1280, height: 1000 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  for (const task of ['einladung', 'fallen']) await page.request.post('/api/task/delete', { data: { task: `formatting-marks/${task}` } });
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/formatting-marks');
  await states;

  // the invitation passed at the first check: the traps are offered
  const card = page.locator(".task-card[data-task='formatting-marks/einladung']:visible");
  let checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await card.locator('.document-input').setInputFiles(path.join(content, 'einladung.docx'));
  await checked;
  await expect(card).toHaveAttribute('data-status', 'correct');
  const offer = card.locator('.document-offer');
  await expect(offer).toContainText('Schnell geschafft');
  await expect(offer.getByRole('link', { name: 'Sechs Fallen' })).toHaveAttribute('href', '#aufgabe-fallen');

  // the letter as downloaded: nothing found, nothing said about where
  const traps = page.locator(".task-card[data-task='formatting-marks/fallen']:visible");
  checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await traps.locator('.document-input').setInputFiles(path.join(statics, '01 Brief an die Jugendherberge.docx'));
  await checked;
  const report = traps.locator('.document-report');
  await expect(report.locator('.document-verdict')).toHaveText('0 von 6 Fallen gefunden.');
  await expect(report.locator('li.document-trap')).toHaveCount(6);
  await expect(report.locator('li.document-trap').first()).toContainText('Falle 1: noch versteckt');
  await expect(report.locator('.document-mark')).toHaveCount(0);
  await expect(report).not.toContainText('Leerzeichen');
  // the text is a point as usual
  await expect(report.locator('li.document-ok')).toContainText('Der Text ist noch vollständig');
  // what the check read would show the traps: not there
  await expect(traps.locator('.task-feedback')).not.toContainText('So hat LibreOffice deine Absätze gelesen');

  // the tips of a trap, one by one
  const third = report.locator('li.document-trap').nth(2);
  await third.getByRole('button', { name: /Tipp/ }).click();
  await expect(third.locator('.document-trap-hints li')).toHaveCount(1);
  await expect(third.locator('.document-trap-hints li')).toContainText('zwischen zwei Sätzen');
  // the next one a minute later, as every tip in steps
  const next = third.getByRole('button', { name: /Noch ein Tipp/ });
  await expect(next).toBeDisabled();
  await expect(next).toContainText(/in \d+ Sekunden/);
  await traps.screenshot({ path: testInfo.outputPath('traps-hidden.png') });

  // all fixed: every trap named, ticked
  checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await traps.locator('.document-input').setInputFiles(path.join(content, 'brief.docx'));
  await checked;
  await expect(traps).toHaveAttribute('data-status', 'correct');
  await expect(report.locator('.document-verdict')).toHaveText('Alle 6 Fallen gefunden.');
  await expect(report.locator('li.document-trap')).toHaveCount(0);
  await expect(report.locator('li.document-ok')).toHaveCount(7);
  await expect(report).toContainText('Nach »Lehrkräfte.« standen zwei Leerzeichen.');
  await expect(report).toContainText('Keine Falle übersehen');
  await traps.screenshot({ path: testInfo.outputPath('traps-found.png') });

  for (const task of ['einladung', 'fallen']) await page.request.post('/api/task/delete', { data: { task: `formatting-marks/${task}` } });
});

test('a card passed after many checks offers another round', async ({ page, e2eEmail }, testInfo) => {
  await loginAsE2eUser(page, e2eEmail, testInfo);
  await page.request.post('/api/task/delete', { data: { task: 'formatting-marks/einladung' } });
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/formatting-marks');
  await states;
  const card = page.locator(".task-card[data-task='formatting-marks/einladung']:visible");
  for (const file of [path.join(statics, '01 Einladung.docx'), path.join(statics, '01 Einladung.docx'), path.join(content, 'einladung.docx')]) {
    const checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
    await card.locator('.document-input').setInputFiles(file);
    await checked;
  }
  await expect(card).toHaveAttribute('data-status', 'correct');
  await expect(card.locator('.document-offer')).toContainText('ein paar Versuche');
  await expect(card.locator('.document-offer').getByRole('link', { name: 'Noch eine Runde' })).toHaveAttribute('href', '#aufgabe-runde');
  await page.request.post('/api/task/delete', { data: { task: 'formatting-marks/einladung' } });
});

test('sheet 2: seven traps, and the two handles checked for Format übertragen and raised digits', async ({ page, e2eEmail }, testInfo) => {
  await page.setViewportSize({ width: 1280, height: 1000 });
  await loginAsE2eUser(page, e2eEmail, testInfo);
  const sheet = path.join(repo, 'src/content/character-formatting/stufen');
  const starters = path.join(repo, 'src/static/character-formatting');
  for (const task of ['handgriffe', 'fallen']) await page.request.post('/api/task/delete', { data: { task: `character-formatting/${task}` } });
  const states = page.waitForResponse((r) => r.url().includes('/api/task/states'));
  await page.goto('/character-formatting');
  await states;

  const handles = page.locator(".task-card[data-task='character-formatting/handgriffe']:visible");
  let checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await handles.locator('.document-input').setInputFiles(path.join(starters, '02 Messprotokoll.docx'));
  await checked;
  await expect(handles.locator('.document-checklist li.document-open')).toHaveCount(2);
  await expect(handles.locator('.document-report')).toContainText('Noch auf der Zeile');
  checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await handles.locator('.document-input').setInputFiles(path.join(sheet, 'messprotokoll.docx'));
  await checked;
  await expect(handles).toHaveAttribute('data-status', 'correct');

  const traps = page.locator(".task-card[data-task='character-formatting/fallen']:visible");
  checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await traps.locator('.document-input').setInputFiles(path.join(starters, '02 Dankesbrief.docx'));
  await checked;
  await expect(traps.locator('.document-verdict')).toHaveText('0 von 7 Fallen gefunden.');
  await expect(traps.locator('li.document-trap')).toHaveCount(7);
  checked = page.waitForResponse((r) => r.url().endsWith('/api/task/document'));
  await traps.locator('.document-input').setInputFiles(path.join(sheet, 'dankesbrief.docx'));
  await checked;
  await expect(traps.locator('.document-verdict')).toHaveText('Alle 7 Fallen gefunden.');
  await expect(traps.locator('.document-report')).toContainText('Ein Satz stand in Arial, alle anderen in Calibri.');

  for (const task of ['handgriffe', 'fallen']) await page.request.post('/api/task/delete', { data: { task: `character-formatting/${task}` } });
});
