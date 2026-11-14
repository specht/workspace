// Pictures for »Leistungsüberprüfungen vorbereiten« (src/content/lehrmaterial/
// lehrmaterial-leistungsueberpruefung), from the running development site:
//
//   HS_PICTURES=1 npx playwright test --project=pictures exam-guide
//
// A folder with a small exam in the e2e teacher's Workspace, the dialog »Aus
// deinem Workspace« with it, then the exam built in
// yellow with the frame and its preview Workspace opened. The folder and the
// exam (with its preview) are deleted afterwards. The Workspace picture is
// 16:9, 1280 x 720 at twice the pixels, and the card's picture too; written
// as .webp next to the page (needs ImageMagick's convert).
import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

const PAGE = path.resolve(__dirname, '../../src/content/lehrmaterial/lehrmaterial-leistungsueberpruefung');
const REPO = path.resolve(__dirname, '../..');
// what the pictures show comes from these files (recreate-screenshots.rb
// --status calls them stale when one changes)
const INPUTS = [
  'e2e/tests/exam-guide.pictures.ts',
  'src/ruby/include/exam_builder_page.rb', 'src/ruby/include/exam_builder.rb', 'src/ruby/include/exams_page.rb',
  'src/ruby/include/test_workspace_package.rb', 'src/static/include/exams.js', 'src/static/include/default.css',
];
const TEACHER = 'e2e-teacher@example.com';
const FOLDER = 'loops-exam';
const taken: string[] = [];

function inputsHash(): string {
  const hash = crypto.createHash('sha256');
  for (const file of [...INPUTS].sort()) {
    hash.update(file + '\n');
    hash.update(fs.readFileSync(path.join(REPO, file)));
    hash.update('\n');
  }
  return hash.digest('hex');
}

function fsTag(email: string): string {
  const hex = crypto.createHash('sha256').update(email).digest('hex');
  return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

const FILES: Record<string, string> = {
  'AUFGABE.md': `# Klassenarbeit: Schleifen

Du hast 45 Minuten. Öffne zuerst diese Datei, dann \`average.py\`.

## Aufgabe 1 (4 Punkte)

Schreib die Funktion \`average(grades)\`: Sie bekommt eine Liste von Noten
und gibt ihren Durchschnitt zurück. Für eine leere Liste gibt sie \`None\`
zurück.

## Aufgabe 2 (6 Punkte)

In \`data/grades.csv\` steht in jeder Zeile ein Fach und eine Note. Gib für
jedes Fach den Durchschnitt seiner Noten aus, mit einer Stelle nach dem
Komma.

Mit \`python3 -m pytest tests\` prüfst du Aufgabe 1.
`,
  'average.py': `def average(grades):
    # Aufgabe 1
    pass


def averages_by_subject(path):
    # Aufgabe 2
    pass
`,
  'data/grades.csv': 'Mathe,2\nDeutsch,1\nMathe,3\nInformatik,1\nDeutsch,2\nInformatik,2\n',
  'tests/test_average.py': `from average import average


def test_average():
    assert average([1, 2, 3]) == 2


def test_empty():
    assert average([]) is None
`,
};

const ruby = (script: string, input?: string) =>
  execFileSync('docker', ['exec', '-i', 'workspace_ruby_1', 'sh', '-c', script], { input }).toString();

function convert(png: string, name: string) {
  execFileSync('convert', [png, '-quality', '85', '-define', 'webp:method=6', path.join(PAGE, name + '.webp')]);
  taken.push(name + '.webp');
}

test('pictures for the exam guide', async ({ browser }, testInfo) => {
  test.setTimeout(600_000);
  const out = testInfo.outputPath();
  fs.mkdirSync(out, { recursive: true });
  const home = `/user/${fsTag(TEACHER)}/workspace`;
  const dir = `${home}/${FOLDER}`;
  ruby(`rm -rf ${dir} && mkdir -p ${dir}/data ${dir}/tests ${dir}/__pycache__`);
  for (const [file, text] of Object.entries(FILES)) ruby(`cat > ${dir}/${file}`, text);
  ruby(`printf 'x' > ${dir}/__pycache__/average.cpython-312.pyc && chown -R 1000:1000 ${home}`);

  const context = await browser.newContext({ viewport: { width: 1280, height: 1500 }, deviceScaleFactor: 2 });
  const teacher = await context.newPage();
  let tag = '';
  try {
    await loginAsE2eUser(teacher, TEACHER, testInfo);
    await teacher.goto('/klasse?group=E2E&tab=pruefungen');
    await teacher.locator('#bu_exam_builder').click();
    const modal = teacher.locator('#exam_builder_modal');
    await teacher.locator('#eb_folder').selectOption(FOLDER);
    await expect(modal.locator('.eb-item[data-path="average.py"]')).toBeVisible();
    await teacher.locator('#eb_name').fill('Klassenarbeit Schleifen');
    await modal.locator('.eb-item[data-path="tests"] .eb-print').uncheck();
    await modal.locator('.eb-swatch', { hasText: 'Gelb' }).click();
    await modal.getByLabel('Rahmen in der Farbe, der Editor bleibt dunkel').check();
    // the page frames the picture: the dialog without its own border, tall
    // enough that nothing scrolls; two wide pictures, the files and below
    // them the colour and Git
    await teacher.evaluate(() => {
      const c = document.querySelector('#exam_builder_modal .modal-content') as HTMLElement;
      c.style.border = '0';
      c.style.borderRadius = '0';
      c.style.boxShadow = 'none';
    });
    await teacher.waitForTimeout(400);
    const content = (await modal.locator('.modal-content').boundingBox())!;
    const summary = (await teacher.locator('#eb_summary').boundingBox())!;
    const settings = (await modal.locator('.eb-settings').boundingBox())!;
    const files = path.join(out, 'aus-dem-workspace.png');
    await teacher.screenshot({ path: files, clip: { x: content.x, y: content.y, width: content.width, height: summary.y + summary.height + 18 - content.y } });
    convert(files, 'aus-dem-workspace');
    const colour = path.join(out, 'aus-dem-workspace-farbe.png');
    await teacher.screenshot({ path: colour, clip: { x: content.x, y: settings.y - 18, width: content.width, height: settings.height + 26 } });
    convert(colour, 'aus-dem-workspace-farbe');

    // the exam, its preview Workspace in yellow with AUFGABE.md open
    await teacher.locator('#bu_eb_build').click();
    await expect(teacher.locator('#test_archive_upload_status')).toContainText('angelegt', { timeout: 120_000 });
    tag = (await (await teacher.request.post('/api/get_my_test_archives', { data: {} })).json()).entries
      .find((e: any) => e.filename === 'Klassenarbeit Schleifen.tar.gz').tag;
    await teacher.setViewportSize({ width: 1280, height: 720 });
    const row = teacher.locator('#table_my_test_archives tr', { hasText: 'Klassenarbeit Schleifen' });
    const popup = teacher.waitForEvent('popup', { timeout: 240_000 });
    await row.getByRole('button', { name: 'Workspace', exact: true }).click();
    await teacher.locator('#__template_modal').getByRole('button', { name: 'Frischen Workspace öffnen' }).click();
    const workspace = await popup;
    await workspace.setViewportSize({ width: 1280, height: 720 });
    await expect(workspace.locator('.monaco-workbench')).toBeVisible({ timeout: 180_000 });
    await expect(workspace.locator('.explorer-folders-view')).toBeVisible({ timeout: 60_000 });
    await workspace.waitForTimeout(3000);
    await workspace.locator('.explorer-folders-view .monaco-list-row', { hasText: 'AUFGABE.md' }).click();
    await expect(workspace.locator('.tab.active', { hasText: 'AUFGABE.md' })).toBeVisible({ timeout: 30_000 });
    await workspace.locator('.explorer-folders-view .monaco-list-row', { hasText: 'average.py' }).click();
    await workspace.locator('.explorer-folders-view .monaco-list-row', { hasText: 'AUFGABE.md' }).click();
    await workspace.mouse.move(640, 700);
    await workspace.waitForTimeout(2000);
    const frame = path.join(out, 'workspace-rahmen.png');
    await workspace.screenshot({ path: frame });
    convert(frame, 'workspace-rahmen');

    fs.writeFileSync(path.join(PAGE, '.pictures.json'), JSON.stringify({
      script: 'e2e/tests/exam-guide.pictures.ts',
      command: 'cd e2e && HS_PICTURES=1 npx playwright test --project=pictures exam-guide',
      inputs: INPUTS, hash: inputsHash(), pictures: [...taken].sort(),
    }, null, 2) + '\n');
  } finally {
    ruby(`rm -rf ${dir}`);
    if (tag) await teacher.request.post('/api/delete_test', { data: { tag } });
    await context.close();
  }
});
