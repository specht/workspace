import { execFileSync } from 'node:child_process';
import crypto from 'node:crypto';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';

// A Leistungsüberprüfung from a folder of the teacher's Workspace (the dialog
// »Aus deinem Workspace«, exam_builder_page.rb, exams.js): the folder's files
// as a tree, a build folder and a Git history in a subfolder unticked, a link
// that cannot be ticked; one file left out of the printout, a colour and the
// frame; the package that is stored holds what was ticked and the
// config.yaml of the dialog. The folder and the exam are deleted at the end.

const TEACHER = 'e2e-teacher@example.com';
const FOLDER = 'e2e-klassenarbeit';

function fsTag(email: string): string {
  const hex = crypto.createHash('sha256').update(email).digest('hex');
  return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

const ruby = (script: string) => execFileSync('docker', ['exec', 'workspace_ruby_1', 'sh', '-c', script]).toString();

test('an exam from a folder of the Workspace: what is ticked, the printout, the colour', async ({ page }, testInfo) => {
  const dir = `/user/${fsTag(TEACHER)}/workspace/${FOLDER}`;
  ruby(`rm -rf ${dir} && mkdir -p ${dir}/tests ${dir}/__pycache__ ${dir}/projekt/.git && cd ${dir} && ` +
    `printf '# Aufgabe\\n' > AUFGABE.md && printf 'print(1)\\n' > main.py && printf 'assert True\\n' > tests/test_main.py && ` +
    `printf 'x' > __pycache__/main.pyc && printf 'ref: refs/heads/main\\n' > projekt/.git/HEAD && printf 'a\\n' > projekt/a.txt && ` +
    `ln -s /etc/hostname link.txt && chown -R 1000:1000 /user/${fsTag(TEACHER)}/workspace`);
  let tag = '';
  try {
    await loginAsE2eUser(page, TEACHER, testInfo);
    // a shared Workspace: the dialog warns and ends the sharing
    await page.request.post('/api/share_server', { data: {} });
    await page.goto('/klasse?tab=pruefungen');
    await page.locator('#bu_exam_builder').click();
    await expect(page.locator('#eb_shared')).toBeVisible();
    await page.locator('#bu_eb_unshare').click();
    await expect(page.locator('#eb_shared')).toBeHidden();
    await page.goto('/profil');
    await expect(page.locator('#bu_share')).toBeVisible();
    await page.goto('/klasse?tab=pruefungen');
    await page.locator('#bu_exam_builder').click();
    const modal = page.locator('#exam_builder_modal');
    await expect(modal).toBeVisible();
    await page.locator('#eb_folder').selectOption(FOLDER);
    const row = (path: string) => modal.locator(`.eb-item[data-path="${path}"]`);
    await expect(row('main.py').locator('.eb-take')).toBeChecked();
    await expect(row('__pycache__')).toContainText('Build-Ordner');
    await expect(row('__pycache__').locator('.eb-take')).not.toBeChecked();
    await expect(row('link.txt').locator('.eb-take')).toBeDisabled();
    await expect(row('projekt/.git')).toContainText('Git-Verlauf');
    await expect(row('projekt/.git').locator('.eb-take')).not.toBeChecked();
    await expect(page.locator('#eb_name')).toHaveValue(FOLDER);

    // the Git history of the subfolder goes with it: a Git task there
    await row('projekt/.git').locator('.eb-take').check();
    await expect(page.locator('#eb_git_note')).toContainText('projekt/');
    // the tests stay out of the printout; magenta, as a frame
    await row('tests/test_main.py').locator('.eb-print').uncheck();
    await expect(row('tests').locator('.eb-print')).not.toBeChecked();
    await modal.locator('.eb-swatch', { hasText: 'Magenta' }).click();
    await modal.getByLabel('Rahmen in der Farbe, der Editor bleibt dunkel').check();
    await expect(page.locator('#eb_preview')).toHaveAttribute('data-color', 'magenta');
    // the whole theme only in blue and red
    await modal.getByLabel('Alles in der Farbe (nur Blau und Rot)').check();
    await expect(modal.locator('input[name=eb_color][value=magenta]')).toBeDisabled();
    await expect(modal.locator('input[name=eb_color][value=blue]')).toBeChecked();
    await modal.getByLabel('Rahmen in der Farbe, der Editor bleibt dunkel').check();
    await modal.locator('.eb-swatch', { hasText: 'Magenta' }).click();

    await page.locator('#bu_eb_build').click();
    await expect(page.locator('#test_archive_upload_status')).toContainText('angelegt', { timeout: 60_000 });
    await expect(modal).toBeHidden();
    const entry = (await (await page.request.post('/api/get_my_test_archives', { data: {} })).json()).entries
      .find((e: any) => e.filename === `${FOLDER}.tar.gz`);
    tag = entry.tag;
    await expect(page.locator('#table_my_test_archives')).toContainText(FOLDER);

    // the stored package: what was ticked and the dialog's config.yaml
    const archive = ruby('ls -t /internal/test_archives | head -1').trim();
    const listed = ruby(`tar -tzf /internal/test_archives/${archive}`).split('\n').filter(Boolean).sort();
    expect(listed).toEqual(['.workspace/', '.workspace/config.yaml', 'AUFGABE.md', 'main.py', 'projekt/.git/HEAD', 'projekt/a.txt', 'tests/test_main.py']);
    const config = ruby(`tar -xzOf /internal/test_archives/${archive} .workspace/config.yaml`);
    expect(config).toContain('color: magenta');
    expect(config).toContain('look: frame');
    expect(config).toContain('mode: fresh');
    expect(config).toContain('- tests/test_main.py');
  } finally {
    await page.request.post('/api/unshare_server', { data: {} });
    ruby(`rm -rf ${dir}`);
    if (tag) await page.request.post('/api/delete_test', { data: { tag } });
  }
});
