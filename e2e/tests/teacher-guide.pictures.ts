// Pictures for »Einstieg für Lehrkräfte« (src/content/lehrmaterial/
// lehrmaterial-einstieg), from the running development site:
//
//   HS_PICTURES=1 npx playwright test --project=pictures
//
// The e2e accounts answer a few tasks of »Interaktive Geschichten« and one
// hands in a small page; in the pictures they appear under made-up names as
// class 9b. Everything they did is deleted again afterwards. Each picture is
// 16:9, 1280 x 720 at twice the pixels (2560 x 1440), written as .webp next to the
// guide (needs ImageMagick's convert).
import { execFileSync } from 'node:child_process';
import path from 'node:path';
import type { Page } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

const GUIDE = path.resolve(__dirname, '../../src/content/lehrmaterial/lehrmaterial-einstieg');
const NAMES: Record<string, string> = {
  'E2E Auth Student 0': 'Aylin Demir', 'E2E Auth Student 1': 'Ben Hoffmann', 'E2E Auth Student 2': 'Clara Weber',
  'E2E Auth Student 3': 'David Novak', 'E2E Auth Student 4': 'Elif Kaya', 'E2E Auth Student 5': 'Finn Schulz',
  'E2E Auth Student 6': 'Greta Lange', 'E2E Auth Student 7': 'Hannes Vogel', 'E2E Peer': 'Ida Krüger',
  'E2E Worker 0': 'Jonas Becker', 'E2E Worker 1': 'Kim Nguyen', 'E2E Worker 2': 'Lea Fischer', 'E2E Worker 3': 'Mats Wolf',
  'E2E Worker 4': 'Nora Schmitt', 'E2E Worker 5': 'Ole Brandt', 'E2E Worker 6': 'Paula Richter', 'E2E Worker 7': 'Quentin Roth',
  'E2E Teacher': 'Frau Berger',
};

// Answers that make a believable class: some right, some wrong, one stuck.
const ANSWERS: [string, string, string][] = [
  ['e2e-0@example.com', 'bif/punkt', '1'], ['e2e-0@example.com', 'bif/anfangswert', '1'],
  ['e2e-1@example.com', 'bif/punkt', '1'], ['e2e-1@example.com', 'bif/anfangswert', '1'],
  ['e2e-2@example.com', 'bif/punkt', '0'],
  ['e2e-3@example.com', 'bif/punkt', '1'], ['e2e-3@example.com', 'bif/anfangswert', '3'],
  ['e2e-3@example.com', 'bif/anfangswert', '0'], ['e2e-3@example.com', 'bif/anfangswert', '2'],
  ['e2e-4@example.com', 'bif/punkt', '3'], ['e2e-4@example.com', 'bif/punkt', '1'],
  ['e2e-5@example.com', 'bif/punkt', '1'],
  ['e2e-6@example.com', 'bif/punkt', '1'], ['e2e-6@example.com', 'bif/anfangswert', '1'],
  ['e2e-auth-student-0@example.com', 'bif/punkt', '1'], ['e2e-auth-student-1@example.com', 'bif/punkt', '0'],
  ['e2e-auth-student-2@example.com', 'bif/punkt', '1'], ['e2e-auth-student-2@example.com', 'bif/anfangswert', '1'],
];

const PAGE = '<!DOCTYPE html>\n<html lang="de">\n<head><meta charset="utf-8"><title>Axolotl</title><link rel="stylesheet" href="styles.css"></head>\n<body>\n<h1>Axolotl</h1>\n<p>Axolotl leben ihr ganzes Leben im Wasser.</p>\n<ul>\n<li>Sie behalten ihre Kiemen.</li>\n<li>Sie können Gliedmaßen nachwachsen lassen.</li>\n<li>Sie leben nur bei Mexiko-Stadt.</li>\n</ul>\n<p id="gruss">Hallo!</p>\n<script src="script.js"></script>\n</body>\n</html>\n';
const STYLES = 'body { font-family: sans-serif; max-width: 40em; margin: 2em auto; background: #fff4f8; }\nh1 { color: #c2185b; }\n';
const SCRIPT = 'const stunde = new Date().getHours();\ndocument.getElementById("gruss").textContent = stunde < 12 ? "Guten Morgen!" : stunde < 18 ? "Guten Tag!" : "Guten Abend!";\n';

async function dress(page: Page, zoom = 1) {
  await page.evaluate(([names, z]) => {
    const fix = (t: string) => {
      for (const [a, b] of Object.entries(names as Record<string, string>)) t = t.split(a).join(b);
      return t.replace(/\bE2E\b/g, '9b');
    };
    const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
    const nodes: Text[] = [];
    while (walker.nextNode()) nodes.push(walker.currentNode as Text);
    nodes.forEach((n) => { const t = fix(n.data); if (t !== n.data) n.data = t; });
    document.documentElement.style.zoom = String(z);
  }, [NAMES, zoom] as const);
  await page.addStyleTag({ content: 'footer { display: none !important; } .class-board-intro { visibility: hidden; }' });
}

async function shoot(page: Page, name: string, anchor: string | null, out: string) {
  if (anchor) {
    await page.evaluate((sel) => {
      const e = document.querySelector(sel)!;
      window.scrollTo(0, e.getBoundingClientRect().top + window.scrollY - 24);
    }, anchor);
  }
  await page.waitForTimeout(300);
  const png = path.join(out, name + '.png');
  await page.screenshot({ path: png });
  execFileSync('convert', [png, '-quality', '85', '-define', 'webp:method=6', path.join(GUIDE, name + '.webp')]);
}

test('pictures for the teacher guide', async ({ freshWorkspace: workspace, browser }, testInfo) => {
  test.setTimeout(400_000);
  const out = testInfo.outputPath();
  const touched = new Set<string>();
  const pages = new Map<string, Page>();
  const as = async (email: string) => {
    if (!pages.has(email)) {
      const p = await (await browser.newContext()).newPage();
      await loginAsE2eUser(p, email, testInfo);
      pages.set(email, p);
    }
    return pages.get(email)!;
  };
  const container = new WorkspaceContainer('e2e-0@example.com');
  try {
    for (const [email, task, answer] of ANSWERS) {
      expect((await (await as(email)).request.post('/api/task/check', { data: { task, answer } })).ok()).toBe(true);
      touched.add(email + ' ' + task);
    }
    await container.waitUntilRunning();
    for (const [file, text] of [['index.html', PAGE], ['styles.css', STYLES], ['script.js', SCRIPT]]) {
      await container.exec(`mkdir -p /workspace/website-test && cat > /workspace/website-test/${file}`, { workdir: '/workspace', input: text });
    }
    const handIn = await (await workspace.request.post('/api/task/check', { data: { task: 'static-website/abgabe', answer: '' } })).json();
    expect(handIn.status, handIn.message).toBe('sent');

    const teacher = await (await browser.newContext({ viewport: { width: 1280, height: 720 }, deviceScaleFactor: 2 })).newPage();
    await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
    await teacher.goto('/klasse?group=E2E&activity=bif');
    await teacher.evaluate(() => localStorage.setItem('hackschule-class-welcome-hidden', '1'));
    await teacher.reload();
    await dress(teacher, 0.78);
    await shoot(teacher, 'klasse-fortschritt', '.class-board-select', out);

    // One Workspace at work, so its row shines.
    const load = container.exec('timeout 25 sh -c "while :; do :; done" || true', { workdir: '/workspace', timeoutMs: 45_000 });
    await teacher.goto('/klasse?group=E2E&tab=lernende');
    await expect(teacher.locator('.class-server-cards')).toBeVisible({ timeout: 20_000 });
    await expect(teacher.locator('.class-people-table tr.class-ws-active')).toHaveCount(1, { timeout: 15_000 });
    await teacher.waitForTimeout(1500);
    await dress(teacher, 0.78);
    await shoot(teacher, 'klasse-lernende', '.class-board-select', out);
    await load;

    await teacher.goto('/klasse?group=E2E&activity=static-website');
    await dress(teacher);
    const box = teacher.locator('details.class-answers', { hasText: 'Abgabe' });
    await box.locator('summary').click();
    await box.locator('a.cb-submission').first().click();
    await expect(teacher.frameLocator('.subview-frame').locator('#gruss')).toContainText('Guten', { timeout: 15_000 });
    await dress(teacher);
    await shoot(teacher, 'abgabe-ansehen', null, out);
    await teacher.keyboard.press('Escape');

    // No one waits for a code here; the projector view gets example rows.
    await teacher.goto('/klasse?group=E2E');
    await teacher.getByRole('button', { name: 'Anmelde-Codes zeigen' }).click();
    await teacher.evaluate(() => {
      const list = document.querySelector('.class-codes-list')!;
      list.innerHTML = '';
      const people = [['Aylin Demir', '483912'], ['Ben Hoffmann', '207655'], ['Clara Weber', '918340'], ['David Novak', '615208'],
        ['Elif Kaya', '552071'], ['Finn Schulz', '369824'], ['Greta Lange', '740193'], ['Ida Krüger', '084517'],
        ['Jonas Becker', '331960'], ['Kim Nguyen', '902446']];
      for (const [n, c] of people) {
        const row = document.createElement('div');
        row.className = 'class-codes-row';
        row.innerHTML = '<span class="class-codes-name"></span><span class="class-codes-code"></span>';
        row.children[0].textContent = n;
        row.children[1].textContent = c;
        list.appendChild(row);
      }
      document.querySelector('.class-codes-group')!.textContent = '9b';
      window.dispatchEvent(new Event('resize'));
    });
    await shoot(teacher, 'codes-beamer', null, out);
  } finally {
    for (const key of touched) {
      const [email, task] = key.split(' ');
      await (await as(email)).request.post('/api/task/delete', { data: { task } });
    }
    await workspace.request.post('/api/task/delete', { data: { task: 'static-website/abgabe' } });
    await container.exec('rm -rf /workspace/website-test', { workdir: '/workspace' }).catch(() => {});
  }
});
