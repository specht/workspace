// Every hand-in task of the site (`type: submit`) once for real: files as a
// student has them at the end of the activity go into the Workspace, they
// are handed in, and the teacher opens the hand-in on the class page. The
// viewer must show it: the page running, the source as text, the picture,
// the PDF. Templates are the real ones at the commits of the walkthroughs;
// raylib is built with make, the SvelteKit page with npm run build.
import fs from 'node:fs';
import path from 'node:path';
import type { Frame, Page } from '@playwright/test';
import { expect, test } from './fixtures';
import { loginAsE2eUser } from './workspace';
import { WorkspaceContainer } from './workspace-container';

const CONTENT = path.resolve(__dirname, '../../src/content');
const read = (file: string) => fs.readFileSync(path.join(CONTENT, file), 'utf8');
const base64 = (file: string) => fs.readFileSync(path.join(CONTENT, file)).toString('base64');

type Shown =
  | { run: string } // the first file runs as a page and shows this text
  | { code: string } // the first file shows as source with this text
  | { pdf: true }; // the first file is a PDF in a frame

interface HandIn {
  task: string;
  setup: (c: WorkspaceContainer) => Promise<void>;
  answer?: string; // the file picked, for `pick`
  shown: Shown;
  folders: string[]; // removed again afterwards
  timeoutMs?: number;
}

// A command in /workspace that must succeed; its output tells why not
async function run(c: WorkspaceContainer, command: string, options: { input?: string; timeoutMs?: number } = {}) {
  const result = await c.exec(command, { workdir: '/workspace', ...options });
  expect(result.exitCode, `${command}\n${result.stdout}\n${result.stderr}`).toBe(0);
}
const write = (c: WorkspaceContainer, file: string, text: string) =>
  run(c, `mkdir -p "$(dirname '${file}')" && cat > '${file}'`, { input: text });
const writeBinary = (c: WorkspaceContainer, file: string, data: string) =>
  run(c, `mkdir -p "$(dirname '${file}')" && base64 -d > '${file}'`, { input: data });
const clone = (c: WorkspaceContainer, url: string, folder: string, commit?: string) =>
  run(c, `git clone -q ${url} ${folder}` + (commit ? ` && git -C ${folder} checkout -q ${commit}` : ''), { timeoutMs: 180_000 });

const HAND_INS: HandIn[] = [
  {
    task: 'anaglyph/galerie',
    // the scene needs the viewer of the template (requires: scene_loads)
    setup: async (c) => {
      await clone(c, 'https://github.com/specht/anaglyph.git', 'anaglyph', 'c2ac11e67cd0f53cde3447a28b830f08f2dc086a');
      await write(c, 'anaglyph/ampel.ini', read('anaglyph/ampel.ini'));
    },
    answer: 'anaglyph/ampel.ini',
    shown: { code: read('anaglyph/ampel.ini').split('\n').find((line) => line.trim())!.trim() },
    folders: ['anaglyph'],
  },
  {
    task: 'bif/abgabe',
    setup: (c) => clone(c, 'https://github.com/specht/bif.git', 'bif', '9b8abe88b071b5f7acdc86252519a9e6eb92a3b6'),
    shown: { run: 'Nach Schulschluss' },
    folders: ['bif'],
  },
  {
    task: 'drawing/eigenes-bild',
    setup: async (c) => {
      await write(c, 'bild.rb', read('drawing/rainbow.rb'));
      await writeBinary(c, 'bild.png', base64('anaglyph/image.png'));
    },
    shown: { code: read('drawing/rainbow.rb').split('\n').find((line) => line.trim())!.trim() },
    folders: ['bild.rb', 'bild.png'],
  },
  {
    task: 'hugo/abgabe',
    // the source of the site, without a built public/: its HTML files are
    // templates, so the viewer starts with hugo.yaml
    setup: async (c) => {
      const root = path.join(CONTENT, 'hugo/stufen/solar-system');
      for (const file of fs.readdirSync(root, { recursive: true }) as string[]) {
        if (fs.statSync(path.join(root, file)).isFile()) await write(c, `solar-system/${file}`, fs.readFileSync(path.join(root, file), 'utf8'));
      }
    },
    shown: { code: 'Unser Sonnensystem' },
    folders: ['solar-system'],
  },
  {
    task: 'latex/abgabe',
    setup: async (c) => {
      await write(c, 'facharbeit/facharbeit.tex', '\\documentclass{article}\n\\begin{document}\nEine Facharbeit.\n\\end{document}\n');
      await run(c, 'cd facharbeit && lualatex -interaction=nonstopmode facharbeit.tex > /dev/null', { timeoutMs: 120_000 });
    },
    answer: 'facharbeit/facharbeit.pdf',
    shown: { pdf: true },
    folders: ['facharbeit'],
  },
  {
    task: 'pixelwand/abgabe',
    setup: async (c) => {
      await write(c, 'pixelwand/server.rb', read('pixelwand/stufen/server5.rb'));
      await write(c, 'pixelwand/index.html', read('pixelwand/stufen/index3.html'));
    },
    shown: { run: 'Pixelwand' },
    folders: ['pixelwand'],
  },
  {
    task: 'raylib/abgabe',
    setup: async (c) => {
      await clone(c, 'https://github.com/specht/raylib-starter.git', 'raylib-starter');
      await run(c, 'cd raylib-starter && make > /dev/null', { timeoutMs: 300_000 });
    },
    shown: { run: '' },
    folders: ['raylib-starter'],
    timeoutMs: 400_000,
  },
  {
    task: 'showerjs/abgabe',
    setup: (c) => clone(c, 'https://git.nhcham.org/specht/shower.js.git', 'shower.js', '50a071df026545793cfa29853e5e4ed6b1ece36e'),
    shown: { run: 'Titel deiner Präsentation' },
    folders: ['shower.js'],
  },
  {
    task: 'static-website/abgabe',
    setup: (c) => write(c, 'website-test/index.html',
      '<!DOCTYPE html>\n<html lang="de">\n<head><meta charset="utf-8"><title>Axolotl</title></head>\n<body>\n<h1>Axolotl</h1>\n</body>\n</html>\n'),
    shown: { run: 'Axolotl' },
    folders: ['website-test'],
  },
  {
    task: 'steganography/abgabe',
    setup: async (c) => {
      await write(c, 'stego/stego.rb', read('steganography/loesung/stego.rb'));
      await writeBinary(c, 'stego/bild.png', base64('anaglyph/image.png'));
    },
    shown: { code: read('steganography/loesung/stego.rb').split('\n').find((line) => line.trim())!.trim() },
    folders: ['stego'],
  },
  {
    task: 'svelte/abgabe',
    // a real SvelteKit page, built as the activity does it
    setup: (c) => run(c, [
      // pinned, so that its options stay as they are
      'npx -y sv@1.1.1 create --template minimal --no-types --no-add-ons --install npm rubik-timer',
      'cd rubik-timer',
      'npm install -D @sveltejs/adapter-static',
      "sed -i 's#@sveltejs/adapter-auto#@sveltejs/adapter-static#' $(ls svelte.config.js vite.config.js 2>/dev/null)",
      "echo 'export const prerender = true;' > src/routes/+layout.js",
      'npm run build',
    ].join(' && '), { timeoutMs: 400_000 }),
    shown: { run: 'Welcome to SvelteKit' },
    folders: ['rubik-timer'],
    timeoutMs: 500_000,
  },
  {
    task: 'tic80/abgabe',
    setup: (c) => write(c, 'TIC-80/stars.lua', read('tic80/stars.lua')),
    answer: 'TIC-80/stars.lua',
    shown: { code: 'function TIC()' },
    folders: ['TIC-80'],
  },
  {
    task: 'webserver/abgabe',
    setup: async (c) => {
      await write(c, 'webserver/server.rb', read('webserver/stufen/6.rb'));
      for (const file of ['index.html', 'poll.html', 'style.css']) await write(c, `webserver/public/${file}`, read(`webserver/stufen/public/${file}`));
    },
    shown: { run: 'Mein eigener Webserver' },
    folders: ['webserver'],
  },
  {
    task: 'wetter/abgabe',
    setup: async (c) => {
      await write(c, 'wetter/wetter.rb', read('wetter/stufen/wetter3.rb'));
      await write(c, 'wetter/index.html', read('wetter/stufen/index.html'));
    },
    shown: { run: 'Wetter' },
    folders: ['wetter'],
  },
];

// The frame of the running hand-in, on the viewer's host
async function handInFrame(page: Page): Promise<Frame> {
  await expect(page.locator('.subview-frame')).toBeVisible();
  let frame: Frame | undefined;
  await expect.poll(() => {
    frame = page.frames().find((f) => f.url().includes('/v/'));
    return !!frame;
  }, { timeout: 15_000 }).toBe(true);
  return frame!;
}

// One Workspace for all of them: one after the other (npx playwright test
// --workers=1, as the project runs anyway)
for (const handIn of HAND_INS) {
  test(`hand-in ${handIn.task} shows in the viewer`, async ({ freshWorkspace: workspace, e2eEmail, browser }, testInfo) => {
    test.setTimeout(handIn.timeoutMs ?? 240_000);
    const container = new WorkspaceContainer(e2eEmail);
    await container.waitUntilRunning();
    const [activity] = handIn.task.split('/');
    const teacherContext = await browser.newContext({ viewport: { width: 1300, height: 850 } });
    try {
      await handIn.setup(container);
      const result = await (await workspace.request.post('/api/task/check', {
        data: { task: handIn.task, answer: handIn.answer ?? '' },
      })).json();
      expect(result.status, result.message).toBe('sent');

      const teacher = await teacherContext.newPage();
      const consoleLines: string[] = [];
      teacher.on('console', (m) => consoleLines.push(m.text()));
      await loginAsE2eUser(teacher, 'e2e-teacher@example.com', testInfo);
      await teacher.goto(`/klasse?group=E2E&activity=${activity}`);
      // the answers of the hand-in task, not the first task with answers
      // (other tests leave answers to other tasks of the same activity)
      const link = teacher.locator('a.cb-submission').first();
      if (!(await link.isVisible())) {
        await teacher.locator('details.class-answers', { has: teacher.locator('a.cb-submission') }).first().locator('summary').click();
      }
      await link.click();
      const dialog = teacher.getByRole('dialog');
      await expect(dialog).toBeVisible();
      await expect(dialog.locator('.subview-file.active')).toBeVisible();
      await expect(dialog.locator('.subview-facts')).toContainText('abgegeben am');

      const shown = handIn.shown;
      if ('run' in shown) {
        const frame = await handInFrame(teacher);
        if (shown.run) {
          await expect(frame.locator('body')).toContainText(shown.run, { timeout: 30_000 });
        } else {
          // raylib: the canvas, and raylib started in it (its own log line,
          // after the .wasm has loaded)
          await expect(frame.locator('canvas').first()).toBeVisible({ timeout: 30_000 });
          await expect.poll(() => consoleLines.some((line) => line.includes('PLATFORM: WEB: Initialized successfully')),
            { timeout: 30_000 }).toBe(true);
        }
      } else if ('code' in shown) {
        await expect(dialog.locator('.subview-code')).toContainText(shown.code, { timeout: 15_000 });
      } else {
        const src = await dialog.locator('.subview-frame').getAttribute('src');
        expect(src).toMatch(/\.pdf$/);
        const response = await teacher.request.get(src!);
        expect(response.status()).toBe(200);
        expect(response.headers()['content-type']).toBe('application/pdf');
        expect(response.headers()['content-security-policy']).not.toContain('sandbox');
      }
      // the close button: a running page (raylib's canvas) can have the
      // keys, and then Escape stays inside its frame
      await dialog.getByRole('button', { name: 'Schließen' }).click();
      await expect(dialog).toHaveCount(0);
    } finally {
      await workspace.request.post('/api/task/delete', { data: { task: handIn.task } }).catch(() => {});
      await container.exec(`rm -rf ${handIn.folders.join(' ')}`, { workdir: '/workspace' }).catch(() => {});
      await teacherContext.close();
    }
  });
}
