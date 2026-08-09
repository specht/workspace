import { expect, test } from './fixtures';
import type { ContainerCommandResult } from './workspace-container';

const SVELTE_EXTENSION =
  'svelte.svelte-vscode';

const PROJECT_NAME = 'rubik-timer';

const PROJECT_DIR =
  `/workspace/.e2e/${PROJECT_NAME}`;

const FINAL_PAGE = [
  "<script>",
  "    import { onMount } from \"svelte\";",
  "    import Icon from \"@iconify/svelte\";",
  "",
  "    let state = $state(0);",
  "    // 0 : idle",
  "    // 1 : space pressed",
  "    // 2 : space pressed for 500 ms",
  "    // 3 : timer running",
  "    // 4 : timer stopped",
  "",
  "    let t0 = 0;",
  "    let timerString = $state(\"00:00<span class='small'>.00</span>\");",
  "    let timeoutId = null;",
  "",
  "    function updateTimer() {",
  "        if (state != 3) return;",
  "",
  "        let t1 = Date.now();",
  "        let duration = (t1 - t0) / 1000.0;",
  "",
  "        let minutes = `${Math.floor(duration / 60.0)}`;",
  "        if (minutes.length < 2) minutes = \"0\" + minutes;",
  "",
  "        let seconds = `${Math.floor(duration % 60)}`;",
  "        if (seconds.length < 2) seconds = \"0\" + seconds;",
  "",
  "        let centiseconds = `${Math.floor(duration * 100.0) % 100}`;",
  "        if (centiseconds.length < 2) centiseconds = \"0\" + centiseconds;",
  "",
  "        timerString = `${minutes}:${seconds}<span class='small'>.${centiseconds}</span>`;",
  "",
  "        requestAnimationFrame(updateTimer);",
  "    }",
  "",
  "    function resetTimer() {",
  "        if (state > 2) {",
  "            state = 0;",
  "            t0 = 0;",
  "            timerString = \"00:00<span class='small'>.00</span>\";",
  "            document.querySelector(\".timer\")?.classList.remove(\"ready\");",
  "        }",
  "    }",
  "",
  "    function handleKeyDown() {",
  "        if (state === 0) {",
  "            state = 1;",
  "            timeoutId = setTimeout(() => {",
  "                if (state === 1) {",
  "                    state = 2;",
  "                    document.querySelector(\".timer\")?.classList.add(\"ready\");",
  "                }",
  "            }, 500);",
  "        } else if (state === 3) {",
  "            state = 4;",
  "        }",
  "    }",
  "",
  "    function handleKeyUp() {",
  "        if (state === 1) {",
  "            state = 0;",
  "            clearTimeout(timeoutId);",
  "        } else if (state === 2) {",
  "            state = 3;",
  "            t0 = Date.now();",
  "            requestAnimationFrame(updateTimer);",
  "        }",
  "    }",
  "",
  "    onMount(() => {",
  "        document.addEventListener(\"keydown\", (e) => {",
  "            if (e.code === \"Space\") {",
  "                handleKeyDown();",
  "            }",
  "        });",
  "",
  "        document.addEventListener(\"keyup\", (e) => {",
  "            if (e.code === \"Space\") {",
  "                handleKeyUp();",
  "            }",
  "        });",
  "",
  "        document.addEventListener(\"touchstart\", () => handleKeyDown());",
  "        document.addEventListener(\"touchend\", () => handleKeyUp());",
  "    });",
  "</script>",
  "",
  "<div class=\"main\">",
  "    <h1>Rubik's Cube Timer</h1>",
  "",
  "    <p>State = {state}</p>",
  "",
  "    <p>",
  "        Halte die Leertaste gedr\u00fcckt, bis der Timer gr\u00fcn wird. Wenn du dann los",
  "        l\u00e4sst, beginnt die Zeit zu laufen.",
  "    </p>",
  "",
  "    <p class=\"timer\">{@html timerString}</p>",
  "",
  "    <button",
  "        id=\"bu_reset\"",
  "        class=\"btn btn-lg {state < 3 ? 'btn-outline-secondary' : 'btn-warning'}\"",
  "        disabled={state < 3 ? \"disabled\" : \"\"}",
  "        onclick={resetTimer}>",
  "        <Icon icon=\"material-symbols:device-reset-rounded\" class=\"icon\" />",
  "        Reset",
  "    </button>",
  "</div>",
  "",
  "<style>",
  "    .main {",
  "        display: flex;",
  "        flex-direction: column;",
  "        align-items: center;",
  "        justify-content: center;",
  "        min-height: 100vh;",
  "        margin: 0 1em;",
  "        user-select: none;",
  "        font-family: Quicksand;",
  "    }",
  "",
  "    .timer {",
  "        font-size: 300%;",
  "        font-weight: bold;",
  "        padding: 0.25em 0.5em;",
  "        border-radius: 0.2em;",
  "        background-color: #eeeeec;",
  "        font-family: \"IBM Plex Mono\";",
  "",
  "        :global(.small) {",
  "            font-size: 75%;",
  "        }",
  "",
  "        :global(&.ready) {",
  "            transition: background-color 0.3s ease-in;",
  "            background-color: #73a946;",
  "        }",
  "    }",
  "",
  "    :global(.btn .icon) {",
  "        margin-right: 0.25em;",
  "        padding-bottom: 0.1em;",
  "        transform: scale(1.3);",
  "    }",
  "</style>",
].join('\n') + '\n';

const HEAD_EXTRAS = [
  "<link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">",
  "<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>",
  "<link href=\"https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:ital,wght@0,400;0,700;1,400;1,700&family=Quicksand:wght@300..700&display=swap\" rel=\"stylesheet\">",
  "<link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.3.6/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-4Q6Gf2aSP4eDXB8Miphtr37CMZZQ5oXLH2yaXMJ2w8e2ZtHTl7GptT4jmndRuHDT\" crossorigin=\"anonymous\">",
].join('\n') + '\n';

const BODY_STYLE = [
  "<style>",
  "    html,",
  "    body {",
  "        margin: 0;",
  "    }",
  "</style>",
].join('\n') + '\n';

function combinedOutput(
  result: ContainerCommandResult,
): string {
  return [
    result.stdout,
    result.stderr,
  ]
    .filter(Boolean)
    .join('\n');
}

function expectSuccess(
  result: ContainerCommandResult,
  description: string,
) {
  expect(
    result.exitCode,
    `${description} failed\n` +
    `stdout:\n${result.stdout}\n` +
    `stderr:\n${result.stderr}`,
  ).toBe(0);
}

function formatTranscript(
  command: string,
  result: ContainerCommandResult,
): string {
  return [
    `$ ${command}`,
    result.stdout.trimEnd(),
    result.stderr.trimEnd()
      ? `[stderr]\n${result.stderr.trimEnd()}`
      : '',
    `[exit ${result.exitCode}]`,
    '',
  ]
    .filter(line => line !== '')
    .join('\n');
}

function replaceOnce(
  text: string,
  needle: string,
  replacement: string,
  description: string,
): string {
  const first = text.indexOf(needle);

  expect(
    first,
    `${description}: expected marker ${JSON.stringify(needle)}`,
  ).toBeGreaterThanOrEqual(0);

  expect(
    text.indexOf(
      needle,
      first + needle.length,
    ),
    `${description}: marker should occur exactly once`,
  ).toBe(-1);

  return (
    text.slice(0, first) +
    replacement +
    text.slice(first + needle.length)
  );
}

test(
  'Svelte tutorial creates, runs, and exports the Rubik timer app',
  async ({
    workspaceContainer: container,
  }, testInfo) => {
    test.setTimeout(480_000);

    const transcript: string[] = [];

    const run = async (
      command: string,
      options: {
        input?: string;
        timeoutMs?: number;
        workdir?: string;
      } = {},
    ) => {
      const result = await container.exec(
        command,
        {
          input: options.input,
          timeoutMs:
            options.timeoutMs ??
            60_000,
          workdir:
            options.workdir ??
            '/workspace/.e2e',
        },
      );

      transcript.push(
        formatTranscript(
          command,
          result,
        ),
      );

      return result;
    };

    /*
     * Start the tutorial's development server, wait until Vite accepts HTTP
     * requests, return the rendered page, and then stop the entire process
     * group so no Vite process leaks into later tests.
     */
    const fetchDevPage = async () => {
      const command = [
        'rm -f .e2e-dev-server.log',
        'setsid npm run dev > .e2e-dev-server.log 2>&1 &',
        'server_pid=$!',
        [
          'cleanup() {',
          'kill -TERM -- -"$server_pid" 2>/dev/null || true;',
          'wait "$server_pid" 2>/dev/null || true;',
          '}',
        ].join(' '),
        'trap cleanup EXIT INT TERM',
        [
          'for i in $(seq 1 60); do',
          'if curl -fs http://127.0.0.1:5173/ 2>/dev/null; then',
          'exit 0;',
          'fi;',
          'if ! kill -0 "$server_pid" 2>/dev/null; then',
          'cat .e2e-dev-server.log >&2;',
          'exit 1;',
          'fi;',
          'sleep 0.5;',
          'done',
        ].join(' '),
        'cat .e2e-dev-server.log >&2',
        'exit 1',
      ].join('\n');

      const result = await run(
        command,
        {
          workdir: PROJECT_DIR,
          timeoutMs: 45_000,
        },
      );

      expectSuccess(
        result,
        'Svelte development server',
      );

      return result.stdout;
    };

    try {
      await test.step(
        'Workspace has Node.js, npm, and npx',
        async () => {
          for (const command of [
            'node --version',
            'npm --version',
            'npx --version',
          ]) {
            const result =
              await run(command);

            expectSuccess(
              result,
              command,
            );

            expect(
              result.stdout.trim(),
            ).not.toBe('');
          }
        },
      );

      await test.step(
        'Svelte for VS Code can still be installed by code-server',
        async () => {
          const command = [
            'rm -rf svelte-extensions svelte-user-data',
            'mkdir -p svelte-extensions svelte-user-data',
            [
              'timeout 90s /app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/svelte-user-data',
              '--extensions-dir /workspace/.e2e/svelte-extensions',
              `--install-extension ${SVELTE_EXTENSION}`,
              '--force',
            ].join(' '),
            [
              '/app/code-server/bin/code-server',
              '--user-data-dir /workspace/.e2e/svelte-user-data',
              '--extensions-dir /workspace/.e2e/svelte-extensions',
              '--list-extensions',
            ].join(' '),
          ].join(' && ');

          const result = await run(
            command,
            {
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            result,
            'Svelte extension installation',
          );

          expect(
            combinedOutput(result)
              .toLowerCase(),
          ).toContain(
            SVELTE_EXTENSION.toLowerCase(),
          );
        },
      );

      await test.step(
        'npx can obtain and run the Svelte CLI',
        async () => {
          /*
           * --yes is the automated equivalent of accepting npx's
           * "Need to install the following package: sv" prompt.
           */
          const result = await run(
            'npx --yes sv create --help',
            {
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            result,
            'npx sv create help check',
          );

          expect(
            combinedOutput(result),
          ).toContain('--template');
        },
      );

      await test.step(
        'Create the tutorial SvelteKit project with npm',
        async () => {
          /*
           * These flags correspond to the choices in the tutorial:
           *
           *   SvelteKit minimal
           *   no type checking
           *   no add-ons
           *   npm
           */
          const command = [
            'rm -rf rubik-timer',
            [
              'npx --yes sv create',
              '--template minimal',
              '--no-types',
              '--no-add-ons',
              '--install npm',
              PROJECT_NAME,
            ].join(' '),
          ].join(' && ');

          const result = await run(
            command,
            {
              timeoutMs: 180_000,
            },
          );

          expectSuccess(
            result,
            'Svelte project creation',
          );

          const scaffold =
            await run(
              [
                'failed=0',
                [
                  'for path in',
                  'package.json',
                  'vite.config.js',
                  'src/app.html',
                  'src/routes/+page.svelte',
                  '; do',
                  'if [ -e "$path" ]; then',
                  'printf "OK      %s\\n" "$path";',
                  'else',
                  'printf "MISSING %s\\n" "$path";',
                  'failed=1;',
                  'fi;',
                  'done',
                ].join(' '),
                'exit "$failed"',
              ].join('; '),
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            scaffold,
            'Svelte project scaffold check',
          );

          const dependencies =
            await run(
              'npm ls svelte @sveltejs/kit --depth=0',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            dependencies,
            'Svelte dependencies check',
          );
        },
      );

      await test.step(
        'Apply the Workspace development-server settings from the tutorial',
        async () => {
          const packageResult =
            await run(
              'cat package.json',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            packageResult,
            'reading package.json',
          );

          const packageJson =
            JSON.parse(
              packageResult.stdout,
            );

          expect(
            packageJson.scripts?.dev,
            'the fresh Svelte template changed its dev script; review the tutorial',
          ).toBe('vite dev');

          packageJson.scripts.dev =
            'vite dev --host --open';

          await container.writeFile(
            `${PROJECT_NAME}/package.json`,
            JSON.stringify(
              packageJson,
              null,
              '\t',
            ) + '\n',
          );

          const viteResult =
            await run(
              'cat vite.config.js',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            viteResult,
            'reading vite.config.js',
          );

          /*
           * Preserve the config generated by the current Svelte CLI.  In
           * particular, it now contains adapter-auto and compilerOptions
           * inside sveltekit(...).  The tutorial only adds Vite's server
           * setting alongside the existing plugins entry.
           */
          const viteConfig =
            replaceOnce(
              viteResult.stdout,
              'export default defineConfig({',
              [
                'export default defineConfig({',
                '    server: {',
                '        allowedHosts: true',
                '    },',
              ].join('\n'),
              'adding Workspace allowedHosts to vite.config.js',
            );

          await container.writeFile(
            `${PROJECT_NAME}/vite.config.js`,
            viteConfig,
          );

          const verify =
            await run(
              [
                'node -e',
                JSON.stringify(
                  "const p=require('./package.json');" +
                  "if(p.scripts.dev!=='vite dev --host --open')process.exit(1)",
                ),
                '&&',
                [
                  'grep -Fq',
                  JSON.stringify(
                    'allowedHosts: true',
                  ),
                  'vite.config.js',
                ].join(' '),
                '&&',
                [
                  'grep -Fq',
                  JSON.stringify(
                    '@sveltejs/adapter-auto',
                  ),
                  'vite.config.js',
                ].join(' '),
              ].join(' '),
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            verify,
            'Workspace Vite configuration check',
          );
        },
      );

      await test.step(
        'The tutorial development server serves the new app',
        async () => {
          const html =
            await fetchDevPage();

          expect(html).toMatch(
            /<!doctype html|<html/i,
          );
        },
      );

      await test.step(
        'Implement the Rubik timer from the tutorial',
        async () => {
          await container.writeFile(
            `${PROJECT_NAME}/src/routes/+page.svelte`,
            FINAL_PAGE,
          );

          const appHtmlResult =
            await run(
              'cat src/app.html',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            appHtmlResult,
            'reading src/app.html',
          );

          let appHtml =
            appHtmlResult.stdout;

          appHtml = replaceOnce(
            appHtml,
            '%sveltekit.head%',
            `${HEAD_EXTRAS}\n\t\t%sveltekit.head%`,
            'adding tutorial font and Bootstrap links',
          );

          appHtml = replaceOnce(
            appHtml,
            '</body>',
            `</body>\n\n${BODY_STYLE}`,
            'adding the tutorial body reset CSS',
          );

          await container.writeFile(
            `${PROJECT_NAME}/src/app.html`,
            appHtml,
          );
        },
      );

      await test.step(
        'Install Iconify as shown in the tutorial',
        async () => {
          const result = await run(
            'npm install @iconify/svelte',
            {
              workdir: PROJECT_DIR,
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            result,
            'Iconify installation',
          );

          const verify =
            await run(
              'npm ls @iconify/svelte --depth=0',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            verify,
            'Iconify dependency check',
          );
        },
      );

      await test.step(
        'The completed Rubik timer runs in the development server',
        async () => {
          const html =
            await fetchDevPage();

          expect(html).toContain(
            "Rubik's Cube Timer",
          );

          expect(html).toContain(
            'Reset',
          );
        },
      );

      await test.step(
        'Install and enable the static SvelteKit adapter',
        async () => {
          const install = await run(
            'npm install -D @sveltejs/adapter-static',
            {
              workdir: PROJECT_DIR,
              timeoutMs: 120_000,
            },
          );

          expectSuccess(
            install,
            'adapter-static installation',
          );

          const configResult =
            await run(
              'cat vite.config.js',
              {
                workdir: PROJECT_DIR,
              },
            );

          expectSuccess(
            configResult,
            'reading vite.config.js',
          );

          expect(
            configResult.stdout,
            'the generated Vite config no longer uses adapter-auto; review the tutorial',
          ).toContain(
            '@sveltejs/adapter-auto',
          );

          expect(
            configResult.stdout,
            'the generated Vite config should contain adapter: adapter()',
          ).toContain(
            'adapter: adapter()',
          );

          const staticConfig =
            replaceOnce(
              configResult.stdout,
              '@sveltejs/adapter-auto',
              '@sveltejs/adapter-static',
              'switching the SvelteKit adapter to adapter-static',
            );

          await container.writeFile(
            `${PROJECT_NAME}/vite.config.js`,
            staticConfig,
          );

          await container.writeFile(
            `${PROJECT_NAME}/src/routes/+layout.js`,
            'export const prerender = true;\n',
          );
        },
      );

      await test.step(
        'The tutorial exports a static website',
        async () => {
          const result = await run(
            'npm run build',
            {
              workdir: PROJECT_DIR,
              timeoutMs: 180_000,
            },
          );

          expectSuccess(
            result,
            'Svelte static build',
          );

          const output = await run(
            [
              'test -s build/index.html',
              'test -d build/_app',
              'find build -type f -name "*.js" -print -quit | grep -q .',
              [
                'grep -Fq',
                JSON.stringify(
                  "Rubik's Cube Timer",
                ),
                'build/index.html',
              ].join(' '),
              [
                'grep -Fq',
                JSON.stringify('Reset'),
                'build/index.html',
              ].join(' '),
              'find build -maxdepth 5 -type f | sort',
            ].join(' && '),
            {
              workdir: PROJECT_DIR,
            },
          );

          expectSuccess(
            output,
            'static build output check',
          );
        },
      );
    }
    finally {
      await testInfo.attach(
        'svelte-container-transcript',
        {
          body: Buffer.from(
            transcript.join('\n'),
            'utf8',
          ),
          contentType: 'text/plain',
        },
      );
    }
  },
);