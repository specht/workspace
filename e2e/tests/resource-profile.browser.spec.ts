import { execFile } from 'node:child_process';
import fs from 'node:fs/promises';
import path from 'node:path';
import { promisify } from 'node:util';
import { expect, test } from './fixtures';
import { readTutorialFile } from './tutorial';
import {
  WorkspaceContainer,
} from './workspace-container';
import type {
  ContainerCommandResult,
} from './workspace-container';

const execFileAsync = promisify(execFile);

const PROFILE_ENABLED =
  process.env.E2E_RESOURCE_PROFILE === '1';
const SAMPLE_INTERVAL_MS = positiveIntegerEnv(
  'E2E_RESOURCE_SAMPLE_MS',
  10,
);
const RUNS_PER_WORKLOAD = positiveIntegerEnv(
  'E2E_RESOURCE_RUNS',
  1,
);

const PROFILE_ROOT =
  '/workspace/.e2e/resource-profile';

const PIXELRAM_REPOSITORY =
  'https://github.com/specht/pixelram-starter.git';
const LATEX_REPOSITORY =
  'https://github.com/specht/latex-tutorial.git';

const C_BUBBLESORT = readTutorialFile(
  'c',
  'bubblesort.c',
);
const CPP_BUBBLESORT = readTutorialFile(
  'cpp',
  'bubblesort.cpp',
);

const PYTHON_BUBBLESORT = readTutorialFile(
  'python',
  'bubblesort.py',
);
const RUBY_BUBBLESORT = readTutorialFile(
  'ruby',
  'bubblesort.rb',
);

type HostConfig = {
  Memory?: number;
  NanoCpus?: number;
  PidsLimit?: number | null;
};

type ResourceCounters = {
  memoryBytes: number;
  pids: number;
};

type ResourceProfileResult = {
  workload: string;
  description: string;
  run: number;
  command: string;
  durationMs: number;
  samples: number;
  baselineMemoryBytes: number;
  peakMemoryBytes: number;
  deltaMemoryBytes: number;
  baselinePids: number;
  peakPids: number;
  deltaPids: number;
};

type Workload = {
  id: string;
  description: string;
  command: string;
  workdir: string;
  timeoutMs: number;
  prepare?: () => Promise<void>;
  beforeRun?: () => Promise<void>;
};

function positiveIntegerEnv(
  name: string,
  fallback: number,
): number {
  const raw = process.env[name];
  if (raw === undefined || raw.trim() === '')
    return fallback;

  const value = Number.parseInt(raw, 10);
  if (!Number.isInteger(value) || value < 1)
    throw new Error(`${name} must be a positive integer`);

  return value;
}

function sleep(milliseconds: number) {
  return new Promise(resolve =>
    setTimeout(resolve, milliseconds),
  );
}

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'\\''`)}'`;
}

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

async function expectCommandSuccess(
  container: WorkspaceContainer,
  command: string,
  options: {
    timeoutMs?: number;
    workdir?: string;
  } = {},
): Promise<ContainerCommandResult> {
  const result = await container.exec(
    command,
    {
      timeoutMs: options.timeoutMs ?? 60_000,
      workdir: options.workdir ?? PROFILE_ROOT,
    },
  );

  expect(
    result.exitCode,
    `Command failed: ${command}\n` +
    `stdout:\n${result.stdout}\n` +
    `stderr:\n${result.stderr}`,
  ).toBe(0);

  return result;
}

async function hostCommand(
  command: string,
  args: string[],
  timeoutMs = 10_000,
): Promise<string> {
  const result = await execFileAsync(
    command,
    args,
    {
      timeout: timeoutMs,
      maxBuffer: 10 * 1024 * 1024,
      encoding: 'utf8',
    },
  );

  return result.stdout;
}

async function fileExists(file: string): Promise<boolean> {
  try {
    await fs.access(file);
    return true;
  }
  catch {
    return false;
  }
}

async function findContainerCgroupDir(
  containerName: string,
): Promise<{
  directory: string;
  source: string;
}> {
  const pidText = await hostCommand(
    'docker',
    [
      'inspect',
      '--format',
      '{{.State.Pid}}',
      containerName,
    ],
  );
  const pid = Number.parseInt(pidText.trim(), 10);

  if (!Number.isInteger(pid) || pid < 1) {
    throw new Error(
      `Could not determine host PID for ${containerName}`,
    );
  }

  /*
   * On current Docker/cgroup-v2 systems this path enters the container's
   * cgroup namespace without starting a process inside the container. That is
   * ideal for PID profiling because the sampler itself is not counted.
   */
  const namespaceView =
    `/proc/${pid}/root/sys/fs/cgroup`;
  if (
    await fileExists(
      path.join(namespaceView, 'memory.current'),
    ) &&
    await fileExists(
      path.join(namespaceView, 'pids.current'),
    )
  ) {
    return {
      directory: namespaceView,
      source: 'container cgroup namespace via /proc/<pid>/root',
    };
  }

  /*
   * Fall back to the host's unified cgroup path. This also deliberately
   * requires cgroup v2: memory.current and pids.current are the same counters
   * that the future Docker memory/PID limits will act on.
   */
  const cgroupText = await fs.readFile(
    `/proc/${pid}/cgroup`,
    'utf8',
  );
  const unifiedLine = cgroupText
    .split(/\r?\n/)
    .find(line => line.startsWith('0::'));

  if (!unifiedLine) {
    throw new Error(
      'Resource profiling requires a Linux cgroup-v2 host. ' +
      `No unified cgroup entry was found for ${containerName}.`,
    );
  }

  const relative = unifiedLine
    .slice(3)
    .replace(/^\/+/, '');
  const hostView = path.join(
    '/sys/fs/cgroup',
    relative,
  );

  if (
    !await fileExists(
      path.join(hostView, 'memory.current'),
    ) ||
    !await fileExists(
      path.join(hostView, 'pids.current'),
    )
  ) {
    throw new Error(
      'Could not find memory.current and pids.current for ' +
      `${containerName} at ${hostView}`,
    );
  }

  return {
    directory: hostView,
    source: 'host unified cgroup path',
  };
}

async function readCounters(
  cgroupDir: string,
): Promise<ResourceCounters> {
  const [memoryText, pidsText] = await Promise.all([
    fs.readFile(
      path.join(cgroupDir, 'memory.current'),
      'utf8',
    ),
    fs.readFile(
      path.join(cgroupDir, 'pids.current'),
      'utf8',
    ),
  ]);

  const memoryBytes = Number.parseInt(
    memoryText.trim(),
    10,
  );
  const pids = Number.parseInt(
    pidsText.trim(),
    10,
  );

  if (
    !Number.isFinite(memoryBytes) ||
    !Number.isFinite(pids)
  ) {
    throw new Error(
      `Invalid cgroup counters: memory=${memoryText.trim()} pids=${pidsText.trim()}`,
    );
  }

  return {
    memoryBytes,
    pids,
  };
}

async function readIdleBaseline(
  cgroupDir: string,
): Promise<ResourceCounters> {
  const durationMs = Math.max(
    500,
    SAMPLE_INTERVAL_MS * 20,
  );
  const deadline = Date.now() + durationMs;
  let peakMemoryBytes = 0;
  let peakPids = 0;

  do {
    const sample = await readCounters(cgroupDir);
    peakMemoryBytes = Math.max(
      peakMemoryBytes,
      sample.memoryBytes,
    );
    peakPids = Math.max(
      peakPids,
      sample.pids,
    );
    await sleep(SAMPLE_INTERVAL_MS);
  } while (Date.now() < deadline);

  return {
    memoryBytes: peakMemoryBytes,
    pids: peakPids,
  };
}

async function profileCommand(
  container: WorkspaceContainer,
  cgroupDir: string,
  workload: Workload,
  run: number,
): Promise<ResourceProfileResult> {
  const baseline = await readIdleBaseline(
    cgroupDir,
  );

  let finished = false;
  const startedAt = performance.now();
  const commandPromise = container.exec(
    workload.command,
    {
      timeoutMs: workload.timeoutMs,
      workdir: workload.workdir,
    },
  ).finally(() => {
    finished = true;
  });

  let peakMemoryBytes = baseline.memoryBytes;
  let peakPids = baseline.pids;
  let samples = 0;

  do {
    const sample = await readCounters(cgroupDir);
    peakMemoryBytes = Math.max(
      peakMemoryBytes,
      sample.memoryBytes,
    );
    peakPids = Math.max(
      peakPids,
      sample.pids,
    );
    samples += 1;

    if (!finished)
      await sleep(SAMPLE_INTERVAL_MS);
  } while (!finished);

  const result = await commandPromise;
  const finalSample = await readCounters(cgroupDir);
  peakMemoryBytes = Math.max(
    peakMemoryBytes,
    finalSample.memoryBytes,
  );
  peakPids = Math.max(
    peakPids,
    finalSample.pids,
  );
  samples += 1;

  expect(
    result.exitCode,
    `${workload.description} failed on run ${run}\n` +
    `command: ${workload.command}\n` +
    `stdout:\n${result.stdout}\n` +
    `stderr:\n${result.stderr}`,
  ).toBe(0);

  return {
    workload: workload.id,
    description: workload.description,
    run,
    command: workload.command,
    durationMs: Math.round(
      performance.now() - startedAt,
    ),
    samples,
    baselineMemoryBytes:
      baseline.memoryBytes,
    peakMemoryBytes,
    deltaMemoryBytes: Math.max(
      0,
      peakMemoryBytes - baseline.memoryBytes,
    ),
    baselinePids: baseline.pids,
    peakPids,
    deltaPids: Math.max(
      0,
      peakPids - baseline.pids,
    ),
  };
}

function bytesToMiB(bytes: number): number {
  return Math.round(
    bytes / 1024 / 1024 * 10,
  ) / 10;
}

function csvCell(value: string | number): string {
  const text = String(value);
  if (!/[",\n]/.test(text))
    return text;
  return `"${text.replace(/"/g, '""')}"`;
}

function resultsToCsv(
  results: ResourceProfileResult[],
): string {
  const rows = [
    [
      'workload',
      'description',
      'run',
      'duration_ms',
      'samples',
      'baseline_memory_mib',
      'peak_memory_mib',
      'delta_memory_mib',
      'baseline_pids',
      'peak_pids',
      'delta_pids',
    ],
    ...results.map(result => [
      result.workload,
      result.description,
      result.run,
      result.durationMs,
      result.samples,
      bytesToMiB(
        result.baselineMemoryBytes,
      ),
      bytesToMiB(
        result.peakMemoryBytes,
      ),
      bytesToMiB(
        result.deltaMemoryBytes,
      ),
      result.baselinePids,
      result.peakPids,
      result.deltaPids,
    ]),
  ];

  return rows
    .map(row => row.map(csvCell).join(','))
    .join('\n') + '\n';
}

async function collectVersions(
  container: WorkspaceContainer,
): Promise<Record<string, string>> {
  const commands: Record<string, string> = {
    gcc: 'gcc --version | head -n 1',
    gpp: 'g++ --version | head -n 1',
    emscripten: 'emcc --version | head -n 1',
    flutter: 'flutter --version | head -n 2',
    node: 'node --version',
    npm: 'npm --version',
    latexmk: 'latexmk -v | head -n 2',
    python: 'python3 --version',
    ruby: 'ruby --version',
  };
  const versions: Record<string, string> = {};

  for (const [name, command] of Object.entries(commands)) {
    const result = await container.exec(
      command,
      {
        timeoutMs: 30_000,
        workdir: '/',
      },
    );

    expect(
      result.exitCode,
      `Could not read ${name} version\n${combinedOutput(result)}`,
    ).toBe(0);

    versions[name] = combinedOutput(result).trim();
  }

  return versions;
}

if (PROFILE_ENABLED) {
  test(
    'measure representative student workspace resource peaks',
    async ({
      freshWorkspace,
      e2eEmail,
    }, testInfo) => {
      test.setTimeout(
        Math.max(
          20 * 60_000,
          RUNS_PER_WORKLOAD * 10 * 60_000,
        ),
      );

      /*
       * Keep the browser connected throughout the measurements. The workload
       * commands themselves still run through docker exec, but this gives us a
       * more realistic code-server/extension-host baseline than profiling a
       * detached or ad-hoc container.
       */
      await expect(
        freshWorkspace.locator(
          '.monaco-workbench',
        ),
      ).toBeVisible();

      const container = new WorkspaceContainer(
        e2eEmail,
      );
      await container.waitUntilRunning();
      await container.resetSandbox();

      const hostConfigText = await hostCommand(
        'docker',
        [
          'inspect',
          '--format',
          '{{json .HostConfig}}',
          container.name,
        ],
      );
      const hostConfig = JSON.parse(
        hostConfigText,
      ) as HostConfig;

      const memoryLimitBytes =
        hostConfig.Memory ?? 0;
      const pidsLimit =
        hostConfig.PidsLimit ?? 0;

      expect(
        memoryLimitBytes,
        'Resource profiling must run before a student-container memory limit is enabled',
      ).toBe(0);
      expect(
        pidsLimit <= 0,
        'Resource profiling must run before a student-container PID limit is enabled',
      ).toBe(true);

      const cgroup = await findContainerCgroupDir(
        container.name,
      );
      const versions = await collectVersions(
        container,
      );

      await expectCommandSuccess(
        container,
        `mkdir -p ${shellQuote(PROFILE_ROOT)}`,
        {
          workdir: '/workspace/.e2e',
        },
      );

      await container.writeFile(
        'resource-profile/native/bubblesort.c',
        C_BUBBLESORT,
      );
      await container.writeFile(
        'resource-profile/native/bubblesort.cpp',
        CPP_BUBBLESORT,
      );
      await container.writeFile(
        'resource-profile/interpreted/bubblesort.py',
        PYTHON_BUBBLESORT,
      );
      await container.writeFile(
        'resource-profile/interpreted/bubblesort.rb',
        RUBY_BUBBLESORT,
      );

      const nativeDir =
        `${PROFILE_ROOT}/native`;
      const interpretedDir =
        `${PROFILE_ROOT}/interpreted`;
      const pixelramDir =
        `${PROFILE_ROOT}/pixelram-starter`;
      const flutterDir =
        `${PROFILE_ROOT}/flutter-profile`;
      const svelteDir =
        `${PROFILE_ROOT}/svelte-profile`;
      const latexDir =
        `${PROFILE_ROOT}/latex-tutorial`;

      const workloads: Workload[] = [
        {
          id: 'c-gcc',
          description:
            'C tutorial compilation (bubblesort.c)',
          command:
            'rm -f bubblesort-c && gcc -O2 bubblesort.c -o bubblesort-c',
          workdir: nativeDir,
          timeoutMs: 60_000,
        },
        {
          id: 'cpp-gpp',
          description:
            'C++ tutorial compilation (bubblesort.cpp)',
          command:
            'rm -f bubblesort-cpp && g++ -O2 bubblesort.cpp -o bubblesort-cpp',
          workdir: nativeDir,
          timeoutMs: 60_000,
        },
        {
          id: 'pixelram-emscripten',
          description:
            'PixelRAM starter make / Emscripten build',
          command: 'make',
          workdir: pixelramDir,
          timeoutMs: 240_000,
          prepare: async () => {
            await expectCommandSuccess(
              container,
              [
                'git clone --depth 1',
                shellQuote(PIXELRAM_REPOSITORY),
                'pixelram-starter',
              ].join(' '),
              {
                workdir: PROFILE_ROOT,
                timeoutMs: 90_000,
              },
            );
          },
          beforeRun: async () => {
            await expectCommandSuccess(
              container,
              'make clean',
              {
                workdir: pixelramDir,
              },
            );
          },
        },
        {
          id: 'flutter-web-debug',
          description:
            'Flutter Web debug build of a fresh app',
          command:
            'flutter build web --debug',
          workdir: flutterDir,
          timeoutMs: 360_000,
          prepare: async () => {
            await expectCommandSuccess(
              container,
              [
                'flutter create',
                '--platforms=web',
                '--project-name=resource_profile_flutter',
                'flutter-profile',
              ].join(' '),
              {
                workdir: PROFILE_ROOT,
                timeoutMs: 180_000,
              },
            );
          },
          beforeRun: async () => {
            await expectCommandSuccess(
              container,
              'rm -rf build .dart_tool/flutter_build',
              {
                workdir: flutterDir,
              },
            );
          },
        },
        {
          id: 'flutter-web-release',
          description:
            'Flutter Web release build of the same fresh app',
          command:
            'flutter build web --release',
          workdir: flutterDir,
          timeoutMs: 360_000,
          beforeRun: async () => {
            await expectCommandSuccess(
              container,
              'rm -rf build .dart_tool/flutter_build',
              {
                workdir: flutterDir,
              },
            );
          },
        },
        {
          id: 'svelte-npm',
          description:
            'SvelteKit / Vite production build from the tutorial toolchain',
          command: 'npm run build',
          workdir: svelteDir,
          timeoutMs: 240_000,
          prepare: async () => {
            await expectCommandSuccess(
              container,
              [
                'npx --yes sv create',
                '--template minimal',
                '--no-types',
                '--no-add-ons',
                '--install npm',
                'svelte-profile',
              ].join(' '),
              {
                workdir: PROFILE_ROOT,
                timeoutMs: 240_000,
              },
            );
          },
          beforeRun: async () => {
            await expectCommandSuccess(
              container,
              'rm -rf .svelte-kit build',
              {
                workdir: svelteDir,
              },
            );
          },
        },
        {
          id: 'latex-lualatex',
          description:
            'LaTeX tutorial book build (wpgtr.tex via latexmk/LuaLaTeX)',
          command: [
            'latexmk',
            '-lualatex',
            '-interaction=nonstopmode',
            '-halt-on-error',
            '-file-line-error',
            'wpgtr.tex',
          ].join(' '),
          workdir: latexDir,
          timeoutMs: 240_000,
          prepare: async () => {
            await expectCommandSuccess(
              container,
              [
                'git clone --depth 1',
                shellQuote(LATEX_REPOSITORY),
                'latex-tutorial',
              ].join(' '),
              {
                workdir: PROFILE_ROOT,
                timeoutMs: 90_000,
              },
            );
          },
          beforeRun: async () => {
            const clean = await container.exec(
              'latexmk -C wpgtr.tex',
              {
                workdir: latexDir,
                timeoutMs: 60_000,
              },
            );

            /* latexmk -C may report that there was nothing to clean. */
            if (clean.exitCode !== 0) {
              await expectCommandSuccess(
                container,
                'rm -f wpgtr.pdf',
                {
                  workdir: latexDir,
                },
              );
            }
          },
        },
        {
          id: 'python',
          description:
            'Python tutorial bubblesort (sequential repeats)',
          command:
            'for i in $(seq 1 50); do python3 bubblesort.py >/dev/null; done',
          workdir: interpretedDir,
          timeoutMs: 60_000,
        },
        {
          id: 'ruby',
          description:
            'Ruby tutorial bubblesort (sequential repeats)',
          command:
            'for i in $(seq 1 50); do ruby bubblesort.rb >/dev/null; done',
          workdir: interpretedDir,
          timeoutMs: 60_000,
        },
      ];

      const results: ResourceProfileResult[] = [];

      for (const workload of workloads) {
        await test.step(
          `Prepare ${workload.description}`,
          async () => {
            if (workload.prepare)
              await workload.prepare();
          },
        );

        for (
          let run = 1;
          run <= RUNS_PER_WORKLOAD;
          run += 1
        ) {
          await test.step(
            `${workload.description} — run ${run}/${RUNS_PER_WORKLOAD}`,
            async () => {
              if (workload.beforeRun)
                await workload.beforeRun();

              /* Let short-lived cleanup/preparation processes leave the cgroup. */
              await sleep(250);

              const result = await profileCommand(
                container,
                cgroup.directory,
                workload,
                run,
              );
              results.push(result);

              console.log(
                `[resource-profile] ${workload.id} run=${run} ` +
                `peak=${bytesToMiB(result.peakMemoryBytes)} MiB ` +
                `delta=${bytesToMiB(result.deltaMemoryBytes)} MiB ` +
                `pids=${result.peakPids} ` +
                `baseline_pids=${result.baselinePids} ` +
                `duration=${result.durationMs} ms`,
              );
            },
          );
        }
      }

      const hostImage = (
        await hostCommand(
          'docker',
          [
            'inspect',
            '--format',
            '{{.Image}}',
            container.name,
          ],
        )
      ).trim();

      let workspaceGitSha = 'unknown';
      try {
        workspaceGitSha = (
          await hostCommand(
            'git',
            ['rev-parse', 'HEAD'],
          )
        ).trim();
      }
      catch {
        // The report is still useful from an unpacked source tree.
      }

      const observed = {
        peakMemoryBytes: Math.max(
          ...results.map(
            result => result.peakMemoryBytes,
          ),
        ),
        peakPids: Math.max(
          ...results.map(
            result => result.peakPids,
          ),
        ),
      };

      const report = {
        generatedAt: new Date().toISOString(),
        workspaceGitSha,
        container: {
          name: container.name,
          image: hostImage,
          cpuLimit:
            (hostConfig.NanoCpus ?? 0) /
            1_000_000_000,
          memoryLimitBytes,
          pidsLimit,
        },
        measurement: {
          cgroup: cgroup.source,
          sampleIntervalMs:
            SAMPLE_INTERVAL_MS,
          runsPerWorkload:
            RUNS_PER_WORKLOAD,
          memoryCounter:
            'cgroup v2 memory.current (bytes, whole container including descendants)',
          pidCounter:
            'cgroup v2 pids.current (kernel tasks/TIDs; relevant to Docker --pids-limit)',
        },
        versions,
        observed,
        results,
      };

      const json =
        JSON.stringify(report, null, 2) + '\n';
      const csv = resultsToCsv(results);
      const outputDirectory = path.resolve(
        process.cwd(),
        'test-results',
      );
      const jsonPath = path.join(
        outputDirectory,
        'resource-profile.json',
      );
      const csvPath = path.join(
        outputDirectory,
        'resource-profile.csv',
      );

      await fs.mkdir(
        outputDirectory,
        { recursive: true },
      );
      await fs.writeFile(
        jsonPath,
        json,
        'utf8',
      );
      await fs.writeFile(
        csvPath,
        csv,
        'utf8',
      );

      await testInfo.attach(
        'resource-profile.json',
        {
          path: jsonPath,
          contentType: 'application/json',
        },
      );
      await testInfo.attach(
        'resource-profile.csv',
        {
          path: csvPath,
          contentType: 'text/csv',
        },
      );

      console.log('\nResource profile summary (observed, not proposed limits):');
      console.table(
        results.map(result => ({
          workload: result.workload,
          run: result.run,
          baselineMiB: bytesToMiB(
            result.baselineMemoryBytes,
          ),
          peakMiB: bytesToMiB(
            result.peakMemoryBytes,
          ),
          deltaMiB: bytesToMiB(
            result.deltaMemoryBytes,
          ),
          baselinePids: result.baselinePids,
          peakPids: result.peakPids,
          deltaPids: result.deltaPids,
          durationMs: result.durationMs,
        })),
      );
      console.log(
        `Highest observed memory: ${bytesToMiB(observed.peakMemoryBytes)} MiB`,
      );
      console.log(
        `Highest observed PID/task count: ${observed.peakPids}`,
      );
      console.log(
        `JSON: ${jsonPath}\nCSV:  ${csvPath}`,
      );
    },
  );
}
