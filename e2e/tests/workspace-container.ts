import crypto from 'node:crypto';
import path from 'node:path';
import { spawn } from 'node:child_process';

export type ContainerCommandResult = {
  command: string;
  stdout: string;
  stderr: string;
  exitCode: number;
};

type CommandOptions = {
  input?: string;
  timeoutMs?: number;
  asRoot?: boolean;
  workdir?: string;
};

const SANDBOX = '/workspace/.e2e';

function fsTagForEmail(email: string): string {
  /*
   * Keep this exactly in sync with Main.fs_tag_for_email in main.rb:
   *
   * Digest::SHA2.hexdigest(email).to_i(16).to_s(36)[0, 16]
   */
  const hex = crypto
    .createHash('sha256')
    .update(email)
    .digest('hex');

  return BigInt(`0x${hex}`)
    .toString(36)
    .slice(0, 16);
}

function shellQuote(value: string): string {
  return `'${value.replace(/'/g, `'\\''`)}'`;
}

function validateRelativePath(relativePath: string) {
  if (
    path.posix.isAbsolute(relativePath) ||
    relativePath.split('/').includes('..')
  ) {
    throw new Error(
      `Workspace test path must be relative and stay inside ${SANDBOX}: ${relativePath}`,
    );
  }
}

function runProcess(
  command: string,
  args: string[],
  input: string | undefined,
  timeoutMs: number,
): Promise<ContainerCommandResult> {
  return new Promise((resolve, reject) => {
    const child = spawn(command, args, {
      stdio: ['pipe', 'pipe', 'pipe'],
    });

    let stdout = '';
    let stderr = '';
    let timedOut = false;

    child.stdout.setEncoding('utf8');
    child.stderr.setEncoding('utf8');

    child.stdout.on('data', chunk => {
      stdout += chunk;
    });

    child.stderr.on('data', chunk => {
      stderr += chunk;
    });

    const timer = setTimeout(() => {
      timedOut = true;
      child.kill('SIGKILL');
    }, timeoutMs);

    child.on('error', error => {
      clearTimeout(timer);
      reject(error);
    });

    child.on('close', code => {
      clearTimeout(timer);

      if (timedOut) {
        reject(
          new Error(
            `${command} ${args.join(' ')} timed out after ${timeoutMs} ms\n` +
            `stdout:\n${stdout}\n` +
            `stderr:\n${stderr}`,
          ),
        );
        return;
      }

      resolve({
        command: [command, ...args].join(' '),
        stdout,
        stderr,
        exitCode: code ?? -1,
      });
    });

    if (input !== undefined)
      child.stdin.write(input);

    child.stdin.end();
  });
}

export class WorkspaceContainer {
  readonly email: string;
  readonly name: string;
  readonly sandbox = SANDBOX;

  constructor(email: string) {
    this.email = email;
    this.name = `hs_code_${fsTagForEmail(email)}`;
  }

  async waitUntilRunning(timeoutMs = 15_000) {
    const deadline = Date.now() + timeoutMs;
    let lastError = '';

    while (Date.now() < deadline) {
      const result = await runProcess(
        'docker',
        [
          'inspect',
          '--format',
          '{{.State.Running}}',
          this.name,
        ],
        undefined,
        5_000,
      );

      if (
        result.exitCode === 0 &&
        result.stdout.trim() === 'true'
      ) {
        return;
      }

      lastError = result.stderr.trim();
      await new Promise(resolve => setTimeout(resolve, 250));
    }

    throw new Error(
      `Workspace container ${this.name} is not running for ${this.email}. ` +
      `Run the workspace-smoke project first. ${lastError}`,
    );
  }

  /**
   * Reset only the test sandbox, not /workspace itself.
   *
   * code-server is currently running from this Workspace, and /workspace also
   * contains its live .local/.extensions state. Deleting the whole mount under
   * a running container is unsafe. All command-line tests therefore work in a
   * disposable /workspace/.e2e directory.
   */
  async resetSandbox() {
    const result = await this.exec(
      `rm -rf ${shellQuote(SANDBOX)} && ` +
      `mkdir -p ${shellQuote(SANDBOX)} && ` +
      `chown 1000:1000 ${shellQuote(SANDBOX)}`,
      {
        asRoot: true,
        workdir: '/',
      },
    );

    if (result.exitCode !== 0) {
      throw new Error(
        `Could not reset ${SANDBOX} in ${this.name}\n` +
        `stdout:\n${result.stdout}\n` +
        `stderr:\n${result.stderr}`,
      );
    }
  }

  /**
   * Run exactly as the Workspace user (uid/gid 1000) by default.
   */
  async exec(
    command: string,
    options: CommandOptions = {},
  ): Promise<ContainerCommandResult> {
    const {
      input,
      timeoutMs = 60_000,
      asRoot = false,
      workdir = SANDBOX,
    } = options;

    const args = [
      'exec',
      '-i',
    ];

    if (!asRoot) {
      args.push(
        '-u',
        '1000:1000',
        '-e',
        'HOME=/workspace',
      );
    }

    args.push(
      '-w',
      workdir,
      this.name,
      'bash',
      '-c',
      command,
    );

    return await runProcess(
      'docker',
      args,
      input,
      timeoutMs,
    );
  }

  async writeFile(
    relativePath: string,
    contents: string,
  ) {
    validateRelativePath(relativePath);

    const target = path.posix.join(
      SANDBOX,
      relativePath,
    );

    const parent = path.posix.dirname(target);

    const result = await this.exec(
      `mkdir -p ${shellQuote(parent)} && ` +
      `cat > ${shellQuote(target)}`,
      {
        input: contents.endsWith('\n')
          ? contents
          : `${contents}\n`,
      },
    );

    if (result.exitCode !== 0) {
      throw new Error(
        `Could not write ${relativePath} in ${this.name}\n` +
        `stdout:\n${result.stdout}\n` +
        `stderr:\n${result.stderr}`,
      );
    }
  }
}
