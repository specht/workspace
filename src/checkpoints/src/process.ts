import { execFile, spawn } from "node:child_process";
import { promisify } from "node:util";

const execFileAsync = promisify(execFile);

export interface RunOptions {
  cwd: string;
  env?: NodeJS.ProcessEnv;
  binary?: boolean;
  allowFailure?: boolean;
}

export async function run(
  executable: string,
  args: string[],
  options: RunOptions,
): Promise<string | Buffer> {
  try {
    const result = await execFileAsync(executable, args, {
      cwd: options.cwd,
      env: { ...process.env, ...options.env },
      encoding: options.binary ? "buffer" : "utf8",
      maxBuffer: 128 * 1024 * 1024,
      windowsHide: true,
    });
    return result.stdout;
  } catch (error: unknown) {
    if (options.allowFailure) return options.binary ? Buffer.alloc(0) : "";
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`${executable} ${args.join(" ")} failed: ${message}`);
  }
}

export async function runWithInput(
  executable: string,
  args: string[],
  input: string | Buffer,
  options: RunOptions,
): Promise<string | Buffer> {
  return new Promise((resolve, reject) => {
    const child = spawn(executable, args, {
      cwd: options.cwd,
      env: { ...process.env, ...options.env },
      stdio: ["pipe", "pipe", "pipe"],
      windowsHide: true,
    });

    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    child.stdout.on("data", chunk => stdout.push(Buffer.from(chunk)));
    child.stderr.on("data", chunk => stderr.push(Buffer.from(chunk)));
    child.on("error", reject);
    child.on("close", code => {
      const output = Buffer.concat(stdout);
      if (code === 0 || options.allowFailure) {
        resolve(options.binary ? output : output.toString("utf8"));
        return;
      }
      reject(new Error(
        `${executable} ${args.join(" ")} failed: ${Buffer.concat(stderr).toString("utf8").trim()}`,
      ));
    });

    child.stdin.end(input);
  });
}
