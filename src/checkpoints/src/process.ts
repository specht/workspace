import { execFile } from "node:child_process";
import { promisify } from "node:util";
const execFileAsync = promisify(execFile);
export async function run(executable: string, args: string[], options: { cwd: string; env?: NodeJS.ProcessEnv; binary?: boolean; allowFailure?: boolean; }): Promise<string | Buffer> {
  try {
    const result = await execFileAsync(executable, args, {
      cwd: options.cwd,
      env: { ...process.env, ...options.env },
      encoding: options.binary ? "buffer" : "utf8",
      maxBuffer: 128 * 1024 * 1024,
      windowsHide: true
    });
    return result.stdout;
  } catch (error: unknown) {
    if (options.allowFailure) return options.binary ? Buffer.alloc(0) : "";
    const message = error instanceof Error ? error.message : String(error);
    throw new Error(`${executable} ${args.join(" ")} failed: ${message}`);
  }
}
