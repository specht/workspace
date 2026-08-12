import * as fs from "node:fs/promises";
import * as path from "node:path";

export const BIF_MARKER_FILE = ".bif-project";
export const NPM_INSTALL_STATE_FILE = ".package-lock.json";

async function modificationTime(filePath: string): Promise<number | undefined> {
  try {
    return (await fs.stat(filePath)).mtimeMs;
  } catch {
    return undefined;
  }
}

async function pathExists(filePath: string): Promise<boolean> {
  try {
    await fs.stat(filePath);
    return true;
  } catch {
    return false;
  }
}

function dependencyPath(root: string, packageName: string): string {
  return path.join(root, "node_modules", ...packageName.split("/"));
}

async function declaredPackageNames(root: string): Promise<string[] | undefined> {
  try {
    const packageJson = JSON.parse(
      await fs.readFile(path.join(root, "package.json"), "utf8"),
    ) as Record<string, unknown>;
    const result = new Set<string>();

    for (const field of ["dependencies", "devDependencies"] as const) {
      const dependencies = packageJson[field];
      if (typeof dependencies !== "object" || dependencies === null) {
        continue;
      }
      for (const packageName of Object.keys(dependencies)) {
        result.add(packageName);
      }
    }

    return [...result];
  } catch {
    return undefined;
  }
}

/** Determine whether npm must refresh the repository's dependencies. */
export async function dependencyInstallRequired(root: string): Promise<boolean> {
  const packageNames = await declaredPackageNames(root);
  if (packageNames === undefined) {
    return true;
  }
  if (packageNames.length === 0) {
    return false;
  }

  for (const packageName of packageNames) {
    if (!(await pathExists(dependencyPath(root, packageName)))) {
      return true;
    }
  }

  const installState = await modificationTime(
    path.join(root, "node_modules", NPM_INSTALL_STATE_FILE),
  );
  if (installState === undefined) {
    return true;
  }

  for (const dependencyFile of [
    "package.json",
    "package-lock.json",
    "npm-shrinkwrap.json",
  ]) {
    const dependencyTime = await modificationTime(path.join(root, dependencyFile));
    if (dependencyTime !== undefined && dependencyTime > installState) {
      return true;
    }
  }

  return false;
}

/** Choose a reproducible npm install command when the project has a lockfile. */
export async function dependencyInstallCommand(root: string): Promise<string> {
  const hasLockfile =
    (await pathExists(path.join(root, "package-lock.json"))) ||
    (await pathExists(path.join(root, "npm-shrinkwrap.json")));

  return hasLockfile
    ? "npm ci --prefer-offline --no-audit --no-fund"
    : "npm install --prefer-offline --no-audit --no-fund";
}
