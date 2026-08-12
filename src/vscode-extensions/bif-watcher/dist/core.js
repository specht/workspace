"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.NPM_INSTALL_STATE_FILE = exports.BIF_MARKER_FILE = void 0;
exports.dependencyInstallRequired = dependencyInstallRequired;
exports.dependencyInstallCommand = dependencyInstallCommand;
const fs = __importStar(require("node:fs/promises"));
const path = __importStar(require("node:path"));
exports.BIF_MARKER_FILE = ".bif-project";
exports.NPM_INSTALL_STATE_FILE = ".package-lock.json";
async function modificationTime(filePath) {
    try {
        return (await fs.stat(filePath)).mtimeMs;
    }
    catch {
        return undefined;
    }
}
async function pathExists(filePath) {
    try {
        await fs.stat(filePath);
        return true;
    }
    catch {
        return false;
    }
}
function dependencyPath(root, packageName) {
    return path.join(root, "node_modules", ...packageName.split("/"));
}
async function declaredPackageNames(root) {
    try {
        const packageJson = JSON.parse(await fs.readFile(path.join(root, "package.json"), "utf8"));
        const result = new Set();
        for (const field of ["dependencies", "devDependencies"]) {
            const dependencies = packageJson[field];
            if (typeof dependencies !== "object" || dependencies === null) {
                continue;
            }
            for (const packageName of Object.keys(dependencies)) {
                result.add(packageName);
            }
        }
        return [...result];
    }
    catch {
        return undefined;
    }
}
/** Determine whether npm must refresh the repository's dependencies. */
async function dependencyInstallRequired(root) {
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
    const installState = await modificationTime(path.join(root, "node_modules", exports.NPM_INSTALL_STATE_FILE));
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
async function dependencyInstallCommand(root) {
    const hasLockfile = (await pathExists(path.join(root, "package-lock.json"))) ||
        (await pathExists(path.join(root, "npm-shrinkwrap.json")));
    return hasLockfile
        ? "npm ci --prefer-offline --no-audit --no-fund"
        : "npm install --prefer-offline --no-audit --no-fund";
}
//# sourceMappingURL=core.js.map