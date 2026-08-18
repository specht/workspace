import crypto from 'node:crypto';
import dns from 'node:dns/promises';
import fs from 'node:fs/promises';
import http from 'node:http';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { pathToFileURL } from 'node:url';
import {
    parseTerminalLineCropDirective,
    terminalLineCropPixels,
    validateCropDirectives,
} from './crop.js';
import { resetScreenshotDatabases } from './workspace-reset.js';
import { chromium } from 'playwright';

const execFileAsync = promisify(execFile);

function positiveIntegerEnv(name, fallback) {
    const value = Number.parseInt(process.env[name] || `${fallback}`, 10);
    if (!Number.isInteger(value) || value <= 0) {
        throw new Error(`${name} must be a positive integer`);
    }
    return value;
}

function positiveNumberEnv(name, fallback) {
    const value = Number.parseFloat(process.env[name] || `${fallback}`);
    if (!Number.isFinite(value) || value <= 0) {
        throw new Error(`${name} must be a positive number`);
    }
    return value;
}

function sleep(seconds) {
    return new Promise(resolve => setTimeout(resolve, seconds * 1000));
}

const CONTENT_ROOT = '/content';
const USER_ROOT = '/user';
const GIT_CACHE_ROOT = process.env.TUTORIAL_SCREENSHOT_GIT_CACHE_ROOT || '/git-cache';
const BASE_URL = process.env.TUTORIAL_SCREENSHOT_BASE_URL || 'http://workspace.test:8025';
const SCREENSHOT_EMAIL = process.env.TUTORIAL_SCREENSHOT_EMAIL || 'student@example.com';
const SCREENSHOT_WORKSPACE_USER = process.env.TUTORIAL_SCREENSHOT_WORKSPACE_USER || 'student';
const LOGIN_CODE = process.env.TUTORIAL_SCREENSHOT_LOGIN_CODE || '123456';
const PORT = Number.parseInt(process.env.PORT || '9393', 10);
const GENERATOR_VERSION = 16;
const PLAYWRIGHT_VERSION = '1.62.1';
const MANIFEST_NAME = '.tutorial-screenshots.json';
const DEFAULT_PROFILE = Object.freeze({
    width: positiveIntegerEnv('TUTORIAL_SCREENSHOT_WIDTH', 1853),
    height: positiveIntegerEnv('TUTORIAL_SCREENSHOT_HEIGHT', 929),
    zoom: positiveNumberEnv('TUTORIAL_SCREENSHOT_ZOOM', 1.5),
    desktopScaleFactor: positiveNumberEnv('TUTORIAL_SCREENSHOT_DESKTOP_SCALE_FACTOR', 1.203125),
});
const WORKBENCH_PARTS = Object.freeze({
    'left-sidebar': {
        selector: '.part.sidebar',
        shortcut: 'Control+B',
    },
    'right-sidebar': {
        selector: '.part.auxiliarybar',
        shortcut: 'Control+Alt+B',
    },
    'bottom-panel': {
        selector: '.part.panel',
        shortcut: 'Control+J',
    },
});
const SIMPLE_ACTIONS = new Set([
    'close-folder',
    'go-live',
    'terminal-open',
    'terminal-wait-for-prompt',
    'terminal-maximize',
    'preview-reload',
    'preview-reset',
    'show-left-sidebar',
    'hide-left-sidebar',
    'show-right-sidebar',
    'hide-right-sidebar',
    'show-bottom-panel',
    'hide-bottom-panel',
]);
const WORKSPACE_SCREENSHOT_STYLE = `
.notifications-toasts {
    display: none !important;
}

/* Explorer welcome view can retain keyboard focus after opening the sidebar. */
.part.sidebar .welcome-view-content:focus {
    outline: none !important;
}

/* Keep the input's normal blue focus border, but make list-focus outlines inside
 * Quick Input transparent. code-server 4.131.0's bundled VS Code paints these
 * from theme variables, so overriding the variables is more robust than trying
 * to out-specificity every list/tree focus selector. */
.quick-input-widget {
    --vscode-list-focusOutline: transparent !important;
    --vscode-list-focusAndSelectionOutline: transparent !important;
    --vscode-list-inactiveFocusOutline: transparent !important;
}

/* Fallback for a native/browser outline on the list container itself. */
.quick-input-widget .quick-input-list > .monaco-list,
.quick-input-widget .quick-input-tree > .monaco-list {
    outline: none !important;
}
`;

function screenshotMetricsFor(viewport, zoom) {
    const deviceScaleFactor = DEFAULT_PROFILE.desktopScaleFactor * zoom;
    if (!Number.isFinite(deviceScaleFactor) || deviceScaleFactor <= 0) {
        throw new Error(`Invalid effective device scale factor: ${deviceScaleFactor}`);
    }

    return {
        width: Math.max(1, Math.round(viewport.width / deviceScaleFactor)),
        height: Math.max(1, Math.round(viewport.height / deviceScaleFactor)),
        deviceScaleFactor,
    };
}

let browserPromise = null;
let context = null;
let dashboard = null;
let loggedIn = false;
let workspace = null;
let preview = null;
let generationQueue = Promise.resolve();
const latestGenerationJobs = new Map();
let workspacePrepared = false;
let workspacePreparationPromise = null;
let terminalShellReady = false;
const deviceMetricSessions = new WeakMap();
const appliedViewportProfiles = new WeakMap();
const SOURCE_LOCATION = Symbol('tutorial-screenshot-source-location');
const TARGET_LOCATION = Symbol('tutorial-screenshot-target-location');

function markdownSourceLocation(markdown, markdownPath, offset) {
    const boundedOffset = Math.max(0, Math.min(offset, markdown.length));
    let line = 1;
    for (let index = 0; index < boundedOffset; index += 1) {
        if (markdown.charCodeAt(index) === 10) line += 1;
    }

    const lineStart = markdown.lastIndexOf('\n', boundedOffset - 1) + 1;
    const nextNewline = markdown.indexOf('\n', boundedOffset);
    const lineEnd = nextNewline < 0 ? markdown.length : nextNewline;

    return {
        markdownPath,
        line,
        text: markdown.slice(lineStart, lineEnd),
    };
}

function withSourceError(error, source) {
    if (!source || error?.tutorialScreenshotSource) return error;

    const original = error instanceof Error ? error : new Error(`${error}`);
    const wrapped = new Error(
        `${source.markdownPath}:${source.line}: ${original.message}\n` +
        `Source: ${source.text || '(blank line)'}`,
    );
    wrapped.cause = original;
    wrapped.tutorialScreenshotSource = source;
    return wrapped;
}

function valueAtSource(source, callback) {
    try {
        return callback();
    } catch (error) {
        throw withSourceError(error, source);
    }
}

function sha256(value) {
    return crypto.createHash('sha256').update(value).digest('hex');
}

function fsTagForEmail(email) {
    const hex = sha256(email);
    return BigInt(`0x${hex}`).toString(36).slice(0, 16);
}

function safeRelativePath(value) {
    if (typeof value !== 'string' || value.length === 0 || value.includes('\0')) {
        throw new Error(`Invalid path: ${JSON.stringify(value)}`);
    }
    const normalized = value.replaceAll('\\', '/').replace(/^\.\//, '');
    if (path.posix.isAbsolute(normalized) || normalized.split('/').includes('..')) {
        throw new Error(`Path escapes its root: ${value}`);
    }
    return normalized;
}

async function loadTutorialScreenshotHooks(tutorialDirectory) {
    const hooksPath = path.join(tutorialDirectory, 'tutorial-screenshot-hooks.mjs');
    let source;
    try {
        source = await fs.readFile(hooksPath, 'utf8');
    } catch (error) {
        if (error?.code === 'ENOENT') {
            return {
                sourceSha256: null,
                writeFileSubdirectory: '',
                waitForPreview: null,
            };
        }
        throw error;
    }

    const sourceSha256 = sha256(source);
    const moduleUrl = pathToFileURL(hooksPath);
    moduleUrl.searchParams.set('sha256', sourceSha256);
    const imported = await import(moduleUrl.href);
    const hooks = imported.default ?? imported;

    if (!hooks || typeof hooks !== 'object') {
        throw new Error('tutorial-screenshot-hooks.mjs must export an object');
    }
    if (hooks.waitForPreview != null && typeof hooks.waitForPreview !== 'function') {
        throw new Error('tutorial-screenshot-hooks.mjs waitForPreview must be a function');
    }

    const rawWriteFileSubdirectory = hooks.writeFileSubdirectory == null
        ? ''
        : `${hooks.writeFileSubdirectory}`.trim();

    return {
        sourceSha256,
        writeFileSubdirectory: rawWriteFileSubdirectory
            ? safeRelativePath(rawWriteFileSubdirectory)
            : '',
        waitForPreview: hooks.waitForPreview ?? null,
    };
}

async function waitForPreviewReady(page, hooks) {
    if (!hooks?.waitForPreview) return;
    await hooks.waitForPreview({ page, timeout: 60_000 });
}

function parsePercent(value) {
    const text = `${value}`.trim();
    const numeric = Number.parseFloat(text.endsWith('%') ? text.slice(0, -1) : text);
    if (!Number.isFinite(numeric)) throw new Error(`Invalid percentage: ${value}`);
    const result = text.endsWith('%') ? numeric / 100 : numeric;
    if (result < 0 || result >= 1) throw new Error(`Crop must be between 0% and 99%: ${value}`);
    return result;
}

function codeBlocks(markdown, markdownPath) {
    const blocks = [];
    const labels = new Set();
    const regex = /```[^\n]*\n([\s\S]*?)```/g;
    let match;
    while ((match = regex.exec(markdown)) !== null) {
        const beforeFence = markdown.slice(0, match.index);
        const labelMatch = /<!--\s*screenshot-code:\s*([^\r\n>]+?)\s*-->\s*$/.exec(beforeFence);
        const label = labelMatch?.[1].trim() || null;

        if (label) {
            if (labels.has(label)) {
                const source = markdownSourceLocation(markdown, markdownPath, labelMatch.index);
                throw withSourceError(
                    new Error(`Duplicate screenshot-code label: ${label}`),
                    source,
                );
            }
            labels.add(label);
        }

        blocks.push({
            start: match.index,
            end: regex.lastIndex,
            contents: match[1].replace(/\n$/, ''),
            label,
        });
    }
    return blocks;
}

function findCodeSource(blocks, recipeStart, selector) {
    const before = blocks.filter(block => block.end <= recipeStart);
    if (selector === 'previous-code') {
        const block = before.at(-1);
        if (!block) throw new Error('previous-code requested before any fenced code block');
        return block.contents;
    }

    const labelledPrevious = /^previous-code\s+\(([^)]+)\)$/.exec(selector);
    if (labelledPrevious) {
        const label = labelledPrevious[1].trim();
        const block = before.find(candidate => candidate.label === label);
        if (!block) {
            throw new Error(`Could not find a previous code block labelled: ${label}`);
        }
        return block.contents;
    }

    if (selector.startsWith('code:')) {
        const needles = selector.slice(5).split('||').map(value => value.trim()).filter(Boolean);
        const block = [...before].reverse().find(candidate =>
            needles.every(needle => candidate.contents.includes(needle)),
        );
        if (!block) {
            throw new Error(`Could not find a previous code block matching: ${needles.join(' / ')}`);
        }
        return block.contents;
    }

    throw new Error(`Unknown code source: ${selector}`);
}

function parseTarget(value) {
    const target = value.trim();
    if (target.startsWith('selector:')) {
        const selector = target.slice('selector:'.length).trim();
        if (!selector) throw new Error('Target selector must not be empty');
        return { selector };
    }
    if (!target) throw new Error('Target text must not be empty');
    return { text: target };
}

function parseRecipe(text, markdown, markdownPath, recipeStart) {
    const recipe = {
        tab: 'workspace',
        viewport: { width: DEFAULT_PROFILE.width, height: DEFAULT_PROFILE.height },
        zoom: DEFAULT_PROFILE.zoom,
        cropTop: 0,
        cropBottom: 0,
        cropTerminalLines: null,
        actions: [],
    };

    let rawOffset = 0;
    let cropSource = null;
    let cropTopSpecified = false;
    let cropBottomSpecified = false;
    let lastSource = null;

    for (const rawLine of text.split('\n')) {
        const lineOffset = rawOffset;
        rawOffset += rawLine.length + 1;
        const line = rawLine.trim();
        if (!line || line.startsWith('#')) continue;

        const source = markdownSourceLocation(
            markdown,
            markdownPath,
            recipeStart + lineOffset,
        );
        recipe[SOURCE_LOCATION] ||= source;
        lastSource = source;

        if (SIMPLE_ACTIONS.has(line)) {
            const action = { type: line };
            action[SOURCE_LOCATION] = source;
            recipe.actions.push(action);
            continue;
        }

        const actionCount = recipe.actions.length;
        let match;
        if ((match = line.match(/^clone-start:\s*(\S+)\s+@\s*(\S+)$/))) {
            const commit = match[2].trim();
            if (!/^[0-9a-f]{40}$/i.test(commit)) {
                throw withSourceError(
                    new Error(`Pinned clone commit must be a full 40-character SHA-1: ${commit}`),
                    source,
                );
            }
            recipe.actions.push({
                type: 'clone-start',
                url: match[1].trim(),
                commit: commit.toLowerCase(),
            });
        } else if ((match = line.match(/^clone-start:\s*(.+)$/))) {
            recipe.actions.push({ type: 'clone-start', url: match[1].trim() });
        } else if ((match = line.match(/^left-sidebar-width:\s*(\d+)$/))) {
            const width = Number(match[1]);
            if (width < 120 || width > 1000) {
                throw withSourceError(
                    new Error(`left-sidebar-width must be between 120 and 1000 CSS pixels: ${width}`),
                    source,
                );
            }
            recipe.actions.push({ type: 'left-sidebar-width', width });
        } else if ((match = line.match(/^open-file:\s*(.+)$/))) {
            recipe.actions.push({
                type: 'open-file',
                path: valueAtSource(source, () => safeRelativePath(match[1].trim())),
            });
        } else if ((match = line.match(/^terminal-run:\s*(.+)$/))) {
            recipe.actions.push({ type: 'terminal-run', text: match[1].trim() });
        } else if ((match = line.match(/^click:\s*(.+)$/))) {
            recipe.actions.push({
                type: 'click',
                ...valueAtSource(source, () => parseTarget(match[1])),
            });
        } else if ((match = line.match(/^hold:\s*([0-9]+(?:\.[0-9]+)?)s\s+(.+)$/))) {
            const seconds = Number.parseFloat(match[1]);
            if (seconds < 0.05 || seconds > 30) {
                throw withSourceError(
                    new Error(`hold must be between 0.05 and 30 seconds: ${match[1]}s`),
                    source,
                );
            }
            recipe.actions.push({
                type: 'hold',
                seconds,
                ...valueAtSource(source, () => parseTarget(match[2])),
            });
        } else if ((match = line.match(/^move-mouse:\s*(.+)$/))) {
            recipe.actions.push({
                type: 'move-mouse',
                ...valueAtSource(source, () => parseTarget(match[1])),
            });
        } else if ((match = line.match(/^type:\s*(.+)$/))) {
            recipe.actions.push({ type: 'type', text: match[1] });
        } else if ((match = line.match(/^press:\s*(.+)$/))) {
            recipe.actions.push({ type: 'press', key: match[1].trim() });
        } else if ((match = line.match(/^sleep:\s*(.+)$/))) {
            const seconds = Number(match[1].trim());
            if (!Number.isFinite(seconds) || seconds < 0 || seconds > 300) {
                throw withSourceError(
                    new Error(`sleep must be between 0 and 300 seconds: ${match[1]}`),
                    source,
                );
            }
            recipe.actions.push({ type: 'sleep', seconds });
        } else if ((match = line.match(/^wait-for-text:\s*(.+)$/))) {
            const text = match[1].trim();
            recipe.actions.push({ type: 'wait-for-text', text });
        } else if ((match = line.match(/^wait-for-input-value:\s*(.+)$/))) {
            const text = match[1].trim();
            recipe.actions.push({ type: 'wait-for-input-value', text });
        } else if ((match = line.match(/^write-file:\s*(.+?)\s*<-\s*(previous-code(?:\s+\([^)]+\))?|code:.+|file:.+)$/))) {
            recipe.actions.push({
                type: 'write-file',
                path: valueAtSource(source, () => safeRelativePath(match[1].trim())),
                source: match[2].trim(),
            });
        } else if ((match = line.match(/^wait-for-file:\s*(.+)$/))) {
            recipe.actions.push({
                type: 'wait-for-file',
                path: valueAtSource(source, () => safeRelativePath(match[1].trim())),
            });
        } else if ((match = line.match(/^wait-for-file-newer:\s*(.+?)\s*<-\s*(.+)$/))) {
            recipe.actions.push({
                type: 'wait-for-file-newer',
                target: valueAtSource(source, () => safeRelativePath(match[1].trim())),
                source: valueAtSource(source, () => safeRelativePath(match[2].trim())),
            });
        } else if ((match = line.match(/^close-tab:\s*(.+)$/))) {
            recipe.actions.push({
                type: 'close-tab',
                label: match[1].trim(),
            });
        } else if ((match = line.match(/^tab:\s*(workspace|preview)$/))) {
            recipe.tab = match[1];
        } else if ((match = line.match(/^viewport:\s*(\d+)x(\d+)$/i))) {
            recipe.viewport = { width: Number(match[1]), height: Number(match[2]) };
        } else if ((match = line.match(/^zoom:\s*([0-9.]+)$/))) {
            recipe.zoom = Number.parseFloat(match[1]);
            if (!Number.isFinite(recipe.zoom) || recipe.zoom <= 0) {
                throw withSourceError(new Error(`Invalid zoom: ${match[1]}`), source);
            }
        } else if ((match = line.match(/^crop-top:\s*(.+)$/))) {
            cropSource = source;
            cropTopSpecified = true;
            recipe.cropTop = valueAtSource(source, () => parsePercent(match[1]));
        } else if ((match = line.match(/^crop-bottom:\s*(.+)$/))) {
            cropSource = source;
            cropBottomSpecified = true;
            recipe.cropBottom = valueAtSource(source, () => parsePercent(match[1]));
        } else if (line.startsWith('crop-terminal-lines:')) {
            cropSource = source;
            recipe.cropTerminalLines = valueAtSource(
                source,
                () => parseTerminalLineCropDirective(line),
            );
        } else {
            throw withSourceError(
                new Error(`Unknown tutorial screenshot instruction: ${line}`),
                source,
            );
        }

        for (let index = actionCount; index < recipe.actions.length; index += 1) {
            recipe.actions[index][SOURCE_LOCATION] = source;
        }
    }

    if (recipe.cropTop + recipe.cropBottom >= 0.98) {
        throw withSourceError(
            new Error('Top and bottom crop leave no useful screenshot area'),
            cropSource || lastSource || recipe[SOURCE_LOCATION],
        );
    }
    valueAtSource(
        cropSource || lastSource || recipe[SOURCE_LOCATION],
        () => validateCropDirectives({
            cropTopSpecified,
            cropBottomSpecified,
            cropTerminalLines: recipe.cropTerminalLines,
            tab: recipe.tab,
        }),
    );
    return recipe;
}

async function resolveWriteFileSource(blocks, recipeStart, selector, markdownPath) {
    if (!selector.startsWith('file:')) {
        return findCodeSource(blocks, recipeStart, selector);
    }

    const relativeSource = safeRelativePath(selector.slice('file:'.length).trim());
    const tutorialDirectory = path.resolve(
        CONTENT_ROOT,
        path.dirname(markdownPath),
    );
    const sourcePath = path.resolve(tutorialDirectory, relativeSource);

    if (
        sourcePath !== tutorialDirectory &&
        !sourcePath.startsWith(`${tutorialDirectory}${path.sep}`)
    ) {
        throw new Error(`write-file source escaped the tutorial directory: ${relativeSource}`);
    }

    let stat;
    try {
        stat = await fs.stat(sourcePath);
    } catch (error) {
        if (error?.code === 'ENOENT') {
            throw new Error(`write-file source does not exist: ${relativeSource}`);
        }
        throw error;
    }
    if (!stat.isFile()) throw new Error(`write-file source is not a file: ${relativeSource}`);

    const bytes = await fs.readFile(sourcePath);
    return {
        sourcePath,
        sourceSha256: sha256(bytes),
    };
}

async function parseTutorial(markdown, markdownPath) {
    const blocks = codeBlocks(markdown, markdownPath);
    const regex = /<!--\s*tutorial-screenshot\s*\n([\s\S]*?)-->/g;
    const shots = [];
    let match;
    let previousState = sha256(`tutorial-screenshot-v${GENERATOR_VERSION}\0${markdownPath}`);

    while ((match = regex.exec(markdown)) !== null) {
        const recipeStart = match.index + match[0].indexOf(match[1]);
        const recipeSource = markdownSourceLocation(markdown, markdownPath, recipeStart);
        let recipe;
        try {
            recipe = parseRecipe(match[1], markdown, markdownPath, recipeStart);
        } catch (error) {
            throw withSourceError(error, recipeSource);
        }

        const afterComment = markdown.slice(regex.lastIndex);
        const nextRecipeIndex = afterComment.search(/<!--\s*tutorial-screenshot\b/);
        const imageMatch = /<img\b[^>]*\bsrc=['"]([^'"]+)['"][^>]*>/i.exec(afterComment);
        if (!imageMatch || (nextRecipeIndex >= 0 && imageMatch.index > nextRecipeIndex)) {
            throw withSourceError(
                new Error('tutorial-screenshot block has no following <img>'),
                recipeSource,
            );
        }

        const targetSource = markdownSourceLocation(
            markdown,
            markdownPath,
            regex.lastIndex + imageMatch.index,
        );
        let target;
        try {
            target = safeRelativePath(imageMatch[1]);
            if (/^[a-z][a-z\d+.-]*:/i.test(target) || target.startsWith('/')) {
                throw new Error(`Generated screenshot must use a relative tutorial image path: ${target}`);
            }
        } catch (error) {
            throw withSourceError(error, targetSource);
        }

        const resolvedActions = await Promise.all(recipe.actions.map(async action => {
            if (action.type !== 'write-file') return action;
            try {
                return {
                    ...action,
                    contents: await resolveWriteFileSource(
                        blocks,
                        match.index,
                        action.source,
                        markdownPath,
                    ),
                };
            } catch (error) {
                throw withSourceError(
                    error,
                    action[SOURCE_LOCATION] || recipeSource,
                );
            }
        }));

        const resolved = {
            target,
            tab: recipe.tab,
            viewport: recipe.viewport,
            zoom: recipe.zoom,
            cropTop: recipe.cropTop,
            cropBottom: recipe.cropBottom,
            actions: resolvedActions,
        };
        if (recipe.cropTerminalLines != null) {
            resolved.cropTerminalLines = recipe.cropTerminalLines;
        }
        resolved[SOURCE_LOCATION] = recipe[SOURCE_LOCATION] || recipeSource;
        resolved[TARGET_LOCATION] = targetSource;

        const stateHash = sha256(`${previousState}\0${JSON.stringify(resolved)}`);
        previousState = stateHash;
        shots.push({ ...resolved, stateHash });
    }

    return shots;
}

async function readManifest(directory) {
    const manifestPath = path.join(directory, MANIFEST_NAME);
    try {
        const parsed = JSON.parse(await fs.readFile(manifestPath, 'utf8'));
        if (parsed?.version === 1 && parsed.screenshots && typeof parsed.screenshots === 'object') {
            return parsed;
        }
    } catch (error) {
        if (error?.code !== 'ENOENT') console.warn(`Ignoring invalid ${manifestPath}:`, error.message);
    }
    return { version: 1, screenshots: {} };
}

async function inheritDirectoryOwnership(target, directory) {
    try {
        const owner = await fs.stat(directory);
        await fs.chown(target, owner.uid, owner.gid);
    } catch (error) {
        console.warn(`Could not copy ownership to ${target}: ${error.message}`);
    }
}

async function writeManifest(directory, manifest) {
    const target = path.join(directory, MANIFEST_NAME);
    const temporary = `${target}.tmp-${process.pid}-${Date.now()}`;
    await fs.writeFile(temporary, `${JSON.stringify(manifest, null, 2)}\n`, 'utf8');
    await fs.rename(temporary, target);
    await inheritDirectoryOwnership(target, directory);
}

async function ensureBrowser() {
    if (!browserPromise) {
        browserPromise = (async () => {
            // Keep the browser-visible workspace.test URLs (cookies and nginx
            // routing depend on them), but connect directly to the Compose nginx
            // service. Chromium host mapping can override both host and port, so
            // URLs containing the host-published :8025 development port still
            // reach nginx on its container port 80. The wildcard rule does the
            // same for code-server and Live Server subdomains.
            let nginxAddress;
            try {
                nginxAddress = (await dns.lookup('nginx', { family: 4 })).address;
            } catch (error) {
                throw new Error(`Could not resolve the Compose nginx service: ${error.message}`);
            }

            const hostname = new URL(BASE_URL).hostname;
            const hostRules = [
                `MAP ${hostname} ${nginxAddress}:80`,
                `MAP *.${hostname} ${nginxAddress}:80`,
                'EXCLUDE localhost',
            ].join(',');
            console.log(`Tutorial screenshot browser routing: ${hostRules}`);

            return chromium.launch({
                headless: true,
                args: [
                    `--host-resolver-rules=${hostRules}`,
                    '--disable-font-subpixel-positioning',
                    '--font-render-hinting=none',
                ],
            });
        })();
    }
    return browserPromise;
}

async function ensureContext() {
    if (context) return context;
    const browser = await ensureBrowser();
    const metrics = screenshotMetricsFor(DEFAULT_PROFILE, DEFAULT_PROFILE.zoom);
    context = await browser.newContext({
        viewport: { width: metrics.width, height: metrics.height },
        deviceScaleFactor: metrics.deviceScaleFactor,
        acceptDownloads: false,
    });
    dashboard = await context.newPage();
    return context;
}

async function ensureBootstrapPage() {
    await ensureContext();
    const expected = new URL('/_tutorial_screenshot_bootstrap.html', BASE_URL).href;
    if (dashboard.url() === expected) return;
    await dashboard.goto(expected, { waitUntil: 'domcontentloaded', timeout: 30_000 });
}

async function browserRequest(pathname, { method = 'POST', body = {}, redirect = 'follow' } = {}) {
    await ensureBootstrapPage();
    return dashboard.evaluate(async ({ pathname, method, body, redirect }) => {
        const options = {
            method,
            credentials: 'include',
            redirect,
            headers: {},
        };
        if (body !== null) {
            options.headers['content-type'] = 'application/json';
            options.body = JSON.stringify(body);
        }
        const response = await fetch(pathname, options);
        return {
            status: response.status,
            type: response.type,
            url: response.url,
            body: response.type === 'opaqueredirect' ? '' : await response.text(),
        };
    }, { pathname, method, body, redirect });
}

function jsonResponse(result, label) {
    if (result.status < 200 || result.status >= 300) {
        throw new Error(`${label} failed (${result.status}): ${result.body}`);
    }
    try {
        return JSON.parse(result.body);
    } catch {
        throw new Error(`${label} returned invalid JSON: ${result.body}`);
    }
}

async function ensureLoggedIn() {
    if (loggedIn) return;

    const requested = jsonResponse(await browserRequest('/api/request_login', {
        body: { email: SCREENSHOT_EMAIL },
    }), 'Tutorial screenshot login request');
    if (!requested.tag) throw new Error('Tutorial screenshot login request returned no tag');

    // Complete login through the same JSON API used by the normal login page.
    // This creates the Session node and sets the current host-only session cookie
    // without rendering /, which would recurse into parse_content() while a
    // tutorial screenshot request is already in progress.
    jsonResponse(await browserRequest('/api/complete_login', {
        body: {
            tag: requested.tag,
            code: LOGIN_CODE,
        },
    }), 'Tutorial screenshot login completion');

    const probe = await browserRequest('/api/server_start_status', { body: {} });
    if (probe.status < 200 || probe.status >= 300) {
        throw new Error(`Tutorial screenshot login did not establish a session (${probe.status}): ${probe.body}`);
    }
    loggedIn = true;
}

async function waitForWorkspaceStart() {
    const deadline = Date.now() + 180_000;
    while (Date.now() < deadline) {
        const result = jsonResponse(await browserRequest('/api/server_start_status', {
            body: {},
        }), 'Workspace start status');
        if (result.status === 'ready') return;
        if (result.status === 'failed') {
            throw new Error(`Tutorial screenshot Workspace failed to start: ${result.error || 'unknown error'}`);
        }
        await sleep(0.25);
    }
    throw new Error('Timed out waiting for tutorial screenshot Workspace to start');
}

async function waitForWorkspaceWorkbench(page, workspaceUrl) {
    const deadline = Date.now() + 120_000;
    let attempt = 0;
    let lastStatus = null;
    let lastError = '';
    let lastTitle = '';
    let lastBody = '';

    while (Date.now() < deadline) {
        attempt += 1;
        try {
            const response = await page.goto(workspaceUrl, {
                waitUntil: 'domcontentloaded',
                timeout: 15_000,
            });
            lastStatus = response?.status() ?? null;
            lastError = '';

            if (lastStatus !== null && lastStatus >= 200 && lastStatus < 400) {
                try {
                    const remaining = Math.max(1_000, deadline - Date.now());
                    await page.locator('.monaco-workbench').waitFor({
                        state: 'visible',
                        timeout: Math.min(30_000, remaining),
                    });
                    return;
                } catch (error) {
                    lastError = error.message;
                }
            } else {
                lastError = `HTTP ${lastStatus ?? 'no response'}`;
            }
        } catch (error) {
            lastError = error.message;
        }

        lastTitle = await page.title().catch(() => '');
        lastBody = await page.locator('body').innerText().catch(() => '');
        lastBody = lastBody.replace(/\s+/g, ' ').trim().slice(0, 500);

        if (lastStatus === 401 || lastStatus === 403) break;
        if (Date.now() >= deadline) break;

        console.log(
            `Tutorial screenshot Workspace not ready yet ` +
            `(attempt ${attempt}, status ${lastStatus ?? 'n/a'}); retrying...`,
        );
        await page.waitForTimeout(1_000);
    }

    throw new Error([
        'Timed out waiting for the tutorial screenshot Workspace UI.',
        `URL: ${workspaceUrl}`,
        `Last HTTP status: ${lastStatus ?? 'n/a'}`,
        `Last title: ${lastTitle || '(empty)'}`,
        `Last page body: ${lastBody || '(empty)'}`,
        `Last error: ${lastError || '(none)'}`,
    ].join('\n'));
}

async function waitForFreshWorkspaceUi(page) {
    // `.monaco-workbench` becomes available before all built-in contributions
    // have finished starting. In particular, Explorer's "Clone Repository"
    // welcome action appears a little later.
    await page.getByText('NO FOLDER OPENED', { exact: false }).first().waitFor({
        state: 'visible',
        timeout: 30_000,
    });
    await page.getByText('Clone Repository', { exact: true }).first().waitFor({
        state: 'visible',
        timeout: 30_000,
    });
    await page.waitForTimeout(250);
}

async function waitForWorkbenchThemeSettled(page) {
    const deadline = Date.now() + 30_000;
    const stableForMs = 1_000;
    let lastSignature = null;
    let stableSince = null;

    while (Date.now() < deadline) {
        const state = await page.locator('.monaco-workbench').evaluate(workbench => {
            const themeClass = ['hc-black', 'hc-light', 'vs-dark', 'vs']
                .find(name => workbench.classList.contains(name));
            if (!themeClass) return null;

            const style = getComputedStyle(workbench);
            return {
                themeClass,
                backgroundColor: style.backgroundColor,
                foregroundColor: style.color,
                editorBackground: style.getPropertyValue('--vscode-editor-background').trim(),
            };
        }).catch(() => null);

        if (!state) {
            lastSignature = null;
            stableSince = null;
        } else {
            const signature = JSON.stringify(state);
            if (signature !== lastSignature) {
                lastSignature = signature;
                stableSince = Date.now();
            } else if (Date.now() - stableSince >= stableForMs) {
                console.log(`Tutorial screenshot Workspace theme settled: ${state.themeClass}`);
                return;
            }
        }

        await sleep(0.1);
    }

    throw new Error('Timed out waiting for the Workspace color theme to settle');
}

async function prepareFreshWorkspace() {
    workspacePrepared = false;
    await ensureLoggedIn();

    for (const page of context.pages()) {
        if (page !== dashboard) await page.close().catch(() => {});
    }
    workspace = null;
    preview = null;
    terminalShellReady = false;

    jsonResponse(await browserRequest('/api/reset_server', {
        body: { confirmation: SCREENSHOT_EMAIL },
    }), 'Tutorial screenshot Workspace reset');

    // A normal Workspace reset deliberately preserves database contents. A
    // screenshot Workspace must be pristine across renders, so compose the same
    // per-user database list/delete/reset/provisioning paths that the application
    // exposes instead of implementing database cleanup in the screenshot service.
    await resetScreenshotDatabases(browserRequest, jsonResponse);

    // The request after reset sees the newly generated server_sid and therefore
    // refreshes Chromium's private-workspace cookie without rendering a page.
    const start = jsonResponse(await browserRequest('/api/start_server', {
        body: {},
    }), 'Tutorial screenshot Workspace start');
    const serverTag = start.server_tag;
    if (!serverTag) throw new Error('Workspace start returned no server_tag');
    if (start.status === 'failed') {
        throw new Error(`Tutorial screenshot Workspace failed to start: ${start.error || 'unknown error'}`);
    }
    if (start.status !== 'ready') await waitForWorkspaceStart();

    const base = new URL(BASE_URL);
    const workspaceUrl = `${base.protocol}//${serverTag}.${base.hostname}${base.port ? `:${base.port}` : ''}/`;
    workspace = await context.newPage();
    await waitForWorkspaceWorkbench(workspace, workspaceUrl);
    await applyViewportProfile(workspace, DEFAULT_PROFILE, DEFAULT_PROFILE.zoom);
    await waitForFreshWorkspaceUi(workspace);
    await waitForWorkbenchThemeSettled(workspace);
    workspacePrepared = true;
}

async function acquireFreshWorkspace() {
    // A previous render may already be preparing the next pristine Workspace in
    // the background. Only wait for that work when screenshots actually need a
    // Workspace; a no-op tutorial reload remains immediate.
    if (workspacePreparationPromise) {
        await workspacePreparationPromise;
    }

    if (!workspacePrepared || !workspace || workspace.isClosed()) {
        await prepareFreshWorkspace();
    }

    // The first recipe action consumes the prepared state. After this point the
    // Workspace belongs to the current render and must never be reused as fresh.
    workspacePrepared = false;
}

function scheduleFreshWorkspace() {
    if (workspacePrepared || workspacePreparationPromise) return;

    console.log('Preparing next tutorial screenshot Workspace in background...');
    workspacePreparationPromise = prepareFreshWorkspace()
        .then(() => {
            console.log('Next tutorial screenshot Workspace is ready');
        })
        .catch(error => {
            workspacePrepared = false;
            console.error(`Could not prepare next tutorial screenshot Workspace: ${error.message}`);
        })
        .finally(() => {
            workspacePreparationPromise = null;
        });
}

function quickInput(page) {
    return page.locator('.quick-input-box input').last();
}

async function locatorIsVisible(locator) {
    return locator.isVisible().catch(() => false);
}

async function runVsCodeCommand(query, expectedText = query) {
    const workbench = workspace.locator('.monaco-workbench');
    await workbench.waitFor({ state: 'visible', timeout: 10_000 });

    // Start from a predictable workbench focus before opening Quick Input. This
    // mirrors the browser tests and avoids inheriting focus from a terminal,
    // editor, or toolbar control.
    await workbench.click({ position: { x: 500, y: 100 } });
    await workspace.keyboard.press('Control+Shift+P');

    const widget = workspace.locator('.quick-input-widget:visible');
    await widget.waitFor({ state: 'visible', timeout: 10_000 });

    const input = widget.locator('input').last();
    await input.waitFor({ state: 'visible', timeout: 10_000 });
    await input.fill(`>${query}`);

    const row = widget
        .locator('.monaco-list-row')
        .filter({ hasText: expectedText })
        .first();
    await row.waitFor({ state: 'visible', timeout: 30_000 });

    // Clicking the resolved command is less racy than pressing Enter while the
    // command-palette result list is still updating.
    await row.click();
    await widget.waitFor({ state: 'hidden', timeout: 10_000 }).catch(() => {});
}

async function focusTerminal() {
    const terminal = workspace.locator('.terminal.xterm:visible').last();
    await terminal.waitFor({ state: 'visible', timeout: 30_000 });

    // Focus xterm's keyboard input directly. VS Code can place terminal widgets
    // above the screen, so a pointer click on .xterm-screen can be intercepted.
    const textarea = terminal.locator('textarea.xterm-helper-textarea').first();
    await textarea.waitFor({ state: 'attached', timeout: 10_000 });
    await textarea.focus();
    await workspace.waitForTimeout(150);
}

async function waitForTerminalPrompt() {
    await workspace.waitForFunction(
        user => {
            const terminals = [...document.querySelectorAll('.terminal.xterm')]
                .filter(terminal => {
                    const rect = terminal.getBoundingClientRect();
                    return rect.width > 0 && rect.height > 0;
                });
            const terminal = terminals.at(-1);
            if (!terminal) return false;

            const lines = [...terminal.querySelectorAll('.xterm-rows > div')]
                .map(row => (row.textContent || '').replace(/\s+/g, ' ').trim())
                .filter(Boolean);
            const lastLine = lines.at(-1) || '';
            return lastLine.includes(`${user}@workspace`) && /[#$]$/.test(lastLine);
        },
        SCREENSHOT_WORKSPACE_USER,
        { timeout: 60_000 },
    );
    await workspace.waitForTimeout(150);
}

async function waitForInitialTerminalPrompt() {
    if (terminalShellReady) return;

    await waitForTerminalPrompt();
    terminalShellReady = true;
}

async function terminalOpen() {
    const terminal = workspace.locator('.terminal.xterm:visible').last();
    if (!(await locatorIsVisible(terminal))) {
        await runVsCodeCommand('View: Toggle Terminal', 'Toggle Terminal');
    }
    await focusTerminal();
    await waitForInitialTerminalPrompt();
}

async function terminalMaximize() {
    await terminalOpen();

    const panel = workspace.locator('.part.panel').first();
    const restore = panel.locator(
        '[aria-label*="Restore Panel"], [title*="Restore Panel"]',
    ).first();

    if (!(await locatorIsVisible(restore))) {
        const maximize = panel.locator(
            '[aria-label*="Maximize Panel"], [title*="Maximize Panel"]',
        ).first();

        if (await locatorIsVisible(maximize)) {
            await maximize.click();
        } else {
            // The title-bar action is normally present, but the command palette
            // is a stable fallback if VS Code changes that toolbar markup.
            await runVsCodeCommand(
                'View: Toggle Maximized Panel',
                'Toggle Maximized Panel',
            );
        }
        await workspace.waitForTimeout(200);
    }

    await focusTerminal();
}

async function terminalRun(text) {
    await terminalOpen();
    await workspace.keyboard.insertText(text);
    await workspace.keyboard.press('Enter');
    await workspace.waitForTimeout(250);
}

async function setWorkbenchPartVisible(partName, visible) {
    const spec = WORKBENCH_PARTS[partName];
    if (!spec) throw new Error(`Unknown workbench part: ${partName}`);

    const part = workspace.locator(spec.selector).first();
    if ((await locatorIsVisible(part)) === visible) return;

    await workspace.keyboard.press(spec.shortcut);
    await part.waitFor({
        state: visible ? 'visible' : 'hidden',
        timeout: 10_000,
    });
    await workspace.waitForTimeout(150);
}

async function setLeftSidebarWidth(width) {
    const sidebar = workspace.locator(WORKBENCH_PARTS['left-sidebar'].selector).first();
    await sidebar.waitFor({ state: 'visible', timeout: 10_000 });

    const sidebarBox = await sidebar.boundingBox();
    if (!sidebarBox) throw new Error('Could not measure the left sidebar');

    const edgeX = sidebarBox.x + sidebarBox.width;
    const sashes = workspace.locator('.monaco-sash.vertical');
    const sashCount = await sashes.count();
    let sashBox = null;
    let bestDistance = Number.POSITIVE_INFINITY;

    for (let index = 0; index < sashCount; index += 1) {
        const box = await sashes.nth(index).boundingBox();
        if (!box) continue;

        const verticalOverlap =
            box.y < sidebarBox.y + sidebarBox.height &&
            box.y + box.height > sidebarBox.y;
        if (!verticalOverlap) continue;

        const distance = Math.abs((box.x + box.width / 2) - edgeX);
        if (distance < bestDistance) {
            bestDistance = distance;
            sashBox = box;
        }
    }

    if (!sashBox || bestDistance > 12) {
        throw new Error(`Could not find the left sidebar resize sash (nearest was ${bestDistance.toFixed(1)}px away)`);
    }

    const startX = sashBox.x + sashBox.width / 2;
    const startY = Math.max(
        sidebarBox.y + 20,
        Math.min(
            sashBox.y + sashBox.height / 2,
            sidebarBox.y + sidebarBox.height - 20,
        ),
    );
    const delta = width - sidebarBox.width;

    await workspace.mouse.move(startX, startY);
    await workspace.mouse.down();
    await workspace.mouse.move(startX + delta, startY, { steps: 8 });
    await workspace.mouse.up();

    // The sash stays blue while the pointer is left on it. Move the synthetic
    // mouse back into the sidebar, just as a human would before taking a
    // screenshot, and wait for VS Code to clear its hover/active state.
    await workspace.mouse.move(
        sidebarBox.x + 20,
        sidebarBox.y + sidebarBox.height / 2,
    );
    await workspace.waitForFunction(
        () => !document.querySelector('.monaco-sash.hover, .monaco-sash.active'),
        null,
        { timeout: 2_000 },
    );

    await workspace.waitForFunction(
        expected => {
            const sidebar = document.querySelector('.part.sidebar');
            if (!sidebar) return false;
            return Math.abs(sidebar.getBoundingClientRect().width - expected) <= 2;
        },
        width,
        { timeout: 5_000 },
    );

    await workspace.waitForTimeout(150);
}

async function closeFolder() {
    const noFolder = workspace.getByText('NO FOLDER OPENED', { exact: true }).first();
    if ((await noFolder.count()) > 0) return;

    await workspace.keyboard.press('Control+K');
    await workspace.keyboard.press('KeyF');
    await workspace.waitForTimeout(250);
}

async function resolveCachedCommit(repository, commit) {
    try {
        const result = await execFileAsync(
            'git',
            ['--git-dir', repository, 'rev-parse', '--verify', `${commit}^{commit}`],
            { maxBuffer: 20 * 1024 * 1024 },
        );
        const fullCommit = result.stdout.trim().toLowerCase();
        return /^[0-9a-f]{40}$/.test(fullCommit) ? fullCommit : null;
    } catch {
        return null;
    }
}

async function preparePinnedCloneSource(url, commit) {
    await fs.mkdir(GIT_CACHE_ROOT, { recursive: true });

    const repositoryId = sha256(url).slice(0, 24);
    const cachedRepository = path.join(GIT_CACHE_ROOT, `${repositoryId}.git`);
    let cacheExists = false;

    try {
        const stat = await fs.stat(cachedRepository);
        if (!stat.isDirectory()) {
            throw new Error(`Git cache path is not a directory: ${cachedRepository}`);
        }
        cacheExists = true;
    } catch (error) {
        if (error?.code !== 'ENOENT') throw error;
    }

    if (!cacheExists) {
        const temporary = `${cachedRepository}.tmp-${process.pid}-${crypto.randomBytes(6).toString('hex')}`;
        console.log(`Tutorial screenshot Git cache: cloning ${url}`);
        try {
            await execFileAsync(
                'git',
                ['clone', '--quiet', '--mirror', url, temporary],
                { maxBuffer: 20 * 1024 * 1024 },
            );
            await fs.rename(temporary, cachedRepository);
            cacheExists = true;
        } catch (error) {
            await fs.rm(temporary, { recursive: true, force: true }).catch(() => {});
            const detail = `${error.stderr || error.message}`.trim();
            throw new Error(`Could not populate Git cache for ${url}: ${detail}`);
        }
    }

    let fullCommit = await resolveCachedCommit(cachedRepository, commit);
    if (fullCommit) {
        console.log(
            `Tutorial screenshot Git cache hit: ${url} @ ${fullCommit.slice(0, 12)}`,
        );
    } else {
        console.log(
            `Tutorial screenshot Git cache miss for ${commit.slice(0, 12)}; refreshing ${url}`,
        );
        try {
            await execFileAsync(
                'git',
                ['--git-dir', cachedRepository, 'fetch', '--quiet', '--prune', 'origin'],
                { maxBuffer: 20 * 1024 * 1024 },
            );
        } catch (error) {
            const detail = `${error.stderr || error.message}`.trim();
            throw new Error(`Could not refresh Git cache for ${url}: ${detail}`);
        }

        fullCommit = await resolveCachedCommit(cachedRepository, commit);
        if (!fullCommit) {
            throw new Error(
                `Pinned commit ${commit} was not found in ${url} after refreshing the cache`,
            );
        }
    }

    let branch = 'tutorial-screenshot';
    try {
        const result = await execFileAsync(
            'git',
            ['--git-dir', cachedRepository, 'symbolic-ref', '--quiet', '--short', 'HEAD'],
        );
        if (result.stdout.trim()) branch = result.stdout.trim();
    } catch {
        // Repositories without a symbolic HEAD use the deterministic fallback.
    }

    const workspaceRoot = screenshotWorkspaceRoot();
    const workspaceCacheRoot = path.join(workspaceRoot, '.tutorial-screenshot-git');
    const sourceId = sha256(`${url}\0${fullCommit}`).slice(0, 16);
    const target = path.join(workspaceCacheRoot, `${sourceId}.git`);
    const workspaceTarget = `/workspace/.tutorial-screenshot-git/${sourceId}.git`;
    const localUrl = `file://${workspaceTarget}`;
    const gitConfig = path.join(workspaceRoot, '.gitconfig');

    await fs.mkdir(workspaceCacheRoot, { recursive: true });
    await fs.rm(target, { recursive: true, force: true });

    try {
        await execFileAsync('git', ['init', '--quiet', '--bare', target]);
        await execFileAsync(
            'git',
            [
                '--git-dir', target,
                'fetch', '--quiet', cachedRepository,
                `${fullCommit}:refs/heads/${branch}`,
            ],
            { maxBuffer: 20 * 1024 * 1024 },
        );
        await execFileAsync('git', ['--git-dir', target, 'symbolic-ref', 'HEAD', `refs/heads/${branch}`]);
        await execFileAsync('git', ['config', '--file', gitConfig, 'protocol.file.allow', 'always']);
        await execFileAsync('git', ['config', '--file', gitConfig, `url.${localUrl}.insteadOf`, url]);
        await execFileAsync('chown', ['-R', '1000:1000', workspaceCacheRoot]);
        await fs.chown(gitConfig, 1000, 1000).catch(() => {});
    } catch (error) {
        const detail = `${error.stderr || error.message}`.trim();
        throw new Error(
            `Could not prepare pinned clone ${url}@${fullCommit.slice(0, 12)}: ${detail}`,
        );
    }

    console.log(
        `Tutorial screenshot Git clone uses cached ${url} @ ${fullCommit.slice(0, 12)}`,
    );
}

async function cloneStart(action) {
    const { url, commit } = action;
    if (commit) {
        await preparePinnedCloneSource(url, commit);
    }

    const cloneButton = workspace.getByText('Clone Repository', { exact: true }).first();
    if (await locatorIsVisible(cloneButton)) {
        await cloneButton.click();
    } else {
        await workspace.keyboard.press('Control+Shift+P');
        const input = quickInput(workspace);
        await input.waitFor({ state: 'visible', timeout: 10_000 });
        await input.fill('Git: Clone');
        await input.press('Enter');
    }
    const input = quickInput(workspace);
    await input.waitFor({ state: 'visible', timeout: 10_000 });

    // Match the real interaction: click into the field and type the URL. `fill()`
    // changes the value in one synthetic step and code-server can leave the
    // suggestion list itself focused, which produces an extra focus frame.
    await input.click();
    await input.press('Control+A');
    await input.pressSequentially(url);

    // Git providers can update the result list asynchronously after the final
    // keystroke. Wait until Quick Input is no longer busy, then restore focus
    // to the text field. This matches the settled state seen when the same URL
    // is entered by hand instead of capturing the transient list-focus state.
    await workspace.waitForFunction(
        () => {
            const progress = document.querySelector(
                '.quick-input-progress.monaco-progress-container',
            );
            return !progress || !progress.classList.contains('active');
        },
        null,
        { timeout: 5_000 },
    ).catch(() => {});
    await input.click();
    await workspace.waitForTimeout(100);
}

async function waitForEditorSettled(page) {
    await page.waitForFunction(
        () => [...document.querySelectorAll('.monaco-editor .view-lines')]
            .some(element => {
                const rect = element.getBoundingClientRect();
                return rect.width > 0 &&
                    rect.height > 0 &&
                    element.querySelector('.view-line');
            }),
        null,
        { timeout: 10_000 },
    );

    await page.evaluate(() => new Promise((resolve, reject) => {
        const candidates = [...document.querySelectorAll(
            '.monaco-editor .view-lines'
        )];

        const viewLines = candidates.find(element => {
            const rect = element.getBoundingClientRect();
            return rect.width > 0 &&
                rect.height > 0 &&
                element.querySelector('.view-line');
        });

        if (!viewLines) {
            reject(new Error('No visible Monaco editor found'));
            return;
        }

        let settleTimer;
        const timeoutTimer = setTimeout(() => {
            observer.disconnect();
            reject(new Error('Editor did not settle'));
        }, 10_000);

        const settled = () => {
            clearTimeout(timeoutTimer);
            observer.disconnect();
            resolve();
        };

        const restartSettleTimer = () => {
            clearTimeout(settleTimer);
            settleTimer = setTimeout(settled, 400);
        };

        const observer = new MutationObserver(restartSettleTimer);
        observer.observe(viewLines, {
            childList: true,
            subtree: true,
            attributes: true,
            characterData: true,
        });

        restartSettleTimer();
    }));
}

async function openFile(relativePath) {
    await workspace.keyboard.press('Control+P');

    const input = quickInput(workspace);
    await input.waitFor({ state: 'visible', timeout: 10_000 });
    await input.fill(relativePath);

    const basename = path.posix.basename(relativePath);
    const parent = path.posix.basename(path.posix.dirname(relativePath));

    await workspace.waitForFunction(
        ({ basename, parent }) =>
            [...document.querySelectorAll(
                '.quick-input-list .monaco-list-row'
            )].some(row => {
                const text = row.innerText || '';
                return text.includes(basename) &&
                    (parent === '.' || text.includes(parent));
            }),
        { basename, parent },
        { timeout: 10_000 },
    );

    await input.press('Enter');
    await input.waitFor({ state: 'hidden', timeout: 10_000 });

    await workspace.waitForFunction(
        name =>
            [...document.querySelectorAll(
                '.tab .label-name, .tabs-container .label-name'
            )].some(node => node.textContent?.trim() === name),
        basename,
        { timeout: 20_000 },
    );

    await waitForEditorSettled(workspace);
}

async function goLive(hooks) {
    const goLive = workspace.getByText('Go Live', { exact: true }).last();
    await goLive.waitFor({ state: 'visible', timeout: 30_000 });
    const popupPromise = context.waitForEvent('page', { timeout: 60_000 });
    await goLive.click();
    preview = await popupPromise;
    await preview.waitForLoadState('domcontentloaded');
    await waitForPreviewReady(preview, hooks);
}

async function previewReload(reset = false, hooks = null) {
    if (!preview || preview.isClosed()) throw new Error('No preview tab is open');
    let previewSession = null;
    let injectedResetScript = null;
    if (reset) {
        previewSession = await context.newCDPSession(preview);
        const installed = await previewSession.send('Page.addScriptToEvaluateOnNewDocument', {
            source: `
                try { sessionStorage.clear(); } catch (error) {}
                try { localStorage.clear(); } catch (error) {}
            `,
        });
        injectedResetScript = installed.identifier;
    }

    try {
        await preview.reload({
            waitUntil: 'domcontentloaded',
            timeout: 60_000,
        });
        await waitForPreviewReady(preview, hooks);
    } finally {
        if (previewSession && injectedResetScript) {
            await previewSession.send('Page.removeScriptToEvaluateOnNewDocument', {
                identifier: injectedResetScript,
            }).catch(() => {});
        }
        if (previewSession) {
            await previewSession.detach().catch(() => {});
        }
    }
}

async function writeWorkspaceFile(relativePath, contents, hooks) {
    const root = path.join(
        USER_ROOT,
        fsTagForEmail(SCREENSHOT_EMAIL),
        'workspace',
        hooks?.writeFileSubdirectory || '',
    );
    const target = path.join(root, safeRelativePath(relativePath));
    const resolvedRoot = path.resolve(root);
    const resolvedTarget = path.resolve(target);
    if (!resolvedTarget.startsWith(`${resolvedRoot}${path.sep}`)) {
        throw new Error(`Workspace path escaped configured write-file root: ${relativePath}`);
    }
    await fs.mkdir(path.dirname(target), { recursive: true });
    if (contents && typeof contents === 'object' && contents.sourcePath) {
        const bytes = await fs.readFile(contents.sourcePath);
        const sourceSha256 = sha256(bytes);
        if (sourceSha256 !== contents.sourceSha256) {
            throw new Error(`write-file source changed while generating screenshots: ${contents.sourcePath}`);
        }
        await fs.writeFile(target, bytes);
    } else {
        const text = contents.endsWith('\n') ? contents : `${contents}\n`;
        await fs.writeFile(target, text, 'utf8');
    }
    await fs.chown(target, 1000, 1000).catch(() => {});
}

function screenshotWorkspaceRoot() {
    return path.join(
        USER_ROOT,
        fsTagForEmail(SCREENSHOT_EMAIL),
        'workspace',
    );
}

async function waitForWorkspaceFile(relativePath) {
    const root = path.resolve(screenshotWorkspaceRoot());
    const target = path.resolve(root, relativePath);

    if (
        target !== root &&
        !target.startsWith(`${root}${path.sep}`)
    ) {
        throw new Error(`Workspace path escaped its root: ${relativePath}`);
    }

    const deadline = Date.now() + 60_000;

    while (Date.now() < deadline) {
        try {
            const stat = await fs.stat(target);

            if (stat.isFile()) {
                return;
            }

            throw new Error(
                `wait-for-file path exists but is not a file: ${relativePath}`,
            );
        } catch (error) {
            if (error?.code !== 'ENOENT') throw error;
        }

        await new Promise(resolve => setTimeout(resolve, 100));
    }

    throw new Error(
        `Timed out waiting for Workspace file: ${relativePath}`,
    );
}

async function waitForWorkspaceFileNewer(targetRelativePath, sourceRelativePath) {
    const root = path.resolve(
        USER_ROOT,
        fsTagForEmail(SCREENSHOT_EMAIL),
        'workspace',
    );

    const resolveWorkspacePath = relativePath => {
        const target = path.resolve(root, safeRelativePath(relativePath));

        if (target !== root && !target.startsWith(`${root}${path.sep}`)) {
            throw new Error(`Workspace path escaped its root: ${relativePath}`);
        }

        return target;
    };

    const sourcePath = resolveWorkspacePath(sourceRelativePath);
    const targetPath = resolveWorkspacePath(targetRelativePath);

    const sourceStat = await fs.stat(sourcePath, { bigint: true });

    if (!sourceStat.isFile()) {
        throw new Error(`Source is not a file: ${sourceRelativePath}`);
    }

    const sourceMtime = sourceStat.mtimeNs;
    const deadline = Date.now() + 60_000;

    let stableSignature = null;
    let stableSince = 0;

    while (Date.now() < deadline) {
        try {
            const stat = await fs.stat(targetPath, { bigint: true });

            if (stat.isFile() && stat.mtimeNs > sourceMtime) {
                const signature = `${stat.mtimeNs}:${stat.size}`;

                if (signature !== stableSignature) {
                    stableSignature = signature;
                    stableSince = Date.now();
                } else if (Date.now() - stableSince >= 250) {
                    return;
                }
            } else {
                stableSignature = null;
            }
        } catch (error) {
            if (error?.code !== 'ENOENT') throw error;
            stableSignature = null;
        }

        await new Promise(resolve => setTimeout(resolve, 100));
    }

    throw new Error(
        `Timed out waiting for ${targetRelativePath} to become newer than ${sourceRelativePath}`,
    );
}

async function closeTab(label) {
    const tabs = workspace.locator('.tabs-container .tab');
    const count = await tabs.count();
    let target = null;

    for (let index = 0; index < count; index += 1) {
        const tab = tabs.nth(index);
        const name = await tab.locator('.label-name')
            .textContent()
            .catch(() => '');

        if (name?.trim() === label) {
            target = tab;
            break;
        }
    }

    // Already closed is the desired state.
    if (!target) return;

    await target.hover();

    const closeButton = target.locator('.codicon-close').first();

    if (await closeButton.count()) {
        await closeButton.click();
    } else {
        // Fallback for a tab whose close control is not exposed until active.
        await target.click();
        await workspace.keyboard.press('Control+W');
    }

    await workspace.waitForFunction(
        expected =>
            ![...document.querySelectorAll('.tabs-container .tab .label-name')]
                .some(node => node.textContent?.trim() === expected),
        label,
        { timeout: 10_000 },
    );

    await workspace.waitForTimeout(100);
}

function pageForTab(tab) {
    return tab === 'preview' ? preview : workspace;
}

function requirePageForTab(tab, actionName) {
    const page = pageForTab(tab);
    if (!page || page.isClosed()) {
        throw new Error(`${actionName} requests unavailable tab: ${tab}`);
    }
    return page;
}

async function pageControl(action, verb, targetTab) {
    const page = requirePageForTab(targetTab, verb);

    let control;
    let description;
    if (action.selector) {
        control = page.locator(action.selector);
        description = `element matching selector ${JSON.stringify(action.selector)}`;
    } else {
        const buttons = page.getByRole('button', { name: action.text, exact: true });
        const links = page.getByRole('link', { name: action.text, exact: true });
        control = buttons.or(links);
        if (await control.count() === 0) {
            control = page.getByText(action.text, { exact: true });
        }
        description = `control named ${JSON.stringify(action.text)}`;
    }

    await control.first().waitFor({ state: 'visible', timeout: 20_000 });
    const count = await control.count();
    if (count !== 1) {
        throw new Error(`${verb} expected exactly one ${description}, found ${count}`);
    }
    return { page, control };
}

async function clickControl(action, targetTab) {
    const { page, control } = await pageControl(action, 'click', targetTab);
    await control.click();
    await page.waitForTimeout(150);
}

async function holdControl(action, targetTab) {
    const { page, control } = await pageControl(action, 'hold', targetTab);
    await control.hover();
    await page.mouse.down();
    try {
        await page.waitForTimeout(action.seconds * 1000);
    } finally {
        await page.mouse.up();
    }
    await page.waitForTimeout(150);
}

async function typeText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'type');
    await page.waitForFunction(
        () => {
            const element = document.activeElement;
            return element instanceof HTMLInputElement ||
                element instanceof HTMLTextAreaElement ||
                element?.isContentEditable === true;
        },
        null,
        { timeout: 10_000 },
    );
    await page.keyboard.insertText(text);
    await page.waitForTimeout(100);
}

async function pressKey(key, targetTab) {
    const page = requirePageForTab(targetTab, 'press');
    await page.keyboard.press(key);
    await page.waitForTimeout(100);
}

async function moveMouse(action, targetTab) {
    const page = requirePageForTab(targetTab, 'move-mouse');

    let target;
    let description;
    if (action.selector) {
        target = page.locator(action.selector);
        description = `element matching selector ${JSON.stringify(action.selector)}`;
    } else {
        target = page.getByText(action.text, { exact: true });
        description = `element with text ${JSON.stringify(action.text)}`;
    }

    await target.first().waitFor({ state: 'visible', timeout: 20_000 });
    const count = await target.count();
    if (count !== 1) {
        throw new Error(`move-mouse expected exactly one ${description}, found ${count}`);
    }

    const box = await target.boundingBox();
    if (!box) throw new Error(`move-mouse could not measure ${description}`);
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    await page.waitForTimeout(150);
}

async function waitForText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'wait-for-text');

    await page.waitForFunction(
        expected => {
            const normalize = value => `${value || ''}`.replace(/\s+/g, ' ').trim();
            return normalize(document.body?.innerText).includes(normalize(expected));
        },
        text,
        { timeout: 60_000 },
    );
}

async function waitForInputValue(text, targetTab) {
    const page = requirePageForTab(targetTab, 'wait-for-input-value');

    await page.waitForFunction(
        expected => [...document.querySelectorAll('input, textarea')]
            .some(element => {
                const rect = element.getBoundingClientRect();
                return rect.width > 0 &&
                    rect.height > 0 &&
                    `${element.value || ''}`.includes(expected);
            }),
        text,
        { timeout: 60_000 },
    );
}

function filenameSafeFragment(value) {
    const normalized = `${value || ''}`
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, '-')
        .replace(/^-+|-+$/g, '');
    return normalized || 'action';
}

async function captureErrorTabs({
    tutorialDirectory,
    shot,
}) {
    const errorDirectory = path.join(
        tutorialDirectory,
        '.tutorial-screenshot-errors',
    );

    await fs.rm(errorDirectory, { recursive: true, force: true }).catch(() => {});
    await fs.mkdir(errorDirectory, { recursive: true });

    const tabName = shot?.tab || 'workspace';
    const page = pageForTab(tabName);
    if (!page || page.isClosed()) return null;

    const pngPath = `/tmp/tutorial-error-${process.pid}-${crypto.randomBytes(6).toString('hex')}.png`;
    const target = path.join(errorDirectory, 'last-failed.webp');
    const temporaryWebp = `${target}.tmp-${process.pid}-${Date.now()}`;

    try {
        const png = await page.screenshot({
            type: 'png',
            fullPage: false,
        });
        await fs.writeFile(pngPath, png);
        await execFileAsync('cwebp', [
            '-quiet',
            '-lossless',
            pngPath,
            '-o',
            temporaryWebp,
        ]);
        await fs.chmod(temporaryWebp, 0o644);
        await fs.rename(temporaryWebp, target);
        await inheritDirectoryOwnership(target, tutorialDirectory);
    } finally {
        await fs.rm(pngPath, { force: true }).catch(() => {});
        await fs.rm(temporaryWebp, { force: true }).catch(() => {});
    }

    const revision = sha256(await fs.readFile(target));
    const relativeTarget = path.posix.join(
        '.tutorial-screenshot-errors',
        'last-failed.webp',
    );
    console.error(
        `Tutorial screenshot failure state captured: ${path.relative(CONTENT_ROOT, target)}`,
    );
    return {
        target: relativeTarget,
        label: `${shot?.target || 'Screenshot'} · Fehler`,
        stale: false,
        missing: false,
        state: 'error',
        revision,
        error_preview: true,
    };
}

async function applyShotViewportProfile(shot) {
    const page = pageForTab(shot.tab);
    if (!page || page.isClosed()) return false;

    await applyViewportProfile(page, shot.viewport, shot.zoom);
    return true;
}

async function executeAction(action, targetTab, hooks) {
    switch (action.type) {
        case 'close-folder': return closeFolder();
        case 'show-left-sidebar': return setWorkbenchPartVisible('left-sidebar', true);
        case 'hide-left-sidebar': return setWorkbenchPartVisible('left-sidebar', false);
        case 'show-right-sidebar': return setWorkbenchPartVisible('right-sidebar', true);
        case 'hide-right-sidebar': return setWorkbenchPartVisible('right-sidebar', false);
        case 'show-bottom-panel': return setWorkbenchPartVisible('bottom-panel', true);
        case 'hide-bottom-panel': return setWorkbenchPartVisible('bottom-panel', false);
        case 'left-sidebar-width': return setLeftSidebarWidth(action.width);
        case 'clone-start': return cloneStart(action);
        case 'open-file': return openFile(action.path);
        case 'terminal-open': return terminalOpen();
        case 'terminal-wait-for-prompt': return waitForTerminalPrompt();
        case 'terminal-maximize': return terminalMaximize();
        case 'terminal-run': return terminalRun(action.text);
        case 'go-live': return goLive(hooks);
        case 'write-file': return writeWorkspaceFile(action.path, action.contents, hooks);
        case 'wait-for-file': return waitForWorkspaceFile(action.path);
        case 'wait-for-file-newer': return waitForWorkspaceFileNewer(action.target, action.source);
        case 'close-tab': return closeTab(action.label);
        case 'preview-reload': return previewReload(false, hooks);
        case 'preview-reset': return previewReload(true, hooks);
        case 'click': return clickControl(action, targetTab);
        case 'hold': return holdControl(action, targetTab);
        case 'move-mouse': return moveMouse(action, targetTab);
        case 'type': return typeText(action.text, targetTab);
        case 'press': return pressKey(action.key, targetTab);
        case 'sleep': return sleep(action.seconds);
        case 'wait-for-text': return waitForText(action.text, targetTab);
        case 'wait-for-input-value': return waitForInputValue(action.text, targetTab);
    default: throw new Error(`Unsupported action: ${action.type}`);
    }
}

async function applyViewportProfile(page, viewport, zoom) {
    let session = deviceMetricSessions.get(page);
    if (!session) {
        session = await context.newCDPSession(page);
        deviceMetricSessions.set(page, session);
    }

    // Match the measured manual setup: Linux desktop scale and Chrome page zoom
    // are separate factors. Their product is the number of output pixels per CSS
    // pixel in the final screenshot.
    const metrics = screenshotMetricsFor(viewport, zoom);

    const profileKey = [
        metrics.width,
        metrics.height,
        metrics.deviceScaleFactor,
    ].join(':');
    if (appliedViewportProfiles.get(page) === profileKey) {
        return { session, metrics };
    }

    await session.send('Emulation.setDeviceMetricsOverride', {
        width: metrics.width,
        height: metrics.height,
        deviceScaleFactor: metrics.deviceScaleFactor,
        mobile: false,
        screenWidth: metrics.width,
        screenHeight: metrics.height,
    });

    // Give VS Code two frames to react to the changed layout metrics.
    await page.evaluate(() => new Promise(resolve => {
        requestAnimationFrame(() => requestAnimationFrame(resolve));
    }));

    appliedViewportProfiles.set(page, profileKey);
    return { session, metrics };
}

async function installWorkspaceScreenshotStyle(page) {
    await page.evaluate(css => {
        const id = 'hackschule-tutorial-screenshot-style';
        let style = document.getElementById(id);
        if (!style) {
            style = document.createElement('style');
            style.id = id;
            document.documentElement.appendChild(style);
        }
        style.textContent = css;
        return new Promise(resolve => {
            requestAnimationFrame(() => requestAnimationFrame(resolve));
        });
    }, WORKSPACE_SCREENSHOT_STYLE);
}

function pngDimensions(buffer) {
    if (buffer.length < 24 ||
        buffer.readUInt32BE(0) !== 0x89504e47 ||
        buffer.readUInt32BE(4) !== 0x0d0a1a0a) {
        throw new Error('Chromium returned an invalid PNG screenshot');
    }
    return {
        width: buffer.readUInt32BE(16),
        height: buffer.readUInt32BE(20),
    };
}

async function visibleTerminalCropGeometry(page) {
    return page.evaluate(() => {
        const terminals = [...document.querySelectorAll('.terminal.xterm')]
            .filter(terminal => {
                const rect = terminal.getBoundingClientRect();
                return rect.width > 0 && rect.height > 0;
            });
        const terminal = terminals.at(-1);
        if (!terminal) {
            throw new Error(
                'crop-terminal-lines requires a visible integrated terminal',
            );
        }

        const panel = terminal.closest('.part.panel');
        const screen = terminal.querySelector('.xterm-screen');
        if (!panel || !screen) {
            throw new Error(
                'crop-terminal-lines could not measure the integrated terminal',
            );
        }

        const panelRect = panel.getBoundingClientRect();
        const screenRect = screen.getBoundingClientRect();
        if (
            panelRect.width <= 0 || panelRect.height <= 0 ||
            screenRect.width <= 0 || screenRect.height <= 0
        ) {
            throw new Error(
                'crop-terminal-lines could not measure the visible terminal geometry',
            );
        }

        const rows = [...terminal.querySelectorAll('.xterm-rows > div')]
            .map(row => {
                const rect = row.getBoundingClientRect();
                return {
                    top: rect.top,
                    bottom: rect.bottom,
                    height: rect.height,
                    empty: !row.textContent.trim(),
                };
            })
            .filter(row =>
                row.height > 0 &&
                row.bottom > screenRect.top &&
                row.top < screenRect.bottom
            )
            .sort((left, right) => left.top - right.top);

        if (rows.length === 0) {
            throw new Error('crop-terminal-lines found no rendered terminal rows');
        }

        return {
            panelTop: panelRect.top,
            screenBottom: screenRect.bottom,
            rows,
        };
    });
}

async function captureShot(shot, tutorialDirectory) {
    const page = pageForTab(shot.tab);
    if (!page || page.isClosed()) throw new Error(`Screenshot requests unavailable tab: ${shot.tab}`);

    const { session, metrics } = await applyViewportProfile(page, shot.viewport, shot.zoom);
    await page.evaluate(() => window.scrollTo(0, 0)).catch(() => {});
    await page.waitForTimeout(100);

    // Recipes express viewport and crop values in final screenshot pixels. The
    // screenshot clip itself is in CSS pixels, so divide by the emulated zoom.
    // Fractional clip dimensions are intentional: at 1853x929 / 150% they make
    // Playwright emit exactly 1853x929 device pixels instead of 1854x930.
    let topPixels = Math.round(shot.viewport.height * shot.cropTop);
    let bottomPixels = Math.round(shot.viewport.height * shot.cropBottom);
    let heightPixels = shot.viewport.height - topPixels - bottomPixels;

    if (shot.cropTerminalLines != null) {
        const geometry = await visibleTerminalCropGeometry(page);
        const crop = terminalLineCropPixels({
            viewportHeight: shot.viewport.height,
            deviceScaleFactor: metrics.deviceScaleFactor,
            ...geometry,
            lineCount: shot.cropTerminalLines,
        });
        ({ topPixels, bottomPixels, heightPixels } = crop);
    }
    const pngPath = `/tmp/tutorial-shot-${process.pid}-${crypto.randomBytes(6).toString('hex')}.png`;
    const target = path.join(tutorialDirectory, shot.target);
    const temporaryWebp = `${target}.tmp-${process.pid}-${Date.now()}`;

    await fs.mkdir(path.dirname(target), { recursive: true });
    try {
        if (shot.tab === 'workspace') {
            await installWorkspaceScreenshotStyle(page);
        }

        // Playwright's screenshot scale is tied to the BrowserContext's device
        // scale factor, so a later CDP device-metrics override did not enlarge
        // the bitmap. Capture through CDP directly instead: the page is laid out
        // at viewport/zoom CSS pixels, while clip.scale restores the configured
        // final pixel dimensions.
        const captured = await session.send('Page.captureScreenshot', {
            format: 'png',
            fromSurface: true,
            captureBeyondViewport: false,
            clip: {
                x: 0,
                y: topPixels / metrics.deviceScaleFactor,
                width: shot.viewport.width / metrics.deviceScaleFactor,
                height: heightPixels / metrics.deviceScaleFactor,
                scale: 1,
            },
        });
        const png = Buffer.from(captured.data, 'base64');
        const actual = pngDimensions(png);
        await fs.writeFile(pngPath, png);

        const cwebpArgs = ['-quiet', '-lossless'];
        if (actual.width !== shot.viewport.width || actual.height !== heightPixels) {
            console.log(
                `Chromium screenshot was ${actual.width}x${actual.height}; ` +
                `expected ${shot.viewport.width}x${heightPixels}; keeping native pixels without resampling`,
            );
        }
        cwebpArgs.push(pngPath, '-o', temporaryWebp);
        await execFileAsync('cwebp', cwebpArgs);
        await fs.chmod(temporaryWebp, 0o644);
        await fs.rename(temporaryWebp, target);
        await inheritDirectoryOwnership(target, tutorialDirectory);
    } finally {
        await fs.rm(pngPath, { force: true }).catch(() => {});
        await fs.rm(temporaryWebp, { force: true }).catch(() => {});
    }
    return sha256(await fs.readFile(target));
}

function screenshotOutputDimensions(shot) {
    const topPixels = Math.round(shot.viewport.height * shot.cropTop);
    const bottomPixels = Math.round(shot.viewport.height * shot.cropBottom);
    return {
        width: shot.viewport.width,
        height: shot.viewport.height - topPixels - bottomPixels,
    };
}

function generationStatusItem(shot, stale, missing, previous) {
    const dimensions = screenshotOutputDimensions(shot);
    return {
        target: shot.target,
        width: dimensions.width,
        height: dimensions.height,
        stale,
        missing,
        state: stale ? (missing ? 'missing' : 'stale') : 'current',
        revision: previous?.output_sha256 || null,
    };
}

async function buildGenerationPlan(payload) {
    const force = payload?.force === true;
    const relativeMarkdown = safeRelativePath(payload?.markdown_path);
    if (!relativeMarkdown.endsWith('.md')) throw new Error('markdown_path must name a Markdown file');
    const markdownPath = path.join(CONTENT_ROOT, relativeMarkdown);
    const resolvedContentRoot = path.resolve(CONTENT_ROOT);
    const resolvedMarkdown = path.resolve(markdownPath);
    if (!resolvedMarkdown.startsWith(`${resolvedContentRoot}${path.sep}`)) {
        throw new Error('markdown_path escaped /content');
    }

    const markdown = await fs.readFile(markdownPath, 'utf8');
    const shots = await parseTutorial(markdown, relativeMarkdown);
    const tutorialDirectory = path.dirname(markdownPath);

    if (shots.length === 0) {
        return {
            force,
            relativeMarkdown,
            markdownPath,
            tutorialDirectory,
            shots,
            stale: [],
            screenshots: [],
            oldManifest: { version: 1, screenshots: {} },
            nextManifest: { version: 1, screenshots: {} },
            tutorialHooks: null,
            environmentFingerprint: null,
            fingerprint: sha256(`${relativeMarkdown}\0empty`),
        };
    }

    const tutorialHooks = await loadTutorialScreenshotHooks(tutorialDirectory);
    const oldManifest = await readManifest(tutorialDirectory);
    const environmentFingerprint = sha256(JSON.stringify({
        generator_version: GENERATOR_VERSION,
        playwright: PLAYWRIGHT_VERSION,
        workspace_image_id: `${payload?.workspace_image_id || 'unknown'}`,
        profile: DEFAULT_PROFILE,
        tutorial_hooks_sha256: tutorialHooks.sourceSha256,
        screenshot_identity: {
            email: SCREENSHOT_EMAIL,
            workspace_user: SCREENSHOT_WORKSPACE_USER,
        },
    }));

    const nextManifest = { version: 1, screenshots: {} };
    const stale = [];
    const screenshots = [];

    for (const shot of shots) {
        const target = path.join(tutorialDirectory, shot.target);
        const previous = oldManifest.screenshots?.[shot.target];
        let exists = true;
        try { await fs.access(target); } catch { exists = false; }
        const needsGeneration = force || !exists || previous?.state_hash !== shot.stateHash ||
            previous?.environment_fingerprint !== environmentFingerprint;
        stale.push(needsGeneration);
        screenshots.push(generationStatusItem(shot, needsGeneration, !exists, previous));
        if (!needsGeneration && previous) nextManifest.screenshots[shot.target] = previous;
    }

    const fingerprint = sha256(JSON.stringify({
        relativeMarkdown,
        force,
        environmentFingerprint,
        shots: shots.map(shot => ({
            target: shot.target,
            stateHash: shot.stateHash,
        })),
    }));

    return {
        force,
        relativeMarkdown,
        markdownPath,
        tutorialDirectory,
        shots,
        stale,
        screenshots,
        oldManifest,
        nextManifest,
        tutorialHooks,
        environmentFingerprint,
        fingerprint,
    };
}

async function finalizeCurrentPlan(plan) {
    if (plan.shots.length > 0 &&
        JSON.stringify(plan.oldManifest) !== JSON.stringify(plan.nextManifest)) {
        await writeManifest(plan.tutorialDirectory, plan.nextManifest);
    }
}

function currentGenerationSnapshot(plan) {
    return {
        status: 'current',
        monitor: false,
        markdown_path: plan.relativeMarkdown,
        fingerprint: plan.fingerprint,
        total: plan.shots.length,
        completed: plan.shots.length,
        stale: 0,
        generated: 0,
        current_target: null,
        error: null,
        screenshots: plan.screenshots,
    };
}

function activeGenerationJob(job) {
    return job && ['queued', 'preparing', 'running'].includes(job.status);
}

async function enqueueGeneration(payload) {
    const plan = await buildGenerationPlan(payload);
    const staleCount = plan.stale.filter(Boolean).length;

    const existing = latestGenerationJobs.get(plan.relativeMarkdown);
    if (activeGenerationJob(existing) && existing.fingerprint === plan.fingerprint) {
        return existing;
    }

    // A different job for the same tutorial may still be about to overwrite the
    // currently-valid files. Queue a reconciliation for the requested source
    // state instead of declaring it current prematurely.
    if (staleCount === 0 && !activeGenerationJob(existing)) {
        await finalizeCurrentPlan(plan);
        const snapshot = currentGenerationSnapshot(plan);
        latestGenerationJobs.set(plan.relativeMarkdown, snapshot);
        return snapshot;
    }

    const job = {
        status: 'queued',
        monitor: true,
        markdown_path: plan.relativeMarkdown,
        fingerprint: plan.fingerprint,
        total: plan.shots.length,
        completed: 0,
        stale: staleCount,
        generated: 0,
        current_target: null,
        error: null,
        screenshots: plan.screenshots.map(item => ({ ...item })),
    };
    latestGenerationJobs.set(plan.relativeMarkdown, job);

    const run = generationQueue.then(async () => {
        try {
            const result = await generateTutorial(payload, job);
            job.status = 'done';
            job.current_target = null;
            job.finished_at = Date.now();
            return result;
        } catch (error) {
            job.status = 'error';
            job.current_target = null;
            job.error = error.message;
            job.finished_at = Date.now();
            throw error;
        }
    });
    generationQueue = run.catch(error => {
        console.error(error);
    });

    return job;
}

function recordGenerationRequestError(payload, error) {
    try {
        const relativeMarkdown = safeRelativePath(payload?.markdown_path);
        if (!relativeMarkdown.endsWith('.md')) return;
        latestGenerationJobs.set(relativeMarkdown, {
            status: 'error',
            monitor: true,
            markdown_path: relativeMarkdown,
            total: 0,
            completed: 0,
            stale: 0,
            generated: 0,
            current_target: null,
            error: error.message,
            screenshots: [],
        });
    } catch {
        // The original request error is more useful than a secondary path error.
    }
}

async function generateTutorial(payload, progressJob = null) {
    // Rebuild the fast plan when the queued job actually starts. Another job for
    // the same tutorial may have changed its files/manifest while this one waited.
    const plan = await buildGenerationPlan(payload);
    const {
        relativeMarkdown,
        shots,
        tutorialDirectory,
        tutorialHooks,
        oldManifest,
        nextManifest,
        environmentFingerprint,
        stale,
    } = plan;

    if (progressJob) {
        progressJob.total = shots.length;
        progressJob.completed = 0;
        progressJob.stale = stale.filter(Boolean).length;
        progressJob.generated = 0;
        progressJob.current_target = null;
        progressJob.screenshots = plan.screenshots.map(item => ({ ...item }));
    }

    if (shots.length === 0) return { generated: 0, current: 0, screenshots: 0 };

    // Removed recipe entries disappear from the manifest. The image itself stays
    // in the tutorial directory and therefore instantly becomes a normal manual
    // tutorial image again.
    if (!stale.some(Boolean)) {
        await finalizeCurrentPlan(plan);
        if (progressJob) progressJob.completed = shots.length;
        return { generated: 0, current: shots.length, screenshots: shots.length };
    }

    if (progressJob) progressJob.status = 'preparing';
    await acquireFreshWorkspace();
    if (progressJob) progressJob.status = 'running';

    let generated = 0;
    for (let index = 0; index < shots.length; index += 1) {
        const shot = shots[index];

        console.log(`Tutorial screenshot ${relativeMarkdown} -> ${shot.target}`);
        if (progressJob) {
            progressJob.current_target = shot.target;
            progressJob.screenshots[index].state = stale[index] ? 'rendering' : 'replaying';
        }

        // Layout-sensitive actions (for example clicking a graph control) must
        // run at the same viewport/zoom as the screenshot itself. If the target
        // tab is created by an action such as go-live, apply the profile as soon
        // as it becomes available.
        const shotSource = shot[SOURCE_LOCATION] || shot[TARGET_LOCATION];
        try {
            await applyShotViewportProfile(shot);
        } catch (error) {
            throw withSourceError(error, shotSource);
        }

        for (const action of shot.actions) {
            const source = action[SOURCE_LOCATION] || shotSource;
            console.log(`  line ${source?.line ?? '?'}: ${source?.text?.trim() || action.type}`);

            try {
                await executeAction(action, shot.tab, tutorialHooks);
                await applyShotViewportProfile(shot);
            } catch (error) {
                const errorPreview = await captureErrorTabs({
                    tutorialDirectory,
                    shot,
                }).catch(captureError => {
                    console.error(
                        `Could not capture tutorial screenshot failure state: ${captureError.message}`,
                    );
                    return null;
                });
                if (progressJob && errorPreview) {
                    progressJob.screenshots = progressJob.screenshots.filter(
                        item => !item.error_preview,
                    );
                    progressJob.screenshots.push(errorPreview);
                }

                throw withSourceError(error, source);
            }
        }

        if (stale[index]) {
            const targetSource = shot[TARGET_LOCATION] || shotSource;
            console.log(`  line ${targetSource?.line ?? '?'}: capture ${shot.target}`);
            let outputSha256;
            try {
                outputSha256 = await captureShot(shot, tutorialDirectory);
            } catch (error) {
                throw withSourceError(error, targetSource);
            }
            generated += 1;
            nextManifest.screenshots[shot.target] = {
                state_hash: shot.stateHash,
                environment_fingerprint: environmentFingerprint,
                output_sha256: outputSha256,
            };
            if (progressJob) {
                Object.assign(progressJob.screenshots[index], {
                    stale: false,
                    missing: false,
                    state: 'fresh',
                    revision: outputSha256,
                });
                progressJob.generated = generated;
            }
            console.log(`Generated ${relativeMarkdown}: ${shot.target}`);
        } else {
            nextManifest.screenshots[shot.target] = oldManifest.screenshots[shot.target];
            if (progressJob) progressJob.screenshots[index].state = 'current';
        }

        if (progressJob) progressJob.completed = index + 1;
    }

    await writeManifest(tutorialDirectory, nextManifest);
    if (generated > 0) {
        scheduleFreshWorkspace();
    }
    return { generated, current: shots.length - generated, screenshots: shots.length };
}

function sendJson(response, status, payload) {
    const body = `${JSON.stringify(payload)}\n`;
    response.writeHead(status, {
        'content-type': 'application/json; charset=utf-8',
        'content-length': Buffer.byteLength(body),
    });
    response.end(body);
}

async function readJson(request) {
    const chunks = [];
    let size = 0;
    for await (const chunk of request) {
        size += chunk.length;
        if (size > 1024 * 1024) throw new Error('Request is too large');
        chunks.push(chunk);
    }
    return JSON.parse(Buffer.concat(chunks).toString('utf8') || '{}');
}

const server = http.createServer(async (request, response) => {
    const requestUrl = new URL(request.url, `http://127.0.0.1:${PORT}`);

    if (request.method === 'GET' && requestUrl.pathname === '/health') {
        return sendJson(response, 200, { ok: true });
    }
    if (request.method === 'GET' && requestUrl.pathname === '/status') {
        try {
            const relativeMarkdown = safeRelativePath(requestUrl.searchParams.get('markdown_path'));
            if (!relativeMarkdown.endsWith('.md')) {
                throw new Error('markdown_path must name a Markdown file');
            }
            const job = latestGenerationJobs.get(relativeMarkdown);
            return sendJson(response, 200, job || {
                status: 'idle',
                monitor: false,
                markdown_path: relativeMarkdown,
                screenshots: [],
            });
        } catch (error) {
            return sendJson(response, 400, { error: error.message });
        }
    }
    if (request.method !== 'POST' || requestUrl.pathname !== '/generate') {
        return sendJson(response, 404, { error: 'not_found' });
    }

    try {
        const payload = await readJson(request);
        if (payload?.async === true && payload?.force !== true) {
            try {
                const result = await enqueueGeneration(payload);
                return sendJson(response, 200, result);
            } catch (error) {
                recordGenerationRequestError(payload, error);
                throw error;
            }
        }

        const job = generationQueue.then(() => generateTutorial(payload));
        generationQueue = job.catch(() => {});
        const result = await job;
        sendJson(response, 200, result);
    } catch (error) {
        console.error(error);
        sendJson(response, 500, { error: error.message, stack: error.stack });
    }
});

server.listen(PORT, '0.0.0.0', () => {
    console.log(`Tutorial screenshot generator listening on port ${PORT}`);
});
