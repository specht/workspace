import crypto from 'node:crypto';
import dns from 'node:dns/promises';
import fs from 'node:fs/promises';
import fsSync from 'node:fs';
import http from 'node:http';
import os from 'node:os';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import { pathToFileURL } from 'node:url';
import {
    parseTerminalLineCropDirective,
    parseTerminalLineSkip,
    scalePixelDimensions,
    terminalLineCropPixels,
    validateCropDirectives,
} from './crop.js';
import { resetScreenshotDatabases } from './workspace-reset.js';
import {
    chainStateHashes,
    checkGitGraphForTarget,
    checkRecordForTarget,
    codeServerRelease,
    entryIsCurrent,
    findRecipeTarget,
    frameTimes,
    isVideoTarget,
    parseFps,
    parseRecordSeconds,
    parseVariants,
    showsWorkspace,
    variantTargets,
    videoEncodeArgs,
} from './plan.js';
import { chromium } from 'playwright';
import { gitGraphSvg } from './git-graph.js';

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
// Part of every state hash: bumping it re-renders every screenshot of every
// tutorial. Recipe features that do not change existing images need no bump.
const GENERATOR_VERSION = 19;
const DEFAULT_VIDEO_FPS = 30;
// A recipe step that addresses an element already on screen fails fast, so a
// wrong selector costs seconds; waiting for the application to get somewhere
// (compiling, cloning) gets longer.
const ACTION_TIMEOUT_MS = 10_000;
const WAIT_FOR_TEXT_TIMEOUT_MS = 30_000;
const WORKSPACE_FONT_FAMILY = '0xProto Nerd Font Mono';
const PLAYWRIGHT_VERSION = '1.62.1';
const MANIFEST_NAME = '.tutorial-screenshots.json';
const DEFAULT_PROFILE = Object.freeze({
    width: positiveIntegerEnv('TUTORIAL_SCREENSHOT_WIDTH', 1853),
    height: positiveIntegerEnv('TUTORIAL_SCREENSHOT_HEIGHT', 929),
    zoom: positiveNumberEnv('TUTORIAL_SCREENSHOT_ZOOM', 1),
    captureScale: positiveNumberEnv('TUTORIAL_SCREENSHOT_CAPTURE_SCALE', 2),
    desktopScaleFactor: positiveNumberEnv('TUTORIAL_SCREENSHOT_DESKTOP_SCALE_FACTOR', 1.203125),
});
// The Workspace tab's window height (config.rb, :workspace_height): taller
// than a preview's since VS Code 1.140 draws the panel as a card with margins.
// Recorded per Workspace shot (workspace_window), so changing it makes only
// those stale, not previews, PDFs, images or Git graphs.
const WORKSPACE_HEIGHT = positiveIntegerEnv('TUTORIAL_SCREENSHOT_WORKSPACE_HEIGHT', 560);
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
    'neo4j-browser',
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
// Chromium draws no mouse pointer into screenshots. show-mouse draws one where
// the recipe last put the mouse: move-mouse, click and hold set the position;
// an action that may click on its own (open-file, close-tab, …) forgets it, so
// a pointer is never drawn where the mouse no longer is.
const MOUSE_NEUTRAL_ACTIONS = new Set([
    'press', 'type', 'type-source', 'sleep', 'write-file', 'select-option',
    'wait-for-text', 'wait-for-input-value', 'wait-for-file', 'wait-for-file-newer',
    'terminal-wait-for-prompt', 'expect-no-text', 'terminal-expect-no-text', 'check-task', 'check-task-fails',
]);
let mousePosition = null; // { page, x, y } in CSS pixels
// 24x24 shapes, drawn at 24 CSS pixels; hotspot is the point the mouse is at.
const MOUSE_POINTERS = Object.freeze({
    arrow: {
        hotspot: [3, 2],
        svg: '<path d="M3 2 V19.5 L7.3 15.6 L10.2 22 L13.2 20.7 L10.4 14.5 H16.2 Z" ' +
            'fill="#fff" stroke="#000" stroke-width="1.2" stroke-linejoin="round"/>',
    },
    pointer: {
        // A short index finger, like the hand cursors of desktop systems.
        hotspot: [8.5, 4.6],
        svg: '<path d="M8.5 4.6 C9.4 4.6 10 5.3 10 6.1 V10.2 C10 9.4 10.6 8.8 11.5 8.8 ' +
            'C12.4 8.8 13 9.4 13 10.2 V10.8 C13 10 13.6 9.5 14.5 9.5 C15.4 9.5 16 10.1 16 10.9 ' +
            'V11.5 C16 10.8 16.6 10.3 17.4 10.3 C18.3 10.3 18.9 10.9 18.9 11.7 V16.5 ' +
            'C18.9 19.6 16.9 22 13.9 22 H11.6 C9.8 22 8.6 21.2 7.6 19.8 L4.1 14.6 ' +
            'C3.6 13.9 3.8 12.9 4.5 12.5 C5.2 12.1 6.1 12.3 6.6 13 L7 13.6 V6.1 ' +
            'C7 5.3 7.6 4.6 8.5 4.6 Z" fill="#fff" stroke="#000" stroke-width="1.1" ' +
            'stroke-linejoin="round"/>' +
            '<path d="M10 10.2 V14.5 M13 10.8 V14.5 M16 11.5 V14.5" stroke="#000" ' +
            'stroke-width="1" stroke-linecap="round"/>',
    },
    text: {
        hotspot: [12, 12],
        svg: '<path d="M9 4 C10.8 4 12 4.6 12 6 C12 4.6 13.2 4 15 4 M12 6 V18 ' +
            'M9 20 C10.8 20 12 19.4 12 18 C12 19.4 13.2 20 15 20" fill="none" stroke="#fff" ' +
            'stroke-width="3.2" stroke-linecap="round"/>' +
            '<path d="M9 4 C10.8 4 12 4.6 12 6 C12 4.6 13.2 4 15 4 M12 6 V18 ' +
            'M9 20 C10.8 20 12 19.4 12 18 C12 19.4 13.2 20 15 20" fill="none" stroke="#000" ' +
            'stroke-width="1.2" stroke-linecap="round"/>',
    },
});
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
// Jobs queued or running, from page views and explicit runs alike.
let pendingJobs = 0;
// Automatic rendering on page views (development). Paused by hand from the
// status bar, or by itself when a page view would start rendering while
// another job is under way (an explicit run, for example), so browsing never
// interferes with it. Explicit runs (recreate-screenshots.rb) ignore it. Kept
// next to the Git cache, so it survives restarts and rebuilds.
const AUTO_RENDER_FILE = path.join(GIT_CACHE_ROOT, '.auto-render.json');
let autoRender = { enabled: true, reason: null };
try {
    const stored = JSON.parse(fsSync.readFileSync(AUTO_RENDER_FILE, 'utf8'));
    if (typeof stored.enabled === 'boolean') autoRender = { enabled: stored.enabled, reason: stored.reason ?? null };
} catch {}

async function setAutoRender(enabled, reason = null) {
    autoRender = { enabled, reason: enabled ? null : reason };
    await fs.writeFile(AUTO_RENDER_FILE, JSON.stringify(autoRender)).catch(error => {
        console.warn(`Could not store the auto-render state: ${error.message}`);
    });
    console.log(`Tutorial screenshot auto-render ${enabled ? 'resumed' : `paused (${reason})`}`);
}

const latestGenerationJobs = new Map();
// For the progress page (/screenshot-progress on the website): the pass that
// recreate-screenshots.rb announced (POST /run) and the files captured last.
let currentRun = null; // { started_at, force, pages: [{ markdown_path, stale }] }
const recentCaptures = [];
const RECENT_CAPTURES = 120;

function rememberCapture(markdownPath, target, revision) {
    recentCaptures.unshift({ markdown_path: markdownPath, target, revision, at: Date.now(), kind: isVideoTarget(target) ? 'video' : 'image' });
    if (recentCaptures.length > RECENT_CAPTURES) recentCaptures.length = RECENT_CAPTURES;
}
let workspacePrepared = false;
let workspacePreparationPromise = null;
let terminalShellReady = false;
const deviceMetricSessions = new WeakMap();
const appliedViewportProfiles = new WeakMap();
const SOURCE_LOCATION = Symbol('tutorial-screenshot-source-location');
const TARGET_LOCATION = Symbol('tutorial-screenshot-target-location');
// Steps after the last screenshot (<!-- tutorial-step --> blocks): parseTutorial
// hands them along on the list of shots; they run at the end of a replay.
const TRAILING_STEPS = Symbol('tutorial-screenshot-trailing-steps');
// The tutorial-scope a shot lies in (an index), and how many of its first
// actions are steps from before the scope (they run even if it is skipped).
const SCOPE = Symbol('tutorial-scope');
const SCOPE_ENTRY = Symbol('tutorial-scope-entry');
// A Workspace shot in the default Workspace window (no viewport: of its
// own): its state hash says »the Workspace window«, not the height, so a
// taller window does not change the chain of the shots after it (plan.js).
const DEFAULT_WORKSPACE_VIEWPORT = Symbol('tutorial-default-workspace-viewport');

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
                setPreviewTime: null,
                actions: {},
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
    if (hooks.setPreviewTime != null && typeof hooks.setPreviewTime !== 'function') {
        throw new Error('tutorial-screenshot-hooks.mjs setPreviewTime must be a function');
    }
    const actions = hooks.actions ?? {};
    if (typeof actions !== 'object' ||
        Object.values(actions).some(action => typeof action !== 'function')) {
        throw new Error('tutorial-screenshot-hooks.mjs actions must map names to functions');
    }

    const variants = hooks.variants ?? {};
    if (typeof variants !== 'object' ||
        Object.values(variants).some(variant => typeof variant !== 'function')) {
        throw new Error('tutorial-screenshot-hooks.mjs variants must map names to functions');
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
        setPreviewTime: hooks.setPreviewTime ?? null,
        actions,
        variants,
    };
}

// variants: NAMES switches each variant through variants.NAME({ page, on })
// from the tutorial's hooks; on lists the ones to switch on, all others go off.
async function setVariants(shot, hooks, on) {
    const page = requirePageForTab(shot.tab, 'variants');
    for (const name of shot.variants) {
        const run = hooks?.variants?.[name];
        if (!run) {
            throw new Error(`variants: ${name} needs variants.${name} in tutorial-screenshot-hooks.mjs`);
        }
        await run({ page, on: on.includes(name), timeout: 60_000 });
    }
}

// hook: NAME ARGS runs actions.NAME from the tutorial's hooks on the page the
// recipe's tab selects, for what only that tutorial's application knows how
// to do (the anaglyph viewer's camera, for example).
async function runHookAction(action, targetTab, hooks) {
    const run = hooks?.actions?.[action.name];
    if (!run) {
        throw new Error(`hook: ${action.name} needs actions.${action.name} in tutorial-screenshot-hooks.mjs`);
    }
    const page = requirePageForTab(targetTab, `hook: ${action.name}`);
    // writeFile(path, text) writes into the Workspace like write-file: for a
    // value only the page knows (a database login shown in the profile).
    const writeFile = (relativePath, text) => writeWorkspaceFile(relativePath, String(text), hooks);
    await run({ page, args: action.args, timeout: 60_000, writeFile });
}

// A promise that rejects after ms with what was being waited for.
function withTimeout(promise, ms, what) {
    let timer;
    const limit = new Promise((_, reject) => {
        timer = setTimeout(() => reject(new Error(`gave up after ${ms / 1000} s: ${what}`)), ms);
    });
    return Promise.race([promise, limit]).finally(() => clearTimeout(timer));
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
            // without context lines (»~ «): they are in the file already,
            // and the copy button leaves them out too
            contents: match[1].replace(/\n$/, '').split('\n').filter(line => !/^~( |$)/.test(line)).join('\n'),
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
        viewportSet: false,
        zoom: DEFAULT_PROFILE.zoom,
        cropTop: 0,
        cropBottom: 0,
        cropLeft: 0,
        cropRight: 0,
        cropTerminalLines: null,
        cropTerminalSkipTop: 0,
        cropTerminalSkipBottom: 0,
        terminalScrollBack: 0,
        record: null,
        fps: null,
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

        // terminal-wait-for-prompt: SECONDS for commands that take longer than
        // a minute (compiling a document with many fonts, for example).
        let waitMatch;
        if ((waitMatch = line.match(/^terminal-wait-for-prompt:\s*(\d+)$/))) {
            const seconds = Number(waitMatch[1]);
            if (seconds < 1 || seconds > 600) {
                throw withSourceError(new Error('terminal-wait-for-prompt waits 1 to 600 seconds'), source);
            }
            const action = { type: 'terminal-wait-for-prompt', seconds };
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
        } else if ((match = line.match(/^preview-port:\s*(\d+)(?:\s+(\/[A-Za-z0-9_\-./%]*))?$/))) {
            const port = Number(match[1]);
            if (port < 1024 || port > 65535) {
                throw withSourceError(new Error(`Invalid preview port: ${port}`), source);
            }
            // An optional path opens a page below the app's root: /axolotl/.
            // Only stored when given, so recipes without one keep their
            // fingerprint (a new key would make every preview stale).
            const action = { type: 'preview-port', port };
            if (match[2]) action.path = match[2];
            recipe.actions.push(action);
        } else if ((match = line.match(/^open-site:\s*(\/[A-Za-z0-9_\-./]*)$/))) {
            recipe.actions.push({ type: 'open-site', path: match[1] });
        } else if ((match = line.match(/^select-option:\s*(#[^\s=]+)\s*=\s*(\S+)$/))) {
            recipe.actions.push({ type: 'select-option', selector: match[1], value: match[2] });
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
        } else if ((match = line.match(/^type:\s*<-\s*(previous-code(?:\s+\([^)]+\))?|code:.+|file:.+)$/))) {
            // A whole source text, typed in one piece (sources as for write-file).
            recipe.actions.push({ type: 'type-source', source: match[1].trim() });
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
        } else if ((match = line.match(/^hook:\s*([a-z][a-z0-9-]*)(?:\s+(.*))?$/))) {
            recipe.actions.push({ type: 'hook', name: match[1], args: (match[2] || '').trim() });
        } else if ((match = line.match(/^terminal-expect-no-text:\s*(.+)$/))) {
            recipe.actions.push({ type: 'terminal-expect-no-text', text: match[1] });
        } else if ((match = line.match(/^expect-no-text:\s*(.+)$/))) {
            recipe.actions.push({ type: 'expect-no-text', text: match[1] });
        } else if ((match = line.match(/^check-task(-fails)?:\s*([a-z0-9][a-z0-9-]*)(?:\s+(ruby|python))?(?:\s+<-\s*(.*))?$/))) {
            // The task lives in the tutorial's own tasks.yaml: activity/id.
            // After <- comes what a student types into the card: a command,
            // an answer, the number of an option.
            const activity = path.basename(path.dirname(markdownPath));
            recipe.actions.push({ type: match[1] ? 'check-task-fails' : 'check-task', task: `${activity}/${match[2]}`, language: match[3] || null, answer: (match[4] || '').trim() });
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
            recipe.viewportSet = true;
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
        } else if ((match = line.match(/^crop-left:\s*(.+)$/))) {
            cropSource = source;
            recipe.cropLeft = valueAtSource(source, () => parsePercent(match[1]));
        } else if ((match = line.match(/^crop-right:\s*(.+)$/))) {
            cropSource = source;
            recipe.cropRight = valueAtSource(source, () => parsePercent(match[1]));
        } else if (line.startsWith('crop-terminal-lines:')) {
            cropSource = source;
            recipe.cropTerminalLines = valueAtSource(
                source,
                () => parseTerminalLineCropDirective(line),
            );
        } else if ((match = line.match(/^crop-terminal-skip-top:\s*(.+)$/))) {
            cropSource = source;
            recipe.cropTerminalSkipTop = valueAtSource(
                source,
                () => parseTerminalLineSkip(match[1], 'crop-terminal-skip-top'),
            );
        } else if ((match = line.match(/^crop-terminal-skip-bottom:\s*(.+)$/))) {
            cropSource = source;
            recipe.cropTerminalSkipBottom = valueAtSource(
                source,
                () => parseTerminalLineSkip(match[1], 'crop-terminal-skip-bottom'),
            );
        } else if ((match = line.match(/^crop-element:\s*(?:selector:)?(.+)$/))) {
            cropSource = source;
            // several lines: the box around all of them (a pane's header
            // and its rows); one stays a string, so its hash is as before
            const selector = match[1].trim();
            recipe.cropElement = recipe.cropElement == null ? selector : [recipe.cropElement, selector].flat();
        } else if ((match = line.match(/^keep-only:\s*(?:selector:)?(.+)$/))) {
            cropSource = source;
            (recipe.keepOnly ||= []).push(match[1].trim());
        } else if ((match = line.match(/^beside:\s*(workspace|preview)$/))) {
            // the other tab captured too, at the same size, to the right
            recipe.beside = match[1];
        } else if (line === 'show-mouse') {
            recipe.showMouse = true;
        } else if (line === 'sharp-canvas') {
            recipe.sharpCanvas = true;
        } else if ((match = line.match(/^variants:\s*(.+)$/))) {
            recipe.variants = valueAtSource(source, () => parseVariants(match[1]));
        } else if ((match = line.match(/^pdf-page(-full)?:\s*(\S+)\s+(\d+)(?:\s+(.+))?$/))) {
            recipe.pdfPage = {
                path: valueAtSource(source, () => safeRelativePath(match[2])),
                page: Number(match[3]),
            };
            // pdf-page-full: the whole sheet with its margins, not cut to
            // the text
            if (match[1]) recipe.pdfPage.full = true;
            if (match[4]) recipe.pdfPage.expect = match[4].trim();
            if (recipe.pdfPage.page < 1) {
                throw withSourceError(new Error('pdf-page counts pages from 1'), source);
            }
        } else if ((match = line.match(/^image-file:\s*(\S+)(?:\s+(\d+))?$/))) {
            // image-file: PATH [SCALE]: a picture a program saved in the
            // Workspace (a PNG), enlarged SCALE times with sharp pixels
            recipe.imageFile = {
                path: valueAtSource(source, () => safeRelativePath(match[1])),
                scale: match[2] ? Number(match[2]) : 1,
            };
            if (recipe.imageFile.scale < 1 || recipe.imageFile.scale > 16) {
                throw withSourceError(new Error('image-file scales from 1 to 16 times'), source);
            }
        } else if ((match = line.match(/^git-graph:\s*(.+)$/))) {
            recipe.gitGraph = valueAtSource(source, () => safeRelativePath(match[1].trim()));
        } else if ((match = line.match(/^record:\s*(.+)$/))) {
            recipe.record = valueAtSource(source, () => parseRecordSeconds(match[1]));
        } else if ((match = line.match(/^fps:\s*(.+)$/))) {
            recipe.fps = valueAtSource(source, () => parseFps(match[1]));
        } else if ((match = line.match(/^terminal-scroll-back:\s*(\d+)$/))) {
            const lines = Number.parseInt(match[1], 10);
            if (lines < 1 || lines > 200) {
                throw withSourceError(
                    new Error(`terminal-scroll-back must be between 1 and 200 lines: ${lines}`),
                    source,
                );
            }
            recipe.terminalScrollBack = lines;
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

    if ((recipe.pdfPage || recipe.imageFile) && (recipe.gitGraph || recipe.record != null || recipe.showMouse ||
        recipe.cropTerminalLines != null || recipe.cropTop > 0 || recipe.cropBottom > 0)) {
        throw withSourceError(
            new Error('pdf-page and image-file make the image from a file: no git-graph, record, show-mouse or crops'),
            lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.gitGraph && (recipe.record != null || recipe.showMouse || recipe.cropTerminalLines != null)) {
        throw withSourceError(
            new Error('git-graph draws an SVG: no record, show-mouse or crops'),
            lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.variants && ((recipe.pdfPage || recipe.imageFile) || recipe.gitGraph)) {
        throw withSourceError(
            new Error('variants needs a screenshot or a video, not pdf-page or git-graph'),
            lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.showMouse && recipe.record != null) {
        throw withSourceError(new Error('show-mouse works for images, not with record'), lastSource || recipe[SOURCE_LOCATION]);
    }
    if (recipe.fps != null && recipe.record == null) {
        throw withSourceError(new Error('fps needs record: SECONDSs'), lastSource || recipe[SOURCE_LOCATION]);
    }
    if (recipe.record != null && (recipe.cropTerminalLines != null || recipe.terminalScrollBack > 0)) {
        throw withSourceError(
            new Error('record cannot be combined with terminal crops or terminal-scroll-back'),
            lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.cropElement && (recipe.cropTop > 0 || recipe.cropBottom > 0 || recipe.cropLeft > 0 ||
        recipe.cropRight > 0 || recipe.cropTerminalLines != null || recipe.record != null ||
        (recipe.pdfPage || recipe.imageFile) || recipe.gitGraph)) {
        throw withSourceError(
            new Error('crop-element replaces the other crops and works for screenshots only'),
            cropSource || lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.beside && (recipe.cropTop > 0 || recipe.cropBottom > 0 || recipe.cropLeft > 0 ||
        recipe.cropRight > 0 || recipe.cropElement || recipe.cropTerminalLines != null || recipe.keepOnly ||
        recipe.record != null || (recipe.pdfPage || recipe.imageFile) || recipe.gitGraph || recipe.beside === recipe.tab)) {
        throw withSourceError(
            new Error('beside: the other tab, whole, next to an uncropped screenshot'),
            lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.keepOnly && (recipe.record != null || (recipe.pdfPage || recipe.imageFile) || recipe.gitGraph)) {
        throw withSourceError(
            new Error('keep-only works for screenshots only'),
            cropSource || lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if (recipe.cropLeft + recipe.cropRight >= 0.98) {
        throw withSourceError(
            new Error('Left and right crop leave no useful screenshot area'),
            cropSource || lastSource || recipe[SOURCE_LOCATION],
        );
    }
    if ((recipe.cropLeft > 0 || recipe.cropRight > 0) &&
        (recipe.record != null || (recipe.pdfPage || recipe.imageFile) || recipe.gitGraph)) {
        throw withSourceError(
            new Error('crop-left and crop-right work for screenshots only'),
            cropSource || lastSource || recipe[SOURCE_LOCATION],
        );
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
            cropTerminalSkipTop: recipe.cropTerminalSkipTop,
            cropTerminalSkipBottom: recipe.cropTerminalSkipBottom,
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

// A <!-- tutorial-step … --> block is a recipe without a picture: what the
// page tells students to do between two screenshots, often with assertions
// (check-task, expect-no-text). Its actions run, in its own tab, before the
// next screenshot's and count towards that screenshot's state; steps after
// the last screenshot run at the end of every replay.
// <!-- tutorial-scope --> … <!-- /tutorial-scope --> encloses recipes whose
// pictures do not matter for what comes after them: a shot after the scope
// does not depend on the shots inside (its hash chains past them), and at
// the end of the scope the generator puts back every file write-file wrote
// inside it and reloads the preview. A scope whose shots are all current is
// not walked at all. Scopes do not nest.
function tutorialScopes(markdown, markdownPath) {
    const scopes = [];
    const marker = /<!--\s*(\/?)tutorial-scope\s*-->/g;
    let open = null;
    let match;
    while ((match = marker.exec(markdown)) !== null) {
        const source = markdownSourceLocation(markdown, markdownPath, match.index);
        if (!match[1]) {
            if (open) throw withSourceError(new Error('tutorial-scope inside another tutorial-scope'), source);
            open = { start: match.index, source };
        } else {
            if (!open) throw withSourceError(new Error('/tutorial-scope without tutorial-scope'), source);
            scopes.push({ start: open.start, end: match.index, source: open.source });
            open = null;
        }
    }
    if (open) throw withSourceError(new Error('tutorial-scope is never closed'), open.source);
    return scopes;
}

async function parseTutorial(markdown, markdownPath) {
    const blocks = codeBlocks(markdown, markdownPath);
    const regex = /<!--\s*tutorial-(screenshot|step)\s*\n([\s\S]*?)-->/g;
    const shots = [];
    let pendingSteps = [];
    let match;
    const scopes = tutorialScopes(markdown, markdownPath);
    const scopeAt = (index) => scopes.findIndex(scope => scope.start < index && index < scope.end);
    let stepsScope = -1; // the scope the pending steps were written in

    const resolveActions = (recipe, matchIndex, recipeSource) => Promise.all(recipe.actions.map(async action => {
        if (action.type !== 'write-file' && action.type !== 'type-source') return action;
        try {
            return {
                ...action,
                contents: await resolveWriteFileSource(blocks, matchIndex, action.source, markdownPath),
            };
        } catch (error) {
            throw withSourceError(error, action[SOURCE_LOCATION] || recipeSource);
        }
    }));

    while ((match = regex.exec(markdown)) !== null) {
        const kind = match[1];
        const recipeStart = match.index + match[0].indexOf(match[2]);
        const recipeSource = markdownSourceLocation(markdown, markdownPath, recipeStart);
        let recipe;
        try {
            recipe = parseRecipe(match[2], markdown, markdownPath, recipeStart);
        } catch (error) {
            throw withSourceError(error, recipeSource);
        }

        const scope = scopeAt(match.index);
        if (pendingSteps.length > 0 && stepsScope !== scope && stepsScope >= 0) {
            throw withSourceError(new Error('a tutorial-step at the end of a tutorial-scope needs a screenshot after it, inside the scope'), recipeSource);
        }
        if (kind === 'step') {
            if (pendingSteps.length === 0) stepsScope = scope;
            const actions = await resolveActions(recipe, match.index, recipeSource);
            pendingSteps.push(...actions.map(action => {
                const tagged = { ...action, tab: recipe.tab };
                tagged[SOURCE_LOCATION] = action[SOURCE_LOCATION];
                return tagged;
            }));
            continue;
        }

        const afterComment = markdown.slice(regex.lastIndex);
        const targetMatch = findRecipeTarget(afterComment);
        if (!targetMatch) {
            throw withSourceError(
                new Error('tutorial-screenshot block has no following <img> or <video>'),
                recipeSource,
            );
        }

        const targetSource = markdownSourceLocation(
            markdown,
            markdownPath,
            regex.lastIndex + targetMatch.index,
        );
        let target;
        try {
            target = safeRelativePath(targetMatch.src);
            if (/^[a-z][a-z\d+.-]*:/i.test(target) || target.startsWith('/')) {
                throw new Error(`Generated screenshot must use a relative tutorial image path: ${target}`);
            }
            checkRecordForTarget(target, recipe.record);
            checkGitGraphForTarget(target, recipe.gitGraph);
        } catch (error) {
            throw withSourceError(error, targetSource);
        }

        // Steps from before a scope run even when the scope is skipped
        const entryActions = scope >= 0 && stepsScope !== scope ? pendingSteps.length : 0;
        const resolvedActions = [...pendingSteps, ...await resolveActions(recipe, match.index, recipeSource)];
        pendingSteps = [];
        stepsScope = -1;

        const resolved = {
            target,
            tab: recipe.tab,
            viewport: recipe.viewportSet || recipe.tab !== 'workspace'
                ? recipe.viewport : { width: DEFAULT_PROFILE.width, height: WORKSPACE_HEIGHT },
            zoom: recipe.zoom,
            cropTop: recipe.cropTop,
            cropBottom: recipe.cropBottom,
            actions: resolvedActions,
        };
        if (!recipe.viewportSet && recipe.tab === 'workspace') resolved[DEFAULT_WORKSPACE_VIEWPORT] = true;
        if (recipe.cropTerminalLines != null) {
            resolved.cropTerminalLines = recipe.cropTerminalLines;
        }
        if (recipe.cropTerminalSkipTop > 0) {
            resolved.cropTerminalSkipTop = recipe.cropTerminalSkipTop;
        }
        if (recipe.cropTerminalSkipBottom > 0) {
            resolved.cropTerminalSkipBottom = recipe.cropTerminalSkipBottom;
        }
        if (recipe.terminalScrollBack > 0) {
            resolved.terminalScrollBack = recipe.terminalScrollBack;
        }
        if (recipe.showMouse) {
            // The version of the pointer drawing: a new one re-renders only
            // the shots that show the pointer.
            resolved.showMouse = 3;
        }
        if (recipe.sharpCanvas) {
            resolved.sharpCanvas = true;
        }
        // Only shots that crop sideways carry these, so all others hash as before.
        if (recipe.cropLeft > 0) resolved.cropLeft = recipe.cropLeft;
        if (recipe.cropRight > 0) resolved.cropRight = recipe.cropRight;
        if (recipe.cropElement) resolved.cropElement = recipe.cropElement;
        if (recipe.keepOnly) resolved.keepOnly = recipe.keepOnly;
        if (recipe.beside) resolved.beside = recipe.beside;
        if (recipe.variants) {
            resolved.variants = recipe.variants;
        }
        // Only graphs carry this, so images hash exactly as before.
        // Only PDF pages carry this, so images hash exactly as before.
        if (recipe.pdfPage) {
            if (!/\.(webp|png)$/i.test(target)) {
                throw withSourceError(new Error(`pdf-page needs a .webp or .png target, not ${target}`), targetSource);
            }
            resolved.pdfPage = recipe.pdfPage;
        }
        // Only pictures from files carry this, so images hash exactly as before.
        if (recipe.imageFile) {
            if (!/\.(webp|png)$/i.test(target)) {
                throw withSourceError(new Error(`image-file needs a .webp or .png target, not ${target}`), targetSource);
            }
            resolved.imageFile = recipe.imageFile;
        }
        if (recipe.gitGraph) {
            resolved.gitGraph = recipe.gitGraph;
        }
        // Only videos carry these, so images hash exactly as before.
        if (recipe.record != null) {
            resolved.record = recipe.record;
            resolved.fps = recipe.fps ?? DEFAULT_VIDEO_FPS;
        }
        resolved[SOURCE_LOCATION] = recipe[SOURCE_LOCATION] || recipeSource;
        resolved[TARGET_LOCATION] = targetSource;
        if (scope >= 0) {
            resolved[SCOPE] = scope;
            if (entryActions > 0) resolved[SCOPE_ENTRY] = entryActions;
        }
        shots.push(resolved);
    }
    if (pendingSteps.length > 0 && stepsScope >= 0) {
        throw withSourceError(new Error('a tutorial-step at the end of a tutorial-scope needs a screenshot after it, inside the scope'), scopes[stepsScope].source);
    }

    // See plan.js: a shot depends on the actions before it, not on their crops.
    const hashes = chainStateHashes(
        shots,
        sha256(`tutorial-screenshot-v${GENERATOR_VERSION}\0${markdownPath}`),
        shot => (shot[SCOPE] === undefined ? null : { scope: shot[SCOPE], entry: shot[SCOPE_ENTRY] || 0 }),
        shot => (shot[DEFAULT_WORKSPACE_VIEWPORT] ? { ...shot, viewport: 'workspace' } : shot),
        shot => (shot[DEFAULT_WORKSPACE_VIEWPORT] ? { ...shot, viewport: { width: DEFAULT_PROFILE.width, height: DEFAULT_PROFILE.height } } : shot),
    );
    shots.forEach((shot, index) => Object.assign(shot, hashes[index]));
    shots[TRAILING_STEPS] = pendingSteps;
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

// The manifest is looked up by name only, so its text has keys sorted at
// every level: rendering in another order leaves the file unchanged.
function sortedKeys(value) {
    if (Array.isArray(value)) return value.map(sortedKeys);
    if (!value || typeof value !== 'object') return value;
    return Object.fromEntries(Object.keys(value).sort().map(key => [key, sortedKeys(value[key])]));
}

function manifestText(manifest) {
    return `${JSON.stringify(sortedKeys(manifest), null, 2)}\n`;
}

async function writeManifest(directory, manifest) {
    const target = path.join(directory, MANIFEST_NAME);
    const temporary = `${target}.tmp-${process.pid}-${Date.now()}`;
    await fs.writeFile(temporary, manifestText(manifest), 'utf8');
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
            // Development runs on plain http. VS Code webviews (the ER-Werkstatt,
            // the Markdown preview) need a service worker, which browsers allow
            // only in a secure context; students on https have one.
            // A Workspace's subdomain changes with every reset: a hostname
            // pattern (no scheme, no port) covers all of them.
            const secureOrigins = `${new URL(BASE_URL).origin},*.${hostname}`;

            return chromium.launch({
                headless: true,
                // The full Chromium in headless mode, not chrome-headless-shell:
                // only it honours --unsafely-treat-insecure-origin-as-secure.
                channel: 'chromium',
                args: [
                    `--host-resolver-rules=${hostRules}`,
                    `--unsafely-treat-insecure-origin-as-secure=${secureOrigins}`,

                    // Give headless Chromium a software-backed WebGL implementation.
                    // This lets VS Code/xterm use its WebGL renderer even without a GPU.
                    '--use-gl=angle',
                    '--use-angle=swiftshader',
                    '--enable-webgl',

                    // '--disable-font-subpixel-positioning',
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
    // A crashed tab otherwise surfaces only as "Not attached to an active page".
    context.on('page', page => {
        page.on('crash', () => console.warn(`Tutorial screenshot tab crashed: ${page.url()}`));
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

// After many WebGL scenes the browser's GPU process can get stuck; its tabs
// then never close and every new one waits. A new browser is the way out.
async function restartBrowser(reason) {
    console.warn(`Restarting the screenshot browser: ${reason}`);
    const old = browserPromise;
    browserPromise = null;
    context = null;
    dashboard = null;
    loggedIn = false;
    workspace = null;
    preview = null;
    const browser = await old?.catch(() => null);
    browser?.process()?.kill('SIGKILL');
    await withTimeout(browser?.close() ?? Promise.resolve(), 10_000, 'closing the old browser').catch(() => {});
}

async function prepareFreshWorkspace() {
    workspacePrepared = false;
    await ensureLoggedIn();

    const others = context.pages().filter(page => page !== dashboard);
    try {
        await withTimeout(Promise.all(others.map(page => page.close().catch(() => {}))), 15_000, 'closing the tabs of the last render');
    } catch (error) {
        await restartBrowser(error.message);
        await ensureLoggedIn();
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
    await applyViewportProfile(workspace, { width: DEFAULT_PROFILE.width, height: WORKSPACE_HEIGHT }, DEFAULT_PROFILE.zoom);
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

// The text must not be in the Workspace's terminals now, whatever tab the
// recipe captures: a compiler's »error:«, make's »***«.
async function terminalExpectNoText(text) {
    if (!workspace) throw new Error('terminal-expect-no-text: no Workspace is open');
    const shown = await workspace.evaluate(expected => {
        const normalize = value => `${value || ''}`.replace(/\s+/g, ' ').trim();
        const rows = [...document.querySelectorAll('.terminal.xterm .xterm-rows > div')].map(row => normalize(row.textContent));
        return rows.find(row => row.includes(normalize(expected))) || null;
    }, text);
    if (shown) throw new Error(`terminal-expect-no-text: ${JSON.stringify(text)} is in the terminal: ${shown}`);
}

async function waitForTerminalPrompt(seconds = 60) {
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
            // Bash, or the second line of bash-git-prompt (Git tutorial,
            // »Etwas Luxus im Terminal«): »19:09 $«.
            return (lastLine.includes(`${user}@workspace`) && /[#$]$/.test(lastLine)) ||
                /^\d{1,2}:\d{2} [#$]$/.test(lastLine);
        },
        SCREENSHOT_WORKSPACE_USER,
        { timeout: seconds * 1000 },
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
    const input = quickInput(workspace);
    const basename = path.posix.basename(relativePath);
    const parent = path.posix.basename(path.posix.dirname(relativePath));

    // Quick Open knows a file a recipe just wrote only once VS Code's file
    // index has caught up, which takes from no time to several seconds. Ask
    // again until it lists the file, as a person would.
    const deadline = Date.now() + 30_000;
    for (;;) {
        await workspace.keyboard.press('Control+P');
        await input.waitFor({ state: 'visible', timeout: 10_000 });
        await input.fill(relativePath);
        const found = await workspace.waitForFunction(
            ({ basename, parent }) =>
                [...document.querySelectorAll(
                    '.quick-input-list .monaco-list-row'
                )].some(row => {
                    const text = row.innerText || '';
                    return text.includes(basename) &&
                        (parent === '.' || text.includes(parent));
                }),
            { basename, parent },
            { timeout: 3_000 },
        ).then(() => true, () => false);
        if (found) break;
        await input.press('Escape');
        await input.waitFor({ state: 'hidden', timeout: 10_000 });
        if (Date.now() > deadline) {
            throw new Error(`open-file: Quick Open does not list ${relativePath}`);
        }
        await workspace.waitForTimeout(1_000);
    }

    await input.press('Enter');
    await input.waitFor({ state: 'hidden', timeout: 10_000 });

    await workspace.waitForFunction(
        name =>
            [...document.querySelectorAll('.tabs-container .tab')]
                .some(tab => (tab.querySelector('.monaco-icon-label-container') ?? tab.querySelector('.label-name'))
                    ?.textContent?.trim() === name),
        basename,
        { timeout: 20_000 },
    );

    if (IMAGE_FILE.test(basename)) await waitForImagePreview(workspace);
    else await waitForEditorSettled(workspace);
}

// VS Code shows a picture in its image preview, a webview, not in a text
// editor. Its content lives in an iframe of another origin, so wait for the
// visible webview to be ready and give the picture a moment to paint.
const IMAGE_FILE = /\.(png|jpe?g|gif|webp|bmp|ico)$/i;

async function waitForImagePreview(page) {
    await page.waitForFunction(
        () => [...document.querySelectorAll('iframe.webview')].some(frame => {
            const rect = frame.getBoundingClientRect();
            return rect.width > 0 && rect.height > 0 && frame.classList.contains('ready');
        }),
        null,
        { timeout: 20_000 },
    );
    await page.waitForTimeout(1_500);
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

async function openPreviewPort(port, hooks, path = '/') {
    // Workspace's /proxy/<port>/ endpoint redirects to its authenticated
    // <server-tag>-<port> subdomain, keeping the path. This also reaches
    // loopback-only servers.
    const url = new URL(`/proxy/${port}${path}`, workspace.url()).href;
    if (preview && !preview.isClosed()) await preview.close();
    preview = await context.newPage();
    const deadline = Date.now() + 180_000;
    let lastError = 'application not ready';
    while (Date.now() < deadline) {
        try {
            const response = await preview.goto(url, {
                waitUntil: 'domcontentloaded',
                timeout: 15_000,
            });
            if (response?.ok()) {
                await waitForPreviewReady(preview, hooks);
                return;
            }
            lastError = `HTTP ${response?.status() ?? 'no response'}`;
            if (response?.status() === 403) break;
        } catch (error) {
            lastError = error.message;
        }
        await sleep(2);
    }
    throw new Error(`Preview on port ${port} unavailable: ${lastError}`);
}

// The screenshot account's Neo4j Browser as the preview tab, opened like the
// »Neo4j« button in the menu (/api/start_neo4j) and logged in with the
// username and password its profile page shows, as a student would.
async function openNeo4jBrowser() {
    const start = jsonResponse(await browserRequest('/api/start_neo4j'), 'Neo4j Browser start');
    if (!start.browser_url) throw new Error('/api/start_neo4j returned no browser_url');

    const profile = await browserRequest('/profil', { method: 'GET', body: null });
    if (profile.status !== 200) throw new Error(`Profile page returned HTTP ${profile.status}`);
    const credentials = await dashboard.evaluate(html => {
        const page = new DOMParser().parseFromString(html, 'text/html');
        return {
            username: page.querySelector('#neo4j_username')?.value || '',
            password: page.querySelector('#neo4j_password')?.value || '',
        };
    }, profile.body);
    if (!credentials.username || !credentials.password) {
        throw new Error('The profile page shows no Neo4j username and password');
    }

    if (preview && !preview.isClosed()) await preview.close();
    preview = await context.newPage();
    const response = await preview.goto(start.browser_url, {
        waitUntil: 'domcontentloaded', timeout: 60_000,
    });
    if (!response?.ok()) throw new Error(`Neo4j Browser returned HTTP ${response?.status()}`);

    const username = preview.locator('input[name=username]');
    await username.waitFor({ state: 'visible', timeout: 60_000 });
    await username.fill(credentials.username);
    await preview.locator('input[name=password]').fill(credentials.password);
    await preview.getByRole('button', { name: 'Connect', exact: true }).click();
    await preview.locator('input[name=password]').waitFor({ state: 'hidden', timeout: 30_000 });
    await preview.locator('[data-testid=main-editor] .cm-content').waitFor({ state: 'visible', timeout: 30_000 });
}

// A page of the Workspace site itself (TIC-80 at /tic80/, say) as the
// preview tab, logged in as the screenshot account like the dashboard.
async function openSitePage(pathname, hooks) {
    if (preview && !preview.isClosed()) await preview.close();
    preview = await context.newPage();
    const response = await preview.goto(new URL(pathname, BASE_URL).href, {
        waitUntil: 'domcontentloaded', timeout: 60_000,
    });
    if (!response?.ok()) throw new Error(`${pathname} returned HTTP ${response?.status()}`);
    await waitForPreviewReady(preview, hooks);
}

async function selectOption(action, targetTab) {
    const page = requirePageForTab(targetTab, 'select-option');
    const select = page.locator(action.selector);
    await select.waitFor({ state: 'visible', timeout: ACTION_TIMEOUT_MS });
    const count = await select.count();
    if (count !== 1) throw new Error(`select-option expected one ${action.selector}; found ${count}`);
    await select.selectOption(action.value, { timeout: ACTION_TIMEOUT_MS });
    await page.waitForTimeout(150);
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
        // Live Server reloads the page by itself when a file changes, now
        // and then exactly while this reload runs; the two navigations get
        // in each other's way. Then wait for the page and reload once more.
        for (let attempt = 1; ; attempt += 1) {
            try {
                await preview.reload({
                    waitUntil: 'domcontentloaded',
                    timeout: 60_000,
                });
                break;
            } catch (error) {
                const raced = /Not attached to an active page|Execution context was destroyed|interrupted by another navigation|frame was detached/i
                    .test(error.message);
                if (!raced) throw error;
                if (attempt === 3) {
                    // Still no page: the tab itself is gone (after many 3D
                    // scenes in a row, for example). A new one at the same
                    // address is what a reload would have shown.
                    // If the browser's GPU process is the reason, a new tab
                    // can wait for it forever; then the shot fails instead.
                    const url = preview.url();
                    console.log(`  the preview tab is gone; opening ${url} in a new one`);
                    await withTimeout(preview.close(), 10_000, 'closing the preview tab').catch(() => {});
                    preview = await withTimeout(context.newPage(), 60_000, 'opening a new preview tab');
                    await preview.goto(url, { waitUntil: 'domcontentloaded', timeout: 60_000 });
                    await withTimeout(waitForPreviewReady(preview, hooks), 120_000, 'the new preview tab getting ready');
                    return;
                }
                console.log(`  preview-reload crossed a reload of the page itself; again (${attempt})`);
                await preview.waitForLoadState('domcontentloaded', { timeout: 60_000 }).catch(() => {});
                await preview.waitForTimeout(1000);
            }
        }
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

// While a <!-- tutorial-scope --> runs, every file write-file touches is
// noted with what it held before (null: it was not there), so the end of the
// scope can put it back (restoreScopeFiles).
let scopeJournal = null;

async function restoreScopeFiles() {
    const journal = scopeJournal;
    scopeJournal = null;
    if (!journal) return;
    for (const [file, before] of [...journal.entries()].reverse()) {
        if (before === null) {
            await fs.rm(file, { force: true });
        } else {
            await fs.writeFile(file, before);
            await fs.chown(file, 1000, 1000).catch(() => {});
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
    if (scopeJournal && !scopeJournal.has(resolvedTarget)) {
        scopeJournal.set(resolvedTarget, await fs.readFile(resolvedTarget).catch(() => null));
    }
    // Directories created here belong to the Workspace user, like the file:
    // owned by root, Git and the editor could not write into them.
    const firstCreated = await fs.mkdir(path.dirname(target), { recursive: true });
    if (firstCreated) {
        for (let directory = path.dirname(target); ; directory = path.dirname(directory)) {
            await fs.chown(directory, 1000, 1000).catch(() => {});
            if (directory === firstCreated || directory === path.dirname(directory)) break;
        }
    }
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
        // The whole label: VS Code 1.140 puts the extension of a file name
        // into an element of its own (label-name »hello«, label-suffix ».txt«).
        const name = await tab.locator('.monaco-icon-label-container, .label-name').first()
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
    // The next tab slides under the pointer, and its close button would
    // show »Close (Ctrl+W)« in the picture: the pointer leaves the window.
    await workspace.mouse.move(-1, -1);

    await workspace.waitForFunction(
        expected =>
            ![...document.querySelectorAll('.tabs-container .tab')]
                .some(tab => (tab.querySelector('.monaco-icon-label-container') ?? tab.querySelector('.label-name'))
                    ?.textContent?.trim() === expected),
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

// byText: the text as it stands (move-mouse), not first a button or link of
// that name (click, hold)
async function pageControl(action, verb, targetTab, { byText = false } = {}) {
    const page = requirePageForTab(targetTab, verb);

    const description = action.selector
        ? `element matching selector ${JSON.stringify(action.selector)}`
        : `control named ${JSON.stringify(action.text)}`;
    const locate = async (frame) => {
        if (action.selector) return frame.locator(action.selector);
        if (byText) return frame.getByText(action.text, { exact: true });
        const buttons = frame.getByRole('button', { name: action.text, exact: true });
        const links = frame.getByRole('link', { name: action.text, exact: true });
        const named = buttons.or(links);
        return await named.count() > 0 ? named : frame.getByText(action.text, { exact: true });
    };

    // The page itself first, then its frames: VS Code's webviews (the
    // ER-Werkstatt, the Markdown preview) are iframes, as for wait-for-text.
    let control = await locate(page);
    const deadline = Date.now() + ACTION_TIMEOUT_MS;
    while (!(await control.count() > 0 && await control.first().isVisible())) {
        let found = null;
        for (const frame of page.frames()) {
            if (frame === page.mainFrame()) continue;
            const candidate = await locate(frame).catch(() => null);
            if (candidate && await candidate.count().catch(() => 0) > 0 &&
                await candidate.first().isVisible().catch(() => false)) {
                found = candidate;
                break;
            }
        }
        if (found) {
            control = found;
            break;
        }
        if (Date.now() > deadline) {
            await control.first().waitFor({ state: 'visible', timeout: 1 });
        }
        await page.waitForTimeout(250);
        control = await locate(page);
    }
    const count = await control.count();
    if (count !== 1) {
        throw new Error(`${verb} expected exactly one ${description}, found ${count}`);
    }
    return { page, control };
}

async function clickControl(action, targetTab) {
    const { page, control } = await pageControl(action, 'click', targetTab);
    // Playwright clicks the middle of the element; it may be gone afterwards.
    await control.scrollIntoViewIfNeeded({ timeout: ACTION_TIMEOUT_MS });
    const box = await control.boundingBox();
    await control.click({ timeout: ACTION_TIMEOUT_MS });
    if (box) rememberMousePosition(page, box);
    await page.waitForTimeout(150);
}

async function holdControl(action, targetTab) {
    const { page, control } = await pageControl(action, 'hold', targetTab);
    await control.hover({ timeout: ACTION_TIMEOUT_MS });
    const box = await control.boundingBox();
    if (box) rememberMousePosition(page, box);
    await page.mouse.down();
    try {
        await page.waitForTimeout(action.seconds * 1000);
    } finally {
        await page.mouse.up();
    }
    await page.waitForTimeout(150);
}

// The text of a write-file style source, without its final line break: typed
// in one piece, the editor takes it as it is (no auto-indent, no closing
// brackets added), and the cursor ends after the last line.
async function sourceText(contents) {
    let text = contents;
    if (contents && typeof contents === 'object' && contents.sourcePath) {
        const bytes = await fs.readFile(contents.sourcePath);
        if (sha256(bytes) !== contents.sourceSha256) {
            throw new Error(`type source changed while generating screenshots: ${contents.sourcePath}`);
        }
        text = bytes.toString('utf8');
    }
    return text.replace(/\n$/, '');
}

// A whole text as a paste into the focused control: the editor takes a paste
// as it is, while typed text gets closing brackets and indentation added.
async function pasteText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'type');
    await page.waitForFunction(
        () => document.activeElement?.closest?.('.monaco-editor') != null ||
            document.activeElement instanceof HTMLTextAreaElement,
        null,
        { timeout: 10_000 },
    );
    await page.evaluate((value) => {
        const data = new DataTransfer();
        data.setData('text/plain', value);
        document.activeElement.dispatchEvent(new ClipboardEvent('paste', {
            clipboardData: data, bubbles: true, cancelable: true,
        }));
    }, text);
    await page.waitForTimeout(150);
}

async function typeText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'type');
    await page.waitForFunction(
        () => {
            const element = document.activeElement;
            // Monaco (also small ones like the Extensions search) takes text
            // through a div with the EditContext API, not a <textarea>.
            // A webview (an extension's page) is an iframe: the keys go to
            // the field focused inside it.
            return element instanceof HTMLInputElement ||
                element instanceof HTMLTextAreaElement ||
                element instanceof HTMLIFrameElement ||
                element?.isContentEditable === true ||
                element?.closest?.('.monaco-editor') != null;
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
    // Control+L clears a terminal later than the key returns: a command
    // typed right after it was sometimes cleared with it (»ls -l« without
    // the nasm line above). Wait until the focused terminal shows at most
    // its prompt, two seconds at most.
    if (/^control\+l$/i.test(key)) await waitForClearedTerminal(page);
}

async function waitForClearedTerminal(page) {
    await page.waitForFunction(() => {
        const active = document.activeElement;
        const terminal = active && active.closest ? active.closest('.terminal.xterm') : null;
        if (!terminal) return true;
        const rows = [...terminal.querySelectorAll('.xterm-rows > div')]
            .filter(row => (row.textContent || '').trim());
        return rows.length <= 1;
    }, null, { timeout: 2000 }).catch(() => {});
}

async function moveMouse(action, targetTab) {
    // on the page, or like a click in a webview (an iframe)
    const { page, control } = await pageControl(action, 'move-mouse', targetTab, { byText: true });
    await control.scrollIntoViewIfNeeded({ timeout: ACTION_TIMEOUT_MS });
    const box = await control.boundingBox();
    if (!box) throw new Error(`move-mouse could not measure ${action.selector || action.text}`);
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    rememberMousePosition(page, box);
    await page.waitForTimeout(150);
}

function rememberMousePosition(page, box) {
    mousePosition = { page, x: box.x + box.width / 2, y: box.y + box.height / 2 };
}

// The pointer's shape follows the CSS cursor under it, as on a real screen:
// a hand over links and buttons, an I-beam over text input, else the arrow.
// show-mouse: which pointer the screen would show where the mouse is, from
// the CSS cursor under it (a hand over buttons and links, an I-beam over
// text, otherwise the arrow).
async function mousePointerShape(page) {
    if (!mousePosition || mousePosition.page !== page) {
        throw new Error(
            'show-mouse needs move-mouse, click or hold on this tab after the last ' +
            'action that moves the mouse itself',
        );
    }
    const cursor = await page.evaluate(({ x, y }) => {
        for (let element = document.elementFromPoint(x, y); element; element = element.parentElement) {
            const value = getComputedStyle(element).cursor;
            if (value !== 'auto') return value;
        }
        return 'auto';
    }, { x: mousePosition.x, y: mousePosition.y });
    return MOUSE_POINTERS[cursor] ? cursor : 'arrow';
}

// The pointer is drawn into the picture afterwards: rendered on its own,
// with its shadow, on a transparent background at the picture's scale.
// Where it reaches past an edge, the picture grows with transparent
// pixels, so the pointer is never cut off. clip: the captured area in CSS
// pixels; width, height: the picture in pixels.
// Screenshots stay lossless. A picture that is still over 800 KB that way is
// full of fine noise (PixelRAM's CRT scanlines, smooth gradients); readers
// would load megabytes for it, so it is encoded again at quality 90 and
// the smaller file is kept.
const LOSSLESS_LIMIT_BYTES = 800 * 1024;

async function shrinkNoisyWebp(pngPath, webpPath) {
    const lossless = (await fs.stat(webpPath)).size;
    if (lossless <= LOSSLESS_LIMIT_BYTES) return;
    const lossyPath = `${webpPath}.lossy`;
    try {
        await execFileAsync('cwebp', ['-quiet', '-q', '90', pngPath, '-o', lossyPath]);
        const lossy = (await fs.stat(lossyPath)).size;
        if (lossy < lossless) {
            await fs.rename(lossyPath, webpPath);
            console.log(`  ${Math.round(lossless / 1024)} KB lossless, ${Math.round(lossy / 1024)} KB at quality 90`);
        }
    } finally {
        await fs.rm(lossyPath, { force: true });
    }
}

async function addMousePointer(pngPath, { clip, width, height, shape }) {
    const pointer = MOUSE_POINTERS[shape];
    const scale = width / clip.width;
    const pad = 4;
    const box = 24 + 2 * pad;
    const pointerPath = `${pngPath}.pointer.png`;
    const page = await context.browser().newPage({
        viewport: { width: box, height: box },
        deviceScaleFactor: scale,
    });
    try {
        await page.setContent(`<html><body style="margin: 0; background: transparent;">
            <div style="position: absolute; left: ${pad}px; top: ${pad}px; width: 24px; height: 24px;
                filter: drop-shadow(0 1px 1.5px rgba(0, 0, 0, 0.35));">
            <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24">${pointer.svg}</svg>
            </div></body></html>`);
        await page.screenshot({ path: pointerPath, omitBackground: true });
    } finally {
        await page.close();
    }
    const size = Math.round(box * scale);
    const left = Math.round((mousePosition.x - pointer.hotspot[0] - pad - clip.x) * scale);
    const top = Math.round((mousePosition.y - pointer.hotspot[1] - pad - clip.y) * scale);
    const growLeft = Math.max(0, -left);
    const growTop = Math.max(0, -top);
    const newWidth = Math.max(width, left + size) + growLeft;
    const newHeight = Math.max(height, top + size) + growTop;
    try {
        await execFileAsync('convert', [
            pngPath, '-background', 'none', '-gravity', 'NorthWest',
            '-extent', `${newWidth}x${newHeight}-${growLeft}-${growTop}`,
            pointerPath, '-geometry', `+${left + growLeft}+${top + growTop}`, '-composite',
            // Only as much transparent space as the pointer and its shadow
            // need: the border makes the corners transparent, so -trim cuts
            // every fully transparent row and column at the edges.
            '-bordercolor', 'none', '-border', '1', '-trim', '+repage',
            `PNG32:${pngPath}`,
        ]);
    } finally {
        await fs.rm(pointerPath, { force: true }).catch(() => {});
    }
}

// Frames count too: VS Code shows webviews (the ER-Werkstatt, the Markdown
// preview) in iframes, whose text the page's own body does not contain.
async function waitForText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'wait-for-text');
    const containsText = expected => {
        const normalize = value => `${value || ''}`.replace(/\s+/g, ' ').trim();
        return normalize(document.body?.innerText).includes(normalize(expected));
    };

    const deadline = Date.now() + WAIT_FOR_TEXT_TIMEOUT_MS;
    for (;;) {
        for (const frame of page.frames()) {
            // A frame may go away while it is being read.
            if (await frame.evaluate(containsText, text).catch(() => false)) return;
        }
        if (Date.now() > deadline) {
            throw new Error(`wait-for-text: ${JSON.stringify(text)} did not appear within ${WAIT_FOR_TEXT_TIMEOUT_MS / 1000} s`);
        }
        await page.waitForTimeout(200);
    }
}

// The text must not be on the page now, in any frame: an error message, a
// Traceback, a file that should not be there.
async function expectNoText(text, targetTab) {
    const page = requirePageForTab(targetTab, 'expect-no-text');
    const containsText = expected => {
        const normalize = value => `${value || ''}`.replace(/\s+/g, ' ').trim();
        return normalize(document.body?.innerText).includes(normalize(expected));
    };
    for (const frame of page.frames()) {
        if (await frame.evaluate(containsText, text).catch(() => false)) {
            throw new Error(`expect-no-text: ${JSON.stringify(text)} is on the page`);
        }
    }
}

// check-task: ID [LANGUAGE] [<- ANSWER] runs the task's check the way the
// »Prüfen« button does, with ANSWER in the card's field, as the screenshot
// user against their Workspace, and fails unless it passes;
// check-task-fails expects it to reject. Like »Prüfen«, it records the
// result for that user.
async function checkTask(action, shouldPass) {
    const key = action.task;
    // From the website's bootstrap page, with the screenshot user's session
    // (browserRequest): Node in this container cannot reach the site's host.
    const result = jsonResponse(await browserRequest('/api/task/check', {
        body: { task: key, answer: action.answer || '', ...(action.language ? { language: action.language } : {}) },
    }), `check-task ${key}`);
    const message = `${result.message || ''}`.replace(/<[^>]+>/g, '').replace(/\s+/g, ' ').trim();
    const passed = result.status === 'correct';
    const rejected = result.status === 'wrong' || result.status === 'invalid';
    if (shouldPass && !passed) throw new Error(`check-task ${key}: ${result.status}: ${message}`);
    if (!shouldPass && !rejected) throw new Error(`check-task-fails ${key}: expected a rejection, got ${result.status}: ${message}`);
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
        { timeout: WAIT_FOR_TEXT_TIMEOUT_MS },
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

    await applyViewportProfile(page, shot.viewport, shot.zoom, shot.sharpCanvas);
    return true;
}

async function executeAction(action, targetTab, hooks) {
    if (!MOUSE_NEUTRAL_ACTIONS.has(action.type)) mousePosition = null;
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
        case 'terminal-wait-for-prompt': return waitForTerminalPrompt(action.seconds);
        case 'terminal-maximize': return terminalMaximize();
        case 'terminal-run': return terminalRun(action.text);
        case 'go-live': return goLive(hooks);
        case 'preview-port': return openPreviewPort(action.port, hooks, action.path);
        case 'open-site': return openSitePage(action.path, hooks);
        case 'neo4j-browser': return openNeo4jBrowser();
        case 'select-option': return selectOption(action, targetTab);
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
        case 'type-source': return pasteText(await sourceText(action.contents), targetTab);
        case 'press': return pressKey(action.key, targetTab);
        case 'sleep': return sleep(action.seconds);
        case 'hook': return runHookAction(action, targetTab, hooks);
        case 'wait-for-text': return waitForText(action.text, targetTab);
        case 'expect-no-text': return expectNoText(action.text, targetTab);
        case 'terminal-expect-no-text': return terminalExpectNoText(action.text);
        case 'check-task': return checkTask(action, true);
        case 'check-task-fails': return checkTask(action, false);
        case 'wait-for-input-value': return waitForInputValue(action.text, targetTab);
    default: throw new Error(`Unsupported action: ${action.type}`);
    }
}

async function applyViewportProfile(page, viewport, zoom, sharpCanvas = false) {
    let session = deviceMetricSessions.get(page);
    if (!session) {
        session = await context.newCDPSession(page);
        deviceMetricSessions.set(page, session);
    }

    // Match the measured manual setup: Linux desktop scale and Chrome page zoom
    // are separate factors. Their product is the number of output pixels per CSS
    // pixel in the final screenshot.
    // A preview tab (a web app, the Neo4j Browser, the Game Studio) renders at
    // the capture resolution and is captured unscaled, so a <canvas> draws
    // sharp instead of being enlarged. The Workspace renders at its layout
    // scale and the capture enlarges it: its screenshots are DOM text, sharp
    // either way, and stay exactly as they were. A shot with sharp-canvas
    // renders the Workspace at capture resolution too, for a <canvas> inside
    // it such as the PDF in LaTeX Workshop's viewer.
    const renderScale = page === workspace && !sharpCanvas ? 1 : DEFAULT_PROFILE.captureScale;
    const metrics = {
        ...screenshotMetricsFor(viewport, zoom),
        captureScale: DEFAULT_PROFILE.captureScale / renderScale,
    };

    const profileKey = [
        metrics.width,
        metrics.height,
        metrics.deviceScaleFactor,
        renderScale,
    ].join(':');
    if (appliedViewportProfiles.get(page) === profileKey) {
        return { session, metrics };
    }

    await session.send('Emulation.setDeviceMetricsOverride', {
        width: metrics.width,
        height: metrics.height,
        deviceScaleFactor: metrics.deviceScaleFactor * renderScale,
        mobile: false,
        screenWidth: metrics.width,
        screenHeight: metrics.height,
    });

    // Give VS Code two frames to react to the changed layout metrics.
    await page.evaluate(() => new Promise(resolve => {
        requestAnimationFrame(() => requestAnimationFrame(resolve));
    }));

    // A preview page sized its <canvas> for the size and pixel ratio it
    // loaded with; the changed metrics alone fire no resize here, so tell it
    // once they are in the layout, and it redraws sharp at the capture size.
    if (page !== workspace) {
        await page.evaluate(() => new Promise(resolve => {
            window.dispatchEvent(new Event('resize'));
            requestAnimationFrame(() => requestAnimationFrame(resolve));
        })).catch(() => {});
    }

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

async function waitForWorkspaceFont(page) {
    const loadedFaceCount = await page.evaluate(async family => {
        const faces = await document.fonts.load(`16px "${family}"`);
        await document.fonts.ready;
        await new Promise(resolve => {
            requestAnimationFrame(() => requestAnimationFrame(resolve));
        });
        return faces.length;
    }, WORKSPACE_FONT_FAMILY);

    if (loadedFaceCount === 0) {
        throw new Error(
            `Workspace webfont is not registered: ${WORKSPACE_FONT_FAMILY}`,
        );
    }
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

async function scrollVisibleTerminalBack(page, lineCount) {
    await focusTerminal();

    for (let index = 0; index < lineCount; index += 1) {
        await page.mouse.wheel(0, -1);
        await page.waitForTimeout(20);
    }

    // Give xterm a couple of frames to repaint the scrolled buffer.
    await page.evaluate(() => new Promise(resolve => {
        requestAnimationFrame(() => requestAnimationFrame(resolve));
    }));

    // We only need to know how many lines to undo afterwards.
    return lineCount;
}

async function restoreVisibleTerminalScroll(page, lineCount) {
    if (!lineCount) return;

    await focusTerminal();

    for (let index = 0; index < lineCount; index += 1) {
        await page.mouse.wheel(0, 1);
        await page.waitForTimeout(20);
    }

    await page.evaluate(() => new Promise(resolve => {
        requestAnimationFrame(() => requestAnimationFrame(resolve));
    }));
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

// The boxes of the elements keep-only names, in CSS pixels of the page,
// with their corner radius. An element without a size of its own that draws
// with ::after (the little triangle of a VS Code tooltip, a rotated square)
// counts as that square: a diamond.
async function keptBoxes(page, selectors) {
    const boxes = [];
    for (const selector of selectors) {
        const element = page.locator(selector).first();
        await element.waitFor({ state: 'attached', timeout: ACTION_TIMEOUT_MS });
        const box = await element.evaluate((node) => {
            const rect = node.getBoundingClientRect();
            const x = rect.left + window.scrollX;
            const y = rect.top + window.scrollY;
            if (rect.width > 0 && rect.height > 0) {
                const radius = parseFloat(getComputedStyle(node).borderTopLeftRadius) || 0;
                return { x, y, width: rect.width, height: rect.height, radius };
            }
            const after = getComputedStyle(node, '::after');
            const px = (v) => parseFloat(v) || 0;
            const w = px(after.width) + px(after.borderLeftWidth) + px(after.borderRightWidth);
            const h = px(after.height) + px(after.borderTopWidth) + px(after.borderBottomWidth);
            if (!(w > 0 && h > 0)) return null;
            const left = after.left === 'auto' ? 0 : px(after.left);
            const top = after.top === 'auto' ? 0 : px(after.top);
            return { diamond: true, cx: x + left + w / 2, cy: y + top + h / 2, half: Math.hypot(w, h) / 2 };
        });
        if (!box) throw new Error(`keep-only: ${selector} has no size`);
        boxes.push(box);
    }
    return boxes;
}

// Makes everything outside the boxes transparent. clip: where the picture
// starts on the page (CSS pixels) and how many picture pixels one CSS pixel is.
async function keepOnlyBoxes(pngPath, boxes, clip) {
    // ImageMagick draws no rounded rectangle that reaches outside the
    // picture: clamp each box, and round only corners that are inside.
    const clamp = (v, max) => Math.min(Math.max(v, 0), max);
    const draws = boxes.map((b) => {
        if (b.diamond) {
            const cx = (b.cx - clip.x) * clip.scale;
            const cy = (b.cy - clip.y) * clip.scale;
            const h = b.half * clip.scale + 0.5;
            const points = [[cx, cy - h], [cx + h, cy], [cx, cy + h], [cx - h, cy]]
                .map(([x, y]) => `${Math.round(x)},${Math.round(y)}`).join(' ');
            return ['-draw', `polygon ${points}`];
        }
        const rx0 = Math.round((b.x - clip.x) * clip.scale);
        const ry0 = Math.round((b.y - clip.y) * clip.scale);
        const rx1 = Math.round((b.x + b.width - clip.x) * clip.scale) - 1;
        const ry1 = Math.round((b.y + b.height - clip.y) * clip.scale) - 1;
        const x0 = clamp(rx0, clip.width - 1);
        const y0 = clamp(ry0, clip.height - 1);
        const x1 = clamp(rx1, clip.width - 1);
        const y1 = clamp(ry1, clip.height - 1);
        const r = Math.round(b.radius * clip.scale);
        const inside = x0 === rx0 && y0 === ry0 && x1 === rx1 && y1 === ry1;
        return ['-draw', r > 0 && inside ? `roundrectangle ${x0},${y0} ${x1},${y1} ${r},${r}` : `rectangle ${x0},${y0} ${x1},${y1}`];
    }).flat();
    await execFileAsync('convert', [
        pngPath,
        '(', '-size', `${clip.width}x${clip.height}`, 'xc:black', '-fill', 'white', ...draws, ')',
        '-alpha', 'off', '-compose', 'CopyOpacity', '-composite', '+repage',
        pngPath,
    ]);
}

// beside: capture the other tab with the shot's viewport and zoom and put it
// to the right of the picture in pngPath
async function appendTabCapture(pngPath, shot) {
    const other = pageForTab(shot.beside);
    if (!other || other.isClosed()) {
        throw new Error(`beside: ${shot.beside} is not open`);
    }
    const { session, metrics } = await applyViewportProfile(other, shot.viewport, shot.zoom, shot.sharpCanvas);
    if (shot.beside === 'workspace') await installWorkspaceScreenshotStyle(other);
    await other.waitForTimeout(300);
    const captured = await session.send('Page.captureScreenshot', {
        format: 'png',
        fromSurface: true,
        captureBeyondViewport: false,
        clip: {
            x: 0,
            y: 0,
            width: shot.viewport.width / metrics.deviceScaleFactor,
            height: shot.viewport.height / metrics.deviceScaleFactor,
            scale: metrics.captureScale,
        },
    });
    const besidePath = `${pngPath}.beside.png`;
    try {
        await fs.writeFile(besidePath, Buffer.from(captured.data, 'base64'));
        await execFileAsync('convert', [pngPath, besidePath, '+append', pngPath]);
    } finally {
        await fs.rm(besidePath, { force: true }).catch(() => {});
    }
}

async function captureShot(shot, tutorialDirectory) {
    const page = pageForTab(shot.tab);
    if (!page || page.isClosed()) {
        throw new Error(`Screenshot requests unavailable tab: ${shot.tab}`);
    }

    const { session, metrics } = await applyViewportProfile(
        page,
        shot.viewport,
        shot.zoom,
        shot.sharpCanvas,
    );

    await page.evaluate(() => window.scrollTo(0, 0)).catch(() => {});

    if (shot.tab === 'workspace') {
        await waitForWorkspaceFont(page);
    }

    let originalTerminalScrollTop = null;

    try {
        // terminal-scroll-back affects only this screenshot. Remember the current
        // xterm scroll position so it can be restored afterwards.
        if (
            shot.tab === 'workspace' &&
            (shot.terminalScrollBack || 0) > 0
        ) {
            originalTerminalScrollTop = await scrollVisibleTerminalBack(
                page,
                shot.terminalScrollBack,
            );
        }

        await page.waitForTimeout(100);

        // Recipes express viewport and crop values in final screenshot pixels. The
        // screenshot clip itself is in CSS pixels, so divide by the emulated zoom.
        // Fractional clip dimensions are intentional: at 1853x929 / 150% they make
        // Playwright emit exactly 1853x929 device pixels instead of 1854x930.
        let topPixels = Math.round(
            shot.viewport.height * shot.cropTop,
        );
        let bottomPixels = Math.round(
            shot.viewport.height * shot.cropBottom,
        );
        let heightPixels =
            shot.viewport.height - topPixels - bottomPixels;
        let leftPixels = Math.round(shot.viewport.width * (shot.cropLeft || 0));
        let widthPixels = shot.viewport.width - leftPixels -
            Math.round(shot.viewport.width * (shot.cropRight || 0));

        // crop-element: the picture is exactly that element's box, or the box
        // around the elements of several lines.
        if (shot.cropElement) {
            const boxes = [];
            for (const selector of [shot.cropElement].flat()) {
                const element = page.locator(selector).first();
                await element.waitFor({ state: 'visible', timeout: ACTION_TIMEOUT_MS });
                // The screenshot is of the viewport, so the element must be in it.
                // boundingBox is relative to the viewport, the clip of
                // Page.captureScreenshot to the document: add the scroll offset.
                await element.scrollIntoViewIfNeeded();
                const box = await element.boundingBox();
                if (!box) throw new Error(`crop-element could not measure ${selector}`);
                boxes.push(box);
            }
            const left = Math.min(...boxes.map((box) => box.x));
            const top = Math.min(...boxes.map((box) => box.y));
            const right = Math.max(...boxes.map((box) => box.x + box.width));
            const bottom = Math.max(...boxes.map((box) => box.y + box.height));
            const scroll = await page.evaluate(() => [window.scrollX, window.scrollY]);
            leftPixels = (left + scroll[0]) * metrics.deviceScaleFactor;
            topPixels = (top + scroll[1]) * metrics.deviceScaleFactor;
            widthPixels = (right - left) * metrics.deviceScaleFactor;
            heightPixels = (bottom - top) * metrics.deviceScaleFactor;
        }

        // Important: measure the terminal only AFTER terminal-scroll-back has
        // changed the visible xterm rows.
        if (shot.cropTerminalLines != null) {
            const geometry = await visibleTerminalCropGeometry(page);
            const crop = terminalLineCropPixels({
                viewportHeight: shot.viewport.height,
                deviceScaleFactor: metrics.deviceScaleFactor,
                ...geometry,
                lineCount: shot.cropTerminalLines,
                skipTop: shot.cropTerminalSkipTop || 0,
                skipBottom: shot.cropTerminalSkipBottom || 0,
            });

            ({
                topPixels,
                bottomPixels,
                heightPixels,
            } = crop);
        }

        const pngPath =
            `/tmp/tutorial-shot-${process.pid}-` +
            `${crypto.randomBytes(6).toString('hex')}.png`;

        const target = path.join(
            tutorialDirectory,
            shot.target,
        );

        const temporaryWebp =
            `${target}.tmp-${process.pid}-${Date.now()}`;

        await fs.mkdir(
            path.dirname(target),
            { recursive: true },
        );

        try {
            if (shot.tab === 'workspace') {
                await installWorkspaceScreenshotStyle(page);
            }
            const pointerShape = shot.showMouse ? await mousePointerShape(page) : null;
            const kept = shot.keepOnly ? await keptBoxes(page, shot.keepOnly) : null;

            // Capture through CDP directly. The page keeps its viewport/zoom
            // layout, while clip.scale asks Chromium to render a larger native
            // snapshot without changing the page's persistent device metrics.
            const captured = await session.send(
                'Page.captureScreenshot',
                {
                    format: 'png',
                    fromSurface: true,
                    captureBeyondViewport: false,
                    clip: {
                        x: leftPixels / metrics.deviceScaleFactor,
                        y: topPixels / metrics.deviceScaleFactor,
                        width:
                            widthPixels /
                            metrics.deviceScaleFactor,
                        height:
                            heightPixels /
                            metrics.deviceScaleFactor,
                        scale: metrics.captureScale,
                    },
                },
            );

            const png = Buffer.from(
                captured.data,
                'base64',
            );

            const actual = pngDimensions(png);
            const expected = scalePixelDimensions(
                widthPixels,
                heightPixels,
                DEFAULT_PROFILE.captureScale,
            );
            await fs.writeFile(pngPath, png);
            // keep-only: everything but these elements transparent, before
            // the pointer, which may stand outside them
            if (kept) {
                await keepOnlyBoxes(pngPath, kept, {
                    x: leftPixels / metrics.deviceScaleFactor,
                    y: topPixels / metrics.deviceScaleFactor,
                    scale: actual.width / (widthPixels / metrics.deviceScaleFactor),
                    width: actual.width,
                    height: actual.height,
                });
            }
            if (pointerShape) {
                await addMousePointer(pngPath, {
                    clip: { x: leftPixels / metrics.deviceScaleFactor, y: topPixels / metrics.deviceScaleFactor,
                        width: widthPixels / metrics.deviceScaleFactor },
                    width: actual.width,
                    height: actual.height,
                    shape: pointerShape,
                });
            }

            // beside: the other tab at the same size, joined on the right
            // (VS Code and the preview next to each other, as two windows)
            if (shot.beside) {
                await appendTabCapture(pngPath, shot);
            }

            const cwebpArgs = [
                '-quiet',
                '-lossless',
            ];

            if (
                actual.width !== expected.width ||
                actual.height !== expected.height
            ) {
                console.log(
                    `Chromium screenshot was ` +
                    `${actual.width}x${actual.height}; ` +
                    `expected ` +
                    `${expected.width}x${expected.height}; ` +
                    `keeping native pixels without resampling`,
                );
            }

            cwebpArgs.push(
                pngPath,
                '-o',
                temporaryWebp,
            );

            await execFileAsync(
                'cwebp',
                cwebpArgs,
            );
            await shrinkNoisyWebp(pngPath, temporaryWebp);

            await fs.chmod(
                temporaryWebp,
                0o644,
            );

            await fs.rename(
                temporaryWebp,
                target,
            );

            await inheritDirectoryOwnership(
                target,
                tutorialDirectory,
            );
        } finally {
            await fs.rm(
                pngPath,
                { force: true },
            ).catch(() => {});

            await fs.rm(
                temporaryWebp,
                { force: true },
            ).catch(() => {});
        }

        return sha256(
            await fs.readFile(target),
        );
    } finally {
        // Never let an artificial screenshot scroll position leak into the
        // following recipe, even if capture/conversion failed.
        if (originalTerminalScrollTop !== null) {
            await restoreVisibleTerminalScroll(
                page,
                originalTerminalScrollTop,
            ).catch(error => {
                console.warn(
                    `Could not restore tutorial screenshot ` +
                    `terminal scroll position: ${error.message}`,
                );
            });
        }
    }
}

// record: SECONDSs before a <video>: the same clip as a screenshot, once per
// frame. Time is not left to the clock: for every frame the tutorial's hook
// setPreviewTime({ page, seconds }) puts the app at exactly that moment and
// waits until it has drawn it, so a slow machine makes the same video as a
// fast one. Afterwards setPreviewTime({ page, seconds: null }) hands the
// page back to real time. ffmpeg turns the frames into .mp4 or .webm.
async function captureVideo(shot, tutorialDirectory, hooks) {
    if (!hooks?.setPreviewTime) {
        throw new Error(
            'record needs setPreviewTime({ page, seconds }) in tutorial-screenshot-hooks.mjs',
        );
    }
    const page = pageForTab(shot.tab);
    if (!page || page.isClosed()) {
        throw new Error(`Video requests unavailable tab: ${shot.tab}`);
    }

    const { session, metrics } = await applyViewportProfile(page, shot.viewport, shot.zoom, shot.sharpCanvas);
    await page.evaluate(() => window.scrollTo(0, 0)).catch(() => {});
    if (shot.tab === 'workspace') {
        await waitForWorkspaceFont(page);
        await installWorkspaceScreenshotStyle(page);
    }

    const topPixels = Math.round(shot.viewport.height * shot.cropTop);
    const bottomPixels = Math.round(shot.viewport.height * shot.cropBottom);
    const heightPixels = shot.viewport.height - topPixels - bottomPixels;
    const clip = {
        x: 0,
        y: topPixels / metrics.deviceScaleFactor,
        width: shot.viewport.width / metrics.deviceScaleFactor,
        height: heightPixels / metrics.deviceScaleFactor,
        scale: metrics.captureScale,
    };

    const target = path.join(tutorialDirectory, shot.target);
    const extension = path.extname(target);
    const temporaryVideo = `${target}.tmp-${process.pid}-${Date.now()}${extension}`;
    const frameDirectory = await fs.mkdtemp(`/tmp/tutorial-video-${process.pid}-`);
    await fs.mkdir(path.dirname(target), { recursive: true });

    try {
        const times = frameTimes(shot.record, shot.fps);
        for (let index = 0; index < times.length; index += 1) {
            await hooks.setPreviewTime({ page, seconds: times[index], timeout: 30_000 });
            const captured = await session.send('Page.captureScreenshot', {
                format: 'png',
                fromSurface: true,
                captureBeyondViewport: false,
                clip,
            });
            await fs.writeFile(
                path.join(frameDirectory, `frame-${String(index).padStart(5, '0')}.png`),
                Buffer.from(captured.data, 'base64'),
            );
        }
        console.log(`  ${times.length} frames at ${shot.fps} fps`);

        await execFileAsync('ffmpeg', videoEncodeArgs({
            framePattern: path.join(frameDirectory, 'frame-%05d.png'),
            fps: shot.fps,
            output: temporaryVideo,
        }), { maxBuffer: 16 * 1024 * 1024 });
        await fs.chmod(temporaryVideo, 0o644);
        await fs.rename(temporaryVideo, target);
        await inheritDirectoryOwnership(target, tutorialDirectory);
    } finally {
        await hooks.setPreviewTime({ page, seconds: null, timeout: 30_000 }).catch(error => {
            console.warn(`Could not hand the preview back to real time: ${error.message}`);
        });
        await fs.rm(frameDirectory, { recursive: true, force: true }).catch(() => {});
        await fs.rm(temporaryVideo, { force: true }).catch(() => {});
    }

    return sha256(await fs.readFile(target));
}

// git-graph: PATH draws the object graph of the screenshot Workspace's
// repository at PATH (below /workspace) as it is at this point of the
// recipe chain. Reads only (git-graph.js).
async function captureGitGraph(shot, tutorialDirectory) {
    const root = path.resolve(USER_ROOT, fsTagForEmail(SCREENSHOT_EMAIL), 'workspace');
    const repo = path.resolve(root, shot.gitGraph);
    if (!repo.startsWith(`${root}${path.sep}`)) {
        throw new Error(`git-graph path escaped the Workspace: ${shot.gitGraph}`);
    }
    const svg = gitGraphSvg(repo);
    const target = path.join(tutorialDirectory, shot.target);
    const temporary = `${target}.tmp-${process.pid}`;
    await fs.writeFile(temporary, svg, { mode: 0o644 });
    await fs.rename(temporary, target);
    await inheritDirectoryOwnership(target, tutorialDirectory);
    return sha256(svg);
}

// pdf-page: PATH PAGE renders one page of a PDF in the screenshot Workspace
// (compiled there by the recipe, with the Workspace's own TeX and fonts) at
// 300 dpi, cuts it to its content and shaves 1 mm off the top and bottom, as
// src/content/latex/fonts/pdf_page_to_svg.rb did by hand: right for a line of
// type. pdf-page-full keeps the whole sheet with its margins, at 200 dpi, for
// a page that should look like a page.
async function capturePdfPage(shot, tutorialDirectory) {
    const root = path.resolve(USER_ROOT, fsTagForEmail(SCREENSHOT_EMAIL), 'workspace');
    const pdf = path.resolve(root, shot.pdfPage.path);
    if (!pdf.startsWith(`${root}${path.sep}`)) {
        throw new Error(`pdf-page path escaped the Workspace: ${shot.pdfPage.path}`);
    }
    await fs.access(pdf).catch(() => { throw new Error(`pdf-page: ${shot.pdfPage.path} does not exist`); });
    const target = path.join(tutorialDirectory, shot.target);
    const work = await fs.mkdtemp(path.join(os.tmpdir(), 'pdf-page-'));
    try {
        const page = String(shot.pdfPage.page);
        // pdf-page: PATH PAGE TEXT: the page must contain TEXT, so a page
        // number that no longer fits fails instead of showing the wrong page.
        if (shot.pdfPage.expect) {
            const { stdout } = await execFileAsync('pdftotext', ['-f', page, '-l', page, pdf, '-']);
            const text = stdout.replace(/\s+/g, ' ');
            if (!text.includes(shot.pdfPage.expect)) {
                throw new Error(`pdf-page: page ${page} of ${shot.pdfPage.path} does not contain »${shot.pdfPage.expect}«`);
            }
        }
        const dpi = shot.pdfPage.full ? '200' : '300';
        await execFileAsync('pdftocairo', ['-png', '-r', dpi, '-f', page, '-l', page, '-singlefile', pdf, path.join(work, 'page')]);
        let trimmed = path.join(work, 'page.png');
        if (!shot.pdfPage.full) {
            trimmed = path.join(work, 'trimmed.png');
            await execFileAsync('convert', [path.join(work, 'page.png'), '-trim', '+repage', '-shave', '0x12', '+repage', trimmed]);
        }
        const temporary = `${target}.tmp-${process.pid}`;
        if (/\.webp$/i.test(target)) {
            await execFileAsync('cwebp', ['-quiet', '-q', '90', trimmed, '-o', temporary]);
        } else {
            await fs.copyFile(trimmed, temporary);
        }
        await fs.chmod(temporary, 0o644);
        await fs.rename(temporary, target);
        await inheritDirectoryOwnership(target, tutorialDirectory);
    } finally {
        await fs.rm(work, { recursive: true, force: true }).catch(() => {});
    }
    return sha256(await fs.readFile(target));
}

// image-file: PATH [SCALE] makes the image from a picture a program wrote
// in the screenshot Workspace (save_as_png in Pixelflow Canvas), enlarged
// SCALE times without smoothing, so each pixel stays a sharp square.
async function captureImageFile(shot, tutorialDirectory) {
    const root = path.resolve(USER_ROOT, fsTagForEmail(SCREENSHOT_EMAIL), 'workspace');
    const source = path.resolve(root, shot.imageFile.path);
    if (!source.startsWith(`${root}${path.sep}`)) {
        throw new Error(`image-file path escaped the Workspace: ${shot.imageFile.path}`);
    }
    await fs.access(source).catch(() => { throw new Error(`image-file: ${shot.imageFile.path} does not exist`); });
    const target = path.join(tutorialDirectory, shot.target);
    const work = await fs.mkdtemp(path.join(os.tmpdir(), 'image-file-'));
    try {
        const enlarged = path.join(work, 'image.png');
        await execFileAsync('convert', [source, '-filter', 'point', '-resize', `${shot.imageFile.scale * 100}%`, enlarged]);
        const temporary = `${target}.tmp-${process.pid}`;
        if (/\.webp$/i.test(target)) {
            await execFileAsync('cwebp', ['-quiet', '-lossless', enlarged, '-o', temporary]);
        } else {
            await fs.copyFile(enlarged, temporary);
        }
        await fs.chmod(temporary, 0o644);
        await fs.rename(temporary, target);
        await inheritDirectoryOwnership(target, tutorialDirectory);
    } finally {
        await fs.rm(work, { recursive: true, force: true }).catch(() => {});
    }
    return sha256(await fs.readFile(target));
}

function screenshotOutputDimensions(shot) {
    const topPixels = Math.round(shot.viewport.height * shot.cropTop);
    const bottomPixels = Math.round(shot.viewport.height * shot.cropBottom);
    const leftPixels = Math.round(shot.viewport.width * (shot.cropLeft || 0));
    const rightPixels = Math.round(shot.viewport.width * (shot.cropRight || 0));
    return scalePixelDimensions(
        shot.viewport.width - leftPixels - rightPixels,
        shot.viewport.height - topPixels - bottomPixels,
        DEFAULT_PROFILE.captureScale,
    );
}

function generationStatusItem(shot, stale, missing, previous) {
    const dimensions = screenshotOutputDimensions(shot);
    return {
        target: shot.target,
        kind: isVideoTarget(shot.target) ? 'video' : 'image',
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
            // A page with steps only still needs its hooks to walk through.
            tutorialHooks: (shots[TRAILING_STEPS] || []).length > 0 ? await loadTutorialScreenshotHooks(tutorialDirectory) : null,
            environmentFingerprint: null,
            fingerprint: sha256(`${relativeMarkdown}\0empty`),
        };
    }

    const tutorialHooks = await loadTutorialScreenshotHooks(tutorialDirectory);
    const oldManifest = await readManifest(tutorialDirectory);
    // The Workspace image counts only through its look (the code-server
    // version, payload.workspace_look): rebuilding the image for a new
    // extension or toolchain must not re-render every tutorial. Use
    // src/scripts/recreate-screenshots.rb when such a change is visible.
    const workspaceLook = `${payload?.workspace_look || 'unknown'}`;
    const environmentFingerprint = sha256(JSON.stringify({
        generator_version: GENERATOR_VERSION,
        playwright: PLAYWRIGHT_VERSION,
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
        for (const file of [shot.target, ...variantTargets(shot.target, shot.variants).map(variant => variant.target)]) {
            try { await fs.access(path.join(tutorialDirectory, file)); } catch { exists = false; }
        }
        const needsGeneration = force || !exists ||
            !entryIsCurrent(previous, shot, { environmentFingerprint, workspaceLook, workspaceWindow: WORKSPACE_HEIGHT });
        stale.push(needsGeneration);
        screenshots.push(generationStatusItem(shot, needsGeneration, !exists, previous));
        if (!needsGeneration) {
            // Current: record today's hashes (this also moves entries from
            // older generator versions to the new fields, without rendering).
            nextManifest.screenshots[shot.target] = {
                ...previous,
                state_hash: shot.stateHash,
                environment_fingerprint: environmentFingerprint,
                // the same release keeps its recorded revision, so a status check
                // on a machine with a newer pull of the image changes no manifest
                workspace_look: workspaceLook === 'unknown' ||
                    codeServerRelease(workspaceLook) === codeServerRelease(previous.workspace_look ?? 'unknown')
                    ? (previous.workspace_look ?? workspaceLook) : workspaceLook,
                ...(showsWorkspace(shot) ? { workspace_window: WORKSPACE_HEIGHT } : {}),
            };
        }
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
        workspaceLook,
        fingerprint,
    };
}

async function finalizeCurrentPlan(plan) {
    if (plan.shots.length > 0 &&
        manifestText(plan.oldManifest) !== manifestText(plan.nextManifest)) {
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

    if (autoRender.enabled && pendingJobs > 0) await setAutoRender(false, 'busy');
    if (!autoRender.enabled) {
        // Stale images keep their warning outline; nothing is queued.
        const paused = {
            status: 'paused',
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
        latestGenerationJobs.set(plan.relativeMarkdown, paused);
        return paused;
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

    pendingJobs += 1;
    const run = generationQueue.then(async () => {
        try {
            job.started_at = Date.now();
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
        } finally {
            pendingJobs -= 1;
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
        nextManifest,
        environmentFingerprint,
        workspaceLook,
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

    // A forced replay walks a page's steps even when its pictures are current,
    // and even on a page that has steps only (tutorial-step).
    const walkSteps = plan.force && (shots[TRAILING_STEPS] || []).length > 0;
    if (shots.length === 0 && !walkSteps) return { generated: 0, current: 0, screenshots: 0 };

    // Removed recipe entries disappear from the manifest. The image itself stays
    // in the tutorial directory and therefore instantly becomes a normal manual
    // tutorial image again.
    if (!stale.some(Boolean) && !walkSteps) {
        await finalizeCurrentPlan(plan);
        if (progressJob) progressJob.completed = shots.length;
        return { generated: 0, current: shots.length, screenshots: shots.length };
    }

    if (progressJob) progressJob.status = 'preparing';
    await acquireFreshWorkspace();
    mousePosition = null;
    if (progressJob) progressJob.status = 'running';

    let generated = 0;
    scopeJournal = null; // a render that failed inside a scope left it behind
    for (let index = 0; index < shots.length; index += 1) {
        const shot = shots[index];

        // The first shot of a tutorial-scope: if none of its shots needs a
        // new file, only the steps from before the scope run, and the walk
        // goes on after it. Otherwise the scope runs, with its files noted
        // from the first of its own actions on, and is undone at its end.
        const scope = shot[SCOPE];
        const scopeStart = scope !== undefined && shots[index - 1]?.[SCOPE] !== scope;
        const scopeEnd = scope !== undefined && shots[index + 1]?.[SCOPE] !== scope;
        let journalFrom = -1;
        if (scopeStart) {
            let last = index;
            while (shots[last + 1]?.[SCOPE] === scope) last += 1;
            const entry = shot[SCOPE_ENTRY] || 0;
            if (!stale.slice(index, last + 1).some(Boolean)) {
                console.log(`Tutorial scope ${relativeMarkdown} -> ${shot.target} … ${shots[last].target}: current, skipped`);
                for (const action of shot.actions.slice(0, entry)) {
                    try {
                        await executeAction(action, action.tab || shot.tab, tutorialHooks);
                    } catch (error) {
                        throw withSourceError(error, action[SOURCE_LOCATION] || shot[SOURCE_LOCATION]);
                    }
                }
                if (progressJob) {
                    for (let i = index; i <= last; i += 1) progressJob.screenshots[i].state = 'current';
                    progressJob.completed = last + 1;
                }
                index = last;
                continue;
            }
            journalFrom = entry;
        }

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

        for (const [actionIndex, action] of shot.actions.entries()) {
            if (actionIndex === journalFrom) scopeJournal = new Map();
            const source = action[SOURCE_LOCATION] || shotSource;
            console.log(`  line ${source?.line ?? '?'}: ${source?.text?.trim() || action.type}`);

            try {
                await executeAction(action, action.tab || shot.tab, tutorialHooks);
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

        // The shot itself shows every variant off, also when it is only
        // replayed, so the next shot starts from the same state either way.
        if (shot.variants) {
            try {
                await setVariants(shot, tutorialHooks, []);
            } catch (error) {
                throw withSourceError(error, shotSource);
            }
        }

        if (stale[index]) {
            const targetSource = shot[TARGET_LOCATION] || shotSource;
            console.log(`  line ${targetSource?.line ?? '?'}: capture ${shot.target}`);
            const capture = (target) => {
                const variantShot = { ...shot, target };
                return shot.pdfPage
                    ? capturePdfPage(variantShot, tutorialDirectory)
                    : shot.imageFile
                    ? captureImageFile(variantShot, tutorialDirectory)
                    : shot.gitGraph
                    ? captureGitGraph(variantShot, tutorialDirectory)
                    : shot.record != null
                        ? captureVideo(variantShot, tutorialDirectory, tutorialHooks)
                        : captureShot(variantShot, tutorialDirectory);
            };
            let outputSha256;
            const variantSha256 = {};
            try {
                outputSha256 = await capture(shot.target);
                rememberCapture(relativeMarkdown, shot.target, outputSha256);
                for (const variant of variantTargets(shot.target, shot.variants)) {
                    console.log(`  capture ${variant.target}`);
                    await setVariants(shot, tutorialHooks, variant.on);
                    variantSha256[variant.target] = await capture(variant.target);
                    rememberCapture(relativeMarkdown, variant.target, variantSha256[variant.target]);
                }
                if (shot.variants) await setVariants(shot, tutorialHooks, []);
            } catch (error) {
                throw withSourceError(error, targetSource);
            }
            generated += 1;
            nextManifest.screenshots[shot.target] = {
                state_hash: shot.stateHash,
                environment_fingerprint: environmentFingerprint,
                workspace_look: workspaceLook,
                ...(showsWorkspace(shot) ? { workspace_window: WORKSPACE_HEIGHT } : {}),
                output_sha256: outputSha256,
            };
            if (shot.variants) {
                nextManifest.screenshots[shot.target].variants = variantSha256;
            }
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
        } else if (progressJob) {
            // buildGenerationPlan already put the current entry into nextManifest.
            progressJob.screenshots[index].state = 'current';
        }

        // The end of a scope: its files as they were, the preview reloaded
        if (scopeEnd) {
            if (journalFrom === shot.actions.length) scopeJournal = scopeJournal || new Map();
            await restoreScopeFiles();
            if (preview && !preview.isClosed()) {
                // As in a recipe (write-file, sleep: 1): Live Server reloads
                // by itself first; a reload right away would cross it.
                await preview.waitForTimeout(1000);
                try {
                    await previewReload(false, tutorialHooks);
                } catch (error) {
                    throw withSourceError(error, shotSource);
                }
            }
            console.log(`  end of tutorial-scope: files restored`);
        }

        if (progressJob) progressJob.completed = index + 1;
    }

    // The pictures are done; a failing step after them must not lose them.
    // A page with steps only gets no manifest.
    if (shots.length > 0) await writeManifest(tutorialDirectory, nextManifest);
    try {
        // Steps after the last screenshot: nothing depends on them, but they
        // check that the page's last steps work (tutorial-step).
        for (const action of shots[TRAILING_STEPS] || []) {
            const source = action[SOURCE_LOCATION];
            console.log(`  line ${source?.line ?? '?'}: ${source?.text?.trim() || action.type}`);
            try {
                await executeAction(action, action.tab, tutorialHooks);
            } catch (error) {
                await captureErrorTabs({ tutorialDirectory, shot: { tab: action.tab } }).catch(captureError => {
                    console.error(`Could not capture tutorial step failure state: ${captureError.message}`);
                });
                throw withSourceError(error, source);
            }
        }
    } finally {
        if (generated > 0) {
            scheduleFreshWorkspace();
        }
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
            return sendJson(response, 200, { ...(job || {
                status: 'idle',
                monitor: false,
                markdown_path: relativeMarkdown,
                screenshots: [],
            }), auto_render: autoRender });
        } catch (error) {
            return sendJson(response, 400, { error: error.message });
        }
    }
    // The state of each generated file, without rendering or queuing anything
    // (recreate-screenshots.rb --status).
    if (request.method === 'POST' && requestUrl.pathname === '/plan') {
        try {
            const payload = await readJson(request);
            const plan = await buildGenerationPlan({ ...payload, force: false });
            return sendJson(response, 200, { markdown_path: plan.relativeMarkdown, screenshots: plan.screenshots });
        } catch (error) {
            return sendJson(response, 400, { error: error.message });
        }
    }
    // A pass of recreate-screenshots.rb: its pages, each with the number of
    // files it will render, planned now so the progress page knows the total.
    if (request.method === 'POST' && requestUrl.pathname === '/run') {
        try {
            const payload = await readJson(request);
            const force = payload.force === true;
            const pages = [];
            for (const markdownPath of payload.markdown_paths || []) {
                const plan = await buildGenerationPlan({ markdown_path: markdownPath, workspace_look: payload.workspace_look, force });
                pages.push({ markdown_path: plan.relativeMarkdown, stale: plan.screenshots.filter(s => s.stale).length, total: plan.screenshots.length });
            }
            currentRun = { started_at: Date.now(), force, pages };
            return sendJson(response, 200, { pages: pages.length, stale: pages.reduce((n, p) => n + p.stale, 0) });
        } catch (error) {
            return sendJson(response, 400, { error: error.message });
        }
    }
    // Everything the progress page shows: the pass, each page's job, the
    // files captured last.
    if (request.method === 'GET' && requestUrl.pathname === '/overview') {
        const pages = (currentRun?.pages || []).map(page => {
            const job = latestGenerationJobs.get(page.markdown_path);
            const fresh = job && currentRun && (job.started_at || 0) >= currentRun.started_at;
            const queued = job && job.status === 'queued' && !job.started_at;
            return {
                ...page,
                status: fresh ? job.status : (queued ? 'queued' : 'waiting'),
                generated: fresh ? job.generated : 0,
                completed: fresh ? job.completed : 0,
                current_target: fresh ? job.current_target : null,
                error: fresh ? job.error : null,
                started_at: fresh ? job.started_at : null,
                finished_at: fresh ? job.finished_at || null : null,
            };
        });
        return sendJson(response, 200, {
            run: currentRun ? { started_at: currentRun.started_at, force: currentRun.force } : null,
            pages,
            recent: recentCaptures,
            pending_jobs: pendingJobs,
            now: Date.now(),
        });
    }
    if (requestUrl.pathname === '/auto-render') {
        if (request.method === 'POST') {
            const payload = await readJson(request).catch(() => ({}));
            if (typeof payload.enabled !== 'boolean') return sendJson(response, 400, { error: 'enabled must be true or false' });
            await setAutoRender(payload.enabled, 'manual');
        }
        return sendJson(response, 200, autoRender);
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

        // A synchronous request (recreate-screenshots.rb) gets a progress
        // entry too, for /status and the progress page.
        const relativeMarkdown = safeRelativePath(payload?.markdown_path);
        const tracked = {
            status: 'queued', monitor: true, markdown_path: relativeMarkdown,
            total: 0, completed: 0, stale: 0, generated: 0, current_target: null, error: null, screenshots: [],
        };
        latestGenerationJobs.set(relativeMarkdown, tracked);
        pendingJobs += 1;
        const job = generationQueue.then(async () => {
            tracked.started_at = Date.now();
            try {
                const result = await generateTutorial(payload, tracked);
                Object.assign(tracked, { status: 'done', current_target: null, finished_at: Date.now() });
                return result;
            } catch (error) {
                Object.assign(tracked, { status: 'error', current_target: null, error: error.message, finished_at: Date.now() });
                throw error;
            }
        }).finally(() => { pendingJobs -= 1; });
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
