import crypto from 'node:crypto';
import dns from 'node:dns/promises';
import fs from 'node:fs/promises';
import http from 'node:http';
import path from 'node:path';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
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

function colorSchemeEnv(name, fallback) {
    const value = (process.env[name] || fallback).trim().toLowerCase();
    if (value !== 'light' && value !== 'dark') {
        throw new Error(`${name} must be "light" or "dark"`);
    }
    return value;
}

const CONTENT_ROOT = '/content';
const USER_ROOT = '/user';
const BASE_URL = process.env.TUTORIAL_SCREENSHOT_BASE_URL || 'http://workspace.test:8025';
const SCREENSHOT_EMAIL = process.env.TUTORIAL_SCREENSHOT_EMAIL || 'screenshots@example.com';
const SCREENSHOT_WORKSPACE_USER = process.env.TUTORIAL_SCREENSHOT_WORKSPACE_USER || 'student';
const LOGIN_CODE = process.env.TUTORIAL_SCREENSHOT_LOGIN_CODE || '123456';
const PORT = Number.parseInt(process.env.PORT || '9393', 10);
const GENERATOR_VERSION = 5;
const PLAYWRIGHT_VERSION = '1.62.1';
const MANIFEST_NAME = '.tutorial-screenshots.json';
const DEFAULT_PROFILE = Object.freeze({
    width: positiveIntegerEnv('TUTORIAL_SCREENSHOT_WIDTH', 1853),
    height: positiveIntegerEnv('TUTORIAL_SCREENSHOT_HEIGHT', 929),
    zoom: positiveNumberEnv('TUTORIAL_SCREENSHOT_ZOOM', 1.5),
    colorScheme: colorSchemeEnv('TUTORIAL_SCREENSHOT_COLOR_SCHEME', 'dark'),
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
    'clone-confirm-url',
    'clone-accept-destination',
    'clone-open',
    'go-live',
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
`;

let browserPromise = null;
let context = null;
let dashboard = null;
let loggedIn = false;
let workspace = null;
let preview = null;
let generationQueue = Promise.resolve();
const deviceMetricSessions = new WeakMap();

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

function parsePercent(value) {
    const text = `${value}`.trim();
    const numeric = Number.parseFloat(text.endsWith('%') ? text.slice(0, -1) : text);
    if (!Number.isFinite(numeric)) throw new Error(`Invalid percentage: ${value}`);
    const result = text.endsWith('%') ? numeric / 100 : numeric;
    if (result < 0 || result >= 1) throw new Error(`Crop must be between 0% and 99%: ${value}`);
    return result;
}

function codeBlocks(markdown) {
    const blocks = [];
    const regex = /```[^\n]*\n([\s\S]*?)```/g;
    let match;
    while ((match = regex.exec(markdown)) !== null) {
        blocks.push({
            start: match.index,
            end: regex.lastIndex,
            contents: match[1].replace(/\n$/, ''),
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

function parseRecipe(text) {
    const recipe = {
        tab: 'workspace',
        viewport: { width: DEFAULT_PROFILE.width, height: DEFAULT_PROFILE.height },
        zoom: DEFAULT_PROFILE.zoom,
        cropTop: 0,
        cropBottom: 0,
        actions: [],
    };

    for (const rawLine of text.split('\n')) {
        const line = rawLine.trim();
        if (!line || line.startsWith('#')) continue;

        if (SIMPLE_ACTIONS.has(line)) {
            recipe.actions.push({ type: line });
            continue;
        }

        let match;
        if ((match = line.match(/^clone-start:\s*(.+)$/))) {
            recipe.actions.push({ type: 'clone-start', url: match[1].trim() });
        } else if ((match = line.match(/^open-file:\s*(.+)$/))) {
            recipe.actions.push({ type: 'open-file', path: safeRelativePath(match[1].trim()) });
        } else if ((match = line.match(/^click:\s*(.+)$/))) {
            recipe.actions.push({ type: 'click', label: match[1].trim() });
        } else if ((match = line.match(/^press:\s*(.+)$/))) {
            recipe.actions.push({ type: 'press', key: match[1].trim() });
        } else if ((match = line.match(/^write-file:\s*(.+?)\s*<-\s*(previous-code|code:.+)$/))) {
            recipe.actions.push({
                type: 'write-file',
                path: safeRelativePath(match[1].trim()),
                source: match[2].trim(),
            });
        } else if ((match = line.match(/^tab:\s*(workspace|preview)$/))) {
            recipe.tab = match[1];
        } else if ((match = line.match(/^viewport:\s*(\d+)x(\d+)$/i))) {
            recipe.viewport = { width: Number(match[1]), height: Number(match[2]) };
        } else if ((match = line.match(/^zoom:\s*([0-9.]+)$/))) {
            recipe.zoom = Number.parseFloat(match[1]);
            if (!Number.isFinite(recipe.zoom) || recipe.zoom <= 0) throw new Error(`Invalid zoom: ${match[1]}`);
        } else if ((match = line.match(/^crop-top:\s*(.+)$/))) {
            recipe.cropTop = parsePercent(match[1]);
        } else if ((match = line.match(/^crop-bottom:\s*(.+)$/))) {
            recipe.cropBottom = parsePercent(match[1]);
        } else {
            throw new Error(`Unknown tutorial screenshot instruction: ${line}`);
        }
    }

    if (recipe.cropTop + recipe.cropBottom >= 0.98) {
        throw new Error('Top and bottom crop leave no useful screenshot area');
    }
    return recipe;
}

function parseTutorial(markdown, markdownPath) {
    const blocks = codeBlocks(markdown);
    const regex = /<!--\s*tutorial-screenshot\s*\n([\s\S]*?)-->/g;
    const shots = [];
    let match;
    let previousState = sha256(`tutorial-screenshot-v${GENERATOR_VERSION}\0${markdownPath}`);

    while ((match = regex.exec(markdown)) !== null) {
        const recipe = parseRecipe(match[1]);
        const afterComment = markdown.slice(regex.lastIndex);
        const nextRecipeIndex = afterComment.search(/<!--\s*tutorial-screenshot\b/);
        const imageMatch = /<img\b[^>]*\bsrc=['"]([^'"]+)['"][^>]*>/i.exec(afterComment);
        if (!imageMatch || (nextRecipeIndex >= 0 && imageMatch.index > nextRecipeIndex)) {
            throw new Error(`tutorial-screenshot block at offset ${match.index} has no following <img>`);
        }

        const target = safeRelativePath(imageMatch[1]);
        if (/^[a-z][a-z\d+.-]*:/i.test(target) || target.startsWith('/')) {
            throw new Error(`Generated screenshot must use a relative tutorial image path: ${target}`);
        }

        const resolvedActions = recipe.actions.map(action => {
            if (action.type !== 'write-file') return action;
            return {
                ...action,
                contents: findCodeSource(blocks, match.index, action.source),
            };
        });

        const resolved = {
            target,
            tab: recipe.tab,
            viewport: recipe.viewport,
            zoom: recipe.zoom,
            cropTop: recipe.cropTop,
            cropBottom: recipe.cropBottom,
            actions: resolvedActions,
        };
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
                args: [`--host-resolver-rules=${hostRules}`],
            });
        })();
    }
    return browserPromise;
}

async function ensureContext() {
    if (context) return context;
    const browser = await ensureBrowser();
    context = await browser.newContext({
        viewport: { width: DEFAULT_PROFILE.width, height: DEFAULT_PROFILE.height },
        deviceScaleFactor: 1,
        colorScheme: DEFAULT_PROFILE.colorScheme,
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

    // /l/<tag>/<code> creates the session and redirects to /. Do not follow that
    // redirect: rendering / while Ruby is in parse_content() would recursively
    // call parse_content() and deadlock on its mutex. The Set-Cookie header is
    // still processed by Chromium before the manual redirect response is exposed.
    await browserRequest(`/l/${encodeURIComponent(requested.tag)}/${encodeURIComponent(LOGIN_CODE)}`, {
        method: 'GET',
        body: null,
        redirect: 'manual',
    });

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
        await new Promise(resolve => setTimeout(resolve, 250));
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
                    await page.waitForFunction(
                        colorScheme => {
                            const workbench = document.querySelector('.monaco-workbench');
                            if (!workbench) return false;
                            if (colorScheme === 'dark') {
                                return workbench.classList.contains('vs-dark') ||
                                    workbench.classList.contains('hc-black');
                            }
                            return workbench.classList.contains('vs') ||
                                workbench.classList.contains('hc-light');
                        },
                        DEFAULT_PROFILE.colorScheme,
                        { timeout: Math.min(30_000, remaining) },
                    );
                    await page.waitForTimeout(250);
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
    // `.monaco-workbench` and the theme become available before all built-in
    // contributions have finished starting. In particular, Explorer's
    // "Clone Repository" welcome action appears a little later.
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

async function freshWorkspace() {
    await ensureLoggedIn();

    for (const page of context.pages()) {
        if (page !== dashboard) await page.close().catch(() => {});
    }
    workspace = null;
    preview = null;

    jsonResponse(await browserRequest('/api/reset_server', {
        body: { confirmation: SCREENSHOT_EMAIL },
    }), 'Tutorial screenshot Workspace reset');

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
}

function quickInput(page) {
    return page.locator('input.quick-input-box').last();
}

async function locatorIsVisible(locator) {
    return locator.isVisible().catch(() => false);
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

async function closeFolder() {
    const noFolder = workspace.getByText('NO FOLDER OPENED', { exact: true }).first();
    if ((await noFolder.count()) > 0) return;

    await workspace.keyboard.press('Control+K');
    await workspace.keyboard.press('KeyF');
    await workspace.waitForTimeout(250);
}

async function cloneStart(url) {
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
    await input.fill(url);
}

async function cloneConfirmUrl() {
    const input = quickInput(workspace);
    await input.waitFor({ state: 'visible', timeout: 10_000 });
    await input.press('Enter');
    await workspace.waitForFunction(() => document.body.innerText.includes('/workspace'), null, { timeout: 20_000 });
}

async function cloneAcceptDestination() {
    const input = quickInput(workspace);
    await input.waitFor({ state: 'visible', timeout: 10_000 });
    await input.press('Enter');
    await workspace.getByRole('button', { name: 'Open', exact: true }).waitFor({ state: 'visible', timeout: 120_000 });
}

async function cloneOpen() {
    const button = workspace.getByRole('button', { name: 'Open', exact: true });
    await button.waitFor({ state: 'visible', timeout: 30_000 });
    await button.click();
    await workspace.waitForFunction(
        () => document.body.innerText.includes('pages-starter') && document.body.innerText.includes('config.js'),
        null,
        { timeout: 60_000 },
    );
}

async function openFile(relativePath) {
    await workspace.keyboard.press('Control+P');
    const input = quickInput(workspace);
    await input.waitFor({ state: 'visible', timeout: 10_000 });
    await input.fill(relativePath);
    await input.press('Enter');
    const basename = path.posix.basename(relativePath);
    await workspace.waitForFunction(
        name => [...document.querySelectorAll('.tab .label-name, .tabs-container .label-name')]
            .some(node => node.textContent?.trim() === name),
        basename,
        { timeout: 20_000 },
    ).catch(() => workspace.waitForTimeout(300));
}

async function goLive() {
    const goLive = workspace.getByText('Go Live', { exact: true }).last();
    await goLive.waitFor({ state: 'visible', timeout: 30_000 });
    const popupPromise = context.waitForEvent('page', { timeout: 60_000 });
    await goLive.click();
    preview = await popupPromise;
    await preview.waitForLoadState('domcontentloaded');
    await waitForBifPreview(preview);
}

async function waitForBifPreview(page) {
    await page.waitForSelector('#content', { state: 'attached', timeout: 60_000 });
    await page.waitForFunction(
        () => (document.querySelector('#content')?.innerText || '').trim().length > 0,
        null,
        { timeout: 60_000 },
    );
}

async function previewReload(reset = false) {
    if (!preview || preview.isClosed()) throw new Error('No preview tab is open');
    if (reset) {
        await preview.evaluate(() => {
            sessionStorage.clear();
            localStorage.clear();
        }).catch(() => {});
    }
    await preview.reload({ waitUntil: 'domcontentloaded', timeout: 60_000 });
    await waitForBifPreview(preview);
}

async function writeWorkspaceFile(relativePath, contents) {
    const root = path.join(USER_ROOT, fsTagForEmail(SCREENSHOT_EMAIL), 'workspace', 'bif');
    const target = path.join(root, safeRelativePath(relativePath));
    const resolvedRoot = path.resolve(root);
    const resolvedTarget = path.resolve(target);
    if (!resolvedTarget.startsWith(`${resolvedRoot}${path.sep}`)) {
        throw new Error(`Workspace path escaped BIF checkout: ${relativePath}`);
    }
    await fs.mkdir(path.dirname(target), { recursive: true });
    const text = contents.endsWith('\n') ? contents : `${contents}\n`;
    await fs.writeFile(target, text, 'utf8');
    await fs.chown(target, 1000, 1000).catch(() => {});
}

async function clickPreview(label) {
    if (!preview || preview.isClosed()) throw new Error('No preview tab is open');
    const control = preview.getByRole('button', { name: label, exact: true });
    await control.waitFor({ state: 'visible', timeout: 20_000 });
    await control.click();
    await preview.waitForTimeout(150);
}

async function executeAction(action) {
    switch (action.type) {
    case 'close-folder': return closeFolder();
    case 'show-left-sidebar': return setWorkbenchPartVisible('left-sidebar', true);
    case 'hide-left-sidebar': return setWorkbenchPartVisible('left-sidebar', false);
    case 'show-right-sidebar': return setWorkbenchPartVisible('right-sidebar', true);
    case 'hide-right-sidebar': return setWorkbenchPartVisible('right-sidebar', false);
    case 'show-bottom-panel': return setWorkbenchPartVisible('bottom-panel', true);
    case 'hide-bottom-panel': return setWorkbenchPartVisible('bottom-panel', false);
    case 'clone-start': return cloneStart(action.url);
    case 'clone-confirm-url': return cloneConfirmUrl();
    case 'clone-accept-destination': return cloneAcceptDestination();
    case 'clone-open': return cloneOpen();
    case 'open-file': return openFile(action.path);
    case 'go-live': return goLive();
    case 'write-file': return writeWorkspaceFile(action.path, action.contents);
    case 'preview-reload': return previewReload(false);
    case 'preview-reset': return previewReload(true);
    case 'click': return clickPreview(action.label);
    case 'press': {
        const page = preview && !preview.isClosed() ? preview : workspace;
        return page.keyboard.press(action.key);
    }
    default: throw new Error(`Unsupported action: ${action.type}`);
    }
}

async function applyViewportProfile(page, viewport, zoom) {
    let session = deviceMetricSessions.get(page);
    if (!session) {
        session = await context.newCDPSession(page);
        deviceMetricSessions.set(page, session);
    }

    // Browser zoom makes fewer CSS pixels fit into the same physical viewport.
    // Give VS Code that smaller layout viewport; Page.captureScreenshot() will
    // scale it back to the requested bitmap dimensions later.
    const cssViewport = {
        width: Math.ceil(viewport.width / zoom),
        height: Math.ceil(viewport.height / zoom),
    };

    await session.send('Emulation.setDeviceMetricsOverride', {
        width: cssViewport.width,
        height: cssViewport.height,
        deviceScaleFactor: 1,
        mobile: false,
        screenWidth: cssViewport.width,
        screenHeight: cssViewport.height,
    });

    // Give VS Code two frames to react to the changed layout metrics.
    await page.evaluate(() => new Promise(resolve => {
        requestAnimationFrame(() => requestAnimationFrame(resolve));
    }));

    return { session, cssViewport };
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

async function captureShot(shot, tutorialDirectory) {
    const page = shot.tab === 'preview' ? preview : workspace;
    if (!page || page.isClosed()) throw new Error(`Screenshot requests unavailable tab: ${shot.tab}`);

    const { session } = await applyViewportProfile(page, shot.viewport, shot.zoom);
    await page.evaluate(() => window.scrollTo(0, 0)).catch(() => {});
    await page.waitForTimeout(100);

    // Recipes express viewport and crop values in final screenshot pixels. The
    // screenshot clip itself is in CSS pixels, so divide by the emulated zoom.
    // Fractional clip dimensions are intentional: at 1853x929 / 150% they make
    // Playwright emit exactly 1853x929 device pixels instead of 1854x930.
    const topPixels = Math.round(shot.viewport.height * shot.cropTop);
    const bottomPixels = Math.round(shot.viewport.height * shot.cropBottom);
    const heightPixels = shot.viewport.height - topPixels - bottomPixels;
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
                y: topPixels / shot.zoom,
                width: shot.viewport.width / shot.zoom,
                height: heightPixels / shot.zoom,
                scale: shot.zoom,
            },
        });
        const png = Buffer.from(captured.data, 'base64');
        const actual = pngDimensions(png);
        await fs.writeFile(pngPath, png);

        const cwebpArgs = ['-quiet', '-lossless'];
        if (actual.width !== shot.viewport.width || actual.height !== heightPixels) {
            console.log(
                `Chromium screenshot was ${actual.width}x${actual.height}; ` +
                `normalizing to ${shot.viewport.width}x${heightPixels}`,
            );
            cwebpArgs.push('-resize', `${shot.viewport.width}`, `${heightPixels}`);
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

async function generateTutorial(payload) {
    const relativeMarkdown = safeRelativePath(payload?.markdown_path);
    if (!relativeMarkdown.endsWith('.md')) throw new Error('markdown_path must name a Markdown file');
    const markdownPath = path.join(CONTENT_ROOT, relativeMarkdown);
    const resolvedContentRoot = path.resolve(CONTENT_ROOT);
    const resolvedMarkdown = path.resolve(markdownPath);
    if (!resolvedMarkdown.startsWith(`${resolvedContentRoot}${path.sep}`)) {
        throw new Error('markdown_path escaped /content');
    }

    const markdown = await fs.readFile(markdownPath, 'utf8');
    const shots = parseTutorial(markdown, relativeMarkdown);
    if (shots.length === 0) return { generated: 0, current: 0, screenshots: 0 };

    const tutorialDirectory = path.dirname(markdownPath);
    const oldManifest = await readManifest(tutorialDirectory);
    const environmentFingerprint = sha256(JSON.stringify({
        generator_version: GENERATOR_VERSION,
        playwright: PLAYWRIGHT_VERSION,
        workspace_image_id: `${payload?.workspace_image_id || 'unknown'}`,
        profile: DEFAULT_PROFILE,
        screenshot_identity: {
            email: SCREENSHOT_EMAIL,
            workspace_user: SCREENSHOT_WORKSPACE_USER,
        },
    }));

    const nextManifest = { version: 1, screenshots: {} };
    const stale = [];

    for (const shot of shots) {
        const target = path.join(tutorialDirectory, shot.target);
        const previous = oldManifest.screenshots?.[shot.target];
        let exists = true;
        try { await fs.access(target); } catch { exists = false; }
        const needsGeneration = !exists || previous?.state_hash !== shot.stateHash ||
            previous?.environment_fingerprint !== environmentFingerprint;
        stale.push(needsGeneration);
        if (!needsGeneration && previous) nextManifest.screenshots[shot.target] = previous;
    }

    // Removed recipe entries disappear from the manifest. The image itself stays
    // in the tutorial directory and therefore instantly becomes a normal manual
    // tutorial image again.

    if (!stale.some(Boolean)) {
        if (JSON.stringify(oldManifest) !== JSON.stringify(nextManifest)) {
            await writeManifest(tutorialDirectory, nextManifest);
        }
        return { generated: 0, current: shots.length, screenshots: shots.length };
    }

    await freshWorkspace();
    let generated = 0;
    for (let index = 0; index < shots.length; index += 1) {
        const shot = shots[index];
        for (const action of shot.actions) await executeAction(action);

        if (stale[index]) {
            const outputSha256 = await captureShot(shot, tutorialDirectory);
            generated += 1;
            nextManifest.screenshots[shot.target] = {
                state_hash: shot.stateHash,
                environment_fingerprint: environmentFingerprint,
                output_sha256: outputSha256,
            };
            console.log(`Generated ${relativeMarkdown}: ${shot.target}`);
        } else {
            nextManifest.screenshots[shot.target] = oldManifest.screenshots[shot.target];
        }
    }

    await writeManifest(tutorialDirectory, nextManifest);
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
    if (request.method === 'GET' && request.url === '/health') {
        return sendJson(response, 200, { ok: true });
    }
    if (request.method !== 'POST' || request.url !== '/generate') {
        return sendJson(response, 404, { error: 'not_found' });
    }

    try {
        const payload = await readJson(request);
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
