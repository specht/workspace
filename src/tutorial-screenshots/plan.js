// Pure helpers for the generation plan: what a recipe produces (an image or
// a video), which shots are stale, and how frames become a video. Kept free
// of Playwright so they can be tested on their own (test/plan.test.js).
import crypto from 'node:crypto';

export function sha256(value) {
    return crypto.createHash('sha256').update(value).digest('hex');
}

export const VIDEO_EXTENSIONS = ['.mp4', '.webm'];
export const RECORD_LIMITS = Object.freeze({ minSeconds: 0.5, maxSeconds: 60, minFps: 1, maxFps: 60 });

export function isVideoTarget(target) {
    const lower = `${target}`.toLowerCase();
    return VIDEO_EXTENSIONS.some(extension => lower.endsWith(extension));
}

// The file a recipe produces: the src of the first <img>, <video> or
// <source> after the recipe comment, or the SVG of an _include_svg(NAME.svg)
// (a git-graph: target), before the next recipe.
// Returns { src, index } or null.
export function findRecipeTarget(afterComment) {
    const nextRecipe = afterComment.search(/<!--\s*tutorial-screenshot\b/);
    const candidates = [
        /<(?:img|video|source)\b[^>]*\bsrc=['"]([^'"]+)['"][^>]*>/i.exec(afterComment),
        /_include_svg\(\s*([^),\s]+)/.exec(afterComment),
    ].filter(Boolean);
    const match = candidates.sort((a, b) => a.index - b.index)[0];
    if (!match || (nextRecipe >= 0 && match.index > nextRecipe)) return null;
    return { src: match[1], index: match.index };
}

// git-graph: draws an SVG, and an SVG target can only come from git-graph:.
export function checkGitGraphForTarget(target, gitGraph) {
    const svg = `${target}`.toLowerCase().endsWith('.svg');
    if (svg && !gitGraph) {
        throw new Error(`${target} is an SVG: the recipe needs git-graph: PATH`);
    }
    if (!svg && gitGraph) {
        throw new Error(`git-graph: needs an _include_svg(NAME.svg) target, not ${target}`);
    }
}

// record: 8s / record: 2.5s
export function parseRecordSeconds(value) {
    const match = /^([0-9]+(?:\.[0-9]+)?)s$/.exec(`${value}`.trim());
    const seconds = match ? Number.parseFloat(match[1]) : Number.NaN;
    if (!(seconds >= RECORD_LIMITS.minSeconds && seconds <= RECORD_LIMITS.maxSeconds)) {
        throw new Error(`record must be between ${RECORD_LIMITS.minSeconds}s and ${RECORD_LIMITS.maxSeconds}s: ${value}`);
    }
    return seconds;
}

export function parseFps(value) {
    const fps = Number(`${value}`.trim());
    if (!Number.isInteger(fps) || fps < RECORD_LIMITS.minFps || fps > RECORD_LIMITS.maxFps) {
        throw new Error(`fps must be a whole number between ${RECORD_LIMITS.minFps} and ${RECORD_LIMITS.maxFps}: ${value}`);
    }
    return fps;
}

// A video needs record:, and record: makes sense only for a video.
export function checkRecordForTarget(target, record) {
    if (isVideoTarget(target) && !record) {
        throw new Error(`${target} is a video: the recipe needs record: SECONDSs`);
    }
    if (!isVideoTarget(target) && record) {
        throw new Error(`record: needs a <video> with an .mp4 or .webm target, not ${target}`);
    }
}

// The time of each frame, in seconds from the start of the recording.
export function frameTimes(seconds, fps) {
    const count = Math.max(1, Math.round(seconds * fps));
    return Array.from({ length: count }, (_, index) => index / fps);
}

// What later shots depend on: the actions and the page layout they ran at.
// Crops, the output name and capture settings shape only this shot's file.
// The files a shot with variants writes besides its own: one per non-empty
// combination of variants switched on, named in recipe order
// (variants: dark anaglyph -> x-dark.webp, x-anaglyph.webp, x-dark-anaglyph.webp).
export function variantTargets(target, variants = []) {
    const extension = target.slice(target.lastIndexOf('.'));
    const base = target.slice(0, -extension.length);
    const result = [];
    for (let mask = 1; mask < (1 << variants.length); mask += 1) {
        const on = variants.filter((_, index) => mask & (1 << index));
        result.push({ on, target: `${base}-${on.join('-')}${extension}` });
    }
    return result;
}

export function parseVariants(value) {
    const names = value.trim().split(/\s+/).filter(Boolean);
    if (names.length === 0 || names.length > 3) {
        throw new Error(`variants needs one to three names, got: ${value}`);
    }
    for (const name of names) {
        if (!/^[a-z][a-z0-9]*$/.test(name)) throw new Error(`Invalid variant name: ${name}`);
    }
    if (new Set(names).size !== names.length) throw new Error(`A variant is named twice: ${value}`);
    return names;
}

// Instructions that only check what is on screen or in the Workspace
// (expect-no-text, terminal-expect-no-text, check-task, check-task-fails).
// They change nothing, so
// they are not part of any hash: adding one to a recipe re-renders nothing.
export const ASSERTION_ACTIONS = new Set(['expect-no-text', 'terminal-expect-no-text', 'check-task', 'check-task-fails']);

function withoutAssertions(shot) {
    if (!shot.actions?.some(action => ASSERTION_ACTIONS.has(action.type))) return shot;
    return { ...shot, actions: shot.actions.filter(action => !ASSERTION_ACTIONS.has(action.type)) };
}

export function stateParts(shot) {
    const plain = withoutAssertions(shot);
    return { tab: plain.tab, viewport: plain.viewport, zoom: plain.zoom, actions: plain.actions };
}

// Chained hashes for a tutorial's shots (resolved recipes, in order).
// stateHash: this shot's own file is current if nothing it depends on
//   changed: everything that ran before it, its own actions and capture.
// legacyStateHash: the hash generator versions up to 19 stored, where every
//   earlier shot's crop and file name were part of the chain. Accepted once
//   so that switching to the narrower chain does not re-render everything.
export function chainStateHashes(shots, seed) {
    let chain = seed;
    let legacy = seed;
    return shots.map(original => {
        const shot = withoutAssertions(original);
        const stateHash = sha256(`${chain}\0${JSON.stringify(shot)}`);
        const legacyStateHash = sha256(`${legacy}\0${JSON.stringify(shot)}`);
        chain = sha256(`${chain}\0${JSON.stringify(stateParts(shot))}`);
        legacy = legacyStateHash;
        return { stateHash, legacyStateHash };
    });
}

// Is a manifest entry still good for this shot?
// environmentFingerprint: generator version, Playwright, capture profile,
//   hooks, screenshot identity. Any change re-renders.
// workspaceLook: what the Workspace looks like (the code-server version), or
//   'unknown' if it could not be read. An unknown look never counts as a
//   change: a failed docker inspect must not re-render everything twice.
// Entries written before workspace_look existed are accepted if their state
//   matches; they record the new fields when the manifest is written next.
export function entryIsCurrent(previous, shot, { environmentFingerprint, workspaceLook }) {
    if (!previous) return false;
    if (previous.workspace_look === undefined) {
        return previous.state_hash === shot.legacyStateHash || previous.state_hash === shot.stateHash;
    }
    if (previous.state_hash !== shot.stateHash) return false;
    if (previous.environment_fingerprint !== environmentFingerprint) return false;
    if (workspaceLook !== 'unknown' && previous.workspace_look !== 'unknown' &&
        previous.workspace_look !== workspaceLook) return false;
    return true;
}

// ffmpeg arguments that turn numbered PNG frames into a video the browser
// plays everywhere: H.264 in yuv420p for .mp4, VP9 for .webm. Even
// dimensions are required by yuv420p, so odd sizes lose one pixel.
export function videoEncodeArgs({ framePattern, fps, output }) {
    const common = ['-y', '-loglevel', 'error', '-framerate', `${fps}`, '-i', framePattern,
        '-vf', 'crop=trunc(iw/2)*2:trunc(ih/2)*2', '-pix_fmt', 'yuv420p', '-an'];
    if (output.toLowerCase().endsWith('.webm')) {
        return [...common, '-c:v', 'libvpx-vp9', '-b:v', '0', '-crf', '32', '-f', 'webm', output];
    }
    return [...common, '-c:v', 'libx264', '-preset', 'slow', '-crf', '20',
        '-movflags', '+faststart', '-f', 'mp4', output];
}
