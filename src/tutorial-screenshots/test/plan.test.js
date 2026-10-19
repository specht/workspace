import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';

import {
    chainStateHashes,
    checkGitGraphForTarget,
    checkRecordForTarget,
    entryIsCurrent,
    findRecipeTarget,
    frameTimes,
    parseFps,
    parseRecordSeconds,
    parseVariants,
    sha256,
    variantTargets,
    videoEncodeArgs,
} from '../plan.js';

const shot = (target, extra = {}) => ({
    target,
    tab: 'preview',
    viewport: { width: 1853, height: 929 },
    zoom: 1,
    cropTop: 0,
    cropBottom: 0,
    actions: [{ type: 'preview-reload' }],
    ...extra,
});

test('the target is the next img, video or source before the next recipe', () => {
    assert.equal(findRecipeTarget("\n<img class='full' src='a.webp' alt=''>").src, 'a.webp');
    assert.equal(findRecipeTarget("\n<video src='animation1.mp4' autoplay loop muted></video>").src, 'animation1.mp4');
    assert.equal(findRecipeTarget('\n<video controls>\n<source src="video/train.webm" type="video/webm">').src, 'video/train.webm');
    assert.equal(findRecipeTarget("\n<!-- tutorial-screenshot\nx\n-->\n<img src='b.webp'>"), null);
    assert.equal(findRecipeTarget('\nnothing here'), null);
});

test('an _include_svg is a target too, whichever comes first', () => {
    assert.equal(findRecipeTarget('\n_include_svg(graph3.svg)\n').src, 'graph3.svg');
    assert.equal(findRecipeTarget("\n_include_svg(graph3.svg)\n<img src='a.webp'>").src, 'graph3.svg');
    assert.equal(findRecipeTarget("\n<img src='a.webp'>\n_include_svg(graph3.svg)").src, 'a.webp');
    assert.equal(findRecipeTarget('\n<!-- tutorial-screenshot\nx\n-->\n_include_svg(g.svg)'), null);
});

test('git-graph: and SVG targets belong together', () => {
    assert.doesNotThrow(() => checkGitGraphForTarget('graph3.svg', 'git-tutorial/alice'));
    assert.doesNotThrow(() => checkGitGraphForTarget('a.webp', undefined));
    assert.throws(() => checkGitGraphForTarget('graph3.svg', undefined), /needs git-graph/);
    assert.throws(() => checkGitGraphForTarget('a.webp', 'git-tutorial/alice'), /_include_svg/);
});

test('record and fps are checked, and only videos record', () => {
    assert.equal(parseRecordSeconds('8s'), 8);
    assert.equal(parseRecordSeconds(' 2.5s'), 2.5);
    assert.throws(() => parseRecordSeconds('8'), /record must be/);
    assert.throws(() => parseRecordSeconds('120s'), /record must be/);
    assert.equal(parseFps('30'), 30);
    assert.throws(() => parseFps('0'), /fps/);
    assert.throws(() => parseFps('24.5'), /fps/);
    assert.throws(() => checkRecordForTarget('animation1.mp4', null), /needs record/);
    assert.throws(() => checkRecordForTarget('sphere.webp', 8), /needs a <video>/);
    checkRecordForTarget('animation1.mp4', 8);
    checkRecordForTarget('sphere.webp', null);
});

test('variants name one file per combination, in recipe order', () => {
    assert.deepEqual(variantTargets('box.webp', ['dark', 'anaglyph']), [
        { on: ['dark'], target: 'box-dark.webp' },
        { on: ['anaglyph'], target: 'box-anaglyph.webp' },
        { on: ['dark', 'anaglyph'], target: 'box-dark-anaglyph.webp' },
    ]);
    assert.deepEqual(variantTargets('a/spin.mp4', ['dark']), [{ on: ['dark'], target: 'a/spin-dark.mp4' }]);
    assert.deepEqual(variantTargets('box.webp', undefined), []);
    assert.deepEqual(parseVariants(' dark  anaglyph '), ['dark', 'anaglyph']);
    assert.throws(() => parseVariants('dark dark'), /twice/);
    assert.throws(() => parseVariants('Dark'), /Invalid/);
    assert.throws(() => parseVariants('a b c d'), /one to three/);
});

test('frames start at zero and are evenly spaced', () => {
    assert.deepEqual(frameTimes(0.1, 30), [0, 1 / 30, 2 / 30]);
    assert.equal(frameTimes(12, 30).length, 360);
    assert.equal(frameTimes(0.01, 30).length, 1);
});

test('a crop change re-renders only its own shot', () => {
    const before = [shot('a.webp'), shot('b.webp'), shot('c.webp')];
    const after = [shot('a.webp', { cropTop: 0.3 }), shot('b.webp'), shot('c.webp')];
    const h1 = chainStateHashes(before, 'seed');
    const h2 = chainStateHashes(after, 'seed');
    assert.notEqual(h1[0].stateHash, h2[0].stateHash);
    assert.equal(h1[1].stateHash, h2[1].stateHash);
    assert.equal(h1[2].stateHash, h2[2].stateHash);
});

test('an action change re-renders its shot and every later one', () => {
    const before = [shot('a.webp'), shot('b.webp'), shot('c.webp')];
    const after = [shot('a.webp'), shot('b.webp', { actions: [{ type: 'go-live' }] }), shot('c.webp')];
    const h1 = chainStateHashes(before, 'seed');
    const h2 = chainStateHashes(after, 'seed');
    assert.equal(h1[0].stateHash, h2[0].stateHash);
    assert.notEqual(h1[1].stateHash, h2[1].stateHash);
    assert.notEqual(h1[2].stateHash, h2[2].stateHash);
});

test('legacy hashes are the chain generator 19 wrote', () => {
    const shots = [shot('a.webp'), shot('b.webp')];
    const first = sha256(`seed\0${JSON.stringify(shots[0])}`);
    const second = sha256(`${first}\0${JSON.stringify(shots[1])}`);
    const hashes = chainStateHashes(shots, 'seed');
    assert.equal(hashes[0].legacyStateHash, first);
    assert.equal(hashes[1].legacyStateHash, second);
});

test('a rebuilt workspace image alone does not make a shot stale', () => {
    const current = { stateHash: 's', legacyStateHash: 'l' };
    const env = { environmentFingerprint: 'e', workspaceLook: '4.138.0-ls300' };
    const entry = { state_hash: 's', environment_fingerprint: 'e', workspace_look: '4.138.0-ls300' };
    assert.ok(entryIsCurrent(entry, current, env));
    // docker inspect failed this time, or last time: not a change.
    assert.ok(entryIsCurrent(entry, current, { ...env, workspaceLook: 'unknown' }));
    assert.ok(entryIsCurrent({ ...entry, workspace_look: 'unknown' }, current, env));
    // A new code-server release is.
    assert.ok(!entryIsCurrent(entry, current, { ...env, workspaceLook: '4.139.0-ls301' }));
    // a new packaging revision of the same code-server is the same look
    assert.ok(entryIsCurrent(entry, current, { ...env, workspaceLook: '4.138.0-ls367' }));
    assert.ok(entryIsCurrent(entry, current, { ...env, workspaceLook: '4.138.0' }));
    assert.ok(!entryIsCurrent(entry, current, { ...env, environmentFingerprint: 'x' }));
    assert.ok(!entryIsCurrent({ ...entry, state_hash: 'other' }, current, env));
    assert.ok(!entryIsCurrent(undefined, current, env));
});

test('a new code-server release leaves what code-server does not draw', () => {
    const env = { environmentFingerprint: 'e', workspaceLook: '4.140.0-ls368' };
    const entry = { state_hash: 's', environment_fingerprint: 'e', workspace_look: '4.138.0-ls366' };
    const shot = extra => ({ stateHash: 's', legacyStateHash: 'l', ...extra });
    assert.ok(!entryIsCurrent(entry, shot({ tab: 'workspace' }), env));
    assert.ok(!entryIsCurrent(entry, shot({}), env));
    // the preview (anaglyph animations, a student's page), PDF pages, image files, Git graphs
    assert.ok(entryIsCurrent(entry, shot({ tab: 'preview', record: 6 }), env));
    assert.ok(entryIsCurrent(entry, shot({ pdfPage: 1 }), env));
    assert.ok(entryIsCurrent(entry, shot({ imageFile: 'out.png' }), env));
    assert.ok(entryIsCurrent(entry, shot({ gitGraph: 'main' }), env));
    // their own changes still count
    assert.ok(!entryIsCurrent({ ...entry, state_hash: 'other' }, shot({ tab: 'preview' }), env));
});

test('entries from generator 19 are kept if their state matches', () => {
    const current = { stateHash: 's', legacyStateHash: 'l' };
    const env = { environmentFingerprint: 'e', workspaceLook: 'unknown' };
    assert.ok(entryIsCurrent({ state_hash: 'l', environment_fingerprint: 'old' }, current, env));
    assert.ok(!entryIsCurrent({ state_hash: 'zzz', environment_fingerprint: 'old' }, current, env));
});

test('frames become a playable video', { skip: spawnSync('ffmpeg', ['-version']).status !== 0 && 'ffmpeg missing' }, () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'plan-video-'));
    try {
        for (let i = 0; i < 6; i += 1) {
            // Odd sizes on purpose: yuv420p needs even ones.
            execFileSync('ffmpeg', ['-loglevel', 'error', '-f', 'lavfi', '-i', `color=c=0x${(i * 40).toString(16).padStart(2, '0')}2020:s=101x51`,
                '-frames:v', '1', path.join(dir, `frame-${String(i).padStart(5, '0')}.png`)]);
        }
        for (const name of ['out.mp4', 'out.webm']) {
            const output = path.join(dir, name);
            execFileSync('ffmpeg', videoEncodeArgs({ framePattern: path.join(dir, 'frame-%05d.png'), fps: 30, output }));
            const probe = execFileSync('ffprobe', ['-v', 'error', '-select_streams', 'v:0', '-count_frames',
                '-show_entries', 'stream=width,height,nb_read_frames', '-of', 'csv=p=0', output]).toString().trim();
            assert.equal(probe, '100,50,6', name);
        }
    } finally {
        fs.rmSync(dir, { recursive: true, force: true });
    }
});

test('assertions are not part of any hash, so adding one re-renders nothing', () => {
    const shot = actions => ({ target: 'a.webp', tab: 'workspace', viewport: { width: 1, height: 1 }, zoom: 1, actions });
    const plain = [shot([{ type: 'press', key: 'Enter' }]), shot([{ type: 'sleep', seconds: 1 }])];
    const checked = [
        shot([{ type: 'press', key: 'Enter' }, { type: 'check-task', task: 'demo/eins', language: null }]),
        shot([{ type: 'expect-no-text', text: 'Traceback' }, { type: 'sleep', seconds: 1 }, { type: 'check-task-fails', task: 'demo/zwei', language: 'python' }]),
    ];
    assert.deepEqual(chainStateHashes(checked, 'seed'), chainStateHashes(plain, 'seed'));
});

test('a tutorial-scope keeps the shots after it independent of the shots inside', () => {
    const shot = (target, actions, scope) => ({ target, tab: 'workspace', viewport: { width: 1, height: 1 }, zoom: 1, actions, scope });
    const scopeOf = s => (s.scope === undefined ? null : { scope: s.scope, entry: s.entry || 0 });
    const strip = shots => shots.map(({ scope, entry, ...rest }) => rest);
    const before = shot('a.webp', [{ type: 'press', key: 'Enter' }]);
    const after = shot('c.webp', [{ type: 'sleep', seconds: 1 }]);
    const inside = actions => shot('b.webp', actions, 0);
    const one = chainStateHashes([before, inside([{ type: 'type', text: 'snowman' }]), after], 'seed', scopeOf);
    const two = chainStateHashes([before, inside([{ type: 'type', text: 'ice cream' }]), after], 'seed', scopeOf);
    // the shot inside changes, the one after does not
    assert.notEqual(one[1].stateHash, two[1].stateHash);
    assert.equal(one[2].stateHash, two[2].stateHash);
    // and the shot after hashes as if the scope were not there
    const without = chainStateHashes(strip([before, after]), 'seed');
    assert.equal(one[2].stateHash, without[1].stateHash);
    assert.equal(one[0].stateHash, without[0].stateHash);
    // without scopes nothing changes
    const plain = [before, inside([{ type: 'type', text: 'snowman' }]), after].map(s => ({ ...s, scope: undefined }));
    assert.deepEqual(chainStateHashes(plain, 'seed', scopeOf), chainStateHashes(plain, 'seed'));
    // steps from before the scope (entry) count for the shots after it
    const entered = (text) => ({ ...inside([{ type: 'press', key: text }, { type: 'type', text: 'x' }]), entry: 1 });
    const a = chainStateHashes([before, entered('a'), after], 'seed', scopeOf);
    const b = chainStateHashes([before, entered('b'), after], 'seed', scopeOf);
    assert.notEqual(a[2].stateHash, b[2].stateHash);
});

test('a taller Workspace window makes only Workspace shots stale', () => {
    const env = { environmentFingerprint: 'e', workspaceLook: '4.140.0-ls368', workspaceWindow: 560 };
    const entry = { state_hash: 's', environment_fingerprint: 'e', workspace_look: '4.140.0-ls368' };
    const shot = extra => ({ stateHash: 's', legacyStateHash: 'l', ...extra });
    // taken before the height was recorded, or at another height
    assert.ok(!entryIsCurrent(entry, shot({ tab: 'workspace' }), env));
    assert.ok(!entryIsCurrent({ ...entry, workspace_window: 516 }, shot({}), env));
    assert.ok(entryIsCurrent({ ...entry, workspace_window: 560 }, shot({}), env));
    // previews (anaglyph animations), PDF pages, image files, Git graphs keep theirs
    assert.ok(entryIsCurrent(entry, shot({ tab: 'preview', record: 6 }), env));
    assert.ok(entryIsCurrent(entry, shot({ pdfPage: 1 }), env));
    assert.ok(entryIsCurrent(entry, shot({ gitGraph: 'main' }), env));
});
