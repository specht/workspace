import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';

import { gitGraphDot } from '../git-graph.js';

function repoWithTwoCommits() {
    const repo = fs.mkdtempSync(path.join(os.tmpdir(), 'git-graph-'));
    const git = (...args) => execFileSync('git', ['-C', repo, ...args], {
        env: {
            ...process.env,
            GIT_AUTHOR_NAME: 'Alice', GIT_AUTHOR_EMAIL: 'alice@example.org',
            GIT_COMMITTER_NAME: 'Alice', GIT_COMMITTER_EMAIL: 'alice@example.org',
            GIT_AUTHOR_DATE: '2026-01-26T18:03:10Z', GIT_COMMITTER_DATE: '2026-01-26T18:03:10Z',
        },
    }).toString();
    git('init', '-q', '-b', 'main');
    fs.writeFileSync(path.join(repo, 'hello.txt'), 'Hello, world!\n');
    git('add', 'hello.txt');
    git('commit', '-q', '-m', 'initial commit');
    git('mv', 'hello.txt', 'README.md');
    git('commit', '-q', '-m', 'rename hello.txt to README.md');
    return { repo, git };
}

test('the graph shows objects, HEAD on its branch, and fades older snapshots', () => {
    const { repo, git } = repoWithTwoCommits();
    const dot = gitGraphDot(repo);
    // »Hello, world!« is blob af5626b in every repository.
    assert.match(dot, /<B><FONT COLOR="#000000">blob af5626b<\/FONT><\/B>/);
    assert.match(dot, /"ref:HEAD" -> "ref:main" \[class="ref"\];/);
    // Classes for the dark theme (dark.css).
    assert.match(dot, /class="git-graph";/);
    assert.match(dot, /"ref:HEAD" \[class="ref head",/);
    assert.match(dot, /author \[\.\.\.\]/);
    assert.doesNotMatch(dot, /Alice|committer/);
    // The first commit's tree (only hello.txt) is no longer in the snapshot.
    const firstTree = git('rev-parse', 'HEAD~1^{tree}').trim();
    assert.match(dot, new RegExp(`"${firstTree}" \\[class="tree faded", label=<<TABLE BGCOLOR="#f3eee2"`));
    const head = git('rev-parse', 'HEAD').trim();
    assert.match(dot, new RegExp(`"${head}" \\[class="commit current",`));
    fs.rmSync(repo, { recursive: true, force: true });
});

test('a detached HEAD points at its commit', () => {
    const { repo, git } = repoWithTwoCommits();
    git('checkout', '-q', 'HEAD~1');
    const first = git('rev-parse', 'HEAD').trim();
    assert.match(gitGraphDot(repo), new RegExp(`"ref:HEAD" -> "${first}" \\[class="ref"\\];`));
    fs.rmSync(repo, { recursive: true, force: true });
});
