// The Git object graph of a repository as Graphviz DOT and SVG, for
// `git-graph:` recipes (README.md, »Git object graphs«). Port of the former
// src/content/git/git-obj-graph.rb:
//
// - every commit, tree and blob reachable from HEAD and from the branches
//   and tags, one column per type;
// - the snapshot HEAD stands for (its commit, tree and blobs, without older
//   history) in full colour, everything else faded;
// - HEAD, branches and tags as notes pointing at their commits (an attached
//   HEAD points at its branch).
//
// It only reads: git cat-file, rev-parse, show-ref and symbolic-ref. The
// repository belongs to the screenshot Workspace, so safe.directory is
// needed, and fsmonitor stays off so its config cannot run a command.

import { execFileSync } from 'node:child_process';

const COLORS = { commit: '#5bc0b5', tree: '#d8c59f', blob: '#e7e6e1' };

function gitRunner(repo) {
    return (args, { allowFailure = false } = {}) => {
        try {
            return execFileSync('git', ['-c', 'safe.directory=*', '-c', 'core.fsmonitor=false', '-C', repo, ...args], {
                encoding: 'utf8',
                stdio: ['ignore', 'pipe', 'pipe'],
                env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' },
            });
        } catch (error) {
            if (allowFailure) return '';
            throw new Error(`git ${args.join(' ')} failed in ${repo}: ${(error.stderr || error.message).toString().trim()}`);
        }
    };
}

function escapeHtml(text) {
    return String(text).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
}

function truncateLines(text, maxLines = 20, maxWidth = 80) {
    const raw = text.split('\n');
    if (raw.length && raw[raw.length - 1] === '') raw.pop();
    const lines = raw.slice(0, maxLines).map((line) => (line.length > maxWidth ? `${line.slice(0, maxWidth)}…` : line));
    if (raw.length > maxLines) lines.push('… (truncated)');
    const width = Math.max(0, ...lines.map((line) => line.length));
    return lines.map((line) => line.padEnd(width, ' '));
}

function lighten(hex, factor) {
    const value = hex.replace('#', '');
    const channels = [0, 2, 4].map((i) => parseInt(value.slice(i, i + 2), 16));
    return `#${channels.map((c) => Math.round(c + (255 - c) * factor).toString(16).padStart(2, '0')).join('')}`;
}

function labelHtml(title, text) {
    const body = truncateLines(text).map(escapeHtml).join('<BR ALIGN="LEFT"/>');
    return '<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0" ALIGN="LEFT">' +
        `<TR><TD ALIGN="LEFT"><B>${escapeHtml(title)}</B></TD></TR>` +
        `<TR><TD ALIGN="LEFT"><FONT FACE="monospace">${body}</FONT></TD></TR>` +
        '</TABLE>';
}

// Reads one object into nodes and edges and queues its children.
function processObject(git, oid, nodes, edges, queue) {
    const type = git(['cat-file', '-t', oid]).trim();
    const body = git(['cat-file', '-p', oid]);
    const short = oid.slice(0, 7);

    if (type === 'commit') {
        // Short tree and parent ids, no author details, no committer.
        const shown = body.split('\n').flatMap((line) => {
            if (line.startsWith('tree ') || line.startsWith('parent ')) {
                const [key, id] = line.split(/\s+/);
                return [`${key} ${id.slice(0, 7)}`];
            }
            if (line.startsWith('author ')) return ['author [...]'];
            if (line.startsWith('committer ')) return [];
            return [line];
        });
        nodes.set(oid, nodes.get(oid) ?? { type, label: labelHtml(`commit ${short}`, shown.join('\n')) });
        for (const line of body.split('\n')) {
            if (line.startsWith('tree ')) {
                const tree = line.split(/\s+/)[1];
                edges.add(JSON.stringify([oid, tree, 'tree']));
                queue.push(tree);
            } else if (line.startsWith('parent ')) {
                const parent = line.split(/\s+/)[1];
                edges.add(JSON.stringify([oid, parent, 'parent']));
                queue.push(parent);
            }
        }
    } else if (type === 'tree') {
        const entries = [];
        for (const line of body.split('\n')) {
            const match = line.match(/^(\d+)\s+(\w+)\s+([0-9a-f]{40})\t(.*)$/);
            if (!match) continue;
            const [, , objectType, child, name] = match;
            entries.push(`${objectType} ${name} (${child.slice(0, 7)})`);
            edges.add(JSON.stringify([oid, child, name]));
            queue.push(child);
        }
        nodes.set(oid, nodes.get(oid) ?? { type, label: labelHtml(`tree ${short}`, entries.join('\n')) });
    } else {
        nodes.set(oid, nodes.get(oid) ?? { type, label: labelHtml(`${type} ${short}`, body) });
    }
}

export function gitGraphDot(repo) {
    const git = gitRunner(repo);
    const start = git(['rev-parse', 'HEAD']).trim();

    // Branches, tags and HEAD.
    const refNodes = new Set();
    const refCommitEdges = new Map();
    const refRefEdges = new Set();
    for (const line of git(['show-ref', '--heads', '--tags'], { allowFailure: true }).split('\n')) {
        if (!line.trim()) continue;
        const [sha, ref] = line.trim().split(' ', 2);
        const name = ref.startsWith('refs/heads/') ? ref.slice('refs/heads/'.length)
            : ref.startsWith('refs/tags/') ? `tag: ${ref.slice('refs/tags/'.length)}`
                : ref;
        refNodes.add(name);
        refCommitEdges.set(`${name}\n${sha}`, [name, sha]);
    }
    const headRef = git(['symbolic-ref', '-q', 'HEAD'], { allowFailure: true }).trim();
    refNodes.add('HEAD');
    if (!headRef) {
        refCommitEdges.set(`HEAD\n${start}`, ['HEAD', start]);
    } else {
        const branch = headRef.replace('refs/heads/', '');
        if (branch) {
            refNodes.add(branch);
            refRefEdges.add(JSON.stringify(['HEAD', branch]));
        }
    }

    // Objects: first what HEAD reaches, then the rest of the repository.
    const nodes = new Map();
    const edges = new Set();
    const seen = new Set();
    const walk = (root) => {
        const queue = [root];
        while (queue.length) {
            const oid = queue.shift();
            if (seen.has(oid)) continue;
            seen.add(oid);
            processObject(git, oid, nodes, edges, queue);
        }
    };
    walk(start);
    for (const [, oid] of refCommitEdges.values()) walk(oid);

    // The snapshot: reachable from HEAD without going back in history.
    const edgeList = [...edges].map((edge) => JSON.parse(edge));
    const snapshot = new Set();
    const snapshotEdges = new Set();
    const queue = [start];
    while (queue.length) {
        const oid = queue.shift();
        if (snapshot.has(oid)) continue;
        snapshot.add(oid);
        for (const [from, to, label] of edgeList) {
            if (from !== oid || label === 'parent') continue;
            snapshotEdges.add(JSON.stringify([from, to, label]));
            queue.push(to);
        }
    }

    const out = ['digraph git_objects {', '  rankdir=LR;', '  node [shape=plain, fontname="monospace"];'];
    const columns = { commit: [], tree: [], blob: [], other: [] };
    for (const [oid, data] of nodes) {
        const base = COLORS[data.type] ?? '#ffffff';
        const current = snapshot.has(oid);
        const background = current ? base : lighten(base, 0.7);
        const border = current ? '#000000' : lighten('#000000', 0.7);
        const font = current ? '#000000' : lighten('#000000', 0.5);
        (columns[data.type] ?? columns.other).push(oid);
        const label = data.label
            .replace('<TABLE ', `<TABLE BGCOLOR="${background}" COLOR="${border}" `)
            .replaceAll('<B>', `<B><FONT COLOR="${font}">`)
            .replaceAll('</B>', '</FONT></B>')
            .replaceAll('<FONT FACE="monospace">', `<FONT FACE="monospace" COLOR="${font}">`);
        out.push(`  "${oid}" [label=<${label}>];`);
    }
    for (const ids of Object.values(columns)) {
        if (ids.length) out.push(`  { rank=same; ${ids.map((id) => `"${id}"`).join(' ')} };`);
    }
    for (const name of refNodes) {
        const fill = name === 'HEAD' ? '#fce94f' : 'white';
        out.push(`  "ref:${name}" [shape=note, style=filled, fillcolor="${fill}", label="${escapeHtml(name)}"];`);
    }
    for (const [from, to, label] of edgeList) {
        const color = snapshotEdges.has(JSON.stringify([from, to, label])) ? '#000000' : lighten('#000000', 0.7);
        out.push(`  "${from}" -> "${to}" [label="${escapeHtml(label)}", color="${color}"];`);
    }
    for (const [name, oid] of refCommitEdges.values()) out.push(`  "ref:${name}" -> "${oid}";`);
    for (const edge of refRefEdges) {
        const [from, to] = JSON.parse(edge);
        out.push(`  "ref:${from}" -> "ref:${to}";`);
    }
    out.push('}');
    return `${out.join('\n')}\n`;
}

export function gitGraphSvg(repo) {
    return execFileSync('dot', ['-Tsvg'], { input: gitGraphDot(repo), encoding: 'utf8' });
}
