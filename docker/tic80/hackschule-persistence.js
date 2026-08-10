/*
 * Hackschule persistence bridge for the TIC-80 Emscripten build.
 *
 * The browser filesystem is deliberately just MEMFS.  The authoritative tree
 * is loaded from and synchronized to /workspace/TIC-80 through the Workspace
 * server API.
 */
(() => {
    const module = globalThis.Module = globalThis.Module || {};

    let root = null;
    let revision = null;
    let retryDelay = 1000;
    let retryTimer = null;
    let syncBlocked = false;

    function statusBox(message, kind = 'error') {
        let box = document.getElementById('hs-tic80-storage-status');
        if (!box) {
            box = document.createElement('div');
            box.id = 'hs-tic80-storage-status';
            Object.assign(box.style, {
                position: 'fixed',
                left: '12px',
                right: '12px',
                bottom: '12px',
                zIndex: '10000',
                padding: '10px 12px',
                font: '14px sans-serif',
                borderRadius: '6px',
                boxShadow: '0 2px 12px rgba(0,0,0,.35)',
            });
            document.body.appendChild(box);
        }

        box.style.background = kind === 'warning' ? '#5d4900' : '#702020';
        box.style.color = '#fff';
        box.textContent = message;
    }

    function clearStatusBox() {
        document.getElementById('hs-tic80-storage-status')?.remove();
    }

    function safeRelative(path) {
        if (typeof path !== 'string') throw new Error('Invalid TIC-80 path');
        if (path.includes('\0')) throw new Error('Invalid TIC-80 path');

        path = path.replaceAll('\\', '/');
        if (path.startsWith('/')) throw new Error(`Absolute TIC-80 path: ${path}`);

        const parts = path.split('/').filter(Boolean);
        if (!parts.length || parts.some(part => part === '.' || part === '..')) {
            throw new Error(`Unsafe TIC-80 path: ${path}`);
        }

        return parts.join('/');
    }

    function fullPath(relative) {
        return `${root}/${safeRelative(relative)}`;
    }

    function dirname(path) {
        const i = path.lastIndexOf('/');
        return i <= 0 ? '/' : path.slice(0, i);
    }

    function removeTree(path) {
        const stat = FS.lstat(path);
        if (!FS.isDir(stat.mode)) {
            FS.unlink(path);
            return;
        }

        for (const name of FS.readdir(path)) {
            if (name === '.' || name === '..') continue;
            removeTree(`${path}/${name}`);
        }
        FS.rmdir(path);
    }

    function clearRoot() {
        for (const name of FS.readdir(root)) {
            if (name === '.' || name === '..') continue;
            removeTree(`${root}/${name}`);
        }
    }

    function base64ToBytes(value) {
        const binary = atob(value);
        const bytes = new Uint8Array(binary.length);
        for (let i = 0; i < binary.length; ++i) {
            bytes[i] = binary.charCodeAt(i);
        }
        return bytes;
    }

    function bytesToBase64(bytes) {
        const chunkSize = 0x8000;
        let binary = '';
        for (let i = 0; i < bytes.length; i += chunkSize) {
            binary += String.fromCharCode(...bytes.subarray(i, i + chunkSize));
        }
        return btoa(binary);
    }

    function mtimeMillis(stat) {
        if (stat.mtime instanceof Date) return stat.mtime.getTime();
        const value = Number(stat.mtime);
        return Number.isFinite(value) ? value : Date.now();
    }

    function snapshot() {
        const dirs = [];
        const files = [];

        function visit(path, relative) {
            for (const name of FS.readdir(path)) {
                if (name === '.' || name === '..') continue;

                const childPath = `${path}/${name}`;
                const childRelative = relative ? `${relative}/${name}` : name;
                const stat = FS.lstat(childPath);

                if (FS.isDir(stat.mode)) {
                    dirs.push(childRelative);
                    visit(childPath, childRelative);
                } else if (FS.isFile(stat.mode)) {
                    const contents = FS.readFile(childPath);
                    files.push({
                        path: childRelative,
                        mtime_ms: mtimeMillis(stat),
                        contents: bytesToBase64(contents),
                    });
                } else {
                    throw new Error(`Unsupported TIC-80 filesystem entry: ${childRelative}`);
                }
            }
        }

        visit(root, '');
        dirs.sort();
        files.sort((a, b) => a.path.localeCompare(b.path));

        return {dirs, files};
    }

    function clearRetryTimer() {
        if (retryTimer !== null) {
            clearTimeout(retryTimer);
            retryTimer = null;
        }
        retryDelay = 1000;
    }

    function scheduleRetry() {
        if (retryTimer !== null || syncBlocked) return;

        const delay = retryDelay;
        retryDelay = Math.min(retryDelay * 2, 30000);
        retryTimer = setTimeout(() => {
            retryTimer = null;
            module.syncFSRequests = (module.syncFSRequests || 0) + 1;
        }, delay);
    }

    module.hsTic80Load = function(dir, done) {
        root = dir.replace(/\/+$/, '');
        syncBlocked = false;

        (async () => {
            try {
                const response = await fetch('/api/tic80/fs', {
                    method: 'GET',
                    credentials: 'same-origin',
                    cache: 'no-store',
                });

                if (!response.ok) {
                    throw new Error(`TIC-80 load failed: HTTP ${response.status}`);
                }

                const tree = await response.json();
                if (!/^[0-9a-f]{64}$/.test(tree.revision || '')) {
                    throw new Error('TIC-80 server returned an invalid revision');
                }
                if (!Array.isArray(tree.dirs) || !Array.isArray(tree.files)) {
                    throw new Error('TIC-80 server returned an invalid filesystem tree');
                }

                clearRoot();

                for (const relative of [...tree.dirs].sort((a, b) =>
                    a.split('/').length - b.split('/').length || a.localeCompare(b))) {
                    FS.mkdirTree(fullPath(relative));
                }

                for (const entry of tree.files) {
                    const path = fullPath(entry.path);
                    FS.mkdirTree(dirname(path));
                    FS.writeFile(path, base64ToBytes(entry.contents), {canOwn: true});

                    if (Number.isFinite(Number(entry.mtime_ms))) {
                        const time = new Date(Number(entry.mtime_ms));
                        FS.utime(path, time, time);
                    }
                }

                revision = tree.revision;
                module.hsTic80Revision = revision;
                clearRetryTimer();
                clearStatusBox();
                done();
            } catch (error) {
                console.error(error);
                statusBox(
                    'TIC-80 could not load its Workspace files. Nothing has been started, so no files can be overwritten. Reload this page to try again.'
                );
                // Deliberately do not call done(): starting with an empty MEMFS
                // would risk overwriting the authoritative Workspace tree later.
            }
        })();
    };

    module.hsTic80Sync = function(done) {
        if (syncBlocked || !root || !revision) {
            done();
            return;
        }

        (async () => {
            try {
                const tree = snapshot();
                const response = await fetch('/api/tic80/fs_sync', {
                    method: 'POST',
                    credentials: 'same-origin',
                    cache: 'no-store',
                    headers: {'Content-Type': 'application/json'},
                    body: JSON.stringify({
                        base_revision: revision,
                        dirs: tree.dirs,
                        files: tree.files,
                    }),
                });

                if (response.status === 409) {
                    syncBlocked = true;
                    const conflict = await response.json().catch(() => ({}));
                    console.error('TIC-80 filesystem conflict', conflict);
                    statusBox(
                        'TIC-80 files changed outside this window. Your browser copy was NOT written over them. Reload TIC-80 before continuing.',
                        'warning'
                    );
                    return;
                }

                if (!response.ok) {
                    throw new Error(`TIC-80 sync failed: HTTP ${response.status}`);
                }

                const result = await response.json();
                if (!/^[0-9a-f]{64}$/.test(result.revision || '')) {
                    throw new Error('TIC-80 server returned an invalid sync revision');
                }

                revision = result.revision;
                module.hsTic80Revision = revision;
                clearRetryTimer();
                clearStatusBox();
            } catch (error) {
                console.error(error);
                statusBox('TIC-80 could not save to the Workspace. Retrying automatically…', 'warning');
                scheduleRetry();
            } finally {
                done();
            }
        })();
    };
})();
