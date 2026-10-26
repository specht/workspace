/*
 * Hackschule persistence bridge for the TIC-80 Emscripten build
 * (build-tic80.sh puts it in front of tic80.js).
 *
 * TIC-80 works on an in-memory filesystem (MEMFS). The real files are in the
 * person's Workspace under /workspace/TIC-80, where VS Code, Git and the task
 * checks see them too (server: src/ruby/include/tic80_storage.rb).
 *
 * - Loading fills MEMFS from the Workspace and remembers, per file, the
 *   version that came from there (bytes and SHA-256).
 * - When TIC-80 has written something (it asks for a sync after each
 *   write), only the files that differ from that version go to the server,
 *   each with the version it started from. The server writes a file only if
 *   it is still that version on disk; otherwise it keeps the file and puts
 *   TIC-80's version next to it (»spiel (TIC-80).lua«). Unrelated changes
 *   elsewhere in the folder never block a save.
 * - The version a file »started from« is the one TIC-80 last read: a cart
 *   loaded before someone edited it in VS Code counts as based on the old
 *   version, so saving it does not silently overwrite the edit.
 * - When the window gets the focus back, files changed elsewhere (and not in
 *   TIC-80) are read again, so »load« sees an edit made in VS Code.
 */
(() => {
    const module = globalThis.Module = globalThis.Module || {};

    let root = null;
    let loaded = false;
    const base = new Map();       // path -> {sha, bytes}: as on the server
    const readBase = new Map();   // path -> sha of the version TIC-80 last read
    let baseDirs = new Set();
    let queue = Promise.resolve();
    let retryDelay = 1000;
    let retryTimer = null;

    // --- messages ------------------------------------------------------------

    function statusBox(message, kind, closable) {
        let box = document.getElementById('hs-tic80-storage-status');
        if (!box) {
            box = document.createElement('div');
            box.id = 'hs-tic80-storage-status';
            box.setAttribute('role', 'status');
            Object.assign(box.style, {
                position: 'fixed', left: '12px', right: '12px', bottom: '12px', zIndex: '10000',
                padding: '10px 40px 10px 12px', font: '15px sans-serif', lineHeight: '1.4',
                borderRadius: '6px', boxShadow: '0 2px 12px rgba(0,0,0,.35)', color: '#fff',
            });
            document.body.appendChild(box);
        }
        box.style.background = kind === 'warning' ? '#5d4900' : kind === 'info' ? '#1d4f6e' : '#702020';
        box.textContent = message;
        if (closable) {
            const x = document.createElement('button');
            x.type = 'button';
            // the site's icon (index.html loads them), alone in the button
            // and in its middle
            x.innerHTML = "<i class='bi bi-x-lg' aria-hidden='true' style='margin: 0;'></i>";
            x.setAttribute('aria-label', 'Schließen');
            Object.assign(x.style, {
                position: 'absolute', right: '8px', top: '4px', border: 'none', background: 'none',
                color: '#fff', fontSize: '18px', cursor: 'pointer',
                display: 'flex', alignItems: 'center', justifyContent: 'center', width: '28px', height: '28px', padding: '0',
            });
            x.addEventListener('click', () => {
                box.remove();
                document.getElementById('canvas')?.focus();
            });
            box.appendChild(x);
        }
    }

    function clearStatusBox() {
        document.getElementById('hs-tic80-storage-status')?.remove();
    }

    // --- paths and bytes -----------------------------------------------------

    function safeRelative(path) {
        if (typeof path !== 'string' || path.includes('\0')) throw new Error('Invalid TIC-80 path');
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

    function relativeOf(path) {
        if (!root || typeof path !== 'string') return null;
        const prefix = root + '/';
        return path.startsWith(prefix) ? path.slice(prefix.length) : null;
    }

    function dirname(path) {
        const i = path.lastIndexOf('/');
        return i <= 0 ? '/' : path.slice(0, i);
    }

    function base64ToBytes(value) {
        const binary = atob(value);
        const bytes = new Uint8Array(binary.length);
        for (let i = 0; i < binary.length; ++i) bytes[i] = binary.charCodeAt(i);
        return bytes;
    }

    function bytesToBase64(bytes) {
        let binary = '';
        for (let i = 0; i < bytes.length; i += 0x8000) {
            binary += String.fromCharCode(...bytes.subarray(i, i + 0x8000));
        }
        return btoa(binary);
    }

    function sameBytes(a, b) {
        if (a.length !== b.length) return false;
        for (let i = 0; i < a.length; ++i) if (a[i] !== b[i]) return false;
        return true;
    }

    function mtimeMillis(stat) {
        if (stat.mtime instanceof Date) return stat.mtime.getTime();
        const value = Number(stat.mtime);
        return Number.isFinite(value) ? value : Date.now();
    }

    // What MEMFS holds now: {files: Map(path -> {bytes, mtime_ms}), dirs: Set}
    function snapshot() {
        const files = new Map();
        const dirs = new Set();
        (function visit(path, relative) {
            for (const name of FS.readdir(path)) {
                if (name === '.' || name === '..') continue;
                const childPath = `${path}/${name}`;
                const childRelative = relative ? `${relative}/${name}` : name;
                const stat = FS.lstat(childPath);
                if (FS.isDir(stat.mode)) {
                    dirs.add(childRelative);
                    visit(childPath, childRelative);
                } else if (FS.isFile(stat.mode)) {
                    files.set(childRelative, { bytes: FS.readFile(childPath), mtime_ms: mtimeMillis(stat) });
                }
            }
        })(root, '');
        return { files, dirs };
    }

    function writeMemfs(relative, bytes, mtimeMs) {
        const path = fullPath(relative);
        FS.mkdirTree(dirname(path));
        FS.writeFile(path, bytes, { canOwn: true });
        if (Number.isFinite(Number(mtimeMs))) {
            const time = new Date(Number(mtimeMs));
            FS.utime(path, time, time);
        }
    }

    function removeMemfs(relative) {
        try { FS.unlink(fullPath(relative)); } catch (e) { /* already gone */ }
    }

    // Which version TIC-80 read: remember it when it opens a file for reading.
    function watchReads() {
        if (FS.__hsWatched) return;
        FS.__hsWatched = true;
        const open = FS.open;
        FS.open = function(path, flags) {
            const stream = open.apply(this, arguments);
            try {
                const readOnly = flags === 'r' || flags === 0 || (typeof flags === 'number' && (flags & 3) === 0);
                const relative = readOnly ? relativeOf(typeof path === 'string' ? path : stream.path) : null;
                if (relative && base.has(relative)) readBase.set(relative, base.get(relative).sha);
            } catch (e) { /* never let bookkeeping break a read */ }
            return stream;
        };
    }

    // --- server --------------------------------------------------------------

    async function api(path, body) {
        const response = await fetch(path, body === undefined
            ? { method: 'GET', credentials: 'same-origin', cache: 'no-store' }
            : { method: 'POST', credentials: 'same-origin', cache: 'no-store',
                headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
        if (!response.ok) {
            const error = new Error(`TIC-80 storage: HTTP ${response.status}`);
            error.status = response.status;
            throw error;
        }
        return response.json();
    }

    // Only one load, sync or refresh at a time.
    function serially(work) {
        const next = queue.then(work, work);
        queue = next.catch(() => {});
        return next;
    }

    // --- refresh: files changed elsewhere -------------------------------------

    // force: paths to take from the Workspace even if TIC-80 changed them
    // (after a clash, the Workspace's version stays at the path).
    async function refresh(force = new Set()) {
        const tree = await api('/api/tic80/fs?meta=1');
        const now = snapshot();
        const wanted = [];
        const onServer = new Map(tree.files.map(f => [f.path, f]));

        for (const [path, entry] of onServer) {
            const known = base.get(path);
            const local = now.files.get(path);
            const untouched = known && local && sameBytes(local.bytes, known.bytes);
            if (force.has(path)) wanted.push(path);
            else if (known && known.sha === entry.sha256) continue;
            else if (untouched || (!known && !local)) wanted.push(path);
        }
        for (const [path, known] of base) {
            if (onServer.has(path)) continue;
            const local = now.files.get(path);
            if (!force.has(path) && local && sameBytes(local.bytes, known.bytes)) {
                removeMemfs(path);
                base.delete(path);
            }
        }
        for (const dir of tree.dirs) if (!now.dirs.has(dir)) FS.mkdirTree(fullPath(dir));
        baseDirs = new Set(tree.dirs);

        if (wanted.length) {
            const withContents = await api('/api/tic80/fs_read', { paths: wanted });
            for (const entry of withContents.files) {
                if (entry.contents === undefined) continue;
                const bytes = base64ToBytes(entry.contents);
                writeMemfs(entry.path, bytes, entry.mtime_ms);
                base.set(entry.path, { sha: entry.sha256, bytes: new Uint8Array(bytes) });
            }
        }
    }

    // --- load ----------------------------------------------------------------

    module.hsTic80Load = function(dir, done) {
        root = dir.replace(/\/+$/, '');
        module.hsTic80Root = root;

        serially(async () => {
            try {
                const tree = await api('/api/tic80/fs');
                if (!Array.isArray(tree.dirs) || !Array.isArray(tree.files)) {
                    throw new Error('TIC-80 server returned an invalid filesystem tree');
                }
                for (const relative of [...tree.dirs].sort((a, b) =>
                    a.split('/').length - b.split('/').length || a.localeCompare(b))) {
                    FS.mkdirTree(fullPath(relative));
                }
                for (const entry of tree.files) {
                    const bytes = base64ToBytes(entry.contents);
                    writeMemfs(entry.path, bytes, entry.mtime_ms);
                    base.set(entry.path, { sha: entry.sha256, bytes: new Uint8Array(bytes) });
                }
                baseDirs = new Set(tree.dirs);
                watchReads();
                loaded = true;
                clearStatusBox();
                done();
            } catch (error) {
                console.error(error);
                statusBox('TIC-80 konnte deine Dateien aus dem Workspace nicht laden. '
                    + 'Es ist nichts gestartet und nichts überschrieben. Lade die Seite neu, um es noch einmal zu versuchen.');
                // Deliberately do not call done(): an empty MEMFS must never
                // become something to save.
            }
        });
    };

    // --- sync ----------------------------------------------------------------

    function scheduleRetry() {
        if (retryTimer !== null) return;
        const delay = retryDelay;
        retryDelay = Math.min(retryDelay * 2, 30000);
        retryTimer = setTimeout(() => {
            retryTimer = null;
            module.syncFSRequests = (module.syncFSRequests || 0) + 1;
        }, delay);
    }

    async function syncNow() {
        const now = snapshot();
        const changes = [];
        const sent = new Map();

        for (const dir of now.dirs) if (!baseDirs.has(dir)) changes.push({ path: dir, dir: true });
        for (const [path, local] of now.files) {
            const known = base.get(path);
            if (known && sameBytes(local.bytes, known.bytes)) continue;
            const from = readBase.has(path) ? readBase.get(path) : (known ? known.sha : null);
            changes.push({ path, base: from, contents: bytesToBase64(local.bytes), mtime_ms: local.mtime_ms });
            sent.set(path, local.bytes);
        }
        for (const [path, known] of base) {
            if (!now.files.has(path)) changes.push({ path, base: known.sha, deleted: true });
        }
        for (const dir of baseDirs) if (!now.dirs.has(dir)) changes.push({ path: dir, dir: true, deleted: true });
        if (!changes.length) return;

        const result = await api('/api/tic80/fs_changes', { changes });

        for (const [path, sha] of Object.entries(result.written || {})) {
            if (sent.has(path)) {
                base.set(path, { sha, bytes: new Uint8Array(sent.get(path)) });
                readBase.set(path, sha);
            }
        }
        for (const path of result.deleted || []) {
            base.delete(path);
            readBase.delete(path);
        }
        baseDirs = new Set([...now.dirs]);

        const conflicts = result.conflicts || [];
        if (conflicts.length) {
            // The Workspace's version stays at the path; TIC-80 gets it too,
            // and its own version under the new name.
            for (const c of conflicts) {
                readBase.delete(c.path);
                base.delete(c.path);
                if (c.saved_as && sent.has(c.path)) {
                    writeMemfs(c.saved_as, sent.get(c.path));
                    base.set(c.saved_as, { sha: result.written[c.saved_as], bytes: new Uint8Array(sent.get(c.path)) });
                }
            }
            await refresh(new Set(conflicts.map(c => c.path)));
            const text = conflicts.map(c => c.saved_as
                ? `${c.path} wurde außerhalb von TIC-80 geändert. Dein Stand aus TIC-80 liegt jetzt in ${c.saved_as}.`
                : `${c.path} wurde außerhalb von TIC-80 geändert und deshalb nicht gelöscht.`).join(' ');
            statusBox(text, 'info', true);
        } else {
            clearStatusBox();
        }
    }

    module.hsTic80Sync = function(done) {
        if (!loaded) {
            done();
            return;
        }
        serially(async () => {
            try {
                await syncNow();
                retryDelay = 1000;
            } catch (error) {
                console.error(error);
                statusBox('TIC-80 konnte gerade nicht im Workspace speichern. Ich versuche es gleich noch einmal …', 'warning');
                scheduleRetry();
            } finally {
                done();
            }
        });
    };

    function refreshSoon() {
        if (!loaded) return;
        serially(() => refresh().catch(error => console.error(error)));
    }

    window.addEventListener('focus', refreshSoon);
    document.addEventListener('visibilitychange', () => {
        if (document.visibilityState === 'visible') refreshSoon();
    });
})();
