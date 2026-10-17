// Hand-ins in the browser (class page): a click on a hand-in opens a viewer
// over the page with its files, the source code and, for web pages, the
// page itself. What students wrote runs only on abgaben.<host>, sandboxed
// (SubmissionView in src/ruby/include/submission_view.rb); this script only
// shows text and points frames at that host.
(function () {
    const IMAGE = /\.(png|jpe?g|gif|webp|svg|ico)$/i;
    const MEDIA = /\.(mp3|ogg|wav|mp4|webm)$/i;
    const PDF = /\.pdf$/i;
    let viewer = null;
    let lastFocus = null;

    function el(tag, className, text) {
        const e = document.createElement(tag);
        if (className) e.className = className;
        if (text !== undefined) e.textContent = text;
        return e;
    }

    // An icon for a file, by its kind
    function fileIcon(path) {
        const ext = (path.match(/\.([a-z0-9]+)$/i) || [])[1];
        const known = { html: 'html', htm: 'html', css: 'css', js: 'js', mjs: 'js', json: 'json', md: 'md', py: 'py',
            rb: 'rb', java: 'java', c: 'c', h: 'h', cpp: 'cpp', cs: 'cs', php: 'php', sql: 'sql', svg: 'svg', txt: 'txt',
            yml: 'yml', yaml: 'yml', xml: 'xml', sh: 'sh', tsx: 'tsx', jsx: 'jsx', ts: 'tsx' };
        if (ext && known[ext.toLowerCase()]) return 'bi-filetype-' + known[ext.toLowerCase()];
        if (IMAGE.test(path)) return 'bi-file-earmark-image';
        if (PDF.test(path)) return 'bi-file-earmark-pdf';
        if (MEDIA.test(path)) return 'bi-file-earmark-play';
        return 'bi-file-earmark';
    }

    function icon(name) {
        const i = el('i', 'bi ' + name);
        i.setAttribute('aria-hidden', 'true');
        return i;
    }

    // A button or link with an icon in front of its label
    function withIcon(tag, className, iconName, label) {
        const e = el(tag, className);
        e.appendChild(icon(iconName));
        e.appendChild(document.createTextNode(label));
        return e;
    }

    function sizeText(bytes) {
        if (bytes < 1000) return bytes + ' B';
        if (bytes < 1000000) return (bytes / 1000).toFixed(1).replace('.', ',').replace(/,0$/, '') + ' kB';
        return (bytes / 1000000).toFixed(1).replace('.', ',').replace(/,0$/, '') + ' MB';
    }

    // Like the login codes: Back closes the viewer, not the page.
    function close(fromHistory) {
        if (!viewer) return;
        viewer.remove();
        viewer = null;
        document.body.classList.remove('subview-open');
        document.removeEventListener('keydown', onKey);
        if (lastFocus) lastFocus.focus();
        if (fromHistory !== true && history.state && history.state.subview) history.back();
    }

    window.addEventListener('popstate', () => close(true));

    function onKey(e) {
        if (e.key === 'Escape') close();
    }

    // The file that shows first, as a page: the index.html nearest the top,
    // else another page nearest the top (raylib's main.html), but not a
    // template (Hugo's layouts/), which makes no sense run on its own;
    // without a page the text file nearest the top.
    function firstFile(files) {
        const depth = (f) => f.path.split('/').length;
        const nearest = (list) => list.slice().sort((a, b) => depth(a) - depth(b) || a.path.localeCompare(b.path))[0];
        const template = (f) => /(^|\/)(layouts|partials|templates|_layouts|_includes)\//.test(f.path);
        return nearest(files.filter((f) => /(^|\/)index\.html?$/i.test(f.path))) ||
            nearest(files.filter((f) => f.html && !template(f))) ||
            nearest(files.filter((f) => f.text)) || files[0];
    }

    // An index.html runs under its folder's address (…/build/ for …/build/
    // index.html): a SvelteKit page takes the address as its route and finds
    // »/« there, not »/index.html«. The viewer's host serves a folder's
    // index.html.
    function runUrl(url) {
        return url.replace(/\/index\.html?$/i, '/');
    }

    // Files as a tree of folders. Big folders (a template's engine, say)
    // start closed, so the student's own files are in view.
    function tree(files, onPick) {
        const root = { dirs: {}, files: [] };
        files.forEach((f) => {
            const parts = f.path.split('/');
            let node = root;
            parts.slice(0, -1).forEach((p) => {
                node.dirs[p] = node.dirs[p] || { dirs: {}, files: [] };
                node = node.dirs[p];
            });
            node.files.push(f);
        });
        const count = (node) => node.files.length + Object.values(node.dirs).reduce((n, d) => n + count(d), 0);
        const render = (node, depth) => {
            const ul = el('ul', 'subview-tree');
            Object.keys(node.dirs).sort().forEach((name) => {
                const li = el('li');
                const details = el('details');
                details.open = depth === 0 || count(node.dirs[name]) <= 12;
                const summary = el('summary');
                summary.appendChild(icon('bi-folder2'));
                summary.appendChild(el('span', null, name));
                details.appendChild(summary);
                details.appendChild(render(node.dirs[name], depth + 1));
                li.appendChild(details);
                ul.appendChild(li);
            });
            node.files.sort((a, b) => a.path.localeCompare(b.path)).forEach((f) => {
                const li = el('li');
                const b = el('button', 'subview-file');
                b.type = 'button';
                b.dataset.path = f.path;
                b.appendChild(icon(fileIcon(f.path)));
                b.appendChild(el('span', 'subview-file-name', f.path.split('/').pop()));
                b.appendChild(el('span', 'subview-file-size', sizeText(f.size)));
                b.addEventListener('click', () => onPick(f));
                li.appendChild(b);
                ul.appendChild(li);
            });
            return ul;
        };
        // A single folder at the top (bif/, feuerwerk/) is opened right away.
        let top = root;
        while (top.files.length === 0 && Object.keys(top.dirs).length === 1) top = Object.values(top.dirs)[0];
        return render(top, 0);
    }

    function codeView(text) {
        const pre = el('pre', 'subview-code');
        const code = el('code');
        const lines = text.replace(/\n$/, '').split('\n');
        lines.forEach((line) => {
            const span = el('span', 'subview-line', line);
            code.appendChild(span);
            code.appendChild(document.createTextNode('\n'));
        });
        pre.appendChild(code);
        return pre;
    }

    function open(link) {
        lastFocus = link;
        const viewUrl = link.getAttribute('href').replace('/api/task/submission/', '/api/task/submission_view/');
        fetch(viewUrl, { credentials: 'same-origin' })
            .then((r) => { if (!r.ok) throw new Error(r.status); return r.json(); })
            .then((data) => show(link, viewUrl, data))
            .catch(() => { window.location.href = link.href; });
    }

    function show(link, viewUrl, data) {
        if (viewer) {
            viewer.remove();
            viewer = null;
        } else {
            history.pushState({ subview: true }, '');
        }
        const textUrl = viewUrl.replace('/api/task/submission_view/', '/api/task/submission_text/');
        viewer = el('div', 'subview');
        const dialog = el('div', 'subview-dialog');
        dialog.setAttribute('role', 'dialog');
        dialog.setAttribute('aria-modal', 'true');
        dialog.setAttribute('aria-label', 'Abgabe: ' + (link.dataset.viewTitle || ''));
        viewer.appendChild(dialog);

        const head = el('div', 'subview-head');
        const heading = el('div', 'subview-heading');
        heading.appendChild(el('div', 'subview-title', link.dataset.viewTitle || 'Abgabe'));
        // when, how many files, how big: the hand-in at a glance
        const facts = [];
        if (data.submitted_at) {
            const when = new Date(data.submitted_at * 1000);
            facts.push('abgegeben am ' + when.toLocaleDateString('de-DE', { day: 'numeric', month: 'numeric', year: 'numeric' }) +
                ' um ' + when.toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' }) + ' Uhr');
        }
        const total = data.files.reduce((n, f) => n + (f.size || 0), 0);
        facts.push(data.files.length === 1 ? '1 Datei' : data.files.length + ' Dateien', sizeText(total));
        heading.appendChild(el('div', 'subview-facts', facts.join(' · ')));
        head.appendChild(heading);
        const download = withIcon('a', 'btn btn-sm btn-outline-secondary subview-action', 'bi-download', 'Herunterladen');
        download.href = data.download;
        download.title = 'Alle Dateien der Abgabe als Archiv';
        head.appendChild(download);
        const x = el('button', 'subview-close');
        x.type = 'button';
        x.setAttribute('aria-label', 'Schließen');
        x.innerHTML = "<i class='bi bi-x-lg' aria-hidden='true'></i>";
        x.addEventListener('click', close);
        head.appendChild(x);
        dialog.appendChild(head);

        const body = el('div', 'subview-body');
        const nav = el('nav', 'subview-nav');
        nav.setAttribute('aria-label', 'Dateien');
        const pane = el('div', 'subview-pane');
        body.appendChild(nav);
        body.appendChild(pane);
        dialog.appendChild(body);

        let current = null;
        const pick = (f, mode) => {
            current = f;
            nav.querySelectorAll('.subview-file').forEach((b) => b.classList.toggle('active', b.dataset.path === f.path));
            pane.innerHTML = '';
            const bar = el('div', 'subview-bar');
            const where = el('div', 'subview-path');
            where.appendChild(icon(fileIcon(f.path)));
            where.appendChild(el('code', null, f.path));
            bar.appendChild(where);
            const url = data.root + f.path.split('/').map(encodeURIComponent).join('/');
            const newTab = () => {
                const tab = withIcon('a', 'btn btn-sm btn-outline-secondary subview-action', 'bi-box-arrow-up-right', 'In neuem Tab');
                tab.href = url;
                tab.target = '_blank';
                tab.rel = 'noopener noreferrer';
                return tab;
            };
            if (f.html) {
                const run = mode !== 'source';
                const seg = el('div', 'btn-group btn-group-sm subview-mode');
                seg.setAttribute('role', 'group');
                seg.setAttribute('aria-label', 'Ansicht');
                const bRun = withIcon('button', 'btn ' + (run ? 'btn-primary' : 'btn-outline-primary'), 'bi-play-fill', 'Starten');
                const bSrc = withIcon('button', 'btn ' + (run ? 'btn-outline-primary' : 'btn-primary'), 'bi-code-slash', 'Quelltext');
                bRun.type = bSrc.type = 'button';
                bRun.setAttribute('aria-pressed', String(run));
                bSrc.setAttribute('aria-pressed', String(!run));
                bRun.addEventListener('click', () => pick(f, 'run'));
                bSrc.addEventListener('click', () => pick(f, 'source'));
                seg.appendChild(bRun);
                seg.appendChild(bSrc);
                bar.appendChild(seg);
                let frame = null;
                if (run) {
                    // the page from the start, as after loading it
                    const reload = el('button', 'btn btn-sm btn-outline-secondary subview-icon-button');
                    reload.type = 'button';
                    reload.title = 'Neu laden';
                    reload.setAttribute('aria-label', 'Seite neu laden');
                    reload.appendChild(icon('bi-arrow-clockwise'));
                    reload.addEventListener('click', () => { if (frame) frame.src = runUrl(url); });
                    bar.appendChild(reload);
                }
                bar.appendChild(newTab());
                pane.appendChild(bar);
                if (run) {
                    frame = el('iframe', 'subview-frame');
                    frame.setAttribute('sandbox', 'allow-scripts allow-forms allow-modals allow-popups allow-downloads');
                    frame.setAttribute('referrerpolicy', 'no-referrer');
                    frame.title = f.path;
                    frame.src = runUrl(url);
                    pane.appendChild(frame);
                    return;
                }
            } else {
                if (IMAGE.test(f.path) || MEDIA.test(f.path) || PDF.test(f.path)) bar.appendChild(newTab());
                pane.appendChild(bar);
            }
            // A PDF in the browser's own viewer: it shows only without a
            // sandbox (SubmissionView.headers), on the viewer's host
            if (PDF.test(f.path)) {
                const frame = el('iframe', 'subview-frame');
                frame.setAttribute('referrerpolicy', 'no-referrer');
                frame.title = f.path;
                frame.src = url;
                pane.appendChild(frame);
                return;
            }
            if (IMAGE.test(f.path)) {
                const img = el('img', 'subview-image');
                img.alt = f.path;
                img.src = url;
                pane.appendChild(img);
            } else if (MEDIA.test(f.path)) {
                const media = el(/\.(mp4|webm)$/i.test(f.path) ? 'video' : 'audio');
                media.controls = true;
                media.src = url;
                pane.appendChild(media);
            } else if (f.text) {
                const wait = el('p', 'subview-note', 'Wird geladen …');
                pane.appendChild(wait);
                fetch(textUrl + (textUrl.includes('?') ? '&' : '?') + 'path=' + encodeURIComponent(f.path), { credentials: 'same-origin' })
                    .then((r) => r.json())
                    .then((d) => { if (current === f) wait.replaceWith(codeView(d.text || '')); })
                    .catch(() => { wait.textContent = 'Die Datei ließ sich nicht laden.'; });
            } else {
                pane.appendChild(el('p', 'subview-note', 'Für diese Datei gibt es keine Vorschau. Sie ist in »Herunterladen« enthalten.'));
            }
        };
        nav.appendChild(tree(data.files, (f) => pick(f)));
        // Up and down move through the files, as in a file list
        nav.addEventListener('keydown', (e) => {
            if (e.key !== 'ArrowDown' && e.key !== 'ArrowUp') return;
            const buttons = [...nav.querySelectorAll('.subview-file')].filter((b) => b.offsetParent);
            const at = buttons.indexOf(document.activeElement);
            const next = buttons[at < 0 ? 0 : Math.max(0, Math.min(buttons.length - 1, at + (e.key === 'ArrowDown' ? 1 : -1)))];
            if (!next) return;
            e.preventDefault();
            next.focus();
            next.click();
        });
        const first = firstFile(data.files);
        if (first) pick(first);
        else pane.appendChild(el('p', 'subview-note', 'Diese Abgabe enthält keine Dateien.'));

        viewer.addEventListener('click', (e) => { if (e.target === viewer) close(); });
        document.body.appendChild(viewer);
        document.body.classList.add('subview-open');
        document.addEventListener('keydown', onKey);
        x.focus();
    }

    document.addEventListener('click', (e) => {
        const link = e.target.closest('a.cb-submission');
        if (!link || e.ctrlKey || e.metaKey || e.shiftKey || e.button !== 0) return;
        e.preventDefault();
        open(link);
    });
})();
