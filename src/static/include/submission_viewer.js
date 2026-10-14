// Hand-ins in the browser (class page): a click on a hand-in opens a viewer
// over the page with its files, the source code and, for web pages, the
// page itself. What students wrote runs only on abgaben.<host>, sandboxed
// (SubmissionView in src/ruby/include/submission_view.rb); this script only
// shows text and points frames at that host.
(function () {
    const IMAGE = /\.(png|jpe?g|gif|webp|svg|ico)$/i;
    const MEDIA = /\.(mp3|ogg|wav|mp4|webm)$/i;
    let viewer = null;
    let lastFocus = null;

    function el(tag, className, text) {
        const e = document.createElement(tag);
        if (className) e.className = className;
        if (text !== undefined) e.textContent = text;
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

    // The file that shows first: the page nearest the top, else the first
    // text file.
    function firstFile(files) {
        const pages = files.filter((f) => f.html).sort((a, b) =>
            a.path.split('/').length - b.path.split('/').length ||
            (b.path.endsWith('index.html') ? 1 : 0) - (a.path.endsWith('index.html') ? 1 : 0));
        return pages[0] || files.find((f) => f.text) || files[0];
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
                details.appendChild(el('summary', null, name + '/'));
                details.appendChild(render(node.dirs[name], depth + 1));
                li.appendChild(details);
                ul.appendChild(li);
            });
            node.files.sort((a, b) => a.path.localeCompare(b.path)).forEach((f) => {
                const li = el('li');
                const b = el('button', 'subview-file');
                b.type = 'button';
                b.dataset.path = f.path;
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
        head.appendChild(el('div', 'subview-title', link.dataset.viewTitle || 'Abgabe'));
        const download = el('a', 'btn btn-sm btn-outline-secondary', 'Herunterladen');
        download.href = data.download;
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
            bar.appendChild(el('code', 'subview-path', f.path));
            const url = data.root + f.path.split('/').map(encodeURIComponent).join('/');
            if (f.html) {
                const run = mode !== 'source';
                const seg = el('div', 'btn-group btn-group-sm');
                const bRun = el('button', 'btn ' + (run ? 'btn-primary' : 'btn-outline-primary'), 'Starten');
                const bSrc = el('button', 'btn ' + (run ? 'btn-outline-primary' : 'btn-primary'), 'Quelltext');
                bRun.type = bSrc.type = 'button';
                bRun.addEventListener('click', () => pick(f, 'run'));
                bSrc.addEventListener('click', () => pick(f, 'source'));
                seg.appendChild(bRun);
                seg.appendChild(bSrc);
                bar.appendChild(seg);
                const tab = el('a', 'subview-newtab', 'In neuem Tab');
                tab.href = url;
                tab.target = '_blank';
                tab.rel = 'noopener noreferrer';
                bar.appendChild(tab);
                pane.appendChild(bar);
                if (run) {
                    const frame = el('iframe', 'subview-frame');
                    frame.setAttribute('sandbox', 'allow-scripts allow-forms allow-modals allow-popups allow-downloads');
                    frame.setAttribute('referrerpolicy', 'no-referrer');
                    frame.title = f.path;
                    frame.src = url;
                    pane.appendChild(frame);
                    return;
                }
            } else {
                pane.appendChild(bar);
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
        const first = firstFile(data.files);
        if (first) pick(first);

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
