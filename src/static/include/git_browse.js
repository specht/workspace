// Reading a group's repository (/git/…, main.rb, »Reading a group's
// repository in the browser«): the tree of files in the sidebar (folders
// open in place and stay open while browsing, arrows and Enter move through
// it, t finds a file), the menu of branches and tags, the buttons that copy,
// the switch between a Markdown file's preview and its code.
(function () {
    'use strict';
    const browse = document.querySelector('.git-browse');
    if (!browse) return;
    browse.classList.add('git-js');
    const base = browse.dataset.gitBase || '';
    const phone = window.matchMedia('(max-width: 767.98px)');

    function store(kind) {
        try { return window[kind]; } catch (e) { return null; }
    }
    function read(kind, key, fallback) {
        try { const value = store(kind)?.getItem(key); return value === null || value === undefined ? fallback : JSON.parse(value); } catch (e) { return fallback; }
    }
    function write(kind, key, value) {
        try { store(kind)?.setItem(key, JSON.stringify(value)); } catch (e) { /* private window */ }
    }

    // --- the sidebar: shown or hidden ------------------------------------
    const side = document.getElementById('git-side');
    // the button in the toolbar (a wide screen) and the bar above the content (a phone)
    const toggles = Array.from(browse.querySelectorAll('.git-side-toggle'));
    function sideShown() {
        return phone.matches ? browse.classList.contains('git-side-shown') : !browse.classList.contains('git-side-hidden');
    }
    function syncToggle() {
        toggles.forEach(function (t) { t.setAttribute('aria-expanded', sideShown() ? 'true' : 'false'); });
    }
    if (read('localStorage', 'git-side-hidden', false)) browse.classList.add('git-side-hidden');
    syncToggle();
    phone.addEventListener('change', syncToggle);
    function flip() {
        if (phone.matches) {
            browse.classList.toggle('git-side-shown');
        } else {
            browse.classList.toggle('git-side-hidden');
            write('localStorage', 'git-side-hidden', browse.classList.contains('git-side-hidden'));
        }
        syncToggle();
        if (sideShown()) (tree?.querySelector('[tabindex="0"]') || filter)?.focus({ preventScroll: phone.matches });
    }
    toggles.forEach(function (t) { t.addEventListener('click', flip); });

    // --- the tree ----------------------------------------------------------
    const tree = side?.querySelector('.git-files');
    const openKey = 'git-open:' + base;
    const opened = new Set(read('sessionStorage', openKey, []));

    function items() {
        // the treeitems that can be seen: not inside a closed folder
        return Array.from(tree.querySelectorAll('[role="treeitem"]')).filter(function (node) {
            return !node.closest('ul[hidden]');
        });
    }
    function focusItem(node) {
        tree.querySelectorAll('[role="treeitem"][tabindex="0"]').forEach(function (n) { n.tabIndex = -1; });
        node.tabIndex = 0;
        node.focus();
    }
    function remember(path, open) {
        if (open) opened.add(path); else opened.delete(path);
        write('sessionStorage', openKey, Array.from(opened));
    }
    function nodeHtml(entry, depth) {
        const li = document.createElement('li');
        li.setAttribute('role', 'none');
        const a = document.createElement('a');
        a.className = 'git-node' + (entry.type === 'tree' ? ' git-node-folder' : '');
        a.href = entry.href;
        a.setAttribute('role', 'treeitem');
        a.dataset.path = entry.path;
        a.style.setProperty('--depth', depth);
        a.tabIndex = -1;
        const twisty = document.createElement(entry.type === 'tree' ? 'i' : 'span');
        twisty.className = (entry.type === 'tree' ? 'bi bi-chevron-right ' : '') + 'git-twisty';
        twisty.setAttribute('aria-hidden', 'true');
        const icon = document.createElement('i');
        icon.className = 'bi ' + entry.icon + ' git-node-icon';
        icon.setAttribute('aria-hidden', 'true');
        const name = document.createElement('span');
        name.className = 'git-node-name';
        name.textContent = entry.name;
        a.append(twisty, icon, name);
        if (entry.type === 'tree') {
            a.setAttribute('aria-expanded', 'false');
            a.dataset.gitLazy = entry.entries;
        }
        li.appendChild(a);
        return li;
    }
    // A folder's entries, read when it is opened (a repository too big for
    // the tree at once, GitBrowse::TREE_MAX).
    async function load(node) {
        const url = node.dataset.gitLazy;
        delete node.dataset.gitLazy;
        node.setAttribute('aria-busy', 'true');
        try {
            const response = await fetch(url, { headers: { 'Accept': 'application/json' } });
            if (!response.ok) throw new Error(response.status);
            const data = await response.json();
            const group = document.createElement('ul');
            group.setAttribute('role', 'group');
            const depth = parseInt(node.style.getPropertyValue('--depth') || '0', 10) + 1;
            data.entries.forEach(function (entry) { group.appendChild(nodeHtml(entry, depth)); });
            node.parentNode.appendChild(group);
        } catch (e) {
            node.dataset.gitLazy = url;
            throw e;
        } finally {
            node.removeAttribute('aria-busy');
        }
    }
    async function setOpen(node, open) {
        if (node.getAttribute('aria-expanded') === null) return;
        if (open && node.dataset.gitLazy) {
            try { await load(node); } catch (e) { window.location.href = node.href; return; }
        }
        const group = node.parentNode.querySelector(':scope > ul');
        if (group) group.hidden = !open;
        node.setAttribute('aria-expanded', open ? 'true' : 'false');
        remember(node.dataset.path, open);
    }

    if (tree) {
        // the folders opened before, on this repository's earlier pages
        tree.querySelectorAll('.git-node-folder').forEach(function (node) {
            if (node.getAttribute('aria-expanded') === 'true') opened.add(node.dataset.path);
        });
        Array.from(opened).sort().forEach(function (path) {
            const node = tree.querySelector('.git-node-folder[data-path="' + CSS.escape(path) + '"]');
            if (node && node.getAttribute('aria-expanded') !== 'true' && !node.dataset.gitLazy) setOpen(node, true);
        });
        const current = tree.querySelector('[aria-current="page"]') || items()[0];
        tree.querySelectorAll('[role="treeitem"]').forEach(function (n) { n.tabIndex = -1; });
        if (current) {
            current.tabIndex = 0;
            // the current file in sight inside the sidebar, the page stays where it is
            const box = tree.getBoundingClientRect();
            const at = current.getBoundingClientRect();
            if (at.bottom > box.bottom || at.top < box.top) tree.scrollTop += at.top - box.top - box.height / 3;
        }

        tree.addEventListener('click', function (event) {
            const node = event.target.closest('.git-node-folder');
            if (!node || event.ctrlKey || event.metaKey || event.shiftKey || event.button !== 0) return;
            event.preventDefault();
            setOpen(node, node.getAttribute('aria-expanded') !== 'true');
            focusItem(node);
        });
        tree.addEventListener('keydown', function (event) {
            const node = event.target.closest('[role="treeitem"]');
            if (!node) return;
            const list = items();
            const i = list.indexOf(node);
            const folder = node.classList.contains('git-node-folder');
            const open = node.getAttribute('aria-expanded') === 'true';
            let handled = true;
            switch (event.key) {
                case 'ArrowDown': if (list[i + 1]) focusItem(list[i + 1]); break;
                case 'ArrowUp': if (list[i - 1]) focusItem(list[i - 1]); break;
                case 'Home': focusItem(list[0]); break;
                case 'End': focusItem(list[list.length - 1]); break;
                case 'ArrowRight':
                    if (folder && !open) setOpen(node, true);
                    else if (folder && list[i + 1]) focusItem(list[i + 1]);
                    break;
                case 'ArrowLeft': {
                    if (folder && open) { setOpen(node, false); break; }
                    const parent = node.parentNode.parentNode.closest('li')?.querySelector(':scope > [role="treeitem"]');
                    if (parent) focusItem(parent);
                    break;
                }
                case 'Enter':
                case ' ':
                    if (folder) setOpen(node, !open);
                    else if (event.key === ' ') node.click();
                    else handled = false;
                    break;
                default: handled = false;
            }
            if (handled) event.preventDefault();
        });
    }

    // --- t: find a file by its name ---------------------------------------
    const filter = side?.querySelector('[data-git-filter]');
    const found = side?.querySelector('.git-found');
    const none = side?.querySelector('.git-found-none');
    function files() {
        return Array.from(tree.querySelectorAll('[role="treeitem"]:not(.git-node-folder)')).map(function (node) {
            return { path: node.dataset.path, href: node.getAttribute('href'), icon: node.querySelector('.git-node-icon').className };
        });
    }
    function showFound() {
        const words = filter.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
        found.innerHTML = '';
        if (words.length === 0) {
            tree.hidden = false; found.hidden = true; none.hidden = true;
            return;
        }
        const matches = files().filter(function (f) {
            const path = f.path.toLowerCase();
            return words.every(function (w) { return path.includes(w); });
        }).sort(function (a, b) {
            // a name that starts with the first word before one that has it inside
            const name = function (f) { return f.path.split('/').pop().toLowerCase().startsWith(words[0]) ? 0 : 1; };
            return name(a) - name(b) || a.path.length - b.path.length;
        }).slice(0, 50);
        matches.forEach(function (f, i) {
            const li = document.createElement('li');
            li.setAttribute('role', 'none');
            const a = document.createElement('a');
            a.className = 'git-node';
            a.href = f.href;
            a.setAttribute('role', 'option');
            a.setAttribute('aria-selected', i === 0 ? 'true' : 'false');
            a.tabIndex = -1;
            const icon = document.createElement('i');
            icon.className = f.icon;
            icon.setAttribute('aria-hidden', 'true');
            const name = document.createElement('span');
            name.className = 'git-node-name';
            name.textContent = f.path.split('/').pop();
            const where = document.createElement('span');
            where.className = 'git-node-path';
            where.textContent = f.path.includes('/') ? f.path.slice(0, f.path.lastIndexOf('/')) : '';
            a.append(icon, name, where);
            li.appendChild(a);
            found.appendChild(li);
        });
        tree.hidden = true;
        found.hidden = matches.length === 0;
        none.hidden = matches.length > 0;
    }
    function moveFound(step) {
        const options = Array.from(found.querySelectorAll('[role="option"]'));
        if (options.length === 0) return;
        let i = options.findIndex(function (o) { return o.getAttribute('aria-selected') === 'true'; });
        options[i]?.setAttribute('aria-selected', 'false');
        i = Math.max(0, Math.min(options.length - 1, i + step));
        options[i].setAttribute('aria-selected', 'true');
        options[i].scrollIntoView({ block: 'nearest' });
    }
    if (filter && tree) {
        filter.addEventListener('input', showFound);
        filter.addEventListener('keydown', function (event) {
            if (event.key === 'ArrowDown') { moveFound(1); event.preventDefault(); }
            else if (event.key === 'ArrowUp') { moveFound(-1); event.preventDefault(); }
            else if (event.key === 'Enter') {
                const chosen = found.querySelector('[aria-selected="true"]');
                if (chosen) window.location.href = chosen.href;
                event.preventDefault();
            } else if (event.key === 'Escape') {
                filter.value = '';
                showFound();
                tree.querySelector('[tabindex="0"]')?.focus();
                event.preventDefault();
            }
        });
        document.addEventListener('keydown', function (event) {
            if (event.key !== 't' || event.ctrlKey || event.metaKey || event.altKey) return;
            if (event.target.closest('input, textarea, select, [contenteditable]')) return;
            event.preventDefault();
            if (!sideShown()) flip();
            filter.focus();
            filter.select();
        });
    }

    // --- branches and tags ---------------------------------------------------
    document.querySelectorAll('[data-git-refs]').forEach(function (menu) {
        const tabs = Array.from(menu.querySelectorAll('[data-git-ref-tab]'));
        const lists = Array.from(menu.querySelectorAll('[data-git-ref-list]'));
        const search = menu.querySelector('[data-git-ref-filter]');
        const nothing = menu.querySelector('[data-git-ref-nothing]');
        function choose(key) {
            tabs.forEach(function (t) { t.setAttribute('aria-selected', t.dataset.gitRefTab === key ? 'true' : 'false'); });
            lists.forEach(function (l) { l.hidden = l.dataset.gitRefList !== key; });
            narrow();
        }
        function narrow() {
            const text = search ? search.value.trim().toLowerCase() : '';
            const list = lists.find(function (l) { return !l.hidden; });
            let shown = 0;
            list.querySelectorAll('.git-ref-item').forEach(function (item) {
                item.hidden = text !== '' && !item.dataset.gitRefName.toLowerCase().includes(text);
                if (!item.hidden) shown += 1;
            });
            list.querySelectorAll('.git-ref-none').forEach(function (p) { p.hidden = text !== ''; });
            nothing.hidden = !(text !== '' && shown === 0);
        }
        tabs.forEach(function (t) { t.addEventListener('click', function () { choose(t.dataset.gitRefTab); }); });
        search?.addEventListener('input', narrow);
        menu.parentNode.addEventListener('shown.bs.dropdown', function () {
            (search || menu.querySelector('.git-ref-item.active') || tabs[0]).focus();
        });
    });

    // --- copying: the address, a path, a hash, a file -----------------------
    function copied(button) {
        const icon = button.querySelector('.bi');
        button.classList.add('copied');
        icon?.classList.replace('bi-copy', 'bi-check-lg');
        const label = button.getAttribute('aria-label');
        button.setAttribute('aria-label', 'Kopiert');
        setTimeout(function () {
            button.classList.remove('copied');
            icon?.classList.replace('bi-check-lg', 'bi-copy');
            if (label) button.setAttribute('aria-label', label);
        }, 1500);
    }
    function copyText(text, button, field) {
        const done = function () { copied(button); };
        if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(done, function () { fallback(text, field); done(); });
        } else {
            fallback(text, field);
            done();
        }
    }
    function fallback(text, field) {
        let input = field;
        if (!input) {
            input = document.createElement('textarea');
            input.value = text;
            input.setAttribute('readonly', '');
            input.style.position = 'fixed';
            input.style.opacity = '0';
            document.body.appendChild(input);
        }
        input.select();
        try { document.execCommand('copy'); } catch (e) { /* the text stays selected */ }
        if (input !== field) input.remove();
    }
    browse.addEventListener('click', function (event) {
        const button = event.target.closest('[data-git-copy], [data-git-copy-text], [data-git-copy-code]');
        if (!button) return;
        if (button.dataset.gitCopy !== undefined) {
            const field = document.querySelector(button.dataset.gitCopy);
            field.select();
            copyText(field.value, button, field);
        } else if (button.dataset.gitCopyText !== undefined) {
            copyText(button.dataset.gitCopyText, button, null);
        } else {
            const code = button.closest('.git-box').querySelector('pre.git-code');
            const text = Array.from(code.querySelectorAll('.git-lc')).map(function (l) { return l.textContent; }).join('\n') + '\n';
            copyText(text, button, null);
        }
    });
    browse.querySelectorAll('[data-git-select]').forEach(function (field) {
        field.addEventListener('focus', function () { field.select(); });
        field.addEventListener('click', function () { field.select(); });
    });
    // the address selected as soon as the menu opens: Ctrl+C copies it
    browse.querySelectorAll('.git-clone-menu').forEach(function (menu) {
        menu.parentNode.addEventListener('shown.bs.dropdown', function () { menu.querySelector('[data-git-select]')?.focus(); });
    });

    // --- a Markdown file: preview or code ----------------------------------
    browse.querySelectorAll('[data-git-show]').forEach(function (button) {
        button.addEventListener('click', function () {
            const box = button.closest('.git-box');
            box.querySelectorAll('[data-git-show]').forEach(function (b) {
                const on = b === button;
                b.classList.toggle('active', on);
                b.setAttribute('aria-pressed', on ? 'true' : 'false');
            });
            box.querySelectorAll('[data-git-view]').forEach(function (view) { view.hidden = view.dataset.gitView !== button.dataset.gitShow; });
        });
    });
    // a picture: its size in pixels beside its size in bytes
    browse.querySelectorAll('.git-picture img').forEach(function (img) {
        const info = img.closest('.git-box').querySelector('.git-file-info');
        const show = function () {
            if (info && img.naturalWidth) info.textContent += ', ' + img.naturalWidth + ' × ' + img.naturalHeight + ' Pixel';
        };
        if (img.complete) show(); else img.addEventListener('load', show);
    });
    // a line in the address (#L12) of a Markdown file: its code shown
    if (/^#L\d+$/.test(window.location.hash)) {
        browse.querySelector('[data-git-show="code"]')?.click();
        document.getElementById(window.location.hash.slice(1))?.scrollIntoView({ block: 'center' });
    }
})();
