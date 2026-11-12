// The card of a document task (DocumentCheck, docs/plan-textverarbeitung.md):
// the student's document from Word, LibreOffice or Nextcloud Office goes
// to the server as a form upload; the answer is a checklist and the pages
// as LibreOffice drew them, with a numbered mark at each place a point of
// the checklist fails. Point and mark light up together (as in the mail
// viewer).
//
// Where the browser can keep hold of a file (Chrome, Edge: the File System
// Access API), »Vorlage speichern« saves the starting file through the page
// itself, and »Prüfen« reads that file again each time, after the student
// saved it in Word; a document chosen with »Dokument wählen« or dropped on
// the card is kept the same way. Other browsers download the starting file
// and need the document chosen or dropped for each check.
//
// tasks.js calls setup(card, check), check(card, busy, done),
// render(card, report) and showStored(card, answer).
(function () {
    const MAX_BYTES = 10 * 1024 * 1024;
    const CAN_KEEP = typeof window.showOpenFilePicker === 'function' && typeof window.showSaveFilePicker === 'function';
    const TYPES = [{
        description: 'Textdokumente',
        accept: {
            'application/vnd.openxmlformats-officedocument.wordprocessingml.document': ['.docx'],
            'application/vnd.oasis.opendocument.text': ['.odt'],
            'application/msword': ['.doc'],
            'application/rtf': ['.rtf'],
        },
    }];

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    function hasFiles(e) {
        return e.dataTransfer && Array.from(e.dataTransfer.types || []).indexOf('Files') >= 0;
    }

    // what the card says about the chosen document
    function chosen(card, name) {
        const line = card.querySelector('.document-name');
        line.textContent = '';
        line.appendChild(el('strong', null, name));
        if (card.documentHandle) line.appendChild(document.createTextNode(': Speichere in Word, dann »Prüfen«.'));
        card.querySelector('.document-pick').lastChild.textContent = 'Anderes Dokument';
    }

    // Chrome and Edge: a starting file saved through the page (»Vorlage
    // speichern«, a link with class document-starter in the text or on the
    // card), which the page then keeps hold of: every card with the same
    // starting file reads it. Elsewhere the link is a plain download.
    const SAVED = {};

    function useSaved(card, handle) {
        card.documentHandle = handle;
        card.documentFile = null;
        const line = card.querySelector('.document-name');
        line.textContent = '';
        line.appendChild(el('strong', null, handle.name));
        line.appendChild(document.createTextNode(' ist gespeichert. Öffne sie in Word oder LibreOffice; »Prüfen« liest sie, sobald du dort gespeichert hast.'));
        card.querySelector('.document-pick').lastChild.textContent = 'Anderes Dokument';
    }

    function bindStarter(link) {
        if (!CAN_KEEP || link.dataset.bound) return;
        link.dataset.bound = 'true';
        link.addEventListener('click', async function (e) {
            e.preventDefault();
            const href = link.getAttribute('href');
            const note = link.parentNode.querySelector('.document-saved') || link.insertAdjacentElement('afterend', el('span', 'document-saved'));
            let handle;
            try {
                handle = await window.showSaveFilePicker({ suggestedName: link.dataset.name || decodeURIComponent(href.split('/').pop()), types: TYPES });
            } catch (err) {
                return;    // closed without saving
            }
            try {
                const response = await fetch(href, { credentials: 'same-origin' });
                if (!response.ok) throw new Error(String(response.status));
                const writable = await handle.createWritable();
                await writable.write(await response.blob());
                await writable.close();
            } catch (err) {
                note.textContent = ' Die Vorlage konnte ich nicht speichern. Versuch es noch einmal.';
                return;
            }
            SAVED[href] = handle;
            note.textContent = ' Gespeichert als ' + handle.name + '.';
            // the last version on a card: that card goes on with it
            const own = link.closest('.task-card');
            if (own) useSaved(own, handle);
            document.querySelectorAll('.task-card .document-starter[href="' + href + '"]').forEach(function (other) {
                const card = other.closest('.task-card');
                if (card) useSaved(card, handle);
            });
        });
    }

    const bindAll = function () { document.querySelectorAll('a.document-starter').forEach(bindStarter); };
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', bindAll);
    else bindAll();

    function invalid(text) {
        return { success: true, status: 'invalid', message: '<p>' + text + '</p>' };
    }

    function setup(card, check) {
        const box = card.querySelector('.document-task');
        if (!box || box.dataset.ready) return;
        box.dataset.ready = 'true';
        card.documentRecheck = check;
        const button = box.querySelector('.document-pick');
        const input = box.querySelector('.document-input');
        const starter = box.querySelector('.document-starter');

        if (starter) bindStarter(starter);
        // how short the loop is in this browser, and why another one helps
        const note = el('p', 'document-browser');
        // Chrome and Edge keep hold of a file only on https (or localhost)
        const chromium = /Chrome\/|Edg\//.test(navigator.userAgent);
        note.innerHTML = CAN_KEEP
            ? "<i class='bi bi-lightning-charge' aria-hidden='true'></i>Hier genügt es, in Word oder LibreOffice zu speichern: Die Seite merkt es und prüft von selbst."
            : chromium && !window.isSecureContext
            ? "<i class='bi bi-info-circle' aria-hidden='true'></i>Diese Seite läuft ohne https, und dann darf auch Chrome eine Datei nicht im Blick behalten. " +
              'Wähle sie vor jedem Prüfen neu oder zieh sie auf die Karte.'
            : "<i class='bi bi-info-circle' aria-hidden='true'></i>In Chrome und Edge genügt es, in Word oder LibreOffice zu speichern, und die Seite prüft von selbst. " +
              'Dieser Browser kann eine Datei nicht im Blick behalten: Wähle sie vor jedem Prüfen neu oder zieh sie auf die Karte.';
        box.appendChild(note);
        // a starting file saved before (a link in the text above the card)
        const saved = starter && SAVED[starter.getAttribute('href')];
        if (saved) useSaved(card, saved);

        button.addEventListener('click', async function () {
            if (!CAN_KEEP) {
                input.click();
                return;
            }
            let handle;
            try {
                [handle] = await window.showOpenFilePicker({ types: TYPES, multiple: false });
            } catch (err) {
                if (err && err.name === 'AbortError') return;
                input.click();
                return;
            }
            card.documentHandle = handle;
            card.documentFile = null;
            chosen(card, handle.name);
            check();
        });
        input.addEventListener('change', function () {
            const file = input.files && input.files[0];
            input.value = '';
            if (!file) return;
            card.documentFile = file;
            card.documentFresh = true;
            card.documentHandle = null;
            chosen(card, file.name);
            check();
        });

        // a document dropped anywhere on the card
        card.addEventListener('dragover', function (e) {
            if (!hasFiles(e)) return;
            e.preventDefault();
            card.classList.add('document-dragging');
        });
        card.addEventListener('dragleave', function (e) {
            if (!card.contains(e.relatedTarget)) card.classList.remove('document-dragging');
        });
        card.addEventListener('drop', async function (e) {
            if (!hasFiles(e)) return;
            e.preventDefault();
            card.classList.remove('document-dragging');
            // both have to be asked for before anything is awaited
            const file = e.dataTransfer.files[0];
            const item = e.dataTransfer.items && e.dataTransfer.items[0];
            const pending = item && typeof item.getAsFileSystemHandle === 'function' ? item.getAsFileSystemHandle() : null;
            let handle = null;
            try { handle = pending ? await pending : null; } catch (err) { handle = null; }
            if (handle && handle.kind === 'file') {
                card.documentHandle = handle;
                card.documentFile = null;
            } else {
                card.documentFile = file;
                card.documentFresh = true;
                card.documentHandle = null;
            }
            if (!file && !handle) return;
            chosen(card, (handle || file).name);
            check();
        });
    }

    // Word on one half of the screen, the page on the other: while the page
    // is visible, a document the browser keeps hold of is looked at every
    // two seconds, and checked when it was saved since the last check (its
    // time or size changed). Ctrl+S in Word is enough.
    const WATCH_MS = 2000;
    function watch(card, run) {
        if (card.documentWatch) return;
        card.documentWatch = setInterval(async function () {
            const handle = card.documentHandle;
            if (!handle || document.hidden || card.documentBusy || !card.documentSeen) return;
            let file;
            try {
                if (handle.queryPermission && await handle.queryPermission({ mode: 'read' }) !== 'granted') return;
                file = await handle.getFile();
            } catch (err) {
                return;
            }
            const seen = card.documentSeen;
            if (file.lastModified === seen.time && file.size === seen.size) return;
            // a save in progress: wait until the size holds still
            if (seen.pending !== file.lastModified + ':' + file.size) {
                seen.pending = file.lastModified + ':' + file.size;
                return;
            }
            run();
        }, WATCH_MS);
    }

    // Reads the document (again, if the browser keeps hold of it) and
    // sends it. busy() when it goes; done(data) with the server's answer.
    async function check(card, busy, done) {
        let file = card.documentFile;
        // A file chosen without a handle is what it was when it was chosen:
        // once Word saved it again, the browser cannot read it any more. So
        // it is chosen again before each check, here while still inside the
        // click (nothing awaited yet), which the file dialog needs.
        if (!card.documentHandle && file && !card.documentFresh) {
            card.querySelector('.document-input').click();
            return;
        }
        card.documentFresh = false;
        if (card.documentHandle) {
            try {
                if (card.documentHandle.queryPermission && await card.documentHandle.queryPermission({ mode: 'read' }) !== 'granted') {
                    await card.documentHandle.requestPermission({ mode: 'read' });
                }
                file = await card.documentHandle.getFile();
            } catch (err) {
                card.documentHandle = null;
                done(invalid('Die Datei kann ich nicht mehr lesen. Wurde sie verschoben, umbenannt oder gelöscht? Wähle sie noch einmal.'));
                return;
            }
        }
        if (!file) {
            card.querySelector('.document-pick').click();
            return;
        }
        if (file.size > MAX_BYTES) {
            done(invalid('Die Datei ist größer als 10 MB. Sind große Bilder darin?'));
            return;
        }
        // changed since it was chosen (saved again in between): unreadable
        if (!card.documentHandle) {
            try {
                await file.slice(0, 1).arrayBuffer();
            } catch (err) {
                card.documentFile = null;
                done(invalid('Die Datei hat sich geändert, seit du sie gewählt hast. Wähle sie noch einmal.'));
                return;
            }
        }
        busy();
        card.documentBusy = true;
        card.documentSeen = { time: file.lastModified, size: file.size };
        if (card.documentHandle && card.documentRecheck) watch(card, card.documentRecheck);
        const form = new FormData();
        form.append('task', card.dataset.task);
        form.append('file', file, file.name);
        try {
            const response = await fetch('/api/task/document', { method: 'POST', body: form, credentials: 'same-origin' });
            if (!response.ok) throw new Error(String(response.status));
            const data = await response.json();
            data.success = true;
            done(data);
        } catch (err) {
            done({ success: false });
        } finally {
            card.documentBusy = false;
        }
    }

    // Where the website cannot scroll the panel to the top of the window
    // (the card at the end of the page), the panel takes the height that is
    // left below it, so that the verdict, the list and the page still fit;
    // with the window's next change of size it goes back to the height in
    // default.css.
    function fit(wrap) {
        const top = Math.max(wrap.getBoundingClientRect().top, 0);
        const room = window.innerHeight - top - 12;
        const css = wrap.getBoundingClientRect().height;
        if (room < css) wrap.style.height = Math.max(280, room) + 'px';
        window.addEventListener('resize', function () { wrap.style.height = ''; }, { once: true });
    }

    // ---- The result: the checklist beside the pages, on one screen

    // A place on the page, in points (the page is 1 px per point before the
    // zoom), drawn as what the formatting marks of Word and LibreOffice
    // would show there, in orange: dots for spaces, ¶ for an empty
    // paragraph, ↵ for a line break; a missing gap as a thin line; a
    // paragraph (its alignment) only framed while its point is active.
    function markNode(mark) {
        const kind = mark.kind || 'box';
        const node = el('div', 'document-mark document-mark-' + kind);
        node.style.left = mark.x + 'px';
        node.style.top = mark.y + 'px';
        node.style.width = Math.max(mark.w, 3) + 'px';
        node.style.height = Math.max(mark.h, 3) + 'px';
        node.dataset.n = mark.n;
        if (mark.title) node.title = mark.title;
        if (kind === 'gap') {
            // one dot per space, as far apart as the spaces are wide
            const step = mark.count ? mark.w / mark.count : 3;
            node.style.backgroundSize = step + 'px 100%';
        } else if (kind === 'blank' || kind === 'spot') {
            const glyph = el('span', 'document-glyph', kind === 'blank' ? '¶' : '↵');
            glyph.setAttribute('aria-hidden', 'true');
            glyph.style.fontSize = Math.min(14, Math.max(8, mark.h * 0.8)) + 'px';
            node.appendChild(glyph);
        }
        return node;
    }

    // The numbers in the left margin of the page, beside their line, like
    // comments: one per point and line; several points on one line side by
    // side, the first nearest the text. Their size stays the same at every
    // zoom (--inv).
    function gutter(sheet, marks, left) {
        const lines = [];
        marks.forEach(function (m) {
            const mid = m.y + Math.min(m.h, 16) / 2;
            let line = lines.find(function (l) { return Math.abs(l.mid - mid) < 7; });
            if (!line) { line = { mid: mid, points: [] }; lines.push(line); }
            if (line.points.indexOf(m.n) < 0) line.points.push(m.n);
        });
        // how many numbers the fullest line has (room in the margin, viewer)
        let most = 0;
        lines.forEach(function (line) { most = Math.max(most, line.points.length); });
        lines.forEach(function (line) {
            line.points.sort(function (a, b) { return a - b; }).forEach(function (n, k) {
                const badge = el('button', 'document-badge', String(n));
                badge.type = 'button';
                badge.dataset.n = n;
                badge.tabIndex = -1;
                badge.setAttribute('aria-hidden', 'true');
                badge.style.top = line.mid + 'px';
                badge.style.left = (left - 8) + 'px';
                badge.style.setProperty('--k', String(k));
                sheet.appendChild(badge);
            });
        });
        return most;
    }

    // The pages one below the other on a stage that moves and zooms:
    // dragging moves it (one finger or the mouse), two fingers, Ctrl and
    // the wheel or a double click zoom, the wheel alone scrolls the website
    // (in full screen it moves the page), the buttons and the keys (+, -,
    // 0, arrows) do the same. It starts on the part of the first page that
    // has text (crop): the sheet as wide as the view, on a phone its text.
    // keep: { z, tx, ty } of the view before the new check, kept there,
    // or 'whole' for the whole first page
    // (the card redraws the result with each check); remember(view) is
    // called after every move.
    function viewer(view, pages, marks, keep, remember) {
        const viewport = el('div', 'document-viewport');
        const stage = el('div', 'document-stage');
        const GAP = 24;
        let most = 1;
        const width = Math.max.apply(null, pages.map(function (p) { return p.width; }));
        let y = 0;
        const tops = [];
        const sheets = [];
        pages.forEach(function (page, i) {
            const sheet = el('div', 'document-sheet');
            sheet.style.width = page.width + 'px';
            sheet.style.height = page.height + 'px';
            sheet.style.left = ((width - page.width) / 2) + 'px';
            sheet.style.top = y + 'px';
            tops.push(y);
            const img = el('img');
            img.src = page.src;
            img.alt = 'Seite ' + (i + 1) + ' deines Dokuments, so wie LibreOffice sie zeichnet';
            img.draggable = false;
            sheet.appendChild(img);
            const own = marks.filter(function (m) { return m.page === i + 1; });
            own.forEach(function (m) { sheet.appendChild(markNode(m)); });
            most = Math.max(most, gutter(sheet, own, page.crop ? page.crop.left : 70));
            stage.appendChild(sheet);
            sheets.push(sheet);
            y += page.height + GAP;
        });
        const height = y - GAP;
        stage.style.width = width + 'px';
        stage.style.height = height + 'px';
        viewport.appendChild(stage);
        view.appendChild(viewport);

        let z = 1, tx = 0, ty = 0;
        // 'whole' or 'width' while a fit is in effect; fitting: inside one
        let fit = null, fitting = false;
        const MIN = 0.15, MAX = 6;
        const size = function () { return { w: viewport.clientWidth, h: viewport.clientHeight }; };
        const offset = function (i) { return (width - pages[i].width) / 2; };
        // keep the stage in view: where it is smaller than the view, wholly
        // inside it, where it stands (re-centring it would push against a
        // zoom at the pointer); where larger, with no more than a strip of
        // desk beside it
        function clamp() {
            const v = size();
            const sw = width * z, sh = height * z, keep = 48;
            tx = sw <= v.w ? Math.max(Math.min(tx, v.w - sw), 0) : Math.min(keep, Math.max(v.w - sw - keep, tx));
            ty = sh <= v.h ? Math.max(Math.min(ty, v.h - sh), 0) : Math.min(keep, Math.max(v.h - sh - keep, ty));
        }
        function apply(animate) {
            clamp();
            stage.classList.toggle('document-gliding', !!animate && !reduced());
            stage.style.transform = 'translate(' + tx + 'px, ' + ty + 'px) scale(' + z + ')';
            // the numbers keep 18 px on the screen, but never get taller than
            // a line of the page (13 pt), down to 12 px
            viewport.style.setProperty('--inv', String(Math.max(12, Math.min(18, 13 * z)) / 18 / z));
            zoomLabel.textContent = Math.round(z * 100) + ' %';
            if (!fitting) fit = null;
            showFit();
            if (remember) remember({ z: z, tx: tx, ty: ty });
        }
        function zoomAt(x, y, factor, animate) {
            const next = Math.min(MAX, Math.max(MIN, z * factor));
            tx = x - (x - tx) * (next / z);
            ty = y - (y - ty) * (next / z);
            z = next;
            apply(animate);
        }
        // a rectangle of the stage (points) as large as fits, with room around
        // a narrow view (a phone) wastes no room around the page
        const edge = function () { return size().w < 560 ? 6 : 16; };
        function show(r, most, animate, pad) {
            const v = size();
            pad = pad === undefined ? 24 : pad;
            z = Math.max(MIN, Math.min(most || MAX, (v.w - 2 * pad) / r.w, (v.h - 2 * pad) / r.h));
            tx = (v.w - r.w * z) / 2 - r.x * z;
            ty = (v.h - r.h * z) / 2 - r.y * z;
            apply(animate);
        }
        // The first view on a narrow view (a phone): the text of page 1 as
        // wide as the view, with room on the left for the numbers in the
        // margin (GUTTER, on the screen), its first line near the top.
        // room for as many numbers as the fullest line has, two at least
        const GUTTER = 14 + 22 * Math.max(2, most);
        function text(animate) {
            const v = size();
            const page = pages[0];
            const c = page.crop || { x: 0, y: 0, w: page.width, h: page.height };
            const left = c.left !== undefined ? c.left : c.x;
            const right = c.left !== undefined ? c.x + c.w - 30 : c.x + c.w;
            z = Math.max(MIN, Math.min(MAX, (v.w - GUTTER - edge()) / (right - left)));
            tx = GUTTER - (offset(0) + left) * z;
            ty = edge() - c.y * z;
            apply(animate);
        }
        // »Seitenbreite«: the whole sheet from edge to edge, as large as the
        // numbers in its left margin allow, from its top edge
        function sheetWidth(animate) {
            fitting = true;
            fit = 'width';
            const v = size();
            const page = pages[0];
            const c = page.crop || { x: 0, y: 0, w: page.width, h: page.height };
            const left = c.left !== undefined ? c.left : c.x;
            z = Math.max(MIN, Math.min(MAX, (v.w - 2 * edge()) / page.width, (v.w - GUTTER - edge()) / (page.width - left)));
            tx = v.w - edge() - (offset(0) + page.width) * z;
            ty = edge();
            apply(animate);
            fitting = false;
        }
        // the first view: the sheet's width where there is room, the text's
        // on a phone
        function start(animate) {
            if (size().w < 560) text(animate);
            else sheetWidth(animate);
        }
        // »Ganze Seite«: the whole first sheet, as large as fits
        function whole(animate) {
            fitting = true;
            fit = 'whole';
            show({ x: offset(0), y: 0, w: pages[0].width, h: pages[0].height }, null, animate, edge());
            fitting = false;
        }

        // the tools above the page: the zoom as one group, the two fits as
        // one (with their names, the one in effect pressed), full screen
        // and »prüfen« at the right; a narrow view keeps only the icons
        const tools = el('div', 'document-tools');
        const button = function (icon, label, act, parent, text) {
            const b = el('button', 'btn btn-sm btn-outline-secondary');
            b.type = 'button';
            b.innerHTML = "<i class='bi " + icon + "' aria-hidden='true'></i>" +
                (text ? "<span class='document-tool-label'>" + text + '</span>' : '');
            b.setAttribute('aria-label', label);
            b.title = label;
            b.addEventListener('click', act);
            (parent || tools).appendChild(b);
            return b;
        };
        const middle = function () { const v = size(); return [v.w / 2, v.h / 2]; };
        const zooms = el('div', 'btn-group document-tool-group');
        zooms.setAttribute('role', 'group');
        zooms.setAttribute('aria-label', 'Zoom');
        tools.appendChild(zooms);
        button('bi-dash-lg', 'Verkleinern', function () { const m = middle(); zoomAt(m[0], m[1], 1 / 1.25, true); }, zooms);
        const zoomLabel = el('span', 'btn btn-sm btn-outline-secondary document-zoom', '100 %');
        zooms.appendChild(zoomLabel);
        button('bi-plus-lg', 'Vergrößern', function () { const m = middle(); zoomAt(m[0], m[1], 1.25, true); }, zooms);
        const fits = el('div', 'btn-group document-tool-group');
        fits.setAttribute('role', 'group');
        fits.setAttribute('aria-label', 'Einpassen');
        tools.appendChild(fits);
        const fitWhole = button('bi-file-earmark', 'Ganze Seite', function () { whole(true); }, fits, 'Ganze Seite');
        const fitWidth = button('bi-arrows', 'Seitenbreite', function () { sheetWidth(true); }, fits, 'Seitenbreite');
        // apart from the fits, at the right beside »prüfen«
        const full = button('bi-arrows-fullscreen', 'Vollbild', function () { toggleFull(); });
        full.classList.add('document-full-toggle');
        view.insertBefore(tools, viewport);
        // which fit is in effect, until the page is zoomed or moved
        function showFit() {
            fitWhole.setAttribute('aria-pressed', String(fit === 'whole'));
            fitWidth.setAttribute('aria-pressed', String(fit === 'width'));
        }

        // dragging and pinching
        const pointers = new Map();
        let pinch = null;
        viewport.addEventListener('pointerdown', function (e) {
            if (e.button !== 0 && e.pointerType === 'mouse') return;
            if (e.target.closest('.document-badge')) return;
            // no selection and no dragging of the picture by the browser:
            // either would cancel the pointer in the middle of a move
            if (e.pointerType === 'mouse') e.preventDefault();
            viewport.focus({ preventScroll: true });
            viewport.setPointerCapture(e.pointerId);
            pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
            viewport.classList.add('document-grabbing');
            if (pointers.size === 2) {
                const p = Array.from(pointers.values());
                pinch = { d: Math.hypot(p[0].x - p[1].x, p[0].y - p[1].y), z: z };
            }
        });
        viewport.addEventListener('pointermove', function (e) {
            const last = pointers.get(e.pointerId);
            if (!last) return;
            const box = viewport.getBoundingClientRect();
            if (pointers.size === 2 && pinch) {
                pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
                const p = Array.from(pointers.values());
                const d = Math.hypot(p[0].x - p[1].x, p[0].y - p[1].y);
                const mx = (p[0].x + p[1].x) / 2 - box.left, my = (p[0].y + p[1].y) / 2 - box.top;
                zoomAt(mx, my, (pinch.z * d / pinch.d) / z, false);
                return;
            }
            tx += e.clientX - last.x;
            ty += e.clientY - last.y;
            pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
            apply(false);
        });
        const up = function (e) {
            pointers.delete(e.pointerId);
            if (pointers.size < 2) pinch = null;
            if (!pointers.size) viewport.classList.remove('document-grabbing');
        };
        viewport.addEventListener('pointerup', up);
        viewport.addEventListener('pointercancel', up);
        viewport.addEventListener('lostpointercapture', up);
        viewport.addEventListener('dblclick', function (e) {
            if (e.target.closest('.document-badge')) return;
            const box = viewport.getBoundingClientRect();
            zoomAt(e.clientX - box.left, e.clientY - box.top, 2, true);
        });
        // The wheel scrolls the website; Ctrl (or ⌘) and the wheel zoom at
        // the pointer, and so does pinching on a touchpad (the browser sends
        // Ctrl with it). In full screen, where there is nothing to scroll,
        // the wheel and two fingers move the page.
        const hint = el('div', 'document-wheel-hint', /Mac|iPhone|iPad/.test(navigator.platform) ? '⌘ und Mausrad zoomen' : 'Strg und Mausrad zoomen');
        hint.setAttribute('aria-hidden', 'true');
        viewport.appendChild(hint);
        let hintTimer = null;
        // zooming by the wheel: toward a target, a third of the way each frame
        let goal = null;
        let gliding = false;
        function glide(x, y, factor) {
            const base = goal ? goal.z : z;
            goal = { x: x, y: y, z: Math.min(MAX, Math.max(MIN, base * factor)) };
            if (reduced()) {
                zoomAt(x, y, goal.z / z, false);
                goal = null;
                return;
            }
            if (gliding) return;
            gliding = true;
            const step = function () {
                if (!goal) { gliding = false; return; }
                const left = goal.z / z;
                if (Math.abs(left - 1) < 0.002) {
                    zoomAt(goal.x, goal.y, left, false);
                    goal = null;
                    gliding = false;
                    return;
                }
                zoomAt(goal.x, goal.y, Math.pow(left, 0.35), false);
                requestAnimationFrame(step);
            };
            requestAnimationFrame(step);
        }
        viewport.addEventListener('wheel', function (e) {
            const box = viewport.getBoundingClientRect();
            const dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
            const dx = e.deltaMode === 1 ? e.deltaX * 16 : e.deltaX;
            if (e.ctrlKey || e.metaKey) {
                e.preventDefault();
                // a notch of a mouse wheel (whole steps, 100 or more, or
                // lines): 12 %, eased over a few frames; pinching on a
                // touchpad (small, fractional steps) follows the fingers
                const notch = e.deltaMode === 1 || (Math.abs(dy) >= 50 && dy % 1 === 0);
                if (notch) glide(e.clientX - box.left, e.clientY - box.top, dy < 0 ? 1.12 : 1 / 1.12);
                else zoomAt(e.clientX - box.left, e.clientY - box.top, Math.exp(-dy * 0.008), false);
                return;
            }
            if (view.closest('.document-fullscreen')) {
                e.preventDefault();
                tx -= dx;
                ty -= dy;
                apply(false);
                return;
            }
            hint.classList.add('document-wheel-hint-on');
            clearTimeout(hintTimer);
            hintTimer = setTimeout(function () { hint.classList.remove('document-wheel-hint-on'); }, 1100);
        }, { passive: false });
        viewport.tabIndex = 0;
        viewport.setAttribute('role', 'group');
        viewport.setAttribute('aria-label', 'Dein Dokument: Ziehen verschiebt, Strg und das Mausrad, Plus und Minus zoomen, 0 zeigt die Seitenbreite');
        viewport.addEventListener('keydown', function (e) {
            const m = middle();
            const step = 60;
            const keys = {
                '+': function () { zoomAt(m[0], m[1], 1.4, true); }, '=': function () { zoomAt(m[0], m[1], 1.4, true); },
                '-': function () { zoomAt(m[0], m[1], 1 / 1.4, true); }, '0': function () { sheetWidth(true); },
                ArrowUp: function () { ty += step; apply(true); }, ArrowDown: function () { ty -= step; apply(true); },
                ArrowLeft: function () { tx += step; apply(true); }, ArrowRight: function () { tx -= step; apply(true); },
            };
            if (keys[e.key]) { e.preventDefault(); keys[e.key](); }
        });
        if (window.ResizeObserver) {
            let seen = false;
            new ResizeObserver(function () {
                if (!size().w) return;
                if (!seen) first();
                else if (restart) { restart = false; start(false); }
                else apply(false);
                seen = true;
            }).observe(viewport);
        } else {
            requestAnimationFrame(first);
        }
        // where the last check left the view, else the text of page 1
        function first() {
            // all met: the whole clean page, with the stamp in its corner
            if (keep === 'whole') {
                whole(false);
            } else if (keep) {
                z = keep.z; tx = keep.tx; ty = keep.ty;
                apply(false);
            } else {
                start(false);
            }
        }

        // Full screen (a phone, or a small window): the result over the whole
        // screen, the page on top, the list below; the back button, Escape
        // or the button again close it.
        const report = view.closest('.document-report');
        let restart = false;
        function toggleFull(on) {
            const want = on === undefined ? !report.classList.contains('document-fullscreen') : on;
            if (want === report.classList.contains('document-fullscreen')) return;
            // out of the card while it covers the screen: the card's body is a
            // size container, which keeps anything fixed inside it
            if (want) {
                report.documentHome = document.createComment('document-report');
                report.parentNode.insertBefore(report.documentHome, report);
                document.body.appendChild(report);
            } else if (report.documentHome) {
                report.documentHome.parentNode.insertBefore(report, report.documentHome);
                report.documentHome.remove();
                report.documentHome = null;
            }
            report.classList.toggle('document-fullscreen', want);
            document.body.classList.toggle('document-full-open', want);
            full.innerHTML = "<i class='bi " + (want ? 'bi-fullscreen-exit' : 'bi-arrows-fullscreen') + "' aria-hidden='true'></i>";
            full.setAttribute('aria-label', want ? 'Vollbild verlassen' : 'Vollbild');
            full.title = full.getAttribute('aria-label');
            if (want) {
                history.pushState({ documentFull: true }, '');
                const back = function () { toggleFull(false); };
                report.documentBack = back;
                window.addEventListener('popstate', back, { once: true });
            } else if (report.documentBack) {
                window.removeEventListener('popstate', report.documentBack);
                report.documentBack = null;
                if (history.state && history.state.documentFull) history.back();
            }
            // the view again once the area has its new size (ResizeObserver)
            restart = true;
        }
        report.addEventListener('keydown', function (e) {
            if (e.key === 'Escape' && report.classList.contains('document-fullscreen')) toggleFull(false);
        });

        return {
            viewport: viewport,
            sheets: sheets,
            // one place and the text around it, near enough to read: the
            // view travels there slowly enough to follow, then the places of
            // the point pulse once
            place: function (m) {
                const page = m.page - 1;
                const r = { x: offset(page) + m.x - 40, y: tops[page] + m.y - 40, w: m.w + 80, h: m.h + 80 };
                const v = size();
                stage.classList.add('document-travel');
                show(r, Math.min(2.2, v.w / Math.max(r.w, 260)), true);
                setTimeout(function () {
                    stage.classList.remove('document-travel');
                    stage.querySelectorAll(".document-mark[data-n='" + m.n + "']").forEach(function (x) {
                        x.classList.remove('document-arrived');
                        void x.offsetWidth;
                        x.classList.add('document-arrived');
                    });
                }, reduced() ? 0 : 650);
            },
            overview: function () { start(true); },
            whole: function () { whole(false); },
        };
    }

    function reduced() {
        return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    }

    // A point and its places belong together: pointing at the one (in the
    // list or its number in the margin) brings out the other and dims the
    // rest; a click on a point's number goes to its next place.
    function connect(wrap, control, items) {
        const activate = function (n) {
            if (n) wrap.dataset.active = n; else delete wrap.dataset.active;
            wrap.querySelectorAll('[data-n]').forEach(function (x) { x.classList.toggle('document-active', String(x.dataset.n) === String(n)); });
        };
        const step = {};
        const go = function (n) {
            const item = items.find(function (i) { return String(i.n) === String(n); });
            if (!item || !item.marks.length) return;
            const i = step[n] === undefined ? 0 : (step[n] + 1) % item.marks.length;
            step[n] = i;
            control.place(item.marks[i]);
            activate(n);
            const where = wrap.querySelector(".document-where[data-for='" + n + "']");
            if (where && item.marks.length > 1) where.textContent = 'Stelle ' + (i + 1) + ' von ' + item.marks.length;
        };
        wrap.querySelectorAll('.document-open[data-n]').forEach(function (li) {
            li.addEventListener('mouseenter', function () { activate(li.dataset.n); });
            li.addEventListener('mouseleave', function () { if (!wrap.dataset.held) activate(null); });
        });
        wrap.querySelectorAll('.document-badge').forEach(function (badge) {
            badge.addEventListener('mouseenter', function () { activate(badge.dataset.n); });
            badge.addEventListener('mouseleave', function () { if (!wrap.dataset.held) activate(null); });
            badge.addEventListener('click', function () { go(badge.dataset.n); });
        });
        wrap.querySelectorAll('.document-chip').forEach(function (chip) {
            chip.addEventListener('focus', function () { activate(chip.dataset.n); });
            chip.addEventListener('blur', function () { activate(null); });
            chip.addEventListener('click', function () { go(chip.dataset.n); });
        });
    }

    // All points met: the points tick one after the other, a check mark
    // draws itself, a stamp lands on the page and a light passes over it.
    function celebrate(wrap, side, control, report) {
        const done = el('div', 'document-done');
        done.innerHTML = "<svg class='document-done-mark' viewBox='0 0 52 52' aria-hidden='true'>" +
            "<circle cx='26' cy='26' r='23'></circle><path d='M15 27 l7 7 l15 -16'></path></svg>" +
            "<div><div class='document-done-title'>Sauber gebaut</div>" +
            "<div class='document-done-text'>Ändere die Schrift, wie du willst: Dein Dokument hält.</div></div>";
        side.insertBefore(done, side.firstChild);
        side.querySelectorAll('.document-checklist li').forEach(function (li, i) { li.style.setProperty('--i', String(i)); });
        control.sheets.slice(0, 1).forEach(function (sheet) {
            const stamp = el('div', 'document-stamp', 'Alle ' + report.items.length + ' Punkte');
            stamp.setAttribute('aria-hidden', 'true');
            // in the corner of the part with text, which the view shows
            const page = report.pages[0];
            const crop = page.crop || { x: 0, y: 0, w: page.width };
            stamp.style.top = (crop.y + 4) + 'px';
            stamp.style.right = (page.width - crop.x - crop.w + 4) + 'px';
            sheet.appendChild(stamp);
            sheet.appendChild(el('div', 'document-shine'));
        });
        wrap.classList.add('document-success');
        if (!reduced()) {
            void wrap.offsetWidth;
            wrap.classList.add('document-success-now');
        }
    }

    function render(card, report) {
        const body = card.querySelector('.task-feedback .task-feedback-body');
        if (!body || !report) return;
        const old = card.querySelector('.document-report');
        if (old) old.remove();
        const wrap = el('div', 'document-report');
        const side = el('div', 'document-side');
        // what was met at the last check, for »neu erfüllt«
        const before = card.documentMet || null;
        const met = report.items.filter(function (i) { return i.ok; }).map(function (i) { return i.label; });
        card.documentMet = met;
        const fresh = before ? met.filter(function (l) { return before.indexOf(l) < 0; }) : [];
        const all = met.length === report.items.length;

        if (!all && fresh.length) {
            const gain = el('p', 'document-gain');
            gain.innerHTML = "<i class='bi bi-arrow-up-circle-fill' aria-hidden='true'></i>" +
                (fresh.length === 1 ? 'Ein Punkt mehr' : fresh.length + ' Punkte mehr') + ' als beim letzten Prüfen.';
            side.appendChild(gain);
        }
        const list = el('ul', 'document-checklist');
        list.setAttribute('aria-label', 'Was die Prüfung nachgesehen hat');
        report.items.forEach(function (item) {
            const li = el('li', item.ok ? 'document-ok' : 'document-open');
            if (fresh.indexOf(item.label) >= 0 && !all) li.classList.add('document-fresh');
            if (item.n) li.dataset.n = item.n;
            const icon = el('i', 'bi ' + (item.ok ? 'bi-check-circle-fill' : 'bi-circle'));
            icon.setAttribute('aria-hidden', 'true');
            li.appendChild(icon);
            const text = el('div', 'document-item');
            const head = el('div', 'document-label');
            if (item.n && item.marks && item.marks.length) {
                const chip = el('button', 'document-chip', String(item.n));
                chip.type = 'button';
                chip.dataset.n = item.n;
                chip.title = item.marks.length === 1 ? 'Die Stelle im Dokument zeigen' : 'Die nächste von ' + item.marks.length + ' Stellen zeigen';
                chip.setAttribute('aria-label', 'Punkt ' + item.n + ': ' + chip.title);
                head.appendChild(chip);
            }
            head.appendChild(el('span', 'visually-hidden', item.ok ? 'Erfüllt: ' : 'Offen: '));
            head.appendChild(document.createTextNode(item.label));
            if (li.classList.contains('document-fresh')) head.appendChild(el('span', 'document-new', 'neu erfüllt'));
            text.appendChild(head);
            if (item.n && item.marks && item.marks.length > 1) {
                const where = el('div', 'document-where', item.marks.length + ' Stellen');
                where.dataset.for = item.n;
                text.appendChild(where);
            }
            if (!item.ok && item.message) {
                const message = el('div', 'document-message');
                message.innerHTML = item.message;
                text.appendChild(message);
            }
            li.appendChild(text);
            list.appendChild(li);
        });
        side.appendChild(list);
        let note = 'So zeichnet LibreOffice deine Seiten; in Word kann eine Zeile ein Wort früher umbrechen.';
        if (report.page_count > report.pages.length) note += ' Gezeigt sind die ersten ' + report.pages.length + ' von ' + report.page_count + ' Seiten.';
        side.appendChild(el('p', 'document-note', note));
        wrap.appendChild(side);

        const view = el('div', 'document-view');
        wrap.appendChild(view);
        body.appendChild(wrap);
        const marks = [].concat.apply([], report.items.map(function (i) { return i.marks || []; }));
        // the view stays where it was between checks (not after the last
        // one, which shows the whole clean page)
        const control = report.pages.length
            ? viewer(view, report.pages, marks, all ? null : card.documentView, all ? null : function (v) { card.documentView = v; })
            : { place: function () {}, sheets: [] };
        // check again from here, after saving in Word, without scrolling up
        if (card.documentRecheck) {
            const again = el('button', 'btn btn-sm btn-success document-recheck');
            again.type = 'button';
            // without a handle the file has to be chosen again (check)
            if (card.documentHandle) {
                again.innerHTML = "<i class='bi bi-arrow-repeat' aria-hidden='true'></i><span class='document-recheck-long'>Erneut </span><span class='document-recheck-word'>prüfen</span>";
                again.setAttribute('aria-label', 'Erneut prüfen');
            } else {
                again.innerHTML = "<i class='bi bi-folder2-open' aria-hidden='true'></i><span class='document-recheck-long'>Neu wählen und </span><span class='document-recheck-word'>prüfen</span>";
                again.setAttribute('aria-label', 'Datei neu wählen und prüfen');
                again.title = 'Dieser Browser sieht nicht, wenn du die Datei speicherst: Wähle sie nach dem Speichern neu.';
            }
            again.addEventListener('click', function () { card.documentRecheck(); });
            const tools = view.querySelector('.document-tools');
            if (tools) tools.appendChild(again);
        }
        connect(wrap, control, report.items);
        if (all) celebrate(wrap, side, control, report);
        // the verdict and the page together in view (the panel is as high
        // as the window allows below the verdict, default.css)
        const box = card.querySelector('.task-feedback');
        const r = wrap.getBoundingClientRect();
        if (box && (r.bottom > window.innerHeight || r.top < 0)) box.scrollIntoView({ block: 'start' });
        fit(wrap);
    }

    // The last version kept on the server (DocumentCheck, task_api.rb), to
    // go on with at home or on another computer: its name and when, and a
    // link that saves it (through the page in Chrome and Edge, which then
    // keeps hold of it, as with the starting file).
    function when(seconds) {
        const t = new Date(seconds * 1000);
        const now = new Date();
        const time = t.toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' });
        if (t.toDateString() === now.toDateString()) return 'heute, ' + time + ' Uhr';
        return t.toLocaleDateString('de-DE', { day: 'numeric', month: 'numeric' }) + ', ' + time + ' Uhr';
    }

    function showFile(card, stored) {
        const box = card.querySelector('.document-task');
        if (!box || !stored) return;
        let line = box.querySelector('.document-latest');
        if (!line) {
            line = el('p', 'document-latest');
            box.insertBefore(line, box.querySelector('.document-browser'));
        }
        line.textContent = '';
        const icon = el('i', 'bi bi-clock-history');
        icon.setAttribute('aria-hidden', 'true');
        line.appendChild(icon);
        line.appendChild(document.createTextNode('Deine letzte Fassung: '));
        const link = el('a', null, stored.name);
        link.href = stored.url;
        link.setAttribute('download', stored.name);
        link.dataset.name = stored.name;
        line.appendChild(link);
        line.appendChild(document.createTextNode(', ' + when(stored.at) + '. Zum Weiterarbeiten, auch zu Hause.'));
        bindStarter(link);
    }

    // the stored line (»01 Einladung.docx: 6 von 8, …«) under the button
    function showStored(card, answer, stored) {
        if (stored) showFile(card, stored);
        const line = card.querySelector('.document-name');
        if (!line || card.documentHandle || card.documentFile) return;
        line.textContent = answer ? 'Zuletzt geprüft: ' + answer : 'oder hierher ziehen';
    }

    window.DocumentTask = { setup: setup, check: check, render: render, showStored: showStored, showFile: showFile };
})();
