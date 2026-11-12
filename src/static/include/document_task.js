// The card of a document task (DocumentCheck, docs/plan-textverarbeitung.md):
// the student's document from LibreOffice, Word or Nextcloud Office goes
// to the server as a form upload; the answer is a checklist and the pages
// as LibreOffice drew them, with a numbered mark at each place a point of
// the checklist fails. Point and mark light up together (as in the mail
// viewer).
//
// The starting file and the last version are plain downloads (they show
// in the browser's downloads, to open from there). Where the browser can
// keep hold of a file (Chrome, Edge on https: the File System Access API),
// the document chosen once with »Prüfen« or »Dokument wählen« (the dialog
// opens in Downloads) or dropped on the card is kept: the page looks at it
// every two seconds and checks it after each save, and says which file it
// watches and when it was saved last. Other browsers need the document
// chosen or dropped for each check.
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
    // a check of an exported PDF (accept: pdf, data-accept on the card's box)
    const PDF_TYPES = [{ description: 'PDF-Dateien', accept: { 'application/pdf': ['.pdf'] } }];
    const wantsPdf = (card) => !!card.querySelector(".document-task[data-accept='pdf']");
    // a spreadsheet (data-accept='sheet'): its own file types
    const SHEET_TYPES = [{
        description: 'Tabellen',
        accept: {
            'application/vnd.oasis.opendocument.spreadsheet': ['.ods'],
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet': ['.xlsx'],
            'application/vnd.ms-excel': ['.xls'],
        },
    }];
    const wantsSheet = (card) => !!card.querySelector(".document-task[data-accept='sheet']");
    // the programs a card names: Excel for a table, Word for a text; a PDF
    // card on a page of tables (office: sheet) names Excel too
    const sheetPage = () => !!document.querySelector(".code-language-switch-office[data-office-kind='sheet']");
    const programs = (card) => wantsSheet(card) || (wantsPdf(card) && sheetPage()) ? 'LibreOffice oder Excel' : 'LibreOffice oder Word';

    function el(tag, className, text) {
        const node = document.createElement(tag);
        if (className) node.className = className;
        if (text !== undefined) node.textContent = text;
        return node;
    }

    function hasFiles(e) {
        return e.dataTransfer && Array.from(e.dataTransfer.types || []).indexOf('Files') >= 0;
    }

    // what the card says about the chosen document; for one the page keeps
    // hold of, when it was saved last, so that a save the page does not
    // see (another file, another format) shows
    function chosen(card, name, saved) {
        const line = card.querySelector('.document-name');
        line.textContent = '';
        if (card.documentHandle) {
            const eye = el('i', 'bi bi-eye');
            eye.setAttribute('aria-hidden', 'true');
            line.appendChild(eye);
            line.appendChild(document.createTextNode(' Im Blick: '));
        }
        line.appendChild(el('strong', null, name));
        if (card.documentHandle && saved) {
            line.appendChild(document.createTextNode(', gespeichert um ' + new Date(saved).toLocaleTimeString('de-DE') + '. Speichere in ' + programs(card) + ', dann prüft die Seite von selbst.'));
        }
        card.querySelector('.document-pick').lastChild.textContent = wantsPdf(card) ? 'Anderes PDF' : wantsSheet(card) ? 'Andere Tabelle' : 'Anderes Dokument';
        mirror(card);
    }

    // the line about the document also at the top of the result, where it
    // is seen in full screen too
    function mirror(card) {
        const line = card.querySelector('.document-name');
        const status = card.querySelector('.document-status');
        if (line && status) {
            status.innerHTML = line.innerHTML;
            if (card.documentCheckedAt) {
                status.appendChild(el('span', 'document-checked-at', ' Geprüft um ' + new Date(card.documentCheckedAt).toLocaleTimeString('de-DE') + '.'));
            }
        }
    }

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

        // how short the loop is in this browser, and why another one helps
        const note = el('p', 'document-browser');
        // Chrome and Edge keep hold of a file only on https (or localhost)
        const chromium = /Chrome\/|Edg\//.test(navigator.userAgent);
        note.innerHTML = CAN_KEEP
            ? "<i class='bi bi-lightning-charge' aria-hidden='true'></i>Hier genügt es, in " + programs(card) + ' zu speichern: Die Seite merkt es und prüft von selbst.'
            : chromium && !window.isSecureContext
            ? "<i class='bi bi-info-circle' aria-hidden='true'></i>Diese Seite läuft ohne https, und dann darf auch Chrome eine Datei nicht im Blick behalten. " +
              'Wähle sie vor jedem Prüfen neu oder zieh sie auf die Karte.'
            : "<i class='bi bi-info-circle' aria-hidden='true'></i>In Chrome und Edge genügt es, in " + programs(card) + ' zu speichern, und die Seite prüft von selbst. ' +
              'Dieser Browser kann eine Datei nicht im Blick behalten: Wähle sie vor jedem Prüfen neu oder zieh sie auf die Karte.';
        box.appendChild(note);

        button.addEventListener('click', async function () {
            if (!CAN_KEEP) {
                input.click();
                return;
            }
            let handle;
            try {
                [handle] = await window.showOpenFilePicker({ types: wantsPdf(card) ? PDF_TYPES : wantsSheet(card) ? SHEET_TYPES : TYPES, multiple: false, id: wantsPdf(card) ? 'workspace-pdf' : wantsSheet(card) ? 'workspace-sheet' : 'workspace-document', startIn: 'downloads' });
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
        const poll = async function () {
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
            if (seen.shown !== file.lastModified) {
                seen.shown = file.lastModified;
                chosen(card, file.name, file.lastModified);
            }
            if (file.lastModified === seen.time && file.size === seen.size) return;
            // a save in progress: wait until the size holds still, looking
            // again in half a second; the result says so at once
            if (seen.pending !== file.lastModified + ':' + file.size) {
                seen.pending = file.lastModified + ':' + file.size;
                indicate(card, 'Gespeichert, wird gleich geprüft …');
                setTimeout(poll, 500);
                return;
            }
            run();
        };
        card.documentWatch = setInterval(poll, WATCH_MS);
    }

    // a check coming or running, from the first moment: the »prüfen«
    // button beside the page turns into »Prüft …« with a spinner (full
    // screen covers the card's own button), the page a little paler;
    // text: what screen readers hear
    function indicate(card, text) {
        const shown = card.documentReportEl && card.documentReportEl.isConnected ? card.documentReportEl : null;
        if (!shown) return;
        shown.classList.add('document-checking');
        const again = shown.querySelector('.document-recheck');
        if (again && !again.dataset.idle) {
            again.dataset.idle = again.innerHTML;
            again.disabled = true;
            again.innerHTML = "<span class='spinner-border spinner-border-sm' aria-hidden='true'></span><span>Prüft …</span>";
        }
        const status = shown.querySelector('.document-status');
        if (status) status.setAttribute('aria-busy', 'true');
        if (again) again.setAttribute('aria-label', text);
    }

    function unindicate(card) {
        const shown = card.documentReportEl;
        if (shown && shown.isConnected) {
            shown.classList.remove('document-checking');
            const again = shown.querySelector('.document-recheck');
            if (again && again.dataset.idle) {
                again.innerHTML = again.dataset.idle;
                delete again.dataset.idle;
                again.disabled = false;
                again.setAttribute('aria-label', card.documentHandle ? 'Erneut prüfen' : 'Datei neu wählen und prüfen');
            }
            const status = shown.querySelector('.document-status');
            if (status) status.removeAttribute('aria-busy');
        }
        mirror(card);
    }

    // »Erneut prüfen« on a kept file that has not changed since its last
    // check: no new check, but what may have happened (the result stays)
    function unchanged(card, file) {
        chosen(card, file.name, file.lastModified);
        const line = card.querySelector('.document-name');
        const note = el('span', 'document-unchanged');
        note.innerHTML = '<br>Seit der letzten Prüfung hat sich diese Datei nicht geändert: Zuletzt gespeichert wurde sie um ' +
            new Date(file.lastModified).toLocaleTimeString('de-DE') + '. Hast du danach gespeichert, ging es in eine andere Datei. ' +
            'LibreOffice fragt beim Speichern nach dem Dateiformat: Mit <em>Word…-Format verwenden</em> bleibt es dieselbe Datei, <em>ODF-Format verwenden</em> legt daneben eine neue .odt-Datei an. ' +
            'Bearbeitest du eine andere Datei (etwa eine zweite aus den Downloads), wähle sie mit <em>Anderes Dokument</em>.';
        line.appendChild(note);
        mirror(card);
    }

    // Reads the document (again, if the browser keeps hold of it) and
    // sends it. busy() when it goes; done(data) with the server's answer.
    async function check(card, busy, done) {
        // one check at a time (a click while the page checks after a save)
        if (card.documentBusy) return;
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
            // before reading the file, which takes a while on a network drive
            indicate(card, 'Prüft …');
            try {
                if (card.documentHandle.queryPermission && await card.documentHandle.queryPermission({ mode: 'read' }) !== 'granted') {
                    await card.documentHandle.requestPermission({ mode: 'read' });
                }
                file = await card.documentHandle.getFile();
                const seen = card.documentSeen;
                if (seen && seen.checked && file.lastModified === seen.time && file.size === seen.size) {
                    unindicate(card);
                    unchanged(card, file);
                    return;
                }
                chosen(card, file.name, file.lastModified);
            } catch (err) {
                card.documentHandle = null;
                unindicate(card);
                done(invalid('Die Datei kann ich nicht mehr lesen. Wurde sie verschoben, umbenannt oder gelöscht? Wähle sie noch einmal.'));
                return;
            }
        }
        if (!file) {
            card.querySelector('.document-pick').click();
            return;
        }
        if (file.size > MAX_BYTES) {
            unindicate(card);
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
        // in the result too (full screen covers the card's button)
        const shown = card.documentReportEl && card.documentReportEl.isConnected ? card.documentReportEl : null;
        indicate(card, 'Prüft ' + file.name + ' …');
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
            card.documentSeen.checked = true;
            card.documentCheckedAt = Date.now();
            done(data);
        } catch (err) {
            done({ success: false });
        } finally {
            card.documentBusy = false;
            // a check that brought no new result: the old one as it was
            if (shown && shown.isConnected) unindicate(card);
        }
    }

    // Where the website cannot scroll the panel to the top of the window
    // (the card at the end of the page), the panel takes the height that is
    // left below it, so that the verdict, the list and the page still fit;
    // with the window's next change of size it goes back to the height in
    // default.css.
    function fit(wrap, box) {
        const room = function () { return window.innerHeight - Math.max(wrap.getBoundingClientRect().top, 0) - 12; };
        const css = wrap.getBoundingClientRect().height;
        // first more room below the page, so that it can scroll the verdict
        // to the top and the panel keeps its height
        if (room() < css && box) {
            const add = css - room() + 24;
            const now = parseFloat(getComputedStyle(document.body).paddingBottom) || 0;
            if (!document.body.dataset.documentRoom || +document.body.dataset.documentRoom < add) {
                document.body.style.paddingBottom = (now - (+document.body.dataset.documentRoom || 0) + add) + 'px';
                document.body.dataset.documentRoom = String(add);
            }
            box.scrollIntoView({ block: 'start' });
        }
        // a window too low even so: the panel takes what is left
        if (room() < css) wrap.style.height = Math.max(280, room()) + 'px';
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
        // the zoom by the wheel: its target, whether a frame is coming, and
        // whether the move in progress is one of its steps
        let goal = null, gliding = false, easing = false;
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
            if (pageLabel) {
                const now = current() + 1;
                pageLabel.textContent = now + ' / ' + pages.length;
                pageLabel.title = 'Seite ' + now + ' von ' + pages.length;
                pageLabel.setAttribute('aria-label', pageLabel.title);
            }
            // anything but the eased zoom itself ends it (a drag, a button)
            if (!easing) goal = null;
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
        // the page in view: the one under the middle of the view
        function current() {
            const v = size();
            const y = (v.h / 2 - ty) / z;
            let i = 0;
            tops.forEach(function (top, k) { if (y >= top - GAP / 2) i = k; });
            return i;
        }
        function text(animate, i) {
            i = i === undefined ? current() : i;
            const v = size();
            const page = pages[i];
            const c = page.crop || { x: 0, y: 0, w: page.width, h: page.height };
            const left = c.left !== undefined ? c.left : c.x;
            const right = c.left !== undefined ? c.x + c.w - 30 : c.x + c.w;
            z = Math.max(MIN, Math.min(MAX, (v.w - GUTTER - edge()) / (right - left)));
            tx = GUTTER - (offset(i) + left) * z;
            ty = edge() - (tops[i] + c.y) * z;
            apply(animate);
        }
        // »Seitenbreite«: the whole sheet in view from edge to edge, as large
        // as the numbers in its left margin allow, from its top edge
        function sheetWidth(animate, i) {
            i = i === undefined ? current() : i;
            fitting = true;
            fit = 'width';
            const v = size();
            const page = pages[i];
            const c = page.crop || { x: 0, y: 0, w: page.width, h: page.height };
            const left = c.left !== undefined ? c.left : c.x;
            z = Math.max(MIN, Math.min(MAX, (v.w - 2 * edge()) / page.width, (v.w - GUTTER - edge()) / (page.width - left)));
            tx = v.w - edge() - (offset(i) + page.width) * z;
            ty = edge() - tops[i] * z;
            apply(animate);
            fitting = false;
        }
        // the first view: the first sheet's width where there is room, its
        // text on a phone
        function start(animate) {
            if (size().w < 560) text(animate, 0);
            else sheetWidth(animate, 0);
        }
        // »Ganze Seite«: the whole sheet in view, as large as fits
        function whole(animate, i) {
            i = i === undefined ? current() : i;
            fitting = true;
            fit = 'whole';
            show({ x: offset(i), y: tops[i], w: pages[i].width, h: pages[i].height }, null, animate, edge());
            fitting = false;
        }
        // another page: its top at the top of the view, as large as now
        // (or whole, if the whole page was in view)
        function go(i) {
            i = Math.max(0, Math.min(pages.length - 1, i));
            if (fit === 'whole') return whole(true, i);
            if (fit === 'width') return sheetWidth(true, i);
            ty = edge() - tops[i] * z;
            apply(true);
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
        // with more than one page: which one is in view, and the one before
        // and after
        let pageLabel = null;
        if (pages.length > 1) {
            const nav = el('div', 'btn-group document-tool-group');
            nav.setAttribute('role', 'group');
            nav.setAttribute('aria-label', 'Seiten');
            tools.appendChild(nav);
            button('bi-chevron-up', 'Vorige Seite', function () { go(current() - 1); }, nav);
            pageLabel = el('span', 'btn btn-sm btn-outline-secondary document-zoom document-page');
            nav.appendChild(pageLabel);
            button('bi-chevron-down', 'Nächste Seite', function () { go(current() + 1); }, nav);
        }
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
        function glide(x, y, factor) {
            const base = goal ? goal.z : z;
            goal = { x: x, y: y, z: Math.min(MAX, Math.max(MIN, base * factor)) };
            if (reduced()) {
                const to = goal.z;
                goal = null;
                zoomAt(x, y, to / z, false);
                return;
            }
            if (gliding) return;
            gliding = true;
            const step = function () {
                if (!goal) { gliding = false; return; }
                const left = goal.z / z;
                if (Math.abs(left - 1) < 0.002) {
                    easing = true;
                    zoomAt(goal.x, goal.y, left, false);
                    easing = false;
                    goal = null;
                    gliding = false;
                    return;
                }
                easing = true;
                zoomAt(goal.x, goal.y, Math.pow(left, 0.4), false);
                easing = false;
                if (goal) requestAnimationFrame(step);
                else gliding = false;
            };
            requestAnimationFrame(step);
        }
        viewport.addEventListener('wheel', function (e) {
            const box = viewport.getBoundingClientRect();
            const dy = e.deltaMode === 1 ? e.deltaY * 16 : e.deltaY;
            const dx = e.deltaMode === 1 ? e.deltaX * 16 : e.deltaX;
            if (e.ctrlKey || e.metaKey) {
                e.preventDefault();
                // every event moves one target, which the view eases to: a
                // notch of a mouse wheel at most 12 %, the small steps of a
                // pinch on a touchpad by their size. (Two ways at once, one
                // immediate and one eased, pulled against each other with a
                // wheel that sends both kinds of steps.)
                const factor = Math.min(1.12, Math.max(1 / 1.12, Math.exp(-dy * 0.01)));
                glide(e.clientX - box.left, e.clientY - box.top, factor);
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
                PageUp: function () { go(current() - 1); }, PageDown: function () { go(current() + 1); },
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
        // adopt: the result of a new check takes over the full screen the
        // last one had: no new step in the history, the view kept
        function toggleFull(on, adopt) {
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
                if (!adopt) history.pushState({ documentFull: true }, '');
                const back = function () { toggleFull(false); };
                report.documentBack = back;
                window.addEventListener('popstate', back, { once: true });
            } else if (report.documentBack) {
                window.removeEventListener('popstate', report.documentBack);
                report.documentBack = null;
                if (history.state && history.state.documentFull) history.back();
            }
            // the view again once the area has its new size (ResizeObserver)
            if (!adopt) restart = true;
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
                // a tall place (many empty paragraphs): its beginning, with
                // what comes before it, not the whole of it filling the view
                const r = { x: offset(page) + m.x - 40, y: tops[page] + m.y - 60, w: m.w + 80, h: Math.min(m.h, 140) + 100 };
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
            adoptFull: function () { toggleFull(true, true); },
            whole: function () { whole(false, 0); },
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
            (report.sheet
                ? "<div><div class='document-done-title'>Rechnet richtig</div>" +
                  "<div class='document-done-text'>Ändere eine Zahl: Alles, was mit ihr rechnet, rechnet mit.</div></div>"
                : report.pdf
                ? "<div><div class='document-done-title'>Fertig zum Weitergeben</div>" +
                  "<div class='document-done-text'>So sieht dein PDF auf jedem Computer aus.</div></div>"
                : "<div><div class='document-done-title'>Sauber gebaut</div>" +
                  "<div class='document-done-text'>Ändere die Schrift, wie du willst: Dein Dokument hält.</div></div>");
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
        // the last result, in the card or (full screen) in the body
        const old = card.documentReportEl && card.documentReportEl.isConnected ? card.documentReportEl : card.querySelector('.document-report');
        const wasFull = !!(old && old.classList.contains('document-fullscreen'));
        if (old) {
            if (old.documentBack) window.removeEventListener('popstate', old.documentBack);
            if (old.documentHome) old.documentHome.remove();
            old.remove();
        }
        const wrap = el('div', 'document-report');
        const side = el('div', 'document-side');
        // the verdict, for full screen, which does not take the card's
        // line along
        const done = report.items.filter(function (i) { return i.ok; }).length;
        side.appendChild(el('p', 'document-verdict', done === report.items.length
            ? 'Alle ' + done + ' Punkte erfüllt.' : done + ' von ' + report.items.length + ' Punkten erfüllt.'));
        const status = el('p', 'document-status');
        status.setAttribute('aria-live', 'polite');
        side.appendChild(status);
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
        let note = report.sheet ? 'So sieht deine Tabelle aus, gezeichnet aus ihren Zellen; die Formeln stehen unter »Was der Workspace gelesen hat«.'
            : report.pdf ? 'Das sind die Seiten deines PDFs, so wie jeder sie sieht.'
            : 'So zeichnet LibreOffice deine Seiten; in Word kann eine Zeile ein Wort früher umbrechen.';
        if (report.page_count > report.pages.length) note += ' Gezeigt sind die ersten ' + report.pages.length + ' von ' + report.page_count + ' Seiten.';
        side.appendChild(el('p', 'document-note', note));
        wrap.appendChild(side);

        const view = el('div', 'document-view');
        wrap.appendChild(view);
        body.appendChild(wrap);
        card.documentReportEl = wrap;
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
        mirror(card);
        if (wasFull && control.adoptFull) control.adoptFull();
        if (all) celebrate(wrap, side, control, report);
        if (wasFull) return;
        // the verdict and the page together in view (the panel is as high
        // as the window allows below the verdict, default.css)
        const box = card.querySelector('.task-feedback');
        const r = wrap.getBoundingClientRect();
        if (box && (r.bottom > window.innerHeight || r.top < 0)) box.scrollIntoView({ block: 'start' });
        fit(wrap, box);
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
