// Class board (/klasse): »Jetzt in den Workspaces nachsehen« runs a
// Workspace task for every student of the group and marks the live result
// in the table, without changing the students' own results.
(function () {
    // Tabler icons from the bundled sprite (include/tabler/tabler.svg), at the
    // versioned address the server put into the page's own icons.
    const used = document.querySelector('use[href*="/include/tabler/tabler.svg"]');
    const SPRITE = used ? used.getAttribute('href').split('#')[0] : '/include/tabler/tabler.svg';
    const ICONS = { correct: 'check', wrong: 'x', unavailable: 'help', invalid: 'help' };

    function icon(name) {
        const ns = 'http://www.w3.org/2000/svg';
        const svg = document.createElementNS(ns, 'svg');
        svg.setAttribute('class', 'ti ti-' + name);
        svg.setAttribute('aria-hidden', 'true');
        const use = document.createElementNS(ns, 'use');
        use.setAttribute('href', SPRITE + '#' + name);
        svg.appendChild(use);
        return svg;
    }

    function plain(html) {
        const div = document.createElement('div');
        div.innerHTML = html || '';
        return div.textContent.trim();
    }

    function nameFor(email) {
        const row = document.querySelector('tr[data-email="' + CSS.escape(email) + '"] th');
        return row ? row.textContent : email;
    }

    function show(task, label, data) {
        const results = data.results || {};
        document.querySelectorAll('td.cb-cell[data-task="' + CSS.escape(task) + '"]').forEach((td) => {
            const email = td.closest('tr').dataset.email;
            const r = results[email];
            if (!r) return;
            // The live result replaces the stored one in this cell (until reload).
            td.classList.remove('cb-correct', 'cb-wrong', 'cb-sent', 'cb-stale', 'cb-none', 'cb-stuck',
                'cb-live-correct', 'cb-live-wrong', 'cb-live-unavailable', 'cb-live-invalid');
            td.classList.add('cb-live');
            td.classList.add('cb-live-' + r.status);
            td.textContent = '';
            td.appendChild(icon(ICONS[r.status] || 'help'));
            td.title = 'Jetzt: ' + plain(r.message);
        });

        const box = document.querySelector('.class-live-results');
        box.innerHTML = '';
        const time = new Date((data.checked_at || 0) * 1000).toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' });
        const h = document.createElement('h2');
        h.textContent = label + ': Stand ' + time;
        box.appendChild(h);
        const emails = Object.keys(results).sort((a, b) => nameFor(a).localeCompare(nameFor(b)));
        const ok = emails.filter((e) => results[e].status === 'correct');
        const p = document.createElement('p');
        p.textContent = ok.length + ' von ' + emails.length + ' erfüllen die Aufgabe gerade.';
        box.appendChild(p);
        const ul = document.createElement('ul');
        ul.className = 'class-live-list';
        emails.filter((e) => results[e].status !== 'correct').forEach((email) => {
            const li = document.createElement('li');
            li.className = 'cb-live-' + results[email].status;
            const name = document.createElement('strong');
            name.textContent = nameFor(email);
            li.appendChild(name);
            const msg = document.createElement('div');
            msg.innerHTML = results[email].message || '';
            li.appendChild(msg);
            ul.appendChild(li);
        });
        box.appendChild(ul);
    }

    // Pin and unpin activities for the group (shown first on students' start page).
    const pins = document.querySelector('.class-pins');
    if (pins) {
        const select = pins.querySelector('.class-pin-select');
        if (select) select.addEventListener('change', function () {
            if (!select.value) return;
            api_call('/api/class/pin', { group: pins.dataset.group, slug: select.value }, function (data) {
                if (data.success) window.location.reload();
            });
        });
        pins.querySelectorAll('.class-unpin').forEach((button) => {
            button.addEventListener('click', function () {
                api_call('/api/class/unpin', { group: pins.dataset.group, slug: button.dataset.slug }, function (data) {
                    if (data.success) window.location.reload();
                });
            });
        });
    }

    // »Vorlage aktuell?«: who lacks the template's newest commit. Nothing is
    // pulled; the list says who should run git pull.
    const LABELS = {
        outdated: 'Alte Version, sollte git pull ausführen',
        current: 'Neueste Version',
        absent: 'Nicht geklont',
        other: 'Ordner ist kein Klon dieser Vorlage',
        unavailable: 'Workspace läuft nicht',
    };
    const ORDER = ['outdated', 'absent', 'other', 'unavailable', 'current'];
    const template = document.querySelector('.class-template');
    const templateBox = document.querySelector('.class-template-results');
    // data-force='yes' (the link under a result): ask the template's host
    // now, not the answer remembered for up to an hour.
    document.addEventListener('click', function (e) {
        const button = e.target.closest('.class-template-check');
        if (!button || !template) return;
        const old = button.innerHTML;
        button.disabled = true;
        button.innerHTML = old + ' …';
        const request = { group: template.dataset.group, slug: template.dataset.slug };
        if (button.dataset.force) request.force = button.dataset.force;
        api_call('/api/class/template_status', request, function (data) {
            button.disabled = false;
            button.innerHTML = old;
            if (!data.success) return;
            const box = templateBox;
            box.innerHTML = '';
            const folder = '/workspace/' + data.folder;
            const p = document.createElement('p');
            if (!data.reachable) {
                p.textContent = 'Die Adresse der Vorlage hat gerade nicht geantwortet. Versuch es gleich noch einmal.';
                box.appendChild(p);
                return;
            }
            const emails = Object.keys(data.results).sort((a, b) => nameFor(a).localeCompare(nameFor(b)));
            const count = (status) => emails.filter((e) => data.results[e] === status).length;
            const time = new Date((data.checked_at || 0) * 1000).toLocaleTimeString('de-DE', { hour: '2-digit', minute: '2-digit' });
            const h = document.createElement('h2');
            h.textContent = 'Vorlage: Stand ' + time;
            box.appendChild(h);
            p.textContent = count('current') + ' von ' + emails.length +
                ' haben die neueste Version in ' + folder + ', ' + count('outdated') + ' eine alte. ';
            const again = document.createElement('button');
            again.type = 'button';
            again.className = 'btn btn-link btn-sm p-0 align-baseline class-template-check';
            again.dataset.force = 'yes';
            again.title = 'Sonst fragt der Workspace die Vorlage höchstens einmal pro Stunde';
            again.textContent = 'Neueste Version neu abfragen';
            p.appendChild(again);
            box.appendChild(p);
            const ul = document.createElement('ul');
            ul.className = 'class-template-list';
            ORDER.filter((s) => s !== 'current' && count(s) > 0).forEach((status) => {
                const li = document.createElement('li');
                li.className = 'class-template-' + status;
                const label = document.createElement('strong');
                label.textContent = LABELS[status] + ': ';
                li.appendChild(label);
                li.appendChild(document.createTextNode(emails.filter((e) => data.results[e] === status).map(nameFor).join(', ')));
                ul.appendChild(li);
            });
            box.appendChild(ul);
            if (count('outdated') > 0 && data.students_see_notice) {
                const hint = document.createElement('p');
                hint.className = 'class-live-note';
                hint.innerHTML = 'Die Betroffenen sehen auf der Seite der Aktivität selbst den Hinweis, <code>git pull</code> auszuführen.';
                box.appendChild(hint);
            }
        });
    });

    document.querySelectorAll('.class-live-check').forEach((button) => {
        button.addEventListener('click', function () {
            const live = button.closest('.class-live');
            const label = button.textContent;
            button.disabled = true;
            const old = button.textContent;
            button.textContent = old + ' …';
            const box = document.querySelector('.class-live-results');
            box.innerHTML = '';
            const wait = document.createElement('p');
            wait.className = 'class-live-wait';
            wait.textContent = label + ': sehe in den Workspaces nach …';
            box.appendChild(wait);
            api_call('/api/class/check_now', { group: live.dataset.group, task: button.dataset.task }, function (data) {
                if (!data.success) wait.textContent = label + ': Das hat gerade nicht geklappt.';
                button.disabled = false;
                button.textContent = old;
                if (data.success) show(button.dataset.task, label, data);
            });
        });
    });
})();

// The people of a group (main.rb class_people_html): the login codes of the
// group while someone is waiting, whether each workspace runs, and looking
// into a student's account. Codes and workspaces come over the teachers'
// websocket, as on the admin page.
(function () {
    const people = document.querySelector('.class-people');
    if (!people) return;
    const group = people.dataset.group;
    const REASON_KEY = 'hackschule-look-in-reason';

    function nameFor(email) {
        const row = people.querySelector('tr[data-email="' + CSS.escape(email) + '"] .class-person');
        return row ? row.textContent : email;
    }

    // The codes on a projector: names and codes only, big, live; the
    // students cannot read their mail at school, so they read it here.
    let codesNow = [];
    let projector = null;

    // Everything on one screen: start big, and while the list does not fit
    // shrink the type (the grid then makes more columns), down to a size
    // that can still be read from the back of the room.
    function fitProjector() {
        if (!projector) return;
        const list = projector.querySelector('.class-codes-list');
        const max = Math.min(42, Math.max(22, window.innerWidth / 30));
        const min = 16;
        let size = max;
        list.style.fontSize = size + 'px';
        const bottom = parseFloat(getComputedStyle(projector).paddingBottom) || 0;
        const room = () => projector.clientHeight - list.offsetTop - bottom;
        while (size > min && list.scrollHeight > room()) {
            size = Math.max(min, size * 0.92);
            list.style.fontSize = size + 'px';
        }
    }

    window.addEventListener('resize', fitProjector);

    function renderProjector() {
        if (!projector) return;
        const list = projector.querySelector('.class-codes-list');
        list.innerHTML = '';
        if (codesNow.length === 0) {
            const p = document.createElement('p');
            p.className = 'class-codes-empty';
            p.textContent = 'Noch wartet niemand auf einen Code. Meldet euch an, die Codes erscheinen dann hier.';
            list.appendChild(p);
            return;
        }
        codesNow.slice().sort((a, b) => nameFor(a.email).localeCompare(nameFor(b.email))).forEach((l) => {
            const row = document.createElement('div');
            row.className = 'class-codes-row';
            const name = document.createElement('span');
            name.className = 'class-codes-name';
            name.textContent = nameFor(l.email);
            const code = document.createElement('span');
            code.className = 'class-codes-code';
            code.textContent = l.code;
            row.appendChild(name);
            row.appendChild(code);
            list.appendChild(row);
        });
        fitProjector();
    }

    // Opening adds a history entry, so the browser's Back button closes the
    // codes instead of leaving the class page; closing otherwise takes the
    // entry away again.
    function closeProjector(fromHistory) {
        if (!projector) return;
        projector.remove();
        projector = null;
        document.body.classList.remove('subview-open');
        document.removeEventListener('keydown', projectorKey);
        if (fromHistory !== true && history.state && history.state.classCodes) history.back();
    }

    window.addEventListener('popstate', () => closeProjector(true));

    function projectorKey(e) {
        if (e.key === 'Escape') closeProjector();
    }

    document.addEventListener('click', (e) => {
        if (!e.target.closest('.class-codes-show')) return;
        history.pushState({ classCodes: true }, '');
        projector = document.createElement('div');
        projector.className = 'class-codes-projector';
        projector.setAttribute('role', 'dialog');
        projector.setAttribute('aria-modal', 'true');
        projector.setAttribute('aria-label', 'Anmelde-Codes für ' + group);
        projector.innerHTML = "<button type='button' class='class-codes-close' aria-label='Schließen'><i class='bi bi-x-lg' aria-hidden='true'></i></button>" +
            "<h2></h2><div class='class-codes-list' aria-live='polite'></div>";
        const title = projector.querySelector('h2');
        title.textContent = 'Anmelde-Codes';
        const groupName = document.createElement('span');
        groupName.className = 'class-codes-group';
        groupName.textContent = group;
        groupName.title = group;
        title.appendChild(groupName);
        projector.querySelector('.class-codes-close').addEventListener('click', closeProjector);
        document.body.appendChild(projector);
        document.body.classList.add('subview-open');
        document.addEventListener('keydown', projectorKey);
        renderProjector();
        projector.querySelector('.class-codes-close').focus();
    });

    function showCodes(lines) {
        const box = document.querySelector('.class-login-codes');
        const tbody = box.querySelector('tbody');
        const now = Math.floor(Date.now() / 1000);
        const mine = (lines || []).filter((l) => l.expires_at > now && (l.groups || []).indexOf(group) >= 0);
        tbody.innerHTML = '';
        mine.forEach((l) => {
            const tr = document.createElement('tr');
            [nameFor(l.email), l.email, l.code].forEach((text, i) => {
                const td = document.createElement('td');
                td.textContent = text;
                if (i === 2) td.className = 'class-login-code';
                tr.appendChild(td);
            });
            tbody.appendChild(tr);
        });
        box.hidden = mine.length === 0;
        codesNow = mine;
        renderProjector();
    }

    function showRunning(stats) {
        people.querySelectorAll('.class-ws-status').forEach((td) => {
            const entry = (stats || {})[td.dataset.fsTag];
            const running = !!entry;
            td.querySelector('.class-ws-text').textContent = running ? 'läuft' : 'aus';
            td.classList.toggle('class-ws-running', running);
            const row = td.closest('tr');
            const s = (entry && entry.stats) || {};
            // The row lights up yellow while the Workspace works, as on the
            // admin page: as strong as its CPU load; quiet or off, it fades back.
            const cpu = running ? parseFloat(String(s.CPUPerc || '0').replace(',', '.')) || 0 : 0;
            row.classList.toggle('class-ws-active', cpu >= 1);
            row.style.setProperty('--ws-activity', Math.min(1, cpu / 100).toFixed(2));
            row.querySelector('.class-ws-cpu').textContent = running && s.CPUPerc ? s.CPUPerc.replace('.', ',') : '–';
            row.querySelector('.class-ws-ram').textContent = running && s.MemUsage ? s.MemUsage.split('/')[0].trim().replace('iB', 'B').replace('.', ',') : '–';
        });
    }

    // The server as a whole, so a slow Workspace can be told from a busy
    // server: the same cards as on the admin page.
    function showServer(data) {
        const cards = people.querySelector('.class-server-cards');
        if (!cards || !data) return;
        // As on the admin page, in English.
        const fill = (card, i, percent, html) => {
            card.querySelectorAll('.progress-bar')[i].style.width = percent + '%';
            card.querySelectorAll('p')[i].innerHTML = html;
        };
        const cpu = cards.querySelector('.class-server-cpu');
        const storage = cards.querySelector('.class-server-storage');
        fill(cpu, 0, data.cpu_usage, Number(data.cpu_usage).toFixed(2) + '% of ' + data.num_cores + ' cores');
        fill(cpu, 1, data.ram_percent, data.ram_used + ' of ' + data.ram_total);
        fill(storage, 0, data.disk_root_percent, data.disk_root_used + ' of ' + data.disk_root_total);
        fill(storage, 1, data.disk_user_percent, data.disk_user_used + ' of ' + data.disk_user_total);
        cards.hidden = false;
    }

    function connect() {
        const ws = new WebSocket((location.port === '8025' ? 'ws://' : 'wss://') + location.host + '/ws');
        ws.onmessage = (event) => {
            let data;
            try { data = JSON.parse(event.data); } catch (e) { return; }
            if (data.action === 'login_codes') showCodes(data.lines);
            if (data.stats) showRunning(data.stats);
            if (data.server_stats) showServer(data.server_stats);
        };
        ws.onclose = () => setTimeout(connect, 5000);
    }
    connect();

    // Before looking in: one sentence for the student, the last one prefilled,
    // so that grading a task across the class needs it only once.
    function askReason(name, title, go) {
        let last = '';
        try { last = localStorage.getItem(REASON_KEY) || ''; } catch (e) {}
        const id = 'look-in-reason';
        showTemplateModal(
            $('<div>').text(title).html(),
            "<p>Schreib in einem Satz, warum. " + $('<div>').text(name).html() +
            " liest ihn bei der nächsten Anmeldung, zum Beispiel: »Ich sehe mir deine Abgabe zu Aufgabe 3 an.«</p>" +
            "<textarea class='form-control' id='" + id + "' rows='2' maxlength='300'></textarea>" +
            "<p class='text-danger mt-2 mb-0' id='" + id + "-missing' hidden>Bitte schreib einen kurzen Satz.</p>",
            "<i class='bi bi-check-lg'></i>Weiter", 'btn-primary',
            "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-secondary',
            function () {
                const reason = $('#' + id).val().trim();
                if (reason.length < 5) {
                    $('#' + id + '-missing').prop('hidden', false);
                    return false;
                }
                try { localStorage.setItem(REASON_KEY, reason); } catch (e) {}
                go(reason);
            });
        $('#' + id).val(last);
        setTimeout(() => $('#' + id).trigger('focus').trigger('select'), 300);
    }

    people.addEventListener('click', (e) => {
        const button = e.target.closest('.bu-class-impersonate, .bu-class-open-workspace');
        if (!button) return;
        const email = button.dataset.email;
        const impersonate = button.classList.contains('bu-class-impersonate');
        const go = (reason) => {
            button.disabled = true;
            if (impersonate) {
                api_call('/api/impersonate', { email: email, reason: reason }, function (data) {
                    if (data.success !== false) location.href = '/';
                });
            } else {
                // The code icon turns until the Workspace answers, as on the
                // start page; check_server_online stops it and opens the tab.
                const icon = button.querySelector('.bi');
                if (icon) icon.classList.add('bi-spin');
                api_call('/api/start_server_as_admin', { email: email, reason: reason }, function (data) {
                    if (data.success === false || !data.watch_tag) {
                        button.disabled = false;
                        if (icon) icon.classList.remove('bi-spin');
                        return;
                    }
                    check_server_tag_update(data.watch_tag);
                    check_server_online(data.watch_tag, true);
                });
            }
        };
        // The test account is the teacher's own: no sentence, no notice.
        if (button.dataset.test) go('');
        else askReason(button.dataset.name, impersonate ? button.dataset.name + ' nachahmen' : 'Workspace von ' + button.dataset.name + ' öffnen', go);
    });
})();

// The tabs of the class page: one job each. The choice stays in the
// address, so a reload or the group selector keeps it.
(function () {
    const tabs = Array.from(document.querySelectorAll('.class-tab'));
    if (tabs.length === 0) return;

    function select(tab, focus) {
        tabs.forEach((t) => {
            const on = t === tab;
            t.setAttribute('aria-selected', on ? 'true' : 'false');
            t.tabIndex = on ? 0 : -1;
            document.getElementById(t.getAttribute('aria-controls')).hidden = !on;
        });
        if (focus) tab.focus();
        const url = new URL(location.href);
        url.searchParams.set('tab', tab.dataset.tab);
        history.replaceState(null, '', url);
        document.querySelectorAll('.class-board-select input[name=tab]').forEach((input) => { input.value = tab.dataset.tab; });
    }

    tabs.forEach((tab, i) => {
        tab.addEventListener('click', () => select(tab, false));
        tab.addEventListener('keydown', (e) => {
            const step = e.key === 'ArrowRight' ? 1 : e.key === 'ArrowLeft' ? -1 : 0;
            if (step === 0) return;
            e.preventDefault();
            select(tabs[(i + step + tabs.length) % tabs.length], true);
        });
    });
})();

// »Neu hier?« on the class page, until the teacher hides it (this browser).
(function () {
    const box = document.querySelector('.class-welcome');
    if (!box) return;
    const KEY = 'hackschule-class-welcome-hidden';
    let hidden = false;
    try { hidden = localStorage.getItem(KEY) === '1'; } catch (e) {}
    box.hidden = hidden;
    box.querySelector('.class-welcome-close').addEventListener('click', () => {
        box.hidden = true;
        try { localStorage.setItem(KEY, '1'); } catch (e) {}
    });
})();

// »Alle Abgaben löschen« under a hand-in list: a new round for the group.
// Asks first; what goes cannot be brought back.
document.addEventListener('click', function (e) {
    const button = e.target.closest('.class-delete-submissions');
    if (!button) return;
    const codes = document.querySelector('.class-login-codes');
    const group = codes ? codes.dataset.group : '';
    const count = Number(button.dataset.count);
    confirm_dialog({
        title: 'Alle Abgaben löschen?',
        text: (count === 1 ? 'Die Abgabe' : 'Alle ' + count + ' Abgaben') + ' zu »' + button.dataset.title + '« aus ' + group +
            ' werden gelöscht. Die Lernenden sehen danach keine Abgabe mehr und können neu abgeben. ' +
            'Wer auch in einer anderen Gruppe ist, verliert die Abgabe dort ebenso. ' +
            'Das lässt sich nicht rückgängig machen; lade vorher herunter, was du behalten willst.',
        confirm: count === 1 ? 'Abgabe löschen' : count + ' Abgaben löschen', icon: 'bi-trash', danger: true,
    }, function () {
        button.disabled = true;
        api_call('/api/class/delete_submissions', { group: group, task: button.dataset.task }, function (data) {
            if (data.success) window.location.reload();
            else button.disabled = false;
        });
    });
});
