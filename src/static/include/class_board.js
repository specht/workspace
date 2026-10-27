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

    // The text of a message, read in a document of its own: nothing in it
    // loads or runs there (an element made here would load its images).
    function plain(html) {
        const doc = new DOMParser().parseFromString(html || '', 'text/html');
        return doc.body.textContent.trim();
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

    // The activity of the board: typed into a search field instead of a
    // list too long to scroll; a choice shows that activity's board.
    // So is the group, with its school as a heading where there are several.
    document.querySelectorAll('.class-activity-choice, .class-group-choice').forEach(function (choice) {
        $(choice).on('change', function () { choice.form.submit(); });
        searchable_select(choice);
    });

    // Pin and unpin activities for the group (shown first on students' start page).
    const pins = document.querySelector('.class-pins');
    if (pins) {
        const select = pins.querySelector('.class-pin-select');
        // bound with jQuery: the search field (searchable_select) announces
        // a choice with a jQuery event, which addEventListener does not hear
        if (select) $(select).on('change', function () {
            if (!select.value) return;
            api_call('/api/class/pin', { group: pins.dataset.group, slug: select.value }, function (data) {
                if (data.success) window.location.reload();
            });
        });
        // Typing a word finds the activity (code.js).
        if (select) searchable_select(select);
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
        // Which group, and of which school: on a projector in a room that
        // other classes use too.
        const school = (document.querySelector('.class-login-codes') || {}).dataset?.school || '';
        groupName.textContent = school ? group + ' (' + school + ')' : group;
        groupName.title = groupName.textContent;
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
            // A running Workspace has an account: opening it creates one.
            const account = row.querySelector('.member-account');
            if (running && account && !account.querySelector('.bi-check-lg')) {
                account.innerHTML = "<i class='bi bi-check-lg' aria-hidden='true'></i><span class='visually-hidden'>ja</span>";
            }
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
    // server: one line, a part from 90 % on marked.
    function showServer(data) {
        const line = people.querySelector('.class-server-line');
        if (!line || !data) return;
        const part = (cls, percent, html) => {
            const span = line.querySelector(cls);
            span.innerHTML = html;
            span.classList.toggle('load-full', percent >= 90);
        };
        part('.class-server-cpu', data.cpu_usage, `${Math.round(data.cpu_usage)} % CPU`);
        // The sizes come with their unit as HTML (»19.6<span class='unit'>G</span>«).
        part('.class-server-ram', data.ram_percent, `${data.ram_used} von ${data.ram_total} RAM`);
        part('.class-server-disk', data.disk_user_percent, `${data.disk_user_used} von ${data.disk_user_total} Speicher belegt`);
        line.hidden = false;
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

    // The teacher's own test account back to a fresh start
    people.addEventListener('click', (e) => {
        const reset = e.target.closest('.bu-class-reset-test');
        if (!reset) return;
        if (!confirm('Das Testkonto zurücksetzen? Sein Workspace, seine Datenbanken, Antworten und Abgaben sind danach weg.')) return;
        reset.disabled = true;
        api_call('/api/reset_test_account', {}, function (data) {
            reset.disabled = false;
            if (data.success !== false) {
                const note = reset.parentElement.querySelector('.class-test-reset-note');
                if (note) note.textContent = 'Zurückgesetzt. Beim nächsten Öffnen startet ein frischer Workspace.';
            }
        });
    });

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

// »Neue Gruppe«: a name, then the new group's tab »Gruppe« to add people.
(function () {
    document.querySelectorAll('.class-group-new').forEach((button) => button.addEventListener('click', () => {
        input_dialog({ title: 'Neue Gruppe', label: 'Name der Gruppe, zum Beispiel »10a Informatik«', confirm: 'Anlegen', icon: 'bi-plus-lg' }, function (name) {
            api_call('/api/groups/create', { name: name }, function (r) {
                if (!r.success) { show_error_message(GROUP_ERRORS[r.error] || 'Das hat nicht geklappt.'); return; }
                location.href = '/klasse?group=' + encodeURIComponent(r.created) + '&tab=lernende';
            }, { no_please_wait: true });
            return false;
        });
    }));
})();

// The tab »Lernende«: the group as the card of /gruppen (include/groups.js:
// names, addresses, welcome mails, consent, co-teachers, adding people),
// each row with its Workspace, live: the cells the server rendered into the
// table of Workspaces move into the card, where the websocket keeps
// updating them. A group not managed here (a teacher's test group, groups
// from the invitation files) keeps that table as it is.
(function () {
    const here = document.querySelector('.class-group-manage');
    if (!here) return;
    const name = here.dataset.group;
    const live = new Map();
    // Only once the group is shown as a card: a group the card does not know
    // keeps its table as it is, with CPU and RAM in their own columns.
    const adopt = () => {
        if (live.size) return;
        document.querySelectorAll('.class-people-table tbody tr[data-email]').forEach((tr) => {
            const status = tr.querySelector('.class-ws-status');
            // the table's own columns go with it, so the live updates find the new ones
            tr.querySelectorAll('td.class-ws-cpu, td.class-ws-ram').forEach((td) => td.remove());
            const usage = document.createElement('span');
            usage.className = 'class-ws-usage';
            usage.innerHTML = "<span class='class-ws-cpu'>–</span>, <span class='class-ws-ram'>–</span>";
            status.append(usage);
            status.classList.add('text-nowrap');
            // Square, as the »remove« next to them: the icon only.
            const buttons = [...tr.querySelectorAll('.class-person-actions .btn')];
            buttons.forEach((b) => b.classList.add('btn-icon'));
            live.set(tr.dataset.email, { cell: status, buttons: buttons });
        });
    };
    let shown = false;
    window.member_table_hook = function (table) {
        table.find('thead th').first().after("<th>Workspace</th>");
    };
    window.member_row_hook = function (member, row) {
        // The same elements on every re-render, so live updates keep working.
        const cells = live.get(member.email);
        row.children().first().after(cells ? cells.cell : $("<td><span class='text-body-secondary'>–</span></td>"));
        if (cells) row.find('.member-actions').prepend(cells.buttons);
    };
    window.on_group_renamed = function (old_name, new_name) {
        location.href = '/klasse?group=' + encodeURIComponent(new_name) + '&tab=lernende';
    };
    window.load_groups = function () {
        api_call('/api/groups/list', {}, function (data) {
            if (!data.success) return;
            all_schools = data.schools || [];
            fat_kb = data.fat_kb;
            const group = (data.groups || []).find((g) => g.name === name);
            if (!group) {
                // Deleted just now: back to the class page.
                if (shown) { location.href = '/klasse'; return; }
                // A group the card does not know comes from the invitation
                // files (before the import; in development also the tools'
                // groups): say who keeps it instead of leaving a gap.
                const contact = here.dataset.contact;
                const write = contact
                    ? `Schreib an <a href="mailto:${esc(contact)}">${esc(contact)}</a>, für welche Gruppe, und schick die E-Mail-Adressen mit.`
                    : 'Sag der Administration, für welche Gruppe, und schick die E-Mail-Adressen mit.';
                here.innerHTML = data.admin
                    ? "<p class='class-group-files'><i class='bi bi-file-earmark-text' aria-hidden='true'></i>Die Einladungen dieser Gruppe stehen in Dateien auf dem Server; hier lässt sie sich nicht bearbeiten.</p>"
                    : "<p class='class-group-files'><i class='bi bi-file-earmark-text' aria-hidden='true'></i>Die Einladungen dieser Gruppe stehen noch in Dateien; Namen und Adressen pflegt die Administration. " + write + '</p>';
                return;
            }
            shown = true;
            adopt();
            open_groups.add(group.name);
            $(here).empty().append(render_group(group, data.admin));
            const old = document.querySelector('.class-people-scroll');
            if (old) old.remove();
        }, { no_please_wait: true });
    };
    window.load_groups();
})();

// The class repositories of the group (ClassRepos in main.rb), in the tab
// »Einstellungen«: one field per activity that clones one, in the order of
// the catalog. Saved together; the server checks every address and saves
// nothing if one is not valid, and the field is marked. /klasse?repos=SLUG
// (the link on the activity page) opens the tab (main.rb) at that field.
(function () {
    const box = document.querySelector('.class-repos');
    if (!box) return;
    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    const group = box.dataset.group;
    // The Workspace's own Git server (GitServer in main.rb): a repository
    // can be created there instead of naming one elsewhere.
    const git_server = box.dataset.gitServer === '1';
    const LOCAL = /^git@git\.workspace:([a-z0-9]{10})\/([a-z0-9-]+)\.git$/;

    function entries() {
        try { return JSON.parse(box.dataset.repos); } catch (e) { return []; }
    }

    function save() {
        const repos = {};
        box.querySelectorAll('.class-repo-input').forEach((input) => {
            repos[input.dataset.slug] = input.value.trim();
            input.classList.remove('is-invalid');
            input.removeAttribute('aria-invalid');
        });
        const confirm = box.querySelector('.class-repos-save');
        confirm.disabled = true;
        api_call('/api/set_group_class_repos', { group: group, repos: repos }, function (r) {
            confirm.disabled = false;
            if (!r.success) return;
            if (!r.ok) {
                let first = null;
                Object.keys(r.errors || {}).forEach((slug) => {
                    const input = box.querySelector(`.class-repo-input[data-slug="${CSS.escape(slug)}"]`);
                    if (!input) return;
                    input.classList.add('is-invalid');
                    input.setAttribute('aria-invalid', 'true');
                    first = first || input;
                });
                if (first) first.focus();
                return;
            }
            const list = entries().map((e) => Object.assign(e, { address: (r.repos || {})[e.slug] || null }));
            box.dataset.repos = JSON.stringify(list);
            list.forEach((e) => set_actions(e));
            const count = list.filter((e) => e.address).length;
            show_success_message(`Gespeichert: ${count === 0 ? 'keine Adresse' : count === 1 ? 'eine Adresse' : `${count} Adressen`} für ${esc(group)}.`);
        }, { no_please_wait: true });
    }

    // Beside a field: create the repository in the Workspace, or, for one
    // there, look at it and delete it (a second click confirms).
    function git_actions(e) {
        if (!git_server) return '';
        const m = (e.address || '').match(LOCAL);
        if (m) {
            return `<span class='class-repo-local'><i class='bi bi-hdd-stack' aria-hidden='true'></i>im Workspace</span>`
                + `<a class='btn btn-sm btn-outline-secondary' href='/git/${esc(m[1])}/${esc(m[2])}' target='_blank' rel='noopener'>`
                + `<i class='bi bi-eye' aria-hidden='true'></i>Ansehen</a>`
                + `<button type='button' class='btn btn-sm btn-outline-danger class-repo-delete' data-slug='${esc(e.slug)}'>`
                + `<i class='bi bi-trash' aria-hidden='true'></i>Löschen</button>`;
        }
        return `<button type='button' class='btn btn-sm btn-outline-secondary class-repo-create' data-slug='${esc(e.slug)}'>`
            + `<i class='bi bi-plus-lg' aria-hidden='true'></i>Im Workspace anlegen${e.starter ? ', mit den Dateien der Seite' : ''}</button>`;
    }

    function set_actions(e) {
        const actions = box.querySelector(`.class-repo-git[data-slug="${CSS.escape(e.slug)}"]`);
        if (actions) actions.innerHTML = git_actions(e);
    }

    function set_entry(slug, address) {
        const list = entries().map((e) => e.slug === slug ? Object.assign(e, { address: address }) : e);
        box.dataset.repos = JSON.stringify(list);
        const input = box.querySelector(`.class-repo-input[data-slug="${CSS.escape(slug)}"]`);
        if (input) input.value = address || '';
        set_actions(list.find((e) => e.slug === slug));
    }

    function git_keys_html() {
        if (!git_server) return '';
        const mode = box.dataset.gitKeys || 'handed_in';
        const option = (value, label, hint) => `<div class='form-check'>`
            + `<input class='form-check-input' type='radio' name='class-repos-keys' id='class-repos-keys-${value}' value='${value}'${mode === value ? ' checked' : ''}>`
            + `<label class='form-check-label' for='class-repos-keys-${value}'>${label}<span class='class-repos-keys-hint'>${hint}</span></label></div>`;
        return `<fieldset class='class-repos-keys'><legend class='class-setting-subtitle'>Schlüssel</legend>`
            + option('handed_in', 'Die Lernenden erzeugen ein Schlüsselpaar und geben den öffentlichen Schlüssel ab',
                'wie bei einem Git-Server im Netz; die Seiten zeigen, wie es geht')
            + option('auto', 'Der Workspace richtet den Schlüssel für alle ein',
                'für die Lernenden ist nichts zu tun; er gilt nur für den Git-Server des Workspace')
            + `</fieldset>`;
    }

    function render() {
        let html = `<p class='class-setting-hint'>Lernende der Gruppe sehen die Adresse im Befehl zum Klonen und bekommen die Aufgaben zu Git. `
            + (git_server
                ? `Ein Repository im Workspace legst du hier an; dafür braucht niemand ein Konto. Für einen Git-Server im Netz trägst du seine Adresse ein und lädst die Lernenden dort ein.</p>`
                : `Dafür lädst du sie auf den Git-Server ein.</p>`)
            + git_keys_html()
            + `<h4 class='class-setting-subtitle'>Adressen der Seiten</h4>`
            + `<p class='class-repos-format' id='class-repos-format'>Eine Adresse eines Git-Servers im Netz ist zum Beispiel <code>git@git.example.org:lehrkraft/klasse.git</code>, `
            + `auch <code>ssh://…</code> oder <code>https://…</code>.</p>`;
        entries().forEach((e) => {
            const id = `class-repo-${e.slug}`;
            html += `<div class='class-repo-field'><label class='form-label' for='${esc(id)}'>${esc(e.title)}</label>`
                + `<a class='class-repo-page' href='/${esc(e.slug)}' target='_blank' rel='noopener' aria-label='${esc(e.title)} in neuem Tab öffnen' title='Seite in neuem Tab öffnen'>`
                + `<i class='bi bi-box-arrow-up-right' aria-hidden='true'></i></a>`
                + `<input type='text' class='form-control class-repo-input' id='${esc(id)}' data-slug='${esc(e.slug)}' value='${esc(e.address || '')}' `
                + `placeholder='keine Adresse' aria-describedby='class-repos-format ${esc(id)}-error' autocomplete='off' autocapitalize='off' spellcheck='false'>`
                + (git_server ? `<div class='class-repo-git' data-slug='${esc(e.slug)}'>${git_actions(e)}</div>` : '')
                + `<div class='invalid-feedback' id='${esc(id)}-error'>Keine Adresse, die der Workspace annimmt: <code>git@server:pfad.git</code>, `
                + `<code>ssh://server/pfad.git</code> oder <code>https://server/pfad.git</code>, ohne Leerzeichen.</div></div>`;
        });
        html += `<p class='class-repos-note'>Ein leeres Feld entfernt die Adresse. Ohne Adresse arbeiten die Lernenden nur in ihrem Workspace.</p>`
            + `<button type='button' class='btn btn-primary class-repos-save'><i class='bi bi-check-lg' aria-hidden='true'></i>Adressen speichern</button>`;
        box.innerHTML = html;
    }

    box.addEventListener('keydown', function (e) {
        if (e.key === 'Enter' && e.target.classList.contains('class-repo-input')) { e.preventDefault(); save(); }
    });
    box.addEventListener('change', function (e) {
        if (e.target.name !== 'class-repos-keys') return;
        const mode = e.target.value;
        api_call('/api/git/set_keys', { group: group, mode: mode }, function (r) {
            if (!r.success || !r.ok) return;
            box.dataset.gitKeys = mode;
            show_success_message(mode === 'auto'
                ? `Der Workspace richtet die Schlüssel ein: ${r.installed === 1 ? 'ein Workspace' : `${r.installed} Workspaces`} sofort, die anderen beim nächsten Start.`
                : 'Die Lernenden geben ihre Schlüssel selbst ab.');
        }, { no_please_wait: true });
    });
    box.addEventListener('click', function (e) {
        const target = e.target.closest('button');
        if (!target) return;
        if (target.classList.contains('class-repos-save')) { save(); return; }
        const slug = target.dataset.slug;
        if (target.classList.contains('class-repo-create')) {
            target.disabled = true;
            api_call('/api/git/create_repo', { group: group, slug: slug }, (r) => {
                target.disabled = false;
                if (!r.success || !r.ok) return;
                set_entry(slug, r.address);
                show_success_message(`Angelegt: ${esc(r.address)}`);
            }, { no_please_wait: true });
        } else if (target.classList.contains('class-repo-delete')) {
            if (!target.classList.contains('confirming')) {
                target.classList.add('confirming');
                target.innerHTML = "<i class='bi bi-trash' aria-hidden='true'></i>Wirklich löschen? Alles darin ist weg.";
                return;
            }
            target.disabled = true;
            api_call('/api/git/delete_repo', { group: group, slug: slug }, (r) => {
                target.disabled = false;
                if (!r.success || !r.ok) return;
                set_entry(slug, (r.repos || {})[slug] || null);
                show_success_message('Das Repository ist gelöscht.');
            }, { no_please_wait: true });
        }
    });

    render();
    const wanted = new URLSearchParams(location.search).get('repos');
    if (wanted) {
        // Once the field is shown, the address bar forgets the wish.
        const url = new URL(location.href);
        url.searchParams.delete('repos');
        history.replaceState(null, '', url);
        const input = box.querySelector(`.class-repo-input[data-slug="${CSS.escape(wanted)}"]`);
        if (input) {
            input.scrollIntoView({ block: 'center' });
            input.focus({ preventScroll: true });
        }
    }
})();
