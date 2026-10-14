// Class board (/klasse): »Jetzt in den Workspaces nachsehen« runs a
// Workspace task for every student of the group and marks the live result
// in the table, without changing the students' own results.
(function () {
    // Tabler icons from the bundled sprite (include/tabler/tabler.svg).
    const ICONS = { correct: 'check', wrong: 'x', unavailable: 'help', invalid: 'help' };

    function icon(name) {
        const ns = 'http://www.w3.org/2000/svg';
        const svg = document.createElementNS(ns, 'svg');
        svg.setAttribute('class', 'ti ti-' + name);
        svg.setAttribute('aria-hidden', 'true');
        const use = document.createElementNS(ns, 'use');
        use.setAttribute('href', '/include/tabler/tabler.svg#' + name);
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
    if (template) {
        const select = template.querySelector('.class-template-select');
        const box = template.querySelector('.class-template-results');
        // data-force='yes': ask the template's host now, not the answer
        // remembered for up to an hour.
        template.querySelectorAll('.class-template-check').forEach((button) => button.addEventListener('click', function () {
            const old = button.textContent;
            button.disabled = true;
            button.textContent = old + ' …';
            const request = { group: template.dataset.group, slug: select.value };
            if (button.dataset.force) request.force = button.dataset.force;
            api_call('/api/class/template_status', request, function (data) {
                button.disabled = false;
                button.textContent = old;
                if (!data.success) return;
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
                p.textContent = 'Stand ' + time + ': ' + count('current') + ' von ' + emails.length +
                    ' haben die neueste Version in ' + folder + ', ' + count('outdated') + ' eine alte.';
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
        }));
    }

    document.querySelectorAll('.class-live-check').forEach((button) => {
        button.addEventListener('click', function () {
            const live = button.closest('.class-live');
            const label = button.textContent;
            button.disabled = true;
            const old = button.textContent;
            button.textContent = old + ' …';
            api_call('/api/class/check_now', { group: live.dataset.group, task: button.dataset.task }, function (data) {
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
        const row = people.querySelector('tr[data-email="' + CSS.escape(email) + '"] td');
        return row ? row.textContent : email;
    }

    function showCodes(lines) {
        const box = people.querySelector('.class-login-codes');
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
    }

    function showRunning(stats) {
        people.querySelectorAll('.class-ws-status').forEach((td) => {
            const running = Object.prototype.hasOwnProperty.call(stats || {}, td.dataset.fsTag);
            td.textContent = running ? 'Workspace läuft' : 'Workspace aus';
            td.classList.toggle('class-ws-running', running);
        });
    }

    function connect() {
        const ws = new WebSocket((location.port === '8025' ? 'ws://' : 'wss://') + location.host + '/ws');
        ws.onmessage = (event) => {
            let data;
            try { data = JSON.parse(event.data); } catch (e) { return; }
            if (data.action === 'login_codes') showCodes(data.lines);
            if (data.stats) showRunning(data.stats);
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
                api_call('/api/start_server_as_admin', { email: email, reason: reason }, function (data) {
                    button.disabled = false;
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
