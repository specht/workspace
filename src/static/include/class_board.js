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
