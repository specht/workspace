// The projector (/beamer, projector_page.rb): how far the class is in one
// activity. A column per task that is not an Extra (per sheet for a
// series), the last for who is done; each student is a dot in the column of the first task still open
// for them, so the columns grow like bars, in each column the first to get
// there at the bottom (the server's order). Who has not been on the site
// today is left out by the server. Without names until the teacher
// shows them (»Namen zeigen«, remembered in the address as names=1, never
// in the browser). Who is absent today on the class page (meeting_status.js)
// is left out. Asked again when a student of the group solved a task
// (/ws_meeting) and every minute; the dots that moved slide there. Who
// solved Extras is a violet dot with their number inside.
(function () {
    const root = document.querySelector('.projector');
    if (!root) return;
    const group = root.dataset.group;
    const board = root.querySelector('.projector-board');
    const total = root.querySelector('.projector-total');
    const note = root.querySelector('.projector-note');
    const namesButton = root.querySelector('.projector-names');
    const fullButton = root.querySelector('.projector-fullscreen');
    const esc = MeetingStatus.esc;
    const motion = window.matchMedia('(prefers-reduced-motion: reduce)');
    let data = JSON.parse(root.querySelector('.projector-data').textContent);
    let names = new URLSearchParams(location.search).get('names') === '1';

    // a number per student for the DOM, so that no address is in the page
    const ids = new Map();
    function id(email) {
        if (!ids.has(email)) ids.set(email, ids.size + 1);
        return ids.get(email);
    }

    function person(s) {
        const extras = s.extras || 0;
        const cls = 'projector-' + (names ? 'chip' : 'dot') + (s.started ? '' : ' is-new') + (extras ? ' has-extras' : '');
        const title = !s.started ? 'noch nichts geprüft' : extras ? extras + (extras === 1 ? ' Extra' : ' Extras') + ' gelöst' : '';
        // the Extras solved: violet with the stars, as the Extras are everywhere on the site
        const badge = extras ? "<span class='projector-extras'><i class='bi bi-stars' aria-hidden='true'></i>" + extras + '</span>' : '';
        return "<span class='" + cls + "' data-key='" + id(s.email) + "'" + (title ? " title='" + title + "'" : '') + '>' +
            (names ? "<span class='projector-name'>" + esc(s.name) + '</span>' : '') + badge + '</span>';
    }

    function column(label, number, people, extra) {
        const count = people.length;
        return "<div class='projector-column" + (extra || '') + "' role='group' aria-label='" + esc(label) + ': ' +
            count + (count === 1 ? ' Person' : ' Personen') + "'>" +
            "<span class='projector-count'" + (count ? '' : ' hidden') + '>' + count + '</span>' +
            "<div class='projector-stack'>" + people.map(person).join('') + '</div>' +
            "<div class='projector-label'>" + number + '</div></div>';
    }

    // where every dot is now, to let it slide from there after drawing
    function places() {
        const rects = new Map();
        board.querySelectorAll('[data-key]').forEach(function (el) { rects.set(el.dataset.key, el.getBoundingClientRect()); });
        return rects;
    }

    function slide(before) {
        if (motion.matches) return;
        board.querySelectorAll('[data-key]').forEach(function (el) {
            const old = before.get(el.dataset.key);
            if (!old) return;
            const now = el.getBoundingClientRect();
            const dx = old.left - now.left;
            const dy = old.top - now.top;
            if (Math.abs(dx) < 1 && Math.abs(dy) < 1) return;
            el.animate([{ transform: 'translate(' + dx + 'px, ' + dy + 'px)' }, { transform: 'none' }],
                { duration: 700, easing: 'cubic-bezier(0.2, 0.7, 0.2, 1)' });
            // one that went on to another column glows a moment
            if (Math.abs(dx) > old.width) el.animate([{ boxShadow: '0 0 0 0.4rem var(--projector-glow)' }, { boxShadow: '0 0 0 0 transparent' }],
                { duration: 1600, delay: 500, easing: 'ease-out' });
        });
    }

    function draw(animate) {
        const away = new Set(MeetingStatus.absent(group));
        const present = data.students.filter(function (s) { return !away.has(s.email); });
        const at = new Map(data.tasks.map(function (t, c) { return [t.index, c]; }));
        const columns = data.tasks.map(function () { return []; });
        const done = [];
        present.forEach(function (s) { (s.at === null ? done : columns[at.get(s.at)]).push(s); });

        const before = animate ? places() : new Map();
        let html = '';
        data.tasks.forEach(function (t, c) {
            const meeting = t.index === data.meeting ? ' is-meeting' : '';
            html += column(data.unit + ' ' + (t.index + 1) + ', ' + t.title + (meeting ? ', danach Treffpunkt' : ''),
                "<span class='projector-number'>" + (t.index + 1) + "</span><span class='projector-task'>" + esc(t.title) + '</span>' +
                (meeting ? "<span class='projector-flag'><i class='bi bi-flag-fill' aria-hidden='true'></i>Treffpunkt</span>" : ''),
                columns[c], meeting);
        });
        html += column('Fertig', "<span class='projector-number'><i class='bi bi-check-lg' aria-hidden='true'></i></span><span class='projector-task'>Fertig</span>", done, ' is-done');
        board.style.setProperty('--columns', data.tasks.length + 1);
        board.classList.toggle('with-names', names);
        board.innerHTML = html;
        fit();
        slide(before);

        const n = present.length;
        const parts = ["<span><strong>" + done.length + ' von ' + n + '</strong> fertig</span>'];
        if (data.meeting !== null) {
            const there = present.filter(function (s) { return s.at === null || s.at > data.meeting; }).length;
            parts.push("<span><i class='bi bi-flag-fill' aria-hidden='true'></i>Treffpunkt nach Aufgabe " + (data.meeting + 1) +
                ': <strong>' + there + ' von ' + n + '</strong> da</span>');
        }
        // not on the site today (left out by the server), and marked absent on the class page
        const stars = present.filter(function (s) { return s.extras; }).length;
        if (stars) parts.push("<span class='projector-total-extras'><i class='bi bi-stars' aria-hidden='true'></i>" + stars +
            ' mit Extras</span>');
        const gone = data.absent + data.students.length - n;
        if (gone) parts.push('<span>' + gone + ' heute nicht da</span>');
        total.innerHTML = parts.join('');

        note.textContent = (data.unit === 'Blatt' ? 'Jede Person steht beim ersten Blatt, das sie noch nicht fertig hat (alle Aufgaben außer den Extras)' :
            'Jede Person steht bei der ersten Aufgabe, die sie noch nicht gelöst hat') +
            (names ? '; gestrichelt: noch nichts geprüft.' : '; ein leerer Kreis: noch nichts geprüft.') +
            (data.extras ? ' Die Extras (' + data.extras + ') zählen nicht mit: Wer fertig ist, kann daran weitermachen. ' +
                (names ? 'Die violette Zahl sagt, wie viele Extras jemand gelöst hat.' : 'Ein violetter Punkt hat Extras gelöst, die Zahl darin sagt, wie viele.') : '');
    }

    // the board as tall as the screen leaves below the site's header and
    // above its footer (the whole screen in Vollbild), the dots as big as
    // the fullest column allows: big for a small class, read from the back
    function fit() {
        if (document.fullscreenElement !== root) {
            const footer = document.querySelector('footer');
            const top = root.getBoundingClientRect().top + window.scrollY;
            // the footer may still be empty while the page loads
            const below = Math.max(footer ? footer.offsetHeight : 0, 40);
            root.style.setProperty('--projector-height', Math.max(320, window.innerHeight - top - below) + 'px');
        }
        if (names) return;
        const columns = Array.from(board.querySelectorAll('.projector-column'));
        if (!columns.length) return;
        const most = Math.max(1, ...columns.map(function (c) { return c.querySelectorAll('.projector-dot').length; }));
        const c = columns[0];
        const style = getComputedStyle(c);
        const width = c.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);
        const space = c.clientHeight - c.querySelector('.projector-label').offsetHeight - c.querySelector('.projector-count').offsetHeight * 1.5 - 24;
        const gap = 5;
        let size = 56;
        for (; size > 12; size--) {
            const row = Math.max(1, Math.floor((width + gap) / (size + gap)));
            if (Math.ceil(most / row) * (size + gap) <= space) break;
        }
        board.style.setProperty('--dot-size', size + 'px');
        board.classList.toggle('dots-big', size >= 40);
        board.classList.toggle('dots-small', size < 20);
    }
    window.addEventListener('resize', fit);
    window.addEventListener('load', fit);
    document.addEventListener('fullscreenchange', fit);

    function showNames(on) {
        names = on;
        namesButton.setAttribute('aria-pressed', on ? 'true' : 'false');
        namesButton.innerHTML = "<i class='bi " + (on ? 'bi-person-fill' : 'bi-person') + "' aria-hidden='true'></i>" + (on ? 'Namen ausblenden' : 'Namen zeigen');
        const url = new URL(location.href);
        if (on) url.searchParams.set('names', '1');
        else url.searchParams.delete('names');
        history.replaceState(null, '', url);
        draw(false);
    }
    namesButton.addEventListener('click', function () { showNames(!names); });
    showNames(names);

    if (document.fullscreenEnabled) {
        fullButton.addEventListener('click', function () {
            if (document.fullscreenElement) document.exitFullscreen();
            else root.requestFullscreen().catch(function () { /* refused: the page stays as it is */ });
        });
        document.addEventListener('fullscreenchange', function () {
            const on = document.fullscreenElement === root;
            fullButton.innerHTML = "<i class='bi " + (on ? 'bi-fullscreen-exit' : 'bi-arrows-fullscreen') + "' aria-hidden='true'></i>" +
                (on ? 'Vollbild beenden' : 'Vollbild');
        });
    } else {
        fullButton.hidden = true;
    }

    // absent today marked on the class page in another tab
    MeetingStatus.onChange(function () { draw(false); });

    // several solved at once: ask once
    let waiting = null;
    function refresh() {
        clearTimeout(waiting);
        waiting = setTimeout(function () {
            api_call('/api/class/projector', { group: group, activity: root.dataset.activity }, function (reply) {
                if (!reply.success) return;
                data = reply.data;
                draw(true);
            }, { no_please_wait: true, quiet: true });
        }, 800);
    }
    setInterval(function () { if (!document.hidden) refresh(); }, 60000);

    function connect(delay) {
        const ws = new WebSocket((location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host +
            '/ws_meeting?activity=' + encodeURIComponent(data.watch.join(',')));
        let ping = null;
        ws.onopen = function () {
            // what happened while the connection was gone
            if (delay > 1000) refresh();
            delay = 1000;
            ping = setInterval(function () { try { ws.send('{"ping":true}'); } catch (e) { /* closing */ } }, 30000);
        };
        ws.onmessage = function (event) {
            let message = null;
            try { message = JSON.parse(event.data); } catch (e) { return; }
            if (message.group !== group) return;
            if (message.action === 'meeting' || message.action === 'progress') refresh();
        };
        ws.onclose = function () {
            clearInterval(ping);
            setTimeout(function () { connect(Math.min(delay * 2, 60000)); }, delay);
        };
    }
    connect(1000);
})();
