// The meeting point on an activity page (docs/plan-treffpunkt.md). The page
// sets window.TASK_PAGE.meeting = { points: { group => task id }, groups,
// teacher, kata, classUrl } (main.rb, meeting_page_config). Of the points
// of one's groups the earliest on the page counts. The box stands after
// the task's card and what reveals its answer, or, when no other task
// follows in that chapter, before the next chapter's heading. Nothing is hidden: the box only says where
// the class meets. A student has arrived when every task up to the point
// that is not an Extra is solved; the box follows the cards (tasks.js sets
// data-status). Set, moved and lifted points arrive over /ws_meeting, and
// for a teacher a note whenever a student of the group solved a task.
(function () {
    const page = window.TASK_PAGE || {};
    const config = page.meeting;
    if (!config) return;
    const points = Object.assign({}, config.points || {});
    const SOLVED = ['correct', 'sent'];

    function esc(text) {
        const div = document.createElement('div');
        div.textContent = String(text);
        return div.innerHTML;
    }

    function cards() {
        return Array.from(document.querySelectorAll('.task-card'));
    }

    function idOf(card) {
        return card.dataset.task.split('/')[1];
    }

    // The tasks in page order, each once (a task can have a card per language)
    function order() {
        const ids = [];
        cards().forEach(function (card) {
            if (!ids.includes(idOf(card))) ids.push(idOf(card));
        });
        return ids;
    }

    function current() {
        const ids = order();
        let best = null;
        config.groups.forEach(function (group) {
            const index = ids.indexOf(points[group]);
            if (index >= 0 && (!best || index < best.index)) best = { group: group, task: points[group], index: index };
        });
        return best;
    }

    function arrived(point) {
        return order().slice(0, point.index + 1).every(function (id) {
            const mine = cards().filter(function (card) { return idOf(card) === id; });
            return mine.some(function (card) { return card.querySelector('.task-extra'); }) ||
                mine.some(function (card) { return SOLVED.includes(card.dataset.status); });
        });
    }

    function place(point) {
        const mine = cards().filter(function (card) { return idOf(card) === point.task; });
        const card = mine[mine.length - 1];
        // up to the level of the chapter headings
        let node = card;
        while (node.parentElement && node.parentElement !== document.body && !node.parentElement.querySelector(':scope > h2')) node = node.parentElement;
        if (node.parentElement === document.body) node = card;
        // what gives the task's answer away belongs to it (AfterTask)
        while (node.nextElementSibling && node.nextElementSibling.matches('.after-task')) node = node.nextElementSibling;
        let next = node.nextElementSibling;
        while (next && next.tagName !== 'H2') {
            if (next.matches('.task-card') || next.querySelector('.task-card')) return { after: node };
            next = next.nextElementSibling;
        }
        return next ? { before: next } : { after: node };
    }

    function studentHtml(point) {
        if (!arrived(point)) {
            return "<div class='meeting-head'><i class='bi bi-flag' aria-hidden='true'></i><strong>Treffpunkt</strong>" +
                '<span>Hier trifft sich deine Klasse, bevor es weitergeht.</span></div>';
        }
        const ways = [];
        const extras = cards().filter(function (card) {
            return card.querySelector('.task-extra') && !SOLVED.includes(card.dataset.status) && card.offsetParent !== null;
        });
        if (extras.length > 0) {
            ways.push('eine Extra-Aufgabe auf dieser Seite lösen: ' + extras.map(function (card) {
                return "<a href='#" + esc(card.id) + "'>" + esc(card.querySelector('.task-title').textContent) + '</a>';
            }).join(', '));
        }
        if (config.kata) ways.push("Katas üben: <a href='/" + esc(config.kata.slug) + "' target='_blank' rel='noopener'>" + esc(config.kata.title) + '</a>');
        ways.push('jemandem aus deiner Klasse helfen, der noch nicht so weit ist');
        return "<div class='meeting-head'><i class='bi bi-flag-fill' aria-hidden='true'></i><strong>Treffpunkt</strong><span>Du bist da.</span></div>" +
            "<div class='meeting-body'><p>Was danach kommt, besprecht ihr gemeinsam. Bis dahin kannst du</p><ul>" +
            ways.map(function (way) { return '<li>' + way + '</li>'; }).join('') + '</ul></div>';
    }

    function teacherHtml(point) {
        return "<div class='meeting-head'><i class='bi bi-flag' aria-hidden='true'></i><strong>Treffpunkt</strong>" +
            '<span>von ' + esc(point.group) + "<span class='meeting-count'></span></span>" +
            "<button type='button' class='btn btn-sm btn-outline-secondary meeting-lift'><i class='bi bi-x-lg' aria-hidden='true'></i>Aufheben</button>" +
            "<a href='" + esc(config.classUrl) + "' target='_blank' rel='noopener'>Auf der Klassenseite</a></div>" +
            "<div class='meeting-body meeting-status'></div>";
    }

    // A teacher sets, moves and lifts the point right on the page: below
    // every task that is not an Extra a small dashed button, faint until
    // pointed at; for the group chosen on the class page.
    function setPoint(task) {
        const group = config.groups[0];
        api_call('/api/class/meeting', { group: group, activity: page.activity, task: task }, function (data) {
            if (!data.success) return;
            if (data.task) points[group] = data.task;
            else delete points[group];
            render();
        });
    }

    const slots = {};
    function placeSlots() {
        order().forEach(function (id) {
            const mine = cards().filter(function (card) { return idOf(card) === id; });
            if (mine.some(function (card) { return card.querySelector('.task-extra'); })) return;
            const slot = document.createElement('div');
            slot.className = 'meeting-slot';
            slot.innerHTML = "<button type='button' class='meeting-slot-button'><i class='bi bi-flag' aria-hidden='true'></i><span></span></button>";
            slot.querySelector('button').addEventListener('click', function () { setPoint(id); });
            const at = place({ task: id });
            if (at.before) at.before.before(slot);
            else at.after.after(slot);
            slots[id] = slot;
        });
    }

    function labelSlots(point) {
        Object.keys(slots).forEach(function (id) {
            const slot = slots[id];
            slot.hidden = !!point && point.task === id;
            slot.querySelector('span').textContent = 'Treffpunkt für ' + config.groups[0] + (point ? ' hierher verschieben' : ' hier setzen');
        });
    }

    // the teacher's count and who is missing (meeting_status.js, with who
    // is absent today); the box turns green once everyone present is there
    let status = null;
    function drawStatus() {
        const target = box && box.querySelector('.meeting-status');
        if (!target || !status) return;
        const s = MeetingStatus.summary(status, config.groups[0]);
        box.classList.toggle('meeting-all', s.all);
        box.querySelector('.meeting-head .bi').className = 'bi ' + (s.all ? 'bi-check-circle-fill' : 'bi-flag');
        box.querySelector('.meeting-count').textContent = ': ' + MeetingStatus.countText(s) + (s.all ? ', alle da' : '');
        const open = !!target.querySelector('.class-meeting-waiting[open]');
        target.innerHTML = MeetingStatus.detailsHtml(s, open);
        MeetingStatus.bind(target, config.groups[0], drawStatus);
    }

    function loadStatus(point) {
        if (!box || !box.querySelector('.meeting-status')) return;
        api_call('/api/class/meeting_status', { group: point.group, activity: page.activity }, function (data) {
            if (!data.success) return;
            status = data.status;
            drawStatus();
        }, { no_please_wait: true });
    }
    if (config.teacher) MeetingStatus.onChange(drawStatus);

    // What comes after the point is the next lesson's: dimmed, readable
    function dimLater() {
        document.querySelectorAll('.meeting-later').forEach(function (el) { el.classList.remove('meeting-later'); });
        if (!box) return;
        // the rest of the chapter, then the chapters after it (each in an
        // .autotoc-container of its own)
        let node = box;
        while (node && node.parentElement) {
            for (let el = node.nextElementSibling; el; el = el.nextElementSibling) {
                if (el.tagName !== 'SCRIPT' && el.tagName !== 'STYLE') el.classList.add('meeting-later');
            }
            if (!node.parentElement.closest('.autotoc-container')) break;
            node = node.parentElement;
        }
    }

    let box = null;
    let shown = '';
    function render() {
        const point = current();
        if (config.teacher) labelSlots(point);
        const key = point ? point.group + '|' + point.task + '|' + (config.teacher ? 't' : arrived(point)) : '';
        if (key === shown) return;
        shown = key;
        if (box) box.remove();
        box = null;
        if (!point) {
            dimLater();
            return;
        }
        box = document.createElement('aside');
        box.className = 'meeting-point' + (!config.teacher && arrived(point) ? ' meeting-arrived' : '');
        box.setAttribute('aria-label', 'Treffpunkt');
        box.innerHTML = config.teacher ? teacherHtml(point) : studentHtml(point);
        const at = place(point);
        if (at.before) at.before.before(box);
        else at.after.after(box);
        dimLater();
        if (config.teacher) {
            box.querySelector('.meeting-lift').addEventListener('click', function () { setPoint(''); });
            loadStatus(point);
        }
    }

    // Solved cards move a student past the point
    let pending = null;
    new MutationObserver(function () {
        clearTimeout(pending);
        pending = setTimeout(render, 0);
    }).observe(document.body, { subtree: true, attributes: true, attributeFilter: ['data-status'] });

    // the count also follows the class without a message (a lost one)
    if (config.teacher) {
        setInterval(function () {
            const point = current();
            if (point && !document.hidden) loadStatus(point);
        }, 60000);
    }

    function connect(delay) {
        const ws = new WebSocket((location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host +
            '/ws_meeting?activity=' + encodeURIComponent(page.activity));
        let ping = null;
        ws.onopen = function () {
            delay = 1000;
            ping = setInterval(function () { try { ws.send('{"ping":true}'); } catch (e) { /* closing */ } }, 30000);
        };
        ws.onmessage = function (event) {
            let data = null;
            try { data = JSON.parse(event.data); } catch (e) { return; }
            if (data.action === 'points') {
                // after a reconnect: what is set now
                Object.keys(points).forEach(function (group) { delete points[group]; });
                Object.assign(points, data.points || {});
            } else if (data.action === 'meeting') {
                if (data.task) points[data.group] = data.task;
                else delete points[data.group];
            } else if (data.action === 'progress') {
                // a student of the group solved a task: count again
                const point = current();
                if (config.teacher && point && point.group === data.group) loadStatus(point);
                return;
            } else {
                return;
            }
            render();
        };
        ws.onclose = function () {
            clearInterval(ping);
            setTimeout(function () { connect(Math.min(delay * 2, 60000)); }, delay);
        };
    }

    if (config.teacher) placeSlots();
    render();
    connect(1000);
})();
