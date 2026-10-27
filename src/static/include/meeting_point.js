// The meeting point on an activity page (docs/plan-treffpunkt.md). The page
// sets window.TASK_PAGE.meeting = { points: { group => task id }, groups,
// teacher, kata, classUrl } (main.rb, meeting_page_config). Of the points
// of one's groups the earliest on the page counts. The box stands after
// the task's card and what reveals its answer, or, when no other task
// follows in that chapter, before the next chapter's heading. Nothing is hidden: the box only says where
// the class meets. A student has arrived when every task up to the point
// that is not an Extra is solved; the box follows the cards (tasks.js sets
// data-status). Set, moved and lifted points arrive over /ws_meeting.
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
        if (config.kata) ways.push("Katas üben: <a href='/" + esc(config.kata.slug) + "'>" + esc(config.kata.title) + '</a>');
        ways.push('jemandem aus deiner Klasse helfen, der noch nicht so weit ist');
        return "<div class='meeting-head'><i class='bi bi-flag-fill' aria-hidden='true'></i><strong>Treffpunkt</strong><span>Du bist da.</span></div>" +
            "<div class='meeting-body'><p>Was danach kommt, besprecht ihr gemeinsam. Bis dahin kannst du</p><ul>" +
            ways.map(function (way) { return '<li>' + way + '</li>'; }).join('') + '</ul></div>';
    }

    function teacherHtml(point) {
        return "<div class='meeting-head'><i class='bi bi-flag' aria-hidden='true'></i><strong>Treffpunkt</strong>" +
            '<span>von ' + esc(point.group) + "</span><a href='" + esc(config.classUrl) + "'>Auf der Klassenseite ändern</a></div>" +
            "<div class='meeting-body meeting-status'></div>";
    }

    // the teacher's count, from the class page's line
    function loadStatus(point) {
        const target = box && box.querySelector('.meeting-status');
        if (!target) return;
        api_call('/api/class/meeting_status', { group: point.group, activity: page.activity }, function (data) {
            if (data.success && box && box.contains(target)) target.innerHTML = data.html;
        }, { no_please_wait: true });
    }

    let box = null;
    let shown = '';
    function render() {
        const point = current();
        const key = point ? point.group + '|' + point.task + '|' + (config.teacher ? 't' : arrived(point)) : '';
        if (key === shown) return;
        shown = key;
        if (box) box.remove();
        box = null;
        if (!point) return;
        box = document.createElement('aside');
        box.className = 'meeting-point' + (!config.teacher && arrived(point) ? ' meeting-arrived' : '');
        box.setAttribute('aria-label', 'Treffpunkt');
        box.innerHTML = config.teacher ? teacherHtml(point) : studentHtml(point);
        const at = place(point);
        if (at.before) at.before.before(box);
        else at.after.after(box);
        if (config.teacher) loadStatus(point);
    }

    // Solved cards move a student past the point
    let pending = null;
    new MutationObserver(function () {
        clearTimeout(pending);
        pending = setTimeout(render, 0);
    }).observe(document.body, { subtree: true, attributes: true, attributeFilter: ['data-status'] });

    if (config.teacher) {
        setInterval(function () {
            const point = current();
            if (point && !document.hidden) loadStatus(point);
        }, 30000);
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

    render();
    connect(1000);
})();
