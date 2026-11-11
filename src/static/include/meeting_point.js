// The meeting point on an activity page (docs/teacher-access.md, »Treffpunkt«). The page
// sets window.TASK_PAGE.meeting = { points: { group => task id }, groups,
// teacher, kata } (main.rb, meeting_page_config). For a student, of the
// points of their groups the earliest on the page counts; a teacher sees
// the point of every group they teach, and chooses the group when setting one. The box stands after
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
        // up to the level of the chapter headings, or of the body of an
        // after block the task sits in (AfterTask): its point stays inside,
        // hidden with the task until the prediction before it is answered
        let node = card;
        while (node.parentElement && node.parentElement !== document.body && !node.parentElement.querySelector(':scope > h2') &&
               !node.parentElement.matches('.after-task-body')) node = node.parentElement;
        if (node.parentElement === document.body) node = card;
        // what gives the task's answer away belongs to it (AfterTask), unless
        // the block holds tasks of its own: then the point goes before it,
        // where everyone has made the prediction (the tasks inside have
        // their own points, at the end of the block)
        while (node.nextElementSibling && node.nextElementSibling.matches('.after-task') &&
               !node.nextElementSibling.querySelector('.task-card')) node = node.nextElementSibling;
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

    // ------------------------------------------------------------------
    // A teacher sees the point of each of their groups that has one on
    // this page, each box with its count and »Aufheben«, and sets, moves
    // and lifts points right on the page: below every task that is not an
    // Extra a small dashed button, faint until pointed at. With several
    // groups the button asks which group the point is for.
    const classUrl = function (group) {
        return '/klasse?group=' + encodeURIComponent(group) + '&tab=fortschritt&activity=' + encodeURIComponent(page.activity);
    };

    function teacherPoints() {
        const ids = order();
        return config.groups.map(function (group) {
            return { group: group, task: points[group], index: ids.indexOf(points[group]) };
        }).filter(function (point) { return point.index >= 0; }).sort(function (a, b) { return a.index - b.index; });
    }

    function teacherHtml(point) {
        return "<div class='meeting-head'><i class='bi bi-flag' aria-hidden='true'></i><strong>Treffpunkt</strong>" +
            '<span>von ' + esc(point.group) + "<span class='meeting-count'></span></span>" +
            "<button type='button' class='btn btn-sm btn-outline-secondary meeting-lift'><i class='bi bi-x-lg' aria-hidden='true'></i>Aufheben</button>" +
            "<a href='" + esc(classUrl(point.group)) + "' target='_blank' rel='noopener'>Auf der Klassenseite</a></div>" +
            "<div class='meeting-body meeting-status'></div>";
    }

    function setPoint(group, task) {
        api_call('/api/class/meeting', { group: group, activity: page.activity, task: task }, function (data) {
            if (!data.success) return;
            if (data.task) points[group] = data.task;
            else delete points[group];
            render();
        });
    }

    const slots = {};
    let openMenu = null;
    function closeMenu() {
        if (!openMenu) return;
        openMenu.menu.hidden = true;
        openMenu.button.setAttribute('aria-expanded', 'false');
        openMenu = null;
    }
    document.addEventListener('click', function (e) { if (openMenu && !openMenu.slot.contains(e.target)) closeMenu(); });
    document.addEventListener('keydown', function (e) {
        if (e.key === 'Escape' && openMenu) { const button = openMenu.button; closeMenu(); button.focus(); }
    });

    function placeSlots() {
        order().forEach(function (id) {
            const mine = cards().filter(function (card) { return idOf(card) === id; });
            if (mine.some(function (card) { return card.querySelector('.task-extra'); })) return;
            const slot = document.createElement('div');
            slot.className = 'meeting-slot';
            slot.innerHTML = "<button type='button' class='meeting-slot-button'><i class='bi bi-flag' aria-hidden='true'></i><span></span></button>" +
                "<div class='meeting-slot-menu' hidden></div>";
            const button = slot.querySelector('.meeting-slot-button');
            const menu = slot.querySelector('.meeting-slot-menu');
            button.addEventListener('click', function () {
                if (config.groups.length === 1) { setPoint(config.groups[0], id); return; }
                const open = openMenu && openMenu.slot === slot;
                closeMenu();
                if (open) return;
                menu.hidden = false;
                button.setAttribute('aria-expanded', 'true');
                openMenu = { slot: slot, menu: menu, button: button };
                const first = menu.querySelector('button:not([disabled])');
                if (first) first.focus();
            });
            menu.addEventListener('click', function (e) {
                const choice = e.target.closest('button[data-group]');
                if (!choice) return;
                closeMenu();
                setPoint(choice.dataset.group, id);
            });
            const at = place({ task: id });
            if (at.before) at.before.before(slot);
            else at.after.after(slot);
            slots[id] = slot;
        });
    }

    // the label of each button, and with several groups the list it opens
    function labelSlots() {
        const several = config.groups.length > 1;
        Object.keys(slots).forEach(function (id) {
            const slot = slots[id];
            const button = slot.querySelector('.meeting-slot-button');
            if (!several) {
                const group = config.groups[0];
                slot.hidden = points[group] === id;
                button.querySelector('span').textContent = 'Treffpunkt für ' + group + (points[group] ? ' hierher verschieben' : ' hier setzen');
                return;
            }
            slot.hidden = config.groups.every(function (group) { return points[group] === id; });
            button.querySelector('span').textContent = 'Treffpunkt hier setzen …';
            button.setAttribute('aria-haspopup', 'true');
            if (!button.hasAttribute('aria-expanded')) button.setAttribute('aria-expanded', 'false');
            slot.querySelector('.meeting-slot-menu').innerHTML = "<p class='meeting-slot-menu-title'>Für welche Gruppe?</p>" +
                config.groups.map(function (group) {
                    const here = points[group] === id;
                    const what = here ? 'ist schon hier' : points[group] ? 'hierher verschieben' : 'hier setzen';
                    return "<button type='button' data-group='" + esc(group) + "'" + (here ? ' disabled' : '') + '><strong>' + esc(group) + '</strong> ' + what + '</button>';
                }).join('');
        });
    }

    // each box's count and who is missing (meeting_status.js, with who is
    // absent today); a box turns green once everyone present is there
    const statuses = {};
    let boxes = {};
    function drawStatus(group) {
        const box = boxes[group];
        const target = box && box.querySelector('.meeting-status');
        if (!target || !statuses[group]) return;
        const s = MeetingStatus.summary(statuses[group], group);
        box.classList.toggle('meeting-all', s.all);
        box.querySelector('.meeting-head .bi').className = 'bi ' + (s.all ? 'bi-check-circle-fill' : 'bi-flag');
        box.querySelector('.meeting-count').textContent = ': ' + MeetingStatus.countText(s) + (s.all ? ', alle da' : '');
        const open = !!target.querySelector('.class-meeting-waiting[open]');
        target.innerHTML = MeetingStatus.detailsHtml(s, open);
        MeetingStatus.bind(target, group, function () { drawStatus(group); });
    }
    const drawAll = function () { Object.keys(boxes).forEach(drawStatus); };

    function loadStatus(group) {
        if (!boxes[group]) return;
        api_call('/api/class/meeting_status', { group: group, activity: page.activity }, function (data) {
            if (!data.success) return;
            statuses[group] = data.status;
            drawStatus(group);
        }, { no_please_wait: true });
    }
    if (config.teacher) MeetingStatus.onChange(drawAll);

    // What comes after the (first) point is the next lesson's: dimmed, readable
    function dimLater() {
        document.querySelectorAll('.meeting-later').forEach(function (el) { el.classList.remove('meeting-later'); });
        if (!box) return;
        // the rest of the chapter, then the chapters after it (each in an
        // .autotoc-container of its own)
        let node = box;
        while (node && node.parentElement) {
            for (let el = node.nextElementSibling; el; el = el.nextElementSibling) {
                if (el.tagName !== 'SCRIPT' && el.tagName !== 'STYLE' && !el.matches('.meeting-point')) el.classList.add('meeting-later');
            }
            if (!node.parentElement.closest('.autotoc-container')) break;
            node = node.parentElement;
        }
    }

    function putBox(point, html, className) {
        const element = document.createElement('aside');
        element.className = className;
        element.setAttribute('aria-label', 'Treffpunkt');
        element.innerHTML = html;
        const at = place(point);
        if (at.before) at.before.before(element);
        else at.after.after(element);
        return element;
    }

    let box = null;
    let shown = '';
    function renderTeacher() {
        labelSlots();
        const list = teacherPoints();
        const key = list.map(function (point) { return point.group + '|' + point.task; }).join(',');
        if (key === shown) return;
        shown = key;
        Object.values(boxes).forEach(function (element) { element.remove(); });
        boxes = {};
        list.forEach(function (point) {
            const element = putBox(point, teacherHtml(point), 'meeting-point');
            element.querySelector('.meeting-lift').addEventListener('click', function () { setPoint(point.group, ''); });
            boxes[point.group] = element;
            loadStatus(point.group);
        });
        box = list.length ? boxes[list[0].group] : null;
        dimLater();
    }

    function render() {
        if (config.teacher) { renderTeacher(); return; }
        const point = current();
        const key = point ? point.group + '|' + point.task + '|' + arrived(point) : '';
        if (key === shown) return;
        shown = key;
        if (box) box.remove();
        box = null;
        if (point) box = putBox(point, studentHtml(point), 'meeting-point' + (arrived(point) ? ' meeting-arrived' : ''));
        dimLater();
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
            if (!document.hidden) Object.keys(boxes).forEach(loadStatus);
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
                if (config.teacher) loadStatus(data.group);
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
