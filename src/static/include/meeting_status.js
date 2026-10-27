// Who has arrived at a group's meeting point, for teachers: the line above
// the class board (class_meeting.js) and the box on the activity page
// (meeting_point.js). status comes from /api/class/meeting_status
// (MeetingPoint.status). A teacher can leave out who is absent today; only
// this browser remembers it, until midnight, and every open page of the
// teacher counts with it. Once everyone present has arrived, »alle da«.
window.MeetingStatus = (function () {
    function esc(text) {
        const div = document.createElement('div');
        div.textContent = String(text);
        return div.innerHTML;
    }

    function key(group) {
        const d = new Date();
        return 'meeting-absent:' + group + ':' + d.getFullYear() + '-' + (d.getMonth() + 1) + '-' + d.getDate();
    }

    function absent(group) {
        try {
            const list = JSON.parse(localStorage.getItem(key(group)) || '[]');
            return Array.isArray(list) ? list : [];
        } catch (e) {
            return [];
        }
    }

    function setAbsent(group, email, away) {
        const list = absent(group).filter(function (e) { return e !== email; });
        if (away) list.push(email);
        try {
            localStorage.setItem(key(group), JSON.stringify(list));
        } catch (e) { /* private window: only this page forgets it */ }
    }

    // { arrived, waiting, away, total, all }: today's absent left out
    function summary(status, group) {
        const gone = absent(group);
        const waiting = status.waiting.filter(function (p) { return !gone.includes(p.email); });
        const away = status.waiting.filter(function (p) { return gone.includes(p.email); });
        const total = status.arrived.length + waiting.length;
        return { arrived: status.arrived, waiting: waiting, away: away, total: total, all: total > 0 && waiting.length === 0 };
    }

    function countText(s) {
        return s.arrived.length + ' von ' + s.total + ' da';
    }

    function person(p, away) {
        return "<li><span>" + esc(p.name) + "</span><button type='button' class='btn btn-link btn-sm meeting-away' data-email='" + esc(p.email) + "'" +
            (away ? " data-away='0' title='Zählt wieder mit'><i class='bi bi-arrow-counterclockwise' aria-hidden='true'></i>doch da"
                : " data-away='1' title='Zählt heute nicht mit; nur dieser Browser merkt es sich, bis Mitternacht'><i class='bi bi-person-dash' aria-hidden='true'></i>fehlt heute") +
            '</button></li>';
    }

    // who is missing, and who is absent today, each with a button
    function detailsHtml(s, open) {
        let html = '';
        if (s.waiting.length > 0) {
            html += "<details class='class-meeting-waiting'" + (open ? ' open' : '') + '><summary>Noch nicht da: ' + s.waiting.length + '</summary><ul>' +
                s.waiting.map(function (p) { return person(p, false); }).join('') + '</ul></details>';
        }
        if (s.away.length > 0) {
            html += "<details class='class-meeting-waiting'><summary>Fehlen heute: " + s.away.length + '</summary><ul>' +
                s.away.map(function (p) { return person(p, true); }).join('') + '</ul></details>';
        }
        return html;
    }

    // the buttons of detailsHtml; changed() draws again
    function bind(container, group, changed) {
        container.querySelectorAll('.meeting-away').forEach(function (button) {
            button.addEventListener('click', function () {
                setAbsent(group, button.dataset.email, button.dataset.away === '1');
                changed();
            });
        });
    }

    // the other pages of this browser follow
    function onChange(callback) {
        window.addEventListener('storage', function (event) {
            if (event.key && event.key.startsWith('meeting-absent:')) callback();
        });
    }

    return { summary: summary, countText: countText, detailsHtml: detailsHtml, bind: bind, onChange: onChange, esc: esc };
})();
