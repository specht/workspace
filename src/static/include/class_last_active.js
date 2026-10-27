// »zuletzt« on the class page: when each student last checked, answered or
// handed in a task, on the board (tab »Fortschritt«) and in the overview
// (tab »Überblick«). The times move on every half minute; every minute the
// board asks the server again (/api/class/last_active, ClassBoard.quiet)
// who is quiet: nothing checked for over ten minutes while the class is at
// work. Whoever the teacher marked »fehlt heute« for the meeting point
// (meeting_status.js) is left out of that line.
window.ClassLastActive = (function () {
    // ClassBoard.ago, the same words
    function ago(t, now) {
        if (!t) return '–';
        const s = Math.max(now - t, 0);
        if (s < 60) return 'gerade eben';
        if (s < 3600) return 'vor ' + Math.floor(s / 60) + ' min';
        if (s < 86400) return 'vor ' + Math.floor(s / 3600) + ' h';
        const days = Math.floor(s / 86400);
        return days === 1 ? 'gestern' : 'vor ' + days + ' Tagen';
    }

    // seconds the server's clock is ahead of this one
    let skew = 0;
    function now() {
        return Math.floor(Date.now() / 1000) + skew;
    }

    function tick() {
        document.querySelectorAll('td.cb-last[data-updated]').forEach(function (td) {
            td.textContent = ago(parseInt(td.dataset.updated, 10) || 0, now());
        });
    }

    function absent(group) {
        return window.MeetingStatus && MeetingStatus.absent ? MeetingStatus.absent(group) : [];
    }

    function apply(board, group, data) {
        skew = data.now - Math.floor(Date.now() / 1000);
        const away = absent(group);
        const names = [];
        board.querySelectorAll('tbody:not(.cb-own) tr[data-email]').forEach(function (tr) {
            const email = tr.dataset.email;
            const td = tr.querySelector('td.cb-last');
            if (td && email in data.last) td.dataset.updated = data.last[email] || 0;
            const quiet = data.quiet.includes(email) && !away.includes(email);
            tr.classList.toggle('cb-quiet', quiet);
            if (quiet) names.push(tr.querySelector('.cb-name').textContent);
        });
        const line = document.querySelector('.cb-quiet-line');
        if (line) {
            line.querySelector('.cb-quiet-names').textContent = names.join(', ');
            line.hidden = names.length === 0;
        }
        tick();
    }

    function start() {
        tick();
        setInterval(tick, 30000);
        const board = document.querySelector('table.class-board');
        const meeting = document.querySelector('.class-meeting-line');
        if (!board || !meeting) return;
        const group = meeting.dataset.group;
        const activity = meeting.dataset.activity;
        let latest = null;
        function refresh() {
            if (document.hidden) return;
            api_call('/api/class/last_active', { group: group, activity: activity }, function (data) {
                if (!data.success) return;
                latest = data;
                apply(board, group, data);
            }, { no_please_wait: true });
        }
        // the server's view at once (clock skew), then every minute
        refresh();
        setInterval(refresh, 60000);
        document.addEventListener('visibilitychange', function () { if (!document.hidden) refresh(); });
        // »fehlt heute« changed on this or another page of this browser
        window.addEventListener('storage', function (event) {
            if (latest && event.key && event.key.startsWith('meeting-absent:')) apply(board, group, latest);
        });
        document.addEventListener('meeting-absent-changed', function () { if (latest) apply(board, group, latest); });
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();

    return { ago: ago };
})();
