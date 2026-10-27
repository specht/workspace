// The meeting point on the class page (»Fortschritt«, main.rb
// meeting_control_html): choosing a task sets the point after it, »keiner«
// lifts it; open pages of the group show it at once (meeting_point.js). The
// line above the board says who has arrived (meeting_status.js, with who is
// absent today): asked again when a student of the group solved a task
// (/ws_meeting), and every minute anyway. Green once everyone is there.
(function () {
    const select = document.querySelector('.class-meeting-choice');
    const line = document.querySelector('.class-meeting-line');
    if (!select || !line) return;
    const group = line.dataset.group;

    select.addEventListener('change', function () {
        select.disabled = true;
        api_call('/api/class/meeting', { group: group, activity: select.dataset.activity, task: select.value }, function (data) {
            // the board draws the line after the new task
            if (data.success) window.location.reload();
            else select.disabled = false;
        });
    });

    let status = null;
    function draw() {
        if (!status) {
            line.innerHTML = '';
            return;
        }
        const open = !!line.querySelector('.class-meeting-waiting[open]');
        const s = MeetingStatus.summary(status, group);
        line.innerHTML = "<div class='class-meeting-status" + (s.all ? ' meeting-all' : '') + "' data-task='" + MeetingStatus.esc(status.task) + "'>" +
            "<i class='bi " + (s.all ? 'bi-check-circle-fill' : 'bi-flag') + "' aria-hidden='true'></i>" +
            '<span>Treffpunkt nach Aufgabe ' + (status.index + 1) + ' (' + MeetingStatus.esc(status.title) + '): ' +
            "<strong class='class-meeting-count'>" + MeetingStatus.countText(s) + '</strong>' + (s.all ? ', alle da' : '') + '</span>' +
            MeetingStatus.detailsHtml(s, open) + '</div>';
        MeetingStatus.bind(line, group, draw);
    }
    MeetingStatus.onChange(draw);

    function refresh() {
        if (document.hidden) return;
        api_call('/api/class/meeting_status', { group: group, activity: line.dataset.activity }, function (data) {
            if (!data.success) return;
            status = data.status;
            draw();
        }, { no_please_wait: true });
    }
    refresh();
    setInterval(refresh, 60000);

    function connect(delay) {
        const ws = new WebSocket((location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host +
            '/ws_meeting?activity=' + encodeURIComponent(line.dataset.activity));
        let ping = null;
        ws.onopen = function () {
            delay = 1000;
            ping = setInterval(function () { try { ws.send('{"ping":true}'); } catch (e) { /* closing */ } }, 30000);
        };
        ws.onmessage = function (event) {
            let data = null;
            try { data = JSON.parse(event.data); } catch (e) { return; }
            if (data.group !== group) return;
            // set or lifted elsewhere (on the activity page): the choice follows
            if (data.action === 'meeting') select.value = data.task || '';
            if (data.action === 'meeting' || data.action === 'progress') refresh();
        };
        ws.onclose = function () {
            clearInterval(ping);
            setTimeout(function () { connect(Math.min(delay * 2, 60000)); }, delay);
        };
    }
    connect(1000);
})();
