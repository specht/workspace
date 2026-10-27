// The meeting point on the class page (»Fortschritt«, main.rb
// meeting_control_html): choosing a task sets the point after it, »keiner«
// lifts it; open pages of the group show it at once (meeting_point.js). The
// line above the board says who has arrived and is asked again every 20
// seconds while the page is visible.
(function () {
    const select = document.querySelector('.class-meeting-choice');
    const line = document.querySelector('.class-meeting-line');
    if (!select || !line) return;

    select.addEventListener('change', function () {
        select.disabled = true;
        api_call('/api/class/meeting', { group: select.dataset.group, activity: select.dataset.activity, task: select.value }, function (data) {
            // the board draws the line after the new task
            if (data.success) window.location.reload();
            else select.disabled = false;
        });
    });

    function refresh() {
        if (document.hidden || !line.firstElementChild) return;
        api_call('/api/class/meeting_status', { group: line.dataset.group, activity: line.dataset.activity }, function (data) {
            if (!data.success) return;
            const open = !!line.querySelector('details[open]');
            line.innerHTML = data.html;
            const details = line.querySelector('details');
            if (details && open) details.open = true;
        }, { no_please_wait: true });
    }
    setInterval(refresh, 20000);
})();
