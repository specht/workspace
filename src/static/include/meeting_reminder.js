// »Offene Treffpunkte« (main.rb, meeting_reminder_html) on the start page
// and the class page: »Aufheben« lifts a point there and then, as on the
// activity page; the list goes once none is left.
(function () {
    document.querySelectorAll('.meeting-reminder-lift').forEach(function (button) {
        button.addEventListener('click', function () {
            api_call('/api/class/meeting', { group: button.dataset.group, activity: button.dataset.activity, task: '' }, function (data) {
                if (!data.success) return;
                const box = button.closest('.meeting-reminder');
                button.closest('li').remove();
                if (box && !box.querySelector('li')) box.remove();
            });
        });
    });
})();
