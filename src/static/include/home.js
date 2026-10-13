// Start page (StudentHome): hide an activity from »Weitermachen« until the
// student works on it again, or show all hidden ones again.
(function () {
    document.querySelectorAll('.student-home-card .shc-hide').forEach((button) => {
        button.addEventListener('click', function () {
            const card = button.closest('.student-home-card');
            api_call('/api/home/hide', { slug: card.dataset.slug }, function (data) {
                if (data.success) window.location.reload();
            }, { no_please_wait: true });
        });
    });
    document.querySelectorAll('.student-home-unhide').forEach((button) => {
        button.addEventListener('click', function () {
            api_call('/api/home/unhide_all', {}, function (data) {
                if (data.success) window.location.reload();
            }, { no_please_wait: true });
        });
    });
})();
