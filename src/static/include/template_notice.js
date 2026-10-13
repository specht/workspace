// »Es gibt eine neue Version …«: on an activity page with a template, ask the
// server whether the student's clone in /workspace lacks the template's
// current commit. The server only looks; pulling is up to the student.
// Runs in the background and stays silent on any error.
(function () {
    const config = window.TEMPLATE_PAGE;
    if (!config) return;

    function ask(callback) {
        fetch('/api/template/status', {
            method: 'POST',
            credentials: 'same-origin',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ slug: config.slug }),
        }).then((r) => (r.ok ? r.json() : null)).then((data) => {
            if (data) callback(data);
        }).catch(() => {});
    }

    // Below the title and the short introduction, above the first section.
    function place(box) {
        const anchor = document.querySelector('.container p.abstract') ||
            document.querySelector('.container h1') ||
            document.querySelector('.page-kind-activity');
        if (anchor) anchor.insertAdjacentElement('afterend', box);
    }

    function show(data) {
        if (data.status !== 'outdated' || !data.notice) return;
        const box = document.createElement('div');
        box.className = 'template-notice';
        box.setAttribute('role', 'status');
        const icon = '<svg class="ti ti-refresh" aria-hidden="true"><use href="/include/tabler/tabler.svg#refresh"></use></svg>';
        box.innerHTML = '<div class="template-notice-icon">' + icon + '</div><div class="template-notice-body"></div>';
        const body = box.querySelector('.template-notice-body');
        body.innerHTML = data.notice;
        const again = document.createElement('button');
        again.type = 'button';
        again.className = 'btn btn-sm btn-outline-secondary template-notice-again';
        again.textContent = 'Erledigt? Noch einmal nachsehen';
        again.addEventListener('click', function () {
            again.disabled = true;
            ask(function (next) {
                again.disabled = false;
                if (next.status === 'current') {
                    body.innerHTML = '<p><strong>Passt, du hast jetzt die neueste Version.</strong></p>';
                    box.classList.add('template-notice-done');
                    setTimeout(() => box.remove(), 4000);
                } else if (next.status === 'outdated') {
                    again.textContent = 'Noch nicht neu. Noch einmal nachsehen';
                }
            });
        });
        body.appendChild(again);
        place(box);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', () => ask(show));
    else ask(show);
})();
