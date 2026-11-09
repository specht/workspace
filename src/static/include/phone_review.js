// The maintainer's buttons on /telefon-freigabe (phone_games_api.rb): try
// the game on the line, publish, or reject with a reason; »Ablehnen«
// waits for the reason.
(function () {
    const tryBox = document.querySelector('.pg-try');
    if (tryBox) {
        tryBox.querySelector('.pg-try-button').addEventListener('click', (e) => {
            const phone = tryBox.querySelector('.pg-try-phone');
            phone.innerHTML = `<iframe class='phone-book-line' title='Telefon' allow='autoplay; screen-wake-lock' src='${phone.dataset.src}'></iframe>`;
            phone.hidden = false;
            e.currentTarget.hidden = true;
        });
    }
})();

(function () {
    const box = document.querySelector('.pg-decide');
    if (!box) return;
    const reason = box.querySelector('#pg-reason');
    const reject = box.querySelector('.pg-reject');
    const publish = box.querySelector('.pg-publish');
    const result = box.querySelector('.pg-result');
    const status = box.querySelector('.pg-prepared');
    let ready = false, deciding = false, timer = null;

    // published only when the speech service has made every fixed sentence,
    // so the first caller does not wait; asked every few seconds
    function music(p) {
        if (!p.music) return '';
        return p.music_ready === p.music ? ` Die Musik ist geladen (${p.music}).`
            : ` Musik geladen: ${p.music_ready} von ${p.music}; was fehlt, holt der Sprachdienst beim Anruf noch einmal.`;
    }
    function check() {
        api_call('/api/phone/prepared', { id: box.dataset.id }, (data) => {
            const p = data.success ? data.prepared : null;
            if (!p) {
                status.innerHTML = "<i class='bi bi-exclamation-triangle'></i> Der Sprachdienst antwortet gerade nicht; ich frage gleich noch einmal.";
            } else if (data.ready) {
                ready = true;
                status.className = 'pg-prepared text-success';
                status.innerHTML = `<i class='bi bi-check-lg'></i> ${p.total === 1 ? 'Der feste Satz ist' : `Alle ${p.total} festen Sätze sind`} vorbereitet.${music(p)}`;
            } else {
                status.innerHTML = `<span class='spinner-border spinner-border-sm' aria-hidden='true'></span> Sätze vorbereitet: ${p.ready} von ${p.total}. ` +
                    `Veröffentlichen geht, sobald alle fertig sind, damit niemand beim ersten Anruf wartet.${music(p)}`;
            }
            publish.disabled = deciding || !ready;
            if (!ready) timer = setTimeout(check, 4000);
        }, { no_please_wait: true });
    }
    check();
    reason.addEventListener('input', () => { reject.disabled = reason.value.trim() === ''; });
    function decide(decision) {
        deciding = true;
        clearTimeout(timer);
        box.querySelectorAll('button').forEach((b) => { b.disabled = true; });
        api_call('/api/phone/decide', { id: box.dataset.id, decision, reason: reason.value }, (data) => {
            if (data.success) {
                result.innerHTML = decision === 'publish'
                    ? "<span class='text-success'><i class='bi bi-check-lg'></i> Veröffentlicht. Die E-Mail an die Schülerin oder den Schüler ist unterwegs.</span>"
                    : "<span class='text-danger'><i class='bi bi-x-lg'></i> Abgelehnt. Die E-Mail mit dem Grund ist unterwegs.</span>";
            } else {
                result.textContent = data.error === 'decided' ? 'Über dieses Spiel ist schon entschieden.'
                    : data.error === 'not_ready' ? 'Noch sind nicht alle Sätze vorbereitet.' : 'Das hat nicht geklappt.';
                deciding = false;
                reject.disabled = reason.value.trim() === '';
                if (data.error === 'not_ready') { ready = false; check(); } else publish.disabled = !ready;
            }
        });
    }
    box.querySelector('.pg-publish').addEventListener('click', () => decide('publish'));
    reject.addEventListener('click', () => decide('reject'));
})();

