// The maintainer's two buttons on /telefon-freigabe (phone_games_api.rb):
// publish, or reject with a reason; »Ablehnen« waits for the reason.
(function () {
    const box = document.querySelector('.pg-decide');
    if (!box) return;
    const reason = box.querySelector('#pg-reason');
    const reject = box.querySelector('.pg-reject');
    const result = box.querySelector('.pg-result');
    reason.addEventListener('input', () => { reject.disabled = reason.value.trim() === ''; });
    function decide(decision) {
        box.querySelectorAll('button').forEach((b) => { b.disabled = true; });
        api_call('/api/phone/decide', { id: box.dataset.id, decision, reason: reason.value }, (data) => {
            if (data.success) {
                result.innerHTML = decision === 'publish'
                    ? "<span class='text-success'><i class='bi bi-check-lg'></i> Veröffentlicht. Die E-Mail an die Schülerin oder den Schüler ist unterwegs.</span>"
                    : "<span class='text-danger'><i class='bi bi-x-lg'></i> Abgelehnt. Die E-Mail mit dem Grund ist unterwegs.</span>";
            } else {
                result.textContent = data.error === 'decided' ? 'Über dieses Spiel ist schon entschieden.' : 'Das hat nicht geklappt.';
                box.querySelectorAll('button').forEach((b) => { b.disabled = false; });
                reject.disabled = reason.value.trim() === '';
            }
        });
    }
    box.querySelector('.pg-publish').addEventListener('click', () => decide('publish'));
    reject.addEventListener('click', () => decide('reject'));
})();
