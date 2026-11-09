// The card »Dein Spiel veröffentlichen« in »Ein Spiel am Telefon«
// (phone_games_api.rb): submit a game from /workspace/telefon with a title,
// a line of description and a free number from 001 to 999; the list of
// one's own games with their state; another number; taking a game back.
(function () {
    const root = document.querySelector('.phone-publish');
    if (!root) return;
    const esc = (t) => String(t == null ? '' : t).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    let state = null;

    function freeNumber() {
        const taken = new Set([...state.taken, ...state.games.filter((g) => g.state !== 'rejected').map((g) => g.number)]);
        const free = [];
        for (let n = 1; n <= 999; n++) {
            const s = String(n).padStart(3, '0');
            if (!taken.has(s)) free.push(s);
        }
        return free.length ? free[Math.floor(Math.random() * free.length)] : '';
    }

    function message(text, kind) {
        root.querySelector('.pp-message').innerHTML = text ? `<div class='alert alert-${kind || 'danger'} py-2 mb-0 mt-2'>${esc(text)}</div>` : '';
    }

    function gameHtml(g) {
        const badge = { pending: 'text-bg-warning', published: 'text-bg-success', rejected: 'text-bg-secondary' }[g.state] || 'text-bg-light';
        const reason = g.reason ? `<div class='pp-reason mt-1'><i class='bi bi-chat-left-text'></i> ${esc(g.reason)}</div>` : '';
        const renumber = g.state !== 'rejected'
            ? `<button class='btn btn-sm btn-outline-secondary pp-renumber' data-id='${esc(g.id)}'><i class='bi bi-123'></i> Nummer ändern</button>` : '';
        return `<li class='list-group-item'>` +
            `<div class='d-flex flex-wrap gap-2 align-items-baseline'><span class='pp-number'>${esc(g.number)}</span>` +
            `<b>${esc(g.title)}</b><span class='badge ${badge}'>${esc(g.stateText)}</span>` +
            `<span class='text-muted small'>${esc(g.file)}</span></div>` +
            (g.description ? `<div class='text-muted small'>${esc(g.description)}</div>` : '') + reason +
            `<div class='d-flex flex-wrap gap-2 mt-2'>${renumber}` +
            `<button class='btn btn-sm btn-outline-danger pp-withdraw' data-id='${esc(g.id)}'><i class='bi bi-x-lg'></i> ${g.state === 'rejected' ? 'Entfernen' : 'Zurückziehen'}</button></div>` +
            `<div class='pp-edit' data-id='${esc(g.id)}'></div></li>`;
    }

    // what the chosen game says about itself (title '…', subtitle '…' in its code)
    function namesHtml() {
        const select = root.querySelector('#pp-file');
        const game = select && state.files.find((f) => f.name === select.value);
        if (!game) return '';
        const missing = [!game.title && 'title', !game.subtitle && 'subtitle'].filter(Boolean);
        if (missing.length) {
            return `<div class='alert alert-warning py-2 mb-0'>In <code>${esc(game.name)}</code> fehlt ` +
                missing.map((m) => `eine Zeile <code>${m} '…'</code>`).join(' und ') + `: Damit gibt das Spiel sich selbst einen Namen und einen Satz fürs Telefonbuch.</div>`;
        }
        return `<div class='pp-names'><b>${esc(game.title)}</b><br><span class='text-muted'>${esc(game.subtitle)}</span></div>`;
    }

    function render() {
        const files = state.files.map((f) => `<option value='${esc(f.name)}'>${esc(f.name)}</option>`).join('');
        const form = state.files.length
            ? `<div class='row g-2'>` +
                `<div class='col-md-5'><label class='form-label' for='pp-file'>Spiel</label><select class='form-select' id='pp-file'>${files}</select></div>` +
                `<div class='col-md-3'><label class='form-label' for='pp-number'>Nummer</label><div class='input-group'>` +
                `<input class='form-control pp-number-input' id='pp-number' maxlength='3' inputmode='numeric' placeholder='123'>` +
                `<button class='btn btn-outline-secondary pp-random' type='button' title='Eine freie Nummer'><i class='bi bi-shuffle'></i></button></div></div>` +
                `<div class='col-12 pp-names-box'></div>` +
                `</div><button class='btn btn-success mt-3 pp-submit'><i class='bi bi-send'></i> Einreichen</button>`
            : `<p class='mb-0'>In <code>/workspace/${esc(state.folder)}</code> liegt noch kein Spiel (eine Datei mit <code>.rb</code>, <code>.py</code> oder <code>.js</code>).</p>`;
        const list = state.games.length
            ? `<h4 class='mt-4'>Deine Spiele</h4><ul class='list-group'>${state.games.map(gameHtml).join('')}</ul>` : '';
        root.innerHTML = `${form}<div class='pp-message'></div>${list}`;
        showNames();
    }

    function showNames() {
        const box = root.querySelector('.pp-names-box');
        if (!box) return;
        box.innerHTML = namesHtml();
        const select = root.querySelector('#pp-file');
        const game = select && state.files.find((f) => f.name === select.value);
        root.querySelector('.pp-submit').disabled = !(game && game.title && game.subtitle);
    }

    function load() {
        if (document.body.dataset.loggedIn !== 'true') {
            root.innerHTML = "<p class='mb-0'><i class='bi bi-box-arrow-in-right'></i> Melde dich an, dann kannst du hier dein Spiel einreichen.</p>";
            return;
        }
        api_call('/api/phone/mine', {}, (data) => {
            if (!data.success) {
                // without a login the server refuses; that is the usual reason
                root.innerHTML = "<p class='mb-0'><i class='bi bi-box-arrow-in-right'></i> Melde dich an, dann kannst du hier dein Spiel einreichen.</p>";
                return;
            }
            state = data;
            render();
        }, { no_please_wait: true });
    }

    function failed(data, fallback) {
        const texts = {
            title: "In deinem Spiel fehlt die Zeile title '…'.",
            subtitle: "In deinem Spiel fehlt die Zeile subtitle '…'.",
            number: 'Die Nummer hat drei Ziffern, 001 bis 999. Die 000 gehört dem Zufall.',
            taken: 'Diese Nummer hat schon ein anderes Spiel. Nimm eine andere.',
        };
        message(texts[data.error] || fallback);
    }

    root.addEventListener('click', (e) => {
        const button = e.target.closest('button');
        if (!button) return;
        if (button.classList.contains('pp-random')) {
            root.querySelector('#pp-number').value = freeNumber();
        } else if (button.classList.contains('pp-submit')) {
            const number = root.querySelector('#pp-number').value.trim();
            if (state.taken.includes(number)) { failed({ error: 'taken' }); return; }
            button.disabled = true;
            message('');
            api_call('/api/phone/submit', { file: root.querySelector('#pp-file').value, number }, (data) => {
                button.disabled = false;
                if (!data.success) { failed(data, 'Das Einreichen hat nicht geklappt. Sieh nach, ob dein Workspace läuft, und versuch es noch einmal.'); return; }
                state = data;
                render();
                message('Eingereicht. Sobald es freigegeben ist, steht es im Telefonbuch, und du bekommst eine E-Mail.', 'success');
            });
        } else if (button.classList.contains('pp-withdraw')) {
            // a second click confirms, instead of a dialog
            if (!button.dataset.sure) {
                button.dataset.sure = '1';
                button.innerHTML = "<i class='bi bi-exclamation-triangle'></i> Wirklich? Noch einmal klicken";
                return;
            }
            api_call('/api/phone/withdraw', { id: button.dataset.id }, (data) => {
                if (data.success) { state = data; render(); }
            });
        } else if (button.classList.contains('pp-renumber')) {
            const edit = root.querySelector(`.pp-edit[data-id='${button.dataset.id}']`);
            edit.innerHTML = `<div class='input-group input-group-sm mt-2' style='max-width: 18rem'>` +
                `<input class='form-control pp-number-input' maxlength='3' inputmode='numeric' aria-label='Neue Nummer' value='${freeNumber()}'>` +
                `<button class='btn btn-outline-success pp-renumber-save' data-id='${esc(button.dataset.id)}'>Übernehmen</button></div>`;
        } else if (button.classList.contains('pp-renumber-save')) {
            const number = button.parentElement.querySelector('input').value.trim();
            api_call('/api/phone/renumber', { id: button.dataset.id, number }, (data) => {
                if (!data.success) { failed(data, 'Die Nummer ließ sich nicht ändern.'); return; }
                state = data;
                render();
            });
        }
    });

    root.addEventListener('change', (e) => {
        if (e.target.id === 'pp-file') showNames();
    });

    root.addEventListener('input', (e) => {
        if (e.target.classList.contains('pp-number-input')) e.target.value = e.target.value.replace(/\D/g, '').slice(0, 3);
    });

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', load);
    else load();
})();
