// The card »Dein Spiel veröffentlichen« in »Ein Spiel am Telefon«
// (phone_games_api.rb): a game from /workspace/telefon, named by its own
// title and subtitle, and a free number of three or four digits. The card
// shows the entry as the Telefonbuch will, says while one types whether the
// number is free, and reloads the files (»Neu laden«, and by itself when
// one comes back to the tab, say from VS Code) without losing what was
// chosen. Below, one's own games with their state; another number; taking
// a game back.
(function () {
    const root = document.querySelector('.phone-publish');
    if (!root) return;
    const esc = (t) => String(t == null ? '' : t).replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
    let state = null;
    let chosen = '';            // the file picked, kept across reloads
    let number = '';            // the number typed, kept across reloads
    let loading = false;
    let busy = false;           // submitting: the test run takes a few seconds

    const ownNumbers = () => state.games.filter((g) => g.state !== 'rejected').map((g) => g.number);

    // the letters on the keys of a phone (ITU E.161): DINO is dialled as 3466
    const KEYPAD = { ABC: '2', DEF: '3', GHI: '4', JKL: '5', MNO: '6', PQRS: '7', TUV: '8', WXYZ: '9' };
    const digitsOf = (text) => [...text].map((c) => /\d/.test(c) ? c : (Object.entries(KEYPAD).find(([l]) => l.includes(c)) || [])[1] || '').join('');
    const label = (g) => g.word ? `${g.word} (${g.number})` : g.number;

    function freeNumber() {
        const taken = new Set([...state.taken, ...ownNumbers()]);
        const free = [];
        for (let n = 0; n <= 999; n++) {
            const s = String(n).padStart(3, '0');
            if (!taken.has(s)) free.push(s);
        }
        return free.length ? free[Math.floor(Math.random() * free.length)] : '';
    }

    // { ok, text, kind } for the number typed: digits, or letters as on the keys
    function numberStatus(typed) {
        if (!typed) return { ok: false, text: 'Drei oder vier Ziffern oder Buchstaben, oder würfle eine freie.', kind: 'muted' };
        if (!/^[0-9A-Z]{3,4}$/.test(typed)) return { ok: false, text: 'Eine Nummer hat drei oder vier Ziffern oder Buchstaben.', kind: 'muted' };
        const n = digitsOf(typed);
        const dialled = /[A-Z]/.test(typed) ? ` ${typed} wählt man als ${n}.` : '';
        if (state.taken.includes(n)) return { ok: false, text: `Diese Nummer hat schon ein anderes Spiel.${dialled}`, kind: 'danger' };
        // one's own number: the new version replaces the old one once it is approved
        const own = state.games.find((g) => g.state !== 'rejected' && g.number === n);
        if (own) return { ok: true, text: `Ersetzt »${own.title}« unter dieser Nummer, sobald es freigegeben ist.${dialled}`, kind: 'info' };
        return { ok: true, text: `Die Nummer ist frei.${dialled}`, kind: 'success' };
    }

    const game = () => state.files.find((f) => f.name === chosen);

    // the entry as the Telefonbuch shows it, or what the game still needs
    function previewHtml() {
        const g = game();
        if (!g) return '';
        const missing = [!g.title && 'title', !g.subtitle && 'subtitle'].filter(Boolean);
        if (missing.length) {
            const ruby = g.name.endsWith('.rb');
            const example = (m) => m === 'title' ? 'Name deines Spiels' : 'Ein Satz dazu.';
            const line = (m) => ruby ? `${m} '${example(m)}'` : `${m}('${example(m)}')`;
            return `<div class='alert alert-warning mb-0'><i class='bi bi-exclamation-triangle'></i> ` +
                `In <code>${esc(g.name)}</code> ${missing.length === 2 ? 'fehlen noch zwei Zeilen' : 'fehlt noch eine Zeile'}, ` +
                `mit denen dein Spiel sich einen Namen und einen Satz fürs Telefonbuch gibt. Schreib sie an den Anfang:` +
                `<pre class='mt-2 mb-2'><code>${missing.map((m) => esc(line(m))).join('\n')}</code></pre>` +
                `Speichern, dann <b>Neu laden</b>.</div>`;
        }
        const valid = /^[0-9A-Z]{3,4}$/.test(number);
        const shown = !valid ? '···' : /[A-Z]/.test(number) ? `${esc(number)} <small>${esc(digitsOf(number))}</small>` : esc(number);
        return `<figure class='pp-preview mb-0'>` +
            `<div class='pp-entry'><div class='pp-line'><span class='pp-title'>${esc(g.title)}</span><span class='pp-dots'></span>` +
            `<span class='pp-entry-number'>${shown}</span></div><div class='pp-sub'>${esc(g.subtitle)}</div></div>` +
            `<figcaption>So steht dein Spiel im Telefonbuch.</figcaption></figure>`;
    }

    function gameHtml(g) {
        const badge = { pending: 'text-bg-warning', published: 'text-bg-success', rejected: 'text-bg-secondary' }[g.state] || 'text-bg-light';
        const reason = g.reason ? `<div class='pp-reason mt-1'><i class='bi bi-chat-left-text'></i> ${esc(g.reason)}</div>` : '';
        const renumber = g.state !== 'rejected'
            ? `<button class='btn btn-sm btn-outline-secondary pp-renumber' data-id='${esc(g.id)}'><i class='bi bi-123'></i> Nummer ändern</button>` : '';
        return `<li class='list-group-item'>` +
            `<div class='d-flex flex-wrap gap-2 align-items-baseline'><span class='pp-own-number'>${esc(label(g))}</span>` +
            `<b>${esc(g.title)}</b><span class='badge ${badge}'>${esc(g.stateText)}</span>` +
            `<span class='text-muted small'>${esc(g.file)}</span></div>` +
            (g.description ? `<div class='text-muted small'>${esc(g.description)}</div>` : '') + reason +
            `<div class='d-flex flex-wrap gap-2 mt-2'>${renumber}` +
            `<button class='btn btn-sm btn-outline-danger pp-withdraw' data-id='${esc(g.id)}'><i class='bi bi-x-lg'></i> ${g.state === 'rejected' ? 'Entfernen' : 'Zurückziehen'}</button></div>` +
            `<div class='pp-edit' data-id='${esc(g.id)}'></div></li>`;
    }

    function reloadButtonHtml() {
        return `<button class='btn btn-sm btn-outline-secondary pp-reload' type='button'${loading ? ' disabled' : ''}>` +
            `<i class='bi bi-arrow-clockwise${loading ? ' bi-spin' : ''}'></i> Neu laden</button>`;
    }

    function headHtml() {
        return `<div class='pp-head'><div class='pp-folder'><i class='bi bi-folder2-open'></i> <code>/workspace/${esc(state.folder)}</code></div>` +
            `${reloadButtonHtml()}</div>`;
    }

    function formHtml() {
        if (!state.workspace) {
            return `<p class='mb-0'>Du hast noch keinen Workspace. Öffne ihn einmal, leg den Ordner <code>${esc(state.folder)}</code> an und schreib dein Spiel hinein.</p>`;
        }
        if (!state.files.length) {
            return `<p class='mb-0'>In <code>/workspace/${esc(state.folder)}</code> liegt noch kein Spiel: eine Datei mit <code>.rb</code>, <code>.py</code> oder <code>.js</code>. ` +
                `Hast du gerade eins angelegt, klick auf <b>Neu laden</b>.</p>`;
        }
        const options = state.files.map((f) => `<option value='${esc(f.name)}'${f.name === chosen ? ' selected' : ''}>${esc(f.name)}</option>`).join('');
        return `<div class='pp-fields'>` +
            `<div><label class='form-label' for='pp-file'>Spiel</label><select class='form-select' id='pp-file'>${options}</select></div>` +
            `<div><label class='form-label' for='pp-number'>Nummer</label><div class='input-group pp-number-group'>` +
            `<input class='form-control pp-number-input' id='pp-number' maxlength='4' autocomplete='off' autocapitalize='characters' spellcheck='false' placeholder='123 oder DINO' value='${esc(number)}' aria-describedby='pp-number-status'>` +
            `<button class='btn btn-outline-secondary pp-random' type='button' title='Eine freie Nummer würfeln' aria-label='Eine freie Nummer würfeln'><i class='bi bi-dice-5'></i></button></div>` +
            `<div class='form-text pp-number-status' id='pp-number-status' aria-live='polite'></div></div></div>` +
            `<div class='pp-preview-box'></div>` +
            `<div class='pp-actions'><button class='btn btn-success pp-submit' type='button'></button><span class='form-text pp-why'></span></div>`;
    }

    function render() {
        if (!game() && state.files.length) chosen = state.files[0].name;
        const list = state.games.length
            ? `<h4 class='mt-4'>Deine Spiele</h4><ul class='list-group'>${state.games.map(gameHtml).join('')}</ul>` : '';
        root.innerHTML = `${headHtml()}<div class='pp-form'>${formHtml()}</div><div class='pp-message'></div>${list}`;
        update();
    }

    // what changes while one types: the number's state, the preview, the button
    function update() {
        const box = root.querySelector('.pp-preview-box');
        if (!box) return;
        box.innerHTML = previewHtml();
        const g = game();
        const status = numberStatus(number);
        const line = root.querySelector('.pp-number-status');
        line.className = `form-text pp-number-status text-${{ muted: 'body-secondary', info: 'primary-emphasis' }[status.kind] || status.kind}`;
        line.innerHTML = status.kind === 'success' ? `<i class='bi bi-check-lg'></i> ${esc(status.text)}`
            : status.kind === 'info' ? `<i class='bi bi-arrow-repeat'></i> ${esc(status.text)}`
            : status.kind === 'danger' ? `<i class='bi bi-x-lg'></i> ${esc(status.text)}` : esc(status.text);
        const named = g && g.title && g.subtitle;
        const button = root.querySelector('.pp-submit');
        button.disabled = busy || !named || !status.ok;
        button.innerHTML = busy
            ? "<span class='spinner-border spinner-border-sm' aria-hidden='true'></span> Wird eingereicht …"
            : "<i class='bi bi-send'></i> Einreichen";
        root.querySelector('.pp-why').textContent = busy ? 'Dein Spiel läuft einmal zur Probe.'
            : !named ? 'Erst braucht dein Spiel einen Namen und einen Satz.'
            : !status.ok ? 'Erst braucht es eine freie Nummer.' : '';
    }

    function message(text, kind) {
        const box = root.querySelector('.pp-message');
        if (box) box.innerHTML = text ? `<div class='alert alert-${kind || 'danger'} py-2 mb-0 mt-3'>${esc(text)}</div>` : '';
    }

    function signedOut() {
        root.innerHTML = "<p class='mb-0'><i class='bi bi-box-arrow-in-right'></i> Melde dich an, dann kannst du hier dein Spiel einreichen.</p>";
    }

    function load(quiet) {
        if (document.body.dataset.loggedIn !== 'true') { signedOut(); return; }
        if (loading || busy) return;
        loading = true;
        const button = root.querySelector('.pp-reload');
        if (button) button.outerHTML = reloadButtonHtml();
        api_call('/api/phone/mine', {}, (data) => {
            loading = false;
            if (!data.success) {
                // without a login the server refuses; that is the usual reason
                if (!state) signedOut();
                else if (!quiet) { render(); message('Neu laden hat nicht geklappt. Versuch es gleich noch einmal.'); }
                else render();
                return;
            }
            state = data;
            render();
        }, { no_please_wait: true });
    }

    function failed(data, fallback) {
        const texts = {
            title: 'In deinem Spiel fehlt die Zeile title.',
            subtitle: 'In deinem Spiel fehlt die Zeile subtitle.',
            number: 'Die Nummer hat drei oder vier Ziffern oder Buchstaben.',
            taken: 'Diese Nummer hat schon ein anderes Spiel. Nimm eine andere.',
        };
        message(texts[data.error] || data.message || fallback);
    }

    root.addEventListener('click', (e) => {
        const button = e.target.closest('button');
        if (!button) return;
        if (button.classList.contains('pp-reload')) {
            message('');
            load(false);
        } else if (button.classList.contains('pp-random')) {
            number = freeNumber();
            root.querySelector('#pp-number').value = number;
            update();
        } else if (button.classList.contains('pp-submit')) {
            busy = true;
            message('');
            update();
            api_call('/api/phone/submit', { file: chosen, number }, (data) => {
                busy = false;
                if (!data.success) {
                    update();
                    failed(data, 'Das Einreichen hat nicht geklappt. Sieh nach, ob dein Workspace läuft, und versuch es noch einmal.');
                    return;
                }
                state = data;
                number = '';
                render();
                message('Eingereicht. Sobald es freigegeben ist, steht es im Telefonbuch, und du bekommst eine E-Mail.', 'success');
            }, { no_please_wait: true });
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
                `<input class='form-control pp-number-input' maxlength='4' autocapitalize='characters' spellcheck='false' aria-label='Neue Nummer' value='${freeNumber()}'>` +
                `<button class='btn btn-outline-success pp-renumber-save' data-id='${esc(button.dataset.id)}'>Übernehmen</button></div>`;
        } else if (button.classList.contains('pp-renumber-save')) {
            const n = button.parentElement.querySelector('input').value.trim();
            api_call('/api/phone/renumber', { id: button.dataset.id, number: n }, (data) => {
                if (!data.success) { failed(data, 'Die Nummer ließ sich nicht ändern.'); return; }
                state = data;
                render();
            });
        }
    });

    root.addEventListener('change', (e) => {
        if (e.target.id === 'pp-file') { chosen = e.target.value; update(); }
    });

    root.addEventListener('input', (e) => {
        if (!e.target.classList.contains('pp-number-input')) return;
        e.target.value = e.target.value.toUpperCase().replace(/[^0-9A-Z]/g, '').slice(0, 4);
        if (e.target.id === 'pp-number') { number = e.target.value; update(); }
    });

    // back from VS Code: the files may have changed
    document.addEventListener('visibilitychange', () => {
        if (document.visibilityState === 'visible' && state) load(true);
    });

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', () => load(false));
    else load(false);
})();
