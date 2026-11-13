// The Telefonbuch's management (/telefon-freigabe, phone_games_list_html):
// title and subtitle of each published game, saved with »Speichern« once
// changed; »im Telefonbuch« shows a game in the book and the phone's menu
// or hides it (a part of another game, still reached by its number).
(function () {
    const rows = document.querySelectorAll('.phone-manage-row');

    function state(row, text, ok) {
        const box = row.querySelector('.phone-manage-state');
        box.textContent = text;
        box.className = 'phone-manage-state small ' + (ok ? 'text-success' : 'text-danger');
    }

    rows.forEach(function (row) {
        const id = row.dataset.id;
        const title = row.querySelector('.phone-manage-title');
        const description = row.querySelector('.phone-manage-description');
        const save = row.querySelector('.phone-manage-save');
        const shown = row.querySelector('.phone-manage-shown');
        let saved = [title.value, description.value];

        const changed = function () {
            save.disabled = title.value === saved[0] && description.value === saved[1];
            if (!save.disabled) state(row, '', true);
        };
        title.addEventListener('input', changed);
        description.addEventListener('input', changed);

        row.addEventListener('submit', function (e) {
            e.preventDefault();
            if (!title.value.trim() || !description.value.trim()) {
                state(row, 'Titel und Untertitel dürfen nicht leer sein.', false);
                return;
            }
            save.disabled = true;
            api_call('/api/phone/rename', { id: id, title: title.value, description: description.value }, function (data) {
                if (!data.success) {
                    save.disabled = false;
                    state(row, 'Nicht gespeichert. Versuch es noch einmal.', false);
                    return;
                }
                title.value = data.title;
                description.value = data.description;
                saved = [data.title, data.description];
                state(row, 'Gespeichert.', true);
            }, { no_please_wait: true });
        });

        shown.addEventListener('change', function () {
            const hidden = !shown.checked;
            shown.disabled = true;
            api_call('/api/phone/hide', { id: id, hidden: hidden ? 'true' : 'false' }, function (data) {
                shown.disabled = false;
                if (!data.success) {
                    shown.checked = hidden;
                    state(row, 'Nicht geändert. Versuch es noch einmal.', false);
                    return;
                }
                row.classList.toggle('phone-manage-hidden', data.hidden);
                state(row, data.hidden ? 'Ausgeblendet: nur über die Nummer erreichbar.' : 'Steht im Telefonbuch.', true);
            }, { no_please_wait: true });
        });
    });
})();
