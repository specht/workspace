// The Telefonbuch's management (/telefon-freigabe, phone_games_list_html):
// title and subtitle of each published game, edited in place: »Speichern«
// appears once one is changed (Enter saves too, Escape puts it back);
// »Im Telefonbuch« shows a game in the book and the phone's menu or hides it
// (a part of another game, still reached by its number); switched on, a game
// taken out after an error is back in the book. A row keeps its place
// until the page is loaded again (the lists: in the book, not in it).
(function () {
    const rows = document.querySelectorAll('.phone-manage-row');

    function state(row, text, ok) {
        const box = row.querySelector('.phone-manage-state');
        box.textContent = text;
        box.className = 'phone-manage-state small ' + (ok ? 'text-success' : 'text-danger');
        clearTimeout(box.timer);
        if (ok && text) box.timer = setTimeout(function () { box.textContent = ''; }, 4000);
    }

    function note(row, html) {
        row.querySelector('.phone-manage-note').innerHTML = html;
    }

    rows.forEach(function (row) {
        const id = row.dataset.id;
        const title = row.querySelector('.phone-manage-title');
        const description = row.querySelector('.phone-manage-description');
        const save = row.querySelector('.phone-manage-save');
        const shown = row.querySelector('.phone-manage-shown');
        let saved = [title.value, description.value];

        const changed = function () {
            const dirty = title.value !== saved[0] || description.value !== saved[1];
            save.hidden = !dirty;
            row.classList.toggle('phone-manage-dirty', dirty);
            if (dirty) state(row, '', true);
        };
        [title, description].forEach(function (input) {
            input.addEventListener('input', changed);
            input.addEventListener('keydown', function (e) {
                if (e.key !== 'Escape') return;
                title.value = saved[0];
                description.value = saved[1];
                changed();
                input.blur();
            });
        });

        row.addEventListener('submit', function (e) {
            e.preventDefault();
            if (save.hidden) return;
            if (!title.value.trim() || !description.value.trim()) {
                state(row, 'Titel und Untertitel dürfen nicht leer sein.', false);
                return;
            }
            save.disabled = true;
            api_call('/api/phone/rename', { id: id, title: title.value, description: description.value }, function (data) {
                save.disabled = false;
                if (!data.success) {
                    state(row, 'Nicht gespeichert. Versuch es noch einmal.', false);
                    return;
                }
                title.value = data.title;
                description.value = data.description;
                saved = [data.title, data.description];
                changed();
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
                row.classList.toggle('phone-manage-out', data.hidden);
                row.classList.remove('phone-manage-broken');
                note(row, data.hidden ? "<i class='bi bi-eye-slash' aria-hidden='true'></i>Ausgeblendet: nur über die Nummer erreichbar" : '');
                state(row, data.hidden ? 'Ausgeblendet.' : 'Steht im Telefonbuch.', true);
            }, { no_please_wait: true });
        });
    });

})();
