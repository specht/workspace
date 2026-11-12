// »Nextcloud der Schule« on the class page (class_nextcloud_html): the
// address goes to the server, which asks it for status.php and keeps it only
// if a Nextcloud answers; the line below says what came back.
(function () {
    'use strict';
    const box = document.querySelector('.class-nextcloud');
    if (!box || box.dataset.ready) return;
    box.dataset.ready = '1';
    const input = box.querySelector('#class-nextcloud-url');
    const save = box.querySelector('.class-nextcloud-save');
    const remove = box.querySelector('.class-nextcloud-remove');
    const state = box.querySelector('.class-nextcloud-state');
    const say = (icon, text, bad) => {
        state.textContent = '';
        if (icon) {
            const i = document.createElement('i');
            i.className = 'bi ' + icon;
            i.setAttribute('aria-hidden', 'true');
            state.appendChild(i);
        }
        state.appendChild(document.createTextNode(text));
        state.classList.toggle('text-danger', !!bad);
    };
    const send = (url) => {
        save.disabled = true;
        remove.disabled = true;
        if (url) say('bi-hourglass-split', 'Frage bei ' + url + ' nach …');
        api_call('/api/set_group_nextcloud', { group: box.dataset.group, url: url }, (data) => {
            save.disabled = false;
            remove.disabled = false;
            if (!data.success) {
                say('bi-exclamation-triangle', 'Das hat nicht geklappt. Versuch es noch einmal.', true);
            } else if (data.refused) {
                say('bi-exclamation-triangle', data.refused, true);
            } else if (data.url) {
                input.value = data.url;
                remove.hidden = false;
                say('bi-check-lg', 'Eingetragen: ' + data.url + ' (' + data.product + ' ' + data.version + ')' +
                    (data.maintenance ? '. Sie wird gerade gewartet; Links gehen danach.' : ''));
            } else {
                input.value = '';
                remove.hidden = true;
                say(null, 'Keine Nextcloud eingetragen.');
            }
        }, { no_please_wait: true });
    };
    save.addEventListener('click', () => {
        const url = input.value.trim();
        if (!url) {
            say('bi-exclamation-triangle', 'Gib zuerst die Adresse ein, zum Beispiel https://cloud.schule.de', true);
            input.focus();
            return;
        }
        send(url);
    });
    input.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') {
            e.preventDefault();
            save.click();
        }
    });
    remove.addEventListener('click', () => send(''));
})();
