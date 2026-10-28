// The page /git and the profile's section »Git« (main.rb, GitOverview):
// »Neues Repository« (in a group, or one's own), deleting one after typing
// its name, and a key pair for one's own Workspace.
(function () {
    'use strict';

    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();

    // --- Neues Repository ----------------------------------------------------
    document.querySelectorAll('[data-git-new]').forEach(function (box) {
        const open = box.querySelector('.git-new-open');
        const form = box.querySelector('.git-new-form');
        const name = form.querySelector('.git-new-name');
        const feedback = name.parentNode.querySelector('.invalid-feedback');
        function show(on) {
            form.hidden = !on;
            open.setAttribute('aria-expanded', on ? 'true' : 'false');
            if (on) name.focus();
        }
        function problem(text) {
            name.classList.toggle('is-invalid', !!text);
            name.setAttribute('aria-invalid', text ? 'true' : 'false');
            feedback.textContent = text || '';
        }
        open.addEventListener('click', function () { show(form.hidden); });
        form.querySelector('.git-new-cancel').addEventListener('click', function () { show(false); open.focus(); });
        // the rule of the address, as you type: lower case, digits, hyphens
        name.addEventListener('input', function () {
            const value = name.value;
            problem(value && !/^[a-z0-9][a-z0-9-]{0,62}$/.test(value)
                ? 'Nur Kleinbuchstaben, Ziffern und Bindestriche, vorne ein Buchstabe oder eine Ziffer.' : null);
        });
        form.addEventListener('submit', function (event) {
            event.preventDefault();
            const data = {
                name: name.value.trim(),
                description: form.elements.description.value,
                readme: form.elements.readme.checked,
            };
            const group = form.elements.group;
            if (group) data.group = group.value;
            const button = form.querySelector('[type="submit"]');
            button.disabled = true;
            api_call(group ? '/api/git/create_group_repo' : '/api/git/create_my_repo', data, function (r) {
                button.disabled = false;
                if (!r.success) return;
                if (!r.ok) { problem(r.error); name.focus(); return; }
                window.location.href = r.url;
            }, { no_please_wait: true });
        });
    });

    // --- Löschen, after typing the name --------------------------------------
    let dialog = null;
    function confirmDelete(button) {
        const name = button.dataset.name;
        if (!dialog) {
            dialog = document.createElement('div');
            dialog.className = 'modal fade';
            dialog.tabIndex = -1;
            dialog.setAttribute('aria-labelledby', 'git-delete-title');
            dialog.innerHTML = "<div class='modal-dialog'><div class='modal-content'>"
                + "<div class='modal-header'><h2 class='modal-title fs-5' id='git-delete-title'></h2>"
                + "<button type='button' class='btn-close' data-bs-dismiss='modal' aria-label='Schließen'></button></div>"
                + "<form class='modal-body' novalidate><p class='git-delete-what'></p>"
                + "<label class='form-label' for='git-delete-name'>Zum Bestätigen den Namen eintippen</label>"
                + "<input class='form-control' id='git-delete-name' autocomplete='off' spellcheck='false'>"
                + "<div class='invalid-feedback'></div></form>"
                + "<div class='modal-footer'><button type='button' class='btn btn-outline-secondary' data-bs-dismiss='modal'>Abbrechen</button>"
                + "<button type='button' class='btn btn-danger git-delete-go' disabled><i class='bi bi-trash' aria-hidden='true'></i>Endgültig löschen</button></div>"
                + "</div></div>";
            document.body.appendChild(dialog);
            dialog.addEventListener('shown.bs.modal', function () { dialog.querySelector('#git-delete-name').focus(); });
        }
        const input = dialog.querySelector('#git-delete-name');
        const go = dialog.querySelector('.git-delete-go');
        const feedback = dialog.querySelector('.invalid-feedback');
        dialog.querySelector('#git-delete-title').textContent = 'Repository »' + name + '« löschen';
        dialog.querySelector('.git-delete-what').innerHTML = 'Alle Dateien und alle Commits von <code>' + esc(name)
            + '</code> sind danach weg; das lässt sich nicht rückgängig machen. Wer es geklont hat, behält seinen Klon.';
        input.value = '';
        input.classList.remove('is-invalid');
        go.disabled = true;
        input.oninput = function () { go.disabled = input.value.trim() !== name; };
        const send = function () {
            if (go.disabled) return;
            const kind = button.dataset.gitDelete;
            const url = { group: '/api/git/delete_group_repo', mine: '/api/git/delete_my_repo', personal: '/api/git/delete_personal_repo' }[kind];
            const data = { name: name, confirmation: input.value.trim() };
            if (kind === 'group') data.group = button.dataset.group;
            if (kind === 'personal') data.id = button.dataset.id;
            go.disabled = true;
            api_call(url, data, function (r) {
                if (r.success && r.ok) { window.location.reload(); return; }
                go.disabled = false;
                input.classList.add('is-invalid');
                feedback.textContent = (r && r.error) || 'Das hat nicht geklappt.';
            }, { no_please_wait: true });
        };
        go.onclick = send;
        dialog.querySelector('form').onsubmit = function (event) { event.preventDefault(); send(); };
        bootstrap.Modal.getOrCreateInstance(dialog).show();
    }
    document.addEventListener('click', function (event) {
        const button = event.target.closest('[data-git-delete]');
        if (button) confirmDelete(button);
    });

    // --- a key pair in one's own Workspace -------------------------------------
    document.querySelectorAll('[data-git-install-key]').forEach(function (button) {
        button.addEventListener('click', function () {
            button.disabled = true;
            api_call('/api/git/install_my_key', {}, function (r) {
                if (r.success && r.ok) { window.location.reload(); return; }
                button.disabled = false;
                const note = document.createElement('p');
                note.className = 'git-key-warning';
                note.textContent = (r && r.error) || 'Das hat nicht geklappt.';
                button.parentNode.after(note);
            }, { no_please_wait: true });
        });
    });
})();
