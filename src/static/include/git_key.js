// The key box for the Workspace's own Git server (GitServer in main.rb;
// Handbuch »Git-Server«, parts ssh-workspace and ssh-workspace-auto):
//
//   <div class='git-key' data-git-key></div>
//
// As on any Git server, the person pastes the line of their public key
// (~/.ssh/id_ed25519.pub) and adds it; the private key never leaves the
// Workspace. The box asks the server once what it knows
// (/api/git/key_state): the key handed in, the public key in the person's
// Workspace (only to say when the two differ), whether their teacher had a
// key set up for them, and the fingerprint of the server's own key, to
// compare when SSH asks the first time.
(function () {
    'use strict';

    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    const ERRORS = {
        private: 'Das ist dein <b>privater</b> Schlüssel. Er bleibt in deinem Workspace; gib nur die Zeile aus <code>~/.ssh/id_ed25519.pub</code> ab.',
        invalid: 'Das ist kein öffentlicher Schlüssel. Kopier die ganze Zeile aus <code>~/.ssh/id_ed25519.pub</code>; sie beginnt mit <code>ssh-ed25519</code>.',
    };
    let boxes = 0;

    function render(box, state, error) {
        let html = '';
        if (state.auto) {
            html += `<p><i class='bi bi-check-lg' aria-hidden='true'></i>Deine Lehrkraft hat für deinen Workspace einen Schlüssel einrichten lassen: `
                + `<code>~/.ssh/workspace_git</code>. Du musst nichts abgeben.</p>`;
        } else {
            if (state.handed_in) {
                html += `<p><i class='bi bi-check-lg' aria-hidden='true'></i>Abgegeben: ein öffentlicher Schlüssel mit dem Fingerabdruck `
                    + `<code>${esc(state.handed_in)}</code>.</p>`;
                if (state.workspace && state.workspace.fingerprint !== state.handed_in) {
                    html += `<p class='git-key-warning'>In deinem Workspace liegt ein anderer Schlüssel (<code>${esc(state.workspace.file)}</code>, `
                        + `<code>${esc(state.workspace.fingerprint)}</code>). Mit ihm lässt dich der Git-Server nicht herein; füg seine Zeile unten ein.</p>`;
                }
                html += `<div class='git-key-buttons'><button type='button' class='btn btn-outline-secondary git-key-remove'>`
                    + `<i class='bi bi-x-lg' aria-hidden='true'></i>Zurückziehen</button></div>`;
            }
            const id = box.dataset.keyId;
            html += `<label class='form-label' for='${id}'>${state.handed_in ? 'Einen anderen öffentlichen Schlüssel einfügen' : 'Dein öffentlicher Schlüssel'}</label>`
                + `<textarea class='form-control git-key-input${error ? ' is-invalid' : ''}' id='${id}' rows='3' spellcheck='false' autocapitalize='off' `
                + `placeholder='ssh-ed25519 AAAA… Vorname Nachname'${error ? ` aria-invalid='true' aria-describedby='${id}-error'` : ''}></textarea>`
                + (error ? `<div class='invalid-feedback' id='${id}-error'>${ERRORS[error] || ERRORS.invalid}</div>` : '')
                + `<div class='git-key-buttons'><button type='button' class='btn btn-success git-key-hand-in'>`
                + `<i class='bi bi-plus-lg' aria-hidden='true'></i>Hinzufügen</button></div>`;
        }
        if (state.host) {
            html += `<p class='git-key-host'>Der Git-Server <code>${esc(state.host_name)}</code> hat den Fingerabdruck `
                + `<code>${esc(state.host)}</code>.</p>`;
        }
        box.innerHTML = html;
    }

    function load(box, error) {
        api_call('/api/git/key_state', {}, function (r) {
            if (!r.success) {
                box.innerHTML = `<p>Melde dich an, dann siehst du hier deinen Schlüssel.</p>`;
                return;
            }
            render(box, r, error);
            if (error) box.querySelector('.git-key-input').focus();
        }, { no_please_wait: true });
    }

    function bind(box) {
        box.addEventListener('click', function (e) {
            const target = e.target.closest('button');
            if (!target) return;
            if (target.classList.contains('git-key-hand-in')) {
                const input = box.querySelector('.git-key-input');
                if (!input.value.trim()) { input.focus(); return; }
                target.disabled = true;
                api_call('/api/git/hand_in_key', { key: input.value }, function (r) {
                    target.disabled = false;
                    if (!r.success) return;
                    load(box, r.ok ? null : r.error);
                }, { no_please_wait: true });
            } else if (target.classList.contains('git-key-remove')) {
                target.disabled = true;
                api_call('/api/git/remove_key', {}, function () { load(box); }, { no_please_wait: true });
            }
        });
    }

    $(function () {
        document.querySelectorAll('[data-git-key]').forEach(function (box) {
            box.dataset.keyId = 'git-key-input-' + (++boxes);
            bind(box);
            load(box);
        });
    });
})();
