// The key box for the Workspace's own Git server (GitServer in main.rb;
// Handbuch »Git-Server«, parts ssh-workspace and ssh-workspace-auto):
//
//   <div class='git-key' data-git-key></div>
//
// It asks the server what it knows (/api/git/key_state): the public key in
// the person's Workspace, the key they handed in, whether their teacher had
// one set up for them, and the fingerprint of the server's own key, to
// compare when SSH asks the first time. »Schlüssel abgeben« hands in the
// public key from ~/.ssh; the private key never leaves the Workspace.
(function () {
    'use strict';

    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();

    function render(box, state) {
        let html = '';
        if (state.auto) {
            html += `<p><i class='bi bi-check-lg' aria-hidden='true'></i>Deine Lehrkraft hat für deinen Workspace einen Schlüssel einrichten lassen: `
                + `<code>~/.ssh/workspace_git</code>. Du musst nichts abgeben.</p>`;
        } else if (state.handed_in) {
            const same = state.workspace && state.workspace.fingerprint === state.handed_in;
            html += `<p><i class='bi bi-check-lg' aria-hidden='true'></i>Abgegeben: dein öffentlicher Schlüssel mit dem Fingerabdruck `
                + `<code>${esc(state.handed_in)}</code>.</p>`;
            if (state.workspace && !same) {
                html += `<p class='git-key-warning'>In deinem Workspace liegt inzwischen ein anderer Schlüssel (<code>${esc(state.workspace.file)}</code>). `
                    + `Gib ihn neu ab, sonst lässt der Git-Server dich nicht herein.</p>`;
            }
            html += `<div class='git-key-buttons'>`
                + (state.workspace && !same ? `<button type='button' class='btn btn-success git-key-hand-in'><i class='bi bi-upload' aria-hidden='true'></i>Neu abgeben</button>` : '')
                + `<button type='button' class='btn btn-outline-secondary git-key-remove'><i class='bi bi-x-lg' aria-hidden='true'></i>Zurückziehen</button></div>`;
        } else if (state.workspace) {
            html += `<p>In deinem Workspace liegt der öffentliche Schlüssel <code>${esc(state.workspace.file)}</code> `
                + `mit dem Fingerabdruck <code>${esc(state.workspace.fingerprint)}</code>.</p>`
                + `<div class='git-key-buttons'><button type='button' class='btn btn-success git-key-hand-in'>`
                + `<i class='bi bi-upload' aria-hidden='true'></i>Schlüssel abgeben</button></div>`;
        } else {
            html += `<p>In deinem Workspace liegt noch kein Schlüssel in <code>~/.ssh</code>. Erzeuge zuerst einen, dann `
                + `<button type='button' class='btn btn-link git-key-reload'>sieh hier noch einmal nach</button>.</p>`;
        }
        if (state.host) {
            html += `<p class='git-key-host'>Der Git-Server <code>${esc(state.host_name)}</code> hat den Fingerabdruck `
                + `<code>${esc(state.host)}</code>.</p>`;
        }
        box.innerHTML = html;
    }

    function load(box) {
        api_call('/api/git/key_state', {}, function (r) {
            if (!r.success) {
                box.innerHTML = `<p>Melde dich an, dann siehst du hier deinen Schlüssel.</p>`;
                return;
            }
            render(box, r);
        }, { no_please_wait: true });
    }

    function bind(box) {
        box.addEventListener('click', function (e) {
            const target = e.target.closest('button');
            if (!target) return;
            if (target.classList.contains('git-key-reload')) { load(box); return; }
            const url = target.classList.contains('git-key-hand-in') ? '/api/git/hand_in_key'
                : target.classList.contains('git-key-remove') ? '/api/git/remove_key' : null;
            if (!url) return;
            target.disabled = true;
            api_call(url, {}, function (r) {
                target.disabled = false;
                if (r.success && r.ok === false && r.error === 'missing') {
                    show_error_message('In deinem Workspace liegt kein öffentlicher Schlüssel in ~/.ssh.');
                }
                load(box);
            }, { no_please_wait: true });
        });
    }

    $(function () {
        document.querySelectorAll('[data-git-key]').forEach(function (box) {
            bind(box);
            load(box);
        });
    });
})();
