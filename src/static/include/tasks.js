// Task cards on activity pages (docs/aufgaben.md). The page sets
// window.TASK_PAGE = { activity, loggedIn, preview } before loading this file.
(function () {
    const page = window.TASK_PAGE || {};
    const STATUS_LABELS = {
        correct: ['task-badge-correct', 'bi-check-circle-fill', 'gelöst'],
        wrong: ['task-badge-wrong', 'bi-arrow-repeat', 'noch nicht gelöst'],
        sent: ['task-badge-sent', 'bi-send-check', 'gespeichert'],
        submitted: ['task-badge-sent', 'bi-send-check', 'abgegeben'],
        stale: ['task-badge-stale', 'bi-exclamation-circle', 'Aufgabe geändert, bitte neu prüfen'],
    };

    function cards() {
        return Array.from(document.querySelectorAll('.task-card'));
    }

    function cardId(card) {
        return card.dataset.task.split('/')[1];
    }

    function setBadge(card, status) {
        const el = card.querySelector('.task-status');
        el.innerHTML = '';
        const label = STATUS_LABELS[status === 'sent' && card.dataset.type === 'submit' ? 'submitted' : status];
        if (!label) return;
        const span = document.createElement('span');
        span.className = 'task-badge ' + label[0];
        const icon = document.createElement('i');
        icon.className = 'bi ' + label[1];
        span.appendChild(icon);
        span.appendChild(document.createTextNode(label[2]));
        el.appendChild(span);
        card.dataset.status = status;
    }

    function readAnswer(card) {
        if (card.dataset.type === 'measure') {
            return JSON.stringify(Array.from(card.querySelectorAll('.task-measure-input')).map((input) => input.value));
        }
        if (card.dataset.type === 'choice') {
            return Array.from(card.querySelectorAll('.form-check-input:checked'))
                .map((input) => input.value).join(',');
        }
        const input = card.querySelector('.task-input');
        return input ? input.value : '';
    }

    // preview: anaglyph (anaglyph_preview.js): a picture of the scene as the
    // class gallery will show it, from its own camera line, with a button to
    // look at it full size. scene is a path below /anaglyph-viewer/.
    function showScenePreview(card, scene, caption) {
        let box = card.querySelector('.task-scene-preview');
        if (!scene || !window.AnaglyphPreview) {
            if (box) box.remove();
            return;
        }
        if (!box) {
            box = document.createElement('div');
            box.className = 'task-scene-preview';
            card.querySelector('.task-actions').before(box);
        }
        const token = String(Date.now() + Math.random());
        box.dataset.token = token;
        box.innerHTML = '';
        const text = document.createElement('p');
        text.textContent = caption;
        const img = document.createElement('img');
        img.alt = 'Die Szene, wie die Galerie sie zeigt.';
        const status = document.createElement('p');
        status.className = 'task-scene-preview-status';
        status.textContent = 'Die Szene wird gezeichnet …';
        box.append(text, img, status);
        window.AnaglyphPreview.picture(scene).then((result) => {
            if (box.dataset.token !== token) return;
            img.src = result.picture;
            status.textContent = result.errors ? 'Der Viewer meldet Fehler in dieser Szene: ' + result.errors : '';
        }).catch((error) => {
            if (box.dataset.token !== token) return;
            status.textContent = error.message;
        });
    }

    // What a submission holds, with a link to download it.
    function showSubmission(card, summary) {
        let box = card.querySelector('.task-submission');
        if (card.dataset.preview === 'anaglyph') {
            showScenePreview(card, summary ? 'abgaben/' + card.dataset.task + '/ich.ini' : null,
                'Deine Abgabe, so wie sie in der Galerie erscheint:');
        }
        if (!summary) {
            if (box) box.remove();
            return;
        }
        if (!box) {
            box = document.createElement('div');
            box.className = 'task-submission';
            card.querySelector('.task-actions').before(box);
        }
        box.innerHTML = '';
        box.appendChild(document.createTextNode('Deine Abgabe: ' + summary + ' · '));
        const link = document.createElement('a');
        link.href = '/api/task/submission/' + card.dataset.task;
        link.textContent = 'herunterladen';
        box.appendChild(link);
    }

    function writeAnswer(card, answer) {
        if (card.dataset.type === 'submit') {
            const select = card.querySelector('.task-pick');
            const parts = String(answer || '').split(' · ');
            if (select && parts.length > 1) {
                select.dataset.stored = parts[0];
                select.value = parts[0];
            }
            showSubmission(card, answer || null);
            return;
        }
        if (card.dataset.type === 'measure') {
            let values = [];
            try { values = JSON.parse(answer || '[]'); } catch (e) { values = []; }
            card.querySelectorAll('.task-measure-input').forEach((input, i) => {
                input.value = values[i] || '';
            });
            return;
        }
        if (card.dataset.type === 'choice') {
            const picked = String(answer || '').split(',');
            card.querySelectorAll('.form-check-input').forEach((input) => {
                input.checked = picked.indexOf(input.value) >= 0;
            });
            return;
        }
        const input = card.querySelector('.task-input');
        if (input) input.value = answer || '';
    }

    // message and solution are HTML rendered on the server from the task
    // file; anything from the student (command output, SQL results) is
    // escaped there before it becomes part of them.
    function showFeedback(card, status, message, solution, read, warnings) {
        const box = card.querySelector('.task-feedback');
        box.className = 'task-feedback task-feedback-' + status;
        box.innerHTML = '';
        if (message) {
            const div = document.createElement('div');
            div.className = 'task-message';
            div.innerHTML = message;
            box.appendChild(div);
        }
        if (warnings && warnings.length > 0) {
            const ul = document.createElement('ul');
            ul.className = 'task-warnings';
            warnings.forEach((w) => {
                const li = document.createElement('li');
                li.innerHTML = w;
                ul.appendChild(li);
            });
            box.appendChild(ul);
        }
        if (read && card.dataset.type === 'sql' && status !== 'unavailable') {
            // SQL: the student's own result is the point, always in view.
            const div = document.createElement('div');
            div.className = 'task-result';
            div.innerHTML = read;
            box.appendChild(div);
        } else if (read) {
            const details = document.createElement('details');
            details.className = 'task-read';
            const summary = document.createElement('summary');
            summary.textContent = 'Was der Workspace gelesen hat';
            details.appendChild(summary);
            const body = document.createElement('div');
            body.innerHTML = read;
            details.appendChild(body);
            // Could not check: what was looked at is the diagnosis, show it.
            if (status === 'unavailable') details.open = true;
            box.appendChild(details);
        }
        if (solution) {
            const details = document.createElement('details');
            details.className = 'task-solution';
            const summary = document.createElement('summary');
            summary.textContent = 'Lösung und Erklärung';
            details.appendChild(summary);
            const body = document.createElement('div');
            body.innerHTML = solution;
            details.appendChild(body);
            box.appendChild(details);
        }
    }

    function setBusy(card, busy) {
        card.querySelectorAll('button, input, textarea, select').forEach((el) => {
            el.disabled = busy;
        });
    }

    function submit(card) {
        const answer = readAnswer(card);
        setBusy(card, true);
        api_call('/api/task/check', { task: card.dataset.task, answer: answer }, function (data) {
            setBusy(card, false);
            if (!data.success) {
                showFeedback(card, 'unavailable', '<p>Das hat gerade nicht geklappt. Versuch es gleich noch einmal.</p>');
                return;
            }
            showFeedback(card, data.status, data.message, data.solution, data.read, data.warnings);
            if (data.state) {
                setBadge(card, data.state.status);
                card.querySelector('.task-delete').hidden = false;
                if (card.dataset.type === 'submit') showSubmission(card, data.state.answer);
            }
        }, { no_please_wait: true });
    }

    function remove(card) {
        const submit = card.dataset.type === 'submit';
        confirm_dialog({
            title: submit ? 'Abgabe löschen?' : 'Antwort löschen?',
            text: (submit ? 'Deine Abgabe' : 'Deine gespeicherte Antwort zu dieser Aufgabe') + ' wird gelöscht. Das kann nicht rückgängig gemacht werden.',
            confirm: 'Löschen', icon: 'bi-trash', danger: true,
        }, function () { reallyRemove(card); });
    }

    function reallyRemove(card) {
        api_call('/api/task/delete', { task: card.dataset.task }, function (data) {
            if (!data.success) return;
            if (card.dataset.type === 'submit') showSubmission(card, null);
            else writeAnswer(card, '');
            setBadge(card, null);
            delete card.dataset.status;
            card.querySelector('.task-delete').hidden = true;
            showFeedback(card, 'sent', '<p>Gelöscht.</p>');
        }, { no_please_wait: true });
    }

    // Submit tasks with pick: list the matching files in the Workspace.
    // The list is read again whenever the dropdown gets focus or the page
    // comes back into view, so a file made in the Workspace tab a moment ago
    // shows up. The choice survives as long as the file is still there.
    function loadOptions(card) {
        const select = card.querySelector('.task-pick');
        if (!select || select.dataset.loading) return;
        select.dataset.loading = '1';
        api_call('/api/task/options', { task: card.dataset.task }, function (data) {
            delete select.dataset.loading;
            if (!data.success) return;
            const paths = data.options || [];
            const current = Array.from(select.options).slice(1).map((o) => o.value);
            if (paths.join('\n') === current.join('\n')) return;
            const chosen = select.value || select.dataset.stored || '';
            while (select.options.length > 1) select.remove(1);
            paths.forEach((path) => {
                const option = document.createElement('option');
                option.value = path;
                option.textContent = path;
                select.appendChild(option);
            });
            select.value = paths.indexOf(chosen) >= 0 ? chosen : '';
        }, { no_please_wait: true });
    }

    function pickCards() {
        return cards().filter((card) => card.querySelector('.task-pick'));
    }

    function watchPicks() {
        pickCards().forEach((card) => {
            card.querySelector('.task-pick').addEventListener('focus', function () { loadOptions(card); });
        });
        window.addEventListener('focus', function () { pickCards().forEach(loadOptions); });
        document.addEventListener('visibilitychange', function () {
            if (!document.hidden) pickCards().forEach(loadOptions);
        });
    }

    // _include(…, done: id): once task id is solved, its Handbuch box
    // shrinks to one line. Only when the page loads, never while someone reads.
    function foldDoneIncludes() {
        document.querySelectorAll('.reference-include[data-done]').forEach((box) => {
            const card = document.getElementById('aufgabe-' + box.dataset.done);
            if (!card || card.dataset.status !== 'correct') return;
            const line = document.createElement('p');
            line.className = 'reference-include-done';
            const icon = document.createElement('i');
            icon.className = 'bi bi-check-circle-fill';
            line.appendChild(icon);
            line.appendChild(document.createTextNode((box.dataset.title || 'Erledigt') + ': erledigt, '));
            const link = document.createElement('a');
            link.href = '#' + card.id;
            link.textContent = card.querySelector('.task-number').textContent;
            line.appendChild(link);
            line.appendChild(document.createTextNode(' ist gelöst.'));
            const show = document.createElement('button');
            show.type = 'button';
            show.className = 'btn btn-link btn-sm';
            show.textContent = 'Anzeigen';
            show.addEventListener('click', function () {
                box.classList.remove('reference-include-folded');
                line.remove();
            });
            line.appendChild(show);
            box.prepend(line);
            box.classList.add('reference-include-folded');
        });
    }

    function loadStates() {
        api_call('/api/task/states', { activity: page.activity }, function (data) {
            if (!data.success) return;
            cards().forEach((card) => {
                const state = (data.states || {})[cardId(card)];
                if (!state) return;
                writeAnswer(card, state.answer);
                setBadge(card, state.status);
                const del = card.querySelector('.task-delete');
                if (del) del.hidden = false;
            });
            foldDoneIncludes();
        }, { no_please_wait: true });
    }

    // Choice options in this person's own order (page.order, from the
    // server). Only their places change; each keeps its original number as
    // its value, which is what is checked and stored.
    function arrangeOptions(card) {
        const order = (page.order || {})[cardId(card)];
        const fieldset = card.querySelector('.task-options');
        if (!order || !fieldset) return;
        const options = Array.from(fieldset.querySelectorAll('.form-check'));
        if (options.length !== order.length) return;
        order.forEach((i) => fieldset.appendChild(options[i]));
    }

    function setup() {
        cards().forEach((card) => {
            arrangeOptions(card);
            if (page.preview) {
                const note = document.createElement('span');
                note.className = 'task-preview';
                note.title = 'Schülerinnen und Schüler sehen diese Aufgabe erst, wenn Aufgaben für die Installation eingeschaltet sind.';
                note.textContent = 'Vorschau für Lehrkräfte';
                card.querySelector('.task-head').appendChild(note);
            }
            const form = card.querySelector('.task-form');
            if (!form) return;
            if (!page.loggedIn) {
                card.classList.add('task-logged-out');
                setBusy(card, true);
                return;
            }
            form.addEventListener('submit', function (e) {
                e.preventDefault();
                submit(card);
            });
            // SQL: Enter makes a new line, Ctrl+Enter (Cmd+Enter) checks.
            const sql = card.querySelector('.task-sql');
            if (sql) sql.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
                    e.preventDefault();
                    submit(card);
                }
            });
            const del = card.querySelector('.task-delete');
            if (del) {
                if (card.dataset.type === 'submit') del.textContent = 'Abgabe löschen';
                else if (['answer', 'choice', 'number', 'reflection', 'measure', 'command', 'sql'].indexOf(card.dataset.type) < 0) del.textContent = 'Ergebnis löschen';
                del.addEventListener('click', function () { remove(card); });
            }
            loadOptions(card);
            const pick = card.querySelector('.task-pick');
            if (pick && card.dataset.preview === 'anaglyph') {
                pick.addEventListener('change', function () {
                    if (!pick.value) return;
                    showScenePreview(card, 'vorschau/' + card.dataset.task + '/' + pick.value.split('/').pop(),
                        'Vorschau: So würde ' + pick.value.split('/').pop() + ' in der Galerie erscheinen.');
                });
            }
        });
        if (page.loggedIn) {
            loadStates();
            watchPicks();
        }
    }

    if (document.readyState === 'loading')
        document.addEventListener('DOMContentLoaded', setup);
    else
        setup();
})();
