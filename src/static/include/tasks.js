// Task cards on activity pages (docs/aufgaben.md). The page sets
// window.TASK_PAGE = { activity, loggedIn, preview } before loading this file.
(function () {
    const page = window.TASK_PAGE || {};
    const STATUS_LABELS = {
        correct: ['task-badge-correct', 'bi-check-circle-fill', 'gelöst'],
        wrong: ['task-badge-wrong', 'bi-arrow-repeat', 'noch nicht gelöst'],
        sent: ['task-badge-sent', 'bi-send-check', 'gespeichert'],
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
        const label = STATUS_LABELS[status];
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
        if (card.dataset.type === 'choice') {
            return Array.from(card.querySelectorAll('.form-check-input:checked'))
                .map((input) => input.value).join(',');
        }
        const input = card.querySelector('.task-input');
        return input ? input.value : '';
    }

    function writeAnswer(card, answer) {
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
    // file; the student's own answer is never part of them.
    function showFeedback(card, status, message, solution) {
        const box = card.querySelector('.task-feedback');
        box.className = 'task-feedback task-feedback-' + status;
        box.innerHTML = '';
        if (message) {
            const div = document.createElement('div');
            div.className = 'task-message';
            div.innerHTML = message;
            box.appendChild(div);
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
        card.querySelectorAll('button, input, textarea').forEach((el) => {
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
            showFeedback(card, data.status, data.message, data.solution);
            if (data.state) {
                setBadge(card, data.state.status);
                card.querySelector('.task-delete').hidden = false;
            }
        }, { no_please_wait: true });
    }

    function remove(card) {
        if (!window.confirm('Deine gespeicherte Antwort zu dieser Aufgabe löschen? Das kann nicht rückgängig gemacht werden.')) return;
        api_call('/api/task/delete', { task: card.dataset.task }, function (data) {
            if (!data.success) return;
            writeAnswer(card, '');
            setBadge(card, null);
            delete card.dataset.status;
            card.querySelector('.task-delete').hidden = true;
            showFeedback(card, 'sent', '<p>Gelöscht.</p>');
        }, { no_please_wait: true });
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
        }, { no_please_wait: true });
    }

    function setup() {
        cards().forEach((card) => {
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
            const del = card.querySelector('.task-delete');
            if (del) del.addEventListener('click', function () { remove(card); });
        });
        if (page.loggedIn) loadStates();
    }

    if (document.readyState === 'loading')
        document.addEventListener('DOMContentLoaded', setup);
    else
        setup();
})();
