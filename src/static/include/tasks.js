// Task cards on activity pages (docs/aufgaben.md). The page sets
// window.TASK_PAGE = { activity, loggedIn, preview } before loading this file;
// for teachers also teacher = { view, groups, group, examples } (main.rb,
// open_answers_config).
(function () {
    const page = window.TASK_PAGE || {};
    const STATUS_LABELS = {
        correct: ['task-badge-correct', 'bi-check-circle-fill', 'gelöst'],
        wrong: ['task-badge-wrong', 'bi-arrow-repeat', 'noch nicht gelöst'],
        sent: ['task-badge-sent', 'bi-paper-plane', 'gespeichert'],
        submitted: ['task-badge-sent', 'bi-paper-plane', 'abgegeben'],
        stale: ['task-badge-stale', 'bi-exclamation-circle', 'Aufgabe geändert, bitte neu prüfen'],
    };

    function cards() {
        return Array.from(document.querySelectorAll('.task-card'));
    }

    function cardId(card) {
        return card.dataset.task.split('/')[1];
    }

    const TICK = "<svg class='task-tick' viewBox='0 0 24 24' aria-hidden='true'>" +
        "<circle class='task-tick-disc' cx='12' cy='12' r='11'/>" +
        "<path class='task-tick-mark' d='M6.8 12.6l3.4 3.4 7-7.4'/></svg>";

    // A solved task has its tick next to the button. Just solved, it pops
    // in and draws itself, and the card glows once.
    function setTick(card, solved, celebrate) {
        const old = card.querySelector('.task-done-tick');
        if (old) old.remove();
        const button = card.querySelector('.task-submit');
        if (solved && button) {
            const tick = document.createElement('span');
            tick.className = 'task-done-tick' + (celebrate ? ' task-done-tick-new' : '');
            tick.innerHTML = TICK;
            button.insertAdjacentElement('afterend', tick);
        }
        if (celebrate) {
            card.classList.remove('task-solved-now');
            void card.offsetWidth;
            card.classList.add('task-solved-now');
        }
    }

    // celebrate: the task was just solved.
    function setBadge(card, status, celebrate) {
        const el = card.querySelector('.task-status');
        el.innerHTML = '';
        setTick(card, status === 'correct', celebrate);
        const label = STATUS_LABELS[status === 'sent' && card.dataset.type === 'submit' ? 'submitted' : status];
        if (!label) return;
        const span = document.createElement('span');
        span.className = 'task-badge ' + label[0];
        // Solved: the tick stands next to the button, the pill just says so.
        if (status !== 'correct') {
            const icon = document.createElement('i');
            icon.className = 'bi ' + label[1];
            span.appendChild(icon);
        }
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
    // Returns the promise of the picture (anaglyph_preview.js), or null.
    function showScenePreview(card, scene, caption) {
        let box = card.querySelector('.task-scene-preview');
        if (!scene || !window.AnaglyphPreview) {
            if (box) box.remove();
            return null;
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
        const promise = window.AnaglyphPreview.picture(scene);
        promise.then((result) => {
            if (box.dataset.token !== token) return;
            img.src = result.picture;
            status.textContent = result.errors ? 'Der Viewer meldet Fehler in dieser Szene: ' + result.errors : '';
        }).catch((error) => {
            if (box.dataset.token !== token) return;
            status.textContent = error.message;
        });
        return promise;
    }

    // The preview of the picked scene, for the check before a hand-in.
    // fresh: draw it again, the file may have changed since it was picked.
    function previewPicked(card, pick, fresh) {
        const name = pick.value.split('/').pop();
        if (fresh || !card.pickedPreview || card.pickedPreview.file !== pick.value) {
            card.pickedPreview = {
                file: pick.value,
                promise: showScenePreview(card, 'vorschau/' + card.dataset.task + '/' + name,
                    'Vorschau: So würde ' + name + ' in der Galerie erscheinen.'),
            };
        }
        return card.pickedPreview.promise;
    }

    // preview: anaglyph: the scene must load and have enough objects. The
    // card checks it with the picture it draws anyway, so no Workspace has
    // to run (task_check.rb). Calls go() when the scene may be handed in.
    function checkPickedScene(card, go) {
        const pick = card.querySelector('.task-pick');
        if (card.dataset.preview !== 'anaglyph' || !pick || !pick.value) return go();
        const promise = previewPicked(card, pick, true);
        if (!promise) return go();
        const file = '<code>' + pick.value.replace(/[&<>]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' }[c])) + '</code>';
        promise.then((result) => {
            const min = Number(card.dataset.minObjects || 0);
            if (result.errors) {
                showFeedback(card, 'invalid', '<p>Der Viewer meldet Fehler in ' + file + '. Repariere das vor der Abgabe; die Meldung steht unter dem Bild.</p>');
            } else if (min && result.objects !== null && result.objects < min) {
                const count = result.objects === 0 ? 'steht kein Objekt' : result.objects === 1 ? 'steht 1 Objekt' : 'stehen ' + result.objects + ' Objekte';
                showFeedback(card, 'invalid', '<p>In ' + file + ' ' + count + '; für die Galerie sind mindestens ' + min + ' verlangt.</p>');
            } else {
                return go();
            }
            setBusy(card, false);
        }, () => go());
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
        if (input && input.codeInputUpdate) input.codeInputUpdate();
    }

    // message and solution are HTML rendered on the server from the task
    // file; anything from the student (command output, SQL results) is
    // escaped there before it becomes part of them.
    // Stars when it is right; a light bulb, not a cross, when it is not yet.
    const FEEDBACK_ICONS = {
        correct: 'bi-stars', wrong: 'bi-lightbulb', sent: 'bi-paper-plane',
        invalid: 'bi-info-circle', unavailable: 'bi-info-circle',
    };

    // In view: the first paragraph of the message (and the result of an SQL
    // query). The rest of the message, hints and what the Workspace read wait
    // behind a small »Details« at the end of that line, the solution behind
    // »Lösung«; each opens below when asked for.
    function showFeedback(card, status, message, solution, read, warnings) {
        const box = card.querySelector('.task-feedback');
        box.className = 'task-feedback task-feedback-' + status;
        box.innerHTML = '';
        const icon = document.createElement('i');
        icon.className = 'bi task-feedback-icon ' + (FEEDBACK_ICONS[status] || 'bi-info-circle');
        icon.setAttribute('aria-hidden', 'true');
        box.appendChild(icon);
        const content = document.createElement('div');
        content.className = 'task-feedback-body';
        box.appendChild(content);

        const line = document.createElement('div');
        line.className = 'task-message';
        content.appendChild(line);
        const more = document.createElement('div');
        if (message) {
            const holder = document.createElement('div');
            holder.innerHTML = message;
            const blocks = Array.from(holder.children);
            if (blocks.length === 0) line.innerHTML = message;
            else {
                line.appendChild(blocks[0]);
                blocks.slice(1).forEach((el) => more.appendChild(el));
            }
        }
        if (warnings && warnings.length > 0) {
            const ul = document.createElement('ul');
            ul.className = 'task-warnings';
            warnings.forEach((w) => {
                const li = document.createElement('li');
                li.innerHTML = w;
                ul.appendChild(li);
            });
            more.appendChild(ul);
        }
        if (read && (card.dataset.type === 'sql' || card.dataset.type === 'cypher') && status !== 'unavailable') {
            // SQL: the student's own result is the point, always in view.
            const div = document.createElement('div');
            div.className = 'task-result';
            div.innerHTML = read;
            content.appendChild(div);
        } else if (read) {
            const title = document.createElement('p');
            title.className = 'task-read-title';
            title.textContent = 'Was der Workspace gelesen hat:';
            more.appendChild(title);
            const body = document.createElement('div');
            body.innerHTML = read;
            more.appendChild(body);
        }

        // The small links go at the end of the first line.
        const last = line.lastElementChild;
        const end = last && last.tagName === 'P' ? last : line;
        const addToggle = (label, panelClass, panelContent) => {
            const panel = document.createElement('div');
            panel.className = 'task-more ' + panelClass;
            panel.id = card.id + '-' + panelClass;
            panel.hidden = true;
            panel.appendChild(panelContent);
            content.appendChild(panel);
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'task-more-toggle';
            button.textContent = label;
            button.setAttribute('aria-expanded', 'false');
            button.setAttribute('aria-controls', panel.id);
            end.appendChild(document.createTextNode(' '));
            end.appendChild(button);
        };
        if (more.childNodes.length > 0) addToggle('Details', 'task-read', more);
        if (solution) {
            const body = document.createElement('div');
            body.innerHTML = solution;
            addToggle('Lösung', 'task-solution', body);
        }
    }

    function setDisabled(card, disabled) {
        card.querySelectorAll('button:not(.task-reads-toggle), input, textarea, select').forEach((el) => {
            el.disabled = disabled;
        });
    }

    // While a check or hand-in runs, the card cannot be used, and its
    // button says that something is happening: a spinner in its middle, its
    // label kept but invisible, so that it keeps its size and nothing jumps;
    // a screen reader hears what happens.
    function setBusy(card, busy) {
        setDisabled(card, busy);
        const button = card.querySelector('.task-submit');
        if (!button) return;
        if (busy && !button.classList.contains('is-busy')) {
            button.classList.add('is-busy');
            button.setAttribute('aria-busy', 'true');
            const spinner = document.createElement('span');
            spinner.className = 'task-submit-busy';
            spinner.innerHTML = "<span class='spinner-border spinner-border-sm' aria-hidden='true'></span>" +
                "<span class='visually-hidden'>" + (card.dataset.type === 'submit' ? 'Wird abgegeben …' : 'Wird geprüft …') + '</span>';
            button.appendChild(spinner);
        } else if (!busy && button.classList.contains('is-busy')) {
            button.classList.remove('is-busy');
            button.removeAttribute('aria-busy');
            button.querySelectorAll('.task-submit-busy').forEach((el) => el.remove());
        }
    }

    function submit(card) {
        const answer = readAnswer(card);
        setBusy(card, true);
        checkPickedScene(card, function () { send(card, answer); });
    }

    function send(card, answer) {
        api_call('/api/task/check', { task: card.dataset.task, answer: answer, language: card.dataset.language }, function (data) {
            setBusy(card, false);
            if (!data.success) {
                showFeedback(card, 'unavailable', '<p>Das hat gerade nicht geklappt. Versuch es gleich noch einmal.</p>');
                return;
            }
            showFeedback(card, data.status, data.message, data.solution, data.read, data.warnings);
            if (data.state) {
                setBadge(card, data.state.status, data.status === 'correct' && card.dataset.status !== 'correct');
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
                setDisabled(card, true);
                return;
            }
            form.addEventListener('submit', function (e) {
                e.preventDefault();
                submit(card);
            });
            // SQL: Enter makes a new line, Ctrl+Enter (Cmd+Enter) checks.
            const sql = card.querySelector('.task-sql');
            if (sql && window.CodeInput) CodeInput.attach(sql);
            if (sql) sql.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
                    e.preventDefault();
                    submit(card);
                }
            });
            const del = card.querySelector('.task-delete');
            if (del) {
                if (card.dataset.type === 'submit') del.textContent = 'Abgabe löschen';
                else if (['answer', 'choice', 'number', 'reflection', 'measure', 'command', 'sql', 'cypher'].indexOf(card.dataset.type) < 0) del.textContent = 'Ergebnis löschen';
                del.addEventListener('click', function () { remove(card); });
            }
            loadOptions(card);
            const pick = card.querySelector('.task-pick');
            if (pick && card.dataset.preview === 'anaglyph') {
                pick.addEventListener('change', function () {
                    if (pick.value) previewPicked(card, pick);
                });
            }
        });
        if (page.loggedIn) {
            loadStates();
            watchPicks();
        }
        setupTeacherAnswers();
    }

    // For teachers, under each open task (reflection): the example answers
    // or the answers of the current group, or nothing. One choice for all
    // cards, stored with the account, so the next page opens the same way.
    const teacher = page.teacher;
    const groupAnswers = {};

    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    function teacherPanels() {
        return Array.from(document.querySelectorAll('.task-teacher'));
    }

    function viewLabel(view) {
        if (view === 'examples') return 'Beispielantworten';
        if (view === 'group') return 'Antworten aus ' + teacher.group;
        return 'Ausblenden';
    }

    function examplesHtml(card) {
        const examples = (teacher.examples || {})[cardId(card)] || [];
        if (examples.length === 0)
            return "<p class='task-teacher-none'>Für diese Aufgabe gibt es noch keine Beispielantworten.</p>";
        return "<ol class='task-teacher-list'>" + examples.map((html) =>
            "<li class='task-teacher-answer task-teacher-example'>" + html + '</li>').join('') + '</ol>';
    }

    function groupHtml(card) {
        const data = groupAnswers[teacher.group];
        if (!data) return "<p class='task-teacher-none'>Einen Moment …</p>";
        const answers = (data.answers || {})[cardId(card)] || [];
        let io = "<p class='task-teacher-count'>" + answers.length + ' von ' + data.members +
            ' in ' + escapeHtml(teacher.group) + (answers.length === 1 ? ' hat' : ' haben') + ' geantwortet.' +
            "<button type='button' class='btn btn-link btn-sm task-teacher-reload'><i class='bi bi-arrow-clockwise' aria-hidden='true'></i>Neu laden</button></p>";
        if (answers.length === 0) return io;
        io += "<ul class='task-teacher-list'>";
        answers.forEach((a) => {
            const day = new Date(a.updated * 1000).toLocaleDateString('de-DE', { day: 'numeric', month: 'numeric', year: 'numeric' });
            io += "<li class='task-teacher-answer'><div class='task-teacher-who'><strong>" + escapeHtml(a.name) + '</strong>' +
                "<span class='task-teacher-when'>" + day + '</span>' +
                (a.stale ? "<span class='task-teacher-stale' title='Die Aufgabe hat sich geändert, seit die Antwort gespeichert wurde.'>vor einer Änderung der Aufgabe</span>" : '') +
                "</div><div class='task-teacher-text'>" + escapeHtml(a.answer) + '</div></li>';
        });
        return io + '</ul>';
    }

    function loadGroupAnswers(fresh) {
        const group = teacher.group;
        if (groupAnswers[group] && !fresh) return;
        api_call('/api/task/group_answers', { activity: page.activity, group: group }, function (data) {
            if (!data.success) return;
            groupAnswers[group] = data;
            if (teacher.view === 'group' && teacher.group === group) showTeacherView();
        }, { no_please_wait: true });
    }

    function showTeacherView() {
        if (teacher.view === 'group') loadGroupAnswers(false);
        teacherPanels().forEach((panel) => {
            const card = panel.closest('.task-card');
            panel.querySelectorAll('.task-teacher-view').forEach((button) => {
                const on = button.dataset.view === teacher.view;
                button.classList.toggle('active', on);
                button.setAttribute('aria-pressed', on ? 'true' : 'false');
                if (button.dataset.view === 'group') button.textContent = viewLabel('group');
            });
            const select = panel.querySelector('.task-teacher-group');
            if (select) {
                select.hidden = teacher.view !== 'group';
                select.value = teacher.group;
            }
            const body = panel.querySelector('.task-teacher-body');
            body.hidden = teacher.view === 'off';
            if (teacher.view === 'examples') body.innerHTML = examplesHtml(card);
            else if (teacher.view === 'group') body.innerHTML = groupHtml(card);
            else body.innerHTML = '';
        });
    }

    function chooseTeacherView(view, group) {
        teacher.view = view;
        if (group) teacher.group = group;
        showTeacherView();
        const data = { view: view };
        if (view === 'group') data.group = teacher.group;
        api_call('/api/task/answer_view', data, function () {}, { no_please_wait: true });
    }

    function setupTeacherAnswers() {
        if (!teacher) return;
        const views = [];
        if (teacher.examples) views.push('examples');
        if (teacher.group) views.push('group');
        views.push('off');
        cards().filter((card) => card.dataset.type === 'reflection').forEach((card) => {
            const panel = document.createElement('div');
            panel.className = 'task-teacher';
            let bar = "<div class='task-teacher-bar'><span class='task-teacher-label'><i class='bi bi-person-video3' aria-hidden='true'></i>Für Lehrkräfte</span>" +
                "<div class='task-teacher-views' role='group' aria-label='Unter der Aufgabe zeigen'>";
            views.forEach((view) => {
                bar += "<button type='button' class='btn btn-sm btn-outline-secondary task-teacher-view' data-view='" + view + "'>" + escapeHtml(viewLabel(view)) + '</button>';
            });
            bar += '</div>';
            if (teacher.groups.length > 1) {
                bar += "<select class='form-select form-select-sm task-teacher-group' aria-label='Gruppe'>" +
                    teacher.groups.map((g) => "<option value='" + escapeHtml(g.name) + "'>" + escapeHtml(g.name) + ' (' + g.size + ')</option>').join('') +
                    '</select>';
            }
            panel.innerHTML = bar + "</div><div class='task-teacher-body' aria-live='polite'></div>";
            const note = card.querySelector('.task-login-note');
            card.insertBefore(panel, note);
        });
        showTeacherView();
    }

    document.addEventListener('click', function (e) {
        const button = e.target.closest('.task-teacher-view');
        if (button) chooseTeacherView(button.dataset.view);
        if (e.target.closest('.task-teacher-reload')) loadGroupAnswers(true);
    });

    document.addEventListener('change', function (e) {
        if (e.target.classList.contains('task-teacher-group')) chooseTeacherView('group', e.target.value);
    });

    // »Details« and »Lösung« under a result: open below on request.
    document.addEventListener('click', function (e) {
        const toggle = e.target.closest('.task-more-toggle');
        if (!toggle) return;
        const panel = document.getElementById(toggle.getAttribute('aria-controls'));
        if (!panel) return;
        const open = toggle.getAttribute('aria-expanded') !== 'true';
        toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
        panel.hidden = !open;
    });

    // (i) on a card: what the check reads, shown on request. Delegated, so
    // it works on every card, also those whose form is set up later.
    document.addEventListener('click', function (e) {
        const info = e.target.closest('.task-reads-toggle');
        if (!info) return;
        const reads = document.getElementById(info.getAttribute('aria-controls'));
        if (!reads) return;
        const open = info.getAttribute('aria-expanded') !== 'true';
        info.setAttribute('aria-expanded', open ? 'true' : 'false');
        reads.hidden = !open;
    });

    if (document.readyState === 'loading')
        document.addEventListener('DOMContentLoaded', setup);
    else
        setup();
})();
