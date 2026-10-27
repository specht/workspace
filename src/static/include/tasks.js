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
        showAfterTasks();
    }

    // What gives away a task's answer (AfterTask, <!-- after: id -->) shows
    // after a prediction once it has an answer, right or wrong (only the
    // first counts, then comes why); after any other task, and after a Kata,
    // where the story goes on, once it is solved: otherwise a wrong answer
    // would show the right one. Without a card for the task on the page it
    // shows at once. Whether it was hidden at all is up to the server
    // (task_page_setup): for everyone who can answer.
    const ANSWERED = ['correct', 'wrong', 'sent', 'stale'];
    const SOLVED = ['correct', 'sent'];
    const KATA = ['function', 'pip', 'tests'];
    function showAfterTasks() {
        document.querySelectorAll('.after-task[data-after]').forEach((box) => {
            const mine = cards().filter((card) => cardId(card) === box.dataset.after);
            const kata = mine.some((card) => KATA.includes(card.dataset.type));
            const prediction = mine.some((card) => card.dataset.prediction === '1');
            const answered = mine.length === 0 || mine.some((card) => (prediction ? ANSWERED : SOLVED).includes(card.dataset.status));
            box.classList.toggle('after-task-shown', answered || box.dataset.revealed === 'yes');
            const wait = box.querySelector('.after-task-wait');
            const text = kata ? 'Hier geht die Geschichte weiter, sobald du die Kata oben gelöst hast.'
                : prediction ? null : 'Hier geht es weiter, sobald du die Aufgabe oben gelöst hast.';
            if (wait && text && wait.dataset.text !== text) {
                wait.textContent = text;
                wait.dataset.text = text;
            }
            // a teacher sees the page as the class does, and opens it here without answering
            if (page.revealAfter && wait && !wait.querySelector('.after-task-reveal')) {
                const button = document.createElement('button');
                button.type = 'button';
                button.className = 'btn btn-link btn-sm after-task-reveal';
                button.innerHTML = "<i class='bi bi-eye' aria-hidden='true'></i> Für Lehrkräfte: jetzt zeigen";
                button.addEventListener('click', () => {
                    box.dataset.revealed = 'yes';
                    box.classList.add('after-task-shown');
                });
                wait.append(' ', button);
            }
        });
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
        // The frame stays while the scene is drawn again: an empty box (never
        // an <img> without a picture, which shows as a broken image), or the
        // last picture until the new one is there.
        let text = box.querySelector('.task-scene-preview-caption');
        let frame = box.querySelector('.task-scene-preview-picture');
        let status = box.querySelector('.task-scene-preview-status');
        if (!text) {
            box.innerHTML = '';
            text = document.createElement('p');
            text.className = 'task-scene-preview-caption';
            frame = document.createElement('div');
            frame.className = 'task-scene-preview-picture';
            status = document.createElement('p');
            status.className = 'task-scene-preview-status';
            box.append(text, frame, status);
        }
        text.textContent = caption;
        frame.hidden = false;
        frame.classList.add('drawing');
        status.textContent = 'Die Szene wird gezeichnet …';
        const promise = window.AnaglyphPreview.picture(scene);
        promise.then((result) => {
            if (box.dataset.token !== token) return;
            frame.classList.remove('drawing');
            frame.innerHTML = '';
            if (result.picture) {
                const img = document.createElement('img');
                img.alt = 'Die Szene, wie die Galerie sie zeigt.';
                img.src = result.picture;
                frame.appendChild(img);
            } else {
                frame.hidden = true;
            }
            status.textContent = result.errors ? 'Der Viewer meldet Fehler in dieser Szene: ' + result.errors
                : result.picture ? '' : 'Die Szene ließ sich nicht zeichnen.';
        }).catch((error) => {
            if (box.dataset.token !== token) return;
            frame.classList.remove('drawing');
            frame.innerHTML = '';
            frame.hidden = true;
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
        if (input && KATA.includes(card.dataset.type)) {
            // A Kata keeps its code per language ({"python": "…"}); without
            // code in this card's language it shows the starter.
            let codes = {};
            try { codes = JSON.parse(answer || '{}') || {}; } catch (e) { codes = {}; }
            const code = codes[input.dataset.codeLanguage];
            input.value = typeof code === 'string' ? code : (input.dataset.starter || '');
        } else if (input) {
            input.value = answer || '';
        }
        if (input && input.codeInputUpdate) input.codeInputUpdate();
        if (card.pipButtons) card.pipButtons.load(input.value);
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
    function setBusy(card, busy, which) {
        setDisabled(card, busy);
        if (!busy && card.pipButtons) card.pipButtons.refresh();
        const button = card.querySelector(which || '.task-submit');
        if (!button) return;
        if (busy && !button.classList.contains('is-busy')) {
            button.classList.add('is-busy');
            button.setAttribute('aria-busy', 'true');
            const spinner = document.createElement('span');
            spinner.className = 'task-submit-busy';
            spinner.innerHTML = "<span class='spinner-border spinner-border-sm' aria-hidden='true'></span>" +
                "<span class='visually-hidden'>" + (card.dataset.type === 'submit' ? 'Wird abgegeben …'
                    : which === '.task-run' || KATA.includes(card.dataset.type) ? 'Läuft …' : 'Wird geprüft …') + '</span>';
            button.appendChild(spinner);
        } else if (!busy && button.classList.contains('is-busy')) {
            button.classList.remove('is-busy');
            button.removeAttribute('aria-busy');
            button.querySelectorAll('.task-submit-busy').forEach((el) => el.remove());
        }
    }

    function submit(card) {
        const answer = readAnswer(card);
        if (KATA.includes(card.dataset.type)) {
            kata(card, 'check', answer);
            return;
        }
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

    // A Kata (KataCheck): the run is queued on the server and the card asks
    // again every half second until it is done, showing its place in the
    // queue meanwhile. kind: check (»Prüfen«) or run (»Ausführen«).
    const KATA_POLL_MS = 500;

    function kata(card, kind, answer) {
        const which = kind === 'run' ? '.task-run' : '.task-submit';
        const queue = card.querySelector('.task-queue');
        setBusy(card, true, which);
        if (card.pipPlayer) card.pipPlayer.lock(true);
        const done = function (data) {
            setBusy(card, false, which);
            if (card.pipPlayer) card.pipPlayer.lock(false);
            if (queue) queue.textContent = '';
            // back to writing, with a mouse (on a touch screen the keyboard
            // would cover the map); without moving the page
            const editor = card.querySelector('.task-code');
            if (editor && !editor.readOnly && window.matchMedia('(pointer: fine)').matches) editor.focus({ preventScroll: true });
            if (!data || !data.success) {
                showFeedback(card, 'unavailable', '<p>Das hat gerade nicht geklappt. Versuch es gleich noch einmal.</p>');
                return;
            }
            if (data.status === 'cancelled') return;
            if (kind === 'run') {
                showConsole(card, data);
                return;
            }
            // a function Kata: what its own program printed, above the result
            if (data.ran) showConsole(card, { status: data.ran === 'ran' ? 'ran' : 'error', output: data.output });
            const result = function () {
                // hidden while a Pip run plays (below)
                card.querySelector('.task-feedback').hidden = false;
                showFeedback(card, data.status, data.message, data.solution, data.read);
                if (data.state) {
                    setBadge(card, data.state.status, data.status === 'correct' && card.dataset.status !== 'correct');
                    card.querySelector('.task-delete').hidden = false;
                    showHandIn(card, data.state);
                }
            };
            // Pip: the run plays first, the result comes when it is over (or
            // stopped, or skipped to its end); what the plan printed below
            // (a clicked plan has just been watched: the result at once)
            if (data.replay && card.pipPlayer && !card.pipButtons) {
                card.querySelector('.task-feedback').hidden = true;
                if (data.replay.output) showConsole(card, { status: 'ran', output: data.replay.output });
                card.pipPlayer.load(data.replay, true, result);
                return;
            }
            result();
        };
        const follow = function (data) {
            if (!data || !data.success || data.done) {
                done(data);
                return;
            }
            if (queue) {
                queue.textContent = data.running ? 'Läuft …'
                    : data.ahead > 0 ? 'Noch ' + data.ahead + ' vor dir' : 'Gleich dran …';
            }
            setTimeout(function () {
                api_call('/api/kata/poll', { ticket: data.ticket || card.dataset.kataTicket }, follow, { no_please_wait: true });
            }, KATA_POLL_MS);
            if (data.ticket) card.dataset.kataTicket = data.ticket;
        };
        const request = { task: card.dataset.task, answer: answer, language: card.dataset.language };
        // Pip: the map in the card's tab runs first
        if (card.pipPlayer) request.map = String(card.pipPlayer.selected);
        api_call('/api/kata/' + kind, request, follow, { no_please_wait: true });
    }

    // Einreichen (KataHandIns): under a solved Kata, a copy of the code for
    // the teacher to show in class; the student's own choice, taken back
    // at any time.
    function showHandIn(card, state) {
        const button = card.querySelector('.task-hand-in');
        if (!button) return;
        const handedIn = !!(state && state.handed_in);
        button.hidden = !(state && state.status === 'correct') && !handedIn;
        card.dataset.handedIn = handedIn ? '1' : '';
        button.textContent = handedIn ? 'Eingereicht · Zurücknehmen' : 'Einreichen';
    }

    function toggleHandIn(card) {
        const handedIn = card.dataset.handedIn === '1';
        api_call(handedIn ? '/api/kata/take_back' : '/api/kata/hand_in', { task: card.dataset.task, language: card.dataset.language }, function (data) {
            if (!data.success) {
                showFeedback(card, 'unavailable', '<p>Das hat gerade nicht geklappt. Versuch es gleich noch einmal.</p>');
                return;
            }
            showHandIn(card, { status: card.dataset.status, handed_in: data.handed_in });
        }, { no_please_wait: true });
    }

    // What »Ausführen« printed, as text, and a line when it did not end well.
    // Pip's run shows on the map; the console only when the plan printed
    // something or broke off.
    function showConsole(card, data) {
        const console_ = card.querySelector('.task-console');
        if (console_) {
            console_.hidden = card.dataset.type === 'pip' && !data.output && data.status === 'ran';
            console_.textContent = data.output || (data.status === 'ran' ? '(Dein Programm hat nichts ausgegeben.)' : '');
            console_.classList.toggle('task-console-error', data.status !== 'ran');
        }
        const box = card.querySelector('.task-feedback');
        if (data.message) showFeedback(card, data.status === 'invalid' ? 'invalid' : 'unavailable', data.message);
        else if (box && box.dataset.from === 'run') box.innerHTML = '';
        if (box) box.dataset.from = data.message ? 'run' : '';
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
            showHandIn(card, null);
            showAfterTasks();
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
                showHandIn(card, state);
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
                // a Kata's code in its editor, to read: colours and line
                // numbers as when logged in (code_input.js)
                const code = card.querySelector('.task-code');
                if (code && window.CodeInput) {
                    code.disabled = false;
                    code.readOnly = true;
                    CodeInput.attach(code);
                }
                // Pip's map is shown, without anything to play
                const map = card.querySelector('.pip-world');
                if (map && window.PipWorld) {
                    const box = document.createElement('div');
                    box.className = 'pip-map-box';
                    map.appendChild(box);
                    const layout = JSON.parse(map.dataset.layout);
                    new PipWorld.PipView(box, layout, JSON.parse(map.dataset.start),
                        { width: layout.width, height: layout.height, columns: PipWorld.columns(box) });
                }
                return;
            }
            form.addEventListener('submit', function (e) {
                e.preventDefault();
                submit(card);
            });
            // SQL and a Kata's code: Enter makes a new line, Ctrl+Enter
            // (Cmd+Enter) checks.
            const sql = card.querySelector('.task-sql, .task-code');
            if (sql && window.CodeInput) CodeInput.attach(sql);
            const run = card.querySelector('.task-run');
            if (run) run.addEventListener('click', function () { kata(card, 'run', readAnswer(card)); });
            const handIn = card.querySelector('.task-hand-in');
            if (handIn) handIn.addEventListener('click', function () { toggleHandIn(card); });
            // Pip's map (pip_world.js): the player under it marks each step's line
            const world = card.querySelector('.pip-world');
            if (world && window.PipWorld) {
                card.pipPlayer = new PipWorld.PipPlayer(world, JSON.parse(world.dataset.layout), JSON.parse(world.dataset.start), JSON.parse(world.dataset.maps || 'null'), function (line) {
                    if (sql && sql.codeInputMark) sql.codeInputMark(line);
                });
                // the first chapter: the plan is clicked and only watched;
                // at the lamp it goes to the check by itself
                const buttons = card.querySelector('.pip-buttons');
                if (buttons) {
                    card.pipButtons = new PipWorld.PipButtons(buttons, card.pipPlayer, JSON.parse(world.dataset.layout), JSON.parse(world.dataset.start), function (plan) {
                        sql.value = plan;
                        if (sql.codeInputUpdate) sql.codeInputUpdate();
                    }, function () { submit(card); });
                } else if (sql) {
                    sql.addEventListener('input', function () { card.pipPlayer.forget(); });
                }
            }
            if (sql) sql.addEventListener('keydown', function (e) {
                if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
                    e.preventDefault();
                    submit(card);
                }
            });
            const del = card.querySelector('.task-delete');
            if (del) {
                if (card.dataset.type === 'submit') del.textContent = 'Abgabe löschen';
                else if (['answer', 'choice', 'number', 'reflection', 'measure', 'command', 'sql', 'cypher', 'function', 'pip', 'tests'].indexOf(card.dataset.type) < 0) del.textContent = 'Ergebnis löschen';
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
        showAfterTasks();
        if (page.loggedIn) {
            loadStates();
            watchPicks();
        }
        setupTeacherAnswers();
        setupTeacherSolutions();
    }

    // For teachers, beside a Kata's buttons: a small button that puts the
    // solution into the editor, in the card's language, to run it or show
    // it in class.
    function setupTeacherSolutions() {
        const solutions = page.solutions;
        if (!solutions) return;
        cards().forEach((card) => {
            const own = solutions[cardId(card)];
            const editor = card.querySelector('.task-code');
            const actions = card.querySelector('.task-actions');
            if (!own || !editor || !actions) return;
            const code = own[card.dataset.language] || Object.values(own)[0];
            const button = document.createElement('button');
            button.type = 'button';
            button.className = 'task-solution-fill';
            button.title = 'Für Lehrkräfte: die Lösung in den Code einsetzen';
            button.innerHTML = "<i class='bi bi-box-arrow-in-down' aria-hidden='true'></i>Lösung einsetzen";
            button.addEventListener('click', () => {
                editor.value = code;
                editor.dispatchEvent(new Event('input', { bubbles: true }));
                editor.focus({ preventScroll: true });
            });
            actions.insertBefore(button, actions.querySelector('.task-reads-toggle'));
        });
    }

    // For teachers, under each open task (reflection): the example answers
    // or the answers of a group, or nothing. One choice for all cards, stored
    // with the account, so the next page opens the same way.
    //
    // Example answers only on a card that has some. A button for every group
    // that has this activity pinned (and the current group), each with how
    // many of the group have answered this task, »12/25«; other groups in a
    // select. The counts and the answers shown are fetched again every
    // REFRESH_SECONDS while the page is visible, so they keep up with a
    // class that is writing.
    const teacher = page.teacher;
    const groupAnswers = {};
    const REFRESH_SECONDS = 30;

    function escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    function teacherPanels() {
        return Array.from(document.querySelectorAll('.task-teacher'));
    }

    function hasExamples(card) {
        return ((teacher.examples || {})[cardId(card)] || []).length > 0;
    }

    // The groups with a button: those that have the activity pinned, in the
    // order of the select, and the current group.
    function buttonGroups() {
        const pinned = teacher.pinned || [];
        const list = teacher.groups.map((g) => g.name).filter((name) => pinned.includes(name));
        if (teacher.group && !list.includes(teacher.group)) list.push(teacher.group);
        return list;
    }

    // How many of the group have answered this card: [answered, members], or null.
    function groupCount(card, group) {
        const data = groupAnswers[group];
        if (!data) return null;
        return [((data.answers || {})[cardId(card)] || []).length, data.members];
    }

    function examplesHtml(card) {
        const examples = (teacher.examples || {})[cardId(card)] || [];
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

    function loadGroupAnswers(group, fresh) {
        if (groupAnswers[group] && !fresh) return;
        api_call('/api/task/group_answers', { activity: page.activity, group: group }, function (data) {
            if (!data.success) return;
            groupAnswers[group] = data;
            showTeacherView();
        }, { no_please_wait: true });
    }

    // The buttons of one card: example answers if it has some, a button per
    // group, »Ausblenden«, and a select for the other groups.
    function renderBar(panel, card) {
        const views = panel.querySelector('.task-teacher-views');
        let html = '';
        if (hasExamples(card))
            html += "<button type='button' class='btn btn-sm btn-outline-secondary task-teacher-view' data-view='examples'>Beispielantworten</button>";
        buttonGroups().forEach((group) => {
            html += "<button type='button' class='btn btn-sm btn-outline-secondary task-teacher-view' data-view='group' data-group='" +
                escapeHtml(group) + "'>Antworten aus " + escapeHtml(group) + "<span class='task-teacher-tally'></span></button>";
        });
        html += "<button type='button' class='btn btn-sm btn-outline-secondary task-teacher-view' data-view='off'>Ausblenden</button>";
        views.innerHTML = html;
        const others = teacher.groups.filter((g) => !buttonGroups().includes(g.name));
        const select = panel.querySelector('.task-teacher-group');
        if (others.length === 0) {
            if (select) select.remove();
            return;
        }
        const options = "<option value=''>Andere Gruppe …</option>" +
            others.map((g) => "<option value='" + escapeHtml(g.name) + "'>" + escapeHtml(g.name) + ' (' + g.size + ')</option>').join('');
        if (select) {
            select.innerHTML = options;
        } else {
            views.insertAdjacentHTML('afterend', "<select class='form-select form-select-sm task-teacher-group' aria-label='Andere Gruppe'>" + options + '</select>');
        }
    }

    function showTeacherView() {
        const groups = buttonGroups().join('\n');
        teacherPanels().forEach((panel) => {
            const card = panel.closest('.task-card');
            if (panel.dataset.groups !== groups) {
                renderBar(panel, card);
                panel.dataset.groups = groups;
            }
            // a card without examples shows nothing while the others show theirs
            const view = teacher.view === 'examples' && !hasExamples(card) ? 'off' : teacher.view;
            panel.querySelectorAll('.task-teacher-view').forEach((button) => {
                const on = button.dataset.view === view && (view !== 'group' || button.dataset.group === teacher.group);
                button.classList.toggle('active', on);
                button.setAttribute('aria-pressed', on ? 'true' : 'false');
                const tally = button.querySelector('.task-teacher-tally');
                if (tally) {
                    const count = groupCount(card, button.dataset.group);
                    tally.textContent = count ? count[0] + '/' + count[1] : '';
                    button.setAttribute('aria-label', 'Antworten aus ' + button.dataset.group +
                        (count ? ', ' + count[0] + ' von ' + count[1] + ' haben geantwortet' : ''));
                }
            });
            const body = panel.querySelector('.task-teacher-body');
            body.hidden = view === 'off';
            const html = view === 'examples' ? examplesHtml(card) : view === 'group' ? groupHtml(card) : '';
            // only when something changed, so a refresh does not jump
            if (body.dataset.html !== html) {
                body.innerHTML = html;
                body.dataset.html = html;
            }
        });
    }

    function chooseTeacherView(view, group) {
        teacher.view = view;
        if (group) teacher.group = group;
        if (view === 'group') loadGroupAnswers(teacher.group, false);
        showTeacherView();
        const data = { view: view };
        if (view === 'group') data.group = teacher.group;
        api_call('/api/task/answer_view', data, function () {}, { no_please_wait: true });
    }

    function refreshGroups() {
        if (document.visibilityState !== 'visible') return;
        buttonGroups().forEach((group) => loadGroupAnswers(group, true));
    }

    function setupTeacherAnswers() {
        if (!teacher) return;
        const reflections = cards().filter((card) => card.dataset.type === 'reflection');
        if (reflections.length === 0) return;
        reflections.forEach((card) => {
            const panel = document.createElement('div');
            panel.className = 'task-teacher';
            panel.innerHTML = "<div class='task-teacher-bar'><span class='task-teacher-label'><i class='bi bi-person-video3' aria-hidden='true'></i>Für Lehrkräfte</span>" +
                "<div class='task-teacher-views' role='group' aria-label='Unter der Aufgabe zeigen'></div></div>" +
                "<div class='task-teacher-body' aria-live='polite'></div>";
            const note = card.querySelector('.task-login-note');
            card.insertBefore(panel, note);
        });
        showTeacherView();
        if (teacher.groups.length > 0) {
            buttonGroups().forEach((group) => loadGroupAnswers(group, false));
            setInterval(refreshGroups, REFRESH_SECONDS * 1000);
            document.addEventListener('visibilitychange', refreshGroups);
        }
    }

    document.addEventListener('click', function (e) {
        const button = e.target.closest('.task-teacher-view');
        if (button) chooseTeacherView(button.dataset.view, button.dataset.group);
        if (e.target.closest('.task-teacher-reload')) loadGroupAnswers(teacher.group, true);
    });

    document.addEventListener('change', function (e) {
        if (e.target.classList.contains('task-teacher-group') && e.target.value) chooseTeacherView('group', e.target.value);
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
