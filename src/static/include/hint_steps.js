// Tips in steps (HintSteps in src/ruby/include/hint_steps.rb,
// docs/didaktik.md). At the start only the first tip is there. Opening a
// tip brings the title of the next one, but it opens only data-delay
// seconds after the one before was opened, so there is time to think about
// each tip instead of clicking through to the last. The waiting time shows
// only to someone who tries to open the next step too early.
//
// What has been opened is kept in this browser only (localStorage), so a
// reload does not start the waiting again. Nothing is sent to the server: no
// teacher sees who needed which tip. The state is kept per account
// (data-hint-owner on body, a hash): on a shared school computer the next
// person finds no tips of the one before already open.
//
// The tips of a »Jetzt bist du dran« box are followed by the page's code
// (details.solution, SolutionBlocks). It is the last step and waits like a
// tip. Has the box a check of the student's own code (data-check='task-id'),
// solving that task opens the way to it at once.
(function () {
    const PREFIX = 'hint-steps:';

    function load(key) {
        try {
            const value = JSON.parse(window.localStorage.getItem(key));
            if (value && Number.isInteger(value.opened)) return value;
        } catch (e) { /* no storage here: start fresh */ }
        return null;
    }

    function save(key, value) {
        try {
            window.localStorage.setItem(key, JSON.stringify(value));
        } catch (e) { /* no storage here: the steps still work until a reload */ }
    }

    function clock(seconds) {
        return Math.floor(seconds / 60) + ':' + String(seconds % 60).padStart(2, '0');
    }

    // The task cards of the check under a box, one per language.
    function checkCards(box) {
        const own = box.previousElementSibling;
        const id = own && own.matches('.hint.own') ? own.dataset.check : null;
        if (!id) return [];
        return Array.from(document.querySelectorAll('.task-card[data-task]'))
            .filter((card) => card.dataset.task.split('/').pop() === id);
    }

    function setup(box, index) {
        const steps = Array.from(box.children).filter((el) => el.matches('details.hint-step'));
        const card = box.closest('[data-task]');
        const solution = !card && box.nextElementSibling && box.nextElementSibling.matches('details.solution')
            ? box.nextElementSibling : null;
        if (solution) steps.push(solution);
        if (steps.length < 2) return;
        const delay = (parseInt(box.dataset.delay, 10) || 60) * 1000;
        const owner = document.body.dataset.hintOwner || 'gast';
        const key = PREFIX + owner + ':' + window.location.pathname + '#' + (card ? card.dataset.task : 'box-' + index);
        // opened: how many steps have been opened, always the first ones;
        // at: when the last of them was opened
        const saved = load(key);
        const state = { opened: Math.min(Math.max(saved ? saved.opened : 0, 0), steps.length), at: saved ? saved.at || 0 : 0 };

        const wait = document.createElement('p');
        wait.className = 'hint-wait';
        wait.setAttribute('aria-live', 'polite');
        wait.hidden = true;
        let timer = null;

        function left() {
            return state.at + delay - Date.now();
        }

        function stopWaiting() {
            window.clearInterval(timer);
            timer = null;
            wait.hidden = true;
        }

        // The next step was asked for too early: say how long it takes.
        function tick() {
            const step = steps[state.opened];
            const ms = left();
            if (!step || ms <= 0) {
                stopWaiting();
                if (step) step.classList.add('hint-step-new');
                return;
            }
            const what = step === solution ? 'Die Lösung' : 'Tipp ' + (state.opened + 1);
            const about = state.opened === 1 ? 'den Tipp' : 'Tipp ' + state.opened;
            wait.textContent = what + ' kannst du in ' + clock(Math.ceil(ms / 1000)) +
                ' aufklappen. Denk so lange über ' + about + ' nach und probier ihn aus.';
            wait.hidden = false;
        }

        function render() {
            // the opened steps and the title of the next one
            steps.forEach((step, i) => { step.hidden = i > state.opened; });
            stopWaiting();
        }

        steps.forEach((step, i) => {
            step.querySelector('summary').addEventListener('click', (event) => {
                if (step.open || i < state.opened || i > state.opened || i === 0 || left() <= 0) return;
                event.preventDefault();
                step.insertAdjacentElement('afterend', wait);
                window.clearInterval(timer);
                timer = window.setInterval(tick, 1000);
                tick();
            });
            step.addEventListener('toggle', () => {
                if (!step.open) return;
                step.classList.remove('hint-step-new');
                if (i === state.opened) {
                    state.opened = i + 1;
                    state.at = Date.now();
                    save(key, state);
                    render();
                }
            });
        });

        // Whoever has written the code and solved its check may compare it
        // with the page's at once.
        const cards = solution ? checkCards(box) : [];
        function unlockIfSolved() {
            if (state.opened >= steps.length - 1 && left() <= 0) return;
            if (!cards.some((c) => c.dataset.status === 'correct')) return;
            state.opened = Math.max(state.opened, steps.length - 1);
            state.at = 0;
            save(key, state);
            render();
            solution.classList.add('hint-step-new');
        }
        if (cards.length > 0) {
            const observer = new MutationObserver(unlockIfSolved);
            cards.forEach((c) => observer.observe(c, { attributes: true, attributeFilter: ['data-status'] }));
        }

        render();
        unlockIfSolved();
    }

    function start() {
        document.querySelectorAll('.hint-steps').forEach(setup);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
