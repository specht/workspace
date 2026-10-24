// Tips in steps (HintSteps in src/ruby/include/hint_steps.rb,
// docs/didaktik.md). At the start only the first tip is there. The next one
// appears data-delay seconds after the one before was opened, so there is
// time to work with each tip instead of clicking through to the last. There
// is no countdown, and the titles do not say how many tips there are
// (»Tipp«, then »Nächster Tipp«): both draw the eye away from the tip.
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
// solving that task brings it at once.
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
        let timer = null;

        // The opened steps are there, and the next one once its time has come.
        function render() {
            window.clearTimeout(timer);
            timer = null;
            const left = state.at + delay - Date.now();
            const next = state.opened === 0 || left <= 0 ? state.opened : state.opened - 1;
            steps.forEach((step, i) => { step.hidden = i > next; });
            if (next < state.opened && state.opened < steps.length) {
                timer = window.setTimeout(() => {
                    render();
                    steps[state.opened].classList.add('hint-step-new');
                }, left);
            }
        }

        steps.forEach((step, i) => {
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
            if (!solution.hidden) return;
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
        if (cards.length > 0) unlockIfSolved();
    }

    function start() {
        document.querySelectorAll('.hint-steps').forEach(setup);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
