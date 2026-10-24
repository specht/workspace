// Tips in steps (HintSteps in src/ruby/include/hint_steps.rb,
// docs/didaktik.md). Only the first tip of a .hint-steps block is there at
// the start; the next one appears data-delay seconds after the one before
// was opened, so there is time to think about each tip instead of clicking
// through to the last. What has been opened is kept in this browser only
// (localStorage), so a reload does not start the waiting again. Nothing is
// sent to the server: no teacher sees who needed which tip.
(function () {
    const PREFIX = 'hint-steps:';

    function load(key) {
        try {
            const value = JSON.parse(window.localStorage.getItem(key));
            if (value && Number.isInteger(value.shown) && Number.isInteger(value.opened)) return value;
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

    function setup(box, index) {
        const steps = Array.from(box.children).filter((el) => el.matches('details.hint-step'));
        if (steps.length < 2) return;
        const delay = (parseInt(box.dataset.delay, 10) || 60) * 1000;
        const card = box.closest('[data-task]');
        const key = PREFIX + window.location.pathname + '#' + (card ? card.dataset.task : 'box-' + index);
        // shown: how many steps are there; opened: how many of them were
        // opened; at: when the last one was opened
        const state = load(key) || { shown: 1, opened: 0, at: 0 };
        state.shown = Math.min(Math.max(state.shown, 1), steps.length);
        state.opened = Math.min(state.opened, state.shown);

        const wait = document.createElement('p');
        wait.className = 'hint-wait';
        wait.setAttribute('aria-live', 'polite');
        wait.hidden = true;
        box.appendChild(wait);
        let timer = null;

        function tick() {
            const left = state.at + delay - Date.now();
            if (left <= 0) {
                window.clearInterval(timer);
                timer = null;
                state.shown += 1;
                save(key, state);
                render();
                steps[state.shown - 1].classList.add('hint-step-new');
                return;
            }
            wait.textContent = 'Tipp ' + (state.shown + 1) + ' kommt in ' + clock(Math.ceil(left / 1000)) +
                '. Denk so lange über diesen nach und probier ihn aus.';
            wait.hidden = false;
        }

        function render() {
            steps.forEach((step, i) => { step.hidden = i >= state.shown; });
            window.clearInterval(timer);
            timer = null;
            wait.hidden = true;
            // only once the last step there was opened does the pause begin
            if (state.shown >= steps.length || state.opened < state.shown) return;
            timer = window.setInterval(tick, 1000);
            tick();
        }

        steps.forEach((step, i) => {
            step.addEventListener('toggle', () => {
                if (!step.open) return;
                step.classList.remove('hint-step-new');
                if (i + 1 === state.shown && state.opened < state.shown) {
                    state.opened = state.shown;
                    state.at = Date.now();
                    save(key, state);
                    render();
                }
            });
        });
        render();
    }

    function start() {
        document.querySelectorAll('.hint-steps').forEach(setup);
    }

    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
    else start();
})();
