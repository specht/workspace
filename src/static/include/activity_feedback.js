// Feedback of teachers on an activity (include/activity_feedback.rb): the
// card and the form in the tab »Fortschritt« of the class page, and the
// hint on the teacher's start page. Nothing opens by itself: the form
// only after a click or with #rueckmeldung in the address (the hint's link).
(function () {
    if (window.activityFeedbackLoaded) return;
    window.activityFeedbackLoaded = true;

    // After »Nicht jetzt« on the start page, no hint there for a week, in
    // this browser: one card at a time, not one after the other.
    const HOME_KEY = 'hackschule-feedback-home-hidden-until';
    const WEEK = 7 * 24 * 3600 * 1000;

    function dismiss(group, activity, mode, done) {
        api_call('/api/activity_feedback/dismiss', { group: group, activity: activity, mode: mode }, function (data) {
            if (data.success) done();
        }, { no_please_wait: true });
    }

    function setupHome() {
        const hint = document.querySelector('.feedback-home');
        if (!hint) return;
        let hiddenUntil = 0;
        try { hiddenUntil = parseInt(localStorage.getItem(HOME_KEY) || '0', 10); } catch (e) { hiddenUntil = 0; }
        if (hiddenUntil > Date.now()) return;
        hint.hidden = false;
        hint.querySelector('.feedback-home-close').addEventListener('click', function () {
            dismiss(hint.dataset.group, hint.dataset.activity, 'later', function () {
                try { localStorage.setItem(HOME_KEY, String(Date.now() + WEEK)); } catch (e) { /* only for this visit then */ }
                hint.remove();
            });
        });
    }

    // A radio of an optional question can be taken back: a click on the
    // chosen one clears it.
    function clearableRadios(form) {
        const radios = form.querySelectorAll("input[type='radio']");
        radios.forEach(function (r) { r.dataset.was = r.checked ? '1' : '0'; });
        radios.forEach(function (r) {
            r.addEventListener('click', function () {
                if (r.dataset.was === '1') {
                    r.checked = false;
                    r.dataset.was = '0';
                } else {
                    form.querySelectorAll("input[type='radio'][name='" + r.name + "']").forEach(function (o) { o.dataset.was = '0'; });
                    r.dataset.was = '1';
                }
            });
        });
    }

    function answersOf(form) {
        const value = function (name) {
            const checked = form.querySelector("input[name='" + name + "']:checked");
            return checked ? checked.value : null;
        };
        const text = function (name) { return form.querySelector("[name='" + name + "']").value.trim(); };
        return {
            overall: value('overall'),
            overall_text: text('overall_text'),
            duration: value('duration'),
            level: value('level'),
            languages: Array.from(form.querySelectorAll("input[name='languages']:checked")).map(function (c) { return c.value; }),
            languages_text: text('languages_text'),
            text: text('text'),
            contact: form.querySelector("input[name='contact']").checked,
        };
    }

    function answered(a) {
        return !!(a.overall || a.duration || a.level || a.overall_text || a.languages_text || a.text || a.languages.length > 0);
    }

    function setupClass() {
        const place = document.querySelector('.feedback-place');
        if (!place) return;
        const form = place.querySelector('.feedback-form');
        const card = place.querySelector('.feedback-card');
        const error = form.querySelector('.feedback-form-error');
        clearableRadios(form);
        // the note that an answer is missing goes as soon as there is one
        form.addEventListener('input', function () { if (answered(answersOf(form))) error.hidden = true; });
        form.addEventListener('click', function () { if (answered(answersOf(form))) error.hidden = true; });

        const open = function () {
            if (card) card.hidden = true;
            form.hidden = false;
            document.querySelectorAll('.feedback-open').forEach(function (b) { b.setAttribute('aria-expanded', 'true'); });
            form.scrollIntoView({ block: 'start', behavior: 'smooth' });
            const first = form.querySelector('input:not([disabled])');
            if (first) first.focus({ preventScroll: true });
        };
        const close = function () {
            form.hidden = true;
            if (card && card.isConnected) card.hidden = false;
            document.querySelectorAll('.feedback-open').forEach(function (b) { b.setAttribute('aria-expanded', 'false'); });
        };
        document.querySelectorAll('.feedback-open').forEach(function (b) {
            b.setAttribute('aria-expanded', 'false');
            b.addEventListener('click', function () { form.hidden ? open() : close(); });
        });
        form.querySelector('.feedback-cancel').addEventListener('click', close);
        if (location.hash === '#rueckmeldung') open();

        place.querySelectorAll('.feedback-dismiss').forEach(function (b) {
            b.addEventListener('click', function () {
                const mode = b.dataset.mode;
                dismiss(form.dataset.group, form.dataset.activity, mode, function () {
                    if (card) card.remove();
                    show_success_message(mode === 'never'
                        ? 'Zu dieser Aktivität und Gruppe kommt die Frage nicht mehr. Über »Rückmeldung geben« kannst du sie trotzdem geben.'
                        : 'Für 14 Tage ausgeblendet.');
                });
            });
        });

        form.addEventListener('submit', function (e) {
            e.preventDefault();
            const answers = answersOf(form);
            if (!answered(answers)) {
                error.hidden = false;
                return;
            }
            error.hidden = true;
            api_call('/api/activity_feedback/save', { group: form.dataset.group, activity: form.dataset.activity, answers: answers }, function (data) {
                if (!data.success) return;
                if (card) card.remove();
                form.hidden = true;
                place.dataset.state = 'answered';
                document.querySelectorAll('.class-toolbar .feedback-open').forEach(function (b) {
                    b.setAttribute('aria-expanded', 'false');
                    b.innerHTML = "<i class='bi bi-chat-square-text' aria-hidden='true'></i>Deine Rückmeldung";
                });
                const submit = form.querySelector("button[type='submit']");
                submit.innerHTML = "<i class='bi bi-send' aria-hidden='true'></i>Änderung speichern";
                show_success_message('Danke für deine Rückmeldung.');
            });
        });
    }

    // /rueckmeldungen: an activity's name, »Nur mit Text«, »Kontakt
    // erwünscht«. An answer shows when it passes both boxes, an activity
    // (its card and its section) when its name matches and one of its
    // answers shows.
    function setupAdmin() {
        const filters = document.querySelector('.fb-filters');
        if (!filters) return;
        const search = filters.querySelector('.fb-search');
        const onlyText = document.getElementById('fb-only-text');
        const onlyContact = document.getElementById('fb-only-contact');
        const none = document.querySelector('.fb-none-found');
        const apply = function () {
            const words = search.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
            const shown = {};
            document.querySelectorAll('.fb-activity').forEach(function (section) {
                const named = words.every(function (w) { return section.dataset.title.includes(w); });
                let any = false;
                section.querySelectorAll('.fb-entry').forEach(function (entry) {
                    const ok = named && (!onlyText.checked || entry.dataset.text === '1') && (!onlyContact.checked || entry.dataset.contact === '1');
                    entry.hidden = !ok;
                    if (ok) any = true;
                });
                section.hidden = !any;
                shown[section.id] = any;
            });
            let cards = 0;
            document.querySelectorAll('.fb-card').forEach(function (card) {
                const link = card.querySelector('.fb-to-texts');
                const id = link ? link.getAttribute('href').slice(1) : '';
                card.hidden = !shown[id];
                if (!card.hidden) cards++;
            });
            none.hidden = cards > 0;
        };
        search.addEventListener('input', apply);
        onlyText.addEventListener('change', apply);
        onlyContact.addEventListener('change', apply);
    }

    function setup() {
        setupHome();
        setupClass();
        setupAdmin();
    }
    if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', setup);
    else setup();
})();
