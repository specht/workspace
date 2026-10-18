// The Workspace may be installed manually from the browser menu,
// but don't interrupt students with an automatic install prompt.
window.addEventListener('beforeinstallprompt', function(event) {
    event.preventDefault();
});

function show_error_message(message) {
    var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('bg-light text-danger').html(message);
    $('.api_messages').empty();
    let button = $("<button class='text-stone-400 btn pull-right form-control' style='width: unset; margin: 8px;' ><i class='bi bi-x-lg'></i></button>");
    $('.api_messages').append(button).append(div).show();
    button.on('click', function(e) { e.preventDefault(); $('.api_messages').hide(); });
}

// In-page confirmation instead of the browser's confirm(): the page's own
// modal (#__template_modal, showTemplateModal in the page template).
// options: { title, text, confirm, icon, danger }
function confirm_dialog(options, on_confirm) {
    const esc = (t) => $('<div>').text(t || '').html();
    if (typeof showTemplateModal !== 'function' || $('#__template_modal').length === 0) {
        if (window.confirm(options.text)) on_confirm();
        return;
    }
    // A question needs a small dialog; the shared modal is large by default.
    const dialog = $('#__template_modal .modal-dialog');
    if (dialog.hasClass('modal-lg')) {
        dialog.removeClass('modal-lg');
        $('#__template_modal').one('hidden.bs.modal', function () { dialog.addClass('modal-lg'); });
    }
    showTemplateModal(esc(options.title), `<p class='mb-0 text-start'>${esc(options.text)}</p>`,
        `<i class='bi ${options.icon || 'bi-check-lg'}'></i>${esc(options.confirm || 'OK')}`,
        options.danger ? 'btn-danger' : 'btn-primary',
        "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary',
        function () { on_confirm(); });
}

// In-page question with one input instead of the browser's prompt().
// options: { title, label, value, placeholder, confirm, icon, type, suggest }
// (suggest: as fetch for attach_suggestions).
// on_ok(value) gets the trimmed text; returning false keeps the dialog open.
function input_dialog(options, on_ok) {
    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    const dialog = $('#__template_modal .modal-dialog');
    if (dialog.hasClass('modal-lg')) {
        dialog.removeClass('modal-lg');
        $('#__template_modal').one('hidden.bs.modal', function () { dialog.addClass('modal-lg'); });
    }
    const body = `<label class='form-label' for='ti_input_dialog'>${esc(options.label)}</label>`
        + `<input id='ti_input_dialog' class='form-control' type='${options.type || 'text'}' autocomplete='off' placeholder='${esc(options.placeholder)}' value='${esc(options.value)}'>`;
    const submit = function () {
        const value = $('#ti_input_dialog').val().trim();
        if (!value) return false;
        return on_ok(value);
    };
    showTemplateModal(esc(options.title), body,
        `<i class='bi ${options.icon || 'bi-check-lg'}'></i>${esc(options.confirm || 'OK')}`, 'btn-primary',
        "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary', submit);
    $('#__template_modal').one('shown.bs.modal', function () { $('#ti_input_dialog').trigger('focus').trigger('select'); });
    // Suggestions first, so that Enter on a suggestion picks it instead of
    // confirming the dialog.
    if (options.suggest) attach_suggestions($('#ti_input_dialog'), options.suggest, function (person) { $('#ti_input_dialog').val(person.email); });
    $('#ti_input_dialog').on('keydown', function (e) {
        if (e.key === 'Enter') { e.preventDefault(); $('#__template_modal .modal-footer .btn').first().trigger('click'); }
    });
}

// Suggestions under an input while typing (people by name or address).
// fetch(q, show) calls show([{ email, name }]); on_pick(person) gets the
// chosen one. Arrow keys move, Enter picks, Escape closes.
function attach_suggestions(input, fetch, on_pick) {
    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    const wrap = $("<div class='position-relative'></div>");
    input.after(wrap);
    wrap.append(input);
    const menu = $("<ul class='dropdown-menu suggest-menu' role='listbox'></ul>");
    wrap.append(menu);
    input.attr({ autocomplete: 'off', role: 'combobox', 'aria-expanded': 'false', 'aria-autocomplete': 'list' });
    let people = [], active = -1, timer = null, asked = '';
    const close = function () { menu.removeClass('show').empty(); input.attr('aria-expanded', 'false'); people = []; active = -1; };
    const mark = function () {
        menu.find('.dropdown-item').removeClass('active').eq(active).addClass('active');
    };
    const pick = function (i) {
        const person = people[i];
        close();
        if (person) on_pick(person);
    };
    const show = function (list) {
        people = list || [];
        active = -1;
        menu.empty();
        if (!people.length) { close(); return; }
        people.forEach(function (p, i) {
            const item = $(`<li><button type='button' class='dropdown-item' role='option'>${p.name ? `${esc(p.name)} <span class='text-body-secondary small'>${esc(p.email)}</span>` : esc(p.email)}</button></li>`);
            item.find('button').on('mousedown', function (e) { e.preventDefault(); pick(i); });
            menu.append(item);
        });
        menu.addClass('show');
        input.attr('aria-expanded', 'true');
    };
    input.on('input', function () {
        clearTimeout(timer);
        const q = input.val().trim();
        if (q.length < 2) { close(); return; }
        timer = setTimeout(function () { asked = q; fetch(q, function (list) { if (asked === q) show(list); }); }, 200);
    });
    input.on('keydown', function (e) {
        if (!menu.hasClass('show')) return;
        if (e.key === 'ArrowDown') { e.preventDefault(); active = Math.min(active + 1, people.length - 1); mark(); }
        else if (e.key === 'ArrowUp') { e.preventDefault(); active = Math.max(active - 1, 0); mark(); }
        else if (e.key === 'Enter' && active >= 0) { e.preventDefault(); e.stopImmediatePropagation(); pick(active); }
        else if (e.key === 'Escape') { e.preventDefault(); e.stopPropagation(); close(); }
    });
    input.on('blur', () => setTimeout(close, 150));
}

function show_success_message(message) {
    var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('bg-light text-success').html(message);
    $('.api_messages').empty();
    $('.api_messages').append(div).show();
}

function api_call(url, data, callback, options) {
    if (typeof (options) === 'undefined')
        options = {};

    if (typeof (window.please_wait_timeout) !== 'undefined')
        clearTimeout(window.please_wait_timeout);

    if (options.no_please_wait !== true) {
        // show 'please wait' message after 500 ms
        (function () {
            window.please_wait_timeout = setTimeout(function () {
                var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('text-muted').html("<i class='fa fa-cog bi-spin'></i>&nbsp;&nbsp;Einen Moment bitte...");
                $('.api_messages').empty().show();
                $('.api_messages').append(div);
            }, 500);
        })();
    }

    if (typeof(data) !== 'string')
        data = JSON.stringify(data);

    let conf = {
        url: url,
        data: data,
        contentType: 'application/json',
        dataType: 'json',
    };
    if (options.dataType)
        conf.dataType = options.dataType;
    if (options.contentType)
        conf.contentType = options.contentType;

    if (typeof (options.headers) !== 'undefined') {
        conf.beforeSend = function (xhr) {
            for (let key in options.headers)
                xhr.setRequestHeader(key, options.headers[key]);
        };
    }
    let jqxhr = null;
    if (options.method === 'GET')
        jqxhr = jQuery.get(conf);
    else
        jqxhr = jQuery.post(conf);

    jqxhr.done(function (data) {
        clearTimeout(window.please_wait_timeout);
        $('.api_messages').empty().hide();
        if (typeof (callback) !== 'undefined') {
            if (options.method !== 'GET')
                data.success = true;
            callback(data);
        }
    });

    jqxhr.fail(function (http) {
        clearTimeout(window.please_wait_timeout);
        $('.api_messages').empty();
        show_error_message('Bei der Bearbeitung der Anfrage ist ein Fehler aufgetreten.');
        if (typeof (callback) !== 'undefined') {
            var error_message = 'unknown_error';
            try {
                error_message = JSON.parse(http.responseText)['error'];
            } catch (err) {
            }
            console.log(error_message);
            callback({ success: false, error: error_message });
        }
    });
}

function show_workspace_reset_modal(email, success_callback) {
    const escaped_email = $('<div>').text(email).html();
    const content = `
        <div class='alert alert-danger' role='alert'>
            <strong>Dieser Vorgang kann nicht rückgängig gemacht werden.</strong>
            <div class='mt-2'>Alle Dateien im Workspace, deine Workspace-Einstellungen und alle installierten VS Code-Erweiterungen werden dauerhaft gelöscht.</div>
        </div>
        <p>Deine Daten in MySQL und Neo4j werden dabei nicht gelöscht.</p>
        <div id='div_reset_workspace_confirmation'>
            <label class='form-label' for='ti_reset_workspace_confirmation'>
                Gib zur Bestätigung exakt deine E-Mail-Adresse <code>${escaped_email}</code> ein:
            </label>
            <input id='ti_reset_workspace_confirmation' class='form-control' type='email' inputmode='email' autocomplete='off' spellcheck='false'>
        </div>
        <div id='div_reset_workspace_please_wait' style='display: none; margin-top: 1em;'><i class='fa fa-cog bi-spin'></i>Moment, dein Workspace wird auf den Ursprungszustand zurückgesetzt…</div>
        <div id='div_reset_workspace_success' style='display: none; margin-top: 1em;'><i class='text-success bi bi-check-lg'></i>&nbsp;Dein Workspace wurde auf den Ursprungszustand zurückgesetzt.</div>
    `;
    let submitting = false;

    showTemplateModal(
        'Workspace endgültig zurücksetzen',
        content,
        "<i class='bi bi-trash-fill'></i>Workspace endgültig zurücksetzen",
        'btn-danger',
        "<i class='bi bi-x-lg'></i>Abbrechen",
        'btn-secondary',
        function() {
            const input = $('#ti_reset_workspace_confirmation');
            const confirm_button = $('#__template_modal .modal-footer .btn').eq(0);
            const cancel_button = $('#__template_modal .modal-footer .btn').eq(1);

            if (submitting || input.val() !== email) return false;

            submitting = true;
            input.prop('disabled', true);
            confirm_button.prop('disabled', true);
            cancel_button.prop('disabled', true);
            $('#div_reset_workspace_please_wait').slideDown();

            api_call('/api/reset_server', {confirmation: input.val()}, function(data) {
                $('#div_reset_workspace_please_wait').hide();

                if (!data.success) {
                    submitting = false;
                    input.prop('disabled', false);
                    cancel_button.prop('disabled', false);
                    confirm_button.prop('disabled', input.val() !== email);
                    input.trigger('focus');
                    return;
                }

                $('#div_reset_workspace_confirmation').hide();
                $('#div_reset_workspace_success').show();
                confirm_button.hide();
                cancel_button
                    .prop('disabled', false)
                    .html("<i class='bi bi-x-lg'></i>Schließen");

                if (typeof success_callback === 'function') {
                    success_callback(data);
                }
            }, {no_please_wait: true});

            return false;
        }
    );

    const input = $('#ti_reset_workspace_confirmation');
    const confirm_button = $('#__template_modal .modal-footer .btn').eq(0);
    confirm_button.prop('disabled', true);
    input.on('input', function() {
        confirm_button.prop('disabled', submitting || input.val() !== email);
    });
    input.trigger('focus');
}

function bytes_to_str(ai_Size) {
    if (ai_Size < 1024) {
        return `${ai_Size} B`;
    } else if (ai_Size < 1024 * 1024) {
        return `${(ai_Size / 1024).toFixed(1)} kB`;
    } else if (ai_Size < 1024 * 1024 * 1024) {
        return `${(ai_Size / 1024 / 1024).toFixed(1)} MB`;
    } else if (ai_Size < 1024 * 1024 * 1024 * 1024) {
        return `${(ai_Size / 1024 / 1024 / 1024).toFixed(1)} GB`;
    }
    return `${(ai_Size / 1024 / 1024 / 1024 / 1024).toFixed(1)} TB`;
}