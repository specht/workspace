// The Workspace may be installed manually from the browser menu,
// but don't interrupt students with an automatic install prompt.
window.addEventListener('beforeinstallprompt', function(event) {
    event.preventDefault();
});

// A link to the login comes back to this page afterwards: /login?next=
// the path, the query, and the task card it stands in (»Melde dich an« on a
// card) or the page's anchor. Set as the link is about to be used, so links
// written later (tcp-console) get it too; the login page takes only a path
// on this site (Authentication.safe_next_path). Not from /qr-login: its
// secret was in the anchor and is gone, the page alone shows nothing.
function add_login_next(e) {
    const link = e.target && e.target.closest && e.target.closest('a[href="/login"], a[data-login-next]');
    if (!link || ['/login', '/qr-login'].includes(window.location.pathname)) return;
    const card = link.closest('.task-card');
    const hash = card && card.id ? '#' + card.id : window.location.hash;
    const next = window.location.pathname + window.location.search + hash;
    link.dataset.loginNext = '1';
    link.setAttribute('href', next === '/' ? '/login' : '/login?next=' + encodeURIComponent(next));
}
for (const type of ['mousedown', 'focusin', 'click'])
    document.addEventListener(type, add_login_next, true);

// The text keeps room on both sides for the close button in the corner,
// so it never runs under it and stays centred (.api-message-text).
function show_error_message(message) {
    var div = $('<div>').addClass('api-message-text bg-light text-danger').html(message);
    $('.api_messages').empty();
    let button = $("<button type='button' class='api-message-close btn' aria-label='Schließen' title='Schließen'><i class='bi bi-x-lg' aria-hidden='true'></i></button>");
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
            const item = $(`<li><button type='button' class='dropdown-item' role='option'>${p.name ? `${esc(p.name)}<span class='d-block text-body-secondary small'>${esc(p.email)}</span>` : esc(p.email)}</button></li>`);
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

// A select one can type into (the schools on /personen and /gruppen): a
// text field in front shows the chosen option; typing lists the options
// that contain the text, arrows and Enter pick one. The select stays,
// hidden, and keeps its value and its change event, so the page's code is
// unchanged. Options that come later count at once.
function searchable_select(select) {
    select = $(select);
    if (select.length === 0 || select.data('searchable')) return;
    select.data('searchable', true);
    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    const small = select.hasClass('form-select-sm');
    const wrap = $("<div class='position-relative searchable-select'></div>");
    if (select.hasClass('w-auto')) wrap.addClass('d-inline-block');
    // The select's own classes (a minimum width, …) go to the wrapper; the
    // field keeps the look of a select, arrow included.
    (select.attr('class') || '').split(/\s+/).filter(k => k && !/^(form-select|form-select-sm|w-auto|d-none)$/.test(k)).forEach(k => wrap.addClass(k));
    const input = $(`<input type='text' class='form-select${small ? ' form-select-sm' : ''}' autocomplete='off' spellcheck='false'>`);
    const id = select.attr('id');
    const label = id ? $(`label[for="${id}"]`) : $();
    input.attr({ role: 'combobox', 'aria-expanded': 'false', 'aria-autocomplete': 'list', placeholder: 'Tippen zum Suchen',
        'aria-label': select.attr('aria-label') || label.text() || 'Auswahl' });
    if (id) { input.attr('id', `${id}_search`); label.attr('for', `${id}_search`); }
    const menu = $("<ul class='dropdown-menu suggest-menu' role='listbox'></ul>");
    select.after(wrap);
    wrap.append(input, menu);
    select.addClass('d-none').attr({ 'aria-hidden': 'true', tabindex: '-1' });
    let shown = [], active = -1, last = select.val();
    // An option without a value (»Aktivität anheften …«) is the prompt: the
    // field's placeholder, not a choice.
    const prompt = select.find('option').filter(function () { return $(this).val() === ''; }).first().text();
    if (prompt && !/alle /i.test(prompt)) input.attr('placeholder', prompt);
    // An option in an <optgroup> (a section of the catalog, a school)
    // belongs to its label, which the list shows as a heading.
    const options = () => select.find('option').map(function () {
        return { value: $(this).val(), text: $(this).text(), more: $(this).attr('data-search') || '', group: $(this).parent('optgroup').attr('label') || '' };
    }).get().filter(o => o.value !== '' || /alle /i.test(o.text));
    const sync = function () {
        last = select.val();
        const chosen = select.find('option:selected');
        input.val(chosen.val() === '' && !/alle /i.test(chosen.text()) ? '' : chosen.text());
    };
    const close = function () { menu.removeClass('show').empty(); input.attr('aria-expanded', 'false'); shown = []; active = -1; };
    const mark = function () {
        const item = menu.find('.dropdown-item').removeClass('active').eq(active).addClass('active');
        // the active one in view when the arrows move through a long list
        if (item.length) item[0].scrollIntoView({ block: 'nearest' });
    };
    const pick = function (option) {
        close();
        if (option && option.value !== select.val()) select.val(option.value).trigger('change');
        sync();
    };
    // A word finds an option by its text or its value (the address of an
    // activity, »pixelram«), without regard to case, accents, spaces and
    // hyphens; several words must all be found. An option's data-search
    // (an activity's description) counts too, but those come after the
    // others and say »im Text«.
    const plain = (t) => `${t}`.toLowerCase().normalize('NFD').replace(/[\u0300-\u036f]/g, '').replace(/[\s\-_.]+/g, '');
    const show = function (query) {
        const words = query.trim().split(/\s+/).filter(Boolean).map(plain);
        const all = options();
        const direct = all.filter(o => { const hay = plain(o.text) + ' ' + plain(o.value); return words.every(w => hay.includes(w)); });
        const in_text = words.length === 0 ? [] : all.filter(o => !direct.includes(o) && o.more &&
            words.every(w => (plain(o.text) + ' ' + plain(o.value) + ' ' + plain(o.more)).includes(w))).map(o => ({ ...o, in_text: true }));
        shown = direct.concat(in_text);
        active = shown.length ? 0 : -1;
        menu.empty();
        // a heading where the group changes; what only the description
        // matched comes last, under its own heading
        let heading = null;
        shown.forEach(function (o, i) {
            const label = o.in_text ? 'Im Text gefunden' : o.group;
            if (label && label !== heading) menu.append(`<li role='presentation'><h6 class='dropdown-header'>${esc(label)}</h6></li>`);
            heading = label;
            const item = $(`<li><button type='button' class='dropdown-item' role='option'>${esc(o.text)}</button></li>`);
            item.find('button').on('mousedown', function (e) { e.preventDefault(); pick(shown[i]); });
            menu.append(item);
        });
        if (!shown.length) menu.append(`<li><span class='dropdown-item-text text-body-secondary small'>Keine passt.</span></li>`);
        mark();
        menu.addClass('show');
        input.attr('aria-expanded', 'true');
    };
    input.on('focus', function () { input.select(); show(''); });
    input.on('input', () => show(input.val()));
    input.on('keydown', function (e) {
        if (e.key === 'ArrowDown') { e.preventDefault(); if (!menu.hasClass('show')) show(''); active = Math.min(active + 1, shown.length - 1); mark(); }
        else if (e.key === 'ArrowUp') { e.preventDefault(); active = Math.max(active - 1, 0); mark(); }
        else if (e.key === 'Enter') { e.preventDefault(); e.stopImmediatePropagation(); if (active >= 0) pick(shown[active]); }
        else if (e.key === 'Escape' && menu.hasClass('show')) { e.preventDefault(); e.stopPropagation(); close(); sync(); }
    });
    input.on('blur', () => setTimeout(function () { close(); sync(); }, 150));
    select.on('change', sync);
    // Options filled in later, or a value set by code without an event.
    new MutationObserver(sync).observe(select[0], { childList: true });
    const watch = setInterval(function () {
        if (!document.contains(select[0])) { clearInterval(watch); return; }
        if (select.val() !== last && !input.is(':focus')) sync();
    }, 300);
    sync();
}

// Sortable columns: a click on a heading sorts by it, a second click the
// other way round. A cell's data-sort, when given, is what counts (a date,
// a size); otherwise its text. Headings with class no-sort, or without
// text, stay as they are.
function make_sortable(table) {
    const collator = new Intl.Collator('de', { numeric: true, sensitivity: 'base' });
    table.find('thead th').each(function (index) {
        const th = $(this);
        const visible = th.clone().find('.visually-hidden').remove().end().text().trim();
        if (th.hasClass('no-sort') || !visible || th.find('input').length) return;
        th.addClass('sortable').attr({ tabindex: 0, 'aria-sort': 'none' });
        const sort = function () {
            const descending = th.attr('aria-sort') === 'ascending';
            table.find('thead th').attr('aria-sort', function () { return $(this).hasClass('sortable') ? 'none' : null; });
            th.attr('aria-sort', descending ? 'descending' : 'ascending');
            const key = function (row) {
                const cell = $(row).children().eq(index);
                const value = cell.attr('data-sort');
                return value !== undefined ? value : cell.text().trim();
            };
            // A row with the class row-detail (a note below a row, over the
            // whole width) stays with the row above it.
            const groups = [];
            table.find('tbody > tr').each(function () {
                if ($(this).hasClass('row-detail') && groups.length) groups[groups.length - 1].push(this);
                else groups.push([this]);
            });
            groups.sort(function (ga, gb) {
                const x = key(ga[0]), y = key(gb[0]);
                const nx = parseFloat(x), ny = parseFloat(y);
                const order = (!isNaN(nx) && !isNaN(ny) && String(nx) === x && String(ny) === y) ? nx - ny : collator.compare(x, y);
                return descending ? -order : order;
            });
            table.find('tbody').append(groups.flat());
        };
        th.on('click', sort);
        th.on('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); sort(); } });
    });
}

function show_success_message(message) {
    var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('bg-light text-success').html(message);
    $('.api_messages').empty();
    $('.api_messages').append(div).show();
    // A floating message box (.page-messages) goes again by itself.
    clearTimeout(window.success_message_timeout);
    if ($('.api_messages').hasClass('page-messages'))
        window.success_message_timeout = setTimeout(function () { $('.api_messages').fadeOut(); }, 6000);
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
                var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('text-muted please-wait').html("<i class='fa fa-cog bi-spin'></i>&nbsp;&nbsp;Einen Moment bitte...");
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
        // Only the »please wait« goes; a message another call has just shown
        // (a success after a reload, say) stays.
        $('.api_messages .please-wait').remove();
        if ($('.api_messages').children().length === 0) $('.api_messages').hide();
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
// Why an administrator opens a Workspace or imitates someone (5 to 300
// characters); the person sees it. The last one is offered again.
// /admin and /personen.
function ask_admin_reason(title, go) {
    let last = '';
    try { last = localStorage.getItem('admin_look_in_reason') || ''; } catch (e) { }
    input_dialog({ title: title, label: 'Warum? Die Person sieht diesen Satz als Hinweis, zum Beispiel »Ich helfe bei einem Problem mit dem Workspace.«', value: last, confirm: 'Weiter', icon: 'bi-check-lg' }, function (reason) {
        if (reason.length < 5) { show_error_message('Bitte einen Satz mit mindestens 5 Zeichen.'); return false; }
        try { localStorage.setItem('admin_look_in_reason', reason); } catch (e) { }
        $('#__template_modal').modal('hide');
        go(reason);
        return false;
    });
}

function admin_open_workspace(email, button) {
    ask_admin_reason(`Workspace von ${email} öffnen`, function (reason) {
        button.prop('disabled', true);
        button.find('.bi').addClass('bi-spin');
        api_call('/api/start_server_as_admin', { email: email, reason: reason }, function (data) {
            if (data.success === false || !data.watch_tag) { button.prop('disabled', false); button.find('.bi').removeClass('bi-spin'); return; }
            check_server_tag_update(data.watch_tag);
            check_server_online(data.watch_tag, true);
        });
    });
}

function admin_impersonate(email, button) {
    ask_admin_reason(`${email} nachahmen`, function (reason) {
        button.prop('disabled', true);
        button.find('.bi').addClass('bi-spin');
        api_call('/api/impersonate', { email: email, reason: reason }, function (data) {
            if (data.success !== false) location.href = '/';
            else { button.prop('disabled', false); button.find('.bi').removeClass('bi-spin'); }
        });
    });
}

// What the administration does in someone's Workspace (stop it, make room):
// a reason the person reads as a notice, and by mail when ticked (the
// default: the person may not come by soon). intro: HTML above the reason,
// for example the folders to delete. go(reason, notify) does it.
function admin_action_dialog(options, go) {
    let last = '';
    try { last = localStorage.getItem('admin_look_in_reason') || ''; } catch (e) { }
    const esc = (t) => $('<div>').text(t === undefined || t === null ? '' : String(t)).html();
    showTemplateModal(esc(options.title), (options.intro || '')
        + `<label class='form-label' for='ti_admin_reason'>Warum? Die Person liest diesen Satz.</label><input id='ti_admin_reason' class='form-control' maxlength='300' autocomplete='off'>`
        + `<div class='form-check mt-2'><input class='form-check-input' type='checkbox' id='cb_admin_notify' checked><label class='form-check-label' for='cb_admin_notify'>Auch per E-Mail Bescheid geben</label></div>`,
        `<i class='bi ${options.icon || 'bi-check-lg'}'></i>${options.confirm}`, options.danger ? 'btn-danger' : 'btn-primary', "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary', function () {
            const reason = $('#ti_admin_reason').val().trim();
            if (reason.length < 5) { show_error_message('Bitte einen Satz mit mindestens 5 Zeichen.'); return false; }
            try { localStorage.setItem('admin_look_in_reason', reason); } catch (e) { }
            const result = go(reason, $('#cb_admin_notify').is(':checked'));
            return result === false ? false : undefined;
        });
    $('#ti_admin_reason').val(last);
}
