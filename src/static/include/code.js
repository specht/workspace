function show_error_message(message) {
    var div = $('<div>').css('text-align', 'center').css('padding', '15px').addClass('bg-light text-danger').html(message);
    $('.api_messages').empty();
    let button = $("<button class='text-stone-400 btn pull-right form-control' style='width: unset; margin: 8px;' ><i class='bi bi-x-lg'></i></button>");
    $('.api_messages').append(button).append(div).show();
    button.on('click', function(e) { e.preventDefault(); $('.api_messages').hide(); });
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