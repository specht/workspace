// A group as a card: members, co-teachers, consent, welcome mails, end date,
// rename, delete, members who never logged in (render_group). /gruppen shows
// every group a person manages; the class page (/klasse, tab »Gruppe«) the
// chosen one. Each page defines load_groups(), which these functions call
// after a change, and may define on_group_renamed(old, new).

const GROUP_ERRORS = {
    count_changed: 'Die Zahl stimmt nicht (oder hat sich inzwischen geändert). Bitte die Zahl eingeben, die der Dialog nennt.',
    roster_in_files: 'Die Einladungen stehen noch in Dateien; die Administration muss sie zuerst importieren.',
    invalid_name: 'Der Name ist leer, zu lang oder enthält < oder >.',
    name_taken: 'Diesen Namen gibt es schon.',
    no_school: 'Dir ist noch keine Schule zugeordnet; das macht die Administration.',
    not_your_group: 'Das ist keine Gruppe von dir.',
    not_your_student: 'Diese Person ist in keiner deiner Gruppen.',
    not_a_teacher: 'Unter dieser Adresse gibt es keine Lehrkraft.',
    other_school: 'Diese Lehrkraft gehört zu einer anderen Schule.',
    last_teacher: 'Eine Gruppe braucht mindestens eine Lehrkraft.',
    confirmation_mismatch: 'Der Name stimmt nicht.',
    same_school: 'Die Gruppe gehört schon zu dieser Schule.',
    students_elsewhere: 'Einige Schülerinnen und Schüler stehen auch in einer Gruppe einer anderen Schule; so lässt sich die Gruppe nicht verschieben.',
    teachers_undecided: 'Bitte wählen, was mit den Lehrkräften der anderen Schule geschieht.',
    teacher_elsewhere: 'Eine Lehrkraft betreut noch Gruppen einer anderen Schule und kann nicht mitgehen; nimm sie stattdessen aus der Gruppe.',
    too_many: 'Höchstens 1000 Adressen auf einmal.',
};

function esc(text) {
    return $('<div>').text(text === null || text === undefined ? '' : String(text)).html();
}

function groups_call(url, data, done) {
    api_call(url, data, function (result) {
        if (!result.success) {
            show_error_message(GROUP_ERRORS[result.error] || 'Das hat nicht geklappt.');
            return;
        }
        if (done) done(result);
    }, { no_please_wait: true });
}

// »vor 3 Tagen« for a date (YYYY-MM-DD).
function ago_text(date) {
    const days = Math.round((new Date(new Date().toISOString().slice(0, 10)) - new Date(date)) / 86400000);
    return days <= 0 ? 'heute' : days === 1 ? 'gestern' : days < 60 ? `vor ${days} Tagen` : days < 730 ? `vor ${Math.round(days / 30.4)} Monaten` : `vor ${Math.round(days / 365)} Jahren`;
}

// Disk space in KB as on the admin dashboard (bytes_to_str): »153.0M«,
// the unit in its own span; at fat_kb and above highlighted.
function disk_html(kb, fat_kb) {
    if (kb === null || kb === undefined) return "<span class='text-body-secondary'>–</span>";
    const [value, unit] = kb < 1024 ? [kb, 'k'] : kb < 1024 * 1024 ? [kb / 1024, 'M'] : kb < 1024 * 1024 * 1024 ? [kb / 1024 / 1024, 'G'] : [kb / 1024 / 1024 / 1024, 'T'];
    const text = `<span class='disk-size'>${value.toFixed(1)}<span class='unit'>${unit}</span></span>`;
    return fat_kb && kb >= fat_kb ? `<span class='disk-fat' title='deutlich mehr als die meisten'>${text}</span>` : text;
}

let fat_kb = null;

// The welcome mail before it goes: recipients, subject, the text as the
// first one gets it, and the attached Datenschutzerklärung to look at.
function welcome_dialog(group, emails, done) {
    const payload = { name: group.name };
    if (emails) payload.emails = emails;
    groups_call('/api/groups/welcome_preview', payload, function (p) {
        if (!p.recipients) { show_error_message('Alle in der Gruppe haben schon eine Willkommensmail bekommen.'); return; }
        const frame = $(`<iframe class='mail-preview' title='Vorschau der E-Mail' sandbox></iframe>`).attr('srcdoc', p.html);
        const body = $(`<div></div>`).append(
            `<p class='mb-1'><strong>An:</strong> ${p.recipients === 1 ? esc(p.first) : `${p.recipients} Personen (die Vorschau zeigt die E-Mail an ${esc(p.first)})`}<br>`
                + `<strong>Antworten an:</strong> ${esc(p.reply_to)}<br><strong>Betreff:</strong> ${esc(p.subject)}</p>`,
            frame,
            `<p class='mt-2 mb-0'><i class='bi bi-paperclip'></i><a href='${esc(p.attachment.url)}' target='_blank'>${esc(p.attachment.filename)}</a> <span class='small text-body-secondary'>(Anhang, zum Ansehen)</span></p>`);
        showTemplateModal('Willkommensmail', '', `<i class='bi bi-envelope'></i>${p.recipients === 1 ? 'Senden' : `An ${p.recipients} senden`}`, 'btn-primary', "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary', function () {
            const button = $('#__template_modal .modal-footer .btn-primary').prop('disabled', true).html("<i class='bi bi-arrow-repeat bi-spin'></i>Wird gesendet …");
            groups_call('/api/groups/send_welcome', payload, function (r) {
                $('#__template_modal').modal('hide');
                show_success_message(`${r.sent} ${r.sent === 1 ? 'Willkommensmail' : 'Willkommensmails'} gesendet.${r.limited ? ' Mehr geht in dieser Stunde nicht; den Rest später.' : ''}`);
                if (done) done(r);
            });
            return false;
        });
        // The iframe goes in as an element: showTemplateModal takes text.
        $('#__template_modal .modal-body').append(body);
    });
}

function suggest_people(group, kind, q, show) {
    api_call('/api/groups/suggest', { name: group.name, kind: kind, q: q }, r => show(r.success ? r.people : []), { no_please_wait: true });
}

// A short green glow: saved.
function flash(el) {
    el.addClass('saved-flash');
    setTimeout(() => el.removeClass('saved-flash'), 900);
}

function day(ts) {
    return ts ? new Date(ts * 1000).toLocaleDateString('de-DE') : '';
}

let all_schools = [];

function school_label(s) {
    return `${esc(s.name)} <span class='small ${s.approved ? 'text-body-secondary' : 'text-warning-emphasis'}'>(${s.approved ? 'hat zugestimmt' : 'hat noch nicht zugestimmt'})</span>`;
}

// Administrators only: move a group to another school. First choose the
// school, then the server's preview says what changes, and only then
// does anything happen.
function change_school(group) {
    const others = all_schools.filter(s => !group.school || s.key !== group.school.key);
    if (!others.length) {
        show_error_message('Es gibt keine andere Schule. Lege sie zuerst unter <a href="/personen#schulen">Personen</a> an.');
        return;
    }
    const select = `<select id='sel_target_school' class='form-select'>${others.map(s => `<option value='${esc(s.key)}'>${esc(s.name)}${s.approved ? '' : ' (hat noch nicht zugestimmt)'}</option>`).join('')}</select>`;
    showTemplateModal('Schule wechseln', `<p>Zu welcher Schule gehört die Gruppe ${esc(group.name)}? Fehlt die Schule, lege sie zuerst unter <a href='/personen#schulen'>Personen</a> an.</p><label class='form-label' for='sel_target_school'>Neue Schule</label>${select}`,
        "<i class='bi bi-arrow-right'></i>Weiter", 'btn-primary', 'Abbruch', 'btn-secondary', function () {
            const target = $('#sel_target_school').val();
            groups_call('/api/groups/school_change_preview', { name: group.name, school: target }, plan => confirm_school_change(plan));
            return false;
        });
    searchable_select('#sel_target_school');
}

function confirm_school_change(plan) {
    let html = `<p><strong>${esc(plan.group)}</strong> mit ${plan.members} ${plan.members === 1 ? 'Mitglied' : 'Mitgliedern'}<br>`
        + `von ${plan.from ? school_label(plan.from) : 'ohne Schule'}<br>nach ${school_label(plan.to)}</p>`
        + `<p class='small'>Es ändert sich nur die Schule der Gruppe. Mitglieder, Namen, Konten, Workspaces, Aufgaben und Abgaben bleiben, wie sie sind.</p>`;
    if (!plan.to.approved) {
        html += `<div class='alert alert-warning small'>${esc(plan.to.name)} hat dem Einsatz noch nicht zugestimmt. Die Schülerinnen und Schüler können sich weiter anmelden; ein vermerktes Einverständnis bleibt.</div>`;
    }
    const ok = plan.students_elsewhere.length === 0;
    if (!ok) {
        html += `<div class='alert alert-danger small'>Diese Personen stehen auch in Gruppen einer anderen Schule. Eine Lehrkraft der neuen Schule sähe dann eine Schülerin oder einen Schüler einer anderen Schule; deshalb geht das nicht. Nimm sie zuerst aus einer der Gruppen:<ul class='mb-0'>`
            + plan.students_elsewhere.map(s => `<li>${esc(s.name || s.email)}: ${s.groups.map(esc).join(', ')}</li>`).join('') + `</ul></div>`;
    }
    const mismatched = plan.teachers.filter(t => !t.matches && !t.admin);
    const can_move = mismatched.every(t => t.teaches_elsewhere.length === 0);
    if (mismatched.length) {
        const one = mismatched.length === 1;
        const who = one ? esc(mismatched[0].name || mismatched[0].email) : 'Diese Lehrkräfte';
        html += `<p class='mb-1'>${one ? 'Diese Lehrkraft gehört' : 'Diese Lehrkräfte gehören'} zu einer anderen Schule als ${esc(plan.to.name)}:</p><ul class='small'>`
            + mismatched.map(t => `<li>${esc(t.name || t.email)} (${t.school_name ? esc(t.school_name) : 'ohne Schule'})`
                + (t.teaches_elsewhere.length ? `; betreut auch ${t.teaches_elsewhere.map(esc).join(', ')}` : '')
                + (t.member_of.length ? `; Mitglied in ${t.member_of.map(esc).join(', ')} (bleibt)` : '') + `</li>`).join('')
            + `</ul><div class='form-check'><input class='form-check-input' type='radio' name='rb_teachers' id='rb_teachers_move' value='move'${can_move ? ' checked' : ' disabled'}>`
            + `<label class='form-check-label' for='rb_teachers_move'>Mit zu ${esc(plan.to.name)} nehmen: ${who} ${one ? 'gehört' : 'gehören'} dann zu dieser Schule${can_move ? '' : ` (geht nicht, solange ${one ? 'die Lehrkraft' : 'eine von ihnen'} noch Gruppen einer anderen Schule betreut)`}</label></div>`
            + `<div class='form-check mb-2'><input class='form-check-input' type='radio' name='rb_teachers' id='rb_teachers_remove' value='remove'${can_move ? '' : ' checked'}>`
            + `<label class='form-check-label' for='rb_teachers_remove'>Aus der Gruppe nehmen: ${who} ${one ? 'betreut' : 'betreuen'} die Gruppe nicht mehr und ${one ? 'bleibt' : 'bleiben'} bei ${one ? esc(mismatched[0].school_name || 'der bisherigen Schule') : 'ihrer bisherigen Schule'}</label></div>`;
    }
    const admins = plan.teachers.filter(t => t.admin && !t.matches);
    if (admins.length) {
        html += `<p class='small text-body-secondary'>Administration (bleibt, wie sie ist): ${admins.map(t => esc(t.name || t.email)).join(', ')}</p>`;
    }
    showTemplateModal('Schule wechseln', html, ok ? "<i class='bi bi-building'></i>Schule wechseln" : 'Schließen', ok ? 'btn-primary' : 'btn-secondary', ok ? 'Abbruch' : null, 'btn-secondary', function () {
        if (!ok) return true;
        const payload = { name: plan.group, school: plan.to.key };
        if (mismatched.length) payload.teachers = $('input[name=rb_teachers]:checked').val();
        groups_call('/api/groups/change_school', payload, function (r) {
            $('#__template_modal').modal('hide');
            load_groups();
        });
        return false;
    });
}

// Which groups are open, so that a reload after every change keeps them.
const open_groups = new Set();
let first_load = true;

function rename_group(group) {
    input_dialog({ title: 'Gruppe umbenennen', label: 'Neuer Name', value: group.name, confirm: 'Umbenennen', icon: 'bi-pencil' }, function (name) {
        if (name === group.name) return true;
        groups_call('/api/groups/rename', { name: group.name, new_name: name }, function (r) {
            $('#__template_modal').modal('hide');
            if (open_groups.delete(group.name)) open_groups.add(r.renamed);
            if (typeof on_group_renamed === 'function') on_group_renamed(group.name, r.renamed);
            else load_groups();
        });
        return false;
    });
}

function render_group(group, admin) {
    const students = group.members.filter(m => !m.teacher);
    const with_consent = students.filter(m => m.consent_at).length;
    const accounts = students.filter(m => m.account).length;
    const details = $(`<details class='group-card'></details>`).attr('data-group', group.name).prop('open', open_groups.has(group.name));
    details.on('toggle', function () { this.open ? open_groups.add(group.name) : open_groups.delete(group.name); });
    // What a closed group says: enough to find it and to see what is open.
    const facts = [`${students.length} ${students.length === 1 ? 'Mitglied' : 'Mitglieder'}`, `${accounts} mit Konto`];
    if (group.school && !group.school.approved && !group.school.guests && students.length) {
        facts.push(`<span class='text-warning-emphasis'>Einverständnis ${with_consent} von ${students.length}</span>`);
    }
    if (group.ends_on) {
        const past = group.ends_on < new Date().toISOString().slice(0, 10);
        facts.push(`<span class='${past ? 'text-warning-emphasis' : ''}'>${past ? 'zu Ende seit' : 'endet am'} ${new Date(group.ends_on).toLocaleDateString('de-DE')}</span>`);
    }
    if (group.teachers.length) {
        const names = group.teachers.map(t => esc(t.name || t.email));
        facts.push(names.length > 2 ? `<span title='${names.join(', ')}'>${names.slice(0, 2).join(', ')} +${names.length - 2}</span>` : names.join(', '));
    }
    else facts.push(`<span class='text-warning-emphasis'>keine Lehrkraft</span>`);
    // For administrators: when anyone of the group was last here, and the
    // disk space of all its Workspaces.
    if (admin && students.length) {
        facts.push(group.last_active ? `zuletzt da ${ago_text(group.last_active)}` : `<span class='text-warning-emphasis'>keine Spur einer Nutzung</span>`);
        if (group.disk_kb) facts.push(disk_html(group.disk_kb, null));
    }
    details.append($('<summary>').append(`<h2>${esc(group.name)}</h2><span class='facts'>${facts.join(' · ')}</span>`));
    const body = $(`<div class='group-body'></div>`);
    details.append(body);
    const school_line = group.school
        ? `${esc(group.school.name)}${group.school.guests ? ' (ohne Schule): nur Erwachsene; jede Person stimmt bei der ersten Anmeldung der <a href="/datenschutz/nutzungsordnung">Nutzungsordnung</a> zu' : group.school.approved ? ' · hat zugestimmt' : ' · <span class="text-warning-emphasis">hat noch nicht zugestimmt</span>'}`
        : 'ohne Schule';
    // One row for what concerns the whole group.
    const toolbar = $(`<div class='group-toolbar'><span class='small text-body-secondary me-auto'>${school_line}</span></div>`);
    // When the group ends: afterwards its teachers get one reminder.
    const ends = $(`<label class='small d-flex align-items-center gap-1 text-body-secondary'>Endet am <input type='date' class='form-control form-control-sm' style='width: auto;'></label>`);
    ends.find('input').val(group.ends_on || '').on('change', function () {
        const input = $(this);
        groups_call('/api/groups/set_end', { name: group.name, date: input.val() }, function (r) { group.ends_on = r.ends_on; flash(input); });
    });
    toolbar.append(ends);
    const rename = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-pencil'></i>Umbenennen</button>`);
    rename.on('click', () => rename_group(group));
    toolbar.append(rename);
    if (admin) {
        const move = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-building'></i>Schule wechseln</button>`);
        move.on('click', () => change_school(group));
        toolbar.append(move);
    }
    const never = never_logged_in(group).length;
    if (never) {
        const clean = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-envelope-slash'></i>Nie Angemeldete entfernen (${never})</button>`);
        clean.on('click', () => remove_never_logged_in([group.name]));
        toolbar.append(clean);
    }
    const del = $(`<button type='button' class='btn btn-sm btn-outline-danger'><i class='bi bi-trash'></i>Löschen</button>`);
    del.on('click', function () {
        showTemplateModal('Gruppe löschen', `<p>Die Gruppe ${esc(group.name)} wird gelöscht. Wer in keiner anderen Gruppe steht, kann sich danach nicht mehr anmelden; Konten bleiben, bis sie nach den Löschregeln gelöscht werden.</p><label class='form-label' for='ti_delete_group'>Zur Bestätigung den Namen der Gruppe eingeben:</label><input id='ti_delete_group' class='form-control' autocomplete='off'>`,
            "<i class='bi bi-trash'></i>Löschen", 'btn-danger', 'Abbruch', 'btn-secondary', function () {
                groups_call('/api/groups/delete', { name: group.name, confirmation: $('#ti_delete_group').val().trim() }, load_groups);
            });
    });
    toolbar.append(del);
    body.append(toolbar);

    const teachers = $(`<div class='group-teachers'><span class='small text-body-secondary'>Lehrkräfte</span></div>`);
    // Many teachers (a whole year group): the first eight, the rest on request.
    const many = group.teachers.length > 10;
    group.teachers.forEach(function (t, i) {
        const chip = $(`<span class='teacher-chip'>${esc(t.name || t.email)}</span>`).attr('title', t.email);
        if (group.teachers.length > 1 || admin) {
            const x = $(`<button type='button' class='btn-close' aria-label='${esc(t.name || t.email)} aus der Gruppe nehmen' title='Aus der Gruppe nehmen'></button>`);
            x.on('click', () => confirm_dialog({ title: 'Lehrkraft aus der Gruppe nehmen', text: `${t.name || t.email} betreut ${group.name} danach nicht mehr. Die Schule der Lehrkraft und ihre anderen Gruppen bleiben.`,
                confirm: 'Herausnehmen', icon: 'bi-x-lg', danger: true },
                () => groups_call('/api/groups/remove_teacher', { name: group.name, email: t.email }, load_groups)));
            chip.append(x);
        }
        if (many && i >= 8) chip.addClass('d-none extra-teacher');
        teachers.append(chip);
    });
    if (many) {
        const more = $(`<button type='button' class='btn btn-sm btn-link'>alle ${group.teachers.length} zeigen</button>`);
        more.on('click', function () { teachers.find('.extra-teacher').removeClass('d-none'); more.remove(); });
        teachers.append(more);
    }
    const add_teacher = $(`<button type='button' class='btn btn-sm btn-link'><i class='bi bi-person-plus'></i>Lehrkraft dazu</button>`);
    add_teacher.on('click', function () {
        input_dialog({ title: 'Lehrkraft dazu', label: 'Name oder E-Mail-Adresse einer Lehrkraft deiner Schule, die die Gruppe mit betreut', confirm: 'Hinzufügen', icon: 'bi-person-plus',
            suggest: (q, show) => suggest_people(group, 'teacher', q, show) }, function (email) {
            groups_call('/api/groups/add_teacher', { name: group.name, email: email }, function () {
                $('#__template_modal').modal('hide');
                load_groups();
            });
            return false;
        });
    });
    teachers.append(add_teacher);
    body.append(teachers);

    const without = group.members.filter(m => !m.welcomed_at && !m.teacher).length;
    if (group.members.length > 0) {
        // The consent column only while the school has not agreed: then a
        // student's written consent covers that student.
        const ask_consent = group.school && !group.school.approved;
        const table = $(`<table class='table table-sm align-middle people-table'><thead><tr><th>Name</th><th>E-Mail-Adresse</th><th>Konto</th>${admin ? '<th>Zuletzt da</th><th>Speicher</th>' : ''}<th>Willkommen</th>${ask_consent ? "<th title='Datum des schriftlichen Einverständnisses, das du aufbewahrst'>Einverständnis</th>" : ''}<th><span class='visually-hidden'>Entfernen</span></th></tr></thead><tbody></tbody></table>`);
        // The class page adds a column of its own (the Workspace, live).
        if (typeof member_table_hook === 'function') member_table_hook(table);
        group.members.forEach(function (m) {
            const row = $('<tr>').attr('data-email', m.email);
            const name = $(`<input class='member-name' placeholder='Namen eintragen' aria-label='Name von ${esc(m.email)}' title='Klicken, um den Namen zu ändern'>`).attr('size', Math.max(16, (m.name || '').length + 1)).val(m.name || '');
            name.on('change', function () {
                groups_call('/api/groups/set_name', { email: m.email, person_name: name.val() }, function () {
                    m.name = name.val().trim() || null;
                    flash(name);
                });
            });
            name.on('keydown', function (e) { if (e.key === 'Enter') this.blur(); });
            const welcome = $(`<button type='button' class='btn btn-sm btn-link p-0'>${m.welcomed_at ? day(m.welcomed_at) + ' · erneut' : 'senden'}</button>`);
            welcome.on('click', function () {
                welcome_dialog(group, [m.email], function () {
                    m.welcomed_at = Date.now() / 1000;
                    welcome.text(day(m.welcomed_at) + ' · erneut');
                    flash(welcome);
                });
            });
            const remove = $(`<button type='button' class='btn btn-sm btn-outline-secondary btn-icon' title='Aus der Gruppe nehmen' aria-label='${esc(m.name || m.email)} aus der Gruppe nehmen'><i class='bi bi-x-lg'></i></button>`);
            remove.on('click', function () {
                showTemplateModal('Aus der Gruppe nehmen', `<p>${esc(m.name || m.email)} aus ${esc(group.name)} nehmen? Steht die Person in keiner Gruppe mehr, kann sie sich nicht mehr anmelden; ihr Konto bleibt, bis es nach den Löschregeln gelöscht wird.</p>`,
                    "<i class='bi bi-x-lg'></i>Herausnehmen", 'btn-danger', 'Abbruch', 'btn-secondary', function () {
                        groups_call('/api/groups/remove_member', { name: group.name, email: m.email }, load_groups);
                    });
            });
            row.append($('<td>').attr('data-sort', (m.name || m.email).toLowerCase()).append(name), $('<td>').text(m.email),
                $('<td class="member-account">').html(m.account ? "<i class='bi bi-check-lg' aria-hidden='true'></i><span class='visually-hidden'>ja</span>" : "<span class='text-body-secondary'>–</span>"));
            if (admin) {
                row.append($('<td class="text-nowrap">').attr('data-sort', m.active_on || '').html(m.active_on ? `<span title='${new Date(m.active_on).toLocaleDateString('de-DE')}'>${ago_text(m.active_on)}</span>` : (m.account ? "<span class='text-warning-emphasis' title='Konto vorhanden, aber keine Spur einer Nutzung: kein Besuch, keine Anmeldung, keine Aufgabe, kein Workspace, keine Datei'>keine Spur</span>" : "<span class='text-body-secondary' title='nur eingeladen, hat nie einen Anmelde-Code angefordert'>kein Konto</span>")),
                    $('<td class="text-nowrap">').attr('data-sort', m.disk_kb === null || m.disk_kb === undefined ? -1 : m.disk_kb).html(disk_html(m.disk_kb, fat_kb)));
            }
            row.append($('<td>').append(m.teacher ? "<span class='small text-body-secondary'>Lehrkraft</span>" : welcome));
            if (ask_consent) {
                const consent = $(`<input type='date' class='form-control form-control-sm consent-date' aria-label='Einverständnis von ${esc(m.name || m.email)} vom'>`).val(m.consent_at || '');
                consent.attr('max', new Date().toISOString().slice(0, 10));
                consent.on('change', function () {
                    groups_call('/api/groups/set_consent', { name: group.name, email: m.email, date: consent.val() }, function () {
                        m.consent_at = consent.val() || null;
                        flash(consent);
                    });
                });
                row.append($('<td>').append(m.teacher ? '' : consent));
            }
            row.append($('<td class="text-end text-nowrap member-actions">').append(remove));
            if (typeof member_row_hook === 'function') member_row_hook(m, row);
            table.find('tbody').append(row);
        });
        make_sortable(table);
        body.append($(`<div class='table-responsive'></div>`).append(table));
    } else {
        body.append(`<p class='text-body-secondary'>Noch niemand in dieser Gruppe.</p>`);
    }

    const field = `add_${esc(group.name).replace(/\W+/g, '_')}`;
    const add = $(`<div class='group-add'>
        <label class='form-label small' for='ti_find_${field}'>Hinzufügen: schon Eingeladene suchen (Name oder Adresse), oder neue Adressen unten eintragen, eine pro Zeile, mit oder ohne Namen (»Lena Muster &lt;lena@example.org&gt;«, »lena@example.org« oder aus einer Tabelle kopiert)</label>
        <input class='form-control form-control-sm mb-1' id='ti_find_${field}' placeholder='Name oder Adresse suchen …' aria-label='Eingeladene suchen'>
        <textarea class='form-control mb-2' rows='2' spellcheck='false' id='ta_${field}' aria-label='Adressen, eine pro Zeile'></textarea>
    </div>`);
    // A suggestion goes into the box as a line; »Hinzufügen« adds them all.
    attach_suggestions(add.find('input'), (q, show) => suggest_people(group, 'member', q, show), function (person) {
        const box = add.find('textarea');
        const line = person.name ? `${person.name} <${person.email}>` : person.email;
        const lines = box.val().split('\n').filter(l => l.trim());
        if (!lines.some(l => l.includes(person.email))) lines.push(line);
        box.val(lines.join('\n') + '\n');
        add.find('input').val('').trigger('focus');
    });
    const add_button = $(`<button type='button' class='btn btn-sm btn-primary'><i class='bi bi-person-plus'></i>Hinzufügen</button>`);
    add_button.on('click', function () {
        const lines = add.find('textarea').val();
        if (!lines.trim()) return;
        groups_call('/api/groups/add_members', { name: group.name, lines: lines }, function (r) {
            let text = `${r.added} hinzugefügt`;
            if (r.already) text += `, ${r.already} waren schon in der Gruppe`;
            if (r.kept_names.length) text += `; bei ${r.kept_names.length} stand schon ein anderer Name, der bleibt`;
            if (r.rejected.length) text += `. Ohne gültige Adresse und nicht übernommen: ${r.rejected.join(' · ')}`;
            if (r.other_school && r.other_school.length) text += `. Schon in einer Gruppe einer anderen Schule und deshalb nicht übernommen (die Administration hilft): ${r.other_school.join(' · ')}`;
            show_success_message(esc(text + '.'));
            load_groups();
        });
    });
    const actions = $(`<div class='d-flex flex-wrap gap-2'></div>`).append(add_button);
    if (without > 0) {
        const send = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-envelope'></i>Willkommensmail an alle ohne (${without})</button>`);
        send.on('click', () => welcome_dialog(group, null, load_groups));
        actions.append(send);
    }
    add.append(actions);
    body.append(add);
    return details;
}

// Shows only the groups whose name, or one of whose members, matches;
// a match on a member opens the group.
// Who never logged in (no account), students and guests: what »Nie
// Angemeldete entfernen« takes out of a group.
const never_logged_in = group => group.members.filter(m => !m.teacher && !m.account);
const today = () => new Date().toISOString().slice(0, 10);
// Takes everyone without an account out of these groups, after the
// exact number (the server counts, each person once) is typed again.
function remove_never_logged_in(names) {
    groups_call('/api/people/unused_count', { groups: names }, function (r) {
        const people = r.people || [];
        const gone = people.filter(p => p.elsewhere === 0).length;
        const where = names.length === 1 ? `der Gruppe ${esc(names[0])}` : `${names.length} Gruppen`;
        showTemplateModal('Nie Angemeldete entfernen', `<p>Aus ${where} werden alle genommen, die nie einen Anmelde-Code angefordert haben: <strong>${r.count} ${r.count === 1 ? 'Person' : 'Personen'}</strong>. Ein Konto hat keine von ihnen; ein Workspace oder eine Datei wird also nicht gelöscht.</p>`
            + (gone ? `<p><strong>${gone === r.count ? (gone === 1 ? 'Sie steht' : 'Alle stehen') : `${gone} davon ${gone === 1 ? 'steht' : 'stehen'}`} danach in keiner Gruppe mehr und ${gone === 1 ? 'verschwindet' : 'verschwinden'} ganz,</strong> mit Name und Adresse: Keine Seite zeigt sie mehr. Wieder eingeladen wird nur, wer neu in eine Gruppe eingetragen wird. Lade die Liste vorher herunter, wenn du sie noch brauchst.</p>` : '')
            + `<p><a href='#' id='a_never_csv'><i class='bi bi-download'></i>Liste herunterladen (CSV)</a></p>`
            + `<label class='form-label' for='ti_never_confirm'>Zur Bestätigung die Zahl eingeben: <strong>${r.count}</strong></label><input id='ti_never_confirm' class='form-control' autocomplete='off' inputmode='numeric'>`,
            "<i class='bi bi-person-dash'></i>Entfernen", 'btn-danger', "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary', function () {
                groups_call('/api/people/remove_unused_invitations', { groups: names, confirmation: $('#ti_never_confirm').val().trim() }, function (done) {
                    $('#__template_modal').modal('hide');
                    show_success_message(`${done.memberships} ${done.memberships === 1 ? 'Mitgliedschaft' : 'Mitgliedschaften'} entfernt; ${done.uninvited} ${done.uninvited === 1 ? 'Person ist' : 'Personen sind'} nicht mehr eingeladen.`);
                    load_groups();
                });
                return false;
            });
        // Name, address, groups and whether the person stays invited, for a
        // spreadsheet (semicolons, a BOM so that umlauts survive).
        $('#a_never_csv').on('click', function (e) {
            e.preventDefault();
            const cell = (t) => `"${String(t ?? '').replace(/"/g, '""')}"`;
            const rows = [['Name', 'E-Mail', 'Gruppen', 'Danach noch eingeladen']]
                .concat(people.map(p => [p.name || '', p.email, p.groups.join(', '), p.elsewhere > 0 ? 'ja' : 'nein']));
            const csv = '\ufeff' + rows.map(row => row.map(cell).join(';')).join('\r\n') + '\r\n';
            const link = document.createElement('a');
            link.href = URL.createObjectURL(new Blob([csv], { type: 'text/csv;charset=utf-8' }));
            link.download = `nie-angemeldet-${today()}.csv`;
            link.click();
            setTimeout(() => URL.revokeObjectURL(link.href), 1000);
        });
    });
}

