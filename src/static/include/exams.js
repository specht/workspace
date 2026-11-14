// Leistungsüberprüfungen on the class page (Klassen, tab »Leistungs-
// überprüfungen«, exams_page.rb#class_exams_html): the exams of the class
// open, a row each (who takes part, start and stop, the preview, the
// results, deleting), and those uploaded before exams belonged to a class,
// to be given to one; an upload belongs to the class, and everyone in it
// takes part; the dialog of who takes part opens with the class, the other
// groups below it to add someone. window.EXAMS: the class and the teacher's
// groups, their members' names and schools.
(function () {
'use strict';
// Exams (»Leistungsüberprüfungen«): one row each, the name and when it
// was uploaded first, then who takes it, whether it runs, the preview
// and the results; deleting comes last and small.
let exam_entries = [];
const E = window.EXAMS || {};
const exam_group = E.group;
const user_group_order = E.order || [];
const user_groups = E.groups || {};
const invitations = E.names || {};
const group_schools = E.schools || {};
const my_school = E.mySchool || null;

function exam_title(entry) {
    return (entry.filename || 'Leistungsüberprüfung').replace(/\.(tar\.gz|tgz|tar)$/i, '');
}

// the teacher's groups that someone taking part in the exam is in, with
// how many of them, in the order of the groups
function participant_groups(entry) {
    const taking = new Set(entry.emails || []);
    return user_group_order.map((g) => [g, (user_groups[g] || []).filter((e) => taking.has(e)).length])
        .filter(([, n]) => n > 0);
}

function refresh_test_archives() {
    if (!document.getElementById('table_my_test_archives')) return;
    api_call('/api/get_my_test_archives', {}, function(data) {
        const tbody = $('#table_my_test_archives tbody').empty();
        if (!data.success) return;
        // this class's exams, and those uploaded before exams belonged to a
        // class (in the profile), to be given to one: in the classes their
        // participants are in (in every class while nobody takes part)
        exam_entries = data.entries.filter((e) => e.group === exam_group ||
                (!e.group && (!(e.emails || []).length || participant_groups(e).some(([g]) => g === exam_group))))
            .sort((a, b) => (!a.group) - (!b.group) || (b.ts || 0) - (a.ts || 0));
        if (exam_entries.length === 0) {
            tbody.append(`<tr><td colspan='6' class='text-body-secondary'>Noch keine für diese Klasse. Leg oben eine an; alle in der Klasse nehmen teil, sobald du sie startest.</td></tr>`);
            return;
        }
        for (let entry of exam_entries) {
            const title = exam_title(entry);
            const row = $('<tr>').appendTo(tbody);
            const uploaded = entry.ts ? new Date(entry.ts * 1000).toLocaleDateString('de-DE', { day: 'numeric', month: 'numeric', year: 'numeric' }) : '';
            const name_cell = $('<td>').append($('<strong>').text(title))
                .append($("<div class='small text-body-secondary'>").text(`${uploaded ? 'hochgeladen am ' + uploaded + ', ' : ''}${entry.size}`))
                .appendTo(row);
            if (!entry.group) {
                const from = participant_groups(entry).map(([g, n]) => `${g} (${n})`).join(', ');
                name_cell.append($("<div class='small text-body-secondary exam-unassigned'>")
                    .text(`Noch keiner Klasse zugeordnet${from ? '. Teilnehmende aus: ' + from : ''}`));
                const assign = $(`<button type='button' class='btn btn-link btn-sm px-0 exam-assign'><i class='bi bi-box-arrow-in-down-right'></i>Dieser Klasse zuordnen</button>`);
                assign.on('click', () => api_call('/api/test_assign_group', { tag: entry.tag, group: exam_group }, d => { if (d.success) refresh_test_archives(); }));
                name_cell.append($('<div>').append(assign));
            }

            const people = $(`<button type='button' class='btn btn-sm ${entry.count ? 'btn-outline-secondary' : 'btn-outline-primary'}'>`)
                .html(entry.count ? `<i class='bi bi-people'></i>${entry.count} ${entry.count === 1 ? 'Person' : 'Personen'}` : `<i class='bi bi-person-plus'></i>Auswählen`)
                .attr('aria-label', `Teilnehmende von ${title}`);
            people.on('click', () => open_exam_people(entry));
            $('<td>').append(people).appendTo(row);

            const status = $("<td class='text-nowrap'>").appendTo(row);
            if (entry.running) {
                status.append(`<span class='badge text-bg-success me-2'>läuft</span>`);
                const stop = $(`<button type='button' class='btn btn-sm btn-outline-danger'><i class='bi bi-stop-fill'></i>Beenden</button>`).appendTo(status);
                stop.on('click', () => confirm_dialog({ title: `»${title}« beenden`, text: 'Die Teilnehmenden kommen danach wieder in ihren normalen Workspace. Ihre Prüfungs-Workspaces bleiben erhalten; ein neuer Start setzt dort fort.', confirm: 'Beenden', icon: 'bi-stop-fill', danger: true },
                    () => api_call('/api/stop_test', { tag: entry.tag }, d => { if (d.success) refresh_test_archives(); })));
            } else {
                status.append(`<span class='badge text-bg-secondary me-2'>bereit</span>`);
                const start = $(`<button type='button' class='btn btn-sm btn-success'><i class='bi bi-play-fill'></i>Starten</button>`).appendTo(status);
                if (!entry.count) start.prop('disabled', true).attr('title', 'Wähle zuerst aus, wer teilnimmt.');
                start.on('click', () => confirm_dialog({ title: `»${title}« starten`, text: `${entry.count} ${entry.count === 1 ? 'Person kommt' : 'Personen kommen'} beim nächsten Öffnen in den Workspace dieser Leistungsüberprüfung statt in ihren eigenen.`, confirm: 'Starten', icon: 'bi-play-fill' },
                    () => api_call('/api/start_test', { tag: entry.tag }, d => { if (d.success) refresh_test_archives(); })));
            }

            // Two buttons side by side, not a btn-group: its overlapping
            // borders look doubled at some zoom levels.
            const preview = $(`<div class='d-flex gap-1' role='group' aria-label='Vorschau'>`);
            const bu_demo = $(`<button type='button' class='btn btn-sm btn-outline-secondary bu-exam-demo'><i class='bi bi-box-arrow-up-right'></i>Workspace</button>`).appendTo(preview);
            const bu_demo_pdf = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-printer'></i>Druckansicht</button>`).appendTo(preview);
            $('<td>').append(preview).appendTo(row);
            bu_demo.on('click', function() {
                showTemplateModal('Workspace-Vorschau öffnen', `
                    <p>Die Vorschau ist von deinem Workspace und von den Workspaces der Teilnehmenden getrennt.</p>
                    <p>Beim Öffnen werden <strong>nur deine bisherigen Vorschau-Dateien und Vorschau-Datenbanken zurückgesetzt</strong>.
                    Du bekommst den frischen Workspace, den auch die Teilnehmenden beim ersten Start sehen.</p>`,
                    "<i class='bi bi-box-arrow-up-right'></i>Frischen Workspace öffnen", 'btn-primary',
                    "<i class='bi bi-x-lg'></i>Abbrechen", 'btn-outline-secondary', function() {
                        bu_demo.prop('disabled', true);
                        bu_demo.find('.bi').addClass('bi-spin');
                        // spinning until the Workspace answers: check_server_online
                        // stops it (.bu-exam-demo) when it opens the tab
                        api_call('/api/start_test_demo', { tag: entry.tag }, function(data2) {
                            if (data2.success && data2.status !== 'failed') {
                                check_server_tag_update(data2.server_tag);
                                check_server_online(data2.server_tag);
                            } else {
                                bu_demo.prop('disabled', false);
                                bu_demo.find('.bi').removeClass('bi-spin');
                            }
                        });
                    });
            });
            bu_demo_pdf.on('click', () => window.open(`/api/pdf_for_test_demo/${entry.tag}`, '_blank'));

            const results = $(`<button type='button' class='btn btn-sm btn-outline-secondary'><i class='bi bi-printer'></i>Ergebnisse</button>`);
            if (!entry.count) results.prop('disabled', true);
            results.on('click', () => print_exam_results(entry));
            $('<td>').append(results).appendTo(row);

            const del = $(`<button type='button' class='btn btn-sm btn-outline-danger btn-icon'><i class='bi bi-trash'></i></button>`)
                .attr({ 'aria-label': `${title} löschen`, title: 'Löschen' });
            del.on('click', () => confirm_dialog({ title: `»${title}« löschen`, text: 'Gelöscht werden die Workspaces und Datenbanken, die für diese Leistungsüberprüfung angelegt wurden, bei allen Teilnehmenden, und deine Vorschau. Die eigenen Workspaces und Datenbanken der Teilnehmenden bleiben, wie sie sind. Lade vorher die Ergebnisse als PDF herunter, wenn du sie noch brauchst.', confirm: 'Löschen', icon: 'bi-trash', danger: true },
                () => api_call('/api/delete_test', { tag: entry.tag }, d => { if (d.success) refresh_test_archives(); })));
            $("<td class='text-end'>").append(del).appendTo(row);
        }
    });
}

function exam_name(email) {
    return (invitations[email] || {}).name || email;
}

// Who takes an exam: the groups one teaches, each with »Alle« and
// »Keine«, a search over all names, and the number chosen. Every change
// is saved at once (/api/test_set_users).
function open_exam_people(entry) {
    api_call('/api/get_assigned_users_for_test', { tag: entry.tag }, function(data) {
        if (!data.success) return;
        const chosen = new Set(data.emails);
        const body = $('#test_modal_users_here').empty();
        $('#test_modal .modal-title').text(`Wer nimmt an »${exam_title(entry)}« teil?`);
        const bar = $(`<div class='exam-people-bar'>
            <input type='search' class='form-control' placeholder='Name suchen …' aria-label='Name suchen' autocomplete='off'>
            <span class='exam-people-count' role='status' aria-live='polite'></span></div>`).appendTo(body);
        const search = bar.find('input');
        const sections = [];
        // Groups by school, one's own first. Other schools stay hidden
        // behind a button, unless someone there is already chosen.
        const school_of = (group) => group_schools[group] || 'Ohne Schule';
        const schools = [...new Set(user_group_order.map(school_of))]
            .sort((a, b) => (b === my_school) - (a === my_school) || a.localeCompare(b, 'de'));
        const own_only = !!my_school && schools.includes(my_school) && schools.length > 1;
        const others = $(`<div class='exam-people-others'></div>`);
        // this class first and open; the other groups below it, folded
        for (let [index, school] of [null, ...schools].entries()) {
          const groups = index === 0 ? [exam_group].filter((g) => (user_groups[g] || []).length)
              : user_group_order.filter((g) => g !== exam_group && school_of(g) === school && (user_groups[g] || []).length);
          if (groups.length === 0) continue;
          const own = index === 0 || !own_only || school === my_school;
          const keep = own || groups.some((g) => user_groups[g].some((e) => chosen.has(e)));
          const target = keep ? body : others;
          if (index === 1 || (index > 1 && schools.length > 1)) target.append($("<h6 class='exam-people-school'>").text(index > 0 && schools.length > 1 ? school : 'Andere Gruppen'));
          for (let group of groups) {
            const members = (user_groups[group] || []).slice().sort((a, b) => exam_name(a).localeCompare(exam_name(b), 'de'));
            const details = $(`<details class='exam-people-group'>`).appendTo(target);
            const summary = $(`<summary><span class='exam-people-group-name'></span><span class='exam-people-group-count text-body-secondary small'></span></summary>`).appendTo(details);
            summary.find('.exam-people-group-name').text(group);
            const actions = $(`<span class='exam-people-group-actions'>
                <button type='button' class='btn btn-link btn-sm' data-flag='1'>Alle</button>
                <button type='button' class='btn btn-link btn-sm' data-flag='0'>Keine</button></span>`).appendTo(summary);
            const grid = $(`<div class='exam-people-grid'>`).appendTo(details);
            for (let email of members) {
                const id = `exam_${Math.random().toString(36).slice(2, 10)}`;
                const item = $(`<div class='form-check'><input class='form-check-input' type='checkbox'><label class='form-check-label'></label></div>`).appendTo(grid);
                item.find('input').attr({ id: id, 'data-email': email });
                item.find('label').attr('for', id).text(exam_name(email));
                if (exam_name(email) !== email) item.attr('title', email);
            }
            // open: the class, and a group with someone added from it
            const in_class = (e) => (user_groups[exam_group] || []).includes(e);
            if (group === exam_group || members.some((e) => chosen.has(e) && !in_class(e))) details.prop('open', true);
            sections.push({ details, members });
            actions.find('button').on('click', function(e) {
                e.preventDefault();
                const flag = $(this).data('flag') === 1;
                set_people(members.filter(m => chosen.has(m) !== flag), flag);
            });
          }
        }
        if (others.children().length) {
            const n = others.find('.exam-people-group').length;
            const more = $(`<button type='button' class='btn btn-link btn-sm px-0 mt-2'><i class='bi bi-chevron-down'></i>Gruppen anderer Schulen zeigen (${n})</button>`);
            body.append(more, others.hide());
            more.on('click', () => { more.remove(); others.show(); });
        }
        if (sections.length === 0) body.append(`<p class='text-body-secondary'>In deinen Gruppen ist noch niemand. Unter <a href='/klasse'>Klassen</a> lädst du deine Schülerinnen und Schüler ein.</p>`);

        const show = function () {
            body.find('input[type=checkbox]').each(function () { this.checked = chosen.has($(this).data('email')); });
            for (let s of sections) {
                const n = s.members.filter(m => chosen.has(m)).length;
                s.details.find('.exam-people-group-count').text(`${n} von ${s.members.length}`);
            }
            bar.find('.exam-people-count').text(`${chosen.size} ausgewählt`);
        };
        const set_people = function (emails, flag) {
            if (emails.length === 0) return;
            const boxes = body.find('input[type=checkbox]').filter(function () { return emails.includes($(this).data('email')); }).prop('disabled', true);
            api_call('/api/test_set_users', { tag: entry.tag, emails: emails, flag: flag }, function(d) {
                boxes.prop('disabled', false);
                if (d.success) { chosen.clear(); d.emails.forEach(e => chosen.add(e)); }
                show();
            }, { no_please_wait: true });
        };
        body.off('change').on('change', 'input[type=checkbox]', function () { set_people([$(this).data('email')], this.checked); });
        search.on('input', function () {
            const q = search.val().trim().toLowerCase();
            // A search looks into the other schools too.
            if (q) others.show();
            for (let s of sections) {
                let hits = 0;
                s.details.find('.form-check').each(function () {
                    const email = $(this).find('input').data('email');
                    const hit = !q || exam_name(email).toLowerCase().includes(q) || email.toLowerCase().includes(q);
                    $(this).toggle(hit);
                    if (hit) hits++;
                });
                s.details.toggle(hits > 0);
                if (q && hits > 0) s.details.prop('open', true);
            }
        });
        show();
        $('#test_modal').one('hidden.bs.modal', refresh_test_archives).modal('show');
        $('#test_modal').one('shown.bs.modal', () => search.trigger('focus'));
    });
}

function print_exam_results(entry) {
    api_call('/api/get_assigned_users_for_test', { tag: entry.tag }, function(data) {
        if (!data.success) return;
        const emails = data.emails.slice().sort((a, b) => exam_name(a).localeCompare(exam_name(b), 'de'));
        showTemplateModal(`Ergebnisse von »${$('<div>').text(exam_title(entry)).html()}«`,
            `<p>Alle zusammen als ein PDF, oder eine Person einzeln. Zum Aufheben und Ausprobieren gibt es alle Dateien als ZIP, ein Ordner je Person.</p>` +
            `<p><a class='btn btn-outline-primary btn-sm' id='test_results_zip' href='/api/zip_for_test/${encodeURIComponent(entry.tag)}'><i class='bi bi-file-earmark-zip'></i>Alle Dateien als ZIP</a></p>` +
            `<div class='list-group' id='test_result_users_here'></div>`,
            "<i class='bi bi-printer'></i>Alle drucken", 'btn-primary', "<i class='bi bi-x-lg'></i>Schließen", 'btn-outline-secondary',
            function() {
                if (emails.length === 0) return false;
                window.open(`/api/pdf_for_test/${entry.tag}`, '_blank');
            });
        const list = $('#test_result_users_here');
        if (emails.length === 0) {
            list.replaceWith($("<p class='text-body-secondary mb-0'>").text('Dieser Leistungsüberprüfung ist noch niemand zugeordnet.'));
            $('#test_results_zip').closest('p').remove();
            $('#__template_modal .modal-footer .btn').eq(0).prop('disabled', true);
            return;
        }
        for (let email of emails) {
            const item = $(`<button type='button' class='list-group-item list-group-item-action d-flex justify-content-between align-items-center'><span></span><i class='bi bi-printer text-body-secondary'></i></button>`);
            item.find('span').text(exam_name(email));
            item.on('click', () => window.open(`/api/pdf_for_test/${entry.tag}?email=${encodeURIComponent(email)}`, '_blank'));
            list.append(item);
        }
    });
}

// a package uploaded here belongs to this class; everyone in it takes part
function upload_test_archive() {
    const fileInput = document.createElement('input');
    fileInput.type = 'file';
    fileInput.accept = '.tar.gz, .tar.bz2';
    fileInput.addEventListener('change', async function (event) {
        const file = event.target.files[0];
        if (!file) return;
        const statusBox = $('#test_archive_upload_status');
        const button = $('#bu_upload_test_archive');
        const formData = new FormData();
        formData.append('file', file);
        formData.append('group', exam_group);
        button.prop('disabled', true);
        statusBox.removeClass('text-danger text-success').text('Paket prüfen und Erweiterungen vorbereiten …');
        try {
            const response = await fetch('/api/upload_test_archive', { method: 'POST', body: formData });
            const data = await response.json();
            if (!response.ok || !data.success) throw new Error(data.error || 'Der Upload ist fehlgeschlagen.');
            statusBox.addClass('text-success').text(`Paket wurde geprüft und hochgeladen; ${data.count === 1 ? 'eine Person nimmt' : `${data.count} Personen nehmen`} teil.`);
            refresh_test_archives();
        } catch (error) {
            statusBox.addClass('text-danger').text(error.message);
        } finally {
            button.prop('disabled', false);
        }
    });
    fileInput.click();
}

// »Aus deinem Workspace« (exam_builder_page.rb): a folder of the teacher's
// Workspace, its files as a tree to tick (what goes into the package, what
// goes into the printout), the colour and the look, Git; the server packs
// it and it becomes this class's exam. What is hidden, a build folder or a
// Git history starts unticked; a link cannot be ticked.
const EB_REASONS = { hidden: 'versteckt', artifact: 'Build-Ordner', link: 'Link, geht nicht', git: 'Git-Verlauf' };
let eb_entries = [];

async function eb_post(url, body) {
    const response = await fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify(body) });
    let data = {};
    try { data = await response.json(); } catch (e) { }
    if (!response.ok || data.error) throw new Error(data.error || 'Das ging nicht; versuch es noch einmal.');
    return data;
}

function eb_size(bytes) {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(bytes < 10240 ? 1 : 0).replace('.', ',')} kB`;
    return `${(bytes / 1024 / 1024).toFixed(1).replace('.', ',')} MB`;
}

// a row's own checkbox controls everything below an opened folder
function eb_children(path) {
    return $('#eb_tree .eb-item').filter((_, el) => el.dataset.path.startsWith(path + '/'));
}

// an opened folder: ticked when all below it are, half when some are
function eb_sync_folders() {
    $($('#eb_tree .eb-item[data-open]').get().reverse()).each((_, el) => {
        $(el).toggleClass('eb-out', !eb_children(el.dataset.path).find('.eb-take:checked').length);
        for (const kind of ['take', 'print']) {
            const own = $(el).find(`.eb-${kind}`)[0];
            const below = eb_children(el.dataset.path).filter((_, c) => !c.dataset.open).find(`.eb-${kind}:not(:disabled)`).get();
            const on = below.filter((b) => b.checked).length;
            own.checked = below.length > 0 && on === below.length;
            own.indeterminate = on > 0 && on < below.length;
        }
    });
}

function eb_update() {
    eb_sync_folders();
    let files = 0, bytes = 0, folders = 0, left = 0;
    $('#eb_tree .eb-item:not([data-open])').each((_, el) => {
        const take = $(el).find('.eb-take')[0];
        $(el).toggleClass('eb-out', !take.checked);
        if (!take.checked) { left += 1; return; }
        if (el.dataset.folder) folders += 1; else { files += 1; bytes += parseInt(el.dataset.size || '0', 10); }
    });
    const parts = [`${files} ${files === 1 ? 'Datei' : 'Dateien'}, ${eb_size(bytes)}`];
    if (folders) parts.push(`dazu ${folders === 1 ? 'ein ganzer Ordner' : folders + ' ganze Ordner'}`);
    if (left) parts.push(`${left} nicht angekreuzt`);
    $('#eb_summary').text(`Im Paket: ${parts.join('; ')}.`);
    // Git: a history at the top kept means the class works with it
    const git_at = $('#eb_tree .eb-item[data-reason="git"]').filter((_, el) => $(el).find('.eb-take')[0].checked).map((_, el) => el.dataset.path).get();
    const top = git_at.includes('.git');
    $('input[name=eb_git]').prop('disabled', top);
    const note = $('#eb_git_note');
    if (git_at.length === 0) note.prop('hidden', true);
    else {
        const nested = git_at.filter((p) => p !== '.git').map((p) => p.replace(/\/\.git$/, '/'));
        let text = top ? 'Der Git-Verlauf des Ordners geht mit, und die Klasse arbeitet damit; ein neues Repository legt der Workspace nicht an.' : '';
        if (nested.length) text += `${text ? ' ' : ''}Der Git-Verlauf in ${nested.join(', ')} geht mit; die Klasse arbeitet dort damit${top ? '' : ', und das Repository zum Wiederherstellen lässt diesen Ordner aus'}.`;
        note.text(text).prop('hidden', false);
    }
    $('#bu_eb_build').prop('disabled', files + folders === 0);
}

function eb_preview() {
    const color = $('input[name=eb_color]:checked').val();
    const look = $('input[name=eb_look]:checked').val();
    const theme = look === 'theme';
    $('input[name=eb_color]').each((_, el) => {
        const allowed = !theme || ['blue', 'red'].includes(el.value);
        el.disabled = !allowed;
        $(el).closest('.eb-swatch').toggleClass('eb-off', !allowed);
    });
    if (theme && !['blue', 'red'].includes(color)) { $('input[name=eb_color][value=blue]').prop('checked', true); return eb_preview(); }
    const swatch = $('input[name=eb_color]:checked').closest('.eb-swatch')[0];
    const preview = $('#eb_preview')[0];
    preview.dataset.look = look;
    preview.dataset.color = color;
    // the whole theme brings its own colours (default.css)
    if (theme) preview.style.removeProperty('--eb-c');
    else preview.style.setProperty('--eb-c', swatch.style.getPropertyValue('--eb-c'));
}

function eb_render(data) {
    eb_entries = data.entries;
    const tree = $('#eb_tree').empty();
    $(`<div class='eb-head'><span>Datei</span><span>Größe</span><span>Im Paket</span><span>Drucken</span></div>`).appendTo(tree);
    for (const e of eb_entries) {
        const depth = e.p.split('/').length - 1;
        const name = e.p.split('/').pop();
        const opened = e.d && !e.r;
        const row = $(`<div class='eb-item'></div>`).attr('data-path', e.p).css('--eb-depth', depth);
        if (opened) row.attr('data-open', '1');
        if (e.d) row.attr('data-folder', '1');
        if (e.r) row.attr('data-reason', e.r);
        if (e.s != null) row.attr('data-size', e.s);
        const label = $(`<span class='eb-name'><i class='bi ${e.d ? 'bi-folder2' : 'bi-file-earmark'}' aria-hidden='true'></i></span>`);
        label.append($('<span>').text(name + (e.d ? '/' : '')));
        if (e.r) label.append($(`<span class='eb-why'>`).text(EB_REASONS[e.r] || e.r));
        row.append(label);
        row.append($(`<span class='eb-size'>`).text(e.s != null ? eb_size(e.s) : ''));
        const take = $(`<input type='checkbox' class='form-check-input eb-take'>`).attr('aria-label', `${e.p} ins Paket`)
            .prop('checked', !e.r).prop('disabled', e.r === 'link');
        const print = $(`<input type='checkbox' class='form-check-input eb-print'>`).attr('aria-label', `${e.p} drucken`)
            .prop('checked', !e.np).prop('disabled', e.r === 'link' || e.r === 'git');
        if (e.r === 'git') print.prop('checked', false);
        row.append($('<span class=eb-check>').append(take), $('<span class=eb-check>').append(print));
        take.on('change', function () {
            if (opened) eb_children(e.p).find('.eb-take:not(:disabled)').prop('checked', this.checked);
            eb_update();
        });
        print.on('change', function () {
            if (opened) eb_children(e.p).find('.eb-print').filter((_, c) => !$(c).closest('[data-reason=git]').length).prop('checked', this.checked);
            eb_update();
        });
        tree.append(row);
    }
    if (data.truncated) tree.append(`<div class='eb-more small text-body-secondary'>Der Ordner hat mehr Einträge, als hier Platz haben; nur die ersten stehen in der Liste.</div>`);
    const c = data.config || {};
    $(`input[name=eb_look][value=${c.look === 'theme' || (c.color && !c.look) ? 'theme' : 'frame'}]`).prop('checked', true);
    $(`input[name=eb_color][value=${c.color || 'yellow'}]`).prop('checked', true);
    if (!$('input[name=eb_color]:checked').length) $('input[name=eb_color][value=yellow]').prop('checked', true);
    $(`input[name=eb_git][value=${c.git === 'none' ? 'none' : 'fresh'}]`).prop('checked', true);
    const kept = (c.kept || []);
    $('#eb_kept').prop('hidden', !c.present).html(c.present ? `Den Ordner beschreibt schon eine <code>.workspace/config.yaml</code>; ihre Farbe, Git und das Drucken stehen oben.${kept.length ? ' Diese Angaben daraus bleiben, wie sie sind: ' + kept.map((k) => `<code>${$('<span>').text(k).html()}</code>`).join(', ') + '.' : ''}` : '');
    $('#eb_shared').prop('hidden', !data.shared);
    eb_preview();
    eb_update();
}

async function eb_load_folder() {
    const folder = $('#eb_folder').val();
    $('#eb_name').val(folder);
    $('#eb_status').removeClass('text-danger text-success').text('');
    $('#eb_tree').html(`<div class='p-2 text-body-secondary'><i class='bi bi-arrow-repeat bi-spin'></i> Ordner lesen …</div>`);
    try {
        eb_render(await eb_post('/api/exam_builder/tree', { folder }));
    } catch (error) {
        $('#eb_tree').empty();
        $('#eb_status').addClass('text-danger').text(error.message);
    }
}

async function open_exam_builder() {
    const modal = bootstrap.Modal.getOrCreateInstance(document.getElementById('exam_builder_modal'));
    $('#eb_status').removeClass('text-danger text-success').text('');
    modal.show();
    let folders = [];
    try {
        folders = (await eb_post('/api/exam_builder/folders', {})).folders || [];
    } catch (error) {
        $('#eb_status').addClass('text-danger').text(error.message);
    }
    $('#eb_empty').prop('hidden', folders.length > 0);
    $('#eb_body, #bu_eb_build').prop('hidden', folders.length === 0);
    $('#eb_folder').empty().prop('disabled', folders.length === 0);
    for (const f of folders) $('#eb_folder').append($('<option>').val(f).text(f));
    if (folders.length) eb_load_folder();
}

async function build_exam() {
    const files = [], print_exclude = [];
    $('#eb_tree .eb-item:not([data-open])').each((_, el) => {
        if (!$(el).find('.eb-take')[0].checked) return;
        files.push(el.dataset.path);
        if (!$(el).find('.eb-print')[0].checked && el.dataset.reason !== 'git') print_exclude.push(el.dataset.path);
    });
    const button = $('#bu_eb_build');
    button.prop('disabled', true).find('.bi').addClass('bi-spin');
    $('#eb_status').removeClass('text-danger text-success').text('Paket packen und prüfen …');
    try {
        const data = await eb_post('/api/exam_builder/build', {
            folder: $('#eb_folder').val(), group: exam_group, name: $('#eb_name').val().trim(), files, print_exclude,
            color: $('input[name=eb_color]:checked').val(), look: $('input[name=eb_look]:checked').val(),
            git: $('input[name=eb_git]:checked').val(),
        });
        bootstrap.Modal.getOrCreateInstance(document.getElementById('exam_builder_modal')).hide();
        $('#test_archive_upload_status').removeClass('text-danger').addClass('text-success')
            .text(`Leistungsüberprüfung angelegt (${data.files} ${data.files === 1 ? 'Datei' : 'Dateien'}); ${data.count === 1 ? 'eine Person nimmt' : `${data.count} Personen nehmen`} teil, sobald du sie startest. Sieh sie dir vorher mit »Workspace« unter Vorschau an.`);
        refresh_test_archives();
    } catch (error) {
        $('#eb_status').addClass('text-danger').text(error.message);
    } finally {
        button.prop('disabled', false).find('.bi').removeClass('bi-spin');
    }
}

$(function () {
    $('#bu_upload_test_archive').on('click', upload_test_archive);
    $('#bu_exam_builder').on('click', open_exam_builder);
    $('#eb_folder').on('change', eb_load_folder);
    $(document).on('change', 'input[name=eb_color], input[name=eb_look]', eb_preview);
    $('#bu_eb_build').on('click', build_exam);
    // a shared Workspace: end the sharing from the dialog
    $('#bu_eb_unshare').on('click', () => api_call('/api/unshare_server', {}, (d) => { if (d.success) $('#eb_shared').prop('hidden', true); }));
    refresh_test_archives();
});
})();
