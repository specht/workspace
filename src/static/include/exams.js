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

function refresh_test_archives() {
    if (!document.getElementById('table_my_test_archives')) return;
    api_call('/api/get_my_test_archives', {}, function(data) {
        const tbody = $('#table_my_test_archives tbody').empty();
        if (!data.success) return;
        // this class's exams, and those uploaded before exams belonged to a
        // class (in the profile), to be given to one
        exam_entries = data.entries.filter((e) => e.group === exam_group || !e.group)
            .sort((a, b) => (!a.group) - (!b.group) || (b.ts || 0) - (a.ts || 0));
        if (exam_entries.length === 0) {
            tbody.append(`<tr><td colspan='6' class='text-body-secondary'>Noch keine für diese Klasse. Lade oben ein Paket hoch; alle in der Klasse nehmen dann teil.</td></tr>`);
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
                const assign = $(`<button type='button' class='btn btn-link btn-sm px-0 exam-assign'><i class='bi bi-box-arrow-in-down-right'></i>Noch keiner Klasse zugeordnet: dieser zuordnen</button>`);
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

$(function () {
    $('#bu_upload_test_archive').on('click', upload_test_archive);
    refresh_test_archives();
});
})();
