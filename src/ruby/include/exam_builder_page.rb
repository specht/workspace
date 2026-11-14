require_relative 'exam_builder'

# A Leistungsüberprüfung from a folder of the teacher's own Workspace, on the
# class page (the dialog »Aus deinem Workspace« in exams.js): the folders,
# the tree of one with what a config.yaml already there says, and the
# package built and made the class's exam (create_exam, exams_page.rb, the
# same checks as an upload). The logic is in exam_builder.rb.
class Main < Sinatra::Base
    def exam_builder_root
        "/user/#{fs_tag_for_email(@session_user[:email])}/workspace"
    end

    # the colours of the dialog: the frame's colour (as in
    # TestWorkspacePackage) and its German name
    EXAM_BUILDER_COLORS = [
        ['yellow', 'Gelb'], ['orange', 'Orange'], ['magenta', 'Magenta'],
        ['green', 'Grün'], ['blue', 'Blau'], ['red', 'Rot'],
    ].freeze

    # The dialog »Aus deinem Workspace« (exams.js fills and sends it)
    def exam_builder_modal_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        swatches = EXAM_BUILDER_COLORS.map do |key, label|
            bg, fg = TestWorkspacePackage::EXAM_FRAME_COLORS[key]
            "<label class='eb-swatch' style='--eb-c: #{bg}; --eb-f: #{fg};'><input type='radio' name='eb_color' value='#{key}'>" \
                "<span class='eb-swatch-chip' aria-hidden='true'></span>#{label}</label>"
        end.join
        "<div class='modal' id='exam_builder_modal' tabindex='-1' aria-labelledby='exam_builder_title' aria-hidden='true' style='z-index: 200000;'>" \
            "<div class='modal-dialog modal-xl modal-dialog-scrollable modal-dialog-centered'><div class='modal-content'>" \
            "<div class='modal-header'><h5 class='modal-title' id='exam_builder_title'>Leistungsüberprüfung aus deinem Workspace</h5>" \
            "<button type='button' class='btn-close' data-bs-dismiss='modal' aria-label='Schließen'></button></div>" \
            "<div class='modal-body exam-builder'>" \
            "<div class='eb-row'><label class='eb-field'><span>Ordner in deinem Workspace</span><select id='eb_folder' class='form-select'></select></label>" \
            "<label class='eb-field'><span>Name der Leistungsüberprüfung</span><input type='text' id='eb_name' class='form-control' maxlength='120'></label></div>" \
            "<p id='eb_empty' class='eb-empty' hidden>In deinem Workspace gibt es noch keinen Ordner. Leg dort einen an, etwa <code>klassenarbeit-schleifen</code>, " \
            'mit allem, was die Klasse zu Beginn haben soll: der Aufgabe, Vorlagen, Daten.</p>' \
            "<div id='eb_body' class='eb-body'>" \
            "<section><h6>Was ins Paket kommt</h6>" \
            "<p class='small text-body-secondary mb-2'>Versteckte Dateien, Build-Ordner und Git-Verläufe sind nicht angekreuzt, Links gehen nicht in ein Paket. " \
            'Was du unter <em>Drucken</em> abwählst, fehlt in der Druckansicht der Ergebnisse; Dateien, die die Klasse neu anlegt, kommen hinein.</p>' \
            "<div class='eb-tree' id='eb_tree' role='group' aria-label='Dateien des Ordners'></div>" \
            "<div id='eb_summary' class='small text-body-secondary mt-1'></div></section>" \
            "<div class='eb-settings'>" \
            "<fieldset><legend>Farbe</legend>" \
            "<div class='eb-swatches'>#{swatches}</div>" \
            "<div class='eb-looks'><label><input type='radio' name='eb_look' value='frame'> Rahmen in der Farbe, der Editor bleibt dunkel</label>" \
            "<label><input type='radio' name='eb_look' value='theme'> Alles in der Farbe (nur Blau und Rot)</label></div>" \
            "<div class='eb-preview' id='eb_preview' aria-hidden='true'><div class='eb-p-title'></div><div class='eb-p-main'><div class='eb-p-bar'></div>" \
            "<div class='eb-p-editor'><div class='eb-p-tabs'><span></span></div><div class='eb-p-code'><i></i><i></i><i></i><i></i></div></div></div><div class='eb-p-status'></div></div>" \
            "<p class='small text-body-secondary mb-0'>So sieht man auch von hinten im Raum, wer im Workspace der Leistungsüberprüfung ist. " \
            'Zwei Gruppen nebeneinander (A und B) bekommen zwei Farben.</p></fieldset>' \
            "<fieldset><legend>Git</legend>" \
            "<label class='eb-git'><input type='radio' name='eb_git' value='fresh'><span>Zum Wiederherstellen: ein neues Repository mit dem Commit »Ausgangszustand«" \
            "<small>Mit <code>git diff</code> und <code>git restore</code> kommen die Lernenden zurück; mit Git arbeiten müssen sie nicht.</small></span></label>" \
            "<label class='eb-git'><input type='radio' name='eb_git' value='none'><span>Kein Repository: Git ist Teil der Aufgabe" \
            "<small>Die Lernenden legen es selbst an (<code>git init</code>) und tragen Namen und Adresse selbst ein.</small></span></label>" \
            "<p id='eb_git_note' class='small eb-git-note' hidden></p></fieldset>" \
            '</div>' \
            "<p id='eb_kept' class='small text-body-secondary mt-3 mb-0' hidden></p>" \
            '</div></div>' \
            "<div class='modal-footer'><span id='eb_status' class='small me-auto' role='status' aria-live='polite'></span>" \
            "<button type='button' class='btn btn-outline-secondary' data-bs-dismiss='modal'>Abbrechen</button>" \
            "<button type='button' id='bu_eb_build' class='btn btn-primary'><i class='bi bi-box-seam'></i>Für #{h.(group)} bereitstellen</button></div>" \
            '</div></div></div>'
    end

    post '/api/exam_builder/folders' do
        assert(teacher_logged_in?)
        respond(:folders => ExamBuilder.folders(exam_builder_root))
    end

    post '/api/exam_builder/tree' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:folder], :max_value_lengths => { :folder => 255 })
        path = ExamBuilder.folder_path(exam_builder_root, data[:folder])
        tree = ExamBuilder.tree(path)
        existing = ExamBuilder.existing_config(path)
        exam = existing['exam'].is_a?(Hash) ? existing['exam'] : {}
        git = existing['git'].is_a?(Hash) ? existing['git'] : {}
        print = existing['print'].is_a?(Hash) ? existing['print'] : {}
        before = Array(print['exclude']).select { |x| x.is_a?(String) }
        # what the config.yaml there leaves out of the printout starts
        # unticked under »Drucken« (np: not printed)
        tree[:entries].each { |e| e[:np] = true if before.any? { |pattern| ExamBuilder.matches?(pattern, e[:p]) } }
        respond(:entries => tree[:entries], :truncated => tree[:truncated],
                :config => {
                    :present => !existing.empty?,
                    :color => exam['color'], :look => exam['look'], :git => git['mode'],
                    # what the dialog does not show and keeps
                    :kept => (existing.keys - ['workspace_package', 'exam', 'git', 'print']) +
                             (exam.keys - ['color', 'look']).map { |k| "exam.#{k}" } +
                             (print.keys - ['exclude']).map { |k| "print.#{k}" },
                })
    rescue ExamBuilder::Refused => e
        respond(:error => e.message)
    end

    post '/api/exam_builder/build' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:folder, :group, :name, :files, :color, :look, :git],
                                  :optional_keys => [:print_exclude],
                                  :types => { :files => Array, :print_exclude => Array },
                                  :max_body_length => 1024 * 1024,
                                  :max_value_lengths => { :folder => 255, :group => 255, :name => 120, :color => 20, :look => 20, :git => 20 })
        group = data[:group].to_s
        halt 403 unless class_groups.include?(group)
        path = ExamBuilder.folder_path(exam_builder_root, data[:folder])
        chosen = data[:files].map(&:to_s)
        raise ExamBuilder::Refused, 'Wähle mindestens eine Datei aus.' if chosen.empty?

        # Git: the recovery repository (fresh) or none; a .git at the top
        # chosen keeps its history (preserve), a Git task
        git_mode = chosen.include?('.git') ? 'preserve' : (data[:git] == 'none' ? 'none' : 'fresh')
        files, = ExamBuilder.expand(path, chosen)
        raise ExamBuilder::Refused, 'In der Auswahl ist keine Datei.' if files.empty?

        existing = ExamBuilder.existing_config(path)
        before = existing['print'].is_a?(Hash) ? Array(existing['print']['exclude']) : []
        unprinted = Array(data[:print_exclude]).map(&:to_s).select { |rel| ExamBuilder.safe_rel?(rel) }
        listed = ExamBuilder.tree(path)[:entries].map { |e| e[:p] }
        config = ExamBuilder.config_yaml(existing, :color => data[:color].to_s, :look => data[:look].to_s, :git_mode => git_mode,
                                         :print_exclude => ExamBuilder.print_patterns(path, unprinted, :before => before, :listed => listed))
        blob = ExamBuilder.pack(path, files, config, :uid => WORKSPACE_UID, :gid => WORKSPACE_GID)
        name = data[:name].to_s.gsub(%r{[/\\\x00-\x1f]}, ' ').squeeze(' ').strip
        name = data[:folder].to_s if name.empty?
        count = create_exam(blob, "#{name}.tar.gz", group)
        STDERR.puts "#{@session_user[:email]} built an exam for #{group} from a Workspace folder (#{files.size} files)"
        respond(:count => count, :files => files.size)
    rescue StandardError => e
        exam_error_response(e)
    end
end
