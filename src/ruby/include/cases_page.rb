require 'cgi'
require 'json'
require 'open3'

# Cases for the development of the Workspace (CaseStore): a teacher saves a
# student's solution that the Workspace does not accept from a cell of the
# class board (class_board.js), the administration finds it at /faelle,
# downloads it and deletes it. A case holds no name or address of the
# student (docs/teacher-access.md, »Für die Entwicklung sichern«).
class Main < Sinatra::Base
    post '/api/class/save_case' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:group, :task, :email], :optional_keys => [:note],
            :max_body_length => 8192, :max_value_lengths => { :group => 200, :task => 120, :email => 200, :note => 2000 })
        group, email = data[:group].to_s, data[:email].to_s
        assert(class_groups.include?(group))
        assert(class_members(group).any? { |m| m[:email] == email })
        found = find_task(data[:task])
        halt 404 if found.nil?

        task = found[:task]
        state = neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, found[:key])).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s;
        END_OF_QUERY
        s = state ? state['s'] : {}
        # what the check read: the folder of the checked file in the
        # student's Workspace, for every language variant of the task
        checks = [task['check']] + CodeLanguages.variant_languages(task).map { |l| (task[l] || {})['check'] }.compact
        relative = checks.map { |c| c.is_a?(Hash) && c['file'].is_a?(String) ? c['file'] : nil }.compact
        workspace = "/user/#{fs_tag_for_email(email)}/workspace"
        files = File.directory?(workspace) ? CaseStore.collect(workspace, relative) : {}
        # the stored hand-in: a document as last checked, a Kata's code
        blobs = {}
        stored = submission_path(found[:key], email)
        blobs['abgabe.tar.gz'] = File.binread(stored) if File.file?(stored) && File.size(stored) <= 20 * 1024 * 1024
        meta = {
            'task' => found[:key],
            'title' => task['title'],
            'check_type' => (task['check'] || {})['type'],
            'version_now' => found[:version],
            'version_then' => s[:version],
            'status' => s[:status],
            'first_status' => s[:first_status],
            'attempts' => s[:attempts],
            'updated' => s[:updated],
            'last_answer' => s[:last_answer].to_s[0, 20_000],
            'first_answer' => s[:first_answer].to_s[0, 20_000],
            'note' => data[:note].to_s.strip,
            'saved_by' => @session_user[:email],
            # to delete the case with the account; no name or address
            'owner' => CaseStore.owner(email),
            'spec' => task,
        }
        id = CaseStore.save(meta, :files => files, :blobs => blobs)
        STDERR.puts "Case #{id}: #{found[:key]} saved by #{@session_user[:email]}"
        respond(:id => id, :files => files.size, :blobs => blobs.size)
    end

    get '/api/faelle/:id' do
        assert(admin_logged_in?)
        dir = CaseStore.dir(params[:id])
        halt 404 unless dir && File.directory?(dir)

        bytes, status = Open3.capture2('tar', 'czf', '-', '-C', File.dirname(dir), File.basename(dir), :binmode => true)
        halt 500 unless status.success?

        content_type 'application/gzip'
        attachment "fall-#{params[:id]}.tar.gz"
        bytes
    end

    post '/api/faelle/delete' do
        assert(admin_logged_in?)
        data = parse_request_data(:required_keys => [:id])
        respond(:deleted => CaseStore.delete(data[:id]))
    end

    # /faelle: the cases, the newest first, each with what it is about and
    # the buttons to download and delete it.
    def cases_page_html
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        cases = CaseStore.list
        io = +"<p class='cases-intro'>Lösungen von Lernenden, die die Prüfung nicht annahm, obwohl sie richtig aussahen; Lehrkräfte sichern sie auf der Klassenseite mit einem Klick auf die Zelle. Ohne Namen; nach #{CaseStore::KEEP_DAYS} Tagen gelöscht.</p>"
        if cases.empty?
            io << "<p class='cases-empty'>Noch kein Fall gesichert.</p>"
            return io
        end
        io << "<div class='cases-list'>"
        cases.each do |c|
            changed = c['version_then'] && c['version_then'] != c['version_now']
            io << "<article class='case-card' data-id='#{h.(c['id'])}'>"
            io << "<h2>#{h.(c['title'])} <small><code>#{h.(c['task'])}</code></small></h2>"
            io << "<p class='case-meta'>#{h.(Time.at(c['saved_at'].to_i).strftime('%d.%m.%Y, %H:%M'))}, gesichert von #{h.(c['saved_by'])}; " \
                  "Prüfung <code>#{h.(c['check_type'])}</code>, Stand »#{h.(c['status'] || 'noch nicht geprüft')}« nach #{c['attempts'].to_i} " \
                  "#{c['attempts'].to_i == 1 ? 'Versuch' : 'Versuchen'}#{changed ? ', die Aufgabe hat sich seitdem geändert' : ''}.</p>"
            io << "<p class='case-note'>#{h.(c['note'])}</p>" unless c['note'].to_s.empty?
            parts = []
            parts << "#{c['files'].size} #{c['files'].size == 1 ? 'Datei' : 'Dateien'} aus dem Workspace" unless Array(c['files']).empty?
            parts << 'die gespeicherte Abgabe' unless Array(c['blobs']).empty?
            parts << 'die Antwort' unless c['last_answer'].to_s.empty?
            io << "<p class='case-contents'>Enthält: #{h.(parts.empty? ? 'nur das Ergebnis' : parts.join(', '))}.</p>"
            io << "<p class='case-actions'><a class='btn btn-sm btn-outline-primary' href='/api/faelle/#{h.(c['id'])}' download><i class='bi bi-download' aria-hidden='true'></i>Herunterladen</a> " \
                  "<button type='button' class='btn btn-sm btn-outline-danger case-delete'><i class='bi bi-trash' aria-hidden='true'></i>Löschen</button></p>"
            io << '</article>'
        end
        io << '</div>'
        io
    end
end
