require 'fileutils'
require 'digest'
require 'base64'
# Tasks on the pages: checking an answer and storing the result, the
# options of a task, hand-ins (download, view, text), the states of a
# person's tasks, the answers of a group for its teacher, deleting answers
# and hand-ins, the class board's template status; and the helpers these
# share (the runners for SQL and Cypher, finding a task, its variables, the
# example answers). Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    # Checks an answer and, if it counts, remembers it (docs/aufgaben.md).
    post '/api/task/check' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task, :answer], :optional_keys => [:language],
            :max_body_length => 16384,
            :max_value_lengths => {:task => 120, :answer => TaskStore::MAX_ANSWER})
        found = find_task(data[:task], data[:language])
        halt 404 if found.nil?

        result = TaskCheck.check(found[:task], data[:answer],
            :variables => task_variables(found[:task], found[:key]),
            :workspace => task_workspace_files,
            :git => lambda { |repo| task_git_state(repo) },
            :runner => lambda { |command, seconds|
                workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => command,
                    :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}", :seconds => [[seconds.to_i, 1].max, 30].min)
            },
            # What the person typed runs as a guest: it can read /workspace but
            # not change or delete anything there (rule 1, checks only read).
            :guest_runner => lambda { |command, seconds|
                workspace_runtime.run_command(fs_tag_for_email(@session_user[:email]), :command => command,
                    :uid => GUEST_UID, :home => '/tmp', :seconds => [[seconds.to_i, 1].max, 30].min)
            },
            :sql => %w[sql schema].include?(found[:task]['check']['type']) ? task_sql_runner : nil,
            :sql_env => %w[sql schema].include?(found[:task]['check']['type']) ? task_sql_env : {},
            :cypher => found[:task]['check']['type'] == 'cypher' ? task_cypher_runner : nil)
        if result.problem == :reference_failed
            STDERR.puts "SQL task #{found[:key]}: the reference query failed for #{@session_user[:email]}"
        end
        answer = data[:answer]
        if (plan = result.plan)
            # submit: copy the files now; the stored answer says what was handed in.
            files = plan.files.size
            answer = [data[:answer].to_s.strip, "#{files == 1 ? '1 Datei' : "#{files} Dateien"}, #{SubmissionStore.size_text(plan.total)}"].reject(&:empty?).join(': ')
            begin
                SubmissionStore.write(task_workspace_files, plan, submission_path(found[:key]))
            rescue StandardError => e
                STDERR.puts "Submission #{found[:key]} failed: #{e.class}: #{e.message}"
                result = TaskCheck::Result.new(:status => :unavailable,
                    :message => 'Die Abgabe hat nicht geklappt, vermutlich hat sich gerade eine Datei geändert. Versuch es gleich noch einmal.')
            end
        end
        state = result.counts? ? store_task_result(found, result.status.to_s, answer) : nil
        respond(
            :status => result.status.to_s,
            :message => render_task_markdown(result.message),
            :solution => render_task_markdown(result.solution, :lineno => true),
            :read => render_task_markdown(result.read),
            :warnings => (result.warnings || []).map { |w| render_task_markdown(w) },
            :state => state,
        )
    end
    # A document task (DocumentCheck): the student's file comes as a form
    # upload, goes to the office service, which reads it and draws its
    # pages, and is checked against the task's rules. The file itself is not
    # kept: only the result and a line for the class overview (which points
    # are open) are stored, as for every task.
    post '/api/task/document' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        key = params['task'].to_s
        halt 400 if key.empty? || key.size > 120
        found = find_task(key)
        halt 404 if found.nil? || found[:task]['check']['type'] != 'document'
        entry = params['file']
        halt 400 unless entry.is_a?(Hash) && entry[:tempfile] || entry.is_a?(Hash) && entry['tempfile']
        upload = entry[:tempfile] || entry['tempfile']
        name = File.basename((entry[:filename] || entry['filename']).to_s.encode('UTF-8', :invalid => :replace, :undef => :replace))[0, 200]
        bytes = upload.read(OfficeClient::MAX_BYTES + 1).to_s
        begin
            model = OfficeClient.analyze(bytes, name)
        rescue OfficeClient::Unavailable => e
            STDERR.puts "Document task #{found[:key]}: #{e.message}"
            respond(:status => 'unavailable',
                :message => render_task_markdown('Dokumente kann ich gerade nicht lesen. Versuch es in ein paar Minuten noch einmal.'))
            return
        ensure
            upload.close! rescue nil
        end
        result = TaskCheck.check(found[:task], model)
        # the document itself, as the person's last version of this task:
        # to go on with at home or elsewhere (»Deine letzte Fassung«), for
        # their teacher to look at; replaced by the next one, gone with
        # »Ergebnis löschen« and the account (submission_path)
        stored = nil
        if model.is_a?(Hash) && !model['error']
            name = name.gsub(/[\x00-\x1f\/\\]/, '_').sub(/\A\.+/, '')[0, 90]
            name = 'dokument.docx' if name.empty?
            SubmissionStore.write_text(submission_path(found[:key]), name, bytes)
            stored = document_stored(found[:key])
        end
        state = nil
        if result.counts?
            state = store_task_result(found, result.status.to_s, DocumentCheck.summary(name, result.report[:items]))
        end
        report = link_document_pages(result.report)
        if report
            report = report.merge(:items => report[:items].map { |i| i.merge(:message => i[:message] ? render_task_markdown(i[:message]) : nil) })
        end
        respond(
            :status => result.status.to_s,
            :message => render_task_markdown(result.message),
            :solution => render_task_markdown(result.solution, :lineno => true),
            :read => render_task_markdown(result.read),
            :report => report,
            :state => state,
            :stored => stored,
        )
    end
    # The last version of a document task kept for a person: its name, when,
    # and where to fetch it; nil without one.
    # The drawn pages of a result as addresses instead of inside it: each
    # page's SVG goes to the cache under the hash of its content (the card's
    # answer and the hand-in page stay small), kept two days.
    DOCUMENT_PAGES = '/webcache/document-pages'
    def link_document_pages(report)
        return report unless report && report[:pages]
        FileUtils.mkdir_p(DOCUMENT_PAGES)
        prune_document_pages
        pages = report[:pages].map do |page|
            src = page[:src].to_s
            next page unless src.start_with?('data:image/svg+xml;base64,')
            svg = Base64.decode64(src.sub('data:image/svg+xml;base64,', ''))
            sha = Digest::SHA1.hexdigest(svg)
            path = File.join(DOCUMENT_PAGES, "#{sha}.svg")
            File.binwrite(path, svg) unless File.file?(path)
            page.merge(:src => "/api/document_page/#{sha}.svg")
        end
        report.merge(:pages => pages)
    end

    # at most once an hour: pages older than two days go
    def prune_document_pages
        @@document_pages_pruned ||= 0
        return if Time.now.to_i - @@document_pages_pruned < 3600
        @@document_pages_pruned = Time.now.to_i
        Dir.glob(File.join(DOCUMENT_PAGES, '*.svg')).each do |f|
            File.delete(f) if File.mtime(f) < Time.now - 2 * 86400
        rescue SystemCallError
            nil
        end
    end

    # a drawn page: only shown as <img> (nothing in it runs), so a sandbox
    # if someone opens it on its own
    get '/api/document_page/:name' do
        name = params[:name].to_s
        halt 404 unless name.match?(/\A[0-9a-f]{40}\.svg\z/)
        path = File.join(DOCUMENT_PAGES, name)
        halt 404 unless File.file?(path)
        response.headers['Cache-Control'] = 'private, max-age=172800'
        response.headers['Content-Security-Policy'] = "default-src 'none'; style-src 'unsafe-inline'; img-src data:; sandbox"
        respond_raw_with_mimetype(File.binread(path), 'image/svg+xml')
    end

    def document_stored(task_key, email = @session_user[:email])
        path = submission_path(task_key, email)
        return nil unless File.file?(path)

        name = SubmissionStore.read_file(path, :max => 0)&.first
        activity, id = task_key.split('/', 2)
        { :name => name, :at => File.mtime(path).to_i, :url => "/api/task/document_file/#{activity}/#{id}" }
    end
    # The kept document as it was uploaded: one's own, or (email=…) that of
    # a student in a group the logged-in teacher looks after.
    get '/api/task/document_file/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil? || found[:task]['check']['type'] != 'document'
        email = submission_owner_for(params)
        file = SubmissionStore.read_file(submission_path(found[:key], email))
        halt 404 if file.nil?
        response.headers['Cache-Control'] = 'no-store'
        types = { 'docx' => 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
                  'odt' => 'application/vnd.oasis.opendocument.text', 'doc' => 'application/msword', 'rtf' => 'application/rtf' }
        respond_raw_with_mimetype_and_filename(file[1], types[File.extname(file[0]).delete('.').downcase] || 'application/octet-stream', file[0])
    end
    # A document hand-in with its marked pages, as the student saw it at the
    # last check: one's own, or (email=…) that of a student in a group the
    # logged-in teacher looks after (the class page links it). The stored
    # file is read and checked again, so the page shows today's rules.
    get '/abgabe/dokument/:activity/:id' do
        this_is_a_page_for_logged_in_users!
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil? || found[:task]['check']['type'] != 'document'
        email = submission_owner_for(params)
        file = SubmissionStore.read_file(submission_path(found[:key], email))
        halt 404 if file.nil?
        report = nil
        problem = nil
        begin
            result = TaskCheck.check(found[:task], OfficeClient.analyze(file[1], file[0]))
            report = link_document_pages(result.report)
            report = report.merge(:items => report[:items].map { |i| i.merge(:message => i[:message] ? render_task_markdown(i[:message]) : nil) }) if report
            problem = result.message unless report
        rescue OfficeClient::Unavailable
            problem = 'Dokumente kann ich gerade nicht lesen. Versuch es in ein paar Minuten noch einmal.'
        end
        Main.load_invitations()
        name = (@@invitations[email] || {})[:name] || email
        at = Time.at(File.mtime(submission_path(found[:key], email)).to_i).strftime('%d.%m.%Y, %H:%M Uhr')
        download = "/api/task/document_file/#{params[:activity]}/#{params[:id]}#{email == @session_user[:email] ? '' : "?email=#{CGI.escape(email)}"}"
        h = ->(t) { CGI.escape_html(t.to_s) }
        content = +"<div class='container document-handin'>"
        content << "<p class='document-handin-what'><a href='/#{h.(params[:activity])}'>#{h.(PageKinds.plain_title(@@content[params[:activity]] || {}) || params[:activity])}</a>, #{h.(found[:task]['title'])}</p>"
        content << "<h1>#{h.(name)}</h1>"
        content << "<p class='document-handin-file'><i class='bi bi-file-earmark-word' aria-hidden='true'></i><strong>#{h.(file[0])}</strong>, zuletzt geprüft am #{h.(at)}. " \
            "<a href='#{h.(download)}' download><i class='bi bi-download' aria-hidden='true'></i>Herunterladen</a></p>"
        content << "<p>#{h.(problem)}</p>" if problem
        # the card's frame, so that the result is drawn as on the card
        content << "<div class='task-card document-handin-card' data-type='document'><div class='task-feedback'><div class='task-feedback-body'></div></div></div></div>"
        content << "<script>window.DOCUMENT_HANDIN = #{report.to_json.gsub('</', '<\\/')};</script>" if report
        content << "<script src='/include/document_task.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        content << "<script>if (window.DOCUMENT_HANDIN) DocumentTask.render(document.querySelector('.document-handin-card'), window.DOCUMENT_HANDIN);</script>"
        path = request.path
        running_tests = my_running_tests()
        template = File.read(File.join(@@static_dir, '_template.html'))
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, content, binding), request.path)
        # through respond_raw: the after filter (helper.rb) puts JSON in
        # place of any other answer with status 200
        response.headers['Cache-Control'] = 'no-store'
        respond_raw_with_mimetype(html, 'text/html; charset=utf-8')
    end

    # Files a submit task with pick: offers from the student's Workspace.
    post '/api/task/options' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:task], :max_value_lengths => {:task => 120})
        found = find_task(data[:task])
        halt 404 if found.nil? || found[:task]['check']['pick'].nil?

        ws = task_workspace_files
        options = ws ? SubmissionStore.candidates(ws, found[:task]['check']['pick']) : []
        respond(:options => options)
    end
    # A submission as .tar.gz: the logged-in person's own, or, with ?email=,
    # that of a student in a group the logged-in teacher looks after.
    # Whose hand-in a request may see: one's own, or (email=…) one of a
    # group the logged-in teacher looks after.
    def submission_owner_for(params)
        email = @session_user[:email]
        if params['email'] && params['email'] != email
            Main.load_invitations()
            allowed = class_groups.any? { |g| (@@group_members[g] || []).include?(params['email']) }
            halt 403 unless allowed
            email = params['email']
        end
        email
    end
    get '/api/task/submission/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        path = submission_path(found[:key], email)
        halt 404 unless File.file?(path)

        suffix = email == @session_user[:email] ? '' : "-#{email.split('@').first.gsub(/[^a-zA-Z0-9._-]/, '')}"
        respond_raw_with_mimetype_and_filename(File.binread(path), 'application/gzip',
            "#{params[:activity]}-#{params[:id]}#{suffix}.tar.gz")
    end
    # The viewer on the class page: the files of a hand-in and the signed
    # address under which they run on abgaben.<host>.
    get '/api/task/submission_view/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        path = submission_path(found[:key], email)
        halt 404 unless File.file?(path)

        token = SubmissionView.token(TaskStore.state_key(email, found[:key]), :secret => SUBMISSION_VIEW_SECRET)
        files = SubmissionView.files(path).map do |f|
            f.merge(:text => SubmissionView.text?(f[:path]) && f[:size] <= SubmissionView::MAX_TEXT,
                    :html => SubmissionView.html?(f[:path]))
        end
        response.headers['Cache-Control'] = 'no-store'
        respond(:files => files, :root => "#{SUBMISSION_VIEW_ROOT}/v/#{token}/",
            :submitted_at => File.mtime(path).to_i,
            :download => "/api/task/submission/#{params[:activity]}/#{params[:id]}#{email == @session_user[:email] ? '' : "?email=#{CGI.escape(email)}"}")
    end
    # The text of one file of a hand-in, for the source view.
    get '/api/task/submission_text/:activity/:id' do
        assert(user_logged_in?)
        found = find_task("#{params[:activity]}/#{params[:id]}")
        halt 404 if found.nil?
        email = submission_owner_for(params)
        archive = submission_path(found[:key], email)
        file = SubmissionView.clean_path(params['path'])
        halt 404 unless File.file?(archive) && file && SubmissionView.text?(file)
        bytes = SubmissionView.read(archive, file, :max => SubmissionView::MAX_TEXT)
        halt 404 if bytes.nil?
        response.headers['Cache-Control'] = 'no-store'
        respond(:text => bytes.force_encoding('UTF-8').scrub('�'))
    end
    # abgaben.<host>/v/<token>/<path>: a file of a hand-in, sandboxed
    # (SubmissionView). The token is the only permission; no cookies.
    get '/v/:token/*' do
        halt 404 unless request.host == SUBMISSION_VIEW_HOST
        state_key = SubmissionView.verify(params[:token], :secret => SUBMISSION_VIEW_SECRET)
        halt 404 if state_key.nil?
        archive = File.join(SUBMISSIONS_ROOT, "#{state_key}.tar.gz")
        halt 404 unless File.file?(archive)
        rel = params['splat'].first.to_s
        wanted = rel.empty? ? '' : SubmissionView.clean_path(rel)
        halt 404 if wanted.nil?
        file = SubmissionView.resolve(SubmissionView.files(archive), wanted)
        halt 404 if file.nil?
        # A folder without its slash: relative addresses inside need it.
        redirect "/v/#{params[:token]}/#{file}" if file != wanted && !rel.end_with?('/')
        bytes = SubmissionView.read(archive, file)
        halt 404 if bytes.nil?
        SubmissionView.headers(file, WEB_ROOT).each { |k, v| response.headers[k] = v }
        respond_raw_with_mimetype(SubmissionView.for_viewer(file, bytes), SubmissionView.content_type(file))
    end
    # Teachers: run a Workspace task (TaskCheck::LIVE_TYPES) for
    # every student of a group right now. Reads only; stores nothing, so the
    # students' own results stay as they are.
    post '/api/class/check_now' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :task], :max_value_lengths => {:group => 200, :task => 120})
        halt 403 unless class_groups.include?(data[:group])
        found = find_task(data[:task])
        halt 404 if found.nil?
        type = found[:task]['check']['type']
        halt 400 unless TaskCheck::LIVE_TYPES.include?(type)

        members = class_members(data[:group])
        results = {}
        lock = Mutex.new
        queue = Queue.new
        members.each { |m| queue << m }
        workers = Array.new([4, members.size].min) do
            Thread.new do
                loop do
                    m = begin
                        queue.pop(true)
                    rescue ThreadError
                        break
                    end
                    fs_tag = fs_tag_for_email(m[:email])
                    path = "/user/#{fs_tag}/workspace"
                    ws = File.directory?(path) ? WorkspaceFiles.new(path) : nil
                    git = lambda do |repo|
                        output = workspace_runtime.git_status(fs_tag, :repo => repo,
                            :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}", :script => GitCheck::STATUS_SCRIPT, :timeout => 30,
                            :host_data_path => PATH_TO_HOST_DATA)
                        GitCheck.complete?(output) ? GitCheck.parse(output) : nil
                    end
                    result = begin
                        TaskCheck.check(found[:task], '', :variables => {}, :workspace => ws, :git => git)
                    rescue StandardError => e
                        STDERR.puts "check_now #{found[:key]} #{m[:email]}: #{e.class}: #{e.message}"
                        TaskCheck::Result.new(:status => :unavailable, :message => 'Konnte nicht geprüft werden.')
                    end
                    lock.synchronize do
                        results[m[:email]] = {
                            :status => result.status.to_s,
                            :problem => result.problem.to_s,
                            :message => result.message,
                        }
                    end
                end
            end
        end
        workers.each(&:join)
        # Markdown is rendered here, not in the threads: one renderer is shared.
        results.each_value { |r| r[:message] = render_task_markdown(r[:message]) }
        respond(:results => results, :checked_at => Time.now.to_i)
    end
    # Is the logged-in person's clone of the page's template out of date?
    # Looks only; the notice tells them to run git pull themselves.
    post '/api/template/status' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:slug], :max_value_lengths => {:slug => 120})
        template = (@@content[data[:slug]] || {})[:template]
        halt 404 if template.nil?

        status = template_status_for(@session_user[:email], template)
        respond(:status => status.to_s,
            :notice => status == :outdated ? render_task_markdown(TemplateFreshness.notice(template)) : nil)
    end
    # Teachers: the same look for every student of a group. Stores nothing,
    # tells no one; the teacher decides whether to say »git pull«.
    post '/api/class/template_status' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :slug], :optional_keys => [:force],
            :max_value_lengths => {:group => 200, :slug => 120})
        halt 403 unless class_groups.include?(data[:group])
        template = (@@content[data[:slug]] || {})[:template]
        halt 404 if template.nil?

        # force: ask the template's host now instead of the remembered answer.
        latest = data[:force] == 'yes' ? @@template_latest.refresh(template[:url]) : @@template_latest.get(template[:url])
        members = class_members(data[:group])
        results = {}
        if latest
            lock = Mutex.new
            queue = Queue.new
            members.each { |m| queue << m }
            workers = Array.new([4, members.size].min) do
                Thread.new do
                    loop do
                        m = begin
                            queue.pop(true)
                        rescue ThreadError
                            break
                        end
                        status = begin
                            template_status_for(m[:email], template)
                        rescue StandardError => e
                            STDERR.puts "template_status #{data[:slug]} #{m[:email]}: #{e.class}: #{e.message}"
                            :unavailable
                        end
                        lock.synchronize { results[m[:email]] = status.to_s }
                    end
                end
            end
            workers.each(&:join)
        end
        respond(:results => results, :reachable => !latest.nil?, :folder => template[:folder],
            :students_see_notice => tasks_enabled_for_everyone? ? true : false, :checked_at => Time.now.to_i)
    end
    # The logged-in person's stored answers for the tasks of one activity.
    post '/api/task/states' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:activity], :max_value_lengths => {:activity => 64})
        activity = data[:activity]
        assert(activity =~ /\A[a-z0-9_-]+\z/)
        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        halt 404 if entry.nil?

        states = {}
        correct = {}
        given = {}
        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :prefix => "#{activity}/").each do |row|
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.task STARTS WITH $prefix
            RETURN s;
        END_OF_QUERY
            stored = row['s']
            id = stored[:task].to_s.split('/', 2)[1]
            task = entry[:tasks][:by_id][id]
            next if task.nil?
            states[id] = TaskStore.public_state(stored, TaskSpec.check_version(task))
            # a document task: the last version kept (»Deine letzte Fassung«)
            states[id][:stored] = document_stored(stored[:task].to_s) if task['check']['type'] == 'document'
            next unless Main::KATA_TYPES.include?(task['check']['type'])

            # a Kata handed in to be shown in class (KataHandIns)
            states[id][:handed_in] = File.file?(submission_path(stored[:task].to_s))
            correct[id] = correct_codes(stored)
            given[id] = stored[:own_filled] if stored[:own_filled]
        end
        # Cards that build on functions of earlier ones (OwnFunctions): the
        # person's own functions in their starters and answers.
        starters, answers = OwnFunctions.for_person(entry[:tasks][:by_id], correct, states.transform_values { |s| s[:answer] }.compact, given)
        answers.each { |id, answer| states[id][:answer] = answer }
        respond(:states => states, :starters => starters)
    end
    # The answers of a group to the open tasks of an activity, for the
    # teacher's view under the cards (tasks.js).
    post '/api/task/group_answers' do
        assert(user_logged_in?)
        assert(tasks_visible?)
        data = parse_request_data(:required_keys => [:activity, :group],
            :max_value_lengths => {:activity => 64, :group => 256})
        group = data[:group]
        assert(class_groups.include?(group))
        activity = data[:activity]
        assert(activity =~ /\A[a-z0-9_-]+\z/)
        slug, entry = @@content.find { |_, c| (c[:tasks] || {})[:activity] == activity }
        halt 404 if entry.nil?

        tasks = OpenAnswers.task_ids(tasks_in_variant(entry, group_page_variant(slug, group))).to_h { |id| [id, { :version => TaskSpec.check_version(entry[:tasks][:by_id][id]) }] }
        members = class_members(group)
        states = class_board_states(members.map { |m| m[:email] }, activity)
        respond(:group => group, :members => members.size, :answers => OpenAnswers.collect(members, tasks, states))
    end
    # What a teacher last chose under the open tasks: example answers, the
    # answers of a group (which becomes the current group, as on /klasse),
    # or nothing.
    post '/api/task/answer_view' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:view], :optional_keys => [:group],
            :max_value_lengths => {:view => 16, :group => 256})
        assert(OpenAnswers::VIEWS.include?(data[:view]))
        prefs = class_prefs
        prefs['answer_view'] = data[:view]
        if data[:group]
            assert(class_groups.include?(data[:group]))
            prefs['group'] = data[:group]
        end
        store_class_prefs(prefs)
        respond(:view => data[:view])
    end
    # Deletes the logged-in person's answer and attempts for one task. Always
    # allowed, also when tasks are switched off again.
    post '/api/task/delete' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:task], :max_value_lengths => {:task => 120})
        task = data[:task]
        assert(task =~ /\A[a-z0-9_-]+\/[a-z0-9-]+\z/)
        email = @session_user[:email]
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => TaskStore.state_key(email, task))
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            DETACH DELETE s;
        END_OF_QUERY
        notify_meeting_progress(email, task.split('/').first)
        @@task_results_changed_at = Time.now
        neo4j_query(<<~END_OF_QUERY, :email => email, :task => task)
            MATCH (a:TaskAttempt {task: $task})-[:BY]->(:User {email: $email})
            DETACH DELETE a;
        END_OF_QUERY
        FileUtils.rm_f(submission_path(task, email))
        respond(:deleted => true)
    end
    # A new round: all hand-ins of one task in one group go, as if each person
    # had pressed »Abgabe löschen« (state, attempts, file). Only for the
    # group's teachers; only members of that group, not the teacher's own.
    post '/api/class/delete_submissions' do
        assert(user_logged_in?)
        Main.load_invitations()
        data = parse_request_data(:required_keys => [:group, :task], :max_value_lengths => {:group => 200, :task => 120})
        halt 403 unless class_groups.include?(data[:group])
        found = find_task(data[:task])
        halt 404 if found.nil?
        halt 400 unless found[:task]['check']['type'] == 'submit'

        task = found[:key]
        emails = class_members(data[:group]).map { |m| m[:email] }
        deleted = emails.count { |email| File.file?(submission_path(task, email)) }
        neo4j_query(<<~END_OF_QUERY, :keys => emails.map { |email| TaskStore.state_key(email, task) })
            MATCH (:User)-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.key IN $keys
            DETACH DELETE s;
        END_OF_QUERY
        neo4j_query(<<~END_OF_QUERY, :emails => emails, :task => task)
            MATCH (a:TaskAttempt {task: $task})-[:BY]->(u:User)
            WHERE u.email IN $emails
            DETACH DELETE a;
        END_OF_QUERY
        emails.each { |email| FileUtils.rm_f(submission_path(task, email)) }
        STDERR.puts "#{@session_user[:email]} deleted #{deleted} hand-ins of #{task} in #{data[:group]}"
        respond(:deleted => deleted)
    end

    # sql tasks: the person's own MySQL database, logged in as them (never
    # as root), read-only per query (SqlCheck.mysql_runner).
    def task_sql_runner
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        return nil if login.nil? || !DatabaseIdentity.valid?(login)

        SqlCheck.mysql_runner(:host => 'mysql', :username => login, :database => login,
            :password => Main.gen_password_for_email(email, MYSQL_PASSWORD_SALT))
    end
    # cypher tasks: the person's own Neo4j database on the students' server,
    # logged in as them (never as an administrator), read-only per query
    # (CypherCheck.runner). nil until their Neo4j login exists.
    def task_cypher_runner
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        return nil if login.nil? || !DatabaseIdentity.valid?(login)

        CypherCheck.runner(:uri => 'bolt://neo4j:7687', :login => login,
            :password => Main.gen_password_for_email(email, NEO4J_PASSWORD_SALT))
    end
    # What an sql check names when it cannot run: which database, which login,
    # whether the Workspace runs, what /workspace/.my.cnf says (never the
    # password). The last two are only looked up when needed.
    def task_sql_env
        email = @session_user[:email]
        login = @session_user[:db_login] || db_login_for_email(email)
        {
            :login => login, :database => login, :host => 'mysql',
            :workspace => lambda {
                state = begin
                    get_server_state(fs_tag_for_email(email))
                rescue StandardError
                    nil
                end
                state.nil? ? nil : state[:running] == true
            },
            :my_cnf => lambda {
                ws = task_workspace_files
                next nil if ws.nil?
                next :missing unless ws.entry('.my.cnf')[:type] == :file

                text = begin
                    ws.read_text('.my.cnf', :max => 4096)
                rescue StandardError
                    ''
                end
                text.scan(/^\s*(user|database)\s*=\s*(\S+)\s*$/).to_h
            },
        }
    end
    # The task for 'activity/id', with its check version, or nil.
    # language: the card's programming language (CodeLanguages); the task
    # comes back as it is in that language, with the version of all of them.
    def find_task(key, language = nil)
        activity, id = key.to_s.split('/', 2)
        return nil unless activity.to_s =~ /\A[a-z0-9_-]+\z/ && id.to_s =~ TaskSpec::ID_PATTERN

        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        task = entry && entry[:tasks][:by_id][id]
        return nil unless task

        version = TaskSpec.check_version(task)
        task = CodeLanguages.variant(task, language) if CodeLanguages.known?(language)
        { :key => "#{activity}/#{id}", :task => task, :version => version }
    end
    # Values of the ${...} variables a task uses, for the logged-in person.
    # A value is nil when it cannot be known right now (Workspace not running).
    def task_variables(task, key)
        names = TaskCheck.variables_used(task['check'])
        email = @session_user[:email]
        vars = {}
        if names.include?('workspace.ip') || names.include?('workspace.mac')
            state = begin
                get_server_state(fs_tag_for_email(email))
            rescue StandardError
                nil
            end
            running = state && state[:running]
            vars['workspace.ip'] = running ? state[:ip] : nil if names.include?('workspace.ip')
            vars['workspace.mac'] = running ? state[:mac] : nil if names.include?('workspace.mac')
        end
        if names.include?('workspace.live_url')
            server_tag = @session_user[:server_tag]
            vars['workspace.live_url'] = server_tag ? WEB_ROOT.sub('//', "//#{server_tag}-5500.") : nil
        end
        if names.include?('person.codeword')
            vars['person.codeword'] = TaskCheck.codeword("#{LOGIN_CODE_SALT}:task-codeword", email, key)
        end
        vars
    end
    # Messages carry text from students' files: rendered and then filtered
    # to harmless HTML (TaskHtml). Feedback boxes are light and style plain
    # code themselves (.task-feedback pre code), so no highlighting here.
    def render_task_markdown(markdown, lineno: false)
        TaskHtml.render(markdown, :lineno => lineno)
    end
    # The student's Workspace folder on the server, read-only, or nil if the
    # Workspace was never opened.
    def task_workspace_files
        path = "/user/#{fs_tag_for_email(@session_user[:email])}/workspace"
        File.directory?(path) ? WorkspaceFiles.new(path) : nil
    end
    # git status of a repository, read as the student in their container, or
    # in a reader container if it is not running (WorkspaceRuntime).
    def task_git_state(repo)
        WorkspaceFiles.clean(repo)
        output = workspace_runtime.git_status(fs_tag_for_email(@session_user[:email]),
            :repo => repo, :uid => "#{WORKSPACE_UID}:#{WORKSPACE_GID}",
            :script => GitCheck::STATUS_SCRIPT, :timeout => 30, :host_data_path => PATH_TO_HOST_DATA)
        GitCheck.complete?(output) ? GitCheck.parse(output) : nil
    end
    # A submit task that shows its scenes with the Anaglyph viewer, or 404.
    def anaglyph_preview_task(key)
        found = find_task(key)
        check = found && found[:task]['check']
        halt 404 unless check && check['type'] == 'submit' && check['preview'] == 'anaglyph'
        found
    end
    # Whose submission »who« is (ich: one's own, otherwise the state key), if
    # the logged-in person may see it: their own, or one of a group they teach.
    def anaglyph_submission_owner(task_key, who)
        own = @session_user[:email]
        return own if who == 'ich' || who == TaskStore.state_key(own, task_key)
        Main.load_invitations()
        class_groups.each do |group|
            (@@group_members[group] || []).each do |email|
                return email if TaskStore.state_key(email, task_key) == who
            end
        end
        nil
    end
    # preview: anaglyph tasks on the class page: a picture of every handed-in
    # scene (anaglyph_preview.js); a click shows the scenes full size.
    def class_scene_galleries_html(activity_key, tasks, members)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        io = +''
        tasks.each do |t|
            found = find_task("#{activity_key}/#{t[:id]}")
            check = found && found[:task]['check']
            next unless check && check['type'] == 'submit' && check['preview'] == 'anaglyph'
            handed_in = members.select { |m| File.file?(submission_path(found[:key], m[:email])) }
            io << "<h3 class='scene-gallery-title'>Galerie: #{h.(t[:title])}</h3>"
            if handed_in.empty?
                io << "<p>Noch hat niemand eine Szene abgegeben.</p>"
                next
            end
            io << "<p class='scene-gallery-note'>Jede Szene aus der Ansicht, die in ihrer Datei unter <code>camera = …</code> steht. Ein Klick zeigt sie groß; dort lässt sie sich drehen, und die Pfeiltasten blättern weiter.</p>"
            io << "<div class='scene-gallery'>"
            handed_in.each do |m|
                scene = "abgaben/#{found[:key]}/#{TaskStore.state_key(m[:email], found[:key])}.ini"
                io << "<figure class='scene-gallery-item' data-scene='#{h.(scene)}' data-name='#{h.(m[:name])}'>"
                io << "<button type='button' title='Groß ansehen'><img alt='Die Szene von #{h.(m[:name])}'></button>"
                io << "<figcaption>#{h.(m[:name])}</figcaption></figure>"
            end
            io << "</div>"
        end
        io << "<script src='/include/anaglyph_preview.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>" unless io.empty?
        io
    end
    def submission_path(task_key, email = @session_user[:email])
        File.join(SUBMISSIONS_ROOT, "#{TaskStore.state_key(email, task_key)}.tar.gz")
    end
    def store_task_result(found, status, answer, email = @session_user[:email], max: TaskStore::MAX_ANSWER)
        key = TaskStore.state_key(email, found[:key])
        now = Time.now.to_i
        previous = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s;
        END_OF_QUERY
        props = TaskStore.next_state(previous && previous['s'],
            :task => found[:key], :version => found[:version], :status => status,
            :answer => answer, :now => now, :max => max)
        neo4j_query(<<~END_OF_QUERY, props.merge(:email => email, :key => key))
            MATCH (u:User {email: $email})
            MERGE (s:TaskState {key: $key})
            MERGE (u)-[:HAS_TASK_STATE]->(s)
            SET s.task = $task, s.version = $version, s.status = $status,
                s.last_answer = $last_answer, s.attempts = $attempts,
                s.first_status = $first_status, s.first_correct = $first_correct,
                s.updated = $updated
            CREATE (a:TaskAttempt {task: $task, version: $version, status: $status, ts: $updated})-[:BY]->(u);
        END_OF_QUERY
        notify_meeting_progress(email, found[:key].split('/').first)
        @@task_results_changed_at = Time.now
        TaskStore.public_state(props, found[:version])
    end
    # The code of a stored task state that passed, per language, for the
    # own functions of later cards; solved before it was kept: the last
    # answer.
    def correct_codes(stored)
        OwnFunctions.parse_codes(stored[:correct_answer] || (stored[:status] == 'correct' ? stored[:last_answer] : nil))
    end
    # For a card whose starter names own functions (OwnFunctions), for one
    # person: :fill, a lambda that fills their own into code (nil: into the
    # starter), and :filled, what the card gives for each function now; nil
    # for any other card.
    def own_functions_for(found, email)
        activity, id = found[:key].split('/', 2)
        entry = @@content.values.find { |c| (c[:tasks] || {})[:activity] == activity }
        tasks = entry[:tasks][:by_id]
        language = found[:task]['check']['language'] || 'ruby'
        _, part = OwnFunctions.parts(tasks[id]).find { |l, _| l == language }
        return nil unless part

        correct = {}
        given = {}
        neo4j_query(<<~END_OF_QUERY, :email => email, :prefix => "#{activity}/").each do |row|
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState)
            WHERE s.task STARTS WITH $prefix
            RETURN s;
        END_OF_QUERY
            task_id = row['s'][:task].to_s.split('/', 2)[1]
            correct[task_id] = correct_codes(row['s'])
            given = OwnFunctions.parse_codes(row['s'][:own_filled])[language] || {} if task_id == id
        end
        mine = OwnFunctions.mine(correct, language)
        {
            :language => language,
            :fill => ->(code) { OwnFunctions.personal(code, part['starter_own'], language, tasks, :mine => mine, :filled => given) },
            :filled => OwnFunctions.filled(part['starter_own'], language, tasks, :mine => mine),
        }
    end
    # With a card's code saved: what it gave for each own function then
    # (TaskState.own_filled, per language), so that a function the person
    # changed in the card stays theirs (OwnFunctions.personal).
    def remember_own_filled(found, email)
        own = own_functions_for(found, email) or return
        key = TaskStore.state_key(email, found[:key])
        previous = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.own_filled AS filled;
        END_OF_QUERY
        filled = OwnFunctions.parse_codes(previous && previous['filled']).merge(own[:language] => own[:filled])
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => key, :filled => JSON.generate(filled))
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            SET s.own_filled = $filled;
        END_OF_QUERY
    end
    # A Kata's code that passed its check, per language
    # (TaskState.correct_answer): later cards fill in their own functions
    # from it (OwnFunctions), also after a run that did not pass.
    def remember_correct_code(found, email, code)
        key = TaskStore.state_key(email, found[:key])
        previous = neo4j_query(<<~END_OF_QUERY, :email => email, :key => key).first
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            RETURN s.correct_answer AS correct;
        END_OF_QUERY
        answer = KataCheck.stored_answer(previous && previous['correct'], found[:task]['check']['language'] || 'ruby', code)
        neo4j_query(<<~END_OF_QUERY, :email => email, :key => key, :answer => answer)
            MATCH (:User {email: $email})-[:HAS_TASK_STATE]->(s:TaskState {key: $key})
            SET s.correct_answer = $answer;
        END_OF_QUERY
    end

    # --- Answers under open tasks (docs/aufgaben.md) ---------------------------
    # Example answers are teaching material, for every teacher; the answers
    # of a group only for those who may see its class board.
    def answer_examples_visible?
        teacher_account_logged_in? || teacher_logged_in?
    end
    # For teachers on an activity page: under each open task the example
    # answers or those of the current group, as chosen last (tasks.js).
    # The example answers go only into this config, never into the cards.
    # pinned: the groups that have this activity pinned (on /klasse); tasks.js
    # gives each a button with how many have answered.
    def open_answers_config(tasks, slug)
        ids = OpenAnswers.task_ids(tasks)
        return nil if ids.empty?

        examples = answer_examples_visible?
        groups = class_groups
        return nil unless examples || !groups.empty?

        prefs = class_prefs
        pinned = pins_for_groups(groups).select { |p| p[:slug] == slug }.map { |p| p[:group] }
        {
            :view => OpenAnswers.view(prefs['answer_view'], :examples => examples, :groups => !groups.empty?),
            :groups => groups.map { |g| { :name => g, :size => (@@group_members[g] || []).size } },
            :group => groups.include?(prefs['group']) ? prefs['group'] : (groups.find { |g| pinned.include?(g) } || groups.first),
            :pinned => groups.select { |g| pinned.include?(g) },
            # an example that is code in one language shows only while the
            # page is in that language (tasks.js, the CSS of .code-language)
            :examples => examples ? ids.to_h { |id| [id, (tasks[:by_id][id]['examples'] || []).map { |e| { :html => render_task_markdown(e), :language => OpenAnswers.example_language(e) } }] } : nil,
        }
    end
    # For teachers, beside each Kata's buttons: its solution's code in every
    # language it has, { id => { language => code } }, for a button that
    # puts it into the editor (tasks.js). Like the example answers only in
    # this config, never in the cards.
    def kata_solutions_config(tasks)
        return nil unless answer_examples_visible? || admin_logged_in?

        solutions = {}
        tasks[:on_page].each do |id|
            task = tasks[:by_id][id]
            next unless task && %w[function pip tests turtle].include?(task.dig('check', 'type'))

            [task, *CodeLanguages.variant_languages(task).map { |l| CodeLanguages.variant(task, l) }].each do |t|
                next unless t['solution']

                code = t['solution'].scan(/^```[^\n]*\n(.*?)^```/m).map(&:first).join("\n")
                (solutions[id] ||= {})[t.dig('check', 'language')] = code unless code.empty?
            end
        end
        solutions.empty? ? nil : solutions
    end
end
