require_relative 'zip_stream'
# Leistungsüberprüfungen (exams), on the class page (tab »Leistungs-
# überprüfungen«, class_exams_html, exams.js): a teacher uploads a Workspace
# package for a class, and everyone in it takes part (one by one changed),
# starts and stops the exam and its demo, and gets the results as a PDF;
# docs/test-workspace-packages.md. Moved out of main.rb, which loads it.
class Main < Sinatra::Base
    # How to make a package, under the upload (folded)
    EXAM_PACKAGE_HELP = <<~'END_OF_HTML'.freeze
        <details class="mb-3">
            <summary>Ein eigenes Paket hochladen</summary>
            <p class="mt-2">
                Ein Paket ist ein Archiv <code>.tar.gz</code> mit dem Inhalt, den die Klasse bekommt, ohne einen Ordner darum herum.
                Im Terminal, im Ordner der Aufgabe:
            </p>
            <pre>tar -czf "../$(basename "$PWD").tar.gz" .</pre>
            <p class="mb-0">
                Farbe, Git, Drucken, Erweiterungen und Befehle beim Start stehen in <code>.workspace/config.yaml</code>;
                was dort stehen kann: <a href="/lehrmaterial-leistungsueberpruefung#9-ein-eigenes-paket-bauen">Ein eigenes Paket bauen</a>.
            </p>
        </details>
    END_OF_HTML

    # The students of a class who take its exams by themselves: its members
    # without teachers and oneself; test accounts only in a test group (a
    # teacher tries an exam with one there)
    def exam_students(group)
        test_group = @@test_groups.include?(group)
        (@@group_members[group] || []).reject do |email|
            email == @session_user[:email] || @@teachers.key?(email) || (!test_group && @@test_accounts.key?(email))
        end
    end

    # An exam from a package's bytes, uploaded or built from a folder
    # (exam_builder_page.rb): checked and its extensions fetched before it
    # shows; for a class (group), everyone in it takes part. The number of
    # them; raises the package's errors (ConfigError, Error).
    def create_exam(blob, filename, group)
        # the file name of a multipart upload arrives as binary: as text, and
        # never invalid UTF-8 (neo4j_bolt refuses that; 0.5.1 stored it as a
        # list of numbers)
        filename = filename.to_s.dup.force_encoding('UTF-8').scrub
        sha1 = Digest::SHA1.hexdigest(blob)
        tag = RandomTag.generate(24)
        debug "Got a file called #{filename} with #{blob.size} bytes."

        # Validate the actual uploaded bytes and resolve/download every VSIX
        # before creating a visible Test entry. The original archive is kept
        # untouched; extension files and the locked manifest live beside it.
        Dir.mktmpdir('exam-upload-') do |dir|
            incoming = File.join(dir, 'package.tar.gz')
            File.binwrite(incoming, blob)
            manifest = TestWorkspaceExtensions.prepare!(incoming, tag: tag)
            FileUtils.mkdir_p(TestWorkspaceExtensions::ARCHIVES)
            archive_path = File.join(TestWorkspaceExtensions::ARCHIVES, sha1)
            File.binwrite(archive_path, blob) unless File.file?(archive_path)
            TestWorkspaceExtensions.publish!(manifest, tag: tag)
        end

        ts = Time.now.to_i
        neo4j_query_expect_one(<<~END_OF_STRING, {:tag => tag, :email => @session_user[:email], :sha1 => sha1, :size => blob.size, :filename => filename, :ts => ts})
            MATCH (u:User {email: $email})
            MERGE (f:File {sha1: $sha1})
            WITH u, f
            CREATE (u)<-[:BELONGS_TO]-(t:Test {tag: $tag})-[:USES]->(f)
            SET f.size = $size
            SET t.filename = $filename
            SET t.ts = $ts
            RETURN f.sha1;
        END_OF_STRING
        students = group ? exam_students(group) : []
        if group
            neo4j_query(<<~END_OF_STRING, {:tag => tag, :group => group, :emails => students})
                MATCH (t:Test {tag: $tag})
                SET t.group = $group
                WITH t
                UNWIND $emails AS email
                MERGE (s:User {email: email})
                MERGE (s)-[:TAKES]->(t);
            END_OF_STRING
            Main.refresh_nginx_config() unless students.empty?
        end
        students.size
    end

    # the package's errors as the dialog shows them
    def exam_error_response(e)
        if e.is_a?(TestWorkspaceExtensions::Error) || e.is_a?(TestWorkspacePackage::ConfigError) || (defined?(ExamBuilder) && e.is_a?(ExamBuilder::Refused))
            STDERR.puts "Exam package rejected: #{e.class}: #{e.message}"
            status 422
            content_type :json
            return JSON.generate(:success => false, :error => e.message)
        end
        STDERR.puts "Exam package failed: #{e.class}: #{e.message}"
        STDERR.puts Array(e.backtrace).first(25).join("\n")
        status 500
        content_type :json
        JSON.generate(:success => false, :error => 'Interner Fehler beim Vorbereiten des Prüfungspakets. Bitte Server-Log prüfen.')
    end

    post '/api/upload_test_archive' do
        assert(teacher_logged_in?)
        # uploaded on the class page: the exam belongs to that class, and
        # everyone in it takes part (one by one changed in the dialog)
        group = params['group'].to_s
        group = nil if group.empty?
        halt 403 if group && !class_groups.include?(group)
        entry = params['file']
        count = create_exam(entry['tempfile'].read, entry['filename'], group)
        respond(:success => true, :yay => 'sure', :count => count)
    rescue StandardError => e
        exam_error_response(e)
    end
    post '/api/get_my_test_archives' do
        assert(teacher_logged_in?)
        entries = []
        neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})<-[:BELONGS_TO]->(t:Test)-[:USES]->(f:File)
            OPTIONAL MATCH (t)<-[:TAKES]-(u2:User)
            RETURN t, f, COUNT(u2), COLLECT(u2.email) AS emails;
        END_OF_STRING
            # who takes part in one uploaded before exams belonged to a
            # class: shown in the classes they are in (exams.js)
            # an exam of the test group (no class one chooses any more) is shown
            # like one without a class, to be given to one
            group = @@test_groups.include?(row['t'][:group]) ? nil : row['t'][:group]
            emails = group ? nil : row['emails']
            entries << {
                tag: row['t'][:tag],
                running: row['t'][:running],
                practice: row['t'][:practice],
                size: bytes_to_str(row['f'][:size]),
                filename: row['t'][:filename],
                title: exam_title(row['t']),
                ts: row['t'][:ts],
                group: group,
                count: row['COUNT(u2)'],
                emails: emails,
            }.compact
        end
        respond(:entries => entries)
    end
    # What a deleted exam leaves to be removed: its Workspaces, moved here at
    # once (a rename, the folder stays on the same disk), and a job file per
    # exam with the containers to stop and the database logins to drop.
    # A thread works off the job (remove_exam_instances); a job a restart
    # interrupted is done again at the next start (configure, main.rb), and
    # the drops are IF EXISTS, so doing one twice does no harm.
    DELETED_EXAM_INSTANCES = '/user/.deleted-exams'

    post '/api/delete_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        # The exam's Workspaces and databases go with it: of every student who
        # took it, and the teacher's own demo.
        takers = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]}).map { |row| row['email'].to_s }
            MATCH (s:User)-[:TAKES]->(t:Test {tag: $tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN s.email AS email;
        END_OF_STRING
        instances = takers.map { |email| [email, data[:tag]] }
        unless neo4j_query('MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag}) RETURN t.tag;', :email => @session_user[:email], :tag => data[:tag]).empty?
            instances << [@session_user[:email], Main.test_demo_tag(@session_user[:email], data[:tag])]
        end
        deleted = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => @session_user[:email]}).size
            MATCH (u:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag})
            WITH t, t.tag AS tag
            DETACH DELETE t
            RETURN tag;
        END_OF_STRING
        # The shared content-addressed VSIX cache is retained: other exams
        # may still reference these files. The manifest only for an exam that
        # was this teacher's, and only under a plain tag.
        if deleted > 0 && data[:tag].match?(/\A[a-z0-9]+\z/)
            FileUtils.rm_f(File.join(TestWorkspaceExtensions::MANIFESTS, "#{data[:tag]}.json"))
        end
        unless instances.empty?
            # Stopping the containers, dropping the databases and removing
            # some 10,000 files per Workspace (a Dart exam) took over a
            # minute for a class, longer than nginx waits for an answer: the
            # teacher saw an error although everything went. Now that is a
            # thread's work, and the answer comes at once.
            job = {
                'containers' => instances.map { |email, tag| fs_tag_for_email("#{email}#{tag}") },
                'logins' => instances.map { |email, tag| DatabaseIdentity.ephemeral_login(db_login_for_email(email), tag) },
            }
            job_path = queue_exam_instances_removal(job)
            Thread.new { remove_exam_instances(job_path) }
        end
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end

    # Writes the job file, then moves the Workspaces aside (a folder that is
    # not there, of a student who never opened the exam, is left out);
    # returns the job file's path. The job comes first: a folder there never
    # lacks its job, and one a restart left in /user goes with the job too.
    def queue_exam_instances_removal(job)
        FileUtils.mkdir_p(DELETED_EXAM_INSTANCES)
        job_path = File.join(DELETED_EXAM_INSTANCES, "#{Time.now.strftime('%Y%m%d%H%M%S')}-#{SecureRandom.hex(4)}.json")
        File.write("#{job_path}.tmp", job.to_json)
        File.rename("#{job_path}.tmp", job_path)
        folder = job_path.delete_suffix('.json')
        FileUtils.mkdir_p(folder)
        job['containers'].each do |container_name|
            path = "/user/#{container_name}"
            File.rename(path, File.join(folder, container_name)) if File.directory?(path)
        end
        job_path
    end

    # Does a job: the containers stopped (side by side, each may wait for
    # its timeout), the databases dropped, the folders removed; the job file
    # last, so that an interrupted job is done again.
    def remove_exam_instances(job_path)
        job = JSON.parse(File.read(job_path))
        job['containers'].map do |container_name|
            Thread.new { workspace_runtime.stop_workspace(container_name, :timeout => shell_timeout(:docker_kill)) }
        end.each(&:join)
        unless job['logins'].empty?
            run_mysql_statements(job['logins'].flat_map { |login| DatabaseProvisioning.mysql_drop_statements(login) })
            run_neo4j_statements(job['logins'].flat_map { |login| DatabaseProvisioning.neo4j_drop_statements(login) })
        end
        FileUtils.rm_rf(job_path.delete_suffix('.json'))
        # a Workspace the move did not reach (a restart in between); its name
        # belongs to this exam alone
        job['containers'].each { |container_name| FileUtils.rm_rf("/user/#{container_name}") }
        FileUtils.rm_f(job_path)
    rescue => e
        STDERR.puts "Removing the Workspaces of a deleted exam failed (#{job_path}, done again at the next start): #{e.class}: #{e.message}"
    end

    # At the start: the jobs a restart interrupted, one after the other.
    def self.resume_exam_instance_removals
        Dir[File.join(DELETED_EXAM_INSTANCES, '*.json')].sort.each do |job_path|
            STDERR.puts "Resuming the removal of a deleted exam's Workspaces: #{File.basename(job_path)}"
            Main.new!.remove_exam_instances(job_path)
        end
    end

    # A mock exam (»Probelauf«, the switch under its name, exams.js): while
    # it runs, who takes part may go to the normal site and the own
    # Workspace and come back (practice_exam_away); a yellow bar under the
    # menu shows the way back. For a mock exam over several days, in which
    # the class also does other work.
    post '/api/test_set_practice' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :practice], :types => { :practice => Object })
        require_test_owner!(data[:tag])
        if data[:practice] == true
            neo4j_query('MATCH (t:Test {tag: $tag}) SET t.practice = TRUE;', :tag => data[:tag])
        else
            # back to a real exam: everyone who is away comes back into it
            neo4j_query(<<~END_OF_STRING, :tag => data[:tag])
                MATCH (t:Test {tag: $tag})
                REMOVE t.practice
                WITH t
                OPTIONAL MATCH (:User)-[r:TAKES]->(t)
                REMOVE r.away;
            END_OF_STRING
        end
        Main.refresh_nginx_config()
        respond(:ok => true)
    end

    # A student during a mock exam: away to the normal site (away true) or
    # back into the exam (false); only for a mock exam that runs.
    post '/api/practice_exam_away' do
        assert(user_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :away], :types => { :away => Object })
        found = neo4j_query(<<~END_OF_STRING, :email => @session_user[:email], :tag => data[:tag].to_s, :away => data[:away] == true).to_a
            MATCH (:User {email: $email})-[r:TAKES]->(t:Test {tag: $tag, running: TRUE, practice: TRUE})
            SET r.away = CASE WHEN $away THEN TRUE ELSE NULL END
            RETURN t.tag;
        END_OF_STRING
        halt 404 if found.empty?
        respond(:ok => true)
    end

    # The mock exams a student went away from (the bar under the menu,
    # look_in_bar_html)
    def my_practice_exams_away
        return [] unless user_logged_in?

        neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email]).map do |row|
            MATCH (:User {email: $email})-[:TAKES {away: TRUE}]->(t:Test {running: TRUE, practice: TRUE})-[:BELONGS_TO]->(tu:User)
            RETURN t.tag AS tag, tu.email AS teacher;
        END_OF_QUERY
            { :tag => row['tag'], :teacher => (@@invitations[row['teacher']] || {})[:name] }
        end
    end

    # The name of an exam the teacher gave it (»Umbenennen« beside the
    # name, exams.js); the file name of the package stays as it was.
    post '/api/test_rename' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :title], :max_value_lengths => { :title => 200 })
        require_test_owner!(data[:tag])
        title = data[:title].to_s.gsub(/[[:space:]]+/, ' ').strip
        halt 400 if title.empty? || title.length > 100
        neo4j_query('MATCH (t:Test {tag: $tag}) SET t.title = $title;', :tag => data[:tag], :title => title)
        respond(:ok => true, :title => title)
    end

    # What an exam is called: the name the teacher gave it, else the file
    # name of the package without .tar.gz and the like.
    def exam_title(test)
        title = test[:title].to_s.strip
        return title unless title.empty?
        test[:filename].to_s.sub(/\.(tar\.gz|tar\.bz2|tgz|tbz2?|tar)\z/i, '')
    end

    # An exam uploaded before exams belonged to a class (in the profile)
    # given to one; who takes part stays as it is
    post '/api/test_assign_group' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :group])
        require_test_owner!(data[:tag])
        halt 403 unless class_groups.include?(data[:group])
        neo4j_query('MATCH (t:Test {tag: $tag}) SET t.group = $group;', :tag => data[:tag], :group => data[:group])
        respond(:ok => true)
    end

    # The tab »Leistungsüberprüfungen« of the class page (exams.js): the
    # upload, how to make a package, the table, the dialog of who takes part
    def class_exams_html(group)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        me = @session_user[:email]
        school = (school_of_teacher(me) || {})[:name] ||
                 (admin_logged_in? ? (neo4j_query('MATCH (s:School) WHERE s.home = true RETURN s.name AS name;').first || {})['name'] : nil)
        data = {
            :group => group, :order => my_user_group_order, :groups => my_user_groups,
            :names => my_invitations, :schools => school_names_of_groups.slice(*my_user_group_order), :mySchool => school,
            # the test group is shown as the teacher's test account
            :testGroup => @@test_group_of[me],
        }
        # trying it first: »Ausprobieren« shows the start page as the class
        # sees it and opens the teacher's preview behind it
        tip = '<strong>Erst ausprobieren?</strong> Mit <em>Ausprobieren</em> unter <em>Vorschau</em> siehst du die Startseite, wie die Klasse sie ' \
              'während der Leistungsüberprüfung sieht, und öffnest von dort deine Vorschau des Workspace; die Klasse merkt davon nichts. ' \
              'Kennt deine Klasse den Ablauf noch nicht, hilft vielleicht ein kurzer Probelauf mit einer kleinen Leistungsüberprüfung von dir, bevor es zählt.'
        "<div class='class-exams'>" \
            "<div class='class-exams-head'><p class='mb-0'>Für eine Leistungsüberprüfung ziehst du ihren Ordner von deinem Rechner hierher, mit der Aufgabe und allem, " \
            "was die Klasse zu Beginn bekommt; ein Ordner aus deinem Workspace oder ein fertiges Paket geht auch. " \
            "Alle aus der Klasse #{h.(group)} nehmen teil, sobald du sie startest; Einzelne nimmst du unter <em>Teilnehmende</em> heraus oder fügst sie aus anderen Gruppen hinzu. " \
            "Wie das geht: <a href='/lehrmaterial-leistungsueberpruefung'>Leistungsüberprüfungen vorbereiten</a>.</p>" \
            "<div class='class-exams-actions'><button id='bu_exam_builder' class='btn btn-primary'><i class='bi bi-plus-lg'></i>Neue Leistungsüberprüfung</button>" \
            "<button id='bu_upload_test_archive' class='btn btn-outline-secondary'><i class='bi bi-upload'></i>Paket hochladen</button></div></div>" \
            "#{tip ? "<div class='hint class-exams-tip'>#{tip}</div>" : ''}" \
            "<div id='test_archive_upload_status' class='small mb-2' role='status' aria-live='polite'></div>" \
            "#{EXAM_PACKAGE_HELP}" \
            "<div class='table-responsive mt-3'><table class='table table-sm align-middle exam-table' id='table_my_test_archives'><thead><tr>" \
            '<th>Leistungsüberprüfung</th><th>Teilnehmende</th><th>Status</th><th>Vorschau</th><th>Ergebnisse</th>' \
            "<th><span class='visually-hidden'>Löschen</span></th></tr></thead><tbody></tbody></table></div>" \
            "<div class='modal' id='test_modal' tabindex='-1' role='dialog' aria-labelledby='test_modal_title' aria-hidden='true' style='z-index: 200000;'>" \
            "<div class='modal-dialog modal-lg modal-dialog-scrollable modal-dialog-centered' role='document'><div class='modal-content'>" \
            "<div class='modal-header'><h5 class='modal-title' id='test_modal_title'>Teilnehmende</h5></div>" \
            "<div class='modal-body'><div id='test_modal_users_here'></div></div>" \
            "<div class='modal-footer'><span class='small text-body-secondary me-auto'>Jede Änderung wird sofort gespeichert.</span>" \
            "<button type='button' class='btn btn-primary' data-bs-dismiss='modal'><i class='bi bi-check-lg'></i>Fertig</button></div>" \
            '</div></div></div>' \
            "#{exam_builder_modal_html(group)}" \
            "<script>window.EXAMS = #{JSON.generate(data).gsub('</', '<\/')};</script>" \
            "<script src='/include/exams.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script></div>"
    end

    post '/api/get_assigned_users_for_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        emails = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :teacher_email => @session_user[:email]}).map { |x| x['u.email'] }
            MATCH (:User {email: $teacher_email})<-[:BELONGS_TO]-(t:Test {tag: $tag})<-[:TAKES]-(u:User)
            RETURN u.email;
        END_OF_STRING
        respond(:emails => emails)
    end
    post '/api/test_toggle_user' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :email])
        # An exam restricts a person's interface: only one's own students
        # (and test accounts), the address alone used to be enough.
        Main.load_invitations()
        halt 403 unless admin_logged_in? || workspace_user_visible_to?(data[:email], @session_user[:email])
        require_test_owner!(data[:tag])
        count = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]}).size
            MATCH (u:User {email: $email})-[:TAKES]->(t:Test {tag: $tag})
            RETURN u.email;
        END_OF_STRING
        if count == 0
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MERGE (u:User {email: $email})
                WITH u
                MATCH (t:Test {tag: $tag})
                CREATE (u)-[:TAKES]->(t)
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => true)
        else
            neo4j_query_expect_one(<<~END_OF_STRING, {:tag => data[:tag], :email => data[:email]})
                MATCH (u:User {email: $email})-[r:TAKES]->(t:Test {tag: $tag})
                DELETE r
                RETURN u.email;
            END_OF_STRING
            Main.refresh_nginx_config()
            respond(:flag => false)
        end
    end
    # Who takes an exam, set for several people at once (a whole group from
    # the selection dialog): flag true adds, false removes. Only one's own
    # students (and test accounts), as for test_toggle_user; others are left
    # out. Answers with everyone who takes it now.
    post '/api/test_set_users' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag, :emails, :flag], :types => { :emails => Array, :flag => Object },
            :max_body_length => 64_000)
        require_test_owner!(data[:tag])
        Main.load_invitations()
        emails = data[:emails].map(&:to_s).uniq.select do |email|
            admin_logged_in? || workspace_user_visible_to?(email, @session_user[:email])
        end
        unless emails.empty?
            if data[:flag] == true
                neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :emails => emails})
                    MATCH (t:Test {tag: $tag})
                    UNWIND $emails AS email
                    MERGE (u:User {email: email})
                    MERGE (u)-[:TAKES]->(t);
                END_OF_STRING
            else
                neo4j_query(<<~END_OF_STRING, {:tag => data[:tag], :emails => emails})
                    MATCH (u:User)-[r:TAKES]->(:Test {tag: $tag})
                    WHERE u.email IN $emails
                    DELETE r;
                END_OF_STRING
            end
            Main.refresh_nginx_config()
        end
        taking = neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]}).map { |row| row['email'] }
            MATCH (u:User)-[:TAKES]->(:Test {tag: $tag})
            RETURN u.email AS email;
        END_OF_STRING
        respond(:emails => taking)
    end
    # Starting or stopping a real test only changes whether assigned students
    # are routed into their existing exam workspace. Their files and databases
    # must survive any number of stop/start cycles.
    # An exam may be started, stopped and given students only by the teacher
    # it belongs to (or an admin).
    def require_test_owner!(tag)
        return if admin_logged_in?

        owned = neo4j_query(<<~END_OF_STRING, {:tag => tag, :email => @session_user[:email]}).size
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag})
            RETURN t.tag;
        END_OF_STRING
        halt 404 if owned == 0
    end
    post '/api/start_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        require_test_owner!(data[:tag])
        # everyone starts in the exam, also who went to the normal site the
        # last time it ran (a mock exam, practice_exam_away)
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            SET t.running = TRUE
            WITH t
            OPTIONAL MATCH (:User)-[r:TAKES]->(t)
            REMOVE r.away;
        END_OF_STRING
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end
    post '/api/stop_test' do
        assert(teacher_logged_in?)
        data = parse_request_data(:required_keys => [:tag])
        require_test_owner!(data[:tag])
        neo4j_query(<<~END_OF_STRING, {:tag => data[:tag]})
            MATCH (t:Test {tag: $tag})
            REMOVE t.running;
        END_OF_STRING
        Main.refresh_nginx_config()
        respond(:ok => 'sure')
    end
    # The teacher's look at an exam as the class sees it (»Ausprobieren« on
    # the class page): the start page during the exam, the menu hidden as
    # for a student, its button opening the teacher's preview Workspace
    # (carrying on where it was; »Von vorn beginnen« starts it again), and
    # a bar back to the class page
    get '/pruefung-ansehen' do
        redirect "#{WEB_ROOT}/" unless teacher_logged_in?
        tag = params['tag'].to_s
        test = neo4j_query('MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $tag}) RETURN t;',
                           :email => @session_user[:email], :tag => tag).first
        halt 404 unless test
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        title = exam_title(test['t'])
        group = params['group'].to_s
        back = "/klasse?#{group.empty? ? '' : "group=#{CGI.escape(group)}&"}tab=pruefungen"
        me = (@@invitations[@session_user[:email]] || {})[:name] || @session_user[:email]
        running_tests = [{ :tag => tag, :teacher => me }]
        @exam_try_bar = "<div class='look-in-bar look-in-bar-impersonating'><div class='container look-in-bar-inner'><i class='bi bi-eye' aria-hidden='true'></i>" \
            "<span>Du siehst die Startseite wie die Klasse während <strong>»#{h.(title)}«</strong>. Der Knopf öffnet deine Vorschau; öffnest du sie wieder, geht es dort weiter.</span>" \
            "<button type='button' class='btn btn-sm btn-light bu-exam-demo' id='bu_exam_try_fresh' data-tag='#{h.(tag)}'>" \
            "<i class='bi bi-arrow-counterclockwise' aria-hidden='true'></i>Von vorn beginnen</button>" \
            "<a class='btn btn-sm btn-light' href='#{h.(back)}'><i class='bi bi-arrow-return-left' aria-hidden='true'></i>Zurück zur Klasse</a></div></div>"
        @exam_try_page = "<div class='container'>" +
            running_tests_html(running_tests, :demo => true) + '</div>' +
            "<script>(function () {\n" \
            "  function open(button, fresh) {\n" \
            "    button.prop('disabled', true).find('.bi').addClass('bi-spin');\n" \
            "    api_call('/api/start_test_demo', { tag: button.data('tag'), keep: fresh ? 'no' : 'yes' }, function (data) {\n" \
            "      if (data.success && data.status !== 'failed') { check_server_tag_update(data.server_tag); check_server_online(data.server_tag); }\n" \
            "      else button.prop('disabled', false).find('.bi').removeClass('bi-spin');\n" \
            "    });\n" \
            "  }\n" \
            "  $('.bu-launch-test-demo').addClass('bu-exam-demo').on('click', function () { open($(this), false); });\n" \
            "  $('#bu_exam_try_fresh').on('click', function () { open($(this), true); });\n" \
            "})();</script>"
        path = request.path
        slug = nil
        template = File.read(File.join(@@static_dir, '_template.html'))
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, '#{@exam_try_page}', binding), request.path)
        respond_raw_with_mimetype(html, 'text/html')
    end

    post '/api/start_test_demo' do
        assert(teacher_logged_in?)
        # keep: 'yes' carries on in the preview as it is (Ausprobieren);
        # otherwise it starts again from the package
        data = parse_request_data(:required_keys => [:tag], :optional_keys => [:keep])
        keep = data[:keep] == 'yes'
        email = @session_user[:email]
        test_tag = data[:tag]

        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :test_tag => test_tag})
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $test_tag})
            RETURN t;
        END_OF_QUERY

        demo_tag = Main.test_demo_tag(email, test_tag)
        base_server_tag = @session_user[:server_tag]
        server_tag = "#{base_server_tag}#{demo_tag}"

        # The destructive reset below is deliberately confined to the teacher's
        # private demo namespace. Real student exam workspaces are never reset here.
        job = Main.enqueue_server_start(
            :email => email,
            :test_tag => demo_tag,
            :server_tag => server_tag
        ) do
            container_name = fs_tag_for_email("#{email}#{demo_tag}")
            unless keep
                workspace_runtime.stop_workspace(
                    container_name,
                    :timeout => shell_timeout(:docker_kill),
                )
                FileUtils.rm_rf("/user/#{container_name}")
                reset_test_demo_databases(email, demo_tag)
            end
            start_server(
                email,
                test_tag,
                :server_tag => base_server_tag,
                :instance_tag => demo_tag,
            )
        end

        status = Main.wait_for_server_start_job(job)

        respond(
            :yay => 'sure',
            :server_tag => server_tag,
            :queued => ['queued', 'running'].include?(status[:status]),
            :status => status[:status],
            :error => status[:error]
        )
    end
    def my_running_tests
        unless user_logged_in?
            return []
        end
        entries = []
        neo4j_query(<<~END_OF_QUERY, {:email => @session_user[:email]}).each do |row|
            MATCH (u:User {email: $email})-[r:TAKES]->(t:Test {running: TRUE})-[:BELONGS_TO]->(tu:User)
            WHERE NOT (coalesce(t.practice, FALSE) AND coalesce(r.away, FALSE))
            RETURN t, tu.email;
        END_OF_QUERY
            entries << {
                :tag => row['t'][:tag],
                :teacher => @@invitations[row['tu.email']][:name],
                :practice => row['t'][:practice] == true,
            }
        end
        entries
    end
    def test_results_html(emails, instance_tag)
        printed_at = Time.now.strftime("%d.%m.%Y %H:%M Uhr")
        document_title = if emails.size == 1
            display_name = (@@invitations[emails.first] || {})[:name] || emails.first
            "Leistungsüberprüfung – #{display_name}"
        else
            'Leistungsüberprüfung – Ergebnisse'
        end

        StringIO.open do |io|
            io.puts <<~END_OF_STRING
                <!DOCTYPE html>
                <html>
                <head>
                    <meta charset="utf-8">
                    <title>#{CGI.escapeHTML(document_title)}</title>
                    <link rel="stylesheet" href="/include/fonts.css?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}">
                    <style>
                        @page {
                            margin: 12mm;
                        }
                        body {
                            font-family: '0xProto Nerd Font Mono', monospace;
                            font-size: 10pt;
                        }
                        .student + .student {
                            break-before: page;
                            page-break-before: always;
                        }
                        .student-meta {
                            border-bottom: 1px solid #999;
                            color: #555;
                            font-size: 9pt;
                            margin: 0 0 0.8em;
                            text-align: right;
                            padding-bottom: 0.35em;
                        }
                        .file {
                            margin: 0 0 1.25em;
                        }
                        .file-header {
                            align-items: baseline;
                            background: #ddd;
                            break-after: avoid;
                            page-break-after: avoid;
                            display: flex;
                            font-weight: bold;
                            gap: 1em;
                            padding: 0.2em 0.35em;
                        }
                        .file-meta {
                            flex-grow: 1;
                            text-align: right;
                        }
                        pre {
                            font-family: '0xProto Nerd Font Mono', monospace;
                            font-size: 10pt;
                            margin: 0.35em 0 0;
                            overflow-wrap: anywhere;
                            white-space: pre-wrap;
                        }
                        .line-number {
                            border-right: 1px solid #aaa;
                            color: #777;
                            display: inline-block;
                            margin-right: 0.7em;
                            padding-right: 0.7em;
                            text-align: right;
                            width: 3.5em;
                        }
                        .empty {
                            color: #666;
                            font-family: sans-serif;
                        }
                    </style>
                </head>

                <body>
            END_OF_STRING

            emails.each do |email|
                container_name = fs_tag_for_email("#{email}#{instance_tag}")
                workspace_path = "/user/#{container_name}/workspace"
                display_name = (@@invitations[email] || {})[:name] || email
                escaped_name = CGI.escapeHTML(display_name)

                io.puts "<section class='student'>"
                io.puts "<div class='student-meta'>#{escaped_name} | #{printed_at}</div>"

                unless File.directory?(workspace_path)
                    io.puts "<p class='empty'>Dieser Prüfungs-Workspace wurde noch nicht geöffnet.</p>"
                    io.puts "</section>"
                    next
                end

                files = TestWorkspacePackage.printable_files(workspace_path)
                if files.empty?
                    io.puts "<p class='empty'>Keine druckbaren Dateien gefunden.</p>"
                end

                files.each do |relative_path, path|
                    io.puts "<div class='file'>"
                    io.puts "<div class='file-header'>"
                    io.puts "<div>#{CGI.escapeHTML(relative_path)}</div>"
                    io.puts "</div>"
                    io.puts "<pre>"
                    File.foreach(path).with_index(1) do |line, index|
                        io.puts sprintf(
                            "<span class='line-number'>%4d</span>%s",
                            index,
                            CGI.escapeHTML(line.chomp),
                        )
                    end
                    io.puts "</pre>"
                    io.puts "</div>"
                end

                io.puts "</section>"
            end

            io.puts "</body>"
            io.puts "</html>"

            io.string
        end
    end
    get '/api/pdf_for_test/:tag' do
        assert(teacher_logged_in?)
        test_tag = params['tag']
        emails = neo4j_query(<<~END_OF_STRING, {:email => @session_user[:email], :test_tag => test_tag}).map { |row| row['u.email'] }
            MATCH (u:User)-[:TAKES]->(t:Test {tag: $test_tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN u.email;
        END_OF_STRING

        requested_email = params['email'].to_s
        unless requested_email.empty?
            assert(emails.include?(requested_email), 'student is not assigned to this test')
            emails = [requested_email]
        end

        emails.sort_by! do |email|
            ((@@invitations[email] || {})[:name] || email).downcase
        end
        respond_raw_with_mimetype(test_results_html(emails, test_tag), 'text/html')
    end
    # All results of a Leistungsüberprüfung as one ZIP, to keep them and run
    # them later (the printout stays the record): a folder per person with
    # every file of the exam Workspace but the hidden folders of code-server
    # (zip_stream.py --skip-hidden; .git and .workspace stay), streamed as
    # the Workspace user, as »Meine Daten herunterladen« is. Who never
    # opened the Workspace is listed in nicht-geoeffnet.txt.
    get '/api/zip_for_test/:tag' do
        assert(teacher_logged_in?)
        test_tag = params['tag'].to_s
        test = neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :test_tag => test_tag).first
            MATCH (t:Test {tag: $test_tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN t;
        END_OF_QUERY
        halt 404 unless test
        emails = neo4j_query(<<~END_OF_QUERY, :email => @session_user[:email], :test_tag => test_tag).map { |row| row['email'] }
            MATCH (u:User)-[:TAKES]->(t:Test {tag: $test_tag})-[:BELONGS_TO]->(:User {email: $email})
            RETURN u.email AS email;
        END_OF_QUERY
        names = emails.to_h { |email| [email, (@@invitations[email] || {})[:name] || email] }
        emails.sort_by! { |email| names[email].downcase }
        folders = emails.zip(ZipStream.folder_names(emails.map { |email| names[email] })).to_h

        trees = []
        missing = []
        emails.each do |email|
            path = "/user/#{fs_tag_for_email("#{email}#{test_tag}")}/workspace"
            File.directory?(path) ? trees << "#{folders[email]}=#{path}" : missing << names[email]
        end
        dir = Dir.mktmpdir('exam-results')
        unless missing.empty?
            File.write(File.join(dir, 'nicht-geoeffnet.txt'),
                "Diese Personen haben ihren Prüfungs-Workspace nicht geöffnet; von ihnen gibt es nichts:\n\n" +
                missing.map { |name| "- #{name}\n" }.join)
        end
        FileUtils.chown_R(WORKSPACE_UID, WORKSPACE_GID, dir)

        title = exam_title(test['t'])
        title = 'leistungsueberpruefung' if title.strip.empty?
        args = ZipStream.command(WORKSPACE_UID, WORKSPACE_GID, '--skip-hidden', "=#{dir}", *trees)
        @respond_streaming = true
        content_type 'application/zip'
        attachment "#{title}-ergebnisse.zip"
        response.headers['Cache-Control'] = 'no-store'
        stream do |out|
            begin
                IO.popen(args, 'rb', :err => File::NULL) do |io|
                    while (chunk = io.read(65_536))
                        out << chunk
                    end
                end
            ensure
                FileUtils.rm_rf(dir)
            end
        end
    end

    get '/api/pdf_for_test_demo/:tag' do
        assert(teacher_logged_in?)
        email = @session_user[:email]
        test_tag = params['tag']
        neo4j_query_expect_one(<<~END_OF_QUERY, {:email => email, :test_tag => test_tag})
            MATCH (:User {email: $email})<-[:BELONGS_TO]-(t:Test {tag: $test_tag})
            RETURN t;
        END_OF_QUERY

        demo_tag = Main.test_demo_tag(email, test_tag)
        respond_raw_with_mimetype(test_results_html([email], demo_tag), 'text/html')
    end
end
