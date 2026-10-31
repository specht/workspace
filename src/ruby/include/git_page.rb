# The Workspace's own Git server on the site: a group's class repositories
# and its free ones, a person's own, the SSH key handed in, and the pages
# that show a repository in the browser (/repos, /git/…: tree, file,
# history, commit); GitServer and GitBrowse do the work. Moved out of
# main.rb, which loads it.
class Main < Sinatra::Base
    # The class repositories of a group (ClassRepos), from the dialog on the
    # class page: { slug => address } for pages with class_repo: group, an
    # empty address removes one. All or nothing: with one invalid address
    # nothing is saved, and the answer names it.
    post '/api/set_group_class_repos' do
        teacher_or_admin!
        data = parse_request_data(:required_keys => [:group, :repos], :types => { :repos => Hash },
            :max_body_length => 16384, :max_value_lengths => { :group => 256 })
        group = data[:group]
        halt 403 unless class_groups.include?(group)
        slugs = class_repo_slugs
        changes = {}
        errors = {}
        data[:repos].each do |slug, address|
            halt 400 unless slugs.include?(slug) && address.is_a?(String) && address.size <= 1000
            text = address.strip
            if text.empty? || ClassRepos.valid?(text)
                changes[slug] = text
            else
                errors[slug] = 'invalid'
            end
        end
        return respond(:ok => false, :errors => errors) unless errors.empty?

        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = row && row['repos']
        changes.each { |slug, address| json = ClassRepos.update(json, slug, address) }
        if json
            neo4j_query(<<~END_OF_QUERY, :group => group, :json => json)
                MERGE (g:GroupSetting {group: $group})
                SET g.class_repos = $json;
            END_OF_QUERY
        elsif row
            neo4j_query('MATCH (g:GroupSetting {group: $group}) REMOVE g.class_repos;', :group => group)
        end
        STDERR.puts "Class repositories of #{group} set by #{@session_user[:email]}: #{json || '{}'}"
        respond(:ok => true, :repos => ClassRepos.parse(json))
    end
    # --- Repositories of groups in the Workspace (GitServer) -----------------
    def git_teacher_of!(group)
        teacher_or_admin!
        halt 404 unless GitServer.enabled?
        halt 403 unless class_groups.include?(group)
    end
    # The folder of a group on the server; with create, a new one if it has none.
    def git_id_for_group(group, create: false)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.git_id AS id;', :group => group).first
        return row['id'] if row && row['id']
        return nil unless create

        neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.git_id = coalesce(g.git_id, $id);',
            :group => group, :id => GitServer.new_git_id)
        neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.git_id AS id;', :group => group).first['id']
    end
    # Sets (or with nil removes) one class repository of a group; returns the JSON.
    def git_set_class_repo(group, slug, address)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = ClassRepos.update(row && row['repos'], slug, address)
        if json
            neo4j_query('MERGE (g:GroupSetting {group: $group}) SET g.class_repos = $json;', :group => group, :json => json)
        elsif row
            neo4j_query('MATCH (g:GroupSetting {group: $group}) REMOVE g.class_repos;', :group => group)
        end
        json
    end
    # A teacher creates the repository of a page for the group, from the
    # page's starter/ folder if it has one; its address becomes the group's
    # class repository of that page.
    post '/api/git/create_repo' do
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => { :group => 256, :slug => 100 })
        group, slug = data[:group], data[:slug]
        git_teacher_of!(group)
        halt 400 unless class_repo_slugs.include?(slug) && slug.match?(GitServer::NAME)
        id = git_id_for_group(group, :create => true)
        unless File.exist?(GitServer.repo_path(id, slug))
            starter = File.join('/src/content', slug, 'starter')
            GitServer.create_repo(id, slug, :starter => File.directory?(starter) ? starter : nil)
        end
        address = GitServer.address(id, slug)
        json = git_set_class_repo(group, slug, address)
        Main.git_refresh!
        STDERR.puts "Git: repository #{id}/#{slug} of #{group} created by #{@session_user[:email]}"
        respond(:ok => true, :address => address, :repos => ClassRepos.parse(json))
    end
    # Deletes the group's repository of a page, with everything in it.
    post '/api/git/delete_repo' do
        data = parse_request_data(:required_keys => [:group, :slug], :max_value_lengths => { :group => 256, :slug => 100 })
        group, slug = data[:group], data[:slug]
        git_teacher_of!(group)
        halt 400 unless slug.match?(GitServer::NAME)
        id = git_id_for_group(group)
        halt 404 unless id
        GitServer.delete_repo(id, slug)
        row = neo4j_query('MATCH (g:GroupSetting {group: $group}) RETURN g.class_repos AS repos;', :group => group).first
        json = row && row['repos']
        # The page's address goes too, if it is this repository's.
        json = git_set_class_repo(group, slug, nil) if GitServer.parse_address(ClassRepos.parse(json)[slug]) == [id, slug]
        STDERR.puts "Git: repository #{id}/#{slug} of #{group} deleted by #{@session_user[:email]}"
        respond(:ok => true, :repos => ClassRepos.parse(json))
    end
    # The names of the repositories in a folder.
    def git_names(id)
        Dir.glob(File.join(GitServer::REPOS, id, '*.git')).map { |path| File.basename(path, '.git') }
    end
    # The README.md a new repository starts with, if it should.
    def git_readme(name, description)
        "# #{name}\n#{description.empty? ? '' : "\n#{description}\n"}"
    end
    # A free repository of a group, not tied to an activity, made on /repos:
    # in the group's folder, for its members and teachers like a class
    # repository, with the group's keys.
    post '/api/git/create_group_repo' do
        data = parse_request_data(:required_keys => [:group, :name], :optional_keys => [:description, :readme],
            :types => { :readme => Object }, :max_value_lengths => { :group => 256, :name => 100, :description => 400 }, :max_body_length => 2048)
        group, name = data[:group], data[:name].to_s.strip
        git_teacher_of!(group)
        id = git_id_for_group(group, :create => true)
        problem = GitServer.name_problem(name, :taken => git_names(id), :reserved => class_repo_slugs)
        return respond(:ok => false, :error => problem) if problem

        description = GitServer.clean_description(data[:description])
        GitServer.create_repo(id, name, :description => description, :readme => data[:readme] == true ? git_readme(name, description) : nil)
        Main.git_refresh!
        STDERR.puts "Git: free repository #{id}/#{name} of #{group} created by #{@session_user[:email]}"
        respond(:ok => true, :address => GitServer.address(id, name), :url => "#{WEB_ROOT}/git/#{id}/#{name}")
    end
    # Deletes a free repository of a group; the confirmation is its name.
    # One of an activity goes on the class page (delete_repo), with its address.
    post '/api/git/delete_group_repo' do
        data = parse_request_data(:required_keys => [:group, :name, :confirmation], :max_value_lengths => { :group => 256, :name => 100, :confirmation => 100 })
        group, name = data[:group], data[:name]
        git_teacher_of!(group)
        halt 400 unless name.match?(GitServer::NAME)
        return respond(:ok => false, :error => 'Der Name stimmt nicht.') unless data[:confirmation].to_s.strip == name

        id = git_id_for_group(group)
        halt 404 unless id && git_names(id).include?(name)
        tied = class_repo_slugs.include?(name) || (group_class_repos([group])[group] || {}).values.include?(GitServer.address(id, name))
        return respond(:ok => false, :error => 'Das Repository gehört zu einer Aktivität; du löschst es auf der Klassenseite.') if tied

        GitServer.delete_repo(id, name)
        STDERR.puts "Git: free repository #{id}/#{name} of #{group} deleted by #{@session_user[:email]}"
        respond(:ok => true)
    end
    # The folder of a person's own repositories; with create, a new one.
    def git_personal_id(email, create: false)
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_id AS id;', :email => email).first
        return row['id'] if row && row['id']
        return nil unless create

        id = nil
        5.times do
            id = GitServer.new_git_id
            used = neo4j_query('OPTIONAL MATCH (g:GroupSetting {git_id: $id}) OPTIONAL MATCH (u:User {git_id: $id}) RETURN count(g) + count(u) AS n;',
                :id => id).first['n'].to_i > 0
            break unless used || File.exist?(File.join(GitServer::REPOS, id))
        end
        neo4j_query('MATCH (u:User {email: $email}) SET u.git_id = coalesce(u.git_id, $id);', :email => email, :id => id)
        neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_id AS id;', :email => email).first['id']
    end
    # Whose own folder this is, or nil.
    def git_owner_of_id(id)
        row = neo4j_query('MATCH (u:User {git_id: $id}) RETURN u.email AS email;', :id => id).first
        row && row['email']
    end
    # A repository of one's own, for practice (»Meine Repositorys«): only
    # its owner may clone, push and read it, at most GitServer::PERSONAL_MAX.
    post '/api/git/create_my_repo' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        data = parse_request_data(:required_keys => [:name], :optional_keys => [:description, :readme],
            :types => { :readme => Object }, :max_value_lengths => { :name => 100, :description => 400 }, :max_body_length => 2048)
        name = data[:name].to_s.strip
        email = @session_user[:email]
        existing = (id = git_personal_id(email)) ? git_names(id) : []
        problem = GitServer.name_problem(name, :taken => existing, :limit => GitServer::PERSONAL_MAX)
        return respond(:ok => false, :error => problem) if problem

        id ||= git_personal_id(email, :create => true)
        description = GitServer.clean_description(data[:description])
        GitServer.create_repo(id, name, :description => description, :readme => data[:readme] == true ? git_readme(name, description) : nil)
        Main.git_refresh!
        respond(:ok => true, :address => GitServer.address(id, name), :url => "#{WEB_ROOT}/git/#{id}/#{name}")
    end
    post '/api/git/delete_my_repo' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        data = parse_request_data(:required_keys => [:name, :confirmation], :max_value_lengths => { :name => 100, :confirmation => 100 })
        name = data[:name]
        halt 400 unless name.match?(GitServer::NAME)
        return respond(:ok => false, :error => 'Der Name stimmt nicht.') unless data[:confirmation].to_s.strip == name

        id = git_personal_id(@session_user[:email])
        halt 404 unless id && git_names(id).include?(name)
        GitServer.delete_repo(id, name)
        respond(:ok => true)
    end
    # An administrator deletes someone's own repository (support), without
    # having seen into it; the confirmation is its name.
    post '/api/git/delete_personal_repo' do
        admins_only!
        halt 404 unless GitServer.enabled?
        data = parse_request_data(:required_keys => [:id, :name, :confirmation], :max_value_lengths => { :id => 20, :name => 100, :confirmation => 100 })
        id, name = data[:id], data[:name]
        halt 400 unless id.match?(GitServer::GIT_ID) && name.match?(GitServer::NAME)
        return respond(:ok => false, :error => 'Der Name stimmt nicht.') unless data[:confirmation].to_s.strip == name

        owner = git_owner_of_id(id)
        halt 404 unless owner && git_names(id).include?(name)
        GitServer.delete_repo(id, name)
        STDERR.puts "Git: own repository #{id}/#{name} deleted by the administrator #{@session_user[:email]}"
        respond(:ok => true)
    end
    # A key pair for the Workspace's Git server in one's own Workspace (as
    # a group in »Automatisch« gets it), for one's own repositories.
    # »Meine Repositorys«: the key for the Git server, which the person
    # makes in their Workspace and hands in, as on any Git server; the site
    # never makes one. The box shows the key handed in and takes it back
    # (git_key.js); without one, how to make it first.
    def git_my_key_html(email)
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_key AS key, u.git_auto_key AS auto;', :email => email).first || {}
        guide = row['key'] || row['auto'] ? '' :
            "<p><i class='bi bi-key' aria-hidden='true'></i>Zum Klonen und Pushen braucht der Git-Server den öffentlichen Schlüssel deines Workspace. " \
            'Erzeuge im Terminal deines Workspace ein Schlüsselpaar mit <code>ssh-keygen -t ed25519</code> und bestätige jede Frage mit Enter. ' \
            'Zeige dann den öffentlichen Schlüssel mit <code>cat ~/.ssh/id_ed25519.pub</code> an und füge die ganze Zeile unten ein. ' \
            'Den privaten Schlüssel, <code>~/.ssh/id_ed25519</code> ohne <code>.pub</code>, gibst du nie weiter. ' \
            "Mehr dazu im <a href='/reference/gitlab#ssh-workspace'>Handbuch</a>.</p>"
        "<div class='git-mine-key'>#{guide}<div class='git-key' data-git-key></div></div>"
    end
    # The person's public key in their Workspace, if there is one.
    def git_workspace_key(email)
        ssh = File.join('/user', fs_tag_for_email(email), 'workspace', '.ssh')
        %w[id_ed25519.pub id_ecdsa.pub id_rsa.pub].each do |name|
            path = File.join(ssh, name)
            next unless File.file?(path) && File.size(path) < 16_384

            key = GitServer.parse_key(File.read(path))
            return { :file => "~/.ssh/#{name}", :line => key[:line] } if key
        end
        nil
    end
    # For the box on a page: what the server knows of this person's key.
    post '/api/git/key_state' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        email = @session_user[:email]
        row = neo4j_query('MATCH (u:User {email: $email}) RETURN u.git_key AS key, u.git_auto_key AS auto;', :email => email).first || {}
        workspace = git_workspace_key(email)
        host = File.exist?(GitServer::HOST_KEY) ? GitServer.fingerprint(File.read(GitServer::HOST_KEY)) : nil
        respond(:handed_in => row['key'] && GitServer.fingerprint(row['key']),
                :workspace => workspace && { :file => workspace[:file], :fingerprint => GitServer.fingerprint(workspace[:line]) },
                :auto => !row['auto'].nil?,
                :host => host, :host_name => GitServer::HOST)
    end
    # The person hands in a public key, pasted as one line (as on any Git
    # server): ~/.ssh/id_ed25519.pub. A private key is refused with a word
    # on why; the comment of the key is not stored (GitServer.parse_key).
    post '/api/git/hand_in_key' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        data = parse_request_data(:required_keys => [:key], :max_value_lengths => { :key => 16_384 })
        text = data[:key].to_s.strip
        return respond(:ok => false, :error => 'private') if text.include?('PRIVATE KEY')

        key = GitServer.parse_key(text.lines.first.to_s)
        return respond(:ok => false, :error => 'invalid') unless key && text.lines.size == 1

        neo4j_query('MATCH (u:User {email: $email}) SET u.git_key = $key;', :email => @session_user[:email], :key => key[:line])
        Main.git_refresh!
        respond(:ok => true, :fingerprint => GitServer.fingerprint(key[:line]))
    end
    post '/api/git/remove_key' do
        assert(user_logged_in?)
        halt 404 unless GitServer.enabled?
        # auto: the key the site made earlier (git_auto_key), else the one
        # handed in; each is taken back by itself
        # without a body (an older page, a script): the key handed in
        body = request.body.read
        request.body.rewind
        data = body.strip.empty? ? {} : parse_request_data(:optional_keys => [:auto])
        field = data[:auto] ? 'git_auto_key' : 'git_key'
        neo4j_query("MATCH (u:User {email: $email}) REMOVE u.#{field};", :email => @session_user[:email])
        Main.git_refresh!
        respond(:ok => true)
    end
    # --- Reading a group's repository in the browser (GitBrowse) -------------
    #
    # /git/<git id>/<name>                        the files of the default branch, README below
    # /git/<git id>/<name>/tree/<ref>/<path>      a folder
    # /git/<git id>/<name>/blob/<ref>/<path>      a file
    # /git/<git id>/<name>/commits/<ref>[/<path>] the history, of one file too
    # /git/<git id>/<name>/commit/<sha>           a commit with its diff
    # /git/<git id>/<name>/archive/<ref>          the files as a zip
    # /git/<git id>/<name>/raw/<ref>/<path>       a file by itself (?download=1: as a download)
    # /git/<git id>/<name>/entries/<ref>/<path>   a folder's entries as JSON, for the sidebar
    #
    # For the members and teachers of the group the folder belongs to; a
    # person's own folder for that person alone (GitServer.may_read?).
    # Sinatra (Mustermann) anchors the pattern itself.
    GIT_BROWSE_PATH = %r{/git/([a-z0-9]{10})/([a-z0-9][a-z0-9-]{0,62})(?:/(tree|blob|commits|commit|archive|raw|entries)(?:/(.*))?)?/?}
    GIT_PICTURES = GitBrowse::PICTURES
    GIT_LANGUAGES = {
        '.rb' => 'ruby', '.py' => 'python', '.js' => 'javascript', '.mjs' => 'javascript', '.json' => 'json',
        '.md' => 'markdown', '.html' => 'html', '.htm' => 'html', '.css' => 'css', '.yaml' => 'yaml', '.yml' => 'yaml',
        '.sh' => 'bash', '.c' => 'c', '.h' => 'c', '.cpp' => 'cpp', '.hpp' => 'cpp', '.cc' => 'cpp', '.java' => 'java',
        '.dart' => 'dart', '.sql' => 'sql', '.go' => 'go', '.rs' => 'rust', '.lua' => 'lua', '.uml' => 'uml', '.er' => 'er',
        '.tex' => 'tex', '.cs' => 'cs', '.php' => 'php', '.svelte' => 'svelte', '.glsl' => 'glsl', '.frag' => 'glsl', '.vert' => 'glsl',
    }.freeze
    def git_group_of_id(id)
        row = neo4j_query('MATCH (g:GroupSetting {git_id: $id}) RETURN g.group AS group;', :id => id).first
        row && row['group']
    end
    def git_may_browse?(group)
        admin_logged_in? || my_groups.include?(group) || class_groups.include?(group)
    end
    # The page of a repository on the Workspace's Git server (its address),
    # if this person may look at it; nil otherwise and for other addresses.
    def git_browse_url(address)
        return nil unless GitServer.enabled?

        id, name = GitServer.parse_address(address.to_s)
        return nil unless id

        @git_groups_of_server ||= Main.git_groups
        group = @git_groups_of_server.find { |_, data| data[:id] == id }&.first
        group && git_may_browse?(group) ? "#{WEB_ROOT}/git/#{id}/#{name}" : nil
    end
    def git_url(*parts)
        base = "#{WEB_ROOT}/git/#{@git[:id]}/#{@git[:name]}"
        rest = parts.flatten.compact.map(&:to_s).reject(&:empty?).flat_map { |p| p.split('/') }.map { |p| ERB::Util.url_encode(p) }
        rest.empty? ? base : "#{base}/#{rest.join('/')}"
    end
    def git_time(time)
        time.localtime.strftime('%d.%m.%Y, %H:%M')
    end
    def git_size(bytes)
        return '' unless bytes
        return "#{bytes} Bytes" if bytes < 1024
        return format('%.1f KB', bytes / 1024.0).tr('.', ',') if bytes < 1024 * 1024

        format('%.1f MB', bytes / 1024.0 / 1024.0).tr('.', ',')
    end
    GIT_ICONS = {
        '.png' => 'bi-file-earmark-image', '.jpg' => 'bi-file-earmark-image', '.jpeg' => 'bi-file-earmark-image',
        '.gif' => 'bi-file-earmark-image', '.webp' => 'bi-file-earmark-image', '.svg' => 'bi-file-earmark-image',
        '.md' => 'bi-file-earmark-text', '.txt' => 'bi-file-earmark-text', '.csv' => 'bi-file-earmark-text',
        '.zip' => 'bi-file-earmark-zip', '.pdf' => 'bi-file-earmark-pdf',
    }.freeze
    GIT_STATUS = { :added => 'neu', :deleted => 'gelöscht', :renamed => 'umbenannt', :modified => 'geändert' }.freeze
    def git_icon(entry)
        return 'bi-folder-fill' if entry[:type] == :tree

        ext = File.extname(entry[:name]).downcase
        GIT_ICONS[ext] || (GIT_LANGUAGES.key?(ext) ? 'bi-file-earmark-code' : 'bi-file-earmark')
    end
    # How long ago, in words; the date and time on hover.
    def git_ago(time)
        seconds = (Time.now - time).to_i
        text = if seconds < 60 then 'gerade eben'
               elsif seconds < 3600 then seconds < 120 ? 'vor einer Minute' : "vor #{seconds / 60} Minuten"
               elsif seconds < 86_400 then seconds < 7200 ? 'vor einer Stunde' : "vor #{seconds / 3600} Stunden"
               elsif seconds < 2 * 86_400 then 'gestern'
               elsif seconds < 30 * 86_400 then "vor #{seconds / 86_400} Tagen"
               else "am #{time.getlocal.strftime('%d.%m.%Y')}"
               end
        "<time datetime='#{time.getutc.iso8601}' title='#{git_time(time.getlocal)}'>#{text}</time>"
    end
    # A file's code, highlighted like the site's code blocks, one row per
    # line with its number, a link to it (#L12).
    def git_code_html(text, path)
        # the site's own lexers first (UML, ER), then Rouge's guess by the name
        language = GIT_LANGUAGES[File.extname(path).downcase]
        lexer_class = (language && Rouge::Lexer.find(language)) || Rouge::Lexer.guess_by_filename(File.basename(path)) || Rouge::Lexers::PlainText
        lexer = language == 'php' ? lexer_class.new(:start_inline => !text.include?('<?')) : lexer_class.new
        lines = [[]]
        CodeHighlight.tokens(lexer, text).each do |token, value|
            value.split("\n", -1).each_with_index do |part, i|
                lines << [] if i > 0
                lines.last << [token, part] unless part.empty?
            end
        end
        lines.pop if lines.size > 1 && lines.last.empty?
        formatter = Rouge::Formatters::HTML.new
        rows = lines.each_with_index.map do |tokens, i|
            "<span class='git-line' id='L#{i + 1}'><a class='git-ln' href='#L#{i + 1}' aria-label='Zeile #{i + 1}'>#{i + 1}</a>" \
                "<span class='git-lc'>#{formatter.format(tokens)}</span></span>"
        end
        "<pre class='git-code no-copy' style='--digits: #{lines.size.to_s.size}'>#{rows.join}</pre>"
    end
    # A code block of the site's own (commands, not a file of the repository).
    def git_snippet_html(text, language)
        fragment = Nokogiri::HTML::DocumentFragment.parse("<pre><code class='#{language}'></code></pre>")
        fragment.at('code').content = text
        CodeHighlight.apply(fragment)
        fragment.to_html
    end
    # A README: Markdown without HTML of its own, code highlighted. Links
    # into the repository lead to its files (at ref, the path from the
    # file's folder dir), others only to http(s) and mailto. Pictures only
    # from the repository, at the commit sha (a picture is kept by the
    # browser, nginx's expires): one from elsewhere would tell its server who
    # reads the page, so its alt text stands instead.
    def git_markdown_html(text, ref, sha, dir)
        renderer = Redcarpet::Render::HTML.new(:filter_html => true, :no_styles => true)
        html = Redcarpet::Markdown.new(renderer, :fenced_code_blocks => true, :tables => true, :autolink => true).render(text)
        fragment = Nokogiri::HTML::DocumentFragment.parse(html)
        fragment.css('a[href]').each do |a|
            href = a['href'].to_s.strip
            next if href.start_with?('#') || href.match?(%r{\A(https?://|mailto:)}i)

            path = GitBrowse.repo_path(dir, href)
            path ? a['href'] = git_url('blob', ref, path) : a.remove_attribute('href')
        end
        fragment.css('img').each do |img|
            path = GitBrowse.picture_path(dir, img['src'])
            if path
                img['src'] = git_url('raw', sha, path)
                img['loading'] = 'lazy'
                img['alt'] ||= ''
                img.attributes.each_key { |name| img.remove_attribute(name) unless %w[src alt title loading].include?(name) }
            else
                img.replace(Nokogiri::XML::Text.new(img['alt'].to_s, fragment.document))
            end
        end
        fragment.css('pre > code').each { |code| code['class'] = 'text' unless code['class'].to_s.match?(/\A[a-z]+\z/) }
        CodeHighlight.apply(fragment)
        fragment.to_html
    end
    # A file's diff, with the numbers of its lines before and after.
    def git_diff_html(patch)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        before = after = 0
        row = ->(cls, old, new, text) { "<span class='git-dl #{cls}'><span class='git-dn'>#{old}</span><span class='git-dn'>#{new}</span><span class='git-dc'>#{h.(text)}</span></span>" }
        rows = patch.each_line.map do |line|
            line = line.chomp
            if line.start_with?('@@')
                m = line.match(/\A@@ -(\d+)(?:,\d+)? \+(\d+)/)
                before, after = m[1].to_i, m[2].to_i if m
                row.('git-hunk', '', '', line)
            elsif line.start_with?('+')
                after += 1
                row.('git-add', '', after - 1, line)
            elsif line.start_with?('-')
                before += 1
                row.('git-del', before - 1, '', line)
            elsif line.start_with?('\\', 'Binary files')
                row.('git-hunk', '', '', line)
            else
                before += 1
                after += 1
                row.('', before - 1, after - 1, line)
            end
        end
        "<pre class='git-diff no-copy'>#{rows.join}</pre>"
    end
    # [lines added, lines removed] of a file's diff.
    def git_diff_counts(patch)
        lines = patch.each_line.reject { |l| l.start_with?('@@') }
        [lines.count { |l| l.start_with?('+') }, lines.count { |l| l.start_with?('-') }]
    end
    # The button of the branch, tag or commit shown, with a menu of the
    # branches and tags (a tab each, a filter when there are many). Each
    # leads to the same view and path in that branch or tag.
    def git_ref_menu_html(ref, kind, view, path)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        refs = @git[:refs]
        default = GitBrowse.default_branch(@git[:repo], refs)
        icon = { :branch => 'bi-diagram-2', :tag => 'bi-tag' }[kind] || 'bi-record-circle'
        target = lambda do |name|
            case view
            when 'commits' then git_url('commits', name, path)
            when 'blob' then git_url('blob', name, path)
            when 'tree' then git_url('tree', name, path)
            else git_url('tree', name)
            end
        end
        tags_first = kind == :tag
        what = { :branch => 'Branch', :tag => 'Tag' }[kind] || 'Commit'
        io = +"<div class='dropdown git-refs'>"
        io << "<button class='btn btn-outline-secondary dropdown-toggle git-ref-button' type='button' data-bs-toggle='dropdown' " \
              "data-bs-auto-close='outside' aria-expanded='false' aria-label='#{what} #{h.(kind ? ref : ref.to_s[0, 7])}, anderen Stand wählen'>"
        io << "<i class='bi #{icon}' aria-hidden='true'></i><span class='git-ref-name'>#{h.(kind ? ref : ref.to_s[0, 7])}</span></button>"
        io << "<div class='dropdown-menu git-ref-menu' data-git-refs>"
        if refs[:branches].size + refs[:tags].size > 8
            io << "<div class='git-ref-search'><i class='bi bi-search' aria-hidden='true'></i><input type='search' class='form-control form-control-sm' " \
                  "placeholder='Branch oder Tag suchen' aria-label='Branch oder Tag suchen' autocomplete='off' data-git-ref-filter></div>"
        end
        io << "<div class='git-ref-tabs' role='tablist'>"
        [[:branches, 'Branches'], [:tags, 'Tags']].each do |key, title|
            selected = (key == :tags) == tags_first
            io << "<button type='button' role='tab' class='git-ref-tab' id='git-ref-tab-#{key}' aria-controls='git-ref-list-#{key}' " \
                  "aria-selected='#{selected}' data-git-ref-tab='#{key}'>#{title}<span class='git-count'>#{refs[key].size}</span></button>"
        end
        io << "</div>"
        item = lambda do |name, current, extra|
            "<a class='dropdown-item git-ref-item#{current ? ' active' : ''}' href='#{h.(target.(name))}'#{current ? " aria-current='true'" : ''} data-git-ref-name='#{h.(name)}'>" \
                "<i class='bi bi-check-lg git-ref-check' aria-hidden='true'></i><span class='git-ref-item-name'>#{h.(name)}</span>#{extra}</a>"
        end
        io << "<div class='git-ref-list' id='git-ref-list-branches' role='tabpanel' aria-labelledby='git-ref-tab-branches' data-git-ref-list='branches'#{tags_first ? ' hidden' : ''}>"
        refs[:branches].each do |b|
            io << item.(b[:name], kind == :branch && b[:name] == ref, b[:name] == default ? "<span class='git-ref-default'>Standard</span>" : '')
        end
        io << "</div>"
        io << "<div class='git-ref-list' id='git-ref-list-tags' role='tabpanel' aria-labelledby='git-ref-tab-tags' data-git-ref-list='tags'#{tags_first ? '' : ' hidden'}>"
        refs[:tags].each do |t|
            io << item.(t[:name], kind == :tag && t[:name] == ref, "<span class='git-ref-date'>#{h.(t[:time].getlocal.strftime('%d.%m.%Y'))}</span>")
        end
        if refs[:tags].empty?
            io << "<p class='git-ref-none'>Noch keine Tags. Einen Tag setzt du mit <code>git tag -a v1.0 -m \"Erste Version\"</code> " \
                  "und lädst ihn mit <code>git push --tags</code> hoch.</p>"
        end
        io << "</div>"
        io << "<p class='git-ref-none' data-git-ref-nothing hidden>Kein Branch und kein Tag mit diesem Namen.</p>"
        io << "</div></div>"
    end
    # Klonen (the address in a field with a button that copies it) and the
    # files of the commit shown as a zip.
    def git_clone_html(ref)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        address = GitServer.address(@git[:id], @git[:name])
        io = +"<div class='git-get'>"
        io << "<div class='dropdown'>"
        io << "<button class='btn btn-success dropdown-toggle' type='button' data-bs-toggle='dropdown' data-bs-auto-close='outside' aria-expanded='false'>" \
              "<i class='bi bi-code-slash' aria-hidden='true'></i>Klonen</button>"
        io << "<div class='dropdown-menu dropdown-menu-end git-clone-menu'>"
        io << "<label class='git-clone-label' for='git-clone-address'><i class='bi bi-terminal' aria-hidden='true'></i>Klonen über SSH</label>"
        io << "<div class='input-group input-group-sm'><input id='git-clone-address' class='form-control git-clone-input' type='text' readonly " \
              "value='#{h.(address)}' spellcheck='false' data-git-select>"
        io << "<button class='btn btn-outline-secondary git-copy' type='button' data-git-copy='#git-clone-address' title='Adresse kopieren' aria-label='Adresse kopieren'>" \
              "<i class='bi bi-copy' aria-hidden='true'></i></button></div>"
        io << "<p class='git-clone-hint'>Im Terminal deines Workspace: <code>git clone</code> und dahinter diese Adresse. " \
              "Von anderen Rechnern aus ist der Git-Server nicht zu erreichen.</p>"
        io << "</div></div>"
        if ref
            io << "<a class='btn btn-outline-secondary' href='#{h.(git_url('archive', ref))}' title='Alle Dateien dieses Stands als ZIP herunterladen'>" \
                  "<i class='bi bi-file-earmark-zip' aria-hidden='true'></i>ZIP</a>"
        end
        io << "</div>"
    end
    # Above every page: where the repository is, then the toolbar: the
    # sidebar's button, branches and tags, the tabs Dateien and Verlauf,
    # Klonen and ZIP.
    def git_header_html(ref:, kind:, tab:, view:, path: '', tab_ref: nil, sha: nil)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        tab_ref ||= ref
        io = +"<nav class='git-crumbs' aria-label='Repository'><i class='bi bi-git' aria-hidden='true'></i>"
        # the overview of every repository first, for those who have one (/repos)
        io << "<a href='#{WEB_ROOT}/repos'>Repositorys</a><span class='git-crumb-sep'>/</span>"
        io << (@git[:owner] ? "<a href='#{WEB_ROOT}/repos#meine'>Meine Repositorys</a>" : "<span>#{h.(@git[:group])}</span>")
        io << "<span class='git-crumb-sep'>/</span><a href='#{h.(git_url)}'><strong>#{h.(@git[:name])}</strong></a></nav>"
        io << "<div class='git-toolbar'><div class='git-toolbar-start'>"
        if ref
            io << "<button class='btn btn-outline-secondary git-side-toggle git-side-button' type='button' aria-controls='git-side' aria-expanded='true' " \
                  "title='Dateibaum ein- oder ausblenden'><i class='bi bi-layout-sidebar' aria-hidden='true'></i></button>"
            io << git_ref_menu_html(ref, kind, view, path)
            count = sha ? GitBrowse.count(@git[:repo], sha) : 0
            io << "<nav class='git-tabs' aria-label='Ansicht'>"
            io << "<a class='git-tab#{tab == :files ? ' active' : ''}' href='#{h.(git_url('tree', tab_ref))}'#{tab == :files ? " aria-current='page'" : ''}>" \
                  "<i class='bi bi-folder2-open' aria-hidden='true'></i>Dateien</a>"
            io << "<a class='git-tab#{tab == :history ? ' active' : ''}' href='#{h.(git_url('commits', tab_ref))}'#{tab == :history ? " aria-current='page'" : ''}>" \
                  "<i class='bi bi-clock-history' aria-hidden='true'></i>Verlauf#{count > 0 ? "<span class='git-count'>#{count}</span>" : ''}</a>"
            io << "</nav>"
        end
        io << "</div>#{git_clone_html(ref)}</div>"
        # a tag: its message and date
        tag = kind == :tag && @git[:refs][:tags].find { |t| t[:name] == ref }
        if tag
            io << "<div class='git-tag-note'><i class='bi bi-tag-fill' aria-hidden='true'></i><div><strong>#{h.(tag[:name])}</strong>"
            io << " <span class='git-tag-subject'>#{h.(tag[:subject])}</span>" unless tag[:subject].empty?
            io << "<div class='git-message'>#{h.(tag[:body])}</div>" unless tag[:body].empty?
            io << "<div class='git-meta'>#{tag[:tagger].empty? ? 'Gesetzt' : "Gesetzt von #{h.(tag[:tagger])}"} #{git_ago(tag[:time])}</div></div></div>"
        end
        io
    end
    # One commit in a line: who, what (a link to it), its hash and when.
    def git_commit_line_html(commit)
        return '' unless commit

        h = ->(t) { CGI.escapeHTML(t.to_s) }
        link = h.(git_url('commit', commit[:sha]))
        "<div class='git-commit-line'><span class='git-who'>#{h.(commit[:author])}</span>" \
            "<a class='git-subject' href='#{link}'>#{h.(commit[:subject])}</a>" \
            "<span class='git-when'><a class='git-sha' href='#{link}'>#{h.(commit[:short])}</a>, #{git_ago(commit[:time])}</span></div>"
    end
    # Where a folder or file is: a link per folder, a button that copies the path.
    def git_path_html(ref, path, prefix: nil)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        parts = path.split('/')
        links = ["<a href='#{h.(git_url('tree', ref))}'>#{h.(@git[:name])}</a>"]
        parts.each_with_index do |part, i|
            sub = parts[0..i].join('/')
            links << (i == parts.size - 1 ? "<strong>#{h.(part)}</strong>" : "<a href='#{h.(git_url('tree', ref, sub))}'>#{h.(part)}</a>")
        end
        "<div class='git-box-head git-path'>#{prefix}<span class='git-path-parts'>#{links.join("<span class='git-crumb-sep'>/</span>")}</span>" \
            "<button class='btn btn-sm git-copy git-path-copy' type='button' data-git-copy-text='#{h.(path)}' title='Pfad kopieren' aria-label='Pfad kopieren'>" \
            "<i class='bi bi-copy' aria-hidden='true'></i></button></div>"
    end
    # The sidebar: the files of a commit as a tree, the folders in open
    # open, path marked. With more than GitBrowse::TREE_MAX entries only
    # those folders are read; the others when they are opened (view entries).
    # changes: { path => { status:, anchor: } } for the files a commit
    # changed; they lead to their diff on the page.
    def git_tree_html(sha, ref, path, open, changes: {})
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        folders = GitBrowse.all_folders(@git[:repo], sha)
        complete = !folders.nil?
        folders ||= ([''] + open.to_a).uniq.to_h { |folder| [folder, GitBrowse.tree(@git[:repo], sha, folder).first(GitBrowse::TREE_MAX)] }
        render = lambda do |folder, depth|
            (folders[folder] || []).map do |e|
                current = e[:path] == path
                mark = current ? " aria-current='page'" : ''
                common = "role='treeitem' data-path='#{h.(e[:path])}' style='--depth: #{depth}'#{mark}"
                if e[:type] == :tree
                    is_open = open.include?(e[:path]) && folders.key?(e[:path])
                    lazy = folders.key?(e[:path]) ? '' : " data-git-lazy='#{h.(git_url('entries', ref, e[:path]))}'"
                    out = +"<li role='none'><a class='git-node git-node-folder#{current ? ' current' : ''}' href='#{h.(git_url('tree', ref, e[:path]))}' " \
                           "#{common} aria-expanded='#{is_open}'#{lazy}><i class='bi bi-chevron-right git-twisty' aria-hidden='true'></i>" \
                           "<i class='bi bi-folder-fill git-node-icon' aria-hidden='true'></i><span class='git-node-name'>#{h.(e[:name])}</span></a>"
                    out << "<ul role='group'#{is_open ? '' : ' hidden'}>#{render.(e[:path], depth + 1)}</ul>" if folders.key?(e[:path])
                    out << '</li>'
                else
                    change = changes[e[:path]]
                    href = change ? "##{change[:anchor]}" : git_url('blob', ref, e[:path])
                    dot = change ? "<span class='git-change git-change-#{change[:status]}' title='#{GIT_STATUS[change[:status]]}'>" \
                                   "<span class='visually-hidden'>#{GIT_STATUS[change[:status]]}</span></span>" : ''
                    "<li role='none'><a class='git-node#{current ? ' current' : ''}' href='#{h.(href)}' #{common}>" \
                        "<span class='git-twisty' aria-hidden='true'></span><i class='bi #{git_icon(e)} git-node-icon' aria-hidden='true'></i>" \
                        "<span class='git-node-name'>#{h.(e[:name])}</span>#{dot}</a></li>"
                end
            end.join
        end
        io = +"<div class='git-filter'><i class='bi bi-search' aria-hidden='true'></i>"
        io << "<input type='search' class='form-control form-control-sm' placeholder='Datei suchen' aria-label='Datei suchen' " \
              "aria-keyshortcuts='t' autocomplete='off' data-git-filter><kbd class='git-filter-key' aria-hidden='true'>t</kbd></div>"
        io << "<ul class='git-files' role='tree' aria-label='Dateien' data-git-complete='#{complete}'>#{render.('', 0)}</ul>"
        io << "<ul class='git-found' role='listbox' aria-label='Gefundene Dateien' hidden></ul>"
        io << "<p class='git-found-none' hidden>Keine Datei mit diesem Namen.</p>"
        io
    end
    # The folders on the way to path, and path if it is one.
    def git_open_folders(path, folder)
        parts = path.split('/')
        parts.pop unless folder
        (1..parts.size).map { |i| parts[0, i].join('/') }
    end
    def git_layout_html(side, main)
        # on a phone the tree is a section above the content, opened with this bar
        bar = "<button class='git-side-bar git-side-toggle' type='button' aria-controls='git-side' aria-expanded='false'>" \
              "<i class='bi bi-chevron-right' aria-hidden='true'></i>Dateibaum</button>"
        "<div class='git-layout'>#{bar}<aside class='git-side' id='git-side' aria-label='Dateibaum'>#{side}</aside>" \
            "<div class='git-main'>#{main}</div></div>"
    end
    # A folder: its entries with the last commit of each, its README below.
    def git_folder_html(sha, ref, path)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        repo = @git[:repo]
        entries = GitBrowse.tree(repo, sha, path)
        last = GitBrowse.last_commits(repo, sha, path)
        io = +"<div class='git-box'>"
        io << git_path_html(ref, path) unless path.empty?
        io << "<div class='git-box-head'>#{git_commit_line_html(GitBrowse.log(repo, sha, path, :max => 1).first)}</div>"
        io << "<table class='git-listing'><thead class='visually-hidden'><tr><th>Name</th><th>Letzter Commit</th><th>Wann</th></tr></thead><tbody>"
        unless path.empty?
            up = File.dirname(path)
            io << "<tr><td colspan='3'><a class='git-up' href='#{h.(git_url('tree', ref, up == '.' ? '' : up))}' title='Ein Ordner nach oben'>" \
                  "<i class='bi bi-arrow-90deg-up' aria-hidden='true'></i>..</a></td></tr>"
        end
        entries.each do |e|
            c = last[e[:name]]
            link = git_url(e[:type] == :tree ? 'tree' : 'blob', ref, e[:path])
            io << "<tr><td class='git-listing-name'><i class='bi #{git_icon(e)}' aria-hidden='true'></i><a href='#{h.(link)}'>#{h.(e[:name])}</a></td>"
            io << "<td class='git-listing-commit'>#{c ? "<a href='#{h.(git_url('commit', c[:sha]))}'>#{h.(c[:subject])}</a>" : ''}</td>"
            io << "<td class='git-listing-time'>#{c ? git_ago(c[:time]) : ''}</td></tr>"
        end
        io << "</tbody></table></div>"
        if entries.empty?
            io << "<p class='git-note'>Der Ordner ist leer.</p>"
        end
        readme = entries.find { |e| e[:type] == :blob && e[:name].match?(/\A(readme|liesmich)\.md\z/i) }
        file = readme && GitBrowse.blob(repo, sha, readme[:path])
        if file && file[:text]
            io << "<article class='git-box git-readme-box'><div class='git-box-head'><i class='bi bi-book' aria-hidden='true'></i>" \
                  "<a href='#{h.(git_url('blob', ref, readme[:path]))}'>#{h.(readme[:name])}</a></div>" \
                  "<div class='git-readme'>#{git_markdown_html(file[:text], ref, sha, path)}</div></article>"
        end
        io
    end
    # A file: the last commit that changed it, its size, Raw, download,
    # copy and its history; code with line numbers, a picture or a word that
    # it is no text.
    def git_file_html(sha, ref, path)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        repo = @git[:repo]
        file = GitBrowse.blob(repo, sha, path)
        halt 404 unless file
        picture = GIT_PICTURES[File.extname(path).downcase]
        markdown = File.extname(path).downcase == '.md' && file[:text]
        # what is shown first, and its source beside it
        shown = (markdown && 'Vorschau') || (picture && file[:text] && !file[:cut] && 'Bild')
        io = +"<div class='git-box'>"
        io << git_path_html(ref, path)
        io << "<div class='git-box-head'>#{git_commit_line_html(GitBrowse.log(repo, sha, path, :max => 1).first)}</div>"
        io << "<div class='git-file-head'><div class='git-file-head-start'>"
        if shown
            io << "<div class='btn-group btn-group-sm git-view-switch' role='group' aria-label='Ansicht'>" \
                  "<button type='button' class='btn btn-outline-secondary active' aria-pressed='true' data-git-show='preview'>" \
                  "<i class='bi #{markdown ? 'bi-eye' : 'bi-image'}' aria-hidden='true'></i>#{shown}</button>" \
                  "<button type='button' class='btn btn-outline-secondary' aria-pressed='false' data-git-show='code'><i class='bi bi-code' aria-hidden='true'></i>Quelltext</button></div>"
        end
        info = []
        info << (file[:text].count("\n") + (file[:text].end_with?("\n") || file[:text].empty? ? 0 : 1)).then { |n| n == 1 ? '1 Zeile' : "#{n} Zeilen" } if file[:text] && !file[:cut]
        info << git_size(file[:size])
        io << "<span class='git-file-info'>#{h.(info.join(', '))}</span></div>"
        io << "<div class='git-file-actions'><div class='btn-group btn-group-sm' role='group' aria-label='Datei'>"
        if file[:size] <= GitBrowse::MAX_RAW
            # a picture at the commit: the browser keeps pictures (nginx's expires)
            raw = git_url('raw', picture ? sha : ref, path)
            io << "<a class='btn btn-outline-secondary' href='#{h.(raw)}' title='Die Datei für sich, ohne diese Seite'>Raw</a>" unless file[:binary] && !picture
            io << "<a class='btn btn-outline-secondary' href='#{h.(raw)}?download=1' title='Herunterladen' aria-label='Herunterladen'>" \
                  "<i class='bi bi-download' aria-hidden='true'></i></a>"
        end
        if file[:text] && !file[:cut]
            io << "<button class='btn btn-outline-secondary git-copy' type='button' data-git-copy-code title='Inhalt kopieren' aria-label='Inhalt kopieren'>" \
                  "<i class='bi bi-copy' aria-hidden='true'></i></button>"
        end
        io << "</div><a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('commits', ref, path))}' title='Die Commits, die diese Datei geändert haben'>" \
              "<i class='bi bi-clock-history' aria-hidden='true'></i>Verlauf</a></div></div>"
        if picture
            # at the commit, not the branch: nginx lets the browser keep a picture
            io << "<div class='git-picture' data-git-view='preview'><img src='#{h.(git_url('raw', sha, path))}' alt='#{h.(File.basename(path))}'></div>"
            io << "<div data-git-view='code' hidden>#{git_code_html(file[:text], path)}</div>" if shown
        elsif file[:binary]
            io << "<div class='git-binary'><i class='bi bi-file-earmark-binary' aria-hidden='true'></i>" \
                  "<p>Keine Textdatei: Ihr Inhalt lässt sich hier nicht lesen. Mit <i class='bi bi-download' aria-label='Herunterladen'></i> lädst du sie herunter.</p></div>"
        elsif file[:text].empty?
            io << "<p class='git-binary'>Die Datei ist leer.</p>"
        elsif markdown
            io << "<div class='git-readme' data-git-view='preview'>#{git_markdown_html(file[:text], ref, sha, File.dirname(path).sub(/\A\.\z/, ''))}</div>"
            io << "<div data-git-view='code' hidden>#{git_code_html(file[:text], path)}</div>"
        else
            io << git_code_html(file[:text], path)
        end
        io << "</div>"
        io << "<p class='git-note'>Die Datei ist länger; hier stehen die ersten #{GitBrowse::MAX_BLOB / 1024} KB.</p>" if file[:cut] && !file[:binary]
        io
    end
    # The history, of one file too: a list per day, tags marked.
    def git_history_html(sha, ref, path, folder)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        page = [params['seite'].to_i, 1].max
        log = GitBrowse.log(@git[:repo], sha, path, :skip => (page - 1) * GitBrowse::PAGE, :max => GitBrowse::PAGE + 1)
        tags = @git[:refs][:tags].group_by { |t| t[:sha] }
        io = +"<div class='git-box git-history'>"
        io << git_path_html(ref, path, :prefix => "<span class='git-path-label'>Verlauf von</span>") unless path.empty?
        log.first(GitBrowse::PAGE).chunk_while { |a, b| a[:time].getlocal.strftime('%F') == b[:time].getlocal.strftime('%F') }.each do |day|
            io << "<h2 class='git-day'><i class='bi bi-calendar3' aria-hidden='true'></i>Commits am #{day.first[:time].getlocal.strftime('%d.%m.%Y')}</h2>"
            io << "<ol class='git-log'>"
            day.each do |c|
                link = h.(git_url('commit', c[:sha]))
                io << "<li><div class='git-log-text'><a class='git-log-subject' href='#{link}'>#{h.(c[:subject])}</a>"
                (tags[c[:sha]] || []).each do |t|
                    io << "<a class='git-tag-badge' href='#{h.(git_url('tree', t[:name]))}' title='Tag #{h.(t[:name])}'><i class='bi bi-tag-fill' aria-hidden='true'></i>#{h.(t[:name])}</a>"
                end
                io << "<div class='git-meta'><span class='git-who'>#{h.(c[:author])}</span> #{git_ago(c[:time])}</div></div>"
                io << "<div class='git-log-actions btn-group btn-group-sm'><a class='btn btn-outline-secondary git-sha' href='#{link}' title='Commit ansehen'>#{h.(c[:short])}</a>"
                io << "<a class='btn btn-outline-secondary' href='#{h.(git_url(folder ? 'tree' : 'blob', c[:sha], path))}' " \
                      "title='Die Dateien zu diesem Stand' aria-label='Die Dateien zu diesem Stand'><i class='bi bi-code-slash' aria-hidden='true'></i></a></div></li>"
            end
            io << "</ol>"
        end
        io << "<p class='git-note git-diff-none'>Noch keine Commits.</p>" if log.empty?
        io << "</div>"
        if page > 1 || log.size > GitBrowse::PAGE
            io << "<nav class='git-pager' aria-label='Seiten'>"
            io << (page > 1 ? "<a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('commits', ref, path))}#{page > 2 ? "?seite=#{page - 1}" : ''}'>" \
                              "<i class='bi bi-chevron-left' aria-hidden='true'></i>Neuere</a>" : '<span></span>')
            if log.size > GitBrowse::PAGE
                io << "<a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('commits', ref, path))}?seite=#{page + 1}'>Ältere<i class='bi bi-chevron-right' aria-hidden='true'></i></a>"
            end
            io << "</nav>"
        end
        io
    end
    # A commit: its message, who and when, the commits before it, then the
    # diff of every file it changed.
    def git_commit_html(commit)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        subject, body = commit[:message].split("\n", 2)
        io = +"<div class='git-box git-commit-box'><div class='git-commit-text'><h2>#{h.(subject)}</h2>"
        @git[:refs][:tags].select { |t| t[:sha] == commit[:sha] }.each do |t|
            io << "<a class='git-tag-badge' href='#{h.(git_url('tree', t[:name]))}'><i class='bi bi-tag-fill' aria-hidden='true'></i>#{h.(t[:name])}</a>"
        end
        io << "<div class='git-message'>#{h.(body.strip)}</div>" if body && !body.strip.empty?
        io << "</div><div class='git-box-foot'><span class='git-who'>#{h.(commit[:author])}</span> #{git_ago(commit[:time])}"
        io << "<span class='git-commit-ids'>"
        commit[:parents].each { |parent| io << "<span>davor <a class='git-sha' href='#{h.(git_url('commit', parent))}'>#{h.(parent[0, 7])}</a></span>" }
        io << "<span>Commit <span class='git-sha'>#{h.(commit[:short])}</span><button class='btn btn-sm git-copy' type='button' data-git-copy-text='#{h.(commit[:sha])}' " \
              "title='Hash kopieren' aria-label='Hash kopieren'><i class='bi bi-copy' aria-hidden='true'></i></button></span>"
        io << "<a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('tree', commit[:sha]))}'><i class='bi bi-code-slash' aria-hidden='true'></i>Dateien zu diesem Stand</a>"
        io << "</span></div></div>"
        counts = commit[:files].map { |f| git_diff_counts(f[:patch]) }
        unless commit[:files].empty?
            n = commit[:files].size
            io << "<p class='git-diff-summary'><i class='bi bi-file-diff' aria-hidden='true'></i>#{n == 1 ? '1 Datei' : "#{n} Dateien"} geändert, " \
                  "<span class='git-plus'>#{counts.sum(&:first) == 1 ? '1 Zeile' : "#{counts.sum(&:first)} Zeilen"} dazu</span>, " \
                  "<span class='git-minus'>#{counts.sum(&:last)} weg</span></p>"
        end
        commit[:files].each_with_index do |file, i|
            plus, minus = counts[i]
            io << "<section class='git-box git-file-diff' id='diff-#{i + 1}'><div class='git-box-head git-diff-head'>"
            io << "<span class='git-status git-status-#{file[:status]}'>#{GIT_STATUS[file[:status]]}</span>"
            io << "<h3 class='git-diff-path'>#{file[:old_path] ? "#{h.(file[:old_path])} <i class='bi bi-arrow-right' aria-label='nach'></i> " : ''}#{h.(file[:path])}</h3>"
            io << "<span class='git-diff-counts'><span class='git-plus'>+#{plus}</span> <span class='git-minus'>−#{minus}</span></span>"
            unless file[:status] == :deleted
                io << "<a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('blob', commit[:sha], file[:path]))}'>Datei ansehen</a>"
            end
            io << "</div>"
            io << (file[:patch].empty? ? "<p class='git-note git-diff-none'>Ohne Änderung am Inhalt.</p>" : git_diff_html(file[:patch]))
            io << "</section>"
        end
        io << "<p class='git-note'>Der Rest dieses Commits ist zu groß, um ihn hier zu zeigen.</p>" if commit[:cut]
        io << "<p class='git-note'>Dieser Commit ändert keine Datei.</p>" if commit[:files].empty? && !commit[:cut]
        io
    end
    def git_page_html(view, rest)
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        repo = @git[:repo]
        refs = @git[:refs] = GitBrowse.refs(repo)
        default = GitBrowse.default_branch(repo, refs)
        if view == 'commit'
            sha = rest.to_s.split('/').first.to_s
            halt 404 unless sha.match?(GitBrowse::SHA)
            commit = GitBrowse.commit(repo, sha)
            halt 404 unless commit
            io = git_header_html(:ref => commit[:sha], :kind => nil, :tab => :history, :view => 'commit', :tab_ref => default, :sha => commit[:sha])
            changes = commit[:files].each_with_index.to_h { |f, i| [f[:path], { :status => f[:status], :anchor => "diff-#{i + 1}" }] }
            open = changes.keys.flat_map { |p| git_open_folders(p, false) }.uniq
            return io << git_layout_html(git_tree_html(commit[:sha], commit[:sha], '', open, :changes => changes), git_commit_html(commit))
        end

        if default.nil?
            io = git_header_html(:ref => nil, :kind => nil, :tab => :files, :view => 'tree')
            io << "<div class='git-empty'><p>Das Repository ist noch leer.</p><p>So kommt der erste Commit hinein:</p>"
            io << git_snippet_html("git clone #{GitServer.address(@git[:id], @git[:name])}\ncd #{@git[:name]}\n" \
                "# Dateien anlegen, dann:\ngit add -A\ngit commit -m \"Der Anfang\"\ngit push\n", 'bash') << "</div>"
            return io
        end
        ref, path, kind = rest.to_s.empty? ? [default, '', :branch] : GitBrowse.split_ref(rest, refs)
        sha = GitBrowse.commit_of(repo, ref, kind, refs)
        halt 404 unless sha
        path = GitBrowse.clean_path(path)
        halt 404 unless path

        if view == 'commits'
            io = git_header_html(:ref => ref, :kind => kind, :tab => :history, :view => 'commits', :path => path, :sha => sha)
            folder = path.empty? || GitBrowse.kind(repo, sha, path) != :blob
            return io << git_layout_html(git_tree_html(sha, ref, path, git_open_folders(path, folder)), git_history_html(sha, ref, path, folder))
        end

        kind_here = GitBrowse.kind(repo, sha, path)
        shown = kind_here == :blob ? 'blob' : 'tree'
        io = git_header_html(:ref => ref, :kind => kind, :tab => :files, :view => shown, :path => kind_here ? path : '', :sha => sha)
        unless kind_here
            # not in this branch or tag (one chosen in the menu, say): its files
            # instead, with 404 (the route sends it, respond_raw only with 200)
            @git_status = 404
            main = "<div class='git-box git-missing'><i class='bi bi-signpost-split' aria-hidden='true'></i><p>In <strong>#{h.(kind ? ref : ref[0, 7])}</strong> " \
                   "gibt es <code>#{h.(path)}</code> nicht.</p><a class='btn btn-sm btn-outline-secondary' href='#{h.(git_url('tree', ref))}'>Zu den Dateien</a></div>"
            return io << git_layout_html(git_tree_html(sha, ref, '', []), main)
        end
        side = git_tree_html(sha, ref, path, git_open_folders(path, kind_here == :tree))
        io << git_layout_html(side, kind_here == :tree ? git_folder_html(sha, ref, path) : git_file_html(sha, ref, path))
    end
    # Every repository on the server in the groups the person teaches (an
    # administrator: all, with their teachers; a student: their own groups),
    # a free one made here, and the person's own repositories for practice;
    # for an administrator, that such exist. One git call per repository,
    # for its last commit.
    # /repos, not /git: that is the activity »Versionsverwaltung mit Git«
    get %r{/repos/?} do
        this_is_a_page_for_logged_in_users!
        halt 404 unless GitServer.enabled?
        teacher = teacher_account_logged_in? || admin_logged_in?
        admin = admin_logged_in?
        h = ->(t) { CGI.escapeHTML(t.to_s) }
        git_groups = Main.git_groups
        groups = (admin ? git_groups.keys.sort_by(&:downcase) : teacher ? class_groups : my_groups).select { |g| git_groups.key?(g) }
        teachers = nil
        if admin
            teachers = Hash.new { |hash, key| hash[key] = [] }
            @@teachers.each do |email, taught|
                name = (@@invitations[email] || {})[:name] || email
                taught.each { |g| teachers[g] << name }
            end
        end
        slugs = class_repo_slugs
        rows = GitOverview.rows(:groups => groups, :git_groups => git_groups, :repos => ->(id) { GitServer.repos(id) },
            :titles => ->(slug) { slugs.include?(slug) && @@content[slug] ? plain_title(slug) : nil },
            :class_repos => group_class_repos(groups), :teachers => teachers)
        email = @session_user[:email]
        id = git_personal_id(email)
        mine = id ? GitServer.repos(id) : []
        # every section says why this person sees it: by which role
        role = ->(icon, text) { "<p class='git-overview-role'><i class='bi bi-#{icon}' aria-hidden='true'></i>#{text}</p>" }
        names = groups.empty? ? '' : " (#{h.(groups.join(', '))})"
        why = if admin then role.('shield-lock', 'Du siehst diesen Teil als Administration: alle Gruppen, mit ihren Lehrkräften.')
              elsif teacher then role.('person-video3', "Du siehst diesen Teil als Lehrkraft: die Gruppen, die du unterrichtest#{names}.")
              else role.('people', "Du siehst diesen Teil als Mitglied deiner Gruppen#{names}.")
              end
        page = +"<div class='container git-overview-page'><h1>Git-Repositorys</h1>"
        page << "<section class='git-overview-section'><div class='git-overview-head'><h2>Repositorys der Gruppen</h2>"
        page << GitOverview.new_form_html(:groups => class_groups) if teacher && !class_groups.empty?
        page << "</div>#{why}#{GitOverview.html(rows, :admin => admin, :student => !teacher, :web_root => WEB_ROOT)}</section>"
        full = mine.size >= GitServer::PERSONAL_MAX ? "Du hast schon #{GitServer::PERSONAL_MAX} Repositorys." : nil
        page << "<section class='git-overview-section' id='meine'><div class='git-overview-head'><h2>Meine Repositorys</h2>"
        page << "#{GitOverview.new_form_html(:disabled => full)}</div>"
        page << role.('person', 'Diesen Teil hat jede Person für sich, in jeder Rolle.')
        page << "<p class='git-overview-intro'>Zum Üben, nur für dich: Niemand sonst kann sie klonen, pushen oder ansehen, auch deine Lehrkräfte nicht. " \
            "Was ihr gemeinsam macht, gehört in ein Repository der Gruppe. #{mine.size} von #{GitServer::PERSONAL_MAX}.</p>"
        page << git_my_key_html(email)
        page << GitOverview.personal_html(mine, :id => id, :web_root => WEB_ROOT)
        page << '</section>'
        if admin
            owners = neo4j_query('MATCH (u:User) WHERE u.git_id IS NOT NULL RETURN u.email AS email, u.git_id AS id;').to_a
            personal = owners.flat_map do |row|
                name = (@@invitations[row['email']] || {})[:name] || row['email']
                GitServer.repos(row['id']).map { |r| r.merge(:owner => name, :id => row['id']) }
            end.sort_by { |r| [r[:owner].downcase, r[:name]] }
            page << "<section class='git-overview-section'><h2>Eigene Repositorys der Personen</h2>"
            page << role.('shield-lock', 'Diesen Teil sieht nur die Administration.')
            page << "<p class='git-overview-intro'>Repositorys, die jemand für sich zum Üben angelegt hat. Hineinsehen kann nur die Person selbst; " \
                'du siehst, dass es sie gibt, und kannst eins löschen, etwa wenn der Platz knapp wird.</p>'
            page << GitOverview.personal_admin_html(personal) << '</section>'
        end
        cache = "#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}"
        page << "</div><script src='/include/git_repos.js?#{cache}'></script><script src='/include/git_key.js?#{cache}'></script>"
        @git_page = page
        running_tests = my_running_tests()
        path = request.path
        slug = nil
        template = File.read(File.join(@@static_dir, '_template.html'))
        # as on the repository pages below: the page is the literal, the
        # table comes in as a value and is never read as a template
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, '#{@git_page}', binding), request.path)
        respond_raw_with_mimetype(html, 'text/html')
    end
    get GIT_BROWSE_PATH do
        this_is_a_page_for_logged_in_users!
        halt 404 unless GitServer.enabled?
        id, name, view, rest = params['captures']
        rest = rest.to_s.split('/').map { |part| URI.decode_www_form_component(part.gsub('+', '%2B')) }.join('/')
        group = git_group_of_id(id)
        # a person's own folder: that person alone, no teacher, no administrator
        owner = group ? nil : git_owner_of_id(id)
        reader = group && git_may_browse?(group)
        halt_forbidden_page! unless (group || owner) && GitServer.may_read?(:owner => owner, :group_reader => reader, :email => @session_user[:email])
        repo = GitServer.repo_path(id, name)
        halt 404 unless File.directory?(repo)
        @git = { :id => id, :name => name, :group => group, :owner => owner, :repo => repo }

        if %w[raw archive entries].include?(view)
            refs = GitBrowse.refs(repo)
            ref, path, kind = GitBrowse.split_ref(rest, refs)
            sha = GitBrowse.commit_of(repo, ref, kind, refs)
            path = sha && GitBrowse.clean_path(path)
            halt 404 unless path
        end
        if view == 'raw'
            halt 404 unless !path.empty? && GitBrowse.kind(repo, sha, path) == :blob
            file = GitBrowse.blob(repo, sha, path, :max => GitBrowse::MAX_RAW)
            halt 404 unless file && !file[:cut]
            # A picture in an <img> or the file as text; as a page of its own
            # nothing in it runs (an SVG's scripts neither), nothing is read as HTML.
            raw = GitBrowse.raw_headers(path, file[:binary], params['download'] == '1')
            type = raw.delete('Content-Type')
            headers raw
            return respond_raw_with_mimetype(file[:data], type)
        end
        if view == 'archive'
            zip = GitBrowse.git(repo, 'archive', '--format=zip', "--prefix=#{name}/", '--end-of-options', sha,
                :binary => true, :max => GitBrowse::MAX_ARCHIVE, :seconds => 30)
            halt 404 unless zip
            return respond_raw_with_mimetype_and_filename(zip, 'application/zip', "#{name}.zip")
        end
        if view == 'entries'
            # a folder of the sidebar, when the tree is too big to come at once
            halt 404 unless GitBrowse.kind(repo, sha, path) == :tree
            @git[:refs] = refs
            entries = GitBrowse.tree(repo, sha, path).first(GitBrowse::TREE_MAX).map do |e|
                { :name => e[:name], :path => e[:path], :type => e[:type], :icon => git_icon(e),
                  :href => git_url(e[:type] == :tree ? 'tree' : 'blob', ref, e[:path]),
                  :entries => e[:type] == :tree ? git_url('entries', ref, e[:path]) : nil }
            end
            headers 'Cache-Control' => 'private, no-store'
            return respond(:entries => entries)
        end

        script = "<script src='/include/git_browse.js?#{CACHE_BUSTER}#{DEVELOPMENT ? Time.now.to_i : nil}'></script>"
        @git_page = "<div class='git-browse' data-git-base='#{CGI.escapeHTML(git_url)}'>#{git_page_html(view, rest)}</div>#{script}"
        # what the template reads, as in get '/*'
        running_tests = my_running_tests()
        path = request.path
        slug = nil
        template = File.read(File.join(@@static_dir, '_template.html'))
        # The page is the literal below; the repository's text comes in as the
        # value of @git_page and is never read as a template (TrustedTemplate).
        html = Main.inject_page_metadata(TrustedTemplate.render_page(template, '#{@git_page}', binding), request.path)
        halt @git_status, { 'Content-Type' => 'text/html' }, html if @git_status
        respond_raw_with_mimetype(html, 'text/html')
    end
end
