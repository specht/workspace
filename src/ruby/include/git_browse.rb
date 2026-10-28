require 'open3'
require 'time'

# Reading a group's repository for the pages under /git/<git id>/<name>
# (GitServer): branches and tags, the files of a commit, a file, the
# history and a commit with its diff. Read-only, with git's plumbing; a ref
# or a path from the address is checked before git sees it, and git gets it
# after --end-of-options or --, never through a shell. Every call is bounded
# in time and in the output it may give. Author names only, no e-mail
# addresses. Tested in test/git_browse_test.rb.
module GitBrowse
    MAX_BLOB = 512 * 1024
    MAX_DIFF = 300 * 1024
    # what a file may have to be downloaded, and the zip of a commit
    MAX_RAW = 20 * 1024 * 1024
    MAX_ARCHIVE = 50 * 1024 * 1024
    # what any other call may print, and how long it may take
    MAX_OUTPUT = 2 * 1024 * 1024
    SECONDS = 5
    # the files of a commit in the sidebar at once up to this many entries;
    # more, and its folders are read one at a time (Main, git_tree_html)
    TREE_MAX = 1000
    # commits looked at for the last commit of each entry of a folder
    LAST_COMMITS = 300
    PAGE = 50
    # what is shown as a picture, and its type
    PICTURES = { '.png' => 'image/png', '.jpg' => 'image/jpeg', '.jpeg' => 'image/jpeg', '.gif' => 'image/gif',
                 '.webp' => 'image/webp', '.svg' => 'image/svg+xml' }.freeze
    REF = %r{\A(?!-)(?!.*\.\.)[A-Za-z0-9._/-]{1,200}\z}
    SHA = /\A[0-9a-f]{7,64}\z/

    # git with these arguments in the repository: { out:, cut:, ok: }. out
    # stops after max bytes (cut: true); ok is false if git failed or took
    # longer than seconds, after which it is stopped.
    def self.run(repo, *args, binary: false, max: MAX_OUTPUT, seconds: SECONDS)
        out = ''.b
        cut = false
        late = false
        IO.popen(['git', '-c', 'safe.directory=*', '--git-dir', repo, *args], 'rb', :in => File::NULL, :err => File::NULL) do |io|
            deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + seconds
            loop do
                left = deadline - Process.clock_gettime(Process::CLOCK_MONOTONIC)
                late = left <= 0 || IO.select([io], nil, nil, left).nil?
                break if late

                chunk = io.read_nonblock(64 * 1024, :exception => false)
                break if chunk.nil?
                next if chunk == :wait_readable

                out << chunk
                next unless out.bytesize > max

                out = out.byteslice(0, max)
                cut = true
                break
            end
            Process.kill('KILL', io.pid) if late || cut
        rescue Errno::ESRCH
            nil
        end
        ok = !late && (cut || $?.success?)
        out = out.force_encoding('UTF-8').scrub unless binary
        { :out => out, :cut => cut, :ok => ok }
    end

    # git's whole output, or nil if it failed, took too long or printed more than max.
    def self.git(repo, *args, binary: false, max: MAX_OUTPUT, seconds: SECONDS)
        result = run(repo, *args, :binary => binary, :max => max, :seconds => seconds)
        result[:ok] && !result[:cut] ? result[:out] : nil
    end

    # A link or picture in a Markdown file in folder dir: the path it names
    # in the repository, or nil for one elsewhere (http://…, //host/…, data:)
    # and for a path that leaves the repository. A path from / starts at the
    # repository's top.
    def self.repo_path(dir, src)
        src = src.to_s.strip.sub(/[?#].*\z/m, '')
        return nil if src.empty? || src.match?(/\A[a-z][a-z0-9+.-]*:/i) || src.start_with?('//')

        parts = src.start_with?('/') ? [] : dir.to_s.split('/').reject(&:empty?)
        src.split('/').each do |part|
            next if part.empty? || part == '.'
            if part == '..'
                return nil if parts.empty?

                parts.pop
            else
                parts << part
            end
        end
        path = clean_path(parts.join('/'))
        path && !path.empty? ? path : nil
    end

    # The same for a picture: nil for what is no picture too.
    def self.picture_path(dir, src)
        path = repo_path(dir, src)
        path && PICTURES.key?(File.extname(path).downcase) ? path : nil
    end

    # The headers of a file on its own (the raw view): a picture as itself,
    # the rest as text or a download; nothing in it runs (sandbox, no
    # scripts, no styles from elsewhere) and the browser takes the type as
    # given (nosniff).
    def self.raw_headers(path, binary, download)
        type = PICTURES[File.extname(path).downcase] || (binary ? 'application/octet-stream' : 'text/plain; charset=utf-8')
        attach = download || type == 'application/octet-stream'
        { 'Content-Type' => type, 'X-Content-Type-Options' => 'nosniff', 'Content-Security-Policy' => "sandbox; default-src 'none'",
          'Cache-Control' => 'private, max-age=60',
          'Content-Disposition' => "#{attach ? 'attachment' : 'inline'}; filename*=UTF-8''#{path.split('/').last.b.gsub(/[^A-Za-z0-9._~-]/) { |c| format('%%%02X', c.ord) }}" }
    end

    # A path inside a commit, as the address names it; nil if it is none.
    def self.clean_path(path)
        parts = path.to_s.split('/').reject(&:empty?)
        return '' if parts.empty?
        return nil if parts.any? { |p| p == '.' || p == '..' || p.start_with?('-') || p.match?(/[\x00-\x1f]/) }

        parts.join('/')
    end

    # The commit a ref (a branch, a tag, a hash) stands for, or nil.
    def self.resolve(repo, ref)
        return nil unless ref.to_s.match?(REF)

        out = git(repo, 'rev-parse', '--verify', '--quiet', '--end-of-options', "#{ref}^{commit}")
        out && out.strip
    end

    # Branches and tags with one call: { branches: [{ name:, sha: }], tags:
    # [{ name:, sha: (the commit), time:, tagger:, subject:, body: }] }.
    # Branches by name, tags newest first; names an address could not carry
    # (REF) and tags of anything but a commit are left out. A tag without a
    # message of its own (lightweight) has the time of its commit.
    def self.refs(repo)
        format = '%(refname)%1f%(objectname)%1f%(*objectname)%1f%(*objecttype)%1f%(creatordate:unix)' \
                 '%1f%(if:equals=tag)%(objecttype)%(then)%(taggername)%1f%(contents:subject)%1f%(contents:body)%(end)%00'
        out = git(repo, 'for-each-ref', "--format=#{format}", 'refs/heads', 'refs/tags') || ''
        list = { :branches => [], :tags => [] }
        out.split("\0\n").each do |record|
            refname, sha, peeled, peeled_type, time, tagger, subject, body = record.sub(/\A\n/, '').split("\x1f", 8)
            next unless refname && sha
            if refname.start_with?('refs/heads/')
                name = refname.delete_prefix('refs/heads/')
                list[:branches] << { :name => name, :sha => sha } if name.match?(REF)
            elsif refname.start_with?('refs/tags/')
                name = refname.delete_prefix('refs/tags/')
                next unless name.match?(REF)
                next if !peeled.to_s.empty? && peeled_type != 'commit'

                list[:tags] << { :name => name, :sha => peeled.to_s.empty? ? sha : peeled, :time => Time.at(time.to_i),
                                 :tagger => tagger.to_s, :subject => subject.to_s.strip, :body => body.to_s.strip }
            end
        end
        list[:branches].sort_by! { |b| b[:name].downcase }
        list[:tags].sort_by! { |t| [-t[:time].to_i, t[:name]] }
        list
    end

    def self.branches(repo)
        refs(repo)[:branches].map { |b| b[:name] }
    end

    # The default branch: main if there is one, else the first.
    def self.default_branch(repo, refs = nil)
        list = (refs || self.refs(repo))[:branches].map { |b| b[:name] }
        list.include?('main') ? 'main' : list.first
    end

    # What the address names after /tree/, /blob/ or /commits/: the ref and
    # the path after it. A branch or tag may have a slash in its name; the
    # longest known one wins, branches before tags. Anything else (a commit's
    # hash) is the first part. [ref, path, :branch / :tag / nil]
    def self.split_ref(rest, refs)
        rest = rest.to_s
        known = refs[:branches].map { |b| [b[:name], :branch] } + refs[:tags].map { |t| [t[:name], :tag] }
        match = known.select { |name, _| rest == name || rest.start_with?("#{name}/") }.max_by { |name, kind| [name.length, kind == :branch ? 1 : 0] }
        return [match[0], rest[(match[0].length + 1)..].to_s, match[1]] if match

        ref, path = rest.split('/', 2)
        [ref.to_s, path.to_s, nil]
    end

    # The commit of a ref from split_ref: a branch's or tag's from the list,
    # anything else through git (resolve).
    def self.commit_of(repo, ref, kind, refs)
        case kind
        when :branch then refs[:branches].find { |b| b[:name] == ref }&.dig(:sha)
        when :tag then refs[:tags].find { |t| t[:name] == ref }&.dig(:sha)
        else resolve(repo, ref)
        end
    end

    # What is at path in a commit: :tree, :blob or nil.
    def self.kind(repo, sha, path)
        return :tree if path.empty?

        out = git(repo, 'ls-tree', '-z', sha, '--', path)
        return nil if out.nil? || out.empty?

        type = out.split("\t", 2).first.split(' ')[1]
        { 'tree' => :tree, 'blob' => :blob }[type]
    end

    # The entries of a folder: [{ name:, path:, type: :tree/:blob, size: }],
    # folders first, then by name.
    def self.tree(repo, sha, path)
        target = path.empty? ? sha : "#{sha}:#{path}"
        out = git(repo, 'ls-tree', '-z', '-l', '--end-of-options', target)
        return [] unless out

        out.split("\0").filter_map do |line|
            meta, name = line.split("\t", 2)
            _mode, type, _object, size = meta.split(' ')
            next unless %w[tree blob].include?(type)

            { :name => name, :path => path.empty? ? name : "#{path}/#{name}", :type => type.to_sym,
              :size => size == '-' ? nil : size.to_i }
        end.sort_by { |e| [e[:type] == :tree ? 0 : 1, e[:name].downcase] }
    end

    # Every folder of a commit with its entries, for the sidebar, in one
    # call: { '' => [{ name:, path:, type: }], 'questions' => [...] }, sorted
    # like tree; nil for more than TREE_MAX entries.
    def self.all_folders(repo, sha, max: TREE_MAX)
        result = run(repo, 'ls-tree', '-r', '-t', '-z', '--end-of-options', sha, :max => max * 300)
        return nil unless result[:ok] && !result[:cut]

        items = result[:out].split("\0")
        return nil if items.size > max

        folders = Hash.new { |hash, key| hash[key] = [] }
        folders['']
        items.each do |line|
            meta, path = line.split("\t", 2)
            type = meta.to_s.split(' ')[1]
            next unless path && %w[tree blob].include?(type)

            parent = path.include?('/') ? File.dirname(path) : ''
            folders[parent] << { :name => File.basename(path), :path => path, :type => type.to_sym }
            folders[path] if type == 'tree'
        end
        folders.each_value { |list| list.sort_by! { |e| [e[:type] == :tree ? 0 : 1, e[:name].downcase] } }
        folders.default_proc = nil
        folders
    end

    # The last commit that changed each entry of a folder, with one call
    # over at most LAST_COMMITS commits: { name => { sha:, short:, author:,
    # time:, subject: } }; entries changed only before them are missing.
    def self.last_commits(repo, sha, path)
        args = ['log', '-z', '--name-only', '--no-renames', "--format=%x1e%H%x1f%h%x1f%an%x1f%at%x1f%s",
                "--max-count=#{LAST_COMMITS}", '--end-of-options', sha]
        args += ['--', path] unless path.empty?
        result = run(repo, *args)
        return {} unless result[:ok]

        prefix = path.empty? ? '' : "#{path}/"
        found = {}
        result[:out].split("\x1e").each do |record|
            head, names = record.split("\0", 2)
            full, short, author, time, subject = head.to_s.split("\x1f", 5)
            next unless full && names

            commit = { :sha => full, :short => short, :author => author.to_s, :time => Time.at(time.to_i), :subject => subject.to_s }
            names.sub(/\A\n/, '').split("\0").each do |name|
                next unless name.start_with?(prefix)

                child = name.delete_prefix(prefix).split('/').first
                found[child] ||= commit if child
            end
        end
        found
    end

    # How many commits lead to this one (for the »Verlauf« tab).
    def self.count(repo, sha, path = '')
        args = ['rev-list', '--count', '--end-of-options', sha]
        args += ['--', path] unless path.empty?
        (git(repo, *args) || '').strip.to_i
    end

    # A file: { size:, binary:, text: (cut at MAX_BLOB), cut: }.
    def self.blob(repo, sha, path, max: MAX_BLOB)
        size = git(repo, 'cat-file', '-s', '--end-of-options', "#{sha}:#{path}")
        return nil unless size

        result = run(repo, 'cat-file', 'blob', '--end-of-options', "#{sha}:#{path}", :binary => true, :max => max)
        data = result[:ok] ? result[:out] : ''.b
        cut = size.to_i > max
        text = data.dup.force_encoding('UTF-8')
        # a character cut in two at the end of what was read is no sign of a binary file
        3.times { text = text.byteslice(0, text.bytesize - 1) if cut && !text.empty? && !text.valid_encoding? }
        binary = data[0, 8000].include?("\0") || !text.valid_encoding?
        text = nil if binary
        { :size => size.to_i, :binary => binary, :text => text, :cut => cut, :data => data }
    end

    # The history from a commit, optionally of one path:
    # [{ sha:, short:, author:, time:, subject: }], PAGE at a time.
    def self.log(repo, sha, path = '', skip: 0, max: PAGE)
        args = ['log', '-z', "--format=%H%x1f%h%x1f%an%x1f%at%x1f%s", "--skip=#{skip.to_i}", "--max-count=#{max.to_i}", '--end-of-options', sha]
        args += ['--', path] unless path.empty?
        (git(repo, *args) || '').split("\0").filter_map do |record|
            full, short, author, time, subject = record.split("\x1f", 5)
            next unless full

            { :sha => full, :short => short, :author => author.to_s, :time => Time.at(time.to_i), :subject => subject.to_s }
        end
    end

    # A commit: { sha:, short:, author:, time:, message:, parents:, files: [{ path:, old_path:, status:, patch: }], cut: }.
    def self.commit(repo, sha)
        head = git(repo, 'show', '-s', '--format=%H%x1f%h%x1f%an%x1f%at%x1f%P%x1f%B', '--end-of-options', sha)
        return nil unless head

        full, short, author, time, parents, message = head.split("\x1f", 6)
        result = run(repo, 'show', '--format=', '-M', '--no-color', '--no-ext-diff', '--end-of-options', sha, :max => MAX_DIFF)
        cut = result[:cut]
        diff = result[:ok] ? result[:out].scrub : ''
        { :sha => full, :short => short, :author => author.to_s, :time => Time.at(time.to_i),
          :parents => parents.to_s.split, :message => message.to_s.strip, :files => split_diff(diff), :cut => cut }
    end

    # A unified diff in one piece per file.
    def self.split_diff(diff)
        diff.split(/^(?=diff --git )/).filter_map do |chunk|
            next unless chunk.start_with?('diff --git ')

            header = chunk.lines.first
            m = header.match(%r{\Adiff --git a/(.*) b/(.*)\n?\z})
            path = m ? m[2] : header.strip
            old_path = m && m[1] != m[2] ? m[1] : nil
            status = if chunk.include?("\nnew file mode") then :added
                     elsif chunk.include?("\ndeleted file mode") then :deleted
                     elsif old_path then :renamed
                     else :modified
                     end
            body = chunk.lines.drop_while { |l| !l.start_with?('@@') && !l.start_with?('Binary files') }.join
            { :path => path, :old_path => old_path, :status => status, :patch => body }
        end
    end
end
