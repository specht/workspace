require 'open3'
require 'time'

# Reading a group's repository for the pages under /git/<git id>/<name>
# (GitServer): branches, the files of a commit, a file, the history and a
# commit with its diff. Read-only, with git's plumbing; a ref or a path
# from the address is checked before git sees it, and git gets it after
# --end-of-options or --, never through a shell. Author names only, no
# e-mail addresses. Tested in test/git_browse_test.rb.
module GitBrowse
    MAX_BLOB = 512 * 1024
    MAX_DIFF = 300 * 1024
    PAGE = 50
    REF = %r{\A(?!-)(?!.*\.\.)[A-Za-z0-9._/-]{1,200}\z}
    SHA = /\A[0-9a-f]{7,64}\z/

    # git with these arguments in the repository; nil if it fails.
    def self.git(repo, *args, binary: false)
        out, _err, status = Open3.capture3('git', '-c', 'safe.directory=*', '--git-dir', repo, *args, :binmode => binary)
        status.success? ? out : nil
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

    def self.branches(repo)
        (git(repo, 'for-each-ref', '--format=%(refname:short)', 'refs/heads') || '').lines.map(&:strip).reject(&:empty?)
    end

    # The default branch: main if there is one, else the first.
    def self.default_branch(repo)
        list = branches(repo)
        list.include?('main') ? 'main' : list.first
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

    # A file: { size:, binary:, text: (cut at MAX_BLOB), cut: }.
    def self.blob(repo, sha, path)
        size = git(repo, 'cat-file', '-s', '--end-of-options', "#{sha}:#{path}")
        return nil unless size

        data = git(repo, 'cat-file', 'blob', '--end-of-options', "#{sha}:#{path}", :binary => true) || ''.b
        binary = data[0, 8000].include?("\0") || !data.dup.force_encoding('UTF-8').valid_encoding?
        cut = data.bytesize > MAX_BLOB
        text = binary ? nil : data.byteslice(0, MAX_BLOB).force_encoding('UTF-8').scrub
        { :size => size.to_i, :binary => binary, :text => text, :cut => cut, :data => data }
    end

    # The history from a commit, optionally of one path:
    # [{ sha:, short:, author:, time:, subject: }], PAGE at a time.
    def self.log(repo, sha, path = '', skip: 0, max: PAGE)
        args = ['log', '-z', "--format=%H%x1f%h%x1f%an%x1f%at%x1f%s", "--skip=#{skip.to_i}", "--max-count=#{max.to_i}", sha]
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
        diff = git(repo, 'show', '--format=', '-M', '--no-color', '--no-ext-diff', '--end-of-options', sha) || ''
        cut = diff.bytesize > MAX_DIFF
        diff = diff.byteslice(0, MAX_DIFF).force_encoding('UTF-8').scrub
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
