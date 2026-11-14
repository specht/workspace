require 'find'
require 'open3'
require 'tmpdir'
require 'yaml'
require_relative 'user_files'
require_relative 'test_workspace_extensions'
require 'fileutils'

# A Leistungsüberprüfung from a folder of the teacher's own Workspace, with
# no terminal and no upload (exam_builder_page.rb, the dialog in exams.js):
# the folders at the top of the Workspace, the files of one as a tree (what
# is hidden, a build folder or a link starts unticked, a link cannot be
# ticked at all), the chosen files packed with a .workspace/config.yaml from
# the dialog's settings. The package then goes the way of an uploaded one
# (the same checks). Plain Ruby, tested on its own (exam_builder_test.rb).
#
# The tree is read as root, names and sizes only, never following a link;
# the files are packed by tar as the Workspace user, so that a link swapped
# in after the tree was read reaches no more than the teacher could read in
# the Workspace (and a link itself is refused by the package check).
module ExamBuilder
    class Refused < StandardError; end

    # left out unless ticked: folders a build or a tool makes, files an
    # editor or a system leaves behind
    ARTIFACT_DIRS = %w[
        node_modules __pycache__ .venv venv env .dart_tool .gradle .idea .vscode-test
        .pytest_cache .mypy_cache .ruff_cache .tox build dist target out bin obj .next .cache
    ].freeze
    ARTIFACT_FILES = %w[.DS_Store Thumbs.db desktop.ini].freeze
    ARTIFACT_SUFFIXES = %w[.pyc .pyo .class .o .obj .exe .dll .so .dylib .swp].freeze
    MAX_ENTRIES = 3000
    MAX_FILES = 10_000
    MAX_BYTES = 100 * 1024 * 1024
    CONFIG = '.workspace/config.yaml'

    # the folders at the top of a Workspace, not hidden, not links
    def self.folders(root)
        return [] unless File.directory?(root)

        Dir.children(root).reject { |name| name.start_with?('.') }
            .select { |name| File.lstat(File.join(root, name)).directory? }
            .sort_by(&:downcase)
    end

    # the folder's path, if the name is one of them
    def self.folder_path(root, name)
        raise Refused, 'Diesen Ordner gibt es nicht.' unless folders(root).include?(name.to_s)

        File.join(root, name)
    end

    # Why a path starts unticked: nil (ticked), 'hidden', 'artifact', 'link'
    # (cannot be ticked), 'git' (a repository, at the top or in a subfolder:
    # ticked, the class works with its history, a Git task; the recovery
    # repository of every exam needs none)
    def self.reason(rel, stat)
        name = File.basename(rel)
        return 'link' if stat.symlink?
        return 'git' if name == '.git' && stat.directory?
        return 'artifact' if stat.directory? && ARTIFACT_DIRS.include?(name)
        return 'artifact' if !stat.directory? && (ARTIFACT_FILES.include?(name) || ARTIFACT_SUFFIXES.include?(File.extname(name).downcase))
        return 'hidden' if name.start_with?('.') && !KEEP_HIDDEN.include?(name)

        nil
    end

    # hidden files that belong to a package, ticked like the others
    KEEP_HIDDEN = %w[.gitignore .gitattributes .editorconfig].freeze

    # The tree, depth first, folders before files: {p: path, d: folder?,
    # s: size, r: reason}; a folder that starts unticked is not opened (it
    # goes in whole when ticked), .workspace is left out (the dialog writes
    # its config.yaml). truncated when there were more than MAX_ENTRIES.
    def self.tree(path)
        entries = []
        truncated = false
        walk = lambda do |dir, prefix|
            Dir.children(dir).sort_by { |n| [File.lstat(File.join(dir, n)).directory? ? 0 : 1, n.downcase] }.each do |name|
                rel = prefix.empty? ? name : "#{prefix}/#{name}"
                next if rel == '.workspace'

                if entries.size >= MAX_ENTRIES
                    truncated = true
                    return
                end
                stat = File.lstat(File.join(dir, name))
                why = reason(rel, stat)
                folder = stat.directory?
                entries << { :p => rel, :d => folder, :s => folder ? nil : stat.size, :r => why }.compact
                walk.call(File.join(dir, name), rel) if folder && why.nil?
            end
        end
        walk.call(path, '')
        { :entries => entries, :truncated => truncated }
    end

    # The settings of a config.yaml already in the folder, or {} (read
    # without following a link; a broken one counts as none)
    def self.existing_config(path)
        text = UserFiles.read(path, CONFIG)
        config = text && YAML.safe_load(text, :permitted_classes => [], :permitted_symbols => [], :aliases => false)
        config.is_a?(Hash) ? config : {}
    rescue UserFiles::Refused, Psych::Exception, SystemCallError
        {}
    end

    def self.safe_rel?(rel)
        rel.is_a?(String) && !rel.empty? && !rel.start_with?('/') && !rel.include?("\0") &&
            rel.split('/').none? { |c| c.empty? || c == '.' || c == '..' } && rel.split('/').first != '.workspace'
    end

    # The files to pack: each chosen path checked (no link on the way), a
    # chosen folder with everything in it but links; [files, bytes]
    def self.expand(path, chosen)
        files = []
        bytes = 0
        add = lambda do |rel, stat|
            files << rel
            bytes += stat.size
            raise Refused, "Mehr als #{MAX_FILES} Dateien; lass Build-Ordner und Ähnliches weg." if files.size > MAX_FILES
            raise Refused, "Zusammen mehr als #{MAX_BYTES / 1024 / 1024} MB; lass große Dateien weg." if bytes > MAX_BYTES
        end
        Array(chosen).uniq.each do |rel|
            raise Refused, "Unzulässiger Pfad: #{rel.inspect}" unless safe_rel?(rel)

            parts = rel.split('/')
            parts.each_index do |i|
                stat = File.lstat(File.join(path, *parts[0..i]))
                raise Refused, "#{rel} ist ein Link; Links gehen nicht in ein Paket." if stat.symlink?
            rescue Errno::ENOENT
                raise Refused, "#{rel} gibt es nicht mehr."
            end
            full = File.join(path, rel)
            stat = File.lstat(full)
            if stat.file?
                add.call(rel, stat)
            elsif stat.directory?
                Find.find(full) do |inner|
                    s = File.lstat(inner)
                    next Find.prune if s.symlink?
                    add.call(inner.delete_prefix("#{path}/"), s) if s.file?
                end
            end
        end
        [files.uniq.sort, bytes]
    end

    # Whether a print pattern matches a path (as TestWorkspacePackage does:
    # a final /** takes the whole tree)
    def self.matches?(pattern, rel)
        flags = File::FNM_PATHNAME | File::FNM_DOTMATCH | File::FNM_EXTGLOB
        File.fnmatch?(pattern, rel, flags) ||
            (pattern.end_with?('/**') && (File.fnmatch?(pattern.delete_suffix('/**'), rel, flags) ||
                                          File.fnmatch?("#{pattern}/*", rel, flags)))
    end

    # The print patterns of the package: the paths the teacher left out of
    # the printout (a folder with everything in it), and the patterns of a
    # config.yaml there before that match none of the folder's paths (meant
    # for files the class will write); those that did are the teacher's
    # choice now
    def self.print_patterns(path, rels, before: [], listed: [])
        chosen = rels.map do |rel|
            File.lstat(File.join(path, rel)).directory? ? "#{glob_escape(rel)}/**" : glob_escape(rel)
        rescue SystemCallError
            nil
        end.compact
        kept = before.select { |pattern| pattern.is_a?(String) && listed.none? { |rel| matches?(pattern, rel) } }
        (kept + chosen).uniq
    end

    # The config.yaml of the package: what was there (extensions, setup,
    # VS Code settings stay), the dialog's colour, look, Git and printing
    def self.config_yaml(existing, color:, look:, git_mode:, print_exclude:, extensions: [])
        config = existing.dup
        named = Array(existing['extensions'])
        have = named.map { |e| (e.is_a?(Hash) ? e['id'] : e).to_s.downcase }
        more = extensions.reject { |id| have.include?(id.downcase) }
        config['extensions'] = named + more unless more.empty?
        config['workspace_package'] = 1
        config['exam'] = (existing['exam'].is_a?(Hash) ? existing['exam'] : {}).merge('color' => color, 'look' => look)
        config['git'] = (existing['git'].is_a?(Hash) ? existing['git'] : {}).merge('mode' => git_mode)
        print = existing['print'].is_a?(Hash) ? existing['print'].dup : {}
        print.delete('exclude')
        print['exclude'] = print_exclude unless print_exclude.empty?
        print.empty? ? config.delete('print') : config['print'] = print
        config.to_yaml
    end

    # a path as a print pattern that matches only itself
    def self.glob_escape(rel)
        rel.gsub(/[\\*?\[\]{}]/) { |c| "\\#{c}" }
    end

    # The package as gzip bytes: the files by tar as uid/gid (nil: as now,
    # for the tests), the config.yaml added after
    def self.pack(path, files, config_yaml, uid: nil, gid: nil)
        Dir.mktmpdir('exam-build-') do |dir|
            list = File.join(dir, 'files')
            File.binwrite(list, files.map { |f| "#{f}\0" }.join)
            archive = File.join(dir, 'package.tar')
            File.chown(uid, gid, dir, list) if uid
            as = uid ? ['setpriv', "--reuid=#{uid}", "--regid=#{gid}", '--clear-groups'] : []
            run!(*as, 'tar', '-C', path, '--no-recursion', '--null', '-T', list, '-cf', archive)
            stage = File.join(dir, 'stage')
            FileUtils.mkdir_p(File.join(stage, '.workspace'))
            File.write(File.join(stage, CONFIG), config_yaml)
            run!('tar', '-C', stage, '--no-recursion', '-rf', archive, '.workspace', CONFIG)
            run!('gzip', '-n', archive)
            File.binread("#{archive}.gz")
        end
    end

    # ---- a folder or an archive dropped into the dialog

    # Kept per teacher (by the folder of their Workspace) until the exam is
    # made, at most a day; an id names one
    DRAFTS = '/internal/exam-drafts'
    DRAFT_ID = /\A[a-z0-9]{24}\z/
    UNZIP = File.expand_path('../tools/unzip_safe.py', __dir__)
    # what a Mac puts into a ZIP beside the folder
    LITTER = %w[__MACOSX .DS_Store Thumbs.db].freeze

    def self.draft_path(root, owner, id)
        raise Refused, 'Diese Auswahl gibt es nicht mehr; zieh den Ordner noch einmal hierher.' unless id.to_s.match?(DRAFT_ID)

        path = File.join(root, owner, id, 'files')
        raise Refused, 'Diese Auswahl gibt es nicht mehr; zieh den Ordner noch einmal hierher.' unless File.directory?(path)

        path
    end

    # drafts older than a day gone (with each new draft, and hourly:
    # Main's background thread)
    def self.expire_drafts(root, now: Time.now)
        Dir.glob(File.join(root, '*', '*')).each do |d|
            FileUtils.rm_rf(d) if File.directory?(d) && now - File.mtime(d) > 24 * 3600
        end
    end

    # a fresh folder for a draft, older ones of anyone gone
    def self.new_draft(root, owner, id, now: Time.now)
        expire_drafts(root, :now => now)
        path = File.join(root, owner, id, 'files')
        FileUtils.mkdir_p(path)
        path
    end

    # The files of a folder dropped or chosen in the browser, in batches
    # (a request takes at most 128 files): [relative path, file to copy]
    # each, the folder's own name already taken off by the browser
    def self.stage_files(path, files)
        raise Refused, 'Keine Datei angekommen.' if files.empty?

        files.each do |rel, source|
            rel = rel.to_s.tr('\\', '/').sub(%r{\A/+}, '')
            next if rel.split('/').any? { |c| LITTER.include?(c) }
            raise Refused, "Unzulässiger Pfad: #{rel.inspect}" unless !rel.empty? && rel.split('/').none? { |c| c.empty? || c == '.' || c == '..' }

            target = File.join(path, rel)
            FileUtils.mkdir_p(File.dirname(target))
            FileUtils.cp(source, target)
        end
        count = Dir.glob(File.join(path, '**', '*'), File::FNM_DOTMATCH).count { |f| File.file?(f) }
        raise Refused, "Mehr als #{MAX_FILES} Dateien." if count > MAX_FILES
    end

    # Extensions of VS Code the dialog offers when files of their language
    # are in the package: an exam Workspace has no internet and only the
    # extensions its package names (docker/code: none for a language).
    # file ending => [id, name, what it does]
    LANGUAGE_EXTENSIONS = {
        '.dart' => ['Dart-Code.dart-code', 'Dart', 'hebt Dart hervor, zeigt Fehler schon beim Tippen und formatiert den Code'],
    }.freeze

    # what to offer for these paths: [{id, name, why, included}], included:
    # a config.yaml there names it already
    def self.offers(paths, existing)
        named = Array(existing['extensions']).map { |e| (e.is_a?(Hash) ? e['id'] : e).to_s.downcase }
        LANGUAGE_EXTENSIONS.select { |ending, _| paths.any? { |p| p.downcase.end_with?(ending) } }.map do |_, (id, name, why)|
            { :id => id, :name => name, :why => why, :included => named.include?(id.downcase) }
        end
    end

    # An archive (.zip, .tar.gz, .tgz, .tar.bz2, .tar) unpacked, after the
    # same checks as an uploaded package (no links, no path out of the
    # folder); a single folder at its top (a ZIP of a folder) becomes the
    # draft itself. The name: that folder's, or the archive's without its
    # ending.
    def self.stage_archive(path, archive, filename)
        base = File.basename(filename.to_s)
        if base.match?(/\.zip\z/i)
            out, status = Open3.capture2e('python3', '-I', UNZIP, archive, path)
            raise Refused, out.strip.lines.first.to_s.strip unless status.success?
        elsif base.match?(/\.(tar\.gz|tgz|tar\.bz2|tbz2?|tar)\z/i)
            TestWorkspaceExtensions.validate_archive!(archive)
            run!('tar', '-xf', archive, '-C', path, '--no-same-owner', '--no-same-permissions')
        else
            raise Refused, 'Das ist kein Archiv, das der Workspace kennt: ZIP oder .tar.gz.'
        end
        LITTER.each { |l| FileUtils.rm_rf(File.join(path, l)) }
        inner = Dir.children(path)
        if inner.size == 1 && File.lstat(File.join(path, inner.first)).directory? && inner.first != '.workspace'
            top = File.join(path, inner.first)
            Dir.children(top).each { |c| FileUtils.mv(File.join(top, c), File.join(path, c)) }
            FileUtils.rmdir(top)
            return inner.first
        end
        base.sub(/\.(zip|tar\.gz|tgz|tar\.bz2|tbz2?|tar)\z/i, '')
    end

    def self.run!(*command)
        out, status = Open3.capture2e(*command)
        raise Refused, "Packen ging nicht: #{out.lines.first.to_s.strip}" unless status.success?

        out
    end
end
