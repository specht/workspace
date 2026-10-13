# Read-only access to one student's Workspace folder on the server
# (/user/<fs_tag>/workspace), for the file checks of docs/aufgaben.md.
#
# Students control everything in that folder, including symbolic links. A
# link could point at another student's files or at the server itself, so:
#
# - no path may be absolute or contain ..
# - no component of a path may be a symbolic link (checked with lstat)
# - files and folders are opened with O_NOFOLLOW, and the server then asks
#   the kernel which file it really opened (/proc/self/fd) and refuses it
#   unless it lies inside the Workspace. That closes the gap between
#   checking a path and opening it.
#
# Nothing here writes, executes or decodes a file.
class WorkspaceFiles
    MAX_READ = 2 * 1024 * 1024
    MAX_WALK_FILES = 5000

    class Refused < StandardError
        attr_reader :problem, :path

        def initialize(problem, path, message)
            @problem = problem
            @path = path
            super(message)
        end
    end

    attr_reader :root

    def initialize(root)
        @root = File.realpath(root)
    end

    # Checks a relative path and returns it normalized ('' is the root).
    def self.clean(rel)
        rel = rel.to_s.strip.sub(%r{\A\./}, '').sub(%r{/+\z}, '')
        return '' if rel.empty? || rel == '.'
        if rel.start_with?('/') || rel.start_with?('~') || rel.split('/').any? { |c| c == '..' || c.empty? } || rel.include?("\0")
            raise Refused.new(:bad_path, rel, "#{rel} ist kein Pfad innerhalb des Workspaces.")
        end
        rel
    end

    # What is at a path, without following links:
    # { type: :file | :dir | :missing | :symlink | :other, size:, mode:, path: }
    # :symlink means the path or one of its folders is a link.
    def entry(rel)
        rel = WorkspaceFiles.clean(rel)
        current = @root
        components = rel.empty? ? [] : rel.split('/')
        components.each_with_index do |component, i|
            current = File.join(current, component)
            stat = begin
                File.lstat(current)
            rescue Errno::ENOENT, Errno::ENOTDIR
                return { :type => :missing, :path => rel }
            end
            if stat.symlink?
                return { :type => :symlink, :path => rel, :link => components[0..i].join('/') }
            end
            if i < components.size - 1 && !stat.directory?
                return { :type => :missing, :path => rel }
            end
        end
        stat = File.lstat(current)
        type = if stat.file? then :file elsif stat.directory? then :dir else :other end
        { :type => type, :size => stat.size, :mode => stat.mode, :path => rel }
    end

    def exists?(rel)
        %i[file dir].include?(entry(rel)[:type])
    end

    # Contents of a file as a binary String (at most max bytes; longer files
    # raise Refused with :too_big).
    def read(rel, max: MAX_READ)
        e = require_type(rel, :file)
        if e[:size] > max
            raise Refused.new(:too_big, e[:path], "#{e[:path]} ist größer als #{max / 1024} kB.")
        end
        open_verified(e[:path]) { |f| f.read(max + 1).to_s }
    end

    # Text of a file, as UTF-8 with invalid bytes replaced.
    def read_text(rel, max: MAX_READ)
        read(rel, max: max).force_encoding('UTF-8').scrub("�")
    end

    # The first bytes of a file (to tell its format by its content).
    def head(rel, bytes = 64)
        e = require_type(rel, :file)
        open_verified(e[:path]) { |f| f.read(bytes).to_s }
    end

    # Entries of a folder, sorted by name, without following links:
    # [{ name:, type:, size:, mode: }]
    def list(rel = '')
        e = require_type(rel, :dir)
        dir_path = e[:path].empty? ? @root : File.join(@root, e[:path])
        names = Dir.open(dir_path) do |dir|
            verify_fd!(dir.fileno, e[:path])
            dir.children
        end
        names.sort.map do |name|
            stat = File.lstat(File.join(dir_path, name))
            type = if stat.symlink? then :symlink elsif stat.file? then :file elsif stat.directory? then :dir else :other end
            { :name => name, :type => type, :size => stat.size, :mode => stat.mode }
        rescue Errno::ENOENT
            nil
        end.compact
    end

    # The real spelling of a path that exists only with other upper and lower
    # case letters (fuji.JPG for fuji.jpg), or nil.
    def other_case(rel)
        rel = WorkspaceFiles.clean(rel)
        return nil if rel.empty? || exists?(rel)

        found = []
        rel.split('/').each do |component|
            parent = found.join('/')
            entries = begin
                list(parent)
            rescue Refused
                return nil
            end
            match = entries.find { |x| x[:name] == component } ||
                entries.find { |x| x[:name].downcase == component.downcase }
            return nil if match.nil? || match[:type] == :symlink
            found << match[:name]
        end
        result = found.join('/')
        result == rel ? nil : result
    end

    # Every regular file below a folder as relative paths, without following
    # links. exclude lists relative paths (files or folders) to leave out.
    # Links are reported in skipped.
    def walk(rel, exclude: [])
        start = require_type(rel, :dir)[:path]
        exclude = exclude.map { |x| WorkspaceFiles.clean(x) }
        files = []
        skipped = []
        queue = [start]
        until queue.empty?
            dir = queue.shift
            list(dir).each do |x|
                path = dir.empty? ? x[:name] : "#{dir}/#{x[:name]}"
                next if exclude.any? { |ex| path == ex || path.start_with?("#{ex}/") }
                case x[:type]
                when :dir then queue << path
                when :file then files << { :path => path, :size => x[:size] }
                else skipped << path
                end
                if files.size > MAX_WALK_FILES
                    raise Refused.new(:too_many_files, start, "In #{start} liegen mehr als #{MAX_WALK_FILES} Dateien.")
                end
            end
        end
        [files.sort_by { |f| f[:path] }, skipped.sort]
    end

    # Opens a file for reading after the same checks as read (for copying).
    def open_file(rel, &block)
        e = require_type(rel, :file)
        open_verified(e[:path], &block)
    end

    private

    def require_type(rel, type)
        e = entry(rel)
        name = e[:path].empty? ? 'Der Workspace-Ordner' : e[:path]
        case e[:type]
        when type then e
        when :missing
            raise Refused.new(:missing, e[:path], "#{name} gibt es nicht.")
        when :symlink
            raise Refused.new(:symlink, e[:path], "#{e[:link]} ist ein symbolischer Link. Links liest der Workspace nicht.")
        else
            what = type == :dir ? 'kein Ordner' : 'keine Datei'
            raise Refused.new(:wrong_type, e[:path], "#{name} ist #{what}.")
        end
    end

    def open_verified(rel)
        path = File.join(@root, rel)
        File.open(path, File::RDONLY | File::NOFOLLOW | File::NONBLOCK) do |f|
            verify_fd!(f.fileno, rel)
            raise Refused.new(:wrong_type, rel, "#{rel} ist keine Datei.") unless f.stat.file?
            f.binmode
            yield f
        end
    rescue Errno::ELOOP
        raise Refused.new(:symlink, rel, "#{rel} ist ein symbolischer Link. Links liest der Workspace nicht.")
    rescue Errno::ENOENT
        raise Refused.new(:missing, rel, "#{rel} gibt es nicht.")
    end

    # The kernel's answer to »which file did I just open?« must lie in the
    # Workspace, whatever happened to the path in between.
    def verify_fd!(fd, rel)
        real = File.readlink("/proc/self/fd/#{fd}")
        return if real == @root || real.start_with?("#{@root}/")

        raise Refused.new(:outside, rel, "#{rel} liegt nicht im Workspace.")
    end
end
