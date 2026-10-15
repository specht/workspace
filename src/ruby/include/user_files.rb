require 'fcntl'

# Files that this process, running as root, writes into or reads from a
# student's folder (/user/<tag>/workspace). The student owns everything below
# that folder and can put a symbolic link, a FIFO or a device node anywhere
# in it, also while we are working, so:
#
# - the folder itself (root) was made by us; everything below it is walked one
#   name at a time, each opened relative to the folder opened before
#   (/proc/self/fd/<fd>/<name>, as openat would) with O_NOFOLLOW, so a link
#   anywhere on the way is refused instead of followed;
# - only directories and regular files are accepted (O_NONBLOCK, so a FIFO
#   cannot make us wait);
# - a file is truncated only after it was opened and checked, and new files
#   and folders are handed to the student's user through the open
#   descriptor (fchown), never by path.
#
# Every method raises UserFiles::Refused when the path is not what it should
# be; callers log it and go on, the student's Workspace still starts.
module UserFiles
    class Refused < StandardError; end

    UID = 1000
    GID = 1000
    MAX_READ = 1024 * 1024

    # Creates the folders of rel below root that do not exist yet, owned by
    # the student, and returns nothing. rel is relative ('a/b/c').
    def self.mkdir_p(root, rel)
        with_dir(root, rel, create: true) { }
        nil
    end

    # Writes contents to root/rel. mode: :replace (the default) or :append;
    # create_only: true leaves an existing file alone and returns false;
    # mtime: a Time for the file afterwards. Missing folders on the way are
    # created. Returns true when written.
    def self.write(root, rel, contents, mode: :replace, create_only: false, mtime: nil)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: true) do |dir|
            flags = File::WRONLY | File::CREAT | File::NOFOLLOW | File::NONBLOCK
            flags |= File::EXCL if create_only
            flags |= File::APPEND if mode == :append
            begin
                File.open(child(dir, name), flags, 0o644) do |f|
                    require_regular!(f, rel)
                    f.truncate(0) if mode == :replace
                    f.write(contents)
                    f.chown(UID, GID)
                    if mtime
                        f.flush
                        File.utime(mtime, mtime, "/proc/self/fd/#{f.fileno}")
                    end
                end
            rescue Errno::EEXIST
                return false
            rescue Errno::ELOOP, Errno::ENXIO, Errno::EISDIR
                raise Refused, "#{rel} is not a regular file"
            end
            true
        end
    end

    # The contents of root/rel, or nil when it does not exist. Refuses links
    # and anything but a regular file; reads at most max bytes.
    def self.read(root, rel, max: MAX_READ)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            return nil if dir.nil?

            begin
                File.open(child(dir, name), File::RDONLY | File::NOFOLLOW | File::NONBLOCK) do |f|
                    require_regular!(f, rel)
                    data = f.read(max + 1) || ''
                    raise Refused, "#{rel} is larger than #{max} bytes" if data.bytesize > max

                    data.force_encoding(Encoding::UTF_8)
                end
            rescue Errno::ENOENT
                nil
            rescue Errno::ELOOP, Errno::ENXIO, Errno::EISDIR
                raise Refused, "#{rel} is not a regular file"
            end
        end
    end

    def self.exist?(root, rel)
        !read(root, rel, max: MAX_READ).nil?
    rescue Refused
        true
    end

    # :file, :directory, :other (a link, a FIFO, …) or nil for root/rel.
    def self.kind(root, rel)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            return nil if dir.nil?

            stat = begin
                File.lstat(child(dir, name))
            rescue Errno::ENOENT
                return nil
            end
            return :file if stat.file?
            return :directory if stat.directory?

            :other
        end
    end

    # Removes the file or link root/rel; true when something was removed.
    def self.delete(root, rel)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            return false if dir.nil?

            File.delete(child(dir, name))
            true
        end
    rescue Errno::ENOENT, Errno::EISDIR, Errno::EPERM
        false
    end

    # Removes the empty folder root/rel; false when it is missing, not empty
    # or not a folder.
    def self.rmdir(root, rel)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            return false if dir.nil?

            Dir.rmdir(child(dir, name))
            true
        end
    rescue Errno::ENOENT, Errno::ENOTEMPTY, Errno::EEXIST, Errno::ENOTDIR
        false
    end

    # Removes root/rel with everything in it. Folders are emptied through
    # the folder that was opened, so a folder swapped for a link meanwhile
    # is never followed.
    def self.rm_r(root, rel)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            remove_entry(dir, name) if dir
        end
        nil
    end

    # Every folder and regular file below root/rel, depth first and sorted
    # by name: yields (relative path, File::Stat, open File) for files and
    # (relative path, File::Stat, nil) for folders. Links, FIFOs and other
    # entries are left out.
    def self.walk(root, rel = '', &block)
        with_dir(root, rel, create: false) do |dir|
            walk_dir(dir, '', &block) if dir
        end
        nil
    end

    # Hands root/rel (a file or folder) to the student, if it is one; a link
    # or a missing path is left alone.
    def self.chown(root, rel)
        dir_rel, name = split(rel)
        with_dir(root, dir_rel, create: false) do |dir|
            return if dir.nil?

            File.open(child(dir, name), File::RDONLY | File::NOFOLLOW | File::NONBLOCK) do |f|
                stat = f.stat
                f.chown(UID, GID) if stat.file? || stat.directory?
            end
        end
    rescue Errno::ENOENT, Errno::ELOOP, Errno::ENXIO
        nil
    end

    # Opens root, then every folder of rel below it, without following links,
    # and yields the last one as an open File (nil when create is false and a
    # folder is missing). Folders it creates belong to the student.
    def self.with_dir(root, rel, create:)
        begin
            dir = File.open(root, File::RDONLY | File::NOFOLLOW | File::NONBLOCK)
        rescue Errno::ELOOP, Errno::ENXIO
            raise Refused, "#{root} is not a folder"
        rescue Errno::ENOENT
            raise if create

            return yield(nil)
        end
        raise Refused, "#{root} is not a folder" unless dir.stat.directory?

        parts(rel).each do |name|
            path = child(dir, name)
            created = false
            if create
                begin
                    Dir.mkdir(path, 0o755)
                    created = true
                rescue Errno::EEXIST
                end
            end
            begin
                inner = File.open(path, File::RDONLY | File::NOFOLLOW | File::NONBLOCK)
            rescue Errno::ENOENT
                raise if create

                return yield(nil)
            rescue Errno::ELOOP, Errno::ENXIO
                raise Refused, "#{rel}: #{name} is not a folder"
            end
            unless inner.stat.directory?
                inner.close
                raise Refused, "#{rel}: #{name} is not a folder"
            end
            inner.chown(UID, GID) if created
            dir.close
            dir = inner
        end
        yield dir
    ensure
        dir&.close unless dir&.closed?
    end

    def self.walk_dir(dir, prefix, &block)
        Dir.children("/proc/self/fd/#{dir.fileno}").sort.each do |name|
            relative = prefix.empty? ? name : "#{prefix}/#{name}"
            begin
                f = File.open(child(dir, name), File::RDONLY | File::NOFOLLOW | File::NONBLOCK)
            rescue Errno::ELOOP, Errno::ENXIO, Errno::ENOENT, Errno::EACCES
                next
            end
            begin
                stat = f.stat
                if stat.directory?
                    yield relative, stat, nil
                    walk_dir(f, relative, &block)
                elsif stat.file?
                    require_regular!(f, relative)
                    yield relative, stat, f
                end
            ensure
                f.close
            end
        end
    end

    def self.remove_entry(dir, name)
        path = child(dir, name)
        stat = begin
            File.lstat(path)
        rescue Errno::ENOENT
            return
        end
        unless stat.directory?
            File.delete(path) rescue nil
            return
        end
        begin
            inner = File.open(path, File::RDONLY | File::NOFOLLOW | File::NONBLOCK)
        rescue Errno::ELOOP, Errno::ENOENT, Errno::ENXIO
            return
        end
        begin
            return unless inner.stat.directory?

            Dir.children("/proc/self/fd/#{inner.fileno}").each { |entry| remove_entry(inner, entry) }
        ensure
            inner.close
        end
        Dir.rmdir(path) rescue nil
    end

    def self.child(dir, name)
        "/proc/self/fd/#{dir.fileno}/#{name}"
    end

    def self.require_regular!(f, rel)
        raise Refused, "#{rel} is not a regular file" unless f.stat.file?

        # Back to blocking mode for the actual read or write.
        f.fcntl(Fcntl::F_SETFL, f.fcntl(Fcntl::F_GETFL) & ~File::NONBLOCK)
    end

    def self.split(rel)
        names = parts(rel)
        raise Refused, "empty path" if names.empty?

        [names[0..-2].join('/'), names.last]
    end

    def self.parts(rel)
        names = rel.to_s.split('/').reject(&:empty?)
        if names.any? { |name| name == '.' || name == '..' }
            raise Refused, "#{rel} contains . or .."
        end
        names
    end
end
