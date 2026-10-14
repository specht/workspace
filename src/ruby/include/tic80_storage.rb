require 'base64'
require 'digest'
require 'fileutils'
require 'set'
require_relative 'atomic_file'

# TIC-80 in the browser works on an in-memory filesystem; the real files are
# in the person's Workspace under /workspace/TIC-80, where code-server, Git
# and the task checks see them too (bridge: docker/tic80/hackschule-
# persistence.js).
#
# The browser sends each file it changed together with the version it
# started from (base, a SHA-256). A file is written only if it is still that
# version on disk. If something else changed it in the meantime (VS Code, cp,
# git pull), the file on disk stays as it is and the browser's version is
# saved next to it, »spiel (TIC-80).lua«. Nothing is refused and nothing is
# overwritten that the browser has not seen.
module Tic80Storage
    MAX_TOTAL_BYTES = 64 * 1024 * 1024
    CONFLICT_SUFFIX = 'TIC-80'

    class Invalid < StandardError; end

    def self.sha256(bytes)
        Digest::SHA256.hexdigest(bytes)
    end

    # A relative path below the root, or Invalid.
    def self.relative_path(path)
        raise Invalid, 'path must be a string' unless path.is_a?(String)
        raise Invalid, 'path contains a NUL byte' if path.include?("\0")
        raise Invalid, 'path is too long' if path.bytesize > 4096

        value = path.tr('\\', '/')
        raise Invalid, 'absolute paths are not allowed' if value.start_with?('/')

        parts = value.split('/').reject(&:empty?)
        raise Invalid, 'empty path is not allowed' if parts.empty?

        parts.each do |part|
            raise Invalid, 'path traversal is not allowed' if part == '.' || part == '..'
            raise Invalid, 'path component is too long' if part.bytesize > 255
        end

        parts.join('/')
    end

    # Refuses a path whose existing parts include a symlink: writing through
    # one could leave the Workspace.
    def self.check_no_symlink!(root, relative)
        current = root
        relative.split('/').each do |part|
            current = File.join(current, part)
            stat = File.lstat(current) rescue nil
            return if stat.nil?
            raise Invalid, "#{relative} goes through a symbolic link" if stat.symlink?
        end
    end

    # The root directory, created if missing. Leftovers of the earlier
    # whole-folder swap (TIC-80.sync-*, TIC-80.backup-*) are recovered or
    # removed.
    def self.prepare_root(root, uid:, gid:)
        parent = File.dirname(root)
        base = File.basename(root)
        raise Invalid, "#{root} is a symbolic link" if File.symlink?(root)
        raise Invalid, "#{root} is not a directory" if File.exist?(root) && !File.directory?(root)

        leftovers = Dir.glob(File.join(parent, "#{base}.{backup,sync}-*")).select do |path|
            File.directory?(path) && !File.symlink?(path)
        end
        unless File.directory?(root)
            newest = leftovers.max_by { |path| File.mtime(path) rescue Time.at(0) }
            if newest
                File.rename(newest, root)
                leftovers.delete(newest)
            else
                FileUtils.mkdir_p(root)
            end
            File.chown(uid, gid, root)
        end
        leftovers.each { |path| FileUtils.rm_rf(path) }
    end

    # All directories and files below root. Symbolic links and other special
    # entries are left out: TIC-80 does not see them and never writes them.
    # contents: true adds each file's bytes (Base64); only: limits that to
    # these paths.
    def self.tree(root, contents: false, only: nil)
        dirs = []
        files = []
        walk = lambda do |dir, prefix|
            Dir.children(dir).sort.each do |name|
                path = File.join(dir, name)
                relative = prefix.empty? ? name : "#{prefix}/#{name}"
                stat = File.lstat(path)
                if stat.directory?
                    dirs << relative
                    walk.call(path, relative)
                elsif stat.file?
                    next if name.start_with?('.') && name.end_with?('.tmp')

                    bytes = File.binread(path)
                    entry = {
                        :path => relative,
                        :size => bytes.bytesize,
                        :mtime_ms => (stat.mtime.to_f * 1000).round,
                        :sha256 => sha256(bytes),
                    }
                    entry[:contents] = Base64.strict_encode64(bytes) if contents && (only.nil? || only.include?(relative))
                    files << entry
                end
            end
        end
        walk.call(root, '')
        { :dirs => dirs, :files => files }
    end

    # »spiel.lua« -> »spiel (TIC-80).lua«, then »spiel (TIC-80 2).lua«, …
    def self.conflict_name(root, relative)
        dir = File.dirname(relative)
        ext = File.extname(relative)
        stem = File.basename(relative, ext)
        (1..).each do |i|
            name = "#{stem} (#{CONFLICT_SUFFIX}#{i > 1 ? " #{i}" : ''})#{ext}"
            candidate = dir == '.' ? name : "#{dir}/#{name}"
            return candidate unless File.exist?(File.join(root, candidate)) || File.symlink?(File.join(root, candidate))
        end
    end

    def self.mkdir_owned(root, relative, uid, gid)
        current = root
        relative.split('/').each do |part|
            current = File.join(current, part)
            stat = File.lstat(current) rescue nil
            if stat.nil?
                Dir.mkdir(current)
                File.chown(uid, gid, current)
            elsif !stat.directory?
                raise Invalid, "#{relative}: #{part} is a file, not a folder"
            end
        end
    end

    def self.write_file(root, relative, bytes, mtime_ms, uid, gid)
        parent = File.dirname(relative)
        mkdir_owned(root, parent, uid, gid) unless parent == '.'
        path = File.join(root, relative)
        new_file = !File.exist?(path)
        AtomicFile.write(path, bytes)
        File.chown(uid, gid, path) if new_file
        if mtime_ms.is_a?(Numeric)
            time = Time.at(mtime_ms.to_f / 1000.0)
            File.utime(time, time, path)
        end
    end

    # changes: [{ path:, base: sha|nil, contents: base64 } |
    #           { path:, base: sha, deleted: true } |
    #           { path:, dir: true } | { path:, dir: true, deleted: true }]
    # Returns { written: { path => sha }, deleted: [...], conflicts: [...] };
    # a conflict is { path:, saved_as: } (the browser's version is next to
    # the file) or { path:, kept: true } (deleted in the browser, changed on
    # disk: the file stays).
    def self.apply(root, changes, uid:, gid:)
        raise Invalid, 'changes must be a list' unless changes.is_a?(Array)

        decoded = changes.map do |change|
            raise Invalid, 'each change needs a path' unless change.is_a?(Hash)

            relative = relative_path(change[:path])
            check_no_symlink!(root, relative)
            entry = { :path => relative, :dir => change[:dir] == true, :deleted => change[:deleted] == true,
                      :base => change[:base], :mtime_ms => change[:mtime_ms] }
            if !entry[:dir] && !entry[:deleted]
                raise Invalid, "#{relative}: contents missing" unless change[:contents].is_a?(String)

                begin
                    entry[:bytes] = Base64.strict_decode64(change[:contents])
                rescue ArgumentError
                    raise Invalid, "#{relative}: contents are not Base64"
                end
            end
            unless entry[:base].nil? || entry[:base].to_s.match?(/\A[0-9a-f]{64}\z/)
                raise Invalid, "#{relative}: base must be a SHA-256"
            end
            entry
        end
        total = decoded.sum { |entry| entry[:bytes].to_s.bytesize }
        raise Invalid, 'too much at once' if total > MAX_TOTAL_BYTES

        result = { :written => {}, :deleted => [], :conflicts => [] }

        decoded.select { |e| e[:dir] && !e[:deleted] }.sort_by { |e| e[:path].count('/') }.each do |entry|
            mkdir_owned(root, entry[:path], uid, gid)
        end

        decoded.reject { |e| e[:dir] }.each do |entry|
            relative = entry[:path]
            path = File.join(root, relative)
            stat = File.lstat(path) rescue nil
            current = stat&.file? ? sha256(File.binread(path)) : nil
            occupied = stat && !stat.file?

            if entry[:deleted]
                if stat.nil?
                    result[:deleted] << relative
                elsif !occupied && current == entry[:base]
                    File.delete(path)
                    result[:deleted] << relative
                else
                    result[:conflicts] << { :path => relative, :kept => true }
                end
                next
            end

            wanted = sha256(entry[:bytes])
            if current == wanted
                result[:written][relative] = wanted
            elsif !occupied && current == entry[:base]
                write_file(root, relative, entry[:bytes], entry[:mtime_ms], uid, gid)
                result[:written][relative] = wanted
            else
                other = conflict_name(root, relative)
                write_file(root, other, entry[:bytes], entry[:mtime_ms], uid, gid)
                result[:written][other] = wanted
                result[:conflicts] << { :path => relative, :saved_as => other }
            end
        end

        # Deepest first, and only empty ones: a folder with files from
        # elsewhere stays.
        decoded.select { |e| e[:dir] && e[:deleted] }.sort_by { |e| -e[:path].count('/') }.each do |entry|
            path = File.join(root, entry[:path])
            begin
                Dir.rmdir(path) if File.directory?(path) && !File.symlink?(path)
            rescue Errno::ENOTEMPTY, Errno::EEXIST
                nil
            end
        end

        result
    end
end
