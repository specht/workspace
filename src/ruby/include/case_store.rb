require 'digest'
require 'fileutils'
require 'json'
require 'securerandom'

# Cases for the development of the Workspace (/faelle): a teacher whose
# student's solution looks right but is not accepted saves it from the class
# board (cases_page.rb), for the administration to look at later and to
# improve the check. A case is a folder under ROOT:
#
#   20261127-131500-a1b2c3/
#     case.json      what the check knew: the task, its version then and now,
#                    the stored result and answer, the teacher's note; no
#                    name or address of the student
#     files/…        a copy of what the check read in the student's Workspace
#                    (the folder of the checked file)
#     abgabe.tar.gz  the stored hand-in (a document, a Kata's code), if any
#
# Cases go after KEEP_DAYS (purge, from the server's five-minute loop), when
# an administrator deletes them, or with the student's account
# (delete_for): case.json holds owner, a hash of the address, shown nowhere.
# The student's files may themselves contain a name (a document); the case
# holds none of its own.
module CaseStore
    ROOT = '/internal/cases'
    KEEP_DAYS = 180
    ID = /\A\d{8}-\d{6}-[0-9a-f]{6}\z/
    # what is copied from a Workspace: regular files only, no symbolic links,
    # at most so many, so large, and in all
    MAX_FILES = 300
    MAX_FILE = 2 * 1024 * 1024
    MAX_TOTAL = 10 * 1024 * 1024
    MAX_DEPTH = 3

    # The files to copy for a task: for each checked file (a path below the
    # Workspace, one per language variant), its folder and what is below it,
    # down to MAX_DEPTH. { relative path => absolute path }, inside base only.
    def self.collect(base, relative_files)
        base = File.realpath(base) rescue (return {})
        found = {}
        total = 0
        relative_files.compact.uniq.each do |rel|
            dir = File.dirname(rel.to_s)
            next if dir.split('/').include?('..')

            start = File.join(base, dir)
            next unless File.directory?(start) && !File.symlink?(start)
            next unless (File.realpath(start) rescue '').start_with?(base)

            walk = lambda do |path, depth|
                (Dir.children(path) rescue []).sort.each do |name|
                    next if name.start_with?('.')

                    full = File.join(path, name)
                    next if File.symlink?(full)

                    if File.directory?(full)
                        walk.call(full, depth + 1) if depth < MAX_DEPTH
                    elsif File.file?(full)
                        size = File.size(full)
                        next if size > MAX_FILE || total + size > MAX_TOTAL || found.size >= MAX_FILES

                        key = full.delete_prefix(base + '/')
                        next if found.key?(key)

                        found[key] = full
                        total += size
                    end
                end
            end
            walk.call(start, 0)
        end
        found
    end

    # Saves a case; returns its id. files: { relative path => source path },
    # blobs: { file name => bytes } (the stored hand-in).
    def self.save(meta, files: {}, blobs: {}, root: ROOT, now: Time.now)
        id = "#{now.strftime('%Y%m%d-%H%M%S')}-#{SecureRandom.hex(3)}"
        dir = File.join(root, id)
        FileUtils.mkdir_p(File.join(dir, 'files'))
        files.each do |rel, source|
            target = File.join(dir, 'files', rel)
            FileUtils.mkdir_p(File.dirname(target))
            FileUtils.cp(source, target)
        end
        blobs.each { |name, bytes| File.binwrite(File.join(dir, File.basename(name)), bytes) }
        record = meta.merge('id' => id, 'saved_at' => now.to_i, 'files' => files.keys.sort, 'blobs' => blobs.keys.map { |n| File.basename(n) })
        File.write(File.join(dir, 'case.json'), JSON.pretty_generate(record))
        id
    end

    # Every case, the newest first: the contents of its case.json.
    def self.list(root: ROOT)
        Dir.glob(File.join(root, '*', 'case.json')).filter_map do |path|
            next unless File.basename(File.dirname(path)).match?(ID)

            JSON.parse(File.read(path)) rescue nil
        end.sort_by { |c| -c['saved_at'].to_i }
    end

    # The key of a student's cases: not the address, but enough to delete
    # them with the account.
    def self.owner(email)
        Digest::SHA256.hexdigest("case:#{email.to_s.downcase}")
    end

    # Deletes a student's cases (with the account); returns how many.
    def self.delete_for(email, root: ROOT)
        key = owner(email)
        list(:root => root).count { |c| c['owner'] == key && delete(c['id'], :root => root) }
    end

    def self.dir(id, root: ROOT)
        id.to_s.match?(ID) ? File.join(root, id) : nil
    end

    def self.delete(id, root: ROOT)
        d = dir(id, :root => root)
        return false unless d && File.directory?(d)

        FileUtils.rm_rf(d)
        true
    end

    # Deletes the cases older than KEEP_DAYS; returns how many.
    def self.purge(root: ROOT, now: Time.now)
        limit = now.to_i - KEEP_DAYS * 24 * 3600
        list(:root => root).count { |c| c['saved_at'].to_i < limit && delete(c['id'], :root => root) }
    end
end
