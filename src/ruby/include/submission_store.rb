require 'digest'
require 'fileutils'
require 'rubygems/package'
require 'set'
require 'stringio'
require 'zlib'
require_relative 'workspace_files'
require_relative 'html_lite'
require_relative 'task_spec'

# Check type submit (docs/aufgaben.md): the server copies the named files
# from the student's Workspace into a .tar.gz that only the student and
# their teachers can fetch. Repositories are never touched; a new
# submission replaces the old one, and the student can delete it.
module SubmissionStore
    MAX_FILES = 2000
    DEFAULT_MAX = 20_000_000

    Plan = Struct.new(:files, :total, :skipped, :notes, keyword_init: true)

    class Refused < StandardError
        attr_reader :problem

        def initialize(problem, message)
            @problem = problem
            super(message)
        end
    end

    def self.code(text)
        "`#{text.to_s.gsub('`', "'")}`"
    end

    # Files a pick: pattern offers (e.g. anaglyph/*.ini), as relative paths.
    def self.candidates(ws, pattern)
        dir = File.dirname(pattern)
        dir = '' if dir == '.'
        glob = File.basename(pattern)
        return [] unless ws.entry(dir)[:type] == :dir

        ws.list(dir).select { |e| e[:type] == :file && File.fnmatch(glob, e[:name], File::FNM_PATHNAME) }
            .map { |e| dir.empty? ? e[:name] : "#{dir}/#{e[:name]}" }
    end

    # Which files a submission would copy. picked is the student's choice for
    # pick: checks. Raises Refused with a message for the student.
    def self.plan(ws, check, picked: nil)
        max = check['max_size'] ? TaskSpec.parse_size(check['max_size']) : DEFAULT_MAX
        files = []
        skipped = []
        if check['pick']
            options = candidates(ws, check['pick'])
            if options.empty?
                raise Refused.new(:nothing, "Ich finde keine Datei, die zu #{code(check['pick'])} passt.")
            end
            unless options.include?(picked.to_s)
                raise Refused.new(:nothing, 'Wähle zuerst die Datei aus, die du abgeben möchtest.')
            end
            e = ws.entry(picked)
            files << { :path => picked, :size => e[:size] }
        else
            (check['paths'] || []).each do |path|
                e = ws.entry(path)
                case e[:type]
                when :file then files << { :path => path, :size => e[:size] }
                when :dir
                    found, links = ws.walk(path, :exclude => check['exclude'] || [])
                    files.concat(found)
                    skipped.concat(links)
                when :missing
                    raise Refused.new(:missing, "#{code(path)} gibt es nicht. Liegt dein Projekt woanders?")
                when :symlink
                    raise Refused.new(:symlink, "#{code(e[:link])} ist ein symbolischer Link. Links gibt der Workspace nicht ab.")
                else
                    raise Refused.new(:missing, "#{code(path)} ist keine Datei und kein Ordner.")
                end
            end
        end
        if files.empty?
            raise Refused.new(:nothing, 'Da ist nichts, was ich abgeben könnte: Die genannten Ordner sind leer.')
        end
        if files.size > MAX_FILES
            raise Refused.new(:too_many_files, "Das sind #{files.size} Dateien, höchstens #{MAX_FILES} gehen in eine Abgabe.")
        end
        total = files.sum { |f| f[:size] }
        if total > max
            biggest = files.max_by(3) { |f| f[:size] }.map { |f| "#{code(f[:path])} (#{size_text(f[:size])})" }
            raise Refused.new(:too_big, "Zusammen sind das #{size_text(total)}; erlaubt sind höchstens #{size_text(max)}. Am größten: #{biggest.join(', ')}.")
        end
        notes = []
        requires = check['requires'] || {}
        if requires['html_parses']
            files.select { |f| f[:path] =~ /\.html?\z/i }.each do |f|
                problems = HtmlLite.parse(ws.read_text(f[:path])).problems
                next if problems.empty?

                p = problems.first
                raise Refused.new(:parse_error, "In #{code(f[:path])} passen Tags nicht zusammen. Zeile #{p.line}: #{p.message} Repariere das vor der Abgabe; sonst sieht deine Lehrkraft vielleicht etwas anderes als du.")
            end
        end
        if requires.key?('scene_loads') || requires.key?('min_objects')
            notes << 'Ob die Szene lädt und genug Objekte hat, prüft der Workspace noch nicht. Schau sie dir vor der Abgabe selbst an.'
        end
        unless skipped.empty?
            notes << "Nicht abgegeben, weil es symbolische Links sind: #{skipped.first(3).map { |s| code(s) }.join(', ')}#{skipped.size > 3 ? ' …' : ''}."
        end
        Plan.new(:files => files, :total => total, :skipped => skipped, :notes => notes)
    rescue WorkspaceFiles::Refused => e
        raise Refused.new(e.problem, e.message)
    end

    def self.size_text(bytes)
        if bytes < 1000 then "#{bytes} Bytes"
        elsif bytes < 1_000_000 then "#{(bytes / 1000.0).round(1).to_s.sub('.', ',').sub(/,0\z/, '')} kB"
        else "#{(bytes / 1_000_000.0).round(1).to_s.sub('.', ',').sub(/,0\z/, '')} MB"
        end
    end

    # Writes the planned files to target (a .tar.gz), replacing an older
    # submission only once the new one is complete. Returns its SHA-256.
    def self.write(ws, plan, target)
        FileUtils.mkdir_p(File.dirname(target))
        tmp = "#{target}.#{Process.pid}.#{Thread.current.object_id}.tmp"
        File.open(tmp, 'wb') do |out|
            gz = Zlib::GzipWriter.new(out)
            Gem::Package::TarWriter.new(gz) do |tar|
                plan.files.each do |f|
                    ws.open_file(f[:path]) do |io|
                        size = io.stat.size
                        tar.add_file_simple(f[:path], 0o644, size) do |entry|
                            IO.copy_stream(io, entry, size)
                        end
                    end
                end
            end
            gz.finish
        end
        File.rename(tmp, target)
        Digest::SHA256.file(target).hexdigest
    ensure
        FileUtils.rm_f(tmp) if tmp && File.exist?(tmp)
    end

    # A Kata handed in (Einreichen, KataHandIns): its code as the one file
    # of a submission, so that deleting and exporting treat it like any
    # other hand-in.
    def self.write_text(target, name, text)
        FileUtils.mkdir_p(File.dirname(target))
        tmp = "#{target}.#{Process.pid}.#{Thread.current.object_id}.tmp"
        bytes = text.to_s.b
        File.open(tmp, 'wb') do |out|
            gz = Zlib::GzipWriter.new(out)
            Gem::Package::TarWriter.new(gz) do |tar|
                tar.add_file_simple(name, 0o644, bytes.bytesize) { |entry| entry.write(bytes) }
            end
            gz.finish
        end
        File.rename(tmp, target)
    ensure
        FileUtils.rm_f(tmp) if tmp && File.exist?(tmp)
    end

    # [name, text] of the first file of a stored submission, or nil.
    def self.read_text(target, max: 64 * 1024)
        File.open(target, 'rb') do |f|
            Zlib::GzipReader.wrap(f) do |gz|
                Gem::Package::TarReader.new(gz) do |tar|
                    tar.each do |entry|
                        next unless entry.file?

                        return [entry.full_name, entry.read(max).to_s.force_encoding('UTF-8').scrub]
                    end
                end
            end
        end
        nil
    rescue Errno::ENOENT, Zlib::Error
        nil
    end

    # [{ path:, size: }] of a stored submission.
    def self.contents(target)
        entries = []
        File.open(target, 'rb') do |f|
            Zlib::GzipReader.wrap(f) do |gz|
                Gem::Package::TarReader.new(gz) do |tar|
                    tar.each { |entry| entries << { :path => entry.full_name, :size => entry.header.size } if entry.file? }
                end
            end
        end
        entries
    end

    # Markdown list of what a submission holds.
    def self.describe(files, total)
        shown = files.first(15).map { |f| "#{f[:path]}  (#{size_text(f[:size])})" }
        more = files.size > 15 ? "\n… und #{files.size - 15} weitere" : ''
        "#{files.size == 1 ? '1 Datei' : "#{files.size} Dateien"}, zusammen #{size_text(total)}:\n\n```\n#{shown.join("\n")}#{more}\n```"
    end
end
