require 'fileutils'
require 'open3'
require 'rubygems/package'
require 'stringio'
require 'zlib'

# The Anaglyph viewer (specht/anaglyph) on the Workspace site, for previews
# of handed-in scenes and the class gallery (check option preview: anaglyph,
# docs/aufgaben.md). It is never copied into this repository: the server
# fetches the pinned commit from GitHub into /internal on first use. Kenney
# kits come on demand, with the template's own download.rb and kits.txt.
module AnaglyphViewer
    COMMIT = 'c2ac11e67cd0f53cde3447a28b830f08f2dc086a'
    ROOT = '/internal/anaglyph-viewer'
    TARBALL = "https://codeload.github.com/specht/anaglyph/tar.gz/#{COMMIT}"
    FETCH_TIMEOUT = 120
    KIT_TIMEOUT = 180

    MIME = {
        '.html' => 'text/html; charset=utf-8', '.js' => 'application/javascript; charset=utf-8',
        '.css' => 'text/css; charset=utf-8', '.ttf' => 'font/ttf', '.png' => 'image/png',
        '.obj' => 'text/plain; charset=utf-8', '.mtl' => 'text/plain; charset=utf-8',
        '.ini' => 'text/plain; charset=utf-8', '.txt' => 'text/plain; charset=utf-8',
    }.freeze

    FETCH_LOCK = Mutex.new
    # download.rb unpacks into a shared _temp folder: one kit at a time.
    KIT_LOCK = Mutex.new

    def self.dir
        File.join(ROOT, COMMIT)
    end

    def self.ready?
        File.file?(File.join(dir, 'index.html'))
    end

    # Fetches the pinned viewer unless it is there. Raises if that fails.
    def self.ensure!
        return dir if ready?
        FETCH_LOCK.synchronize do
            next if ready?
            temporary = "#{dir}.tmp-#{Process.pid}"
            tarball = "#{temporary}.tar.gz"
            begin
                FileUtils.rm_rf([temporary, tarball])
                FileUtils.mkdir_p(temporary)
                run!(['curl', '-fsSL', '--max-time', FETCH_TIMEOUT.to_s, '-o', tarball, TARBALL], FETCH_TIMEOUT + 10)
                run!(['tar', '-xzf', tarball, '-C', temporary, '--strip-components=1'], 60)
                raise 'the viewer archive has no index.html' unless File.file?(File.join(temporary, 'index.html'))
                FileUtils.mv(temporary, dir)
            ensure
                FileUtils.rm_rf([temporary, tarball])
            end
        end
        dir
    end

    # Kit folders as download.rb names them ("City Kit (Commercial)" ->
    # city-kit-commercial), from kits.txt of the pinned viewer.
    def self.kits
        @kits ||= begin
            File.readlines(File.join(ensure!, 'kits.txt')).map(&:strip)
                .reject { |line| line.empty? || line.start_with?('#') }
                .map { |line| line.split(' ')[0...-1].join(' ').downcase.gsub(/\s+/, '-').gsub(/[()]/, '') }
        end
    end

    def self.kit_ready?(kit)
        File.directory?(File.join(dir, kit, 'textures'))
    end

    # Fetches a kit the first time a scene asks for one of its files.
    def self.ensure_kit!(kit)
        return unless kits.include?(kit)
        return if kit_ready?(kit)
        KIT_LOCK.synchronize do
            return if kit_ready?(kit)
            run!(['ruby', 'download.rb', kit], KIT_TIMEOUT, chdir: dir)
        end
    end

    # The file under the viewer for a request path, or nil. Fetches the
    # viewer and, for a path inside a known kit, that kit.
    def self.file(rel)
        rel = rel.to_s
        return nil if rel.empty? || rel.start_with?('/') || rel.split('/').any? { |part| part.empty? || part.start_with?('.') }
        ensure!
        kit = rel.split('/').first
        ensure_kit!(kit) if rel.include?('/') && kits.include?(kit)
        path = File.join(dir, rel)
        return nil unless File.file?(path) && File.realpath(path).start_with?("#{File.realpath(dir)}/")
        path
    end

    def self.mime(path)
        MIME[File.extname(path).downcase] || 'application/octet-stream'
    end

    # The scene file in a submission (.tar.gz): the first .ini in it.
    def self.scene_from_submission(tar_gz_path)
        Zlib::GzipReader.open(tar_gz_path) do |gz|
            Gem::Package::TarReader.new(gz) do |tar|
                tar.each do |entry|
                    next unless entry.file? && entry.full_name.downcase.end_with?('.ini')
                    return entry.read.to_s.force_encoding('UTF-8')
                end
            end
        end
        nil
    end

    # Runs a command (no shell), kills it after seconds, raises unless it
    # succeeded.
    def self.run!(command, seconds, chdir: nil)
        options = { [:out, :err] => File::NULL }
        options[:chdir] = chdir if chdir
        pid = Process.spawn(*command, **options)
        waiter = Process.detach(pid)
        unless waiter.join(seconds)
            Process.kill('KILL', pid) rescue nil
            waiter.join
            raise "#{command.first} took longer than #{seconds} s"
        end
        raise "#{command.first} failed (#{waiter.value.exitstatus})" unless waiter.value.success?
    end
end
