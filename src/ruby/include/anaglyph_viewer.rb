require 'fileutils'
require 'open3'
require 'rubygems/package'
require 'stringio'
require 'zlib'
require 'base64'
require 'json'
require 'openssl'
require_relative 'submission_view'

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

    # The viewer runs on abgaben.<host>, sandboxed like a hand-in
    # (SubmissionView): an opaque origin without the site's cookies, so what
    # a scene's expressions do stays there instead of running in the session
    # of whoever looks at it, a teacher on the class page for one. Two
    # changes to the pinned script make it work there: an opaque origin
    # cannot name itself, so its messages go to the parent with '*' (only
    # the site may frame it: frame-ancestors), and the picture message
    # carries the number of objects, which the page cannot read out of a
    # frame from another origin. Each change has to match exactly once; a
    # new viewer commit without these lines fails closed. Once specht/
    # anaglyph has them itself (PARENT_ORIGIN, objects), bump COMMIT and
    # empty this list in the same change.
    SANDBOX_PATCHES = [
        ["    window.parent.postMessage({\n        type: 'anaglyph-picture',\n",
         "    window.parent.postMessage({\n        type: 'anaglyph-picture',\n" \
         "        objects: Array.isArray(sceneDescription) ? sceneDescription.filter((entry) => " \
         "(entry.shape || entry.model || entry.text !== undefined) && entry.shape !== 'grid').length : null,\n"],
        ["    }, window.location.origin);\n", "    }, '*');\n"],
        ["{ type: 'anaglyph-key', key: e.key }, window.location.origin);", "{ type: 'anaglyph-key', key: e.key }, '*');"],
    ].freeze

    class Unpatchable < StandardError; end

    # The bytes of a viewer file as the sandboxed viewer gets them.
    def self.for_sandbox(path, bytes)
        if path == File.join(dir, 'include', 'script.js')
            text = bytes.dup.force_encoding(Encoding::UTF_8)
            SANDBOX_PATCHES.each do |from, to|
                count = text.scan(from).size
                raise Unpatchable, "script.js: expected one #{from.strip[0, 40].inspect}, found #{count}" unless count == 1

                text = text.sub(from) { to }
            end
            text
        elsif path == File.join(dir, 'index.html')
            # sessionStorage (the camera of a scene) throws in an opaque
            # origin; the same in-memory stand-in as for hand-ins.
            text = bytes.dup.force_encoding(Encoding::UTF_8)
            if text.match?(/<head[^>]*>/i)
                text.sub(/<head[^>]*>/i) { |tag| tag + SubmissionView::STORAGE_SHIM }
            else
                SubmissionView::STORAGE_SHIM + text
            end
        else
            bytes
        end
    end

    # A scene's address on abgaben.<host>: what to read, signed and valid as
    # long as a hand-in address. data: { 's' => state key } for a handed-in
    # scene, { 'e' => email, 'f' => path } for a file in someone's Workspace
    # (the preview before handing in).
    SCENE_TOKEN = /\A([A-Za-z0-9_-]{1,600})\.(\d{1,12})\.([0-9a-f]{32})\z/

    def self.scene_token(data, secret:, now: Time.now.to_i)
        payload = Base64.urlsafe_encode64(JSON.generate(data), padding: false)
        expires = now + SubmissionView::LIFETIME
        "#{payload}.#{expires}.#{SubmissionView.sign("anaglyph-scene.#{payload}.#{expires}", secret)}"
    end

    # The data of a genuine, unexpired scene token, or nil.
    def self.verify_scene_token(token, secret:, now: Time.now.to_i)
        m = SCENE_TOKEN.match(token.to_s)
        return nil if m.nil? || m[2].to_i < now

        expected = SubmissionView.sign("anaglyph-scene.#{m[1]}.#{m[2]}", secret)
        return nil unless OpenSSL.fixed_length_secure_compare(expected, m[3])

        data = JSON.parse(Base64.urlsafe_decode64(m[1]))
        data.is_a?(Hash) ? data : nil
    rescue ArgumentError, JSON::ParserError
        nil
    end
end
