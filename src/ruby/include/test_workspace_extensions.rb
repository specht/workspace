require 'digest'
require 'fileutils'
require 'json'
require 'net/http'
require 'open3'
require 'tempfile'
require 'tmpdir'
require 'uri'

# Resolves package-declared extensions while the teacher uploads a test. A test
# only ever refers to cached, verified VSIX files; exam containers need no network.
module TestWorkspaceExtensions
    class Error < StandardError; end

    ARCHIVES = '/internal/test_archives'
    CACHE = '/internal/test_extensions'
    MANIFESTS = '/internal/test_manifests'
    REGISTRY = 'https://open-vsx.org/api'
    ID = /\A[A-Za-z][A-Za-z0-9-]*\.[A-Za-z][A-Za-z0-9-]*\z/
    VERSION = /\A\d+\.\d+\.\d+(?:[-+][0-9A-Za-z.-]+)?\z/
    SHA = /\A[0-9a-f]{64}\z/
    TAG = /\A[a-z0-9]{24}\z/
    MAX_ARCHIVE_BYTES = 128 * 1024 * 1024
    MAX_VSIX_BYTES = 100 * 1024 * 1024
    MAX_EXTENSIONS = 24

    def self.requested(config)
        value = config.fetch('extensions', [])
        raise Error, 'extensions muss eine Liste sein.' unless value.is_a?(Array)
        raise Error, "Maximal #{MAX_EXTENSIONS} Erweiterungen pro Paket." if value.size > MAX_EXTENSIONS

        result = value.map do |entry|
            item = entry.is_a?(String) ? {'id' => entry} : entry
            unless item.is_a?(Hash) && (item.keys - ['id', 'version']).empty?
                raise Error, 'Erweiterung: erwartet ID oder {id, version}.'
            end
            id = item['id']
            version = item['version']
            raise Error, "Ungültige Erweiterungs-ID: #{id.inspect}" unless id.is_a?(String) && ID.match?(id)
            if !version.nil? && (!version.is_a?(String) || !VERSION.match?(version))
                raise Error, "Ungültige Version für #{id}: #{version.inspect}"
            end
            {'id' => id, 'version' => version}
        end
        if result.map { |e| e['id'].downcase }.uniq.size != result.size
            raise Error, 'Eine Erweiterung darf nur einmal aufgeführt werden.'
        end
        result
    end

    def self.run!(*command)
        output, error, status = Open3.capture3(*command)
        raise Error, "#{command.first} fehlgeschlagen: #{error.strip[0, 240]}" unless status.success?
        output
    end

    # Reject links, special files and paths escaping the workspace before tar
    # extracts anything. Do not permit the reserved extension staging directory.
    def self.validate_archive!(archive)
        raise Error, 'Das Archiv ist zu groß.' if File.size(archive) > MAX_ARCHIVE_BYTES
        entries = run!('tar', '-tf', archive).lines.map(&:chomp)
        types = run!('tar', '-tvf', archive).lines
        raise Error, 'Das Archiv ist leer oder beschädigt.' if entries.empty? || types.size != entries.size
        entries.zip(types).each do |path, description|
            relative = path.sub(%r{\A\./}, '').sub(%r{/\z}, '')
            components = relative.split('/')
            if path.start_with?('/') || components.include?('..') || path.include?("\0") ||
                    components.any?(&:empty?) || components.first == '.exam-extensions'
                raise Error, "Unsicherer Archivpfad: #{path.inspect}"
            end
            next if relative == '.'
            unless ['-', 'd'].include?(description[0])
                raise Error, "Links und Spezialdateien im Paket sind nicht erlaubt: #{path}"
            end
        end
    end

    def self.prepare!(archive, tag:, cache: CACHE)
        validate_archive!(archive)
        Dir.mktmpdir('exam-package-') do |dir|
            run!('tar', '-xf', archive, '-C', dir, '--no-same-owner', '--no-same-permissions')
            # Existing package validation happens at upload rather than on the
            # first student launch; keep the original archive unchanged.
            TestWorkspacePackage.validate_git_layout!(dir)
            TestWorkspacePackage.vscode_config(dir)
            config = TestWorkspacePackage.load_config(dir)
            roots = requested(config)
            resolved = {}
            visiting = {}
            roots.each { |entry| resolve!(entry['id'], entry['version'], resolved, visiting, cache: cache) }
            {'version' => 1, 'extensions' => resolved.values}
        end
    end

    def self.resolve!(id, requested_version, resolved, visiting, cache:)
        key = id.downcase
        if (existing = resolved[key])
            if requested_version && existing['version'] != requested_version
                raise Error, "Versionskonflikt bei #{id}: #{requested_version} / #{existing['version']}"
            end
            return
        end
        raise Error, "Zyklische Erweiterungsabhängigkeit: #{id}" if visiting[key]
        raise Error, "Maximal #{MAX_EXTENSIONS} Erweiterungen einschließlich Abhängigkeiten." if resolved.size + visiting.size >= MAX_EXTENSIONS
        visiting[key] = true
        begin
            publisher, name = id.split('.', 2)
            endpoint = "#{REGISTRY}/#{publisher}/#{name}/#{requested_version || 'latest'}"
            metadata = JSON.parse(fetch(endpoint, limit: 2 * 1024 * 1024))
            version = metadata.fetch('version')
            unless version.is_a?(String) && VERSION.match?(version) && (!requested_version || version == requested_version)
                raise Error, "Ungültige Registry-Version für #{id}: #{version.inspect}"
            end
            files = metadata.fetch('files')
            url = files.fetch('download')
            Dir.mktmpdir('exam-vsix-') do |dir|
                vsix = File.join(dir, 'download.vsix')
                download(url, vsix)
                run!('unzip', '-tq', vsix)
                manifest = JSON.parse(run!('unzip', '-p', vsix, 'extension/package.json'))
                actual_id = "#{manifest['publisher']}.#{manifest['name']}"
                if actual_id.downcase != key || manifest['version'] != version
                    raise Error, "Erweiterungsmanifest stimmt nicht mit #{id}@#{version} überein."
                end
                raise Error, "#{id}: engines.vscode fehlt." unless manifest.dig('engines', 'vscode').is_a?(String)
                dependencies = Array(manifest['extensionDependencies']) + Array(manifest['extensionPack'])
                dependencies.each do |dependency|
                    raise Error, "Ungültige Abhängigkeit für #{id}." unless dependency.is_a?(String) && ID.match?(dependency)
                    resolve!(dependency, nil, resolved, visiting, cache: cache)
                end
                sha = Digest::SHA256.file(vsix).hexdigest
                FileUtils.mkdir_p(cache)
                dest = File.join(cache, "#{sha}.vsix")
                if File.file?(dest)
                    raise Error, "Beschädigter Extension-Cache: #{sha}" unless Digest::SHA256.file(dest).hexdigest == sha
                else
                    # Rename within the cache filesystem: no partial file becomes visible.
                    tmp = Tempfile.create(['exam-extension-', '.vsix'], cache)
                    begin
                        FileUtils.copy_file(vsix, tmp.path)
                        tmp.close
                        File.rename(tmp.path, dest)
                    ensure
                        File.unlink(tmp.path) if File.exist?(tmp.path)
                    end
                end
                resolved[key] = {'id' => actual_id, 'version' => version, 'sha256' => sha}
            end
        rescue KeyError, TypeError, JSON::ParserError, SystemCallError, Timeout::Error => e
            raise Error, "#{id}: Vorbereitung fehlgeschlagen (#{e.message})"
        ensure
            visiting.delete(key)
        end
    end

    # HTTPS-only, bounded downloads. Redirects must remain at the registry or
    # its known object-storage host; arbitrary URLs from package YAML are never used.
    def self.fetch(url, limit:, destination: nil, redirects: 4)
        uri = URI.parse(url)
        allowed = uri.host == 'open-vsx.org' || uri.host == 'storage.googleapis.com' ||
            uri.host&.end_with?('.open-vsx.org')
        raise Error, 'Ungültiger Erweiterungs-Download-Link.' unless uri.is_a?(URI::HTTPS) && allowed && !uri.userinfo
        raise Error, 'Zu viele Download-Weiterleitungen.' if redirects < 0
        response = nil
        Net::HTTP.start(uri.host, uri.port, use_ssl: true, open_timeout: 12, read_timeout: 40) do |http|
            http.request(Net::HTTP::Get.new(uri.request_uri)) do |r|
                response = r
                if r.is_a?(Net::HTTPSuccess)
                    bytes = 0
                    buffer = +'' unless destination
                    if destination
                        File.open(destination, 'wb') do |file|
                            r.read_body do |part|
                                bytes += part.bytesize
                                raise Error, 'Der Erweiterungs-Download ist zu groß.' if bytes > limit
                                file.write(part)
                            end
                        end
                    end
                    unless destination
                        r.read_body do |part|
                            bytes += part.bytesize
                            raise Error, 'Die Registry-Antwort ist zu groß.' if bytes > limit
                            buffer << part
                        end
                        return buffer
                    end
                    return destination
                end
            end
        end
        if response.is_a?(Net::HTTPRedirection)
            location = URI.join(uri.to_s, response['location']).to_s
            return fetch(location, limit: limit, destination: destination, redirects: redirects - 1)
        end
        raise Error, "Registry-Download fehlgeschlagen (HTTP #{response&.code || 'unbekannt'})."
    rescue URI::InvalidURIError, SocketError, IOError, SystemCallError, Net::OpenTimeout, Net::ReadTimeout => e
        raise Error, "Registry nicht erreichbar: #{e.message}"
    end

    def self.download(url, path)
        fetch(url, limit: MAX_VSIX_BYTES, destination: path)
    end

    def self.publish!(manifest, tag:, manifests: MANIFESTS)
        raise Error, 'Ungültiger Prüfungstag.' unless TAG.match?(tag)
        FileUtils.mkdir_p(manifests)
        dest = File.join(manifests, "#{tag}.json")
        tmp = Tempfile.create(['exam-manifest-', '.json'], manifests)
        begin
            tmp.write(JSON.generate(manifest))
            tmp.close
            File.rename(tmp.path, dest)
        ensure
            File.unlink(tmp.path) if File.exist?(tmp.path)
        end
    end

    def self.stage!(tag:, workspace_path:, cache: CACHE, manifests: MANIFESTS)
        raise Error, 'Ungültiger Prüfungstag.' unless TAG.match?(tag)
        manifest_path = File.join(manifests, "#{tag}.json")
        return unless File.file?(manifest_path) # Archives uploaded before this feature.
        manifest = JSON.parse(File.read(manifest_path))
        raise Error, 'Ungültiges Erweiterungsmanifest.' unless manifest['version'] == 1 && manifest['extensions'].is_a?(Array)
        target = File.join(workspace_path, '.exam-extensions')
        FileUtils.rm_rf(target)
        FileUtils.mkdir_p(target)
        manifest['extensions'].each do |item|
            sha, id, version = item.values_at('sha256', 'id', 'version')
            unless sha.is_a?(String) && SHA.match?(sha) && id.is_a?(String) && ID.match?(id) &&
                    version.is_a?(String) && VERSION.match?(version)
                raise Error, 'Ungültige Erweiterungsdaten im Prüfungsmanifest.'
            end
            from = File.join(cache, "#{sha}.vsix")
            raise Error, "Extension-Cache fehlt: #{id}@#{version}" unless File.file?(from)
            raise Error, "Extension-Cache beschädigt: #{id}@#{version}" unless Digest::SHA256.file(from).hexdigest == sha
            FileUtils.cp(from, File.join(target, "#{sha}.vsix"))
        end
        File.write(File.join(target, 'manifest.json'), JSON.generate(manifest))
    end
end
