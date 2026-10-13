require 'json'
require 'net/http'
require 'open3'
require 'uri'

module TutorialScreenshots
    MARKER = '<!-- tutorial-screenshot'
    ENDPOINT = URI('http://tutorial_screenshots:9393/generate')
    STATUS_ENDPOINT = URI('http://tutorial_screenshots:9393/status')
    AUTO_RENDER_ENDPOINT = URI('http://tutorial_screenshots:9393/auto-render')
    WORKSPACE_IMAGE = 'hs_code_server'

    def self.prepare(markdown, markdown_path, generate: true)
        return markdown unless defined?(DEVELOPMENT) && DEVELOPMENT
        return markdown unless generate
        return markdown unless markdown.include?(MARKER)

        relative_path = relative_markdown_path(markdown_path)
        unless relative_path
            STDERR.puts ">>> Tutorial screenshots: refusing unexpected content path #{markdown_path.inspect}"
            return markdown
        end

        response = request_generation(relative_path, :async => true)
        if response && response['monitor']
            STDERR.puts ">>> Tutorial screenshots: #{response['stale'].to_i} stale image(s) queued for #{relative_path}"
        end
        markdown
    rescue => e
        warn_once("#{e.class}: #{e.message}")
        markdown
    end

    def self.recreate(relative_path)
        unless valid_relative_markdown_path?(relative_path)
            raise "invalid tutorial Markdown path: #{relative_path.inspect}"
        end

        request_generation(relative_path, :force => true)
    end

    # Renders only what is out of date, and waits for it (src/scripts/recreate-screenshots.rb --stale).
    def self.refresh(relative_path)
        unless valid_relative_markdown_path?(relative_path)
            raise "invalid tutorial Markdown path: #{relative_path.inspect}"
        end

        request_generation(relative_path)
    end

    def self.status(markdown_path)
        relative_path = relative_markdown_path(markdown_path)
        raise "invalid tutorial Markdown path: #{markdown_path.inspect}" unless relative_path

        uri = STATUS_ENDPOINT.dup
        uri.query = URI.encode_www_form(:markdown_path => relative_path)
        request = Net::HTTP::Get.new(uri)

        http = Net::HTTP.new(uri.host, uri.port)
        http.open_timeout = 1
        http.read_timeout = 2
        response = http.request(request)

        unless response.is_a?(Net::HTTPSuccess)
            raise "generator returned HTTP #{response.code}: #{response.body.to_s[0, 500]}"
        end
        JSON.parse(response.body)
    end

    # Pauses or resumes rendering on page views (the status bar's button).
    # Explicit runs (recreate-screenshots.rb) render either way.
    def self.set_auto_render(enabled)
        request = Net::HTTP::Post.new(AUTO_RENDER_ENDPOINT)
        request['content-type'] = 'application/json'
        request.body = JSON.generate({ :enabled => enabled == true })
        http = Net::HTTP.new(AUTO_RENDER_ENDPOINT.host, AUTO_RENDER_ENDPOINT.port)
        http.open_timeout = 1
        http.read_timeout = 5
        response = http.request(request)
        raise "generator returned HTTP #{response.code}" unless response.is_a?(Net::HTTPSuccess)
        JSON.parse(response.body)
    end

    def self.valid_relative_markdown_path?(relative_path)
        relative_path.is_a?(String) &&
            relative_path.end_with?('.md') &&
            !relative_path.start_with?('/') &&
            !relative_path.split('/').include?('..')
    end

    def self.relative_markdown_path(markdown_path)
        return nil unless markdown_path.is_a?(String)
        relative_path = markdown_path.delete_prefix('/src/content/')
        return nil if relative_path == markdown_path
        return nil unless valid_relative_markdown_path?(relative_path)
        relative_path
    end

    # What the Workspace looks like in a screenshot: the code-server release
    # the image is built on (the label linuxserver/code-server sets, which
    # hs_code_server inherits). Rebuilding the image for a new extension or
    # toolchain keeps it, so screenshots are not re-rendered for that.
    # 'unknown' if docker cannot tell; the generator never treats that as a
    # change.
    LOOK_LABELS = ['org.opencontainers.image.version', 'build_version'].freeze

    def self.workspace_look
        format = LOOK_LABELS.map { |label| "{{ index .Config.Labels \"#{label}\" }}" }.join('|')
        stdout, _stderr, status = Open3.capture3('docker', 'image', 'inspect', WORKSPACE_IMAGE, '--format', format)
        return 'unknown' unless status.success?

        look_from_labels(stdout)
    rescue
        'unknown'
    end

    # »4.138.0-ls300|Linuxserver.io version:- …« -> »4.138.0-ls300«.
    def self.look_from_labels(output)
        value = output.to_s.strip.split('|').map(&:strip).find { |v| !v.empty? && v != '<no value>' }
        value || 'unknown'
    end

    def self.request_generation(relative_path, force: false, async: false)
        request = Net::HTTP::Post.new(ENDPOINT)
        request['content-type'] = 'application/json'
        request.body = JSON.generate({
            :markdown_path => relative_path,
            :workspace_look => workspace_look,
            :force => force,
            :async => async,
        })

        http = Net::HTTP.new(ENDPOINT.host, ENDPOINT.port)
        http.open_timeout = 1
        http.read_timeout = async ? 10 : 3600
        http.write_timeout = 5
        response = http.request(request)

        unless response.is_a?(Net::HTTPSuccess)
            raise "generator returned HTTP #{response.code}: #{response.body.to_s[0, 500]}"
        end
        JSON.parse(response.body)
    end

    def self.warn_once(message)
        @last_warning ||= nil
        @last_warning_at ||= Time.at(0)
        return if @last_warning == message && Time.now - @last_warning_at < 30

        @last_warning = message
        @last_warning_at = Time.now
        STDERR.puts ">>> Tutorial screenshots unavailable: #{message}"
    end

    private_class_method :workspace_look, :request_generation, :warn_once
end
