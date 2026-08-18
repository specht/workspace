require 'json'
require 'net/http'
require 'open3'
require 'uri'

module TutorialScreenshots
    MARKER = '<!-- tutorial-screenshot'
    ENDPOINT = URI('http://tutorial_screenshots:9393/generate')
    STATUS_ENDPOINT = URI('http://tutorial_screenshots:9393/status')
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

    def self.workspace_image_id
        stdout, _stderr, status = Open3.capture3(
            'docker', 'image', 'inspect', WORKSPACE_IMAGE,
            '--format', '{{.Id}}'
        )
        return stdout.strip if status.success? && !stdout.strip.empty?
        'unknown'
    rescue
        'unknown'
    end

    def self.request_generation(relative_path, force: false, async: false)
        request = Net::HTTP::Post.new(ENDPOINT)
        request['content-type'] = 'application/json'
        request.body = JSON.generate({
            :markdown_path => relative_path,
            :workspace_image_id => workspace_image_id,
            :force => force,
            :async => async,
        })

        http = Net::HTTP.new(ENDPOINT.host, ENDPOINT.port)
        http.open_timeout = 1
        http.read_timeout = force ? 3600 : 10
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

    private_class_method :workspace_image_id, :request_generation, :warn_once
end
