require 'json'
require 'net/http'
require 'open3'
require 'uri'

module TutorialScreenshots
    MARKER = '<!-- tutorial-screenshot'
    ENDPOINT = URI('http://tutorial_screenshots:9393/generate')
    WORKSPACE_IMAGE = 'hs_code_server'

    def self.prepare(markdown, markdown_path, generate: true)
        return markdown unless defined?(DEVELOPMENT) && DEVELOPMENT
        return markdown unless generate
        return markdown unless markdown.include?(MARKER)

        relative_path = markdown_path.delete_prefix('/src/content/')
        unless relative_path != markdown_path && !relative_path.include?('..')
            STDERR.puts ">>> Tutorial screenshots: refusing unexpected content path #{markdown_path.inspect}"
            return markdown
        end

        response = request_generation(relative_path)
        if response && response['generated'].to_i > 0
            STDERR.puts ">>> Tutorial screenshots: generated #{response['generated']} image(s) for #{relative_path}"
        end
        markdown
    rescue => e
        warn_once("#{e.class}: #{e.message}")
        markdown
    end

    def self.recreate(relative_path)
        unless relative_path.is_a?(String) &&
                relative_path.end_with?('.md') &&
                !relative_path.start_with?('/') &&
                !relative_path.split('/').include?('..')
            raise "invalid tutorial Markdown path: #{relative_path.inspect}"
        end

        request_generation(relative_path, :force => true)
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

    def self.request_generation(relative_path, force: false)
        request = Net::HTTP::Post.new(ENDPOINT)
        request['content-type'] = 'application/json'
        request.body = JSON.generate({
            :markdown_path => relative_path,
            :workspace_image_id => workspace_image_id,
            :force => force,
        })

        http = Net::HTTP.new(ENDPOINT.host, ENDPOINT.port)
        http.open_timeout = 1
        http.read_timeout = force ? 3600 : 600
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
