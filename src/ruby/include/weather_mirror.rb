require 'digest'
require 'fileutils'
require 'json'
require 'net/http'
require 'uri'

# The school's mirror of Open-Meteo, for the activity »Daten aus dem Netz«.
#
# Open-Meteo counts requests per address (600 a minute, 5,000 an hour,
# 10,000 a day), and a whole school shares one: the Workspaces the server's,
# the browsers the school network's. One runaway loop or a class reloading
# its weather page can use that up for everyone. The activity tells students
# to switch to this mirror when Open-Meteo answers 429. It forwards the two
# endpoints the activity uses, under the same paths and parameters:
#
#   /open-meteo/api/v1/forecast        -> https://api.open-meteo.com/v1/forecast
#   /open-meteo/geocoding/v1/search    -> https://geocoding-api.open-meteo.com/v1/search
#
# Every answer is kept on disk under the normalised request (coordinates to
# two decimals, names in lower case). The same request within FRESH_SECONDS
# comes from there, so a class asking for Berlin costs Open-Meteo one
# request. When Open-Meteo refuses or does not answer, the last kept answer
# is served, however old, with X-Hackschule-Cache: stale. At most
# UPSTREAM_PER_HOUR requests a hour go to Open-Meteo.
module WeatherMirror
    UPSTREAMS = {
        'api/v1/forecast' => 'https://api.open-meteo.com/v1/forecast',
        'geocoding/v1/search' => 'https://geocoding-api.open-meteo.com/v1/search',
    }.freeze

    # The parameters the activity and its ideas use; anything else is refused.
    PARAMETERS = {
        'api/v1/forecast' => %w[latitude longitude current hourly daily timezone forecast_days past_days
                                temperature_unit wind_speed_unit precipitation_unit timeformat],
        'geocoding/v1/search' => %w[name count language format countryCode],
    }.freeze

    FRESH_SECONDS = 15 * 60
    UPSTREAM_PER_HOUR = 600
    MAX_VALUE = 300
    MAX_ENTRIES = 20_000
    TIMEOUT = 8

    class Refused < StandardError; end

    # The request as one canonical string, or Refused with a reason.
    def self.key(endpoint, params)
        allowed = PARAMETERS[endpoint] or raise Refused, 'Diesen Endpunkt gibt es im Spiegel nicht.'
        unknown = params.keys - allowed
        raise Refused, "Unbekannter Parameter: #{unknown.first}" unless unknown.empty?

        pairs = params.map do |name, value|
            value = value.to_s.strip
            raise Refused, "#{name} ist zu lang." if value.size > MAX_VALUE

            value = format('%.2f', Float(value)) if %w[latitude longitude].include?(name) && value.match?(/\A-?\d+(\.\d+)?\z/)
            value = value.downcase if name == 'name'
            [name, value]
        end
        "#{endpoint}?#{URI.encode_www_form(pairs.sort)}"
    end

    # A mirror with its store in dir. fetch: ->(url) { [status, body] }, raises
    # on network errors; limit: ->() { true when one more upstream request is
    # allowed }. Both replaceable for tests.
    class Mirror
        def initialize(dir, fetch: WeatherMirror.method(:fetch), limit: nil, now: -> { Time.now })
            @dir = dir
            @fetch = fetch
            @limit = limit || WeatherMirror.default_limit
            @now = now
            @mutex = Mutex.new
        end

        # [status, body, cache] where cache is hit, miss, stale or nil (passed
        # through, nothing kept).
        def get(endpoint, params)
            key = WeatherMirror.key(endpoint, params)
            kept = read(key)
            return [200, kept['body'], 'hit'] if kept && @now.call.to_f - kept['time'] < FRESH_SECONDS

            unless @limit.call
                return kept ? [200, kept['body'], 'stale'] : [429, reason('Der Spiegel der Schule fragt gerade zu oft bei Open-Meteo nach. Versuch es in ein paar Minuten noch einmal.'), nil]
            end

            begin
                status, body = @fetch.call("#{UPSTREAMS.fetch(endpoint)}?#{key.split('?', 2)[1]}")
            rescue StandardError
                return kept ? [200, kept['body'], 'stale'] : [502, reason('Open-Meteo antwortet gerade nicht.'), nil]
            end

            if status == 200
                write(key, body)
                [200, body, 'miss']
            elsif kept && (status == 429 || status >= 500)
                [200, kept['body'], 'stale']
            else
                [status, body, nil]
            end
        end

        private

        def reason(text)
            JSON.generate({ 'error' => true, 'reason' => text })
        end

        def path(key)
            File.join(@dir, "#{Digest::SHA256.hexdigest(key)}.json")
        end

        def read(key)
            JSON.parse(File.read(path(key)))
        rescue Errno::ENOENT, JSON::ParserError
            nil
        end

        def write(key, body)
            @mutex.synchronize do
                FileUtils.mkdir_p(@dir)
                tmp = "#{path(key)}.#{Process.pid}.tmp"
                File.write(tmp, JSON.generate({ 'key' => key, 'time' => @now.call.to_f, 'body' => body }))
                File.rename(tmp, path(key))
                prune
            end
        end

        # Keeps the store from growing without end: past MAX_ENTRIES the
        # oldest tenth goes.
        def prune
            files = Dir[File.join(@dir, '*.json')]
            return if files.size <= MAX_ENTRIES

            files.sort_by { |f| File.mtime(f) }.first(files.size / 10).each { |f| File.delete(f) rescue nil }
        end
    end

    def self.fetch(url)
        uri = URI(url)
        Net::HTTP.start(uri.host, uri.port, :use_ssl => true, :open_timeout => TIMEOUT, :read_timeout => TIMEOUT) do |http|
            response = http.get(uri.request_uri, 'User-Agent' => 'Hackschule-Workspace-Spiegel')
            [response.code.to_i, response.body.to_s]
        end
    end

    # At most UPSTREAM_PER_HOUR upstream requests in any hour, for the whole
    # server.
    def self.default_limit
        times = []
        mutex = Mutex.new
        lambda do
            mutex.synchronize do
                now = Time.now.to_f
                times.shift while times.any? && times.first <= now - 3600
                next false if times.size >= UPSTREAM_PER_HOUR

                times << now
                true
            end
        end
    end
end
