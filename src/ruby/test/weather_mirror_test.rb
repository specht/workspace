require 'minitest/autorun'
require 'tmpdir'
require 'fileutils'
require_relative '../include/weather_mirror'

class WeatherMirrorTest < Minitest::Test
    def setup
        @dir = Dir.mktmpdir
        @now = 1_000_000.0
        @calls = []
        @answer = [200, '{"current":{"temperature_2m":14.2}}']
        @allowed = true
        @mirror = WeatherMirror::Mirror.new(@dir,
            :fetch => ->(url) { @calls << url; @answer.is_a?(Proc) ? @answer.call : @answer },
            :limit => -> { @allowed },
            :now => -> { Time.at(@now) })
    end

    def teardown
        FileUtils.rm_rf(@dir)
    end

    def forecast(lat = '52.5244', lon = '13.4105')
        @mirror.get('api/v1/forecast', { 'latitude' => lat, 'longitude' => lon, 'current' => 'temperature_2m' })
    end

    def test_the_same_request_comes_from_the_store
        assert_equal [200, '{"current":{"temperature_2m":14.2}}', 'miss'], forecast
        assert_equal 'hit', forecast[2]
        assert_equal 'hit', forecast('52.52', '13.41')[2]   # rounded to two decimals, the same place
        assert_equal 1, @calls.size
        assert_equal 'https://api.open-meteo.com/v1/forecast?current=temperature_2m&latitude=52.52&longitude=13.41', @calls.first
    end

    def test_after_fifteen_minutes_it_asks_again
        forecast
        @now += WeatherMirror::FRESH_SECONDS + 1
        @answer = [200, '{"new":true}']
        assert_equal [200, '{"new":true}', 'miss'], forecast
        assert_equal 2, @calls.size
    end

    def test_a_refusing_open_meteo_gets_the_last_answer
        forecast
        @now += 3600
        @answer = [429, '{"error":true,"reason":"Hourly API request limit exceeded"}']
        assert_equal [200, '{"current":{"temperature_2m":14.2}}', 'stale'], forecast
        @answer = -> { raise Errno::ECONNREFUSED }
        assert_equal 'stale', forecast[2]
    end

    def test_without_a_kept_answer_the_refusal_passes
        @answer = [429, '{"error":true,"reason":"Minutely API request limit exceeded"}']
        status, body, cache = forecast
        assert_equal 429, status
        assert_match(/Minutely/, body)
        assert_nil cache
    end

    def test_the_upstream_limit_serves_what_is_kept_or_says_why
        forecast
        @now += 3600
        @allowed = false
        assert_equal 'stale', forecast[2]
        status, body, = forecast('48.14', '11.58')
        assert_equal 429, status
        assert_match(/zu oft/, JSON.parse(body)['reason'])
    end

    def test_names_are_case_insensitive_and_unknown_parameters_are_refused
        @mirror.get('geocoding/v1/search', { 'name' => 'Berlin', 'count' => '1' })
        assert_equal 'hit', @mirror.get('geocoding/v1/search', { 'name' => ' berlin ', 'count' => '1' })[2]
        error = assert_raises(WeatherMirror::Refused) { @mirror.get('api/v1/forecast', { 'apikey' => 'x' }) }
        assert_match(/apikey/, error.message)
        assert_raises(WeatherMirror::Refused) { @mirror.get('api/v1/archive', {}) }
        assert_raises(WeatherMirror::Refused) { @mirror.get('api/v1/forecast', { 'current' => 'x' * 400 }) }
    end

    def test_errors_from_open_meteo_are_not_kept
        @answer = [400, '{"error":true,"reason":"Latitude must be in range"}']
        assert_equal 400, forecast('999', '0')[0]
        @answer = [200, '{"ok":true}']
        assert_equal 'miss', forecast('999', '0')[2]
    end
end
