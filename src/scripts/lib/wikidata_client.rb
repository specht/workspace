# frozen_string_literal: true

require 'digest'
require 'fileutils'
require 'json'
require 'net/http'
require 'openssl'
require 'uri'

# Shared by the classroom datasets built from Wikidata (collect-movies,
# collect-countries).
# Talks to the Wikidata Query Service politely: one request at a time, a pause
# between requests, Retry-After on 429, and every answer cached on disk so a
# second run (or a run after an interruption) asks only for what is missing.
class WikidataClient
  ENDPOINT = URI('https://query.wikidata.org/sparql')
  USER_AGENT = 'hackschule-workspace-movie-dataset/2.0 (https://github.com/specht/workspace; classroom dataset)'

  attr_reader :requests

  # user_agent: names the dataset and this repository, as the query service asks
  def initialize(cache_dir:, pause: 1.0, offline: false, user_agent: USER_AGENT)
    @user_agent = user_agent
    @cache_dir = cache_dir
    @pause = pause
    @offline = offline
    @requests = 0
    FileUtils.mkdir_p(@cache_dir)
  end

  # Returns the bindings as [{ 'var' => 'value' }].
  def select(query)
    path = File.join(@cache_dir, "#{Digest::SHA1.hexdigest(query)}.json")
    body = File.exist?(path) ? File.read(path) : fetch(query, path)
    JSON.parse(body).dig('results', 'bindings').map { |row| row.transform_values { |v| v['value'] } }
  end

  private

  # Gave up on a query: it timed out on the server or the connection kept
  # breaking. Callers may split the batch and try the halves.
  class Failed < StandardError; end

  NETWORK_ERRORS = [EOFError, Errno::ECONNRESET, Errno::ECONNREFUSED, Errno::EPIPE, Net::OpenTimeout,
                    Net::ReadTimeout, OpenSSL::SSL::SSLError, SocketError, IOError].freeze

  def fetch(query, path)
    raise "Not cached and --offline given: #{query[0, 120]}" if @offline

    attempts = 0
    loop do
      attempts += 1
      sleep @pause if @requests.positive?
      @requests += 1
      request = Net::HTTP::Post.new(ENDPOINT)
      request['User-Agent'] = @user_agent
      request['Accept'] = 'application/sparql-results+json'
      request.set_form_data('query' => query)
      begin
        response = Net::HTTP.start(ENDPOINT.host, ENDPOINT.port, use_ssl: true, open_timeout: 30, read_timeout: 90) do |http|
          http.request(request)
        end
      rescue *NETWORK_ERRORS => e
        # The query service drops the connection when a query runs into its
        # 60 s limit; a dropped connection is also just the network.
        raise Failed, "connection broke #{attempts} times (#{e.class})" if attempts >= 3

        STDERR.puts "  connection broke (#{e.class}), trying again in #{10 * attempts}s"
        sleep 10 * attempts
        next
      end
      case response
      when Net::HTTPSuccess
        # A query that runs out of time while the answer streams comes back
        # as 200 with half a JSON document and a stack trace: not an answer.
        begin
          JSON.parse(response.body)
        rescue JSON::ParserError
          raise Failed, 'query timed out while streaming' if attempts >= 2

          sleep 5
          next
        end
        File.write("#{path}.part", response.body)
        File.rename("#{path}.part", path)
        return response.body
      when Net::HTTPTooManyRequests, Net::HTTPServiceUnavailable, Net::HTTPGatewayTimeout, Net::HTTPBadGateway
        raise Failed, "Wikidata keeps refusing (#{response.code})" if attempts >= 6

        wait = [response['Retry-After'].to_i, 5 * attempts].max
        STDERR.puts "  Wikidata says #{response.code}, waiting #{wait}s"
        sleep wait
      when Net::HTTPInternalServerError
        # A query that ran too long comes back as 500 with a TimeoutException.
        raise Failed, 'query timed out on the server' if response.body.to_s.include?('TimeoutException') || attempts >= 2

        sleep 5
      else
        raise "Wikidata query failed: #{response.code} #{response.message}\n#{response.body.to_s[0, 500]}"
      end
    end
  end
end
