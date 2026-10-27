require 'net/http'
require 'uri'

# Wer der Crawler ist; viele Websites schreiben das in ihr Protokoll
USER_AGENT = 'Hackschule-Crawler (Informatikkurs, Hackschule Workspace)'
# Höchstens so viele Seiten, und so viele Sekunden Pause vor jeder
MAX_PAGES = 30
PAUSE = 1

# Die Ziele aller Links einer Seite, so wie sie im HTML stehen
def href_targets(html)
  html.scan(/<a\s[^>]*?href\s*=\s*["']([^"']*)["']/i).flatten
end

# Die Links einer Seite als ganze Adressen, nur auf ihrer eigenen Website,
# ohne #…, jede nur einmal; base ist die Adresse der Seite
def absolute_links(html, base)
  base_uri = URI(base)
  result = []
  href_targets(html).each do |target|
    uri = (URI.join(base, target) rescue nil)
    next if uri.nil?
    next unless %w[http https].include?(uri.scheme)
    next unless uri.host == base_uri.host && uri.port == base_uri.port

    uri.fragment = nil
    url = uri.to_s
    result << url unless result.include?(url)
  end
  result
end

# Was robots.txt allen Crawlern verbietet (User-agent: *), als Liste von
# Anfängen von Pfaden; vereinfacht, RFC 9309 kennt noch mehr
def robots_rules(text)
  rules = []
  applies = false
  text.each_line do |line|
    key, value = line.sub(/#.*/, '').split(':', 2).map(&:strip)
    next if value.nil?

    case key.downcase
    when 'user-agent' then applies = (value == '*')
    when 'disallow' then rules << value if applies && !value.empty?
    end
  end
  rules
end

# Ob der Crawler den Pfad holen darf: Keine Regel ist sein Anfang
def allowed?(rules, path)
  rules.none? { |rule| path.start_with?(rule) }
end

# Eine Anfrage mit dem User-Agent des Crawlers
def get(url)
  uri = URI(url)
  Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https',
                  open_timeout: 10, read_timeout: 10) do |http|
    http.get(uri.request_uri, 'User-Agent' => USER_AGENT)
  end
end

# robots.txt der Website, oder nichts, wenn es keine gibt
def fetch_robots(start)
  response = get(URI.join(start, '/robots.txt').to_s)
  response.is_a?(Net::HTTPSuccess) ? response.body.force_encoding('UTF-8').scrub : ''
rescue StandardError
  ''
end

# Eine HTML-Seite als [Adresse, HTML], oder nil; folgt Weiterleitungen,
# solange sie auf derselben Website bleiben
def fetch_url(url)
  host = URI(url).host
  5.times do
    response = get(url)
    if response.is_a?(Net::HTTPRedirection)
      url = URI.join(url, response['location']).to_s
      return nil unless URI(url).host == host

      next
    end
    return nil unless response.is_a?(Net::HTTPSuccess) && response['content-type'].to_s.start_with?('text/html')

    return [url, response.body.force_encoding('UTF-8').scrub]
  end
  nil
rescue StandardError
  nil
end

# Höchstens max_pages Seiten der Website von start, mit Pause und nach
# robots.txt, als Hash von Adresse zu HTML
def polite_crawl(start, max_pages = MAX_PAGES)
  rules = robots_rules(fetch_robots(start))
  puts "robots.txt verbietet: #{rules.empty? ? 'nichts' : rules.join(', ')}"
  pages = {}
  queue = [start]
  seen = [start]
  until queue.empty? || pages.size >= max_pages
    url = queue.shift
    unless allowed?(rules, URI(url).request_uri)
      puts "übersprungen: #{url}"
      next
    end
    sleep PAUSE
    found = fetch_url(url)
    next if found.nil?

    address, html = found
    puts "geholt: #{address}"
    pages[address] = html
    absolute_links(html, address).each do |link|
      next if seen.include?(link)

      seen << link
      queue << link
    end
  end
  pages
end
