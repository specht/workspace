# Writes ../starter/access.log, the log of an invented school website for
# »Reguläre Ausdrücke«: one Tuesday in the combined format of nginx. Every
# address is from a range for documentation (RFC 5737) or the school's own
# 10.12.0.0/16; the names in the paths are invented. The tasks count lines in
# it (tasks.yaml), so after a change run the task examples again.
#
#   ruby src/content/regex/daten/access_log.rb
srand(2026)

PAGES = {
    '/' => 9200, '/index.html' => 9200, '/stundenplan.html' => 18_300, '/termine.html' => 7600,
    '/mensa.html' => 5100, '/vertretungsplan.html' => 12_800, '/kontakt.html' => 4300,
    '/ag/informatik.html' => 6200, '/ag/theater.html' => 5800, '/impressum.html' => 3900,
    '/style.css' => 8100, '/script.js' => 2600, '/bilder/logo.png' => 14_042,
    '/bilder/sportfest.jpg' => 248_300, '/bilder/mensa-heute.webp' => 61_400,
    '/downloads/elternbrief-oktober.pdf' => 182_900, '/favicon.ico' => 4048,
}.freeze
SCHOOL = (1..40).map { |i| "10.12.#{[3, 3, 4, 7][i % 4]}.#{20 + i * 3}" }
OUTSIDE = %w[203.0.113.7 203.0.113.25 203.0.113.80 203.0.113.142 198.51.100.17 198.51.100.203 192.0.2.44 192.0.2.91]
AGENTS = [
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36',
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:131.0) Gecko/20100101 Firefox/131.0',
    'Mozilla/5.0 (X11; Linux x86_64; rv:131.0) Gecko/20100101 Firefox/131.0',
    'Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Mobile/15E148 Safari/604.1',
    'Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Mobile Safari/537.36',
].freeze
SCANNER = '198.51.100.66'
SCANNER_AGENT = 'Mozilla/5.0 (compatible; scan/0.9)'
SCAN = ['/wp-login.php', '/wp-admin/', '/xmlrpc.php', '/.env', '/phpmyadmin/index.php', '/admin.php',
        '/old/index.php', '/.git/config', '/wp-content/plugins/', '/backup.zip', '/config.php', '/shell.php'].freeze

lines = []
time = Time.utc(2026, 10, 27, 6, 58, 0)
stamp = ->(t) { (t + 3600).strftime('[%d/%b/%Y:%H:%M:%S +0100]') }
add = lambda do |ip, method, path, status, size, agent, referer = '-'|
    time += rand(5..140)
    lines << %(#{ip} - - #{stamp.(time)} "#{method} #{path} HTTP/1.1" #{status} #{size} "#{referer}" "#{agent}")
end

220.times do |i|
    ip = rand < 0.7 ? SCHOOL.sample : OUTSIDE.sample
    agent = AGENTS.sample
    path, size = PAGES.to_a.sample
    status = rand < 0.12 ? 304 : 200
    size = 0 if status == 304
    add.(ip, 'GET', path, status, size, agent, path == '/' ? '-' : 'https://www.example.org/')
    case i
    when 15, 88, 160 # an old link to last year's pictures
        add.(ip, 'GET', '/bilder/sportfest-2025.jpg', 404, 162, agent, 'https://www.example.org/termine.html')
    when 40, 131 # a typo
        add.(ip, 'GET', '/stundenplan.htm', 404, 162, agent)
    when 60
        add.(ip, 'GET', '/ag/informatik/php-kurs.html', 200, 7300, agent, 'https://www.example.org/ag/informatik.html')
    when 61
        add.(ip, 'GET', '/ag/informatik/php-kurs.html', 304, 0, agent, 'https://www.example.org/ag/informatik.html')
    when 70, 150, 190
        add.(ip, 'POST', '/kontakt.html', 200, 4410, agent, 'https://www.example.org/kontakt.html')
    when 100 # the scanner, one path after another
        SCAN.each { |p| add.(SCANNER, 'GET', p, 404, 162, SCANNER_AGENT) }
    when 120, 121, 122 # the substitution plan breaks for a few minutes
        add.(ip, 'GET', '/vertretungsplan.html', i == 122 ? 502 : 500, 0, agent)
    when 175
        add.(ip, 'GET', '/lehrerzimmer.html', 403, 154, agent)
    when 200
        add.('203.0.113.25', 'GET', '/robots.txt', 404, 162, 'curl/8.5.0')
    end
end
File.write(File.join(__dir__, '..', 'starter', 'access.log'), lines.join("\n") + "\n")
puts "#{lines.size} lines"
