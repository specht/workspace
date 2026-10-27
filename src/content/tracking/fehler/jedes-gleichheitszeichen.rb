require 'socket'
require 'uri'
require 'cgi'
require 'securerandom'

# Ein Bild, das niemand sieht: ein einziges durchsichtiges Pixel im Format GIF
PIXEL = 'R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7'.unpack1('m')

# Hier schreibt der Tracker jeden Besuch auf, eine Zeile pro Besuch
LOG = File.expand_path('visits.log', __dir__)
$log_lock = Mutex.new

# Schickt eine vollständige HTTP-Antwort, auf Wunsch mit weiteren Headern
def respond(client, status, type, body, extra = {})
  client.print "HTTP/1.1 #{status}\r\n"
  client.print "Content-Type: #{type}\r\n"
  client.print "Content-Length: #{body.bytesize}\r\n"
  extra.each { |name, value| client.print "#{name}: #{value}\r\n" }
  client.print "Connection: close\r\n"
  client.print "\r\n"
  client.print body
  client.close
end

# Text, der in HTML sicher ist: aus < wird &lt; und so weiter
def h(text)
  CGI.escapeHTML(text.to_s)
end

# Ein wenig CSS, damit die Seiten lesbar sind
STYLE = "<style>body { font-family: system-ui, sans-serif; margin: 2em; }\n" \
        "table { border-collapse: collapse; }\n" \
        "th, td { padding: 0.3em 0.8em; border-bottom: 1px solid #ddd; text-align: left; vertical-align: top; }\n" \
        "th { color: #666; font-weight: normal; }\n" \
        "td { word-break: break-all; }</style>\n"

# Die Seite unter /: alle Header, die der Browser geschickt hat
def headers_page(headers)
  rows = headers.map { |name, value| "<tr><th>#{h(name)}</th><td>#{h(value)}</td></tr>" }
  "<!DOCTYPE html><meta charset='utf-8'><title>Was ich über dich weiß</title>\n#{STYLE}" \
    "<h1>Was dieser Server über dich weiß</h1>\n<table>\n#{rows.join("\n")}\n</table>\n"
end

# Eine Zeile im Protokoll: wann, wer, welche Seite, worum es dort geht
def log_visit(visitor, page, keywords)
  line = [Time.now.strftime('%Y-%m-%d %H:%M:%S'), visitor, page, keywords].join("\t")
  $log_lock.synchronize { File.write(LOG, line + "\n", mode: 'a') }
  puts "  Besuch: #{line}"
end

# Aus dem Header Cookie (»visitor=3f9a1c2b; theme=dark«) ein Hash
def parse_cookies(header)
  cookies = {}
  header.split(';').each do |pair|
    name, value = pair.strip.split('=')
    cookies[name] = value.to_s if name && !name.empty?
  end
  cookies
end

# Die Besuche einer Kennung im Protokoll, als Paare aus Seite und Stichwörtern
def visits_of(lines, visitor)
  visits = []
  lines.each do |line|
    time, id, page, keywords = line.split("\t")
    visits << [page, keywords] if id == visitor
  end
  visits
end

# Wie oft jedes Stichwort vorkommt; jeder Text sind Stichwörter mit Komma
def count_keywords(texts)
  counts = {}
  texts.each do |text|
    text.to_s.split(',').each do |keyword|
      keyword = keyword.strip
      next if keyword.empty?

      counts[keyword] = (counts[keyword] || 0) + 1
    end
  end
  counts
end

# Die Seite unter /profil: was der Tracker über diese Kennung gesammelt hat
def profile_page(visitor, visits, counts, headers)
  pages = visits.map { |page, keywords| "<li>#{h(page)}</li>" }
  interests = counts.sort_by { |keyword, count| -count }.first(5).map do |keyword, count|
    "<li>#{h(keyword)} (#{count})</li>"
  end
  "<!DOCTYPE html><meta charset='utf-8'><title>Dein Profil</title>\n#{STYLE}" \
    "<h1>Dein Profil</h1>\n" \
    "<p>Deine Kennung: #{h(visitor || 'keine')}</p>\n" \
    "<p>Dein Browser: #{h(headers['user-agent'])}<br>Deine Sprache: #{h(headers['accept-language'])}</p>\n" \
    "<h2>Seiten, auf denen du warst</h2>\n<ol>\n#{pages.join("\n")}\n</ol>\n" \
    "<h2>Was dich vermutlich interessiert</h2>\n<ol>\n#{interests.join("\n")}\n</ol>\n"
end

server = TCPServer.new(8000)
puts "Der Tracker wartet auf Port 8000 ..."

loop do
  Thread.new(server.accept) do |client|
    method, target = client.gets.to_s.split(' ')
    # Die Header in einen Hash: »Name: Wert«
    headers = {}
    while (line = client.gets) && line != "\r\n"
      name, value = line.split(':', 2)
      headers[name.strip.downcase] = value.to_s.strip
    end
    path, query = target.to_s.split('?', 2)
    params = URI.decode_www_form(query.to_s).to_h
    puts "#{method} #{target}"
    puts "  Referer: #{headers['referer']}" if headers['referer']

    case path
    when '/'
      respond(client, '200 OK', 'text/html; charset=utf-8', headers_page(headers))
    when '/pixel.gif'
      visitor = parse_cookies(headers['cookie'].to_s)['visitor']
      # no-store: Der Browser soll das Bild jedes Mal neu holen
      extra = { 'Cache-Control' => 'no-store' }
      if visitor.to_s.empty?
        visitor = SecureRandom.hex(4)
        # Ein Jahr lang schickt der Browser die Kennung bei jeder Anfrage mit
        extra['Set-Cookie'] = "visitor=#{visitor}; Max-Age=31536000; Path=/"
      end
      log_visit(visitor, params['page'], params['keywords'])
      respond(client, '200 OK', 'image/gif', PIXEL, extra)
    when '/profil'
      visitor = parse_cookies(headers['cookie'].to_s)['visitor']
      lines = File.exist?(LOG) ? File.readlines(LOG, chomp: true) : []
      visits = visits_of(lines, visitor)
      counts = count_keywords(visits.map { |page, keywords| keywords })
      respond(client, '200 OK', 'text/html; charset=utf-8', profile_page(visitor, visits, counts, headers))
    else
      respond(client, '404 Not Found', 'text/plain; charset=utf-8', "Die Seite #{path} gibt es nicht.\n")
    end
  end
end
