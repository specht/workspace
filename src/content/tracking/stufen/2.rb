require 'socket'
require 'uri'
require 'cgi'

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

# Die Seite unter /: alle Header, die der Browser geschickt hat
def headers_page(headers)
  rows = headers.map { |name, value| "<tr><th>#{h(name)}</th><td>#{h(value)}</td></tr>" }
  "<!DOCTYPE html><meta charset='utf-8'><title>Was ich über dich weiß</title>\n" \
    "<h1>Was dieser Server über dich weiß</h1>\n<table>\n#{rows.join("\n")}\n</table>\n"
end

# Eine Zeile im Protokoll: wann, wer, welche Seite, worum es dort geht
def log_visit(visitor, page, keywords)
  line = [Time.now.strftime('%Y-%m-%d %H:%M:%S'), visitor, page, keywords].join("\t")
  $log_lock.synchronize { File.write(LOG, line + "\n", mode: 'a') }
  puts "  Besuch: #{line}"
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
      # no-store: Der Browser soll das Bild jedes Mal neu holen
      log_visit('-', params['page'], params['keywords'])
      respond(client, '200 OK', 'image/gif', PIXEL, 'Cache-Control' => 'no-store')
    else
      respond(client, '404 Not Found', 'text/plain; charset=utf-8', "Die Seite #{path} gibt es nicht.\n")
    end
  end
end
