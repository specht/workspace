require 'socket'
require 'uri'

# Welcher Content-Type zu welcher Endung gehört
TYPES = {
  '.html' => 'text/html; charset=utf-8',
  '.css'  => 'text/css; charset=utf-8',
  '.js'   => 'text/javascript; charset=utf-8',
  '.png'  => 'image/png',
  '.jpg'  => 'image/jpeg',
  '.txt'  => 'text/plain; charset=utf-8',
}

PUBLIC_DIR = File.expand_path('public', __dir__)

# Der Zähler gehört dem Server, nicht einer Verbindung: Alle Threads
# benutzen ihn, darum zählt immer nur einer auf einmal (Mutex).
$visits = 0
$visits_lock = Mutex.new

# Die Umfrage: was im Formular steht, wie es auf der Seite heißt, und die
# Stimmen dazu, ebenfalls geschützt durch einen Lock
OPTIONS = { 'pizza' => 'Pizza', 'pasta' => 'Nudeln', 'curry' => 'Curry', 'salad' => 'Salat' }
$votes = Hash.new(0)
$votes_lock = Mutex.new

# Schickt eine vollständige HTTP-Antwort und schließt die Verbindung
def respond(client, status, type, body)
  client.print "HTTP/1.1 #{status}\r\n"
  client.print "Content-Type: #{type}\r\n"
  client.print "Content-Length: #{body.bytesize}\r\n"
  client.print "Connection: close\r\n"
  client.print "\r\n"
  client.print body
  client.close
end

# Schickt den Browser weiter zu einer anderen Adresse: »Sieh dort nach«
def redirect(client, location)
  client.print "HTTP/1.1 303 See Other\r\n"
  client.print "Location: #{location}\r\n"
  client.print "Content-Length: 0\r\n"
  client.print "Connection: close\r\n"
  client.print "\r\n"
  client.close
end

# Eine Datei aus dem Ordner public, aber nie etwas außerhalb davon
def serve_file(client, path)
  path = '/index.html' if path == '/'
  file = File.expand_path('.' + path, PUBLIC_DIR)
  if file.start_with?(PUBLIC_DIR + '/') && File.file?(file)
    type = TYPES[File.extname(file)] || 'application/octet-stream'
    respond(client, '200 OK', type, File.binread(file))
  else
    respond(client, '404 Not Found', 'text/plain; charset=utf-8',
            "Die Seite #{path} gibt es nicht.\n")
  end
end

server = TCPServer.new(8000)
puts "Der Server wartet auf Port 8000 ..."

loop do
  Thread.new(server.accept) do |client|
    request_line = client.gets.to_s
    method, target, version = request_line.split(' ')
    puts "#{method} #{target}"
    # Die Header in einen Hash: »Name: Wert«
    headers = {}
    while (line = client.gets) && line != "\r\n"
      name, value = line.split(':', 2)
      headers[name.strip.downcase] = value.to_s.strip
    end

    # /hello?name=Ada: der Pfad und die Parameter nach dem Fragezeichen
    path, query = target.to_s.split('?', 2)
    params = URI.decode_www_form(query.to_s).to_h

    case path
    when '/time'
      respond(client, '200 OK', 'text/plain; charset=utf-8',
              "Es ist #{Time.now.strftime('%H:%M:%S')} Uhr.\n")
    when '/hello'
      name = params['name'] || 'Unbekannt'
      respond(client, '200 OK', 'text/plain; charset=utf-8', "Hallo, #{name}!\n")
    when '/visits'
      count = $visits_lock.synchronize { $visits += 1 }
      respond(client, '200 OK', 'text/plain; charset=utf-8',
              "Du bist Besuch Nummer #{count}.\n")
    when '/vote'
      if method != 'POST'
        respond(client, '405 Method Not Allowed', 'text/plain; charset=utf-8',
                "Abstimmen geht nur mit POST.\n")
        next
      end
      # Nach der leeren Zeile kommt der Inhalt, genau Content-Length Bytes
      body = client.read(headers['content-length'].to_i).to_s
      choice = URI.decode_www_form(body).to_h['choice']
      if OPTIONS.key?(choice)
        $votes_lock.synchronize { $votes[choice] += 1 }
        redirect(client, '/results')
      else
        respond(client, '400 Bad Request', 'text/plain; charset=utf-8',
                "Diese Antwort gibt es nicht.\n")
      end
    when '/results'
      rows = $votes_lock.synchronize do
        OPTIONS.map { |key, label| "<li>#{label}: #{$votes[key]}</li>" }.join("\n")
      end
      respond(client, '200 OK', 'text/html; charset=utf-8',
              "<!DOCTYPE html><meta charset='utf-8'><link rel='stylesheet' href='style.css'>" \
              "<h1>Ergebnis</h1><ul>#{rows}</ul><p><a href='poll.html'>Noch einmal abstimmen</a></p>\n")
    else
      serve_file(client, path)
    end
  end
end
