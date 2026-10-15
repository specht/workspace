require 'socket'

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
    method, path, version = request_line.split(' ')
    puts "#{method} #{path}"
    while (line = client.gets) && line != "\r\n"
    end

    if path == '/time'
      respond(client, '200 OK', 'text/plain; charset=utf-8',
              "Es ist #{Time.now.strftime('%H:%M:%S')} Uhr.\n")
    else
      serve_file(client, path.to_s)
    end
  end
end
