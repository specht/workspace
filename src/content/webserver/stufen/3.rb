require 'socket'

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

server = TCPServer.new(8000)
puts "Der Server wartet auf Port 8000 ..."

loop do
  Thread.new(server.accept) do |client|
    # Die erste Zeile: Methode, Pfad, Version
    request_line = client.gets.to_s
    method, path, version = request_line.split(' ')
    puts "#{method} #{path}"
    # Die übrigen Zeilen (Header) brauchen wir noch nicht
    while (line = client.gets) && line != "\r\n"
    end

    case path
    when '/'
      respond(client, '200 OK', 'text/html; charset=utf-8',
              "<h1>Willkommen!</h1><p>Das ist mein eigener Webserver.</p>")
    when '/time'
      respond(client, '200 OK', 'text/plain; charset=utf-8',
              "Es ist #{Time.now.strftime('%H:%M:%S')} Uhr.\n")
    else
      respond(client, '404 Not Found', 'text/plain; charset=utf-8',
              "Die Seite #{path} gibt es nicht.\n")
    end
  end
end
