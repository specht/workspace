require 'socket'
require 'digest'
require_relative 'websocket'

# Dieser String steht im Standard für WebSockets (RFC 6455), für alle gleich
MAGIC = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'

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

# Alle offenen Verbindungen. Mehrere Threads benutzen die Liste, darum
# ändert und benutzt sie immer nur einer auf einmal (Mutex). Unter demselben
# Lock wird auch geschickt: So mischen sich zwei Nachrichten nie.
$clients = []
$lock = Mutex.new

def broadcast(text)
  $lock.synchronize do
    $clients.each do |c|
      WebSocket.write_text(c, text) rescue nil   # wer weg ist, bekommt nichts
    end
  end
end

# Eine offene WebSocket-Verbindung: Was kommt, geht an alle
def connected(client)
  $lock.synchronize { $clients << client }
  broadcast("Jemand ist dazugekommen. Jetzt sind wir #{$clients.size}.")
  while (text = WebSocket.read_text(client))
    puts "Empfangen: #{text}"
    broadcast(text)
  end
  $lock.synchronize { $clients.delete(client) }
  client.close
  broadcast("Jemand ist gegangen. Jetzt sind wir #{$clients.size}.")
end

server = TCPServer.new(8000)
puts "Der Server wartet auf Port 8000 ..."

loop do
  Thread.new(server.accept) do |client|
    method, path, version = client.gets.to_s.split(' ')
    # Diesmal brauchen wir die Header: »Name: Wert« in einen Hash
    headers = {}
    while (line = client.gets) && line != "\r\n"
      name, value = line.split(':', 2)
      headers[name.strip.downcase] = value.to_s.strip
    end

    if path == '/ws' && headers['upgrade'].to_s.downcase == 'websocket'
      # Der Handshake: aus dem Schlüssel des Browsers die Antwort berechnen
      accept = Digest::SHA1.base64digest(headers['sec-websocket-key'] + MAGIC)
      client.print "HTTP/1.1 101 Switching Protocols\r\n"
      client.print "Upgrade: websocket\r\n"
      client.print "Connection: Upgrade\r\n"
      client.print "Sec-WebSocket-Accept: #{accept}\r\n"
      client.print "\r\n"
      connected(client)
    elsif path == '/'
      respond(client, '200 OK', 'text/html; charset=utf-8',
              File.read(File.join(__dir__, 'index.html')))
    else
      respond(client, '404 Not Found', 'text/plain; charset=utf-8', "Gibt es nicht.\n")
    end
  end
end
