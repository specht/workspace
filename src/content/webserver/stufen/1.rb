require 'socket'

server = TCPServer.new(8000)
puts "Der Server wartet auf Port 8000 ..."

loop do
  client = server.accept          # wartet, bis jemand eine Verbindung aufbaut
  puts "--- Eine neue Verbindung"
  # Die Anfrage zeilenweise lesen, bis zur ersten leeren Zeile
  while (line = client.gets) && line != "\r\n"
    puts line
  end
  client.close
end
