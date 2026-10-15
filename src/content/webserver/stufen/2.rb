require 'socket'

server = TCPServer.new(8000)
puts "Der Server wartet auf Port 8000 ..."

loop do
  client = server.accept
  # Jede Verbindung bekommt einen eigenen Thread: So wartet der Server nicht
  # auf einen Besucher, während ein anderer schon etwas will.
  Thread.new(client) do |client|
    while (line = client.gets) && line != "\r\n"
      puts line
    end

    text = "Hallo aus meinem eigenen Server!\n"
    client.print "HTTP/1.1 200 OK\r\n"
    client.print "Content-Type: text/plain; charset=utf-8\r\n"
    client.print "Content-Length: #{text.bytesize}\r\n"
    client.print "Connection: close\r\n"
    client.print "\r\n"
    client.print text
    client.close
  end
end
