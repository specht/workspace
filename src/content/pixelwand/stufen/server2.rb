require 'faye/websocket'
require 'rackup'

# Alle offenen Verbindungen. Faye ruft die on-Blöcke aller Verbindungen
# nacheinander auf, nie zwei gleichzeitig: Die Liste braucht keinen Lock.
$clients = []

def broadcast(text)
  $clients.each { |client| client.send(text) }
end

App = lambda do |env|
  if Faye::WebSocket.websocket?(env)
    ws = Faye::WebSocket.new(env)

    ws.on(:open) do
      $clients << ws
      broadcast("Jemand ist dazugekommen. Jetzt sind wir #{$clients.size}.")
    end

    # Was kommt, geht an alle
    ws.on(:message) do |event|
      puts "Empfangen: #{event.data}"
      broadcast(event.data)
    end

    ws.on(:close) do
      $clients.delete(ws)
      broadcast("Jemand ist gegangen. Jetzt sind wir #{$clients.size}.")
    end

    ws.rack_response
  else
    [200, { 'content-type' => 'text/html; charset=utf-8' },
     [File.read(File.join(__dir__, 'index.html'))]]
  end
end

puts "Der Server wartet auf Port 8000 ..."
Rackup::Server.start(app: App, Host: '0.0.0.0', Port: 8000, server: 'puma', Silent: true)
