require 'faye/websocket'
require 'rackup'

# Das Programm hinter dem Server: Für jede Anfrage bekommt es env (Methode,
# Pfad, Header …) und gibt Status, Header und Inhalt zurück.
App = lambda do |env|
  if Faye::WebSocket.websocket?(env)
    # Der Browser möchte eine WebSocket-Verbindung. Faye erledigt den
    # Handshake und liest und schreibt die Nachrichten.
    ws = Faye::WebSocket.new(env)

    # Jede Nachricht geht mit »Echo:« davor zurück
    ws.on(:message) do |event|
      puts "Empfangen: #{event.data}"
      ws.send("Echo: #{event.data}")
    end

    ws.rack_response
  else
    # Jede andere Anfrage bekommt die Webseite
    [200, { 'content-type' => 'text/html; charset=utf-8' },
     [File.read(File.join(__dir__, 'index.html'))]]
  end
end

puts "Der Server wartet auf Port 8000 ..."
Rackup::Server.start(app: App, Host: '0.0.0.0', Port: 8000, server: 'puma', Silent: true)
