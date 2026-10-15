require 'faye/websocket'
require 'rackup'
require 'json'

# Das Bild: 64 × 40 Pixel, jedes eine Farbnummer von 0 bis 15
WIDTH = 64
HEIGHT = 40
$pixels = Array.new(WIDTH * HEIGHT, 0)

# Alle offenen Verbindungen
$clients = []

def broadcast(message)
  text = JSON.generate(message)
  $clients.each { |client| client.send(text) }
end

App = lambda do |env|
  if Faye::WebSocket.websocket?(env)
    ws = Faye::WebSocket.new(env)

    # Wer neu ist, bekommt das ganze Bild, danach nur noch einzelne Pixel
    ws.on(:open) do
      $clients << ws
      ws.send(JSON.generate({ 'type' => 'picture', 'width' => WIDTH,
                              'height' => HEIGHT, 'pixels' => $pixels }))
      broadcast({ 'type' => 'people', 'count' => $clients.size })
    end

    ws.on(:message) do |event|
      # Jeder kann alles schicken: nur glauben, was passt
      message = JSON.parse(event.data) rescue next
      next unless message.is_a?(Hash)
      x, y, color = message.values_at('x', 'y', 'color')
      next unless [x, y, color].all?(Integer)
      next unless x.between?(0, WIDTH - 1) && y.between?(0, HEIGHT - 1) && color.between?(0, 15)

      $pixels[y * WIDTH + x] = color
      broadcast({ 'type' => 'pixel', 'x' => x, 'y' => y, 'color' => color })
    end

    ws.on(:close) do
      $clients.delete(ws)
      broadcast({ 'type' => 'people', 'count' => $clients.size })
    end

    ws.rack_response
  else
    [200, { 'content-type' => 'text/html; charset=utf-8' },
     [File.read(File.join(__dir__, 'index.html'))]]
  end
end

puts "Der Server wartet auf Port 8000 ..."
Rackup::Server.start(app: App, Host: '0.0.0.0', Port: 8000, server: 'puma', Silent: true)
