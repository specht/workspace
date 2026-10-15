<div class='meta'>
image: pixelwand.webp
kind: projekt
language: Ruby
topic: networks
duration: 2 Doppelstunden
requires: [webserver]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Eine Pixelwand für alle

<p class='abstract'>
Die ganze Klasse malt gleichzeitig an einem Bild: Jeder Pixel, den jemand setzt, erscheint sofort bei allen anderen. Dafür erweiterst du deinen Ruby-Webserver um WebSockets, Verbindungen, die offen bleiben und in beide Richtungen funktionieren. Du schreibst den Handshake selbst, schickst Nachrichten an alle und baust die Pixelwand mit einer kleinen Webseite.
</p>

## Warum WebSockets?

Bei HTTP fragt immer der Browser, und der Server antwortet. Ein Server kann nicht von sich aus etwas schicken. Für eine Pixelwand ist das ein Problem: Wenn jemand anderes malt, soll dein Browser das sofort erfahren. Er könnte jede halbe Sekunde fragen, ob es etwas Neues gibt. Bei 30 Leuten wären das 60 Anfragen pro Sekunde, und fast alle bekämen die Antwort »nichts Neues«.

Eine **WebSocket**-Verbindung beginnt wie eine HTTP-Anfrage, bleibt dann aber offen. Danach können beide Seiten jederzeit Nachrichten schicken, in beide Richtungen. So funktionieren Chats, Online-Spiele und gemeinsame Dokumente.

Du brauchst deinen Webserver aus [Ein eigener Webserver](/webserver) nicht, aber dieselben Ideen: `TCPServer`, Threads, die Funktion `respond`.

## Einrichten

Lege einen neuen Ordner an und öffne ihn mit <kbd>Strg</kbd><kbd>K</kbd> <kbd>Strg</kbd><kbd>O</kbd>:

```bash
mkdir /workspace/pixelwand
```

Nach dem Handshake reden Browser und Server in einem eigenen Format, in **Frames**: Jede Nachricht hat einen kleinen Header aus Bytes, der sagt, wie lang sie ist. Nachrichten vom Browser sind außerdem mit vier zufälligen Bytes »maskiert« (XOR). Verschlüsselt ist das nicht, denn die vier Bytes stehen im Frame mit dabei. Das Lesen und Schreiben dieser Frames übernimmt eine kleine Datei. Lege sie als `websocket.rb` an; du musst sie nicht im Einzelnen verstehen, aber es lohnt sich, die Kommentare zu lesen:

```ruby
# Nachrichten über eine WebSocket-Verbindung lesen und schicken.
#
# Nach dem Handshake reden Browser und Server nicht mehr in Textzeilen,
# sondern in »Frames«: Jede Nachricht hat einen kleinen Header aus Bytes, der
# sagt, was für eine Nachricht es ist und wie lang sie ist. Nachrichten vom
# Browser sind außerdem mit vier zufälligen Bytes »maskiert« (XOR), damit
# alte Proxys im Netz sie nicht für HTTP halten.

module WebSocket
  # Liest eine Textnachricht. Gibt nil zurück, wenn die Verbindung zu ist.
  def self.read_text(client)
    loop do
      head = client.read(2)
      return nil if head.nil? || head.bytesize < 2
      byte1, byte2 = head.bytes
      opcode = byte1 & 0x0f          # 1: Text, 8: Schluss, 9: Ping
      masked = (byte2 & 0x80) != 0
      length = byte2 & 0x7f
      if length == 126               # Länge steht in den nächsten 2 Bytes
        length = client.read(2).unpack1('n')
      elsif length == 127            # … oder in den nächsten 8 Bytes
        length = client.read(8).unpack1('Q>')
      end
      mask = masked ? client.read(4).bytes : [0, 0, 0, 0]
      data = client.read(length).to_s.bytes
      data = data.each_with_index.map { |b, i| b ^ mask[i % 4] }.pack('C*')

      case opcode
      when 1 then return data.force_encoding('UTF-8')
      when 8 then return nil                      # der Browser verabschiedet sich
      when 9 then write_frame(client, 10, data)   # auf Ping mit Pong antworten
      end
    end
  rescue IOError, SystemCallError
    nil
  end

  # Schickt eine Textnachricht
  def self.write_text(client, text)
    write_frame(client, 1, text.b)
  end

  def self.write_frame(client, opcode, data)
    head = [0x80 | opcode].pack('C')  # 0x80: das ist die ganze Nachricht
    if data.bytesize < 126
      head << [data.bytesize].pack('C')
    elsif data.bytesize < 65536
      head << [126, data.bytesize].pack('Cn')
    else
      head << [127, data.bytesize].pack('CQ>')
    end
    client.write(head + data)
  end
end
```

`WebSocket.read_text(client)` wartet auf die nächste Nachricht und gibt sie als Text zurück, oder `nil`, wenn die Verbindung zu ist. `WebSocket.write_text(client, text)` schickt eine Nachricht.

## Der Handshake

Der Browser öffnet eine WebSocket-Verbindung mit einer ganz normalen HTTP-Anfrage, mit zwei besonderen Headern: `Upgrade: websocket` sagt, dass er die Verbindung umwandeln möchte, und `Sec-WebSocket-Key` bringt einen zufälligen String mit. Der Server antwortet mit dem Statuscode **101 Switching Protocols** und beweist mit `Sec-WebSocket-Accept`, dass er WebSockets versteht: Er hängt an den Key einen festen String aus dem Standard an, berechnet davon den SHA-1-Hashwert und schickt ihn in Base64.

Lege `server.rb` an:

```ruby
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

# Eine offene WebSocket-Verbindung: Was kommt, geht zurück
def connected(client)
  while (text = WebSocket.read_text(client))
    puts "Empfangen: #{text}"
    WebSocket.write_text(client, "Echo: #{text}")
  end
  client.close
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
```

- Diesmal liest der Server die Header in einen Hash, `headers['upgrade']` zum Beispiel. HTTP-Header sind unabhängig von Groß- und Kleinschreibung, darum `downcase`.
- `Digest::SHA1.base64digest` berechnet den Hashwert und schreibt ihn gleich in Base64.
- Nach dem Handshake ruft der Server `connected` auf. Solange die Verbindung offen ist, wartet `WebSocket.read_text` auf die nächste Nachricht, und der Server schickt sie mit »Echo:« davor zurück.
- Alles andere bleibt HTTP: Auf `/` schickt er die Webseite `index.html`.

Die Webseite `index.html` hat ein Eingabefeld und zeigt alles, was vom Server kommt:

```html
<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="utf-8">
  <title>Nachrichten</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 32em; margin: 2em auto; padding: 0 1em; }
    #log { border: 1px solid #ccc; border-radius: 6px; padding: 0.5em 1em; min-height: 10em; }
    #log p { margin: 0.3em 0; }
    form { display: flex; gap: 0.5em; margin-top: 0.8em; }
    input { flex: 1; font: inherit; padding: 0.3em; }
  </style>
</head>
<body>
  <h1>Nachrichten</h1>
  <div id="log"></div>
  <form id="form">
    <input id="message" placeholder="Eine Nachricht …" autocomplete="off">
    <button>Senden</button>
  </form>

  <script>
    // Dieselbe Adresse wie die Seite, nur mit ws:// (oder wss:// bei https)
    const url = (location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host + '/ws';
    const socket = new WebSocket(url);
    const log = document.getElementById('log');

    function show(text) {
      const paragraph = document.createElement('p');
      paragraph.textContent = text;
      log.append(paragraph);
    }

    socket.addEventListener('open', () => show('Verbunden.'));
    socket.addEventListener('close', () => show('Die Verbindung ist zu.'));
    // Jede Nachricht vom Server landet hier, ohne dass die Seite fragt
    socket.addEventListener('message', (event) => show(event.data));

    document.getElementById('form').addEventListener('submit', (event) => {
      event.preventDefault();          // die Seite nicht neu laden
      const input = document.getElementById('message');
      if (input.value !== '') socket.send(input.value);
      input.value = '';
    });
  </script>
</body>
</html>
```

`new WebSocket(url)` öffnet die Verbindung, mit `ws://` statt `http://`. `addEventListener('message', …)` ruft eine Funktion auf, sobald eine Nachricht kommt, ohne dass die Seite fragt. `socket.send(…)` schickt eine Nachricht.

Starte den Server und öffne daneben mit <kbd>Strg</kbd><kbd>Shift</kbd><kbd>5</kbd> ein zweites Terminal, wie beim Webserver:

```bash
ruby server.rb
```

<!-- tutorial-screenshot
# Ordner pixelwand mit websocket.rb, server.rb (Echo) und index.html
write-file: pixelwand/websocket.rb <- file:stufen/websocket.rb
write-file: pixelwand/server.rb <- file:stufen/server1.rb
write-file: pixelwand/index.html <- file:stufen/index1.html
press: Control+K
press: Control+O
wait-for-input-value: /workspace/
type: pixelwand
press: Enter
wait-for-input-value: /workspace/pixelwand/
press: Enter
wait-for-text: server.rb
open-file: server.rb
terminal-open
terminal-run: ruby server.rb
wait-for-text: Der Server wartet
sleep: 1
-->
<img class='full' src='ordner.webp' alt='Der Ordner pixelwand mit index.html, server.rb und websocket.rb; server.rb ist offen, im Terminal wartet der Server auf Port 8000.'>

Den Handshake kannst du im zweiten Terminal selbst ansehen. `curl` schickt hier die Header, die sonst der Browser schickt, mit dem Beispiel-Key aus dem Standard:

```bash
curl -i --http1.1 -m 2 -H "Connection: Upgrade" -H "Upgrade: websocket" -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" localhost:8000/ws
```

<!-- tutorial-screenshot
# Der Handshake mit curl, im zweiten Terminal
open-file: server.rb
hide-left-sidebar
terminal-open
terminal-maximize
press: Control+Shift+5
sleep: 1
terminal-run: curl -i --http1.1 -m 2 -H "Connection: Upgrade" -H "Upgrade: websocket" -H "Sec-WebSocket-Version: 13" -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" localhost:8000/ws
sleep: 3
-->
<img class='full' src='handschlag.webp' alt='Rechts antwortet der Server auf die Anfrage von curl mit HTTP/1.1 101 Switching Protocols und Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=; nach zwei Sekunden gibt curl auf.'>

Der Server antwortet mit `101 Switching Protocols` und `Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=`, genau dem Wert, den der Standard für diesen Schlüssel als Beispiel nennt. Danach bleibt die Verbindung offen; `curl` versteht keine Frames und gibt nach zwei Sekunden auf (`-m 2`).

Öffne jetzt die Adresse von Port 8000 aus dem Reiter »Ports« (»Forwarded Address«) im Browser und schreib eine Nachricht:

<!-- tutorial-screenshot
preview-port: 8000
tab: preview
wait-for-text: Verbunden.
click: selector:#message
type: Hallo, Server!
press: Enter
wait-for-text: Echo: Hallo, Server!
sleep: 1
-->
<img class='full border' src='echo.webp' alt='Die Seite »Nachrichten«: Verbunden, dann »Echo: Hallo, Server!« als Antwort auf die eigene Nachricht.'>

_task(handshake)

_task(frame-header)

## An alle

Ein Echo ist noch langweilig. Spannend wird es, wenn eine Nachricht bei **allen** ankommt, die gerade verbunden sind. Dafür merkt sich der Server alle offenen Verbindungen in einer Liste `$clients`. Ersetze `connected` durch:

```ruby
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
```

- Wer sich verbindet, kommt in die Liste; wer geht, fliegt wieder raus.
- `broadcast` schickt eine Nachricht an jede Verbindung in der Liste. `rescue nil` sorgt dafür, dass eine Verbindung, die gerade abgerissen ist, die anderen nicht stört.
- Mehrere Threads benutzen die Liste gleichzeitig, darum gehört sie zu einem Lock, `$lock`, wie der Zähler beim Webserver. Auch gesendet wird unter diesem Lock: Schickten zwei Threads gleichzeitig an denselben Browser, könnten sich die Bytes ihrer beiden Nachrichten mischen.

Starte den Server neu und öffne die Seite in zwei Tabs nebeneinander. Was du in einem Tab schreibst, erscheint in beiden.

_task(wer-bekommt-was)

## Die Pixelwand

Jetzt wird aus dem Chat ein Bild. Der Server bewahrt das Bild auf: 64 × 40 Pixel, jedes eine Farbnummer von 0 bis 15. Wer neu dazukommt, bekommt das ganze Bild, danach nur noch die Pixel, die sich ändern. Die Nachrichten sind jetzt keine einfachen Texte mehr, sondern **JSON**, eine Schreibweise für Daten, die Ruby und JavaScript beide verstehen: `{"x":10,"y":5,"color":4}`.

Ergänze oben `require 'json'` und ersetze `connected` und `broadcast` durch:

```ruby
# Das Bild: 64 × 40 Pixel, jedes eine Farbnummer von 0 bis 15
WIDTH = 64
HEIGHT = 40
$pixels = Array.new(WIDTH * HEIGHT, 0)

# Alle offenen Verbindungen. Mehrere Threads benutzen die Liste und das Bild,
# darum ändert und benutzt sie immer nur einer auf einmal (Mutex). Unter
# demselben Lock wird auch geschickt: So mischen sich zwei Nachrichten nie.
$clients = []
$lock = Mutex.new

def broadcast(message)
  text = JSON.generate(message)
  $clients.each do |c|
    WebSocket.write_text(c, text) rescue nil   # wer weg ist, bekommt nichts
  end
end

# Eine offene WebSocket-Verbindung: Wer neu ist, bekommt das ganze Bild,
# danach nur noch einzelne Pixel
def connected(client)
  $lock.synchronize do
    $clients << client
    WebSocket.write_text(client, JSON.generate({ 'type' => 'picture', 'width' => WIDTH,
                                                 'height' => HEIGHT, 'pixels' => $pixels }))
    broadcast({ 'type' => 'people', 'count' => $clients.size })
  end

  while (text = WebSocket.read_text(client))
    # Jeder kann alles schicken: nur glauben, was passt
    message = JSON.parse(text) rescue next
    next unless message.is_a?(Hash)
    x, y, color = message.values_at('x', 'y', 'color')
    next unless [x, y, color].all?(Integer)
    next unless x.between?(0, WIDTH - 1) && y.between?(0, HEIGHT - 1) && color.between?(0, 15)

    $lock.synchronize do
      $pixels[y * WIDTH + x] = color
      broadcast({ 'type' => 'pixel', 'x' => x, 'y' => y, 'color' => color })
    end
  end

  $lock.synchronize do
    $clients.delete(client)
    broadcast({ 'type' => 'people', 'count' => $clients.size })
  end
  client.close
end
```

- `JSON.generate` macht aus einem Hash Text, `JSON.parse` aus Text wieder einen Hash.
- Jede Nachricht hat einen `type`: `picture` für das ganze Bild, `pixel` für einen einzelnen Pixel, `people` für die Zahl der Verbundenen.
- **Jeder kann alles schicken**, nicht nur deine Webseite. Darum glaubt der Server keiner Nachricht: Kein JSON, keine ganzen Zahlen, ein Pixel außerhalb des Bildes, eine Farbe über 15? `next` überspringt die Nachricht. Ein Server, der fremden Eingaben traut, ist eine Einladung, ihn kaputt zu machen.

Die neue `index.html` zeichnet das Bild in ein `<canvas>`, ein Element, in das JavaScript zeichnen kann. Es ist nur 64 × 40 Pixel groß, CSS zieht es auf das Zehnfache:

```html
<!DOCTYPE html>
<html lang="de">
<head>
  <meta charset="utf-8">
  <title>Pixelwand</title>
  <style>
    body { font-family: system-ui, sans-serif; margin: 1.5em; background: #0e0f17; color: #f4f4f4; }
    #wall { width: 640px; height: 400px; image-rendering: pixelated; cursor: crosshair;
            outline: 2px solid #333c57; border-radius: 2px; touch-action: none; }
    #colors { display: flex; gap: 4px; margin-top: 0.8em; }
    #colors button { width: 32px; height: 32px; border: 2px solid transparent; border-radius: 4px; cursor: pointer; }
    #colors button.active { border-color: #f4f4f4; }
  </style>
</head>
<body>
  <h1>Pixelwand</h1>
  <p id="people">Verbinde …</p>
  <canvas id="wall" width="64" height="40"></canvas>
  <div id="colors"></div>

  <script>
    // Die Palette »Sweetie 16«, wie bei PixelRAM
    const PALETTE = ['#1a1c2c', '#5d275d', '#b13e53', '#ef7d57', '#ffcd75', '#a7f070', '#38b764', '#257179',
                     '#29366f', '#3b5dc9', '#41a6f6', '#73eff7', '#f4f4f4', '#94b0c2', '#566c86', '#333c57'];

    const wall = document.getElementById('wall');
    const pen = wall.getContext('2d');
    let color = 4;

    function setPixel(x, y, c) {
      pen.fillStyle = PALETTE[c];
      pen.fillRect(x, y, 1, 1);
    }

    // Die Knöpfe für die Farben
    PALETTE.forEach((value, c) => {
      const button = document.createElement('button');
      button.style.background = value;
      button.title = 'Farbe ' + c;
      if (c === color) button.classList.add('active');
      button.addEventListener('click', () => {
        document.querySelector('#colors .active').classList.remove('active');
        button.classList.add('active');
        color = c;
      });
      document.getElementById('colors').append(button);
    });

    const url = (location.protocol === 'https:' ? 'wss://' : 'ws://') + location.host + '/ws';
    const socket = new WebSocket(url);

    socket.addEventListener('message', (event) => {
      const message = JSON.parse(event.data);
      if (message.type === 'picture') {
        // Das ganze Bild, gleich nach dem Verbinden
        message.pixels.forEach((c, i) => setPixel(i % message.width, Math.floor(i / message.width), c));
      } else if (message.type === 'pixel') {
        setPixel(message.x, message.y, message.color);
      } else if (message.type === 'people') {
        document.getElementById('people').textContent =
          message.count === 1 ? 'Du malst allein.' : message.count + ' Leute malen gerade mit.';
      }
    });
    socket.addEventListener('close', () => {
      document.getElementById('people').textContent = 'Die Verbindung ist zu. Lade die Seite neu.';
    });

    // Malen: Mausposition in Pixel umrechnen und dem Server schicken.
    // Gezeichnet wird erst, wenn der Server den Pixel an alle schickt.
    function sendPixel(x, y) {
      if (x >= 0 && x < wall.width && y >= 0 && y < wall.height) {
        socket.send(JSON.stringify({ x: x, y: y, color: color }));
      }
    }

    function pixelAt(event) {
      const rect = wall.getBoundingClientRect();
      return [Math.floor((event.clientX - rect.left) / rect.width * wall.width),
              Math.floor((event.clientY - rect.top) / rect.height * wall.height)];
    }

    // Die Maus meldet sich nicht bei jedem Pixel: Von der letzten Stelle bis
    // zur neuen jeden Pixel dazwischen schicken, sonst hat die Linie Lücken
    let last = null;
    function paint(event) {
      const [x, y] = pixelAt(event);
      const [x0, y0] = last || [x, y];
      const steps = Math.max(Math.abs(x - x0), Math.abs(y - y0), 1);
      for (let i = 1; i <= steps; i++) {
        sendPixel(Math.round(x0 + (x - x0) * i / steps), Math.round(y0 + (y - y0) * i / steps));
      }
      last = [x, y];
    }
    wall.addEventListener('pointerdown', (e) => { last = pixelAt(e); sendPixel(...last); });
    wall.addEventListener('pointermove', (e) => { if (last) paint(e); });
    window.addEventListener('pointerup', () => { last = null; });
  </script>
</body>
</html>
```

- `setPixel(x, y, c)` malt ein Pixel in der Farbe Nummer `c`. Die 16 Farben sind die Palette »Sweetie 16«, wie bei [PixelRAM](/pixelram).
- Malt jemand, schickt die Seite den Pixel nur an den Server. Gezeichnet wird er erst, wenn der Server ihn an alle zurückschickt, auch an dich selbst. So sehen alle immer dasselbe Bild.
- Bewegt sich die Maus schnell, meldet der Browser nicht jeden Pixel. `paint` füllt die Pixel zwischen der letzten und der neuen Stelle auf, sonst hätten Linien Lücken.

Starte den Server neu und lade die Seite. Das Bild ist noch leer; male etwas, und lade die Seite dann neu: Dein Bild ist noch da, denn der Server hat es.

Prüfe im zweiten Terminal, ob der Server die neue Seite schickt:

```bash
curl -s localhost:8000/ | head -n 5
```

<!-- tutorial-screenshot
# Stufe 3: Neustart mit der Pixelwand, curl holt die Seite
write-file: pixelwand/server.rb <- file:stufen/server3.rb
write-file: pixelwand/index.html <- file:stufen/index3.html
terminal-open
terminal-maximize
press: Alt+ArrowLeft
sleep: 1
press: Control+C
sleep: 1
press: Control+L
type: ruby server.rb
press: Enter
sleep: 2
terminal-run: clear
sleep: 1
terminal-run: curl -s localhost:8000/ | head -n 5
sleep: 2
-->
<img class='full' src='neustart.webp' alt='Links wartet der neu gestartete Server, rechts zeigt curl die ersten Zeilen der neuen Seite mit dem Titel »Pixelwand«.'>

Lade die Seite im Browser neu und male. Hier malt gerade jemand anderes:

<!-- tutorial-screenshot
preview-reload
tab: preview
wait-for-text: Du malst allein
hook: paint
-->
<img class='full border' src='pixelwand.webp' alt='Die Pixelwand: blauer Himmel mit gelber Sonne, eine grüne Wiese, ein Haus mit rotem Dach und ein Baum. Darüber steht »Du malst allein.«'>

_task(pixel-zahl)

_task(vertrauen)

_task(pixelwand)

## Mit der ganzen Klasse

Jetzt kommt der beste Teil. Gib in deinem [Profil](/profil) unter »Shared Live Apps« Port 8000 frei und sag deiner Klasse die Adresse. Alle, die sie öffnen, malen an **deinem** Bild, und jeder sieht sofort, was die anderen malen. Oben steht, wie viele gerade mitmalen.

Vielleicht merkt ihr schnell, dass einige wenige das ganze Bild übermalen. Ein paar Ideen, die Pixelwand weiterzubauen, von leicht bis schwer:

- **Eine größere Wand:** Ändere `WIDTH` und `HEIGHT` im Server und die Größe des `<canvas>` in der Seite.
- **Wartezeit:** Jede Verbindung darf nur einen Pixel pro Sekunde setzen, wie bei »r/place«. Merke dir in `connected`, wann die Verbindung zuletzt gemalt hat (`Time.now`), und überspringe Pixel, die zu früh kommen.
- **Das Bild speichern:** Schreib `$pixels` alle paar Sekunden als JSON in eine Datei und lies es beim Start wieder ein. Dann übersteht das Bild einen Neustart des Servers.
- **Ein Chat daneben:** Ein neuer `type` für Textnachrichten und ein Eingabefeld unter der Wand.
- **Ein Spiel:** Zwei Teams mit zwei Farben, wer nach fünf Minuten mehr Pixel hat, gewinnt.

_task(abgabe)

## Wie geht es weiter?

Eure Pixelwand funktioniert genauso wie große Anwendungen, nur kleiner: Ein Server hält den gemeinsamen Zustand, jeder Browser hat eine offene Verbindung, und jede Änderung geht an alle. So arbeiten Online-Spiele, Chats und Programme, in denen mehrere Leute gleichzeitig an einem Dokument schreiben.

Im Ernstfall läuft eine WebSocket-Verbindung verschlüsselt über `wss://`, und der Server muss mit Tausenden Verbindungen und mit Leuten umgehen können, die ihm absichtlich falsche Daten schicken. Die Prüfung in `connected` war ein erster Schritt dahin.
