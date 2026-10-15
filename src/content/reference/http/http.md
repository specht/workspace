<div class='meta'>
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# HTTP

<p class='abstract'>
Wie Browser und Webserver miteinander reden: Anfrage und Antwort, Methoden, Statuscodes, Header und Adressen, wie man das alles mit <code>curl</code> und im Browser ansieht, wie Daten zum Server kommen, und was HTTPS und WebSockets daran ändern. Die Beispiele stammen aus <a href='/webserver'>Ein eigener Webserver</a>.
</p>

## Anfrage und Antwort

HTTP ist ein Protokoll aus Text. Der Browser (oder `curl`, oder ein Programm) öffnet eine TCP-Verbindung zum Server, meist auf Port 80, schickt eine **Anfrage** (Request) und bekommt eine **Antwort** (Response). Beide haben dieselbe Form: eine erste Zeile, Header, eine leere Zeile, dann vielleicht ein Inhalt (Body).

<!-- part: http-anfrage -->
<!-- short: Request-Line `GET /time HTTP/1.1`, dann Header wie `Host:`, dann eine leere Zeile. -->
### Die Anfrage

```text
GET /time HTTP/1.1
Host: localhost:8000
User-Agent: curl/8.5.0
Accept: */*

```

- Die erste Zeile ist die **Request-Line**: die Methode (`GET`), der Pfad (`/time`) und die Version des Protokolls.
- Danach folgen **Header**, einer pro Zeile, je ein Name, ein Doppelpunkt und ein Wert.
- Eine leere Zeile beendet die Header. Jede Zeile endet mit `\r\n`, die leere also nur aus `\r\n`.
- Bei `POST` folgt nach der leeren Zeile der Inhalt, zum Beispiel ausgefüllte Formularfelder oder JSON.
<!-- /part -->

<!-- part: http-antwort -->
<!-- short: Status-Line `HTTP/1.1 200 OK`, Header wie `Content-Type` und `Content-Length`, leere Zeile, Inhalt. -->
### Die Antwort

```text
HTTP/1.1 200 OK
Content-Type: text/plain; charset=utf-8
Content-Length: 21
Connection: close

Es ist 10:46:44 Uhr.
```

- Die erste Zeile ist die **Status-Line**: die Version, ein **Statuscode** (`200`) und ein kurzer Text dazu (`OK`).
- `Content-Type` sagt, was der Inhalt ist, damit der Browser weiß, ob er HTML zeigen, CSS anwenden oder ein Bild anzeigen soll.
- `Content-Length` ist die Länge des Inhalts in **Bytes**, nicht in Zeichen: Ein `ü` sind in UTF-8 zwei Bytes. Stimmt die Zahl nicht, wartet der Browser auf mehr oder schneidet ab.
- Nach der leeren Zeile kommt der Inhalt, hier ein Text, sonst eine Seite, ein Bild, JSON.
<!-- /part -->

Eine Webseite braucht meist viele Anfragen: eine für die Seite und je eine für jede CSS-Datei, jedes Skript und jedes Bild, das sie einbindet.

## Methoden und Statuscodes

<!-- part: http-methoden -->
<!-- short: `GET` holt, `POST` schickt Daten, `PUT`/`PATCH` ändern, `DELETE` löscht, `HEAD` holt nur die Header. -->
### Methoden

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Methode</th><th>Wofür</th></tr></thead>
<tbody>
<tr><td><code>GET</code></td><td>etwas holen: eine Seite, ein Bild, Daten. Ändert nichts auf dem Server; Werte stehen höchstens in der Adresse (<code>?name=Ada</code>).</td></tr>
<tr><td><code>POST</code></td><td>Daten schicken, die etwas bewirken: ein Formular, eine neue Nachricht, eine Anmeldung. Die Daten stehen im Inhalt der Anfrage.</td></tr>
<tr><td><code>PUT</code> · <code>PATCH</code></td><td>etwas ersetzen · einen Teil davon ändern</td></tr>
<tr><td><code>DELETE</code></td><td>etwas löschen</td></tr>
<tr><td><code>HEAD</code></td><td>wie <code>GET</code>, aber nur die Header der Antwort, ohne Inhalt; <code>curl -I</code> fragt so</td></tr>
</tbody>
</table>
</div>

Ein Browser schickt `GET`, wenn du eine Adresse eingibst oder einem Link folgst, und `POST`, wenn ein Formular mit `method='post'` abgeschickt wird. Die übrigen Methoden benutzen Programme, zum Beispiel ein Skript mit `fetch`.
<!-- /part -->

<!-- part: statuscodes -->
<!-- short: 2xx geklappt, 3xx woanders, 4xx Fehler der Anfrage (404 gibt es nicht), 5xx Fehler des Servers. -->
### Statuscodes

Die erste Ziffer sagt, wie es ausgegangen ist: **2** geklappt, **3** steht woanders, **4** die Anfrage war falsch, **5** der Server hatte ein Problem.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Code</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>101 Switching Protocols</code></td><td>Der Server wechselt das Protokoll, etwa zu WebSocket.</td></tr>
<tr><td><code>200 OK</code></td><td>Alles in Ordnung, hier ist der Inhalt.</td></tr>
<tr><td><code>201 Created</code> · <code>204 No Content</code></td><td>etwas angelegt · erledigt, ohne Inhalt in der Antwort</td></tr>
<tr><td><code>301 Moved Permanently</code> · <code>302 Found</code></td><td>Die Seite steht unter einer anderen Adresse, für immer · vorübergehend. Die neue Adresse steht im Header <code>Location</code>; der Browser folgt ihr von selbst.</td></tr>
<tr><td><code>303 See Other</code></td><td>Die Antwort auf ein <code>POST</code>: »Sieh unter <code>Location</code> nach.« Der Browser holt diese Adresse mit <code>GET</code>, und Neuladen schickt das Formular nicht noch einmal.</td></tr>
<tr><td><code>304 Not Modified</code></td><td>Die Kopie im Browser ist noch aktuell, nichts Neues zu schicken.</td></tr>
<tr><td><code>400 Bad Request</code></td><td>Die Anfrage ergibt keinen Sinn, etwa kaputtes JSON.</td></tr>
<tr><td><code>401 Unauthorized</code> · <code>403 Forbidden</code></td><td>erst anmelden · angemeldet, aber nicht erlaubt</td></tr>
<tr><td><code>404 Not Found</code></td><td>Unter diesem Pfad gibt es nichts.</td></tr>
<tr><td><code>405 Method Not Allowed</code></td><td>Den Pfad gibt es, aber nicht mit dieser Methode.</td></tr>
<tr><td><code>500 Internal Server Error</code></td><td>Das Programm auf dem Server ist abgestürzt; die Ursache steht im Log des Servers, nicht im Browser.</td></tr>
<tr><td><code>502 Bad Gateway</code> · <code>503 Service Unavailable</code></td><td>Ein Server davor erreicht das eigentliche Programm nicht · der Server ist gerade überlastet oder in Wartung.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Header

<!-- part: http-header -->
<!-- short: `Host`, `User-Agent`, `Accept`, `Content-Type`, `Content-Length`, `Location`, `Cookie` und `Set-Cookie`. -->
### Häufige Header

Header sind unabhängig von Groß- und Kleinschreibung: `content-type` ist dasselbe wie `Content-Type`.

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Header</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td><code>Host: example.org</code></td><td>Anfrage: An welchen Namen geht die Anfrage? Ein Server kann viele Websites unter einer IP-Adresse haben.</td></tr>
<tr><td><code>User-Agent: …</code></td><td>Anfrage: welches Programm fragt, etwa ein Browser oder <code>curl</code></td></tr>
<tr><td><code>Accept: text/html</code></td><td>Anfrage: was das Programm gern zurückbekäme</td></tr>
<tr><td><code>Content-Type: text/html; charset=utf-8</code></td><td>Was im Inhalt steht: <code>text/html</code>, <code>text/css</code>, <code>text/javascript</code>, <code>application/json</code>, <code>image/png</code> …; bei Text dazu die Kodierung</td></tr>
<tr><td><code>Content-Length: 196</code></td><td>Länge des Inhalts in Bytes</td></tr>
<tr><td><code>Location: /neu</code></td><td>Antwort: wohin der Browser bei 301, 302, 303 oder 201 gehen soll</td></tr>
<tr><td><code>Set-Cookie: sid=…</code> · <code>Cookie: sid=…</code></td><td>Die Antwort legt einen Cookie im Browser ab · der Browser schickt ihn bei jeder weiteren Anfrage an denselben Server mit. So erkennt ein Server, wer angemeldet ist.</td></tr>
<tr><td><code>Cache-Control: no-cache</code></td><td>wie lange der Browser eine Kopie aufheben darf</td></tr>
<tr><td><code>Connection: close</code></td><td>Nach dieser Antwort schließt der Server die Verbindung. Ohne den Header bleibt sie für weitere Anfragen offen.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

## Adressen

<!-- part: url -->
<!-- short: `http://host:port/pfad?name=wert#stelle`; Sonderzeichen werden als `%20` usw. kodiert. -->
### Teile einer Adresse

<div class='url-diagram'><div class='url-parts' role='img' aria-label='Die Adresse http://localhost:8000/hello?name=Ada%20Lovelace#gruss in Teilen: Schema http, Host localhost, Port 8000, Pfad /hello, Query name=Ada%20Lovelace, Fragment gruss.'><span class='url-part'>http<span class='url-label'>Schema</span></span><span class='url-sep'>://</span><span class='url-part url-low'>localhost<span class='url-label'>Host</span></span><span class='url-sep'>:</span><span class='url-part'>8000<span class='url-label'>Port</span></span><span class='url-part url-low'>/hello<span class='url-label'>Pfad</span></span><span class='url-sep'>?</span><span class='url-part'>name=Ada%20Lovelace<span class='url-label'>Query</span></span><span class='url-sep'>#</span><span class='url-part url-low'>gruss<span class='url-label'>Fragment</span></span></div></div>

- **Schema:** das Protokoll, `http` oder `https`.
- **Host:** welcher Rechner gefragt wird, ein Name oder eine IP-Adresse.
- **Port:** ohne Angabe 80 bei `http`, 443 bei `https`.
- **Pfad** und **Query** gehen in die Request-Line: `GET /hello?name=Ada%20Lovelace HTTP/1.1`. Die Query bringt Parameter mit, `name=wert`, mehrere getrennt durch `&`.
- **Fragment:** eine Stelle in der Seite. Es bleibt im Browser; der Server sieht es nie.

Leerzeichen, Umlaute und Zeichen wie `&` oder `?` haben in einer Adresse eine Bedeutung oder sind nicht erlaubt. Sie werden **kodiert**: `%` und der Wert des Bytes als Hexzahl, ein Leerzeichen ist `%20`, ein `ü` sind die zwei Bytes `%C3%BC`. In Ruby macht `URI.decode_www_form` daraus wieder Text, in JavaScript `new URLSearchParams(…)`.
<!-- /part -->

## Ansehen und ausprobieren

<!-- part: curl -->
<!-- short: `curl URL` zeigt den Inhalt, `-i` auch Status-Line und Header, `-I` nur die Header, `-v` alles, auch die Anfrage. -->
### curl

`curl` schickt eine Anfrage aus dem Terminal und zeigt die Antwort:

```bash
curl localhost:8000/time           # nur der Inhalt
curl -i localhost:8000/time        # mit Status-Line und Headern
curl -I localhost:8000/style.css   # nur die Header (Methode HEAD)
curl -v localhost:8000/time        # auch die Anfrage, mit > markiert
curl -L http://example.org/alt     # Weiterleitungen (301, 302) folgen
```

Daten mit `POST` schicken:

```bash
curl -d "name=Ada&klasse=10b" localhost:8000/anmelden            # wie ein Formular
curl --json '{"x": 3, "y": 2, "color": 5}' localhost:8000/pixel  # als JSON
curl -H "Accept: application/json" localhost:8000/api/time       # ein eigener Header
```

Liefert ein Server JSON, macht `jq` es lesbar: `curl -s https://api.github.com/users/octocat | jq`. Mehr dazu im Handbuch [JSON](/reference/json).
<!-- /part -->

<!-- part: netzwerk-tab -->
<!-- short: <kbd>F12</kbd> › »Network«: jede Anfrage einer Seite mit Methode, Status, Typ, Größe und Headern. -->
### Im Browser: der Reiter »Network«

<kbd>F12</kbd> öffnet die Entwicklerwerkzeuge des Browsers, der Reiter **Network** (Netzwerk) zeigt jede Anfrage, die eine Seite auslöst, mit Methode, Statuscode, Typ, Größe und Dauer. Ein Klick auf eine Zeile zeigt die Header von Anfrage und Antwort und den Inhalt. Lade die Seite neu, während der Reiter offen ist; sonst ist die Liste leer.

So findest du zum Beispiel ein Bild, das nicht lädt: Es steht rot mit `404` in der Liste, und der Pfad daneben zeigt, wo der Browser gesucht hat.
<!-- /part -->

## Daten schicken

<!-- part: http-formulare -->
<!-- short: `GET` hängt Formularfelder an die Adresse, `POST` schickt sie im Inhalt; JSON mit `Content-Type: application/json`. -->
### Formulare und JSON

Ein Formular schickt seine Felder als `name=wert`-Paare. Mit `method='get'` hängen sie an der Adresse (`/suche?q=Pendel`), mit `method='post'` stehen sie im Inhalt der Anfrage:

```text
POST /anmelden HTTP/1.1
Host: localhost:8000
Content-Type: application/x-www-form-urlencoded
Content-Length: 19

name=Ada&klasse=10b
```

Programme schicken Daten oft als JSON. Dann sagt der Header `Content-Type: application/json`, was im Inhalt steht. In JavaScript:

```js
const response = await fetch('/pixel', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ x: 3, y: 2, color: 5 }),
});
const data = await response.json();
```

Ein Server liest den Inhalt genau so lang, wie `Content-Length` sagt. Er darf keiner Anfrage trauen: Jeder kann mit `curl` beliebige Daten an jeden Pfad schicken, nicht nur das eigene Formular.
<!-- /part -->

<!-- part: https -->
<!-- short: HTTPS ist HTTP in einer verschlüsselten TLS-Verbindung, auf Port 443; das Zertifikat beweist, wer der Server ist. -->
### HTTPS

HTTPS ist dasselbe HTTP, aber in einer verschlüsselten Verbindung (TLS), meist auf Port 443. Wer unterwegs mitliest, sieht nur, mit welchem Server gesprochen wird, nicht den Pfad, die Header oder den Inhalt. Das **Zertifikat** des Servers beweist, dass er wirklich zu dem Namen gehört; der Browser zeigt sonst eine Warnung.

Im Workspace sprechen deine eigenen Server einfaches HTTP. Die weitergeleiteten Adressen aus dem Reiter »Ports« und die Shared Live Apps bekommen HTTPS vom Workspace davor.
<!-- /part -->

<!-- part: websocket-upgrade -->
<!-- short: Eine Anfrage mit `Upgrade: websocket`, die Antwort `101 Switching Protocols`, danach Frames in beide Richtungen. -->
### WebSockets

HTTP ist Frage und Antwort: Der Server kann nichts schicken, worum niemand gefragt hat. Ein **WebSocket** beginnt als HTTP-Anfrage mit `Upgrade: websocket` und `Sec-WebSocket-Key`; der Server antwortet mit `101 Switching Protocols` und `Sec-WebSocket-Accept`. Danach bleibt die Verbindung offen, und beide Seiten schicken sich Nachrichten in kleinen Frames, wann sie wollen. In JavaScript:

```js
const socket = new WebSocket('wss://' + location.host + '/ws');
socket.addEventListener('message', (event) => console.log(event.data));
socket.send('Hallo');
```

Wie der Handshake im Einzelnen geht, zeigt [Eine Pixelwand für alle](/pixelwand).
<!-- /part -->
