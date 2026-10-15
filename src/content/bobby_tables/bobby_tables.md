<div class='meta'>
image: xkcd-327.webp:0:50
kind: labor
language: Ruby / SQL
duration: 1 Doppelstunde
requires: [mysql]
reference: [sql]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# SQL Injection

<p class='abstract'>
Viele Programme bauen aus dem, was jemand in ein Formular tippt, eine SQL-Abfrage. Geschieht das unvorsichtig, kann eine geschickte Eingabe die Abfrage selbst verändern. In diesem Labor schreibst du ein kleines Anmeldeprogramm, findest seine Lücke in deiner eigenen Testdatenbank und schließt sie mit einem Prepared Statement.
</p>

<div class='hint'>
Das Eindringen in fremde Computersysteme und das Ausspähen von Daten ist strafbar. Probiere alles auf dieser Seite nur an deinem eigenen Programm und deiner eigenen Testdatenbank aus. Mehr dazu steht am <a href='#5-wichtiger-hinweis'>Ende der Seite</a>.
</div>

## Eine Testdatenbank

Für dieses Labor bekommst du eine eigene, leere Datenbank, die du danach wieder löschen kannst. Öffne dein [Profil](/profil) und klicke unter MySQL auf »Weitere Datenbank erstellen«. Sie heißt zum Beispiel `db_1234`; ersetze diesen Namen auf dieser Seite immer durch den Namen deiner Datenbank.

Unter der neuen Datenbank stehen »Zugangsdaten nur für diese Datenbank«: ein Benutzername, der genauso heißt wie die Datenbank, und ein Passwort. Mit diesem Zugang erreicht ein Programm nur diese eine Datenbank. Was dort schiefgeht, bleibt dort, und genau das wollen wir für ein Programm, das gleich eine Lücke haben wird.

Öffne ein Terminal (<kbd>Strg</kbd><kbd>J</kbd>), lege einen Ordner an und lade die Testdaten herunter:

```bash_copy
mkdir ~/bobby_tables
cd ~/bobby_tables
wget https://github.com/specht/workspace-files/raw/main/users.sql
```

Importiere sie in deine neue Datenbank:

```bash
mycli db_1234 < users.sql
```

Starte dann `mycli db_1234` und prüfe mit `SHOW TABLES;` und `SELECT * FROM users;`, ob die Daten angekommen sind:

<div style="max-width: 100%; overflow-x: auto;">
<table class='table table-sm'>
<thead>
<tr>
<th>login</th>
<th>password</th>
<th>address</th>
</tr>
</thead>
<tbody>
<tr>
<td>admin</td>
<td>YLrelnmPDPB-ZhQ1GzRxiEf</td>
<td>2683 John Calvin Drive, Chicago IL 60603</td>
</tr>
<tr>
<td>alice</td>
<td>asdf</td>
<td>578 Gordon Street, Claremont CA 91711</td>
</tr>
<tr>
<td>bob</td>
<td>1234</td>
<td>525 Cambridge Drive, Phoenix AZ 85039</td>
</tr>
<tr>
<td>mallory</td>
<td>hunter2</td>
<td>3747 Haven Lane, Lansing MI 48933</td>
</tr>
</tbody>
</table>
</div>

<div class='hint'>
Die Passwörter stehen hier im Klartext, damit du siehst, was passiert. Ein echter Server speichert nie das Passwort selbst, sondern einen Hash mit Salz; wie das geht, steht in <a href='/crypto'>Kryptographie im Terminal</a>.
</div>

## Ein Anmeldeprogramm

Das Programm braucht die Ruby-Bibliothek `mysql2`. Installiere sie einmal:

```bash
gem install mysql2
```

Lege im Ordner `bobby_tables` die Datei `login.rb` an und füge diesen Code ein. Trage oben den Namen deiner Datenbank und ihr Passwort aus dem Profil ein:

_include_file(login.rb, ruby)

Hinweise:

- `$stdin.noecho(&:gets)` liest das Passwort, ohne es auf dem Bildschirm zu zeigen.
- Mit <code>#&#8288;{ … }</code> wird der Wert einer Variable in einen String eingebettet (Z. 22 und 23), wie in Python mit f-Strings. Man nennt das [String Interpolation](https://en.wikipedia.org/wiki/String_interpolation).
- Der Backslash am Ende von Z. 21 und 22 setzt den String in der nächsten Zeile fort: Es ist ein einziger String ohne Zeilenumbruch, nur zur besseren Lesbarkeit auf drei Zeilen verteilt.

<div class='hint'>
Für dieses Labor steht das Passwort im Quelltext. In einem echten Programm gehört es nicht dorthin, sondern zum Beispiel in eine Umgebungsvariable: So machen es <code>MYSQL_HOST</code>, <code>MYSQL_USER</code> und <code>MYSQL_PASSWORD</code> für deine Hauptdatenbank.
</div>

Starte das Programm mit `ruby login.rb` und melde dich als `alice` mit dem Passwort `asdf` an, dann mit einem falschen Passwort.

## Experimentieren

Notiere dir die SQL-Abfragen, die in den folgenden Aufgaben entstehen; du brauchst sie später noch.

_task(zweck)

_task(abfrage)

_uses(select, where)

Wie in vielen anderen Sprachen können in SQL Kommentare stehen: Mit `#` oder `--` beginnt ein Kommentar, der bis zum Ende der Zeile gilt und nicht ausgeführt wird.

Das Programm setzt die Eingabe unverändert in die SQL-Abfrage ein. Damit kann, wer etwas eintippt, die Abfrage selbst verändern.

_task(admin)

_task(union)

_task(passwort-auslesen)

## Die Lücke schließen

Der Angriff, den du ausprobiert hast, heißt **SQL Injection**: Eigener SQL-Code wird in eine Abfrage eingeschleust und bringt das Programm dazu, etwas anderes zu tun als vorgesehen. Das Problem liegt im Programm, das Eingaben ungeprüft in den SQL-Code mischt, und dort lässt es sich auch beheben.

Statt die Eingabe per String Interpolation einzubauen, benutzt man ein **Prepared Statement**: Die Abfrage wird mit Platzhaltern `?` vorbereitet und erst danach mit den Eingaben ausgeführt. Die Datenbank bekommt den SQL-Code und die Werte getrennt; was in einem Wert steht, wird nie zu SQL, egal welche Zeichen darin vorkommen.

Kommentiere die Zeilen 21 bis 23 aus und füge dahinter ein:

```ruby
query = CLIENT.prepare("SELECT login FROM users WHERE login = ? AND password = ? LIMIT 1;")
rows = query.execute(login, password).to_a
```

_uses(platzhalter)

<div class='hint'>
Zeilen kommentierst du in Ruby mit <code>#</code> am Anfang aus. Schneller geht es, wenn du sie markierst und <kbd>Strg</kbd><kbd>#</kbd> drückst (mit US-Tastaturlayout <kbd>Ctrl</kbd><kbd>/</kbd>).
</div>

_task(prepared)

_task(behoben)

_task(warum-prepared)

_task(eigener-zugang)

Wenn du fertig bist, kannst du die Testdatenbank im [Profil](/profil) wieder löschen.

## Wichtiger Hinweis

Das Eindringen in fremde Computersysteme und das Ausspähen von Daten ist illegal und wird bestraft. Die Beispiele hier zeigen eine Sicherheitslücke, damit du sie in deinen eigenen Programmen erkennst und vermeidest. Probiere sie nur an Systemen aus, die du selbst betreibst oder für die du eine ausdrückliche Erlaubnis hast.

Ein Vergleich zum Schluss: Es ist in Ordnung, mit einem Lockpicking-Set dein eigenes Schloss zu öffnen, weil du den Schlüssel verloren hast. Öffnest du fremde Schlösser ohne Erlaubnis, begehst du eine Straftat.

<img class='full' src='xkcd-327-full.webp' alt='Die Schule fragt eine Mutter, ob ihr Sohn „Robert’); DROP TABLE Students;--“ heißt; sie bestätigt „Little Bobby Tables“ und mahnt, Datenbankeingaben zu bereinigen.'>

<em>Cartoon von Randall Munroe, [xkcd.com](https://xkcd.com/327/).</em>
