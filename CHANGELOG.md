# Changelog

Alle nennenswerten Änderungen am Hackschule Workspace stehen hier. Andere
Schulen mit eigener Installation folgen getaggten Releases statt `main`.

Versionen folgen [Semantic Versioning](https://semver.org/lang/de/):

- **Major** (`2.0.0`): Eine bestehende Installation braucht einen Handgriff
  beim Update, zum Beispiel neue Pflichtwerte in der Konfiguration oder eine
  Datenmigration, die nicht von selbst läuft.
- **Minor** (`1.1.0`): Neue Funktionen und Inhalte; Update ohne Handgriff.
  Neue Funktionen sind zunächst abgeschaltet.
- **Patch** (`1.0.1`): Fehlerkorrekturen.

Jeder Eintrag sagt, was sich für Lernende, Lehrkräfte und Betreibende ändert.

## Unreleased

### Hinzugefügt

- Neue Aktivität »DNS: Wie aus einem Namen eine Adresse wird« unter »Netze &
  Sicherheit«, nach TCP/IP: Lernende fragen mit `dig` nach Adressen und
  anderen Einträgen, gehen den Weg einer Frage von den Root-Servern über die
  DENIC bis zum zuständigen Nameserver selbst ab, sehen die TTL im
  Zwischenspeicher und was eine gefälschte Antwort anrichtet. Zehn Aufgaben,
  mit Notiz für Lehrkräfte. Antworten aus dem Netz prüft die neue Auswertung
  `dns_answers` von `measure` live im Workspace.
- Neue Aktivität »Indizes: Warum eine Abfrage schnell ist« unter
  »Datenbanken«: Lernende legen in ihrer eigenen Datenbank ein Telefonbuch mit
  einer Million Einträgen an, messen eine Suche, sehen mit `EXPLAIN` den Full
  Table Scan, legen einen Index an, lernen den B-Baum kennen und finden
  heraus, wann ein Index nicht hilft und was er kostet. Elf Aufgaben, mit
  Notiz für Lehrkräfte.
- Die Prüfart `schema` kennt `indexes`: ein Index der Tabelle, der mit den
  verlangten Spalten anfängt.
- »Ein Krimi im Terminal«, mit Fallserver: ein Abschnitt »Die Kameras«. Auf
  `museum.bitstein` liegen die Bilder dreier Überwachungskameras aus der
  Tatnacht. Lernende holen sie mit `scp` und einem `*` in ihren Workspace,
  wählen mit einem Muster wie `cam3_20260314_2338*` die Bilder einer Minute
  aus und sehen sie in VS Code an: Kamera 3 zeigt, welches der vier gelben
  Autos es war. Zwei neue Aufgaben. Damit alles zusammenpasst, sieht die
  Zeugin das Auto jetzt kurz nach halb zwölf, und der Nachtwächter sieht den
  Täter aus dem Fenster. Wer den Fallserver betreibt, baut `case_museum` neu.
- »Einführung in SQL«, Teil 2: ein Abschnitt »Wie weit ist es?«. Mit
  `ST_Distance_Sphere` und `CROSS JOIN` rechnen Lernende Entfernungen
  zwischen Städten aus, finden die fünf nächsten Städte zu Hamburg und die
  Stadt, die am weitesten von Berlin entfernt ist (erst vermuten, dann
  abfragen). Ein kurzer Teil erklärt, wann sich ein `SPATIAL INDEX` lohnt.
  Im Handbuch SQL neu: »Entfernungen auf der Erde«.
- Unter offenen Aufgaben (Reflexionen) sehen Lehrkräfte auf der Seite einen
  Kasten »Für Lehrkräfte«: Beispielantworten, die Antworten der aktuellen
  Gruppe mit Namen und Datum, oder nichts. Zu Beginn ist der Kasten zu; die
  Wahl gilt für alle Aufgaben und bleibt am Konto gespeichert. Wer mehrere
  Gruppen betreut, wählt die Gruppe im Kasten. Alle 15 Reflexionen haben je zwei Beispielantworten
  (neues Feld `examples` in `tasks.yaml`, nur für `reflection`). Lernende
  sehen davon nichts.
- Die Tabellen eines Datensatzes als Schema, eine Zeile pro Tabelle:
  `movie_genre(↑movie_id, ↑genre_id)`, der Primärschlüssel unterstrichen,
  ein Fremdschlüssel mit ↑ und im Tooltip, worauf er verweist. Der
  Query Stepper zeigt sie unter der Auswahl der Tabellen, die »Einführung in
  SQL« nach `SHOW TABLES` und `DESCRIBE` (mit einer Erklärung von Primär- und
  Fremdschlüssel), die »Filmdatenbank«, sobald alle sechs Tabellen geladen
  sind. Für Autorinnen und Autoren: `_schema(countries)`,
  `_schema(movies)` oder `_schema(music)` auf einer Zeile für sich; die
  Schlüssel liest `src/scripts/query-stepper-data.rb` aus den SQL-Dateien.

- Ein neuer Datensatz »Länder der Welt«, frisch aus Wikidata: die 197 Länder
  (Mitglieder der Vereinten Nationen, Vatikanstadt, Palästina, Kosovo und
  Taiwan) mit Einwohnern, Fläche und Kontinent, gut 4.700 Städte ab 100.000
  Einwohnern und jede Hauptstadt, und die Landgrenzen zwischen den Ländern,
  in vier Tabellen `continent`, `country`, `city` und `border` mit
  englischen Spalten und deutschen Namen. Er liegt in
  `specht/workspace-files` unter `country-archive/`, mit denselben Dateien
  wie die Filme und die Musik (auch für Neo4j). Gebaut wird er von
  `src/scripts/collect-countries`, das man jederzeit neu laufen lassen kann.

- Die Länder der Welt gibt es in zwei Fassungen. Die kleine,
  `countries-basic.sql`, hat nur `continent`, `country` und `city`; die
  ganze, `countries.sql`, hat dazu die Grenzen, die Währungen (`currency`,
  der Euro in 27 Ländern) und die Amtssprachen (`language` und
  `country_language`, die Schweiz mit vier), und in `country` die
  Internet-Endung, die Vorwahl und die Straßenseite. Die ganze ersetzt beim
  Import die Tabellen der kleinen.

- Die »Einführung in SQL« hat einen zweiten Teil, »Mehr Tabellen«: Lernende
  laden die ganze Fassung der Länder der Welt und lernen `GROUP BY` und
  `HAVING`, `JOIN` mit Hauptstädten, Währungen und Nachbarländern (dieselbe
  Tabelle zweimal, mit `AS`), `LEFT JOIN` für Länder ohne Nachbarn und eine
  Beziehung n:m an den Amtssprachen. 14 neue Aufgaben, darunter eine zur
  Frage, warum jede Grenze zweimal dasteht; der erste Teil lädt die kleine
  Fassung und ist unverändert.

- Die Dateien der Datensätze tragen den Namen ihres Datensatzes:
  `countries.sql`, `movies-data-films.sql`, `music-neo4j.dump` statt
  `mysql.sql`, `mysql-data-movies.sql`, `neo4j.dump`. Wer eine Datei
  heruntergeladen hat, sieht im Explorer, was sie ist, und zwei Datensätze
  im selben Ordner überschreiben sich nicht. Die Filmdaten liegen bis zum
  Ende des Schuljahrs auch noch unter den alten Namen, für alle, die mitten
  in der »Filmdatenbank« sind.

- Für Autorinnen und Autoren: `src/scripts/publish-datasets.rb`
  veröffentlicht einen gebauten Datensatz in `specht/workspace-files`: Es
  kopiert ihn ohne Caches, schreibt dort eine Übersicht aller Datensätze mit
  ihren Tabellen und Zeilen, baut die Tabellen des Query Steppers neu,
  committet und pusht. Einen Bau, dessen Zeilenzahlen die Aufgaben nicht
  erwarten, verweigert es (docs/entwicklung.md).

- Neue Handbuch-Seite »Datensätze« unter »Datenbanken«: die Länder der
  Welt, die Filme und die Musik, jeweils mit ihren Tabellen als Schema, was
  die Spalten bedeuten, was drin ist und was nicht, woher die Daten kommen
  und wie man sie lädt. Die Aktivitäten verweisen darauf.

- Die Handbuch-Seite »Farbräume« erzählt, wie die Farbräume entstanden
  sind: drei Farben bei Young, Helmholtz und Maxwell, die Messungen von
  Wright und Guild und XYZ von 1931, YCbCr aus dem Farbfernsehen, CIELab
  aus Herings Gegenfarben, Munsells Helligkeitsstufen und MacAdams
  Messungen, HSV aus den ersten Malprogrammen und sRGB aus dem
  Röhrenmonitor. Dazu die Normfarbtafel mit dem Dreieck, das ein
  sRGB-Bildschirm zeigen kann. Berichtigt: XYZ beschreibt nicht, wie stark
  die Zapfen angeregt werden.

- Die Handbuch-Seite »Farbräume« zeigt jeden Farbraum als Körper zum
  Drehen: alle Farben des RGB-Würfels an ihrer Stelle in RGB (der Würfel
  auf seiner schwarzen Ecke), im HSV-Kegel, in YCbCr, CIELab und OKLab,
  das Helle immer oben. Ein Schieber schneidet den Körper waagerecht ab und
  zeigt auf der Schnittfläche alle Farben derselben Helligkeit, ein Viertel
  lässt sich herausnehmen. Geteilt in die 8 × 8 × 8 Würfel von RGB und
  auseinandergezogen, zeigt jeder Körper, wie sein Farbraum gleiche Schritte
  in RGB verbiegt. Neu ist der Teil »OKLab und OKLCh« mit den Formeln und
  `oklch()` in CSS.

- Neue Aktivität »Zwei Faktoren: Einmalcodes selbst gebaut« in »Netze &
  Sicherheit«, in Ruby und Python: Lernende bauen das Verfahren hinter
  Authenticator-Apps selbst, Base32, HOTP nach RFC 4226 und TOTP nach
  RFC 6238, und einen Prüfer mit Zeitfenster, wie ihn ein Server braucht.
  Zum Schluss scannen sie ihren eigenen Schlüssel als QR-Code mit dem Handy,
  und App und Programm zeigen dieselben Codes. Dazu, wovor ein zweiter
  Faktor schützt und wovor nicht. Neun Aufgaben mit den Testwerten aus den
  RFCs, zwei Screenshots, eine Lehrkraftnotiz.

- Neue Handbuch-Seite »Farbräume« unter »Farben«: RGB, gespeicherte Werte
  und Licht (sRGB, Gamma), Graustufen, HSV, YCbCr und CIELab, jeweils mit
  den Formeln zum Umrechnen und demselben Foto in seine drei Kanäle
  zerlegt. Dazu eine Tabelle, welcher Farbraum wofür gut ist.
  »Eine JPEG-Datei decodieren« verweist auf den Teil zu YCbCr.

- »Eine JPEG-Datei decodieren« hat eine Zeitlupe zur iDCT: Sie setzt
  den Beispielblock Koeffizient für Koeffizient aus seinen Basisbildern
  zusammen, zeigt dabei das Basisbild, den Block bisher und seine
  Pixelwerte und landet genau bei dem, was der Decoder ausrechnet. Dazu
  kommen drei Aufgaben, eine für jede Stufe, die bisher keine hatte: der
  Wert nach der Dequantisierung, ein Pixel nach der iDCT und die Lage der
  Blöcke in der MCU, jede mit Rückmeldungen für die typischen Fehler.

- Der Query Stepper kennt auch die Filme der »Filmdatenbank«:
  `movie`, `genre` und `movie_genre` ganz, von `crew`, `job` und
  `movie_crew` Regie und Schauspiel. Die »Filmdatenbank« zeigt mit ihm, wie
  die Tabelle mit jedem `JOIN` wächst und wie `HAVING` ganze Gruppen
  streicht; auf der Werkzeugseite wählt man die Tabellen aus. Vor `SELECT`
  zeigt er nur die Spalten, die die Abfrage benutzt. Ein `JOIN … ON a = b`
  sucht die Partner über einen Schlüssel statt jede Zeile mit jeder zu
  vergleichen. Zwölf Abfragen der Seite geben dieselben Zeilen wie MySQL,
  auch »Wer spielt mit wem?« über sechs Tabellen.

- Neues Werkzeug: der Query Stepper (`/query-stepper`). Er führt
  eine SQL-Abfrage so aus, wie die Datenbank es tut, Teil für Teil: FROM,
  JOIN, WHERE, GROUP BY, HAVING, SELECT, DISTINCT, ORDER BY, LIMIT. Nach
  jedem Schritt zeigt er die Tabelle, die gerade entstanden ist, mit
  durchgestrichenen Zeilen, die wegfallen, Gruppen in eigenen Farben und den
  Spalten, mit denen der Schritt arbeitet. ← → schalten weiter. Er rechnet im
  Browser mit den Tabellen aus terra1 und hält sich an MySQL, wo man es
  merken würde. Groß- und Kleinschreibung zählen bei Texten nicht, NULL fällt
  aus WHERE, einen Namen aus AS gibt es bei WHERE noch nicht, und mit GROUP BY
  braucht jede andere Spalte eine Funktion wie COUNT. Fehler erklärt er,
  statt sie nur zu melden. Die »Einführung in SQL« hat dazu einen Abschnitt
  »In welcher Reihenfolge arbeitet die Datenbank?« mit zwei Aufgaben, eine
  davon eine Vermutung.

- »Ein Krimi im Terminal« hat ein letztes Kapitel, »Beweise vom Server«, wo
  der Fallserver eingeschaltet ist: Lernende melden sich mit `ssh` auf dem
  Server des Museums an, springen mit `ssh -J` weiter zur Alarmanlage, die
  nur von dort erreichbar ist, finden im VPN-Protokoll und in der
  `.bash_history` der Wartung die letzten Beweise und kopieren sie mit `scp`
  in ihren Workspace. Vier Aufgaben, zwei Screenshots. Jede Person hat einen
  eigenen Zugang; ohne Fallserver fehlt das Kapitel samt Aufgaben.
- Seiten können Teile haben, die es nur gibt, wenn die Installation eine
  Funktion eingeschaltet hat (`<!-- feature: case_server -->`).

- Neue Aktivität »Ein Rechner für Ausdrücke« in »Wie Computer rechnen«, in
  Ruby und Python, nach den Endlichen Automaten: Lernende zerlegen Text in
  Tokens (die Zahl als Automat in Automatron), bauen mit rekursivem Abstieg
  einen Baum nach einer Grammatik, rechnen ihn aus, melden Fehler, bauen
  `^` von rechts nach links und Variablen ein und haben am Ende einen Rechner
  zum Tippen im Terminal. Ein Baumzeichner auf der Seite zeigt den Baum
  jedes Ausdrucks und rechnet ihn mit ← → Knoten für Knoten aus. Zehn
  Aufgaben, davon zwei Vermutungen.
- Ruby- und Python-Prüfungen können verlangen, dass ein Aufruf mit einem
  Fehler abbricht (`raises: true`), etwa ein Rechner bei einer Klammer, die
  nie zugeht.

- Neue Aktivität »Ein Krimi im Terminal« in »Terminal & Git«: Aus dem
  Stadtmuseum Bitstein ist die Goldene Diskette verschwunden, und die Akten
  sind zu groß zum Durchlesen. Lernende lösen den Fall mit `grep` (mit `^`,
  `-i`, `-c`, `-l`), `head | tail`, einer Kette aus `grep`, `sort | uniq -d`
  und zum Schluss mit `git log` und `git show` in einem Repository, in dem
  ein Commit mit harmloser Nachricht zwei Zeilen löscht. Sieben Screenshots,
  elf Aufgaben, davon vier Befehle, deren Ausgabe der Workspace prüft, eine
  Lehrkraftnotiz mit der Lösung und zum Schluss ein eigener Fall zum
  Abgeben. Die Idee stammt aus »The Command Line Murders« von Noah Veltman.

- Neue Aktivität »Geheime Botschaften in Bildern« in »Bilder und
  Kompression«, in Ruby (ChunkyPNG) und Python (Pillow): Lernende lesen ein
  PNG als Liste von Zahlen, sehen sich Bitebenen an, verstecken Text mit
  einem Längenkopf in den untersten Bits und finden die Botschaft in einem
  vorbereiteten Bild. Danach rechnen sie aus, wie viel hineinpasst, sehen
  die Artefakte bei mehr Bits pro Kanal, entdecken Botschaften mit dem
  Chi-Quadrat-Test von Westfeld und Pfitzmann und erfahren, warum
  komprimierte Botschaften schwerer zu entdecken sind als Klartext. Zum
  Schluss bauen sie Farbklassen und eine Zahl mit wechselnder Basis, erst
  mit Würfeln im RGB-Raum, dann mit Klassen in CIELab. Zwölf Aufgaben, eine
  Lehrkraftnotiz, Screenshots aus dem Workspace; die Fotos sind von Ian
  Kobylanski.

- »Daten aus dem Netz« gibt es jetzt auch in Python, nur mit der
  Standardbibliothek (`urllib.request`, `json`): die drei Stufen von
  `wetter.py` mit Fehlerbehandlung und Cache, die Erklärungen dazu, zwei
  Screenshots und Varianten der drei Aufgaben, die `wetter.rb` lesen. Die
  Wetterseite ist für beide Sprachen dieselbe. »Das Wetter aus Ruby« heißt
  jetzt »Das Wetter aus deinem Programm«; wer sie oder »Ein Cache« schon
  gelöst hat, sieht sie einmal als »geändert«.
- »SQL Injection« gibt es jetzt auch in Python: `login.py` mit
  `mysql.connector`, die Hinweise dazu und das Prepared Statement mit
  `cursor(prepared=True)`. Python braucht dafür nichts zu installieren. Wer
  »Ein Prepared Statement« schon gelöst hat, sieht die Aufgabe einmal als
  »geändert«.
- »Eine Pixelwand für alle« gibt es jetzt auch in Python: die fünf Stufen
  des Servers mit dem Paket `websockets` (`pip install websockets`), die
  Erklärungen dazu, sechs Screenshots aus dem Workspace und Varianten der
  Aufgaben, die Ruby nennen. Webseite und `flood.js` sind für beide
  Sprachen dieselben. Wer »Jeder kann alles schicken«, »Die Pixelwand« oder
  »Ein Lock« schon gelöst hat, sieht sie einmal als »geändert«.
- »Ein eigener Webserver« gibt es jetzt auch in Python: alle sechs Stufen
  des Servers mit `socket`, `threading` und `urllib.parse`, die Erklärungen
  dazu, sechs Screenshots aus dem Terminal und Varianten der Aufgaben, die
  Ruby nennen. Die Seiten in `public` sind für beide Sprachen dieselben. Zwei
  Antworten in »Nur aus public« und »Zwei auf einmal« nennen Ruby nicht mehr;
  wer diese beiden Aufgaben schon gelöst hat, sieht sie einmal als
  »geändert«.
- »Endliche Automaten« gibt es jetzt auch in Ruby, und Ruby ist dort wie
  überall die Voreinstellung: alle sechs Programme, die Erklärungen dazu,
  drei Screenshots aus dem Workspace und die vier Programmieraufgaben mit
  Prüfungen in beiden Sprachen. Der Titel heißt nur noch »Endliche
  Automaten«. Wer die vier Programmieraufgaben schon in Python gelöst hat,
  sieht sie einmal als »geändert«, weil ihre Prüfung jetzt beide Sprachen
  umfasst.
- Aktivitäten in Ruby und in Python: Lehrkräfte stellen auf der
  Klassenseite unter »Sprache« ein, welche Programmiersprache ihre Gruppe
  sieht, ohne Einstellung Ruby. Auf Seiten, die es in beiden Sprachen gibt,
  schalten Lernende mit »Ruby | Python« neben der Art der Aktivität auch
  selbst um, ohne Neuladen; ihre Wahl gilt dann für sie überall. Lauflängen,
  Huffman und LZW gibt es jetzt in beiden Sprachen, mit Gerüst, Hinweisen,
  Lösungen und Prüfungen für jede. Eine Aufgabe, die jemand in einer
  Sprache gelöst hat, gilt auch in der anderen. Für Autorinnen und Autoren:
  `languages: [ruby, python]` im Meta-Block, Teile für eine Sprache in
  `<!-- language: python -->` … `<!-- /language -->`, und in `tasks.yaml`
  unter `python:` (oder `ruby:`) nur, was sich ändert.
- Neuer Abschnitt »Bilder und Kompression« im Katalog, mit einer Schraubzwinge
  als Zeichen: Lauflängen, Huffman, LZW, GIF und JPEG, in dieser Reihenfolge.
  JPEG zieht aus »Wie Computer rechnen« dorthin um.
- Neue Aktivität »Komprimieren mit Lauflängen« (`/rle`), in Ruby, die
  einfachste der Reihe: ein Pixeleditor mit der Diskette aus dem GIF-Labor
  zeigt neben jeder Zeile ihre Läufe und die Größe, während man malt.
  Lernende schreiben RLE erst als Text, merken an `2021`, warum das mit
  Ziffern nicht geht, und dann als Paare von Bytes; gemessen wird an einer
  Pixeldatei, die auf 4 % schrumpft, und an einem Text, der um die Hälfte
  wächst. Sieben Aufgaben.
- Neue Aktivität »Komprimieren mit Huffman« (`/huffman`), in Ruby, im
  neuen Abschnitt »Bilder und Kompression«: vom Morsecode über präfixfreie
  Codes zum Huffman-Baum, den eine Zeitlupe für einen eingetippten Text
  Schritt für Schritt aufbaut und zeichnet. Lernende schreiben aus einem
  Gerüst mit leeren Methoden Zählen, Baum, Codetabelle, Codieren und
  Decodieren, vergleichen mit LZW und `gzip` und berechnen zum Schluss
  kanonische Codes, wie JPEG sie speichert. Weil es bei Gleichstand mehrere
  richtige Bäume gibt, prüfen die Aufgaben, was für jeden gilt: präfixfrei
  und so wenige Bits wie möglich. Acht Aufgaben.
- Neue Aktivität »Komprimieren mit LZW« (`/lzw`), in Ruby, im neuen
  Abschnitt »Bilder und Kompression«: Zwei Zeitlupen komprimieren einen Text, den
  man selbst eintippt, und entpacken die Codes wieder, mit dem Sonderfall
  rot markiert. Lernende schreiben `compress` und `decompress` aus einem
  Gerüst mit leeren Methoden und messen an der Spezifikation von GIF und an
  einem JPEG, das größer statt kleiner wird. Sieben Aufgaben.
- Neue Prüfart `ruby` für Aufgaben, wie `python`: Sie lädt eine Ruby-Datei
  der Lernenden in ihrem Workspace, als Gast, ruft Methoden mit festen
  Eingaben auf und nennt den ersten Aufruf, der nicht passt. Ein Block
  `if __FILE__ == $0` läuft dabei nicht.
- »Eine JPEG-Datei decodieren« ist jetzt ein Labor in C mit PixelRAM statt
  Pixelflow Canvas: Vorbereitung mit der PixelRAM-Vorlage, ein kleines Gerüst
  zum Kopieren und zu jedem Teil ein Kasten »Für deinen Decoder« mit
  Funktion, Signatur und einem Zwischenstand zum Vergleichen. Neu erklärt
  sind Byte Stuffing und die Zick-Zack-Reihenfolge, die einem Decoder sonst
  fehlen. Ein zweites Baseline-Foto ohne Subsampling und mit eigenen
  Huffman-Tabellen (`/jpeg/91-baseline.jpg`) fängt Decoder, die nur das
  Beispiel können. Sechs Aufgaben, die letzten mit Toleranz, weil zwei
  JPEG-Decoder nie Pixel für Pixel dasselbe liefern; eine Notiz für
  Lehrkräfte und ein vollständiger Decoder in `src/content/jpeg/loesung/`.
- Neue Aktivität »Eine GIF-Datei decodieren« (`/gif`), im neuen Abschnitt
  »Bilder und Kompression« neben JPEG: Lernende schreiben in C einen GIF-Decoder, der
  mit PixelRAM zeichnet. Die Seite geht jeden Block an einer Diskette mit
  16 × 16 Pixeln durch, mit den Diagrammen der Spezifikation und farbigen
  Hexdumps; eine Zeitlupe zeigt die LZW-Decodierung Code für Code, mit
  Bits, Tabelle und dem Bild, das dabei entsteht. Dann folgen ein Foto mit
  256 Farben (von Ian Kobylanski), Interlacing und Animationen mit
  Transparenz, Disposal Method und Local Color Table; zum Schluss spielt der
  Decoder Bugs Bunny und Daffy Duck im Streit über die Aussprache von GIF ab.
  Als Code gibt die Seite nur ein Gerüst und die Prüfsumme vor, alles andere
  beschreibt sie mit Signaturen, Zwischenständen und Hinweisen. Acht
  Aufgaben, vier davon Prüfsummen, die der Decoder selbst ausgibt; bei der
  Animation erkennt die Rückmeldung die typischen Fehler an der Prüfsumme.
  Für Lehrkräfte gibt es die
  Spezifikation GIF89a von 1990 als Heft zum Ausdrucken (A4, gefaltet A5) und
  einen vollständigen Decoder mit Player in `src/content/gif/loesung/`.
- Spiegel von Open-Meteo für »Daten aus dem Netz«: unter `/open-meteo/api/v1/forecast`
  und `/open-meteo/geocoding/v1/search` dieselben Endpunkte wie bei
  Open-Meteo. Er merkt sich jede Antwort eine Viertelstunde für alle und gibt
  die letzte bekannte heraus, wenn Open-Meteo `429` meldet oder schweigt. Die
  Aktivität erklärt im neuen Abschnitt »Wenn die API nicht mehr will«, wann man
  umstellt; die Adressen stehen dafür in Konstanten oben in `wetter.rb` und
  `index.html`. Die Wetterseite merkt sich Antworten zehn Minuten im Browser,
  zeigt, von wann die Daten sind, und hat »Neu laden«; das Neuladen durch
  »Go Live« bei jedem Speichern kostet so keine Anfragen mehr. Eine neue
  Aufgabe fragt, warum ein gemeinsamer Cache das Kontingent der Klasse schont.
- TCP-Konsole (`/tcp-console`): Lernende tippen eine TCP-Verbindung Zeile für
  Zeile wie mit `nc -C`, auf Wunsch mit TLS, und sehen Anfrage und Antwort als
  Sprechblasen mit Uhrzeit, wie in einem Chat. Die Verbindung beginnt im
  eigenen Workspace (als Gast), erreicht also nur, was `nc` dort auch
  erreicht; `localhost` ist der eigene Workspace. Die Konsole liest höchstens
  1 KB pro Sekunde, so dass Antworten nach und nach ankommen; »Stopp« bricht
  eine Antwort ab, »Trennen« oder Strg+C trennen die Verbindung. Beginn und
  Ende jeder Verbindung stehen als eigene Zeile im Verlauf, mit Uhrzeit, Dauer
  und Grund (Server, du, Zeitgrenze). Nur angemeldet, eine Verbindung pro
  Person, höchstens 30 in zehn Minuten, zwei Minuten ohne Daten, zehn Minuten
  und 512 kB je Verbindung. TCP/IP hat dazu die Abschnitte »Die
  TCP-Konsole« (HTTP über TLS von Hand) und »Wer zu langsam tippt« (Server
  legen auf) mit einer neuen Aufgabe; der Webserver verweist darauf.
- Neue Aktivität »Endliche Automaten in Python« unter »Experimente«:
  Lernende bauen ein Drehkreuz, einen Automaten für »durch 3 teilbar« und ein
  Zahlenschloss als DEA, finden heraus, warum das Schloss bei 121213 nicht
  aufgeht, schreiben das Schloss als NEA, bauen mit der
  Potenzmengenkonstruktion einen DEA daraus und sehen ihn explodieren.
  Automatron dient zum Vergleichen. Zehn Aufgaben, Hinweise für Lehrkräfte.
- Neue Prüfart `python` (`docs/aufgaben.md`): lädt eine Python-Datei der
  Lernenden in ihrem Workspace, als Gast, ruft Funktionen daraus auf und
  vergleicht die Ergebnisse. Die Rückmeldung nennt den ersten Aufruf, der
  nicht passt, mit seinem Ergebnis oder seinem Fehler samt Zeile.
- Format für Aufgaben, die der Workspace selbst prüft: `docs/aufgaben.md`.
  Noch nicht aktiv; Seiten binden noch keine Aufgaben ein.
- `src/ruby/include/task_spec.rb` prüft `tasks.yaml`-Dateien;
  `src/ruby/test/task_spec_test.rb` prüft alle Aufgaben im Inhaltsordner.
- Erste Aufgabendateien für TCP/IP (12), Anaglyphen (12) und shower.js (11).
- Handbuch: eigener Katalog in `src/content/reference/`, Übersicht unter
  `/reference`, Seiten unter `/reference/<slug>`. Erste Seiten: »Workspace:
  Grundlagen« und »HTML-Folien gestalten«.
- Platzhalter `_include(seite#teil)` und `_uses(teil, …)` sowie Teile in
  Handbuch-Seiten (`<!-- part: name -->`), siehe `docs/aufgaben.md`.
- Notizen für Lehrkräfte: `lehrkraft.md` neben einer Seite erscheint für
  Lehrkräfte als aufklappbare Notiz über der Aktivität.
- Neue, freiwillige Felder im Meta-Block: `kind`, `topic`, `duration`,
  `requires`, `reference`, `template`.
- Aktivitäten und Handbuch-Seiten sehen unterschiedlich aus: Über einer
  Aktivität stehen Art, Dauer und die Handbuch-Seiten, die sie nutzt; über
  einer Handbuch-Seite stehen »Handbuch › Abschnitt« und die
  Aktivitäten, die sie verwenden. Inhalte aus dem Handbuch sind türkis markiert.
- Menüpunkt »Handbuch« für alle, auch ohne Anmeldung.
- Aufgabenkarten: `_task(id)` setzt eine Aufgabe aus der `tasks.yaml` in die
  Seite. Der Workspace prüft Antworten, Auswahl, Zahlen und Reflexionen
  (`answer`, `choice`, `number`, `reflection`) und merkt sich das Ergebnis.
  Lernende können ihre Antwort jederzeit selbst löschen. Erste Aufgaben in
  »TCP/IP-Verbindungen« (5 Stück).
- Aufgaben sind für Lernende ausgeschaltet, bis eine Installation
  `TASKS_ENABLED = true` in `credentials.rb` setzt. Lehrkräfte sehen sie
  vorher schon als Vorschau.
- Aufgaben, die im Workspace nachsehen: `files` (Dateien, Formate,
  Dateinamen, Größen), `html` (Tags passen zusammen, Folien, Bilder mit
  `alt`), `css` (Klammern, Doppelpunkte, Semikolons, eigene Regeln),
  `references` (nichts aus dem Netz, alle Pfade stimmen) und `git`
  (committet und gepusht?). Die Rückmeldung nennt Datei und Zeile und zeigt,
  was gelesen wurde. Der Server folgt dabei keinen symbolischen Links und
  führt Git nur im Workspace der Person aus, nur lesend.
- Abgaben (`submit`): eine Kopie der genannten Dateien als `.tar.gz` unter
  `/internal/submissions/`; Lernende laden sie herunter, geben neu ab oder
  löschen sie.
- Neue Aktivitäten »Mit der Data Structures Sandbox arbeiten«, »Mit der
  Sorting Sandbox arbeiten« und »Mit der CPU Sandbox arbeiten« unter »Terminal
  & Git«: begleiten Einrichtung, Klonen und Veröffentlichen und prüfen am Ende
  jeder Stunde, ob alles committet und gepusht ist. Die Adresse des
  Klassen-Repositorys steht jeweils im Meta-Block (`class_repo`).
- Handbuch-Seite »Git und GitLab«: SSH-Schlüssel, Name und E-Mail für Git, was
  tun, wenn ein Push abgelehnt wird. Die drei Sandbox-Seiten binden sie ein.
- Alle elf Aufgaben in »Eine Präsentation in HTML erstellen« und die
  2048-Aufgabe in »TCP/IP-Verbindungen« stehen jetzt auf der Seite.
- Klassenübersicht unter `/klasse` (Menüpunkt »Klassen« für Lehrkräfte mit
  Gruppe): Lernende × Aufgaben einer Aktivität, wer hängt, was häufig falsch
  ist, Antworten zum Nachlesen und Abgaben zum Herunterladen. »Jetzt in den
  Workspaces nachsehen« prüft eine Workspace-Aufgabe sofort für die ganze
  Gruppe, ohne etwas zu speichern.
- Eine Adresse kann in mehreren Gruppen stehen (Klasse und Kurs); die
  Klassenübersicht zeigt sie in jeder. Für alles andere zählt wie bisher die
  erste Gruppe.
- Lehrmaterial listet die Aktivitäten mit einer Notiz für Lehrkräfte.
- Tabler Icons (MIT) als gebündeltes Sprite in `src/static/include/tabler/`,
  zunächst für die Klassenübersicht; Liste in `icons.txt`, neu bauen mit
  `src/ruby/tools/build_tabler_sprite.rb`.
- Code Bites wird zu Übungen: »Programmieren üben« im Katalog mit drei
  Übungen (Zahlen, Arrays, Strings) und 17 Aufgabenkarten der Prüfart `code`.
  Gelöst wird weiter im Code-Editor; wer dort alle Tests besteht, ist auf der
  Karte und in der Klassenübersicht abgehakt. Der Knopf »Code Bites« in der
  Menüleiste und sein Schalter im Profil entfallen; `/codebites` bleibt
  erreichbar.
- Startseite: angemeldete Lernende sehen »Weitermachen« mit ihren
  angefangenen Aktivitäten und der nächsten offenen Aufgabe. Katalogkarten
  zeigen die Art der Aktivität und den eigenen Stand.
- Prüfarten `measure` und `command`: Messwerte vergleicht der Server mit
  einer eigenen Messung im Workspace der Person (ping, traceroute, curl);
  Befehle laufen im eigenen Workspace mit Zeitlimit, die Ausgabe wird
  geprüft. Alle TCP/IP-Aufgaben außer dem Codewort stehen jetzt auf der Seite.
- Anleitung für Lehrkräfte, die eigene Aktivitäten schreiben:
  `docs/aktivitaet-schreiben.md`, mit Vorlage in `docs/vorlage-aktivitaet/`
  (ein Test hält sie gültig). Hinweis darauf im README.
- Startseite: Lernende blenden angefangene Aktivitäten aus, bis sie dort
  weiterarbeiten. Lehrkräfte heften in der Klassenübersicht Aktivitäten für
  eine Gruppe an; diese stehen ganz oben und lassen sich nicht ausblenden.
  Neuer Knoten `Pin` (Constraint
  `Pin/key`).
- Prüfart `scene`: Der Workspace liest eine Anaglyph-Szene mit dem Viewer
  der Person in ihrem eigenen Workspace und prüft Formen, Positionen, Größen,
  Reihen, Gitter und Umlaufbahnen; Fehler des Viewers erscheinen wörtlich.
  Alle zwölf Anaglyph-Aufgaben stehen jetzt auf der Seite; eine abgegebene
  Szene muss sich laden lassen.
- Prüfart `sql`: Lernende schreiben eine `SELECT`-Abfrage; sie läuft in
  ihrer eigenen MySQL-Datenbank, als sie selbst, nur lesend und mit
  Zeitlimit, und wird mit dem Ergebnis einer Musterabfrage verglichen. Die
  Karte zeigt das eigene Ergebnis als Tabelle. Kann sie nicht prüfen, nennt
  sie Datenbank, Anmeldung, die Meldung von MySQL, vorhandene Tabellen, den
  Workspace und `.my.cnf`. »Einführung in SQL« hat jetzt
  13 Aufgaben; die alte Aufgabenliste erscheint nur noch, wenn Aufgaben
  ausgeschaltet sind (`tasks-off-only`).
- Neue Version einer Vorlage: Auf der Seite einer Aktivität mit `template`
  sieht der Workspace nach, ob der Klon in `/workspace` die neueste Version
  enthält, und sagt sonst »Es gibt eine neue Version der CPU Sandbox! Du
  solltest `git pull` ausführen«. Er holt nie selbst etwas. In der
  Klassenübersicht prüft »Vorlage aktuell?« die ganze Gruppe. Neues Feld
  `template_name`; `template` steht jetzt auch bei BIF, LaTeX, PixelRAM,
  Videothek und dem Digits-Experiment. Neuer Handbuch-Teil
  `workspace#neue-version`.
- ER-Werkstatt: VS Code-Erweiterung `hackschule-er` im Workspace-Image.
  ER-Modelle werden als Text in `.er`-Dateien geschrieben; daneben erscheinen
  sie als Chen-, Krähenfuß- und Tabellendiagramm (Kästen verschiebbar), Fehler
  werden unterstrichen, »SQL erzeugen« schreibt das MySQL-Schema, »Als SVG
  speichern« das Diagramm. Kern und Kommandozeile:
  `src/vscode-extensions/er/core/er-core.js`.
- Handbuch-Seite »ER-Modelle als Text« (`/reference/er-modelle`): Syntax,
  Übersetzung in Tabellen, die drei Diagramme, die ER-Werkstatt in VS Code und
  auf der Kommandozeile, mit einem Feld zum Ausprobieren, das denselben Kern
  im Browser nutzt. Skripte neben einer Seite (`<script src='x.js'>`) werden
  wie Bilder über `/cache/` ausgeliefert.
- »Dateien und Verzeichnisse« hat 12 Aufgaben: entpackte Beispieldateien,
  mit `nano` und `vim` geschriebene Dateien, die Wortliste, das übersetzte
  Zork, Zahlen, die man mit `grep`, `wc` und der Tab-Ergänzung findet, und
  eine Notiz für Lehrkräfte.
- Aufgaben für »Versionsverwaltung mit Git« (11: Commits, Merge, Remote,
  Konflikt, jeweils aus `git status` des eigenen Repositorys) und »Statische
  Webseiten mit HTML & CSS« (10, mit Abgabe), jeweils mit Notiz für
  Lehrkräfte.
- Prüfarten `er` (Entitäten, Schlüssel, Attribute, Beziehungen,
  Kardinalitäten und die entstehenden Tabellen eines `.er`-Modells, gelesen mit
  dem Kern der ER-Werkstatt im Workspace) und `schema` (Spalten, Primär- und
  Fremdschlüssel und Zeilenzahl in der eigenen MySQL-Datenbank, nur lesend).
- »Filmdatenbank mit MySQL« ersetzt »Videothek mit MySQL«: vom Modell in der
  ER-Werkstatt über `mysql < film.sql` zu `JOIN`, `GROUP BY` und `HAVING`,
  mit 15 Aufgaben und einer Notiz für Lehrkräfte. `/videothek` leitet weiter.
  Ruby, das Import-Skript und der ERD Editor werden nicht mehr gebraucht.
- Filmdaten aus Wikidata (CC0): `src/scripts/collect-movies` wählt die Filme
  nach der Zahl der IMDb-Stimmen, übernimmt aber nur Daten aus Wikidata. Die
  Spalte `rating` entfällt; `crew.birth_year` darf leer sein.
- Neue Neo4j-Knoten `TaskState` und `TaskAttempt` (Constraint
  `TaskState/key`, Index `TaskAttempt/task`); sie werden beim Start von selbst
  angelegt.
- `_include(seite#teil, done: aufgabe)`: Ist die genannte Aufgabe auf der
  Seite gelöst, zeigt der Handbuch-Kasten beim nächsten Laden nur noch eine
  Zeile mit »Anzeigen«. Die drei Sandbox-Seiten nutzen das für »Git sagen,
  wer du bist«.
- Prüfart `git` ohne `repo` mit `expect: { identity: true }`: Kennt Git
  Namen und E-Mail-Adresse, und sind es nicht die Beispielwerte aus dem
  Handbuch? Neue Aufgabe »Git kennt dich« in den drei Sandbox-Begleitern.
- Auswahlaufgaben mischen ihre Optionen für jede Person anders, aber immer
  gleich (E-Mail-Adresse und Aufgabe als Seed). Antworten, `correct` und
  `picked` zählen weiter nach `tasks.yaml`; `shuffle: false` schaltet das
  für eine Aufgabe ab.
- Tutorial-Screenshots: Vor einem `<video>` erzeugt ein Rezept mit
  `record: 8s` (und `fps:`) das Video, Bild für Bild mit fester Zeit über den
  neuen Hook `setPreviewTime`, kodiert mit ffmpeg. Das Anaglyph-Tutorial hat
  Rezepte für seine Screenshots und alle drei Animationen.
  `src/scripts/recreate-screenshots.rb` kennt `--all` und `--stale`.
- Prüfart `scene` erkennt Farben: `color: yellow` (oder eine Liste von
  Farbfamilien) wählt Objekte nach ihrer Füllfarbe aus, `background: black`
  prüft den Hintergrund, `label` benennt einen Eintrag für Rückmeldungen und
  `orbits`. Die Rückmeldung zeigt bei jedem Objekt, welche Farbe der Viewer
  liest. Feedback in `scene` kann zusätzlich auf `problem` achten.
- Handbuch-Seite »Farben angeben«: Farbnamen, Farbcodes mit `#`, Rot-, Grün-
  und Blauanteile, Kontrast. Das Anaglyph-Tutorial verweist darauf.
- Code-Blöcke mit ```` ```er ```` sind auf der Website eingefärbt wie in der
  ER-Werkstatt (`src/ruby/include/er_lexer.rb`, ein Rouge-Lexer).
- Neue Aktivität »Filmdatenbank als Graph mit Neo4j« (`neo4j-filmdatenbank`),
  die Fortsetzung der Filmdatenbank: dieselben Daten aus `neo4j.dump`, Cypher
  im Vergleich zu SQL, Schlüssel und Indizes, Schauspielpaare, Wege durch den
  Graphen; sechs Aufgaben und eine Notiz für Lehrkräfte. Vorerst nur in der
  Entwicklung im Katalog, bis `neo4j.dump` veröffentlicht ist.
- Code-Blöcke mit ```` ```cypher ```` sind eingefärbt
  (`src/ruby/include/cypher_lexer.rb`): Labels, Beziehungstypen und
  Eigenschaften in denselben Farben wie Entitäten, Beziehungen und Attribute
  in ```` ```er ````. Die Muster in »Filmdatenbank als Graph« sind jetzt echtes
  Cypher (`(:Movie)` statt `(Movie)`).
- Neue Aktivität »Feuerwerk aus Partikeln« (`feuerwerk`) unter
  Computergrafik, in C mit PixelRAM: vom ersten Funken (Position,
  Geschwindigkeit, Schwerkraft) über Explosionen, Farbverläufe und Leuchtspuren
  bis zu Raketen über einer nächtlichen Stadt, die man mit der Maus startet;
  fünf Aufgaben, darunter zwei Rechnungen zur Physik und eine Abgabe.
- »Echtzeitgrafik mit C« (PixelRAM) ist neu geschrieben: Bilder aus Formeln
  (bis zum Sierpinski-Dreieck aus `x & y`), Wellen und Interferenz, das Spiel
  »Lichtspur« für zwei an einer Tastatur, die Beispiele der Vorlage
  (`make PROGRAM=beispiele/tunnel`) und Crystal Caves (`make crystal`). Eine
  Notiz für Lehrkräfte nennt größere Projekte: Raycaster, Raytracer, GIF- und
  JPEG-Decoder.
- Bilder und Videos in Varianten: Im dunklen Modus erscheint die dunkle
  Fassung von selbst. Ein Knopf in der Ecke zeigt die Bilder für die
  Rot-Cyan-Brille (Anaglyphen) oder wie auf einem Röhrenbildschirm (PixelRAM);
  die Wahl gilt für alle Bilder, bis man sie wieder ausschaltet.
- Anaglyphen: Eine Szene bringt ihre Kamera mit (`camera = …`, im Viewer
  »Kamera kopieren«). Die Prüfart `submit` kennt `preview: anaglyph`: Die
  Karte zeigt die gewählte Szene so, wie die Galerie sie zeigt, und die
  Klassenübersicht zeigt eine Galerie aller Abgaben, groß, drehbar und mit
  den Pfeiltasten zum Weiterblättern. Kenney-Modelle funktionieren auch in
  abgegebenen Szenen.
- »Mit der CPU Sandbox arbeiten« zeigt eine Galerie aus acht
  Beispielprogrammen. Die drei Sandbox-Seiten sagen gleich am Anfang, dass sie
  mit Dart, Python, JavaScript oder Java arbeiten.
- Das Keyboard-Tutorial ist eine eigene Aktivität; die Prüfart `keyboard`
  zeigt Lehrkräften in der Klassenübersicht, wer welches Kapitel geschafft hat.
- Handbuch-Seite »Shortcuts« zum Nachschlagen; »Farben angeben« hat einen
  Farbmischer und erklärt RGB und OKLCH.
- Code-Blöcke mit ```` ```ini ```` (Anaglyph-Szenen) sind eingefärbt, wie der
  Viewer sie liest (`src/ruby/include/anaglyph_scene_lexer.rb`): `#` am
  Zeilenanfang ist ein Kommentar, in `fill = #4aa03f` ein Farbcode.
- Der Workspace lässt sich über das Browsermenü als App installieren; eine
  Installationsleiste erscheint nicht.
- Tutorial-Screenshots: Der Generator zeichnet den Mauszeiger (`show-mouse`),
  rendert Varianten (`variants: dark anaglyph crt`), schneidet seitlich und auf
  ein Element zu (`crop-left`, `crop-right`, `crop-element`), rendert Leinwände
  scharf (`sharp-canvas`), macht Bilder aus PDF-Seiten (`pdf-page`) und
  Git-Objektgraphen (`git-graph`), öffnet den Neo4j Browser (`neo4j-browser`)
  und kennt Aktionen einzelner Tutorials (`hook: NAME ARGS`).
  `recreate-screenshots.rb --status` zeigt den Stand aller Seiten, ohne zu
  rendern; wer im Browser Tutorials aufruft, pausiert das Rendern im
  Hintergrund nur, statt es zu stören.
- Kopierknopf für Code-Blöcke, die abzutippen nichts lehrt: Adressen zum
  Klonen, Downloads, Einrichtungsbefehle (```` ```bash_copy ````). Was man
  lernen soll, wird weiter abgetippt; gemischte Blöcke sind geteilt, sodass
  nur die schwer zu tippende Zeile den Knopf hat.
- Handbuch »HTML und CSS«: drei Seiten für Webseiten und Folien zugleich,
  [HTML](/reference/html), [CSS](/reference/css) und
  [Layout](/reference/layout), dazu [Folien mit Shower](/reference/shower)
  für das, was nur Präsentationen brauchen. Jedes Beispiel ist ein erzeugtes
  Bild als Webseite und als Folie; ein Knopf in der Ecke schaltet um. Auf
  der Shower-Aktivität und den Handbuch-Seiten, die man von dort öffnet,
  erscheinen die Folien, sonst die Webseite.
- Aufgaben, die der Workspace selbst entscheidet, statt dass Lernende nur
  hinschauen: eine Liste mit drei Punkten, ein Link, `styles.css` wirklich
  eingebunden, `main` mit Flexbox, eine Media Query, die alles untereinander
  stellt. Dafür kennt die Prüfart `html` jetzt `elements`, `css` kennt
  `declares` und `linked_from`.
- BIF »Interaktive Geschichten schreiben«: 13 Aufgaben entlang der
  Geschichte bis zur eigenen Geschichte und ihrer Abgabe, eine Notiz für
  Lehrkräfte und die neue Prüfart `story`, die liest, was BIF selbst über die
  Geschichte herausgefunden hat (`.story-tools/analysis.json`), ohne etwas
  auszuführen.
- Handbuch »Interaktive Geschichten mit BIF« (`/reference/bif`): die
  Schreibweisen zum Nachschlagen und was BIFs Meldungen unter »Problems«
  bedeuten; die Aktivität bindet die Teile ein.
- MySQL: nach `ORDER BY`, `LIMIT`, `COUNT`, `LIKE` und `DISTINCT` je eine
  kurze Aufgabe.
- Klassenseite: Abgaben öffnen sich im Browser, mit Dateien, Quelltext und
  »Starten« für Webseiten und BIF-Geschichten. Sie laufen auf einem eigenen
  Host, `abgaben.<host>`, in einer Sandbox ohne Zugriff auf die Sitzung der
  Lehrkraft.
- Klassenseite: Wer eine Gruppe betreut, ohne in ihr zu stehen, sieht die
  eigenen Versuche und Abgaben in einer Zeile »Du (Lehrkraft)«.
- Klassenseite: »Anmelde-Codes zeigen« für den Beamer, groß und nur mit
  Namen, ohne E-Mail-Adressen; neue Codes erscheinen von selbst.
- Klassenseite: Reiter »Fortschritt«, »Lernende« und »Startseite«. Unter
  »Lernende« die Karten des Servers wie auf der Verwaltungsseite und jeder
  Workspace mit CPU und RAM; eine Zeile leuchtet gelb, solange ihr Workspace
  arbeitet.
- »Einstieg für Lehrkräfte« (nur für Lehrkräfte): Anmeldung mit den Codes am
  Beamer, die Klassenseite, Abgaben, das Testkonto und was der Server
  speichert, mit Bildern. Die Klassenseite und das Profil weisen darauf hin.
- Klassenseite: »Alle Abgaben löschen« unter den Abgaben einer Aufgabe, für
  eine neue Runde in einer Gruppe, nach einer Warnung.
- Klassenseite: merkt sich am Konto die zuletzt gewählte Gruppe und je Gruppe
  die zuletzt gewählte Aktivität.
- Neue Prüfart `cypher`: Abfragen in der eigenen Neo4j-Datenbank, verglichen
  mit einer Musterabfrage, wie `sql` für MySQL. Nur lesend, doppelt
  abgesichert: Der Server lässt nur lesende Abfragen durch, und Neo4j führt
  sie in einer Lese-Transaktion aus (5 Sekunden, höchstens 5000 Zeilen).
- Neue Aktivität »Ein Spiel in 64 KB« (TIC-80, unter Computergrafik): Lua,
  Konsole und Editoren, Speichern als Textdatei in `/workspace/TIC-80`, der
  Bildschirm, dann Schritt für Schritt das Spiel »Star Catcher« (Figur,
  fallende Sterne, eigene Figur im Sprite-Editor, Leben, Ton), Ideen für ein
  eigenes Spiel und eine Abgabe. Neun Aufgaben, Notiz für Lehrkräfte,
  erzeugte Screenshots aus dem echten TIC-80.
- Handbuch »MiniCPU« für die CPU Sandbox, mit drei Seiten. »MiniCPU«: die
  Stufen 8, 8S und 8X, Register, Flags, jeder Befehl mit Opcode,
  betroffenen Flags, Bytes und Takten, Stack und Aufrufregeln, X und Y,
  Takte, Maschinencode, Assembler. »Geräte und Bildschirm«: alle Ports,
  Joystick, Tastatur, Ton, Timer und Vsync, Pixelformate, Palettenchip,
  Seitenwechsel, die Paletten als Farbfelder. »MiniC«: C für die MiniCPU.
  Die Tabellen erzeugt `src/scripts/minicpu-reference.mjs` aus den Tabellen
  der Sandbox selbst. Die Aktivität »Vom Byte zum 3D-Labyrinth« verweist an
  passender Stelle darauf.
- Handbuch »SQL«: mycli, Tabellen ansehen, `SELECT`, `WHERE`, Sortieren,
  Zählen und Gruppieren, `JOIN`, `CREATE TABLE`, `INSERT`, `UPDATE` und
  `DELETE`, Werte über Platzhalter in Programmen (PHP, Ruby, Python) und
  die häufigen Meldungen von MySQL. »MySQL kennenlernen« und
  »Filmdatenbank« verweisen darauf. Code-Blöcke in PHP sind farbig; ein
  Test hatte die fehlende Sprache bemerkt.
- Neue Aktivität »Eine Klötzchenwelt in 3D« (C mit raylib, unter
  Computergrafik): eine Welt aus Blöcken in der Ich-Perspektive, Kamera aus
  zwei Winkeln, Laufen, Schwerkraft und Kollision, Zielen mit einem Strahl,
  Bauen und Abbauen, ein eigener Licht-Shader in GLSL mit Tag und Nacht,
  Fackeln und Nebel, Texturen aus dem 2D Game Studio. Acht Stufen, Aufgaben,
  Notiz für Lehrkräfte; die Vorlage ist `specht/raylib-starter`, die
  Webseite um das Programm kommt von PixelRAM.
- Handbuch »raylib« (Fenster, 2D, Kamera, 3D, Farben, Eingaben, Zeit,
  Vektoren, Kollision, eigene Flächen, Texturen, Shader, GLSL) und Handbuch
  »PixelRAM«.
- »Eine Facharbeit mit LaTeX« ersetzt die bisherige LaTeX-Seite unter
  derselben Adresse: eine kurze Facharbeit über ein Fadenpendel, von der
  leeren Datei bis zum PDF, mit Titelseite, Gliederung, Formel, Bild,
  Tabelle, Diagramm (TikZ), Quellen mit biblatex und Seitenlayout. 13
  Aufgaben, Notiz für Lehrkräfte. Alles zum Nachschlagen, die Schriften und
  die Vorlagen (Bewerbung, Lebenslauf, Ausarbeitung …) stehen im neuen
  Handbuch »Dokumente mit LaTeX«.
- Neue Aktivitäten unter »Netzwerke«: »Ein eigener Webserver« (Ruby, ohne
  Bibliothek: zuhören, antworten, Pfade und 404, Dateien aus `public`,
  Parameter und ein Zähler mit Mutex) und »Eine Pixelwand für alle«
  (WebSockets mit selbst geschriebenem Handshake, Nachrichten an alle, eine
  gemeinsame Pixelwand für die Klasse über Shared Live Apps). TCP/IP
  verweist auf beide.
- Basics: »VS Code nach deinem Geschmack« mit einer kleinen Galerie
  eingebauter Farbschemata und solcher aus dem Marketplace.
- Feld `language` im Meta-Block: Die Art einer Aktivität nennt ihre
  Sprache, »Projekt (Ruby)« oder »Projekt (C / raylib)«, über der Seite und
  auf der Karte im Katalog. Gesetzt bei 17 Aktivitäten mit einer festen
  Sprache.
- Bilder in einer Galerie (`.screen-gallery`) öffnen sich auf einen Klick
  so groß wie das Fenster, mit ihrer Unterschrift; ← → blättern durch die
  Galerie. Kleine Pixelbilder bleiben scharf, dunkle und CRT-Varianten
  bleiben erhalten. Auf dem Handy öffnet sich nichts.
- TIC-80: ein Abschnitt »Ein Himmel« mit Aufgabe: funkelnde Sterne mit
  `pix`, Sternschnuppen mit Schweif und eine Wiese. Das fertige Spiel ist
  das neue Bild auf der Karte.
- Handbuch-Abschnitt »Netzwerk und Daten« mit drei Seiten. »HTTP«: Anfrage
  und Antwort, Methoden, Statuscodes, Header, Adressen, `curl`, der Reiter
  »Network« im Browser, Formulare und JSON, HTTPS, WebSockets. »JSON«:
  Aufbau, was JSON nicht erlaubt (mit den echten Fehlermeldungen), Ruby,
  JavaScript, Python, `jq`. »YAML«: Aufbau, Listen, lange Texte, die Fallen
  (`no`, `1.10`, `12:30`, `01234` in Ruby und `yq`), Front Matter, `yq` und
  Ruby. Webserver, Pixelwand, TCP/IP und Filmdatenbank verweisen darauf.
- Neue Aktivität »Eine Website mit Hugo« unter »Web & Schnittstellen«: ein
  Lexikon über die Planeten aus Markdown-Seiten mit Front Matter, ein eigenes Layout
  (`baseof.html`, `page.html`, `home.html`, `list.html`) mit Navigation,
  Startseite, CSS und Schlagwörtern, dann `hugo` und `public` zum
  Veröffentlichen auf der eigenen Subdomain. Neun Aufgaben, Notiz für
  Lehrkräfte, ohne fertiges Theme.
- Neue Aktivität »Daten aus dem Netz« unter »Web & Schnittstellen«: der freie
  Wetterdienst Open-Meteo als Beispiel für eine API, erst mit `curl` und
  `jq`, dann aus Ruby mit Fehlerbehandlung und Cache, zum Schluss eine eigene
  Wetterseite mit `fetch`. Erklärt Query-Parameter, Fehler, die Grenzen einer
  API (alle Workspaces einer Schule teilen sich eine Adresse), CORS und
  warum API-Schlüssel nicht in eine Webseite gehören. Acht Aufgaben, Notiz
  für Lehrkräfte. Die Reihen im Abschnitt: Webseiten (HTML & CSS, Hugo,
  Svelte), dann Netzwerk (Daten aus dem Netz, Webserver, Pixelwand).
- Neue Aktivität »Kryptographie im Terminal« unter »Terminal & Git«:
  Hashes mit `sha256sum`, wie Server Passwörter mit Salz und absichtlich
  langsamen Hashfunktionen speichern, Verschlüsselung mit einem Schlüssel
  (AES mit `openssl enc`) und mit zweien (RSA mit `openssl pkeyutl`),
  Unterschriften mit Ed25519 und `ssh-keygen -Y`, Fingerabdrücke und was
  davon in SSH, Git und HTTPS steckt. Ein Partnerteil tauscht öffentliche
  Schlüssel über Port 1234. Zehn Aufgaben, Notiz für Lehrkräfte.
- »SQL Injection« steht wieder im Katalog unter »Datenbanken«. Das Labor
  läuft in einer weiteren Datenbank aus dem Profil mit ihrem eigenen Zugang,
  sodass die Lücke nur diese Testdatenbank erreicht. `login.rb` braucht nur
  noch `mysql2` (`gem install mysql2`) und hat englische Namen; die
  Aufgaben sind Karten, dazu eine Prüfung des Prepared Statements und zwei
  Fragen, warum es schützt und warum ein eigener Zugang den Schaden
  begrenzt. Hinweis auf die Klartext-Passwörter und auf »Kryptographie im
  Terminal«, Notiz für Lehrkräfte.

### Behoben

- »Ein Krimi im Terminal«: Der Zettel mit dem Zugang zum Museumsserver
  fehlte in den Akten, wenn ein Workspace sie über die öffentliche Adresse
  der Installation lud; die Website erkannte den Workspace nicht. Wer
  angemeldet ist, bekommt auf der Seite jetzt eine eigene Adresse der Akten,
  und der Zettel liegt immer darin.
- Der Fallserver von »Ein Krimi im Terminal«, frisch aus dem Repository
  gebaut, hatte kein VPN-Protokoll (`logs/vpn.log` fiel unter eine
  `.gitignore`-Regel), und die Aufgabe »Wer kam in der Nacht?« ließ sich
  nicht lösen. Wer den Fallserver betreibt, baut `case_museum` neu.
- BIF: Die Aufgaben »Der Schlüssel« und »Ein Ende« ließen sich im Workspace
  nicht lösen: Die Prüfung erwartete Skripte, Bedingungen und Enden an
  anderer Stelle, als BIF sie in die Analyse der Geschichte schreibt, und las
  sie als 0.
- Die Karten von »Endliche Automaten« und »Ein Rechner für Ausdrücke« im
  Katalog zeigen ihre Zeichnung ganz, statt sie abzuschneiden: `image:
  bild.webp:contain` füllt eine Zeichnung auf 16:9 auf.
- Formeln mit großen Klammern, Summenzeichen und einigen Symbolen sahen auf
  allen Seiten falsch aus, weil die meisten Schriften von KaTeX fehlten;
  nur `KaTeX_Main` und `KaTeX_Math` lagen in `src/static/include/katex/fonts`.
  Jetzt sind alle 20 Schriften von KaTeX 0.16.9 da.

- Automatron: In den Beispielen steht die Beschreibung unter einem langen
  Ausdruck statt in einer schmalen Spalte daneben, in der sie Wort für Wort
  umbrach.
- »Statische Webseiten«, Aufgabe »Ein eigenes Bild«: Ein Bild aus dem Netz
  zählt nicht mehr als eigenes. Die Prüfung verlangt jetzt mindestens ein
  Bild mit einem Pfad im Workspace (`img: { local: 1 }`, neu für alle
  `html`-Prüfungen). Bisher genügte die Seite aus dem Schritt davor mit dem
  Bild von Wikipedia. Wer die Aufgabe schon gelöst hat, sieht sie einmal als
  »geändert«. Gefunden haben das die neuen Beispiele für die Prüfungen.
- »SQL Injection«: Die Testdaten kommen jetzt mit `mysql db_1234 < users.sql`
  in die Datenbank. Mit `mycli` hielt der Import mit der Frage »You're about
  to run a destructive command« an, die die Seite nicht erwähnte. Gefunden hat
  das der neue Durchgang durch die Seite, der jetzt alle Schritte in Ruby und
  Python prüft: Testdatenbank, Import, die drei Angriffe, die Lücke schließen
  wie beschrieben, dieselben Angriffe noch einmal.
- Automatron bricht bei langen Ausdrücken nicht mehr ganz ab: Ein Automat mit
  mehr als 300 Übergängen, etwa der DEA auf dem Weg zu »eine Dezimalzahl ist
  durch 3 teilbar«, wird nicht gezeichnet, sondern durch einen Hinweis
  ersetzt, und der kleinste DEA, die Grammatik und die Beispielwörter kommen
  trotzdem. Bisher kam nach zehn Sekunden nur eine Fehlermeldung.
- Unter der Fußzeile blieb auf allen Seiten ein leerer Streifen von 100
  Pixeln; er war für die feste Fußzeile der Entwicklungsumgebung gedacht.
- Die Kacheln der Programmiersprachen auf der Startseite folgen wieder dem
  dunklen Modus, mit den dunklen Fassungen der Logos und ohne weiße Fläche.
- Das Bild des Workspaces neben der Einleitung auf der Startseite ist 16:9
  statt fast quadratisch und so nicht mehr höher als der Text.

### Geändert

- Code-Blöcke können Kontext zeigen: Zeilen mit `~ ` davor erscheinen blass,
  lassen sich nicht markieren und werden nicht mitkopiert. Eine dünne
  Zickzacklinie schneidet den Block dort, wo Kontext und neuer Code
  aneinanderstoßen, und der Kopierknopf steht neben dem neuen Code.
  »Eine Klötzchenwelt in 3D« zeigt so bei jedem Stück Code, wo es in `main.c`
  hinkommt, mit drei Zeilen davor und danach, so wie die Datei an dieser
  Stelle aussieht.
- Handbuch HTML, »Bilddateien ins Projekt holen«: alle elf Bilder kommen aus
  Rezepten, vom Explorer über Drag & Drop, Einfügen aus der Zwischenablage
  und Umbenennen bis zur Umwandlung nach WebP. Das Kontextmenü des Browsers
  ist nachgebaut, weil ein Screenshot das echte nicht zeigt. Nach einem Update
  von VS Code entstehen sie neu.
- Kontextzeilen: Von der Zickzacklinie fällt ein leichter Schatten in den
  Kontext, so liegt der neue Code sichtbar obenauf. Leerzeilen, wo Kontext und neuer
  Code aneinanderstoßen, gehören zum Kontext; der Schnitt liegt direkt an der
  ersten und letzten Zeile Code.
- Klassenseite und Startseite: Titel mit »&« (»Statische Webseiten mit HTML &
  CSS«) standen dort als »&amp;«.
- Abgaben ansehen funktioniert jetzt in allen 15 Aktivitäten mit Abgabe; ein
  neuer Test (`e2e/tests/hand-ins.browser.spec.ts`) gibt jede einmal echt ab
  und öffnet sie als Lehrkraft. Die Facharbeit (PDF) erscheint im Browser statt
  »keine Vorschau«, Lua, TOML, Svelte, Makefiles und weitere Quelltexte als
  Text (die TIC-80-Abgabe zeigte bisher nichts), raylib startet `main.html`,
  Hugo zeigt zuerst `hugo.yaml` statt eine Vorlage roh, und eine gebaute
  SvelteKit-Seite findet ihre Startseite.
- Abgaben ansehen: Der Kopf sagt, wann die Abgabe kam, wie viele Dateien sie
  hat und wie groß sie ist. Alle Knöpfe haben Symbole, die Dateien ihre Art
  als Symbol, eine laufende Seite lässt sich neu laden, und die Pfeiltasten
  gehen durch die Dateien.
- »Einstieg für Lehrkräfte«, Reiter »Lernende«: Im Bild läuft jeder Workspace
  der Klasse, die meisten fast im Leerlauf, ein paar beschäftigt, und die
  Karten zeigen die Werte eines echten Servers statt des Entwicklungsrechners.
- Galerien können mit `hide-lone` ein viertes Bild weglassen, wenn es allein
  in einer Reihe stünde; so die Farbschemata in den Grundlagen.
- Pascal, »Programmieren wie in den 90ern«: Die Entwicklungsumgebung fp
  kennt die Ordner der Units inzwischen selbst (`/etc/fp.cfg`); der Schritt
  über »Options« und »Directories…« entfällt, stattdessen kompiliert
  <kbd>F9</kbd> gleich. Beide Bilder von fp kommen aus Rezepten.
- »Eine Präsentation in HTML erstellen«: VS Code und die Vorschau
  nebeneinander kommen aus einem Rezept. Dafür kennt der Screenshot-Generator
  `beside: preview`: der andere Tab in derselben Größe rechts daneben.
- BIF: auch »New File…« im Explorer und die Geschichte mit `has_key: true`
  unter »State« kommen aus Rezepten; die Seite hat kein Bild mehr von Hand.
- Die Entwürfe »Autostereogramme« und »Anaglyphen« binden »Terminal öffnen«
  aus dem Handbuch ein, statt den Text mit eigenen (fehlenden) Bildern zu
  wiederholen; »Feueranimation« zeigt den leeren Workspace aus dem Handbuch.
- Grundlagen: der Verzeichnisbaum kommt aus einem Rezept, ein Ausschnitt des
  Explorers mit der Shower-Vorlage. Die Seite hat jetzt kein Bild mehr, das
  von Hand gemacht ist.
- Screenshot-Generator: mehrere `crop-element`-Zeilen schneiden auf den
  Kasten um alle ihre Elemente zu (die Kopfzeile des Explorers und seine
  Zeilen).
- Screenshot-Generator: `move-mouse` findet auch Text in einem Webview, und
  `type` schreibt in ein Feld darin.
- Die Symbole für Panel, Erweiterungen und »Ausführen« im Text der
  Sprachseiten und für »Build« und »View« von LaTeX Workshop im Handbuch
  sind SVGs aus der Symbolschrift von VS Code statt kleiner Bildausschnitte:
  scharf in jeder Größe.
- Ein Hinweis in einem eingebundenen Teil des Handbuchs hat keinen eigenen
  grauen Hintergrund mehr, sondern den des Kastens.
- Bilder im SVG-Format bleiben SVG; bisher wurden sie wie Fotos umgewandelt
  und kamen mit dem falschen Typ beim Browser an.
- »Hades in der Altgriechisch-App« ist ganz entfernt; die Seite stand schon
  nicht mehr im Katalog.
- Kontextzeilen auch in BIF, »Taschenrechner«, GIF, TIC-80, Hugo und
  »Pixelwand« (Ruby und Python) und auf den Seiten der Sprachen: beim Shebang
  und beim absichtlichen Tippfehler. Wo man den Code selbst schreibt
  (Taschenrechner, GIF, TOTP), zeigt der Kontext nichts aus der Lösung.
- Der Screenshot-Generator lässt Kontextzeilen weg, wenn ein Rezept Code von
  der Seite tippt oder in eine Datei schreibt (`previous-code`, `code:`).
- `src/scripts/code-context.rb` schlägt Kontextzeilen für die Code-Blöcke
  einer Seite vor, aus den Dateien, die ihre Rezepte Stufe für Stufe
  schreiben (docs/aktivitaet-schreiben.md).
- Kontextzeilen auch in »Svelte«, »Eine Facharbeit mit LaTeX« und »Statische
  Webseiten mit HTML & CSS«: Jedes Stück Code zeigt, wo es in der Datei
  hinkommt. In LaTeX stehen die Pakete am Ende der Präambel, wie man sie
  ergänzt, und die Überschriften der Gliederung sind schon da. Der Block der
  Selbstständigkeitserklärung enthielt versehentlich die ganze Arbeit noch
  einmal; jetzt nur die Erklärung. Durchführung und Messwerte sind zwei Blöcke,
  damit jeder ganz kopiert. In Svelte kommen die Schriften in `app.html` unter
  die `<meta>`-Zeilen, wo sie in der fertigen Datei stehen.
- Die raylib-Vorlage sagt Editor-Erweiterungen für C (clangd, C/C++), wo
  `raylib.h` liegt; `#include <raylib.h>` ist nicht mehr rot unterstrichen.
  Der Patch für `specht/raylib-starter` liegt bei.
- Handbuch »Farbräume«: Die Körper passen in jeder Drehung ganz ins Bild; der
  HSV-Kegel ist kleiner, und bei CIELab und YCbCr ragt keine Ecke mehr hinaus.
  Ziehen dreht um die Achsen des Bildschirms: Was vorne liegt, folgt dem
  Zeiger, auch wenn die Oberseite schon zum Betrachter gekippt ist.
  Losgelassen dreht der Körper kurz weiter und läuft aus (nicht bei »weniger
  Bewegung«). Der Knopf »In 8 × 8 × 8 Würfel teilen« heißt »Zerteilen«: Im
  Farbraum sind die Stücke meist keine Würfel mehr. Die Körper sind am Anfang
  ganz; das Viertel nimmt erst ein Klick heraus.
- Der Sprachwechsel auf Seiten in Ruby und Python ist ein Umschalter mit den
  Logos der beiden Sprachen.
- Das Widget zum Entpacken in »LZW« markiert den Code, der gerade gelesen
  wird, und zeigt die Codes in Festbreitenschrift.
- Die Bilder der Knöpfe »Maximize Panel«, »Kill Terminal« und »Go Live«
  entstehen aus Rezepten, mit Mauszeiger und Tooltip über durchsichtigem
  Grund (`keep-only` im Screenshot-Generator).
- »Nach oben« in der Leiste über einer Seite ist ein runder Knopf.
- Die Karten der Grafik-Aktivitäten nennen die Plattform mit der Sprache,
  etwa »TIC-80 / Lua«, »PixelRAM / C« oder »Pixelflow Canvas / Ruby«; BIF
  steht als »Markdown / JavaScript« da.
- »Einführung in SQL« ist zweigeteilt: Der zweite Teil, mit `GROUP BY`,
  `JOIN`, den Amtssprachen und den Entfernungen, ist jetzt eine eigene
  Aktivität, »SQL: Gruppieren und Verbinden«, im Katalog direkt danach.
  Was Lernende dort schon gelöst hatten, zieht beim Start mit um. Beide Seiten
  haben eine Notiz für Lehrkräfte, die auch die Musikdatenbank in
  `workspace-files` nennt. »SQL Injection« steht jetzt unter »Datenbanken«.
- Die Karten der beiden SQL-Seiten zeigen ihr ER-Diagramm in
  Krähenfuß-Notation statt eines Ausschnitts aus dem Terminal, die
  Filmdatenbank die ER-Werkstatt mit dem ganzen Modell. Die Diagramme macht
  `src/scripts/er-teaser.cjs` aus einem `.er`-Modell, hell und dunkel.
- Befehlsaufgaben laufen dort, wo die Seite gerade arbeitet (`dir` in
  `tasks.yaml`): In »Ein Krimi im Terminal« schreibt man `grep '^HINWEIS'
  tatort.txt` wie im eigenen Terminal, ohne `bitstein/` davor; mit geht es
  weiterhin. Die Karte sagt, in welchem Ordner der Befehl läuft.
- »Ein Krimi im Terminal«: Der Abschnitt »Ein eigener Fall« mit seiner
  Abgabe ist weg. Das Bild vor der Aufgabe »Wer ist noch da?« verriet die
  Antwort; es zeigt jetzt nur noch `who`, was der Eindringling tut, finden
  Lernende selbst mit `ps aux` heraus. Der Spickzettel kennt `scp` mit `*`.
- Huffman: Die beiden Arrays einer kanonischen Tabelle heißen wie in der
  JPEG-Norm `bits` und `huffval`, mit dem Beispiel ausgeschrieben; die
  Musterlösung des JPEG-Decoders benutzt dieselben Namen.
- »Ein Krimi im Terminal«, Beweise vom Server: Der eigene Zugang zum
  Museumsserver liegt als Zettel der Kommissarin in den Akten
  (`bitstein/zugang-museum.txt`), wenn man sie im eigenen Workspace
  herunterlädt; die Seite zeigt keine Umgebungsvariablen mehr. Auf die
  Alarmanlage kommt man nur mit dem Passwort der Wartung, das Lernende im
  Übergabeprotokoll auf dem Museumsserver finden (neue Aufgabe). Dort ist
  außerdem noch jemand angemeldet: `who` und `ps aux` zeigen `wartung` von
  Reuters altem Laptop, der gerade versucht, die `.bash_history` zu löschen
  (neue Aufgabe). Für Betreibende: die beiden Images des Fallservers neu bauen
  (`./config.rb build case_museum case_alarm`); die Akten liefert jetzt die
  Website selbst aus.
- Wer sich als Testkonto (`name+test@…`) anmeldet, bekommt den Anmelde-Code
  an die eigene Adresse ohne `+test`, mit dem Hinweis, dass er für das
  Testkonto ist.
- Startseite auf dem Tablet (zwei Spalten): Steht eine Karte allein in der
  letzten Reihe ihres Bereichs, nimmt sie die ganze Breite ein, mit dem Bild
  links und dem Text daneben, statt eine halbe Reihe leer zu lassen.
- Aufgaben, deren Lösung direkt davor stand, fragen jetzt etwas Eigenes:
  - Einführung in SQL: Die Beispiele zu `GROUP BY`, `SUM` und `HAVING` zählen
    Straßenseiten und Städte pro Land statt der Länder pro Kontinent, die die
    Aufgaben danach erfragen. »Die größten zuerst« sortiert nach der Fläche,
    »Ein Kontinent ohne Länder« sucht mit `LEFT JOIN` Antarktika, und »Die
    Reihenfolge« ist eine Vermutung vor der Erklärung.
  - Filmdatenbank: Das Beispiel zählt die Genres jedes Films, die Aufgabe die
    Filme jedes Genres.
  - Neo4j-Filmdatenbank: Die beiden Fragen nach `IN_GENRE` und `DIRECTOR` sind
    Vermutungen, bevor das Modell zu sehen ist.
  - Git: Die Aufgabe zum Rückgängigmachen verbindet zwei Zeilen der Tabelle.
  - Sandboxen: Gefragt ist, warum `git add DEIN_NAME/` und nicht `git add -A`.
  - Zwei Faktoren: gefragt sind die Bytes eines Schlüssels mit 16 Zeichen,
    nicht die 20 aus dem Text davor.
- »Einführung in SQL«: Klar getrennt, wo Abfragen hingehören. Ausprobiert
  wird in `mycli`. Fragt eine Aufgabe nach einem Wert (wie viele Länder, wie
  heißt die Währung), trägt man nur das Ergebnis ein; sieben Aufgaben sind
  dafür von Abfrage- zu Zahl- und Textaufgaben geworden, mit Rückmeldungen für
  die typischen falschen Werte. Sollen Zeilen ausgegeben werden, kommt die
  fertige Abfrage in die Karte und wird dort geprüft. Wer eine dieser sieben
  Aufgaben schon gelöst hatte, löst sie neu.
- Testkonten: Jede Lehrkraft hat genau ein Testkonto in einer eigenen
  Testgruppe, auch Lehrkräfte mit `*` und Admins. Niemand sieht die
  Testgruppen anderer; die Gruppenauswahl von Admins ist nicht mehr voll
  davon. Neu: »Testkonto zurücksetzen« auf der Klassenseite (Workspace,
  Datenbanken, Antworten und Abgaben des Testkontos, sonst nichts). Im Profil
  können Lehrkräfte ihren eigenen Gruppen und dem Testkonto eine
  Leistungsüberprüfung zuweisen, nicht mehr nur Admins. Lehrkräfte mit `*`
  sehen jetzt den Menüpunkt »Klassen« mit ihrer Testgruppe.
  Der Einstieg für Lehrkräfte erklärt, wie man ins Testkonto kommt (Nachahmen
  oder direkt mit `name+test@…`), was man damit ausprobiert, bis hin zu
  Leistungsüberprüfungen, und wie man es zurücksetzt.
- »Endliche Automaten«: Die Zustände heißen überall q₀, q₁ und so weiter, in
  den Bildern, im Text, in den Programmen (`'q0'`) und in den Aufgaben;
  fehlende Übergänge führen in den Fehlerzustand q<sub>F</sub>, der genannt,
  aber nicht gezeichnet wird. Die Seite erklärt, warum Automatron seine
  Zustände q, r und s nennt. Wer »Eintritt zwei Euro« mit den alten Namen
  (`zu`, `offen`) gelöst hat, muss die Aufgabe neu lösen.
- »Einführung in SQL«: Nach jedem Import steht ein Kasten »Die Tabellen« mit
  jeder Tabelle, ihren Spalten und ihren ersten Zeilen; im zweiten Teil sind
  die neuen Tabellen markiert und aufgeklappt (`_schema(name, rows, new: …)`).
  Das Beispiel zu den Amtssprachen ist jetzt Finnland, damit es die Aufgabe zur
  Schweiz nicht vorwegnimmt. Neu ist die Frage, warum Brandenburg an der Havel
  nicht unter den Städten nahe Berlin steht: Eine Datenbank ist ein Ausschnitt
  der Welt.
- Query Stepper: In der Abfrage selbst ist der Teil markiert, den der
  gezeigte Schritt ausführt, etwa `WHERE population > 1000000`. In den
  Kästen »Die Tabellen« tragen auch die Spaltenköpfe der Beispielzeilen die
  Zeichen für Primär- und Fremdschlüssel.
- GIF decodieren, LZW zum Durchklicken: Die Tabelle steht von Anfang an da,
  mit den Einträgen 0 bis 3 für die Farben und 4 und 5 für Clear und End, in
  einer eigenen Spalte rechts neben der Erklärung statt darunter und von Anfang
  an so breit wie ihr längster Eintrag. Feine Linien zeigen das Raster der
  16 × 16 Pixel, bevor sie da sind. Die Einträge 4 und 5 heißen dort, wie in der
  Spezifikation, CC (Clear Code) und EOI (End of Information).
- Lauflängenkodierung: Das Ruby-Gerüst gibt das Ergebnis mit `puts` aus
  statt mit `p`, wie in Python mit `print`.
- Mehr Aufgaben werden bei jedem Durchlauf der Screenshot-Rezepte selbst
  geprüft: in der Filmdatenbank das ER-Modell, die Tabellen und alle
  SQL-Abfragen, in der Neo4j-Filmdatenbank alle Cypher-Abfragen, in »Git« die
  fünf Repository-Aufgaben, in BIF die Geschichte Schritt für Schritt und in
  »3D-Grafik mit Anaglyphen« jede Szene-Aufgabe (mit Lösungen in
  `anaglyph/solutions/`).
  Jede Prüfung muss den gelösten Stand annehmen und den Stand davor ablehnen.
  Für Betreibende: `recreate-screenshots.rb` bricht ab, wenn eine Prüfung
  nicht mehr zu ihrer Seite passt.
- shower.js: »Eine andere Adresse« ist jetzt eine Vermutung mit vier
  Antworten statt einer Adresse zum Eintippen. Die alte Aufgabe ließ sich
  ohne eigenen Rechner nicht lösen und mit `file://…index.html` erraten.
  Die Lösung lädt ein, es mit einem eigenen Rechner auszuprobieren.
- Alle Bilder haben einen Alt-Text, der sagt, was zu sehen ist: rund 450
  Screenshots auf den Seiten der Programmiersprachen, in »Git«, »Mit Dateien
  arbeiten«, im KI-Experiment mit Ziffern und anderswo, die bisher keinen
  hatten. Für Lernende mit Screenreader beschreiben sie jetzt Terminal,
  Editor und Fehlermeldungen. Ein Test (`alt_text_test.rb`) sorgt dafür,
  dass es so bleibt.
- Die ER-Werkstatt (0.3.0) ordnet »Krähenfuß« und »Tabellen« in Schichten,
  wie Mermaid es tut: Worauf andere Tabellen verweisen, steht oben, die
  Verbindungstabellen unten, und die Reihenfolge hält Kreuzungen gering.
  Die Linien sind Kurven, die senkrecht aus dem Kasten kommen, mit dem
  Krähenfuß oder dem Strich gerade an der Kante; zwei Fremdschlüssel auf
  dieselbe Tabelle sind zwei Linien, und eine lange Linie läuft um die
  Kästen dazwischen herum. Die Kästen zeigen Typ, Name und Schlüssel in drei
  Spalten. Verschobene Kästen bleiben, wo sie sind; »Neu anordnen« zeigt die
  neue Anordnung. Eine Beziehung mit sich selbst hat eine Schleife mit Platz
  für Krähenfuß und Strich. Bei Chen läuft keine Linie mehr durch einen
  Kasten oder eine Raute, und Kardinalitäten liegen nicht mehr aufeinander.
  Unter dem Diagramm steht eine Zeile pro Entitätstyp oder Tabelle (Schlüssel
  unterstrichen, Fremdschlüssel mit ↑); »Kopieren« legt sie als Text in die
  Zwischenablage. Die Bilder der »Filmdatenbank« und die Vorschau im Handbuch
  »ER-Modelle« zeigen die neue Darstellung.
- Die »Einführung in SQL« arbeitet mit den Ländern der Welt statt mit der
  alten Datenbank `terra1`, die Fehler hatte und deren Zahlen veraltet
  waren (Einwohner in Millionen, `Europa` neben `Europe`). Lernende laden
  `country-archive/mysql.sql`, sehen vier Tabellen mit Primär- und
  Fremdschlüsseln und erleben, wie ein Fremdschlüssel einen Kontinent
  ablehnt, den es nicht gibt. Die Aufgaben gehen auf die neuen Tabellen;
  wo sich die Frage geändert hat, ist es eine neue Aufgabe (zum Beispiel
  »Nördlich von Hamburg« statt »Westlich von Aachen«, neu »Hauptstädte im
  Süden«). Wer `terra1` schon importiert hat, lädt die neuen Daten, sonst
  meldet »Prüfen«, dass die Beispieldaten fehlen. `terra1.sql` bleibt in
  `specht/workspace-files` für alte Links.
- Der Query Stepper rechnet mit den Ländern der Welt statt mit `terra1`,
  mit neuen Beispielen für Hauptstädte, Nachbarländer und `LEFT JOIN`.
- Die READMEs der Film- und der Musikdaten zeigen die Tabellen als Schema
  und sagen, was die Spalten bedeuten.
- Die Widgets zum Durchschalten heißen nicht mehr »Schrittmacher«, das an
  einen Herzschrittmacher denken ließ, sondern »Zeitlupe«; das Werkzeug für
  SQL heißt »Query Stepper«, wie seine Adresse `/query-stepper`.
- Die TCP-Konsole scrollt nicht mehr in einem Kasten innerhalb der Seite:
  Das Protokoll wächst mit dem Gespräch, nur die Seite scrollt, und die
  Zeile zum Senden bleibt unten im Fenster. Neue Antworten ziehen die Seite
  mit, solange man unten ist; wer nach oben gescrollt hat, um zu lesen,
  bleibt dort und bekommt einen Knopf »Neue Antwort«.
- Das Bild der Startseite auf der Karte »Was ist der Hackschule
  Workspace?« zeigt die Überschrift in der Mitte des Kopfes: Die Zeile mit
  dem Sponsor fehlt jetzt samt ihrem Platz, und die Karte schneidet das Bild
  von oben zu.
- Die Prüfungen in Ruby und Python laden von der Datei der Lernenden nur
  die Definitionen: Methoden und Funktionen, Klassen, Konstanten, Importe
  und was eine Tabelle füllt. Das Programm unten in der Datei läuft beim
  Prüfen nicht, auch ohne `if __FILE__ == $0` oder `if __name__ ==
  '__main__':`. Der Rechner für Ausdrücke, Huffman, LZW, RLE, Steganographie
  und TOTP kommen darum ohne diese Zeile aus; die Programme stehen einfach
  unten in der Datei. Nur »Endliche Automaten« behält sie, weil dort ein
  Programm die Datei eines anderen lädt.
- Im Text heißt die Zählvariable einer Schleife oder die Position in einem
  Array »Index«, nicht »Stelle« (Rechner, TOTP, GIF, JPEG, Feuerwerk).
- Bilder nebeneinander sind überall auf der Website Galerien derselben
  Art: jedes Bild mit Alt-Text und Unterschrift darunter, groß zu öffnen mit
  einem Klick (außer es ist ein Link). Die Bitebenen und die Tiefen in
  »Geheime Botschaften in Bildern« und die Kanäle auf der Handbuch-Seite
  »Farbräume« sind einzelne Bilder mit Unterschrift statt eines langen
  Bildes; die Grundlagen der JPEG-Seite, die Themes von BIF, die Kits in
  »Anaglyph 3D«, die Filterkernel im Feuer und das Vorher-Nachher in der
  statischen Website haben Unterschriften und Alt-Texte bekommen. Für
  Autorinnen und Autoren: `<div class='gallery'>` mit `columns-2` bis
  `columns-4`, `portrait` und `tiles`, beschrieben in
  docs/aktivitaet-schreiben.md und geprüft von `gallery_markup_test.rb`.
- »Geheime Botschaften in Bildern« gibt den Farbklassen eine Aufgabe, die
  nur sie können: Würfel der Breite 3 oder 5 schlüpfen am Chi-Quadrat-Test
  vorbei, weil er Paare von Werten vergleicht und sie Gruppen von drei oder
  fünf gleich machen. Die Lernenden sagen das voraus (»Am Test vorbei?«),
  verallgemeinern ihren Test auf Gruppen (»Gruppen statt Paare«) und finden
  damit die Breite in einem zweiten Fundstück, den Möwen mit dem Codewort
  LEUCHTTURM. Dazu das Prinzip von Kerckhoffs.
- »Geheime Botschaften in Bildern« endet mit den Farbklassen im
  RGB-Würfel; der Teil zu CIELab mit den Aufgaben »Nach Lab« und »Mehr,
  als das Auge merkt« ist entfallen. Wer Klassen in einem anderen
  Farbraum bauen will, findet unter »Wie geht es weiter?« den Weg ins
  Handbuch »Farbräume«.
- Startseite: Die Programmiersprachen stehen zu viert in einer Reihe (auf
  dem Handy zu zweit), mit dem Logo über die ganze Breite der Kachel im
  Format 16:9 und Name und Jahr in einer Zeile darunter. Damit die Reihen
  aufgehen, sind COBOL und Smalltalk zurückgestellt; ihre Seiten bleiben
  über einen direkten Link erreichbar.
- »Geheime Botschaften in Bildern« hat auf seiner Karte ein Foto, das
  harmlos aussieht: die Möwen, mit der Botschaft darin, statt der
  aufgedeckten Bitebene.
- Das Foto der Strandhäuser in »Geheime Botschaften in Bildern« und auf
  der Handbuch-Seite »Farbräume« ist ein neuer Ausschnitt ohne Menschen:
  Himmel, Giebel, eine rote und zwei hellblaue Türen. Die Bilder, die
  daraus entstehen (Fundstück, Bitebenen, Kanäle, Screenshots), sind neu
  gerechnet, ebenso die Werte in Text und Aufgaben: das erste Pixel und der
  Chi-Quadrat-Test, jetzt in Streifen von 40 Zeilen. Wer das Fundstück schon gelöst hat, findet
  darin weiter dasselbe Codewort.

- Der Baumzeichner beim Rechner für Ausdrücke behält seine Kanten, während
  sich der Baum umordnet: Jedes Bild der Bewegung wird ganz gezeichnet,
  Knoten und Kanten zusammen. Was ausgerechnet wird, gleitet samt seinen
  Kanten in den Knoten, der daraus wird; zurück kommt es wieder heraus.
  Bisher verschwanden die Kanten, bis die Knoten an ihrem Platz waren.

- Kleine Zeichnungen stehen in ihrer eigenen Größe auf der Seite, statt auf
  die ganze Breite gezogen und unscharf zu werden: der Baum und der
  Zahlen-Automat beim Rechner für Ausdrücke, der Automat des Zahlenschlosses
  bei den Endlichen Automaten. Ihre Rezepte zeichnen sie mit `zoom: 2`; die
  Seite zeigt sie mit `<img class='drawing' style='width: …'>`
  (`default.css`). Die Vergleichsbilder der Steganographie sind doppelt bis
  viermal so groß, mit scharfen Pixeln.

- Huffman: Die Blätter im Baum sind jetzt rund wie die inneren Knoten, leicht
  getönt, damit man sie unterscheidet. So endet jede Kante sichtbar in der
  Mitte eines Kreises statt an der Ecke eines Kästchens; das Bild auf der
  Karte ist mit dem neuen Baum erzeugt.

- `check-task: ID <- ANTWORT` in Screenshot-Rezepten trägt eine Antwort in
  die Karte ein, bevor geprüft wird: einen Befehl, eine Zahl, eine Auswahl.
  So prüfen Rezepte auch Befehls- und Antwortaufgaben. Beispiele in
  `task-examples.yaml` gibt es jetzt auch für Befehlsaufgaben (`command`,
  ausgeführt mit dem `sh` des Rechners); `archive:` entpackt dafür ein
  Archiv der Aktivität in den Ordner des Beispiels.

- Ruby- und Python-Prüfungen von Aufgaben laden Gems und pip-Pakete, die
  Lernende in ihrem Workspace installiert haben (`gem install`,
  `pip install`). Bisher lief Ruby ohne Gems, Python ohne die Pakete im
  Benutzerverzeichnis.

- GIF: Die Zeitlupe der Bilddaten zeigt statt der Bytes in Hex einen
  Strom aus Bits, wie die Seite ihn erklärt (das erste Byte rechts, gelesen
  von rechts nach links), mit Bytegrenzen und dem Hexwert über jedem Byte.
  Gelesene Bits sind blass, jeder zweite Code getönt, der aktuelle Code gelb
  mit seinem Wert darunter, und das Fenster gleitet beim Weiterschalten mit.
  Neue Pixel tauchen im Bild auf, der neue Eintrag der Tabelle leuchtet kurz.
- Huffman: Beim Weiterschalten gleiten die beiden leichtesten Bäume
  zusammen, rutschen eine Ebene tiefer, und ihr neuer Knoten erscheint
  darüber; zurück geht es genauso. Wer im System »Bewegung reduzieren«
  eingestellt hat, sieht beide Widgets ohne Animation.
- Alle Widgets zum Durchschalten lassen sich mit den Pfeiltasten bedienen
  wie bisher nur der JPEG-Decoder: der Huffman-Baum, die beiden Zeitlupen
  von LZW und die von GIF. Bei zwei Widgets auf einer Seite schaltet die
  Taste das, das gerade im Fenster steht; im Textfeld eines Widgets bewegt sie
  weiter den Cursor.
- Widgets zum Durchschalten heben sich vom Text ab: ein heller Streifen mit
  einer Linie links und darüber »Zum Ausprobieren · ← → schalten weiter«. So
  sieht man, wo ein Widget anfängt und wo es aufhört.
- LZW nennt das, was der Kompressor gerade aufbaut, jetzt **Präfix** statt
  »Stück«, auf der Seite, in den Aufgaben und in der Tabelle der
  Zeitlupe.
- »Von Hand« bei LZW und bei Huffman ist jetzt eine Vermutung: erst auf
  Papier, dann in der Zeitlupe oben nachsehen, die die Antwort ja zeigt. Die
  Prüfung bleibt dieselbe.
- Die drei Sandboxes nennen ihre Sprachen neben der Art: »Projekt (Dart /
  Python / JavaScript / Java)« für Datenstrukturen und Sortieren, »Projekt
  (Assembler / C)« für die CPU Sandbox.
- Das Bild der Endlichen Automaten auf der Startseite zeigt statt eines
  Automaten mit 16 Zuständen den kleinsten DEA, der prüft, ob eine
  Dezimalzahl durch 3 teilbar ist: drei Zustände für die Quersumme modulo 3,
  größer und ohne Binärzahlen zu lesen.
- Jede Prüfung einer Aufgabe kann Beispiele haben (`task-examples.yaml` neben
  der `tasks.yaml`): eine Lösung, die sie annehmen, und einen unfertigen
  Stand, den sie ablehnen muss. `task_examples_test.rb` führt sie durch
  dieselbe Prüfung wie der Knopf »Prüfen«, dazu jede Auswahl- und Zahlaufgabe
  mit ihrer eigenen Lösung: 675 Fälle in 31 Aktivitäten. Jede Prüfung von
  Dateien oder Programmen hat jetzt Beispiele, und eine neue braucht welche;
  für die Endlichen Automaten liegen dafür Musterlösungen in `loesung/`.
- `page_code_test.rb` prüft, dass der Code auf einer Aktivität in den Dateien
  steht, die ihre Screenshot-Rezepte schreiben. So kann der Code auf der
  Seite nicht unbemerkt von dem abweichen, was Bilder und Beispiele testen.
- Screenshot-Rezepte prüfen auf Wunsch, ob die Schritte einer Seite aufgehen:
  `check-task` führt die Prüfung einer Aufgabe im Workspace des
  Screenshot-Kontos aus wie »Prüfen«, `check-task-fails` verlangt, dass sie
  an dieser Stelle noch scheitert, `expect-no-text` schlägt bei einer
  Fehlermeldung an, und `<!-- tutorial-step … -->` beschreibt Schritte ohne
  Bild. Solche Prüfungen ändern keine Bilder. Die Pixelwand und Daten aus dem
  Netz prüfen ihre Aufgaben jetzt so, in beiden Sprachen.
- `pip install` funktioniert im Workspace wie `gem install`: Pakete landen
  ohne Administratorrechte in `/workspace/.local` und bleiben über einen
  Neustart erhalten; `python3` findet sie ohne virtuelle Umgebung. Bisher
  lehnte pip jede Installation außerhalb einer virtuellen Umgebung ab
  (»externally-managed-environment«). Virtuelle Umgebungen funktionieren
  wie zuvor. `mysql.connector` für Python kommt jetzt in Version 26.7 aus
  pip statt aus Ubuntu; die alte Fassung 8.0.15 konnte sich unter Python
  3.12 nicht bei MySQL 8.4 anmelden. Betreibende bauen das Workspace-Image
  neu (`./build-image.sh`); laufende Workspaces bekommen die Änderung beim
  nächsten Start.
- Startseite für alle, die nicht angemeldet sind: Statt Begrüßung und vier
  Textkacheln ein kompakter Kopf mit einem Satz, was der Workspace ist, einem
  Link zur Anmeldung und einem Bild des Workspaces
  (ein Rezept auf der Seite »Was ist der Hackschule Workspace?«). Der Katalog
  beginnt gleich darunter. »Anmelden« bleibt oben im Menü.
- Startseite neu geordnet: erst, was der Workspace bietet, dann das Werkzeug:
  Erste Schritte, Grafik & Spiele, Web, Netze & Sicherheit (TCP/IP vor dem
  Webserver, Pixelwand, Kryptographie, SQL Injection), Datenbanken, Wie
  Computer rechnen (die Sandboxes, Automaten, neuronales Netz, JPEG), Texte &
  Präsentationen, Terminal & Git, Programmiersprachen. Die Abschnitte
  »Experimente« und »Sandboxes« gehen darin auf.
- Die Programmiersprachen stehen als kleine Kacheln mit Logo, Name und Jahr
  da statt als große Karten (`tiles: true` in `sections.yaml`); alle 18 sind
  auf einen Blick zu sehen.
- Endliche Automaten: Die Bilder aus Automatron gibt es auch im dunklen
  Design.
- Startseite: Alle Karten sind gleich groß, drei nebeneinander (zwei auf dem
  Tablet); ein Abschnitt endet, wo er endet, statt die letzten Karten zu
  strecken. Die Art (»Projekt (Ruby)«, »Labor« …) steht über dem Titel und ist
  damit immer zu sehen, auch bei langen Titeln. Auf dem Handy ist jede Karte
  eine kompakte Zeile mit kleinem Bild; die Seite ist weniger als halb so
  lang, und nichts ist versteckt. Die Fußzeile liegt am Ende der Seite statt
  über dem Inhalt (in der Entwicklung bleibt sie fest, wegen der Alt-Text-
  Prüfung).
- Automatron und die TCP-Konsole brauchen eine Anmeldung; ohne geht es zur
  Anmeldeseite. Seiten zeigen ein Werkzeug als Kasten mit »Öffnen« dort, wo
  es gebraucht wird (`_tool(name)`, `docs/aufgaben.md`), und öffnen es in
  einem neuen Tab, ebenso jeden anderen Link darauf. TCP/IP, Webserver,
  Endliche Automaten und das Handbuch »HTTP« haben solche Kästen.
- Automatron: Erklärung und Eingabe oben, die Beispiele darunter, nach
  Themen geordnet und einklappbar; nach dem Klick rollt die Seite zum
  Ergebnis, während der Rechnung zeigt der Knopf »Rechnet …«. Jeder Schritt
  sagt, wie seine Zustände heißen. Tippfehler im Ausdruck (ein unbekanntes
  Zeichen, offene Klammern, ein `|` ohne etwas dahinter) bekommen eine
  verständliche Meldung statt eines Serverfehlers. Breite Tabellen rollen auf
  dem Handy für sich; die vereinfachte Grammatik erscheint nur, wenn sie
  anders ist. Alles auf Deutsch.
- TCP-Konsole: Host, Port, TLS und »Verbinden« stehen in einer Leiste wie
  eine Adresszeile; verbunden wird daraus ein roter »Trennen«-Knopf.
- Pixelwand: Die Wand passt sich der Fenstergröße an, in der Breite und in
  der Höhe, von kleinen Vorschaufenstern bis zum Beamer (höchstens 960 Pixel
  breit). Das Gitter wächst mit; die Farbknöpfe stehen in der Mitte.
- Automatron zeichnet schöner: in der Schrift der Seite, mit tiefgestellten
  Nummern (s₀ statt eines losen »s 0«), Bereichen wie `0, 2–9` statt langer
  Listen, Farben für das helle und das dunkle Design und akzeptierenden
  Zuständen in Grün. Kleine Automaten stehen wie im Schulbuch in einer Reihe,
  nur die Übergänge zurück laufen als Bögen.
- Screenshot-Generator: `crop-element` scrollt ein Element unterhalb des
  sichtbaren Bereichs erst ins Bild, statt eine leere Fläche aufzunehmen.
- Sicherheit: Befehle, die Lernende in eine Aufgabe vom Typ `command` tippen,
  laufen nicht mehr als die Person, sondern als Gast (`nobody`): Sie können
  `/workspace` lesen, aber nichts darin ändern oder löschen.
- SQL-Aufgaben: Das eigene Ergebnis steht immer sichtbar als Tabelle unter
  der Rückmeldung. Gefragt sind genau die verlangten Spalten (`extra_columns`
  ist jetzt standardmäßig `wrong`); Fehlermeldungen von MySQL sind auch im
  hellen Modus lesbar und werden umgebrochen.
- Keine Browser-Popups mehr: Löschen fragt in einem eigenen Dialog nach,
  Fehler erscheinen als Meldung auf der Seite.
- Die Kopfleiste hat einen starken Knopf (»Workspace öffnen« bzw.
  »Anmelden«); alle anderen sehen gleich aus, die aktuelle Seite ist markiert.
  Das Profil steht rechts und zeigt den Namen oder die E-Mail-Adresse.
- Die Startseite begrüßt nicht mehr mit »Hallo …«; über den eigenen
  Aktivitäten steht schlicht »Deine Aktivitäten«. Wer angemeldet ist, steht
  wie bisher rechts in der Kopfleiste.
- Das Handbuch zeigt alle Kapitel als gleich große Karten mit ihren
  Überschriften als Links, ohne Bilder, und hat ein Suchfeld. Überschriften auf
  Handbuch-Seiten haben Anker.
- SQL-Aufgaben prüfen nicht, wenn die Musterabfrage nichts findet (meist
  fehlen dann die Daten); sie sagen das und wie man die Daten lädt.
- Die drei Sandboxes haben einen eigenen Abschnitt im Katalog statt
  »Terminal & Git«.
- Sprünge zu einer Aufgabe (»Weitermachen«) landen unter der Abschnittsleiste.
- »Eine Präsentation in HTML erstellen« ist kürzer: Der Referenzteil steht
  jetzt unter `/reference/html-folien`. Alte Links auf Abschnitte der Seite
  führen dorthin weiter. Die Mini-Folien liegen in
  `src/content/reference/html-folien/screenshots/`; `screenshot-range.mjs`
  schreibt dorthin.
- Eine Seite ist immer `<slug>.md` aus ihrem Ordner. Bisher war es die erste
  `.md`-Datei; bei allen vorhandenen Seiten ist das dieselbe.
- Abgaben mit `pick`: Die Liste der Dateien wird neu gelesen, sobald man
  sie anklickt oder zur Seite zurückkehrt. Eine eben im Workspace angelegte
  Szene steht also gleich darin; die Auswahl bleibt, solange die Datei da ist.
- Tutorial-Screenshots werden seltener neu erzeugt: Ein neu gebautes
  Workspace-Image zählt nur noch, wenn sich die code-server-Version ändert,
  und Zuschnitt oder Dateiname eines früheren Rezepts machen spätere nicht
  mehr ungültig. Bestehende Manifeste bleiben gültig.
- Anaglyphen: »Die Latte« heißt jetzt »Der Balken« (`balken.ini`, Aufgabe
  `balken`). »Sonne, Erde, Mond« verlangt eine gelbe Sonne, eine blaue Erde,
  einen weißen Mond und einen schwarzen Hintergrund statt `name = …`, das das
  Tutorial nie eingeführt hat. Wer `latte` schon gelöst hat, sieht die
  Aufgabe wieder offen.
- Filmdatenbank: Statt »The Matrix« dient »Back to the Future« (1985) als
  Beispiel, im Datensatz vom Anfang und in den JOIN-Aufgaben (jetzt
  `genres-film` und `besetzung-film`).
- ER-Werkstatt: Im Chen-Diagramm stehen die Attribute gleichmäßig um ihre
  Entität, zwischen den Linien zu den Beziehungen und in der Reihenfolge der
  Datei. Die Vorschau zoomt (−, +, »Einpassen«, Strg + Mausrad) und springt
  beim Tippen und Ziehen nicht mehr: Die Ansicht bleibt, Kästen bleiben
  stehen, nur neue werden platziert.
- Profil: Der Hinweis nennt die Umgebungsvariable richtig, `NEO4J_DATABASE`
  statt `NEO4J_DB`.
- ER-Werkstatt 0.2.0: Der Primärschlüssel heißt im Modell `primary key`, wie
  in SQL (`id: int, primary key`). `key` allein funktioniert weiter; die
  Erweiterung schlägt dann `primary key` vor. Filmdatenbank, Handbuch und
  Prüfart `er` sprechen von `primary key`. Der Text zum Krähenfuß sagt, warum
  das erste Diagramm keinen Strich zeigt, und zeigt ihn im Tabellendiagramm.
- Code in Aufgabenkarten ist eingefärbt wie im Text der Seite; bisher sah ein
  Code-Block in einer Aufgabe aus wie lauter einzelne Inline-Code-Stücke. Ein
  Code-Block ohne Sprache wird wie ```` ```text ```` dargestellt.
- Das Inhaltsverzeichnis in der Seitenleiste zeigt Kapitel mit ihren
  Abschnitten und steht auf jeder längeren Seite.
- Keine Seite ist breiter als ein Handybildschirm.
- Links haben auf der ganzen Website dasselbe Petrolblau.
- Das Handbuch setzt beim Öffnen den Cursor ins Suchfeld (nicht auf
  Touch-Bildschirmen, wo sonst die Tastatur die Seite verdeckt).
- Der Profil-Knopf zeigt ein Ausweis-Symbol.
- Das 2D Game Studio ist ein eigener Spielplatz (2d.hackschule.de) und hat
  keine Seite mehr im Workspace; damit entfällt auch »Lehrmaterial«. Die
  Liste der Aktivitäten mit einer Notiz für Lehrkräfte steht jetzt im Profil
  unter »Notizen für Lehrkräfte«. `/lehrmaterial` führt dorthin,
  `/lehrmaterial-2d` zum Game Studio.
- »Statische Webseiten mit HTML & CSS« und »Eine Präsentation in HTML
  erstellen« holen ihre Erklärungen aus denselben Handbuch-Abschnitten. Wer
  eine der beiden schon gemacht hat, sieht sie in der anderen nur noch als
  eine Zeile. »HTML-Folien gestalten« ist in den neuen Seiten aufgegangen;
  alte Links führen weiter. KaTeX war in der Vorlage nur scheinbar
  eingerichtet: Das Handbuch zeigt jetzt die drei Zeilen für den `<head>`.
- Die kleinen Verweise auf Handbuch-Abschnitte (`_uses`) nennen deren
  Überschrift statt des internen Namens.
- Auf dem Handy rücken Hinweise, Aufgabenkarten und Code-Blöcke weniger weit
  ein; das Symbol eines Hinweises steht im Text statt daneben.
- Jede Karte im Katalog nennt ihre Art (Projekt, Labor, Grundlagen, Übung,
  bei den Programmiersprachen »Steckbrief«), klein rechts neben dem Titel,
  wenn er Platz lässt; bisher hatten nur elf Karten ein Etikett, auf dem Bild.
  Die Kurztexte sind im Blocksatz, die Karten einer Reihe gleich hoch.
- Silbentrennung macht der Browser für jedes Wort; die Liste
  `hyphenation.txt` mit 177 Wörtern ist weg. Code wird nie getrennt.
- Ein Handbuch-Kasten, der auf eine Zeile zusammengeklappt ist, zeigt die
  Adresse zum Klonen trotzdem, mit Kopierknopf.
- Eine Anaglyph-Szene abzugeben braucht keinen laufenden Workspace mehr: Ob
  sie lädt und genug Objekte hat, prüft die Karte mit dem Bild, das sie
  ohnehin zeichnet. Während der Abgabe zeigt der Knopf, dass er arbeitet.
- »Repository klonen« steht einmal im Handbuch und wird in den Tutorials
  eingebunden statt kopiert.
- LaTeX: Die Hilfsdateien beim Übersetzen landen in `.build`; im Ordner
  bleiben nur `.tex` und `.pdf`.
- Startseite: ruhigerer Katalog, beide Filmdatenbanken in einer Reihe,
  »Bobby Tables« vorerst ausgeblendet. »Pixelgrafik programmieren« heißt
  jetzt »Pixelgrafik mit Ruby«, neben »Echtzeitgrafik mit C«.
- Erzeugte statt von Hand gemachte Screenshots in Git, Dateien und
  Verzeichnisse, TCP/IP, LaTeX, MySQL, beiden Filmdatenbanken, den Sandboxes,
  den Grundlagen, Anaglyphen und PixelRAM. »Go Live« ist ein gemeinsames Bild
  für alle Tutorials, mit Mauszeiger und dem Anhalten des Live Servers.
- Behoben: Vier kleine VS-Code-Symbole fehlten seit August auf 17 Seiten
  (Programmiersprachen, LaTeX, Svelte, Pixelgrafik). KaTeX lud auf
  Handbuch-Seiten nicht. Die drei Übungen (»Programmieren üben«) waren seit
  dem Ausblenden nicht mehr erreichbar, obwohl das Profil auf sie verweist;
  ein Abschnitt kann jetzt mit `hidden: true` aus dem Katalog verschwinden,
  ohne dass seine Seiten wegfallen. `/profil` führt Abgemeldete zur
  Anmeldung statt zu einem Serverfehler. Auf dem Handy scrollen breite
  Formeln und Tabellen für sich, statt die Seite zu verbreitern.
- Barrierefreiheit: Die Symbole in Knöpfen sind als Schmuck markiert. Bisher
  las ein Screenreader das Symbolzeichen mit, etwa »\uF116 Neu starten«.
- Erzeugte Screenshots auch für »Statische Webseiten mit HTML & CSS« (alle
  Bilder, mit den Ständen der Beispielseite in `lehrkraft/`) und für die
  geklonte Shower-Vorlage.
- Erzeugte statt von Hand gemachte Screenshots für alle 19
  Programmiersprachen-Tutorials (Python, C, C++, C#, Java, Go, Rust, Dart,
  JavaScript, Lua, Ruby, BASIC, Fortran, COBOL, Pascal, Smalltalk, Lisp,
  Erlang, Assembler). Die Bilder aus August 2024 zeigten ein altes VS Code,
  eine Eingabeaufforderung wie `abc@992fa5e1a3fe` und ältere Versionen der
  Sprachen; jedes neue Bild wurde neben das alte gelegt und zeigt denselben
  Schritt. Die Fehlerbilder zeigen jetzt den Fehler, den der Text beschreibt
  (`hello-error.*`). Wo eine Erweiterung nach »Trust Publisher & Install«
  fragt, sagt der Text das jetzt. Das Assembler-Tutorial hat das fehlende
  Bubblesort-Programm (`nasm/bubblesort.asm`) bekommen.
- »Pixelgrafik mit Ruby« überarbeitet: acht Aufgaben (das erste Programm,
  Koordinaten, Speicher mit Palette, Bézierkurven, zwei Vorhersagen zu
  `flood_fill` und zur Maske, das gespeicherte PNG), ein neuer Abschnitt
  »Dein eigenes Bild« mit Ideen und einer Abgabe von `bild.rb` und
  `bild.png`, eine Notiz für Lehrkräfte und erzeugte Screenshots. Die Seite
  nennt die Meldung, wenn die Zeichenfläche nicht offen ist.
- »Frontend-Entwicklung mit Svelte« auf SvelteKit 3 gebracht (`sv` 1.1,
  Einstellungen in `vite.config.js`), mit neun Aufgaben, einer Abgabe des
  gebauten Ordners `build`, einer Notiz für Lehrkräfte und erzeugten
  Screenshots aller Schritte, von `npx sv create` bis `npm run build`. Die
  Vorschau läuft über den Reiter »Ports«; dafür braucht der
  Entwicklungsserver `host: '127.0.0.1'`, das die Seite erklärt.
- Prüfart `css` kennt `style_tags: true` und liest dann nur den
  `<style>`-Block, etwa einer Svelte-Komponente. `html` versteht
  Svelte-Ausdrücke wie `disabled={state < 3}` in Attributen.
- Behoben: Dateien, die das Keyboard-Tutorial geöffnet hatte, tauchten
  später immer wieder in <kbd>Strg</kbd><kbd>P</kbd> auf. Sie sind jetzt
  ausgeschlossen (`**/.hs-kbd-tutorial/**` in `files.exclude`); VS Code
  nimmt sie beim nächsten Start auch aus dem gespeicherten Verlauf.
- Der Knopf »TIC-80« im Menü zeigt TIC-80s kleinen Computer als Strich-Icon
  wie die übrigen Icons; der Tab von TIC-80 zeigt das bunte Original.
- TIC-80 1.3.1 statt einer Entwicklerversion von Juli, mit Forth als neuer
  Sprache. Speichern im Workspace ist robuster: TIC-80 schickt nur die
  Dateien, die es geändert hat. Hat sich eine Datei außerhalb von TIC-80
  geändert (VS Code, `git pull`), bleibt sie, und der Stand aus TIC-80
  landet daneben, etwa in `spiel (TIC-80).tic`. Bisher blockierte jede
  Änderung im Ordner `TIC-80` alle weiteren Speicherungen, bis man neu lud,
  und das verlor den Stand im Browser. Was außerhalb geändert wurde, sieht
  TIC-80, sobald das Fenster wieder den Fokus hat. Meldungen auf Deutsch.
  Der CRT-Filter (Röhrenfernseher-Look) ist anfangs aus, damit Code im
  Editor scharf bleibt; in den Einstellungen lässt er sich einschalten.
- Git-Aufgaben (»Git kennt dich«, »Committet und gepusht« …) prüfen auch,
  wenn der Workspace aus ist: in einem Lese-Container mit demselben Image,
  ohne Netz, der Ordner nur lesbar. Bisher hieß es dann »Starte zuerst
  deinen Workspace«, auch auf der Klassenseite.
- »Filmdatenbank als Graph mit Neo4j«: Die Abfrage-Übungen aus den Kästen
  sind jetzt sieben geprüfte Aufgaben. »Toy Story« von 1995, nach dem eine
  Übung fragte, gibt es in den Daten nicht; jetzt ist es »The Lion King« von
  1994 (es gibt auch eine Fassung von 2019). Statt der »zehn Personen mit den
  meisten Filmen«, bei denen Platz 9 bis 11 gleichauf liegen, fragt die
  Aufgabe nach allen mit mindestens 15 Filmen.
- Behoben: Rückmeldungen für typische Fehler (`same_result_as`) griffen bei
  SQL-Aufgaben mit verlangter Reihenfolge nie.
- Auf dem Tablet bleiben die Schalter auf breiten Bildern (CRT-Filter,
  Anaglyph, dunkel) rechts oben stehen, während man das Bild zur Seite
  schiebt; bisher lagen sie am rechten Rand des Bildes, außer Sicht.
- Handbuch »Interaktive Geschichten«: die 17 Abschnitte in sechs Gruppen
  (Eine Geschichte schreiben, Was sich die Geschichte merkt, Ordnung im
  Projekt, Gestalten, Für Fortgeschrittene, Wenn etwas nicht klappt), mit
  Unterabschnitten für »Aussehen anpassen«. Adressen und eingebundene Teile
  bleiben gleich.
- Die Verweise ins Handbuch unter Abschnitten (»Mehr zum Text auf Folien:
  Überschriften und Absätze …«) sind kleine Knöpfe im Türkis des Handbuchs
  mit dessen Buch-Symbol statt beiger Pillen.
- Die Leiste mit den Kapiteln, die oben erscheint, sobald die Kopfzeile aus
  dem Bild ist, hat links »Nach oben« (Chevron): zurück zur Kopfzeile mit
  dem Weg zur Startseite und dem ganzen Menü. Die Linien unter den Kapiteln
  gehören jetzt zu ihren Einträgen und bleiben auch beim seitlichen Scrollen
  darunter; an einem Rand, hinter dem noch Kapitel kommen, blendet die
  Leiste weich aus. Solange sie verborgen ist,
  springt Tab nicht hinein.
- Klassenseite: »Zurück« im Browser schließt die Anmelde-Codes und den
  Abgabe-Viewer, statt die Seite zu verlassen. »Vorlage aktuell?« steht auf
  einer Linie mit »Jetzt nachsehen«.
- C#: Die Seite übersetzt und startet Programme mit `dotnet run Datei.cs`
  (.NET 10) statt mit Mono (`mcs`, `mono`). Workspaces starten mit
  `DOTNET_CLI_TELEMETRY_OPTOUT=1`: .NET schickt keine Nutzungsdaten an
  Microsoft, zeigt kein Begrüßungsbanner und legt kein HTTPS-Zertifikat an.
- Das Profil hat keinen Abschnitt »Notizen für Lehrkräfte« mehr; die
  Notizen stehen oben auf den Aktivitäten. `/lehrmaterial` führt zum
  »Einstieg für Lehrkräfte«.
- Lange Code-Blöcke (ab 20 Zeilen) stehen in derselben etwas kleineren
  Schrift wie die Listings aus `_include_file`.
- Die Kurzzeile eines eingeklappten Handbuch-Kastens zeigt Tasten als
  `<kbd>`, wie im übrigen Text; vorher standen die Tags als Text da.
- Handbuch »Workspace«: neuer Teil »Erweiterung installieren«, mit der Frage
  »Trust Publisher & Install«. »Farben« nennt die RGB-Schreibweise von
  Pixelflow Canvas.
- Inhaltsverzeichnisse aufgeräumt. Handbuch »HTML« (Aufbau einer Seite,
  Text, Bilder) und »CSS« (Grundlagen, Gestalten) gliedern sich in Gruppen;
  »Tastatur-Shortcuts« und »Terminal« haben keine Abschnitte mehr mit nur
  einem Unterpunkt. Bei den Aktivitäten fassen »Vorbereitung« (LaTeX) und
  »TIC-80 kennenlernen« den Anfang zusammen; »Statische Website« ordnet
  »Ein eigenes Bild verwenden« unter HTML ein, Flexbox und kleine
  Bildschirme unter CSS. Anker und Teile bleiben gleich.
- Ein Handbuch-Kasten in einer Aktivität erscheint nicht mehr im
  Inhaltsverzeichnis der Seite; seine Zwischenüberschriften stehen immer
  eine Stufe unter dem Abschnitt, egal wie tief der Teil im Handbuch sitzt.
- SQL- und Cypher-Aufgaben: Das Antwortfeld färbt die Abfrage beim Tippen
  wie die Code-Blöcke der Seite (dieselben Farben, dieselben Schlüsselwörter
  wie Rouge; bei Cypher Labels, Beziehungstypen und Eigenschaften wie in
  den Beispielen). Ein fehlendes Anführungszeichen fällt sofort auf. Das
  Feld wächst mit der Abfrage.
- Ein Test prüft, dass jede Überschrift einer Handbuch-Seite auch als
  Überschrift erscheint: Ein `</table></div>` am Zeilenende ließ Redcarpet
  sonst den Rest der Seite als rohen Text ausgeben.
- Haken sind Symbole aus Bootstrap Icons statt Unicode-Zeichen: in den
  Flag-Spalten des MiniCPU-Handbuchs und bei »Deine Kopie … ist aktuell« auf
  der Startseite. `no_check_marks_test.rb` hält das so (Code ausgenommen).
- MiniCPU-Assembler in Code-Blöcken (```` ```minicpu ````) ist farbig wie im
  Editor der CPU Sandbox: Befehle, Register, Marken, Direktiven, Zahlen,
  Kommentare. Vorher blieben die Blöcke im Handbuch ungefärbt (```` ```asm ````
  kannte die Seite nicht), die Aktivität nutzte das x86-Schema von NASM.
  Ein Test prüft, dass jede Sprache an einem Code-Block bekannt ist; dabei
  fiel ```` ```javascript ```` auf (p5.js), das jetzt wie `js` gefärbt wird.
- Handbuch »MiniCPU« mit Screenshots aus der CPU Sandbox: Register und
  Flags nach `SUB A, B`, der Stack nach `CALL`, Adressen und Bytes im
  Editor, die Takte, die Geräteleiste, der Bildschirm mit seiner Leiste und
  der Reiter »Assembly« für ein C-Programm. Die Programme sind die
  Beispiele auf der Seite. Das C-Beispiel hatte Kommentare hinter
  `#pragma`, die MiniC nicht versteht; sie stehen jetzt darüber.
- Handbuch-Tabellen auf dem Handy: Jede Zeile steht als Block untereinander,
  kurze Werte nebeneinander und mit ihrer Spaltenüberschrift beschriftet
  (»Bytes: 2«). Nichts ist mehr breiter als der Bildschirm.
- Ein Test für die Erweiterungen der Leistungsüberprüfungen braucht kein
  `zip` mehr, das im Ruby-Image fehlt.
- Die Listen der erzeugten Screenshots (`.tutorial-screenshots.json`) sind
  nach Namen sortiert. Ein Rendern in anderer Reihenfolge ändert die Datei
  nicht mehr.
- Profil, MySQL: Jede Datenbank ist eine Karte mit Name, Art, Tabellen,
  Einträgen und Größe, ihrem Knopf zum Zurücksetzen oder Löschen und ihren
  Zugangsdaten. Unter den Karten steht, wie viele weitere Datenbanken noch
  möglich sind. Auf dem Handy bleiben Feld und Knöpfe in einer Zeile (die
  Knöpfe als Symbole). Der Hinweis nennt die richtige Variable
  `MYSQL_DATABASE` statt `MYSQL_DB`.
- Jede weitere MySQL-Datenbank aus dem Profil hat einen eigenen Zugang
  (Benutzername wie die Datenbank), der nur diese Datenbank erreicht. Das
  Profil zeigt ihn unter der Datenbank, mit Knöpfen zum Kopieren. Gedacht
  für Programme, die andere benutzen, etwa ein geteiltes Gästebuch.
  Bestehende weitere Datenbanken bekommen ihren Zugang, wenn das Profil sie
  das nächste Mal anzeigt; mit der Datenbank verschwindet auch er.
- Behoben: Jede angemeldete Person konnte über das Profil eine beliebige
  MySQL-Datenbank löschen, wenn sie deren Namen kannte. Gelöscht werden
  jetzt nur die eigene Datenbank (zurückgesetzt) und die selbst
  angelegten; Namen aus anderen Zeichen als Buchstaben, Ziffern und `_`
  werden abgelehnt.
- Behoben: »Git kennt dich« schlug fehl, obwohl `git config --global` Name
  und E-Mail kannte. Die Prüfung lief mit dem `HOME` des Images (`/config`)
  statt mit dem der Lernenden (`/workspace`) und fand `.gitconfig` nicht.
- Behoben: Lehrkräfte konnten mit einer E-Mail-Adresse jeden Workspace
  öffnen, nicht nur die ihrer Gruppen.
- Lernende in mehreren Gruppen (etwa Klasse 12 und LK) sehen alle
  Lehrkräfte dieser Gruppen: Anmelde-Code und Workspace. Der Name muss nur in
  einer Zeile der Einladungen stehen.
- Lehrkräfte finden auf der Klassenseite die Anmelde-Codes ihrer Gruppe und
  bei jeder Person »Workspace öffnen« und »Nachahmen«; mit »Zurück zu
  deinem Konto« geht es zurück. Vorher nennen sie in einem Satz den Grund,
  den die Person bei der nächsten Anmeldung liest. Jede Lehrkraft hat ein
  Testkonto in einer eigenen Testgruppe, um alles aus Sicht der Lernenden
  auszuprobieren. Die Verwaltungsseite ist nur noch für Admins.
  Beschreibung: `docs/teacher-access.md`.
- Behoben: Lehrkräfte konnten mit einer Adresse jede Person in ihre
  Leistungsüberprüfung aufnehmen, nicht nur ihre eigenen Lernenden.
- Aufgabenkarten: klarer und lebendiger. Farbe zeigt den Stand, Antworten
  sind Kacheln, ein gelöster Haken erscheint animiert neben dem Knopf. Was
  ein Check liest, steht hinter einem (i); Einzelheiten eines Ergebnisses
  und die Lösung öffnen sich erst auf Nachfrage.
- Farben: Der Farbmischer springt nicht mehr, und der Hinweis auf Farben,
  die ein Bildschirm nicht zeigen kann, ist weg.
- Handbuch: Die Seite heißt »Tastatur-Shortcuts«.
- Klassenseite: Die Titel der Aufgaben stehen aufrecht und ganz über ihren
  Spalten, »Jetzt nachsehen« ist ein Menü, und beim Öffnen eines Workspaces
  dreht sich das Code-Symbol wie auf der Startseite.
- Abgaben zeigen überall denselben gefalteten Papierflieger (Font Awesome
  Free, CC BY 4.0).
- VS Code startet mit dem Farbschema »Dark 2026«. Wer noch das bisherige
  Standardschema hatte, bekommt das neue; ein selbst gewähltes Schema
  bleibt. Alle Screenshots sind neu gerendert.
- Dunkler Modus: Git-Graphen, ER-Diagramme und Bilder mit schwarzer
  Schrift auf Weiß passen sich an (`class='invert-dark'`); Neo4j-Browser,
  BIF-Entwickleransicht und die Karten von Anaglyph, BIF und »Über den
  Workspace« haben dunkle Varianten.
- Code in GLSL (```` ```glsl ````) ist farbig, ```` ```sh ```` wie
  `bash`.
- Kopierknopf auch für große Blöcke ab etwa 15 Zeilen, die in eine Datei
  eingefügt werden (ganze Dateien, lange Funktionen, Shader); kurzer Code
  wird weiter abgetippt. Regel in `docs/aktivitaet-schreiben.md`.
- Code in den neuen Aktivitäten (raylib, Webserver, Pixelwand) und in der
  raylib-Vorlage hat englische Namen; die Kommentare bleiben deutsch.
- Screenshot-Generator: Hooks bekommen `writeFile`; `pdf-page-full:` zeigt
  eine ganze PDF-Seite mit ihren Rändern (die Seiten der Facharbeit).
- »Ein eigener Webserver«: Lernende probieren den Path-Traversal-Angriff
  mit `curl --path-as-is` selbst aus, mit und ohne die Prüfung. »Statuszeile«
  heißt dort jetzt »Status-Line«, wie »Request-Line«.
- Code in YAML (```` ```yaml ````) ist farbig.
- »Eine Pixelwand für alle« nutzt faye-websocket statt eines selbst
  geschriebenen Handshakes und Frame-Parsers: Die Seite beginnt mit einem
  Echo in zehn Zeilen, der Handshake bleibt mit `curl` sichtbar. Neuer
  Abschnitt »Viele Threads«: Vier Threads arbeiten eine Warteschlange ab,
  ein Zähler verzählt sich bei 200 Pixeln auf einmal (`flood.js`), ein Lock
  behebt es. Die Seite sagt deutlich, dass das Überschütten fremder Server
  ein Angriff ist.
- Handbuch »HTTP«: Die Teile einer Adresse als Grafik aus HTML und CSS,
  mit Klammern und Namen unter jedem Teil.
- Screenshot-Generator: `preview-port: 1313 /axolotl/` öffnet eine Seite
  unterhalb der Wurzel einer App.
- »Ein eigener Webserver« bekommt eine sechste Stufe, »Ein Formular
  annehmen«: eine Umfrage mit `method="post"`. Der Server hebt die Header in
  einem Hash auf, liest den Body so lang, wie `Content-Length` sagt, zählt
  die Stimmen unter einem Lock und leitet mit `303 See Other` auf das
  Ergebnis weiter (Post/Redirect/Get). Drei neue Aufgaben; die Klasse kann
  über Shared Live Apps abstimmen. Das Handbuch »HTTP« nennt `303 See Other`.
- »TCP/IP-Verbindungen« ist die erste Hälfte von TCP → HTTP → Webserver:
  Neu sind die Schichten (Netzzugang, IP, TCP, Anwendung), Ports und der
  Handshake, den Lernende mit `curl -w '%{time_connect}'` messen und mit
  `ping` vergleichen. `netcat` geht jetzt auch allein mit zwei Terminals,
  und »HTTP von Hand« holt mit `nc -C info.cern.ch 80` die erste Webseite
  der Welt von 1992. Der Server mit der Endung `.pe` ist jetzt ein Rätsel
  (die Antwort kommt aus Belgien) statt ein Beispiel für einen fernen
  Server. Fünf neue Aufgaben, eine Notiz für Lehrkräfte, alle Bilder mit
  Alt-Text und aus Rezepten; nicht mehr benutzte Dateien sind entfernt.
- »Git und GitHub«: Die Seite behauptete, ohne `git mv` bemerke Git eine
  Umbenennung nicht. Jetzt erklärt sie, dass Git Umbenennungen gar nicht
  speichert, sondern am gleichen Inhalt erkennt; `mv` und `git add -A`
  ergeben denselben Commit.
- Sicherheit: Was der Server als root in den Ordner einer Person schreibt
  oder dort liest (`.hackschule`, `.bashrc`, `.gitconfig`, `.my.cnf`,
  `.myclirc`, die VS-Code-Einstellungen, die Marker und `.git/info/exclude`
  in Prüfungen, die TIC-80-Dateien), geht jetzt durch `UserFiles`: Jeder
  Pfad wird Ordner für Ordner geöffnet, ohne einem symbolischen Link zu
  folgen, und nur reguläre Dateien werden gelesen oder geschrieben. Bisher
  konnte ein Link im eigenen Workspace den Server dazu bringen, fremde
  Dateien zu überschreiben und der Person zu übergeben. Ein abgelehnter
  Pfad steht im Log; der Workspace startet trotzdem. TIC-80 schreibt
  Dateien nicht mehr per Umbenennen, sondern direkt.
- Sicherheit: Der Anaglyphen-Viewer für die Vorschau auf der Aufgabenkarte
  und die Galerie der Klassenseite läuft nicht mehr auf der Website, sondern
  auf `abgaben.<host>` in einem Frame mit `sandbox`, wie die
  Abgaben-Ansicht. Die Ausdrücke einer Szene sind JavaScript; bisher liefen
  sie mit der Sitzung dessen, der die Galerie ansah, also auch einer
  Lehrkraft. Die Seite holt für jede Szene eine signierte Adresse
  (`/api/anaglyph/address`), der Viewer schickt Bild, Fehler und die Zahl
  der Objekte als Nachricht zurück. Unter `/anaglyph-viewer/` antwortet die
  Website selbst nicht mehr. Der Viewer (`specht/anaglyph` 114bc3b) schickt
  seine Nachrichten dafür selbst richtig; der Server ergänzt nur einen Ersatz
  für `sessionStorage`.
- Sicherheit: Rückmeldungen der Aufgaben enthalten Text aus den Dateien der
  Lernenden (eine CSS-Zeile, ein Branch-Name, der Name einer Szene). Das
  HTML, das Aufgabenkarte und Klassenseite davon anzeigen, wird jetzt nach
  dem Markdown auf eine kurze Liste harmloser Elemente und Attribute
  gefiltert (`TaskHtml`): kein Skript, keine Event-Attribute, keine
  `javascript:`-Links, keine Frames. `<kbd>`, Code, Listen, Links und
  Bootstrap-Icons der Autorinnen und Autoren bleiben. Bisher konnte eine
  präparierte Datei Skript in die Sitzung einer Lehrkraft bringen, die
  »Jetzt nachsehen« drückte.
- Sicherheit: Das Cookie `hs_server_sid`, mit dem nginx den Zugang zu einem
  Workspace prüft, geht nicht mehr an code-server und an Apps auf
  `<tag>-<port>` weiter, ebenso `hs_watch_tag`; eigene Cookies einer App
  bleiben. Bisher konnte ein Programm im Workspace das Cookie einer
  Lehrkraft lesen, die ihn ansah. Beim Abmelden bekommt die Person einen
  neuen Wert, ein abgegriffener gilt dann nicht mehr; andere Geräte
  bekommen den neuen bei ihrer nächsten Anfrage.
- Sicherheit, Anmeldung: Der Anmeldecode steht nicht mehr im Log. Gibt es
  für eine Adresse keine Einladung, sagt die Anmeldeseite das jetzt
  ausdrücklich. Neue Grenzen: höchstens 20
  Codes pro Adresse und Tag, und pro Absender-Adresse 120 Code-Anfragen
  und 300 Anmeldeversuche in zehn Minuten (großzügig, weil eine ganze
  Schule über eine Adresse ins Netz geht). Die Zähler liegen nur im
  Speicher.
- Sicherheit, Prüfungen: Starten, Beenden und Schüler zuordnen geht nur
  noch für die eigene Prüfung (oder als Admin). Beim Löschen einer Prüfung
  wird ihre Erweiterungsliste nur entfernt, wenn die Prüfung wirklich der
  Lehrkraft gehörte. Den Startzustand eines fremden Workspace sehen nur
  noch Lehrkräfte dieser Person, und ein Startfehler erscheint im Browser
  nur noch als »Der Workspace ließ sich nicht starten.«; die Einzelheiten,
  in denen Passwörter stehen konnten, stehen nur im Log.
- Neue Bilder auf den Karten von »Daten aus dem Netz«, »Ein eigener
  Webserver«, »Eine Pixelwand für alle«, »TCP/IP-Verbindungen« und
  »Kryptographie im Terminal«: eigene Rezepte am Ende der Seite
  (`<img class='teaser' hidden>`), mit wenigen großen Dingen statt einer
  ganzen Seite, die auf der Karte unlesbar klein wird. Die Ergebnisseite der
  Umfrage im Webserver zeichnet jetzt für jede Antwort einen Balken mit
  `<progress>`.
- Schönere Seiten zum Abschreiben: Der Webserver bekommt ein `style.css`
  mit einer weißen Karte, Antworten als Kacheln und Balken in der Farbe der
  Seite; die Wetterseite zeigt die Temperatur groß und die Tage als Karten
  und baut ihr HTML nur noch mit `textContent`; die Pixelwand steht in der
  Mitte, mit runden Farbknöpfen.
- Kleinigkeiten aus der Durchsicht: Das Symbol für Erweiterungen im Handbuch
  »Workspace« (und in den Seiten, die es einbinden) erscheint wieder;
  Anmeldung, Profil, Admin und Lehrmaterial heißen im Browser-Tab nicht
  mehr »Mit dem Handy anmelden«, sondern »Anmelden«; jede Seite lädt
  `moment.js` nicht mehr, das nirgends benutzt wurde; der Link von raylib
  zu den Wellen in PixelRAM springt an die richtige Stelle.
- Sachfehler und Tippfehler: Rust steht mit 2015 (Version 1.0) im Titel und
  im Text, Common Lisp und Smalltalk nennen beide Jahre, C# schreibt
  Hejlsberg richtig, COBOL nennt den CODASYL-Ausschuss und Grace Hoppers
  FLOW-MATIC, die NASM-Seite sagt, dass es um NASM geht; die
  Ziffernerkennung ist von »boostrap«, »Kenrel«, »beboachten« und »des die«
  befreit.
- Auswahlaufgaben: In 28 Aufgaben war die richtige Antwort deutlich die
  längste und damit leicht zu erraten; die falschen Antworten sind jetzt
  ebenso ausführlich und plausibel. Zwölf Aufgaben ohne jede Rückmeldung
  bekommen eine zu den naheliegenden Fehlern, als Frage statt als Lösung.
- Datei-Prüfungen, die richtige Lösungen ablehnten: Hugo nimmt auch
  `.Site.RegularPages`, `site.Pages` und `{{- … -}}`; raylib nimmt einen
  Rahmen um den Zielblock auch über eine eigene Variable und eine Textur
  unter anderem Namen. Wo eine Prüfung einen bestimmten Namen verlangt
  (`sky` in TIC-80, `vy` in raylib, `Thread.new` im Webserver), steht er
  jetzt in der Aufgabe.
- Sicherheit, ohne Anmeldung erreichbare Dienste: Automatron bleibt offen,
  rechnet aber nur noch Ausdrücke bis 200 Zeichen und Automaten bis 60
  Zustände (»Der Automat wird zu groß zum Zeichnen«), zeichnet höchstens
  zehn Sekunden pro Bild, höchstens zwei Anfragen gleichzeitig und pro
  Absender 60 in zehn Minuten. Bisher konnte ein einziger Ausdruck den
  Server minutenlang beschäftigen. QR-Anmeldungen und Workspaces hinter
  einem Freigabe-Link lassen sich pro Absender 30-mal in zehn Minuten
  starten.

### Entfernt

- Der Endpunkt, der Keras-Modelle für die Ziffernerkennung in TensorFlow.js
  umwandelte. Die Aktivität wandelt ihr Modell inzwischen selbst in ONNX um;
  der Endpunkt nahm ohne Anmeldung bis zu 100 MB an und legte sie dauerhaft
  ab.

- »Hades in der Altgriechisch-App« steht nicht mehr im Katalog. Die Seite
  brauchte eine echte Anmeldung bei der App einer Schule; was sie über
  `POST` zeigte, steht jetzt im Webserver.

### Für Betreibende

- ER-Werkstatt 0.3.0 (`hackschule-er-0.3.0.vsix`): das Workspace-Image neu
  bauen.
- Der Screenshot-Generator sieht nur noch die Version von code-server als
  Aussehen des Workspaces, nicht die Paketrevision von linuxserver
  (`4.138.0-ls366` und `-ls367` gelten als gleich). Das Image baut auf dem
  Tag `4.138.0`, der von Zeit zu Zeit auf eine neue Revision springt; zwei
  Rechner, die es an verschiedenen Tagen gebaut haben, hielten bisher die
  Screenshots des anderen für veraltet und rechneten sie neu. Ein Abgleich
  schreibt auch keine neue Revision mehr in die Manifeste. Nach dem Update
  den Dienst neu bauen: `./config.rb build tutorial_screenshots &&
  ./config.rb up -d tutorial_screenshots`.
- Neu und abgeschaltet: der Fallserver für »Ein Krimi im Terminal«, zwei
  kleine SSH-Server (`case_museum`, `case_alarm`). Einschalten mit
  `CASE_SERVER = true` in `credentials.rb`, dann
  `./config.rb build case_museum case_alarm peer_firewall` und
  `./config.rb up -d`; Workspaces bekommen ihren Zugang beim nächsten Start.
  Die Server lassen nur lesen, begrenzen jedes Konto und erreichen keinen
  Workspace; die `peer_firewall` lässt zu ihnen nur Port 22 durch (eigenes
  Label, keine Infrastruktur). Neue Pflichtwerte gibt es nicht: Die
  Passwörter kommen aus `LOGIN_CODE_SALT`. Einzelheiten in
  docs/entwicklung.md, »Der Fallserver«.

- Die schnellen Tests der Aufgabenbeispiele (`task_examples_test.rb`)
  brauchen für »Geheime Botschaften in Bildern« lokal `gem install
  chunky_png` und `pip install pillow`.

- Der Spiegel von Open-Meteo (`/open-meteo/…`) ist ohne Anmeldung erreichbar,
  weil Wetterseiten auf den Subdomains der Lernenden und Programme im
  Workspace ihn nutzen. Er leitet nur die zwei Endpunkte mit einer festen
  Liste von Parametern weiter, fragt Open-Meteo höchstens 600-mal pro Stunde
  und speichert die Antworten unter `data/internal/weather-mirror`
  (höchstens 20.000 Dateien, die ältesten gehen zuerst).
- Neue Einstellung `CONTACT_EMAIL` in `credentials.rb` (siehe
  `credentials.template.rb`): die Adresse für Einladungen, das Impressum und
  »schreib eine E-Mail an« auf Seiten (`${contact_email}`). Keine Adresse steht
  mehr fest im Code. Ohne die Einstellung fehlt die Adresse dort einfach;
  bitte nach dem Update eintragen.
- Der Ordner `/internal/tfjs` (alte Uploads für den entfernten
  Keras-Endpunkt) kann gelöscht werden.

- Das Workspace-Image bekommt `php-mysql` (PDO-Treiber `pdo_mysql` und
  `mysqli`), damit PHP-Programme die MySQL-Datenbanken erreichen. Eine
  eigene, kleine Schicht kurz vor den Erweiterungen; Image neu bauen
  (`./build-image.sh`).
- TIC-80 einmal neu bauen: `./build-tic80.sh` (TIC-80 1.3.1, sokol-Schicht;
  `tic80.js` und `tic80.wasm` stehen nicht in Git). Das Build-Image braucht
  jetzt `gcc-multilib` für Forth; `docker/tic80/Dockerfile` installiert es.
- Keine Änderung nötig. Die Prüfarten `scene` und `er` brauchen `node` im
  Workspace-Image (ist dort schon vorhanden). Für die ER-Werkstatt muss das
  Workspace-Image neu gebaut werden (`build-image.sh`).
- Die Filmdatenbank lädt ihre Daten mit `wget` aus
  `specht/workspace-files/movie-archive/`; andere Schulen können die Dateien
  mit `src/scripts/collect-movies` selbst bauen. Die Datendateien enden mit
  `ANALYZE TABLE`: Ohne aktuelle Statistik dauert »Wer spielt mit wem?« direkt
  nach dem Import 50 Sekunden statt einer. Neu erzeugen geht ohne Netz mit
  `./collect-movies.rb --offline --output …/workspace-files/movie-archive`.
- Für den Hinweis auf neue Versionen braucht der Server ausgehende
  HTTPS-Verbindungen zu den Adressen aus `template` (GitHub, GitLab).
  Kommt keine Antwort, erscheint einfach kein Hinweis.
- Das Workspace-Image enthält `neo4j_bolt` (Gem, Version im Dockerfile als
  `NEO4J_BOLT_VERSION`). Neu bauen mit `build-image.sh`.
- Neue ER-Werkstatt `hackschule-er-0.2.0.vsix`: Workspace-Image neu bauen.
- `build-image.sh` bricht ab, bevor es `hs_code_server:latest` taggt, wenn eine
  `.vsix` aus `docker/code/svc-code-server-run` im Image fehlt. Bisher startete
  ein Workspace dann still ohne die Erweiterung.
- Workspace-Image neu bauen (`build-image.sh`): `emcc` warnt nicht mehr bei
  jedem Übersetzen über zwei GCC-Versionen, was Lernende für einen Fehler im
  eigenen Programm hielten.
- Ruby-Image neu bauen (`./config.rb build ruby`): Es braucht jetzt `wget`
  für die Kenney-Modelle der Anaglyph-Galerie.
- Für Vorschau und Galerie der Anaglyph-Abgaben braucht der Server
  ausgehende HTTPS-Verbindungen zu `codeload.github.com` (der Viewer in der
  Version `AnaglyphViewer::COMMIT`, beim ersten Gebrauch nach
  `/internal/anaglyph-viewer/`) und zu `kenney.nl` (Modelle, erst wenn eine
  Szene sie braucht). Ohne Netz gibt es keine Bilder; Abgaben funktionieren
  trotzdem.
- Abgaben im Browser kommen von `abgaben.<host>`. Der Platzhalter-Eintrag
  im DNS und das Zertifikat für `*.<host>` decken das schon ab; die
  nginx-Konfiguration schreibt der Server selbst.
- Screenshot-Generator neu bauen (nur Entwicklung):
  `./config.rb build tutorial_screenshots && ./config.rb up -d
  tutorial_screenshots`.
- Das Workspace-Image bekommt `yq` (mikefarah/yq, Version und SHA-256 im
  Dockerfile): Workspace-Image neu bauen (`./build-image.sh`).
- Das Workspace-Image bekommt `xxd`, das Dateien Byte für Byte in
  Hexadezimal zeigt (und zurück). Workspace-Image neu bauen
  (`./build-image.sh`).
