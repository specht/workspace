Lernende bauen einen Tracker: einen Webserver, der ein unsichtbares Bild ausliefert, das zwei kleine Websites in ihrem Workspace einbinden. Mit einem Cookie erkennt er dieselbe Kennung auf beiden Websites wieder und macht aus den Stichwörtern der gelesenen Seiten ein Profil. Danach geht es darum, was DSGVO und § 25 TDDDG dazu sagen. Ruby oder Python.

<table class='table'>
<tr><th>Voraussetzungen</th><td><a href='/webserver'>Ein eigener Webserver</a>: Anfrage, Header, <code>respond</code>, Threads. Dazu Dictionaries, etwa nach den <a href='/kata-dicts'>Katas: Dictionaries</a>.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Einstellungen« ein. Die beiden Websites liefert in beiden Fällen Pythons eingebauter Webserver aus.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> was ein Server sieht, der Tracker mit der Seite seiner Header, das Tracking-Pixel und <code>visits.log</code>, die beiden Websites auf Port 8001 und 8002, der <code>Referer</code>, <code>parse_cookies</code> und die Kennung im Cookie.</li>
<li><b>2. Doppelstunde:</b> <code>visits_of</code> und <code>count_keywords</code>, die Seite mit dem eigenen Profil, was echte Tracker daraus machen (Werbebörsen, Fingerprinting), DSGVO und § 25 TDDDG, das Gespräch über Cookie-Banner.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/tracking</code>: die Ordner <code>rezepte</code> und <code>sport</code> (mit <code>wget</code> von dieser Seite), <code>tracker.rb</code> oder <code>tracker.py</code>, und <code>visits.log</code>, die der Tracker schreibt. Keine Vorlage: Den Anfang des Trackers kopieren die Lernenden von der Seite.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Drei Aufgaben rufen die Funktionen der Lernenden in ihrem Workspace auf, als Gast, der nichts verändern kann, und vergleichen die Rückgabe mit kurzen, ausgedachten Eingaben; die Aufgabe sagt jeweils, womit.</li>
<li>Zwei Aufgaben führen einen <code>curl</code>-Befehl der Lernenden aus und lesen die Antwort ihres Trackers; er muss dafür laufen.</li>
<li>Dazu eine Zahl, vier Fragen, zwei Vorhersagen und ein Text über Cookie-Banner.</li>
</ul></td></tr>
</table>

#### Die Aufgaben

- Die Seite gibt den Anfang des Trackers vor, das Bild, das Protokoll und die Seite mit dem Profil. Selbst geschrieben werden <code>parse_cookies</code>, <code>visits_of</code> und <code>count_keywords</code>, jede mit Tipps in Stufen.
- Die beiden Fragen zum Gesetz fragen, was in DSGVO und § 25 TDDDG steht, nicht, was die Lernenden von Werbung halten. Für ein Gespräch eignet sich der Text über Cookie-Banner am Ende: welche Fragen die Lernenden einer Website stellen würden, und warum.
- Der Schnipsel auf den Websites baut die Adresse des Trackers aus der eigenen: aus <code>-8001.</code> in der weitergeleiteten Adresse wird <code>-8000.</code>. Alle drei Adressen enden auf derselben Domain; für den Browser ist das eine Site, das Cookie kommt also an. Die Seite erklärt, dass es bei einem Tracker auf einer fremden Domain ein Third-Party-Cookie wäre.

#### Was schiefgeht

- **Im Terminal des Trackers kommt nichts an:** Die Website wurde über <code>localhost</code> geöffnet oder nicht über die weitergeleitete Adresse; der Schnipsel findet den Tracker nur über die Adresse aus »Forwarded Address«. Oder ein Werbeblocker im Browser hält <code>pixel.gif</code> zurück, weil die Anfrage aussieht wie ein Tracking-Pixel; dann hilft es, ihn für diese Adressen auszuschalten.
- **Jede Seite bekommt eine neue Kennung:** Der Tracker lief vor dem Cookie-Schritt, oder der Browser lässt keine Cookies zu (privates Fenster mit strengen Einstellungen). Im Reiter »Network« zeigt die Anfrage nach <code>pixel.gif</code>, ob ein Header <code>Cookie</code> mitgeht.
- **<code>/profil</code> zeigt keine Seiten:** Es wurde mit <code>curl</code> geöffnet, das kein Cookie mitschickt, oder aus einem anderen Browser.
- **Python: <code>os</code> ist nicht definiert:** <code>import os</code> fehlt; er kommt mit dem Bild dazu.

#### Das Recht, in Kürze

- <a href='https://www.gesetze-im-internet.de/ttdsg/__25.html'>§ 25 TDDDG</a> (bis Mai 2024 TTDSG): Speichern und Auslesen im Endgerät nur mit Einwilligung, außer es ist unbedingt erforderlich für einen Dienst, den die Person ausdrücklich wünscht. Reichweitenmessung mit einer Kennung fällt nach Ansicht der Datenschutzkonferenz nicht darunter.
- <a href='https://dsgvo-gesetz.de/erwaegungsgruende/nr-30/'>DSGVO, Erwägungsgrund 30</a>: Cookie-Kennungen und IP-Adressen als Online-Kennungen, mit denen sich Profile bilden lassen.
- EuGH, <a href='https://curia.europa.eu/juris/liste.jsf?num=C-673/17'>Planet49</a> (C-673/17, 2019): Ein vorausgefülltes Ankreuzkästchen ist keine Einwilligung.
