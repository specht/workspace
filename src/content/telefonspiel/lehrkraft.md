Lernende programmieren ein Spiel, das man anrufen kann, in Ruby oder Python: Es spricht mit der Stimme Thorsten, spielt Musik und verzweigt nach den Tasten des Telefons. Am Ende steht ein eigenes Abenteuer aus Szenen, das jemand aus der Klasse durchspielt.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Verzweigungen mit <code>if</code> und <code>else</code> (<a href='/kata-decisions'>Katas: Verzweigungen</a>) und eigene Funktionen ohne Parameter. Mehr braucht es nicht; das Spiel ist ein guter erster Grund, Funktionen zu schreiben.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprachen der Klasse« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen. Der Befehl <code>telefon</code> spielt auch Spiele in JavaScript (<code>require('telefon')</code>); die Seite zeigt nur Ruby und Python.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der erste Anruf, Musik und Pausen, die Frage mit Tasten, Text oder Zahl, die dritte Wahl.</li>
<li><b>2. Doppelstunde:</b> die Höhle aus Szenen, »Niemand drückt«, der Gang als eigene Funktion; dann die Karte der eigenen Geschichte auf Papier.</li>
<li><b>3. Doppelstunde:</b> die eigene Geschichte programmieren, Spieltest in Paaren, danach verbessern.</li>
<li><b>Für Schnelle:</b> das Zahlenschloss (Extra), Musik, die mit der Szene wechselt.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/telefon</code>: <code>hallo</code>, <code>frage</code>, <code>hoehle</code> und <code>geschichte</code>, je mit <code>.rb</code> oder <code>.py</code>.</li>
<li>Der Befehl <code>telefon spiel.rb</code> startet das Spiel und zeigt auf Port 8040 ein Telefon mit Tastenfeld im Browser; das Spiel startet beim Klick auf den grünen Hörer, bei jedem Anruf neu aus der Datei.</li>
<li>Die Stimme und die Musik von YouTube kommen vom Sprachdienst des Workspace (<code>phone-speech</code>); ohne ihn liest der Browser mit seiner eigenen Stimme vor, und die Seite sagt das.</li>
</ul></td></tr>
<tr><th>Prüfung</th><td>Die Aufgaben spielen das Spiel der Lernenden selbst durch, mit <code>telefon --test</code> und festen Tasten, im Workspace als Gast, der nichts verändern kann: ohne Stimme, ohne Pausen, die Musik nur dem Namen nach. Die Rückmeldung nennt die Tasten und was danach fehlt, etwa »Nach der Taste 1 sagt das Spiel nichts mit »Fackel««, bei einem Absturz die Zeile und die Fehlermeldung. Unter »Was der Workspace gelesen hat« steht der ganze Verlauf. Der Workspace muss laufen.</td></tr>
<tr><th>Lösung</th><td>Die Stände jedes Schritts im Repository unter <a href='https://github.com/specht/workspace/tree/main/src/content/telefonspiel/stufen'><code>src/content/telefonspiel/stufen</code></a>; mit »Code bis hier« an jedem Schritt der Seite.</td></tr>
</table>

#### Was schiefgeht

- **Kein Ton.** Der Browser spielt erst nach einem Klick; das Spiel startet darum mit dem grünen Hörer. Ist der Tab stumm geschaltet oder die Lautstärke des Rechners aus, hört man nichts, obwohl der Text erscheint.
- **Viele Rechner, ein Raum.** Dreißig Telefone gleichzeitig sind laut. Kopfhörer helfen, für den Spieltest erst recht.
- **`if key == 1`.** Die Zahl statt des Textes `'1'`; das Spiel nimmt dann immer den letzten Zweig. Darum gibt es die Vorhersage »Text oder Zahl?«.
- **Ein Zweig nach `else`.** Ein neuer Zweig für die 3 hinter dem für »nichts gedrückt« wird nie erreicht; die Aufgabe »Eine dritte Wahl« sagt das.
- **Die Musik fehlt.** Ein Tippfehler im Link oder im Namen einer Datei; das Terminal nennt das Problem, die Seite schweigt dazu mit Absicht (es zählt nur, was man hört). Eigene Musikdateien als MP3, Ogg spielt Safari nicht.
- **Python: `sleep` aus `time`.** Wer `from time import sleep` schreibt, hält das Programm an, ohne dass das Telefon davon weiß; die Bibliothek bringt ein eigenes `sleep` mit.

#### Die Aufgaben

- »Hallo« und »Musik« prüfen nur, dass das Spiel etwas sagt und Musik spielt; der Text ist frei.
- »Text oder Zahl?« und »Niemand drückt« sind Vorhersagen; die Erklärung steht erst nach der Antwort auf der Seite.
- »Eine dritte Wahl« spielt fünf Fälle: ohne Taste (die Frage nennt die Fische), 1, 2, 3 und nichts gedrückt.
- »Der Gang« spielt fünf Wege durch die Höhle, auch die Schleifen: im Gang die 2 und im Gang gar nichts müssen wieder nach einer Taste fragen.
- »Deine Geschichte« prüft nur den Anfang (Musik, ein Satz, dann wartet das Spiel auf eine Taste); ob es vier Szenen und zwei Enden gibt, zeigt der Spieltest.
- »Spieltest« ist eine Reflexion: wer gespielt hat, wo es hakte, was sich geändert hat.

#### Eigene Projekte

- **Ein Quiz mit Punkten** (eine Doppelstunde): Fragen mit drei Antworten, ein Zähler für die richtigen, am Ende das Ergebnis und je nach Punktzahl ein anderer Satz. Von der Seite kommen `keys` und die Verzweigung, neu ist die Variable, die mitzählt. Bewertet wird im Spieltest: Stimmt die Zählung, auch wenn man nichts drückt?
- **Eine Hörspiel-Szene mit Geräuschen** (eine Doppelstunde): eigene MP3-Dateien für Schritte, Türen oder Regen, die mit `music` wechseln, kurze Pausen mit `sleep` an den richtigen Stellen. Bewertet wird, ob man die Szene ohne Bildschirm versteht.
- **Das Abenteuer mit Gedächtnis** (ein Projekt): Wer die Fackel nimmt, sieht im dunklen Gang etwas, wer nicht, hört nur etwas. Dafür braucht es Variablen, die alle Szenen kennen, oder die Szenen als Daten in einem Hash. Bewertet wird das Vorführen und das Erklären, wie das Spiel sich die Fackel merkt.
- **Ein Telefonbuch der Klasse** (eine Doppelstunde): ein Menü, das mit den Tasten zu kurzen Steckbriefen führt, die jede und jeder selbst schreibt, mit `keys(3)` für eine dreistellige Nummer. Bewertet wird, ob jede Nummer ankommt und ob eine falsche Nummer freundlich abgefangen wird.
