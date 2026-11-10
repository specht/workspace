Lernende programmieren ein Spiel, das man anrufen kann, in Ruby, Python oder JavaScript: Es spricht mit der Stimme Thorsten, spielt Musik und verzweigt nach den Tasten des Telefons. Am Ende steht ein eigenes Abenteuer aus Szenen, das jemand aus der Klasse durchspielt und das, wenn die Lernenden es einreichen, im öffentlichen Telefonbuch steht.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Verzweigungen mit <code>if</code> und <code>else</code> (<a href='/kata-decisions'>Katas: Verzweigungen</a>). Mehr braucht es nicht: Eigene Funktionen ohne Parameter führt die Seite an der Höhle ein, erst dieselben Zeilen ohne und dann in einer Funktion, die genauso klingt; das Spiel ist ein guter erster Grund, Funktionen zu schreiben.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprachen der Klasse« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in den anderen. In JavaScript (Node) holt die erste Zeile die Befehle mit <code>require('telefon')</code>, Stimmung und Ausschnitte stehen in geschweiften Klammern (<code>say('…', { mood: 'whisper' })</code>), und verglichen wird mit <code>===</code>: Mit <code>==</code> wäre <code>'1' == 1</code> wahr, darum fragt »Text oder Zahl?« dort nach <code>===</code>.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> der erste Anruf, Musik und Pausen (auch aus einer Datei, mit einem eigenen Stück für die Höhle), die Frage mit Tasten, Text oder Zahl, die dritte Wahl. Zum Weiterlesen, ohne Aufgabe: wie aus Ton Zahlen werden, mit demselben Satz zum Anhören bei 44.100 bis 2.000 Hz, und wie Telefonieren heute über das Internet läuft.</li>
<li><b>2. Doppelstunde:</b> die Höhle erst ohne Funktionen, dann jede Szene als Funktion, »Niemand drückt«, der Gang als eigene Funktion, Stimmungen (Thorsten flüstert, staunt, ist müde), Stille im Gang und Musik beim Schatz, Abkürzen mit <code>interrupt</code>, Auflegen mit <code>hangup</code>; dann die Karte der eigenen Geschichte auf Papier.</li>
<li><b>3. Doppelstunde:</b> die eigene Geschichte programmieren, Spieltest in Paaren, danach verbessern und einreichen.</li>
<li><b>Für Schnelle:</b> das Zahlenschloss (Extra), die Aussprache englischer Wörter mit Lautschrift.</li>
</ul></td></tr>
<tr><th>Dateien</th><td><ul>
<li>Alles in <code>/workspace/telefon</code>: <code>hallo</code>, <code>frage</code>, <code>hoehle</code> und <code>geschichte</code>, je mit <code>.rb</code>, <code>.py</code> oder <code>.js</code>.</li>
<li>Der Befehl <code>telefon spiel.rb</code> startet das Spiel und zeigt auf Port 8040 ein Telefon mit Tastenfeld im Browser; das Spiel startet beim Klick auf den grünen Hörer, bei jedem Anruf neu aus der Datei.</li>
<li>Die Stimme und die Musik von YouTube kommen vom Sprachdienst des Workspace (<code>phone-speech</code>); ohne ihn liest der Browser mit seiner eigenen Stimme vor, und die Seite sagt das.</li>
</ul></td></tr>
<tr><th>Telefonbuch</th><td>Fertige Spiele reichen die Lernenden auf der Seite ein, mit einer Nummer aus drei oder vier Ziffern. Freigegeben werden sie von der Betreiberseite des Workspace, nicht von Lehrkräften; die Lernenden bekommen eine E-Mail, bei einer Ablehnung mit dem Grund. Im <a href='/telefonbuch'>Telefonbuch</a> stehen Titel, Untertitel und Nummer, kein Name und keine Klasse. Angerufen werden die Spiele dort im Browser, in einer abgeschotteten Umgebung, nicht im Workspace der Lernenden. Zurückziehen können die Lernenden ein Spiel jederzeit.</td></tr>
<tr><th>Prüfung</th><td>Die Aufgaben spielen das Spiel der Lernenden selbst durch, mit <code>telefon --test</code> und festen Tasten, im Workspace als Gast, der nichts verändern kann: ohne Stimme, ohne Pausen, die Musik nur dem Namen nach. Die Rückmeldung nennt die Tasten und was danach fehlt, etwa »Nach der Taste 1 sagt das Spiel nichts mit »Fackel««, bei einem Absturz die Zeile und die Fehlermeldung. Unter »Was der Workspace gelesen hat« steht der ganze Verlauf. Der Workspace muss laufen.</td></tr>
<tr><th>Lösung</th><td>Die Stände jedes Schritts im Repository unter <a href='https://github.com/specht/workspace/tree/main/src/content/telefonspiel/stufen'><code>src/content/telefonspiel/stufen</code></a>; mit »Code bis hier« an jedem Schritt der Seite.</td></tr>
</table>

#### Was schiefgeht

- **Kein Ton.** Der Browser spielt erst nach einem Klick; das Spiel startet darum mit dem grünen Hörer. Ist der Tab stumm geschaltet oder die Lautstärke des Rechners aus, hört man nichts, obwohl der Text erscheint.
- **Viele Rechner, ein Raum.** Dreißig Telefone gleichzeitig sind laut. Kopfhörer helfen, für den Spieltest erst recht.
- **`if key == 1`.** Die Zahl statt des Textes `'1'`; das Spiel nimmt dann immer den letzten Zweig. Darum gibt es die Vorhersage »Text oder Zahl?«.
- **Ein Zweig nach `else`.** Ein neuer Zweig für die 3 hinter dem für »nichts gedrückt« wird nie erreicht; die Aufgabe »Eine dritte Wahl« sagt das.
- **Die Musik fehlt.** Ein Tippfehler im Link oder im Namen einer Datei; das Terminal nennt das Problem, die Seite schweigt dazu mit Absicht (es zählt nur, was man hört). Eigene Musik liegt als MP3, WAV, Ogg oder M4A direkt im Ordner <code>telefon</code>; ein anderes Format oder ein Unterordner bricht das Spiel mit einer Fehlermeldung in dieser Zeile ab, weil die Datei sonst beim Veröffentlichen fehlen würde. Ogg und M4A spielt nicht jeder Browser im Workspace, das Telefon spielt alle vier.
- **Das Auflegen fehlt.** Nach dem Satz vom Sprung sagt das Spiel noch »Ende.«: Ohne `hangup` kehrt das Programm aus den Funktionen zurück und läuft bis zur letzten Zeile. Eine gute Gelegenheit, den Weg der Aufrufe an der Tafel nachzuzeichnen.
- **Python: `sleep` aus `time`.** Wer `from time import sleep` schreibt, hält das Programm an, ohne dass das Telefon davon weiß; die Bibliothek bringt ein eigenes `sleep` mit.

#### Die Aufgaben

- »Hallo« und »Musik« prüfen nur, dass das Spiel etwas sagt und Musik spielt; der Text ist frei.
- »Text oder Zahl?« und »Niemand drückt« sind Vorhersagen; die Erklärung steht erst nach der Antwort auf der Seite.
- »Eine dritte Wahl« spielt fünf Fälle: ohne Taste (die Frage nennt die Fische), 1, 2, 3 und nichts gedrückt.
- »Der Gang« spielt fünf Wege durch die Höhle, auch die Schleifen: im Gang die 2 und im Gang gar nichts müssen wieder nach einer Taste fragen.
- »Stimmen in der Höhle« prüft, dass der Satz im Gang mit `mood: 'whisper'` und der Satz beim Gold mit `mood: 'surprised'` gesagt wird; der Text selbst bleibt.
- »Stille und Musik« prüft, dass die Musik im Gang ausgeht (`music nil`) und beim Schatz »No Quarter« von 0:16 bis 0:32 spielt; die acht Sekunden danach prüft sie nicht.
- »Ein Sprung ins Wasser« prüft, dass der Gang die 3 nennt, dass das Spiel nach der 3 von der Strömung spricht und danach nichts mehr sagt, auch nicht »Ende.«: Ohne `hangup` liefe das Programm in `cave` und am Ende weiter. Die 2 im Gang muss weiter zum eiskalten Wasser führen.
- »Deine Geschichte« prüft nur den Anfang (Musik, ein Satz, dann wartet das Spiel auf eine Taste); ob es vier Szenen und zwei Enden gibt, zeigt der Spieltest.
- »Spieltest« ist eine Reflexion: wer gespielt hat, wo es hakte, was sich geändert hat.

#### Eigene Projekte

- **Ein Quiz mit Punkten** (eine Doppelstunde): Fragen mit drei Antworten, ein Zähler für die richtigen, am Ende das Ergebnis und je nach Punktzahl ein anderer Satz. Von der Seite kommen `keys` und die Verzweigung, neu ist die Variable, die mitzählt. Bewertet wird im Spieltest: Stimmt die Zählung, auch wenn man nichts drückt?
- **Eine Hörspiel-Szene mit Geräuschen** (eine Doppelstunde): eigene MP3-Dateien für Schritte, Türen oder Regen, die mit `music` wechseln, kurze Pausen mit `sleep` an den richtigen Stellen. Bewertet wird, ob man die Szene ohne Bildschirm versteht.
- **Das Abenteuer mit Gedächtnis** (ein Projekt): Wer die Fackel nimmt, sieht im dunklen Gang etwas, wer nicht, hört nur etwas. Dafür braucht es Variablen, die alle Szenen kennen, oder die Szenen als Daten in einem Hash. Bewertet wird das Vorführen und das Erklären, wie das Spiel sich die Fackel merkt.
- **Ein Telefonbuch der Klasse** (eine Doppelstunde): ein Menü, das mit den Tasten zu kurzen Steckbriefen führt, die jede und jeder selbst schreibt, mit `keys(3)` für eine dreistellige Nummer. Bewertet wird, ob jede Nummer ankommt und ob eine falsche Nummer freundlich abgefangen wird.
