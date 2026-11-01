Lernende schreiben Tests mit dem Testwerkzeug ihrer Sprache für einen kleinen Notenrechner der Oberstufe und finden damit einen echten Fehler: Er rundet die Durchschnittsnote im Abitur, statt nach der Tabelle der KMK abzuschneiden. Dabei lernen sie Klassen von Eingaben und Grenzwerte, Tests, die einen Fehler erwarten, und zum Schluss testgetriebene Entwicklung (rot, grün, aufräumen) an der Zulassung zur Prüfung. Ruby mit minitest, Python mit pytest oder JavaScript mit <code>node --test</code>.

<table class='table'>
<tr><th>Voraussetzungen</th><td>Funktionen, Verzweigungen, Listen in einer der drei Sprachen. Was ein Testwerkzeug ist und wie man es startet, erklärt die Seite. In Ruby erbt die Testklasse von <code>Minitest::Test</code>; mehr über Klassen braucht es nicht.</td></tr>
<tr><th>Sprache</th><td>Ruby oder Python stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprachen der Klasse« ein. JavaScript wählen die Lernenden auf der Seite selbst; die Wahl gilt nur für Seiten, die sie anbieten. Eine Aufgabe, in einer Sprache gelöst, gilt in jeder.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> die Geschichte (805 Punkte, 1,2 statt 1,1), Einrichten, der erste Test, ein Test, der scheitert, Klassen und Grenzen, Tests für <code>grade</code> mit Fehlern, die erwartet werden.</li>
<li><b>2. Doppelstunde:</b> die Tabelle der KMK, Tests an ihren Grenzen, die mit dem gegebenen Code rot werden, die Reparatur, dann die Zulassung testgetrieben: erst die Tests, dann der Code, dann aufräumen. Zum Schluss, was Tests nicht können.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>Alles in <code>/workspace/notenrechner</code>: <code>noten.rb</code>, <code>.py</code> oder <code>.js</code> laden die Lernenden mit <code>wget</code> von der Website; die Tests schreiben sie selbst in <code>test_noten.rb</code>, <code>.py</code> oder <code>.js</code>.</td></tr>
<tr><th>Prüfung</th><td><ul>
<li>Vier Aufgaben führen die Tests der Lernenden in ihrem Workspace aus, als Gast, der nichts verändern kann, in einem Ordner in <code>/tmp</code>: einmal mit einem richtigen <code>noten</code>, bei dem alle grün sein müssen, und mit fünf fehlerhaften, von denen jedes an mindestens einem Test scheitern muss (ein Mutationstest). Das eigene <code>noten</code> der Lernenden läuft dabei nicht; ihre Tests dürfen damit rot sein.</li>
<li>Zwei Aufgaben rufen <code>noten</code> der Lernenden auf: die reparierte Durchschnittsnote und die Zulassung, mit anderen Werten als die Tests der Seite.</li>
<li>Drei Fragen, zwei davon Vorhersagen. Der Workspace muss für die Prüfungen laufen.</li>
</ul></td></tr>
</table>

#### Die Aufgaben

- »Ein Test, der scheitert« ist eine Vorhersage: Wer rät, das Testwerkzeug melde einen Fehler in <code>noten</code>, hat den Kern verfehlt, den das ganze Kapitel braucht. Ein roter Test sagt nur, dass Test und Code nicht zusammenpassen, nicht, wer recht hat.
- »Tests für grade« verlangt die beiden Enden, Werte dazwischen und die Werte gleich daneben. Die fehlerhaften Fassungen: 16 Punkte gültig, 0 Punkte ungültig, −1 gültig, 4 und 5 Punkte vertauscht, jede Note um einen Punkt verschoben. Die Rückmeldung sagt, welche Fassung durchgekommen ist, ohne den Test vorzugeben.
- »823 Punkte« ist die Vorhersage zum Fehler der Geschichte: <code>round(1,094; 1)</code> ist 1,1, die Tabelle sagt 1,0. Im Kommentar über der Funktion steht die richtige Regel; der Code tut etwas anderes. Gut für die Frage, warum ein Kommentar nichts prüft.
- In »Tests für den Abi-Schnitt« sind die Tests mit dem eigenen <code>noten</code> rot, gewollt. Die Seite sagt das, trotzdem fragen manche, warum die Aufgabe trotzdem gelöst ist.
- Die Reparatur geht ganz ohne Kommazahlen: Die Note in Zehnteln ist <code>(1020 − E) / 18</code>, ganzzahlig geteilt. <code>floor</code> auf Kommazahlen stimmt hier ebenfalls, für alle 601 Punktzahlen nachgerechnet.
- Die Zulassung folgt Ziffer 9.5.7 der Vereinbarung der KMK, vereinfacht (jedes Ergebnis zählt einfach, keine doppelte Gewichtung, keine Facharbeit). Python rundet mit <code>round</code> ,5 zur geraden Zahl; die Seite nimmt deshalb <code>math.floor(x + 0.5)</code>, wie die Vereinbarung es verlangt (»ab der Dezimalen 5 wird aufgerundet«).

#### Was schiefgeht

- **<code>pytest: command not found</code>:** Das Image des Workspace bringt pytest ab dem Update mit; ein Workspace, der vorher gestartet wurde, braucht einen Neustart.
- **Python: <code>ImportError: cannot import name 'is_admitted'</code>** vor dem Schreiben der Zulassung: Das ist das Rot der testgetriebenen Entwicklung, kein Fehler im Test. pytest meldet es als Fehler beim Sammeln, nicht als gescheiterten Test.
- **JavaScript: <code>TypeError: isAdmitted is not a function</code>** auch nach dem Schreiben: <code>isAdmitted</code> fehlt in <code>module.exports</code>.
- **Ruby: Kommazahlen mit Komma** (<code>assert_equal 1,1, …</code>): Ruby liest das als zwei Argumente; im Code steht der Punkt.
- **Alle Tests grün, die Prüfung findet trotzdem eine durchgekommene Fassung:** Dann fehlt ein Test an genau der Grenze, die die Rückmeldung nennt. Gut zum Besprechen: Grün heißt nicht fertig.

#### Eigene Projekte

- **Tests für eigenen Code** (eine Doppelstunde): eine Funktion aus einer früheren Aktivität, etwa die Prüfziffer aus [»Prüfziffern«](/check-digits), mit Tests nach Klassen und Grenzen. Abgegeben wird die Testdatei mit einem Satz je Test, welche Klasse oder Grenze er prüft.
- **Mutanten von Hand** (eine Doppelstunde): Zwei Lernende tauschen ihre Testdateien; jede baut in ihr <code>noten</code> heimlich einen kleinen Fehler ein und lässt die Tests der anderen laufen. Wer einen Fehler findet, den die Tests nicht finden, schreibt den fehlenden Test dazu.
- **Die ganze Gesamtqualifikation** (ein Projekt): Block I mit doppelt gewichteten Leistungskursen nach Anlage 3 der Vereinbarung, Block II mit den Prüfungen, dazu die Mindestbedingungen der Prüfung, alles testgetrieben, Regel für Regel. Bewertet werden die Tests so wie der Code.
