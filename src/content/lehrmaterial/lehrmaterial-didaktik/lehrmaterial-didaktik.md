<div class='meta'>
image: didaktik.webp
image_alt: "Ein Regal mit neun bunten Büchern, auf den Buchrücken: PRIMM, Parsons, Notional Machine, Rollen, Fehlvorstellungen, Hinge Questions, Semantic Waves, Cognitive Load, Unplugged; das letzte lehnt am vorletzten"
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Didaktik im Workspace

<p class='abstract'>
Warum eine Aktivität erst nach einer Vermutung fragt, warum die Seite nur die Zeilen zeigt, die sich ändern, und warum ein Tipp eine Minute auf den nächsten wartet: Hinter den Aufgaben im Workspace stehen Ideen aus der Informatikdidaktik und der Lernforschung. Hier steht jede mit dem, was sie sagt, wo du sie im Workspace findest, und wo du mehr darüber liest. Diese Seite sehen nur Lehrkräfte.
</p>

## Wozu diese Seite

Viele dieser Ideen kennst du aus dem Studium oder dem Referendariat, manche unter einem anderen Namen. Neu ist hier vor allem die zweite Hälfte jedes Abschnitts: welche Karte, welche Zeile einer Notiz für Lehrkräfte und welche Spalte der Klassenseite eine Idee umsetzt. Wer eine Idee noch nicht kennt, sieht sie an einem Beispiel, das sich öffnen lässt.

Die Abschnitte folgen ungefähr dem Lauf einer Stunde, nicht der Geschichte der Forschung. Zu jedem stehen ein oder zwei Aufsätze oder Bücher zum Weiterlesen; wo es eine freie Fassung gibt, führt der Link dorthin.

Wer nur einen Text lesen will, fängt hiermit an:

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>The Big Book of Computing Pedagogy</strong>, Hello World, Raspberry Pi Foundation, 2021. Zwölf Grundsätze für den Informatikunterricht, darunter »erst Code lesen«, »Fehlvorstellungen aufdecken«, »ohne Rechner, dann zurück zum Konzept« und PRIMM, jeweils kurz und mit Beispielen aus Schulen, auf Englisch: <a href='https://www.raspberrypi.org/hello-world/issues/the-big-book-of-computing-pedagogy'>frei als PDF</a>.</p>
</div>

## PRIMM und Use-Modify-Create

Programmieren lernt man in Stufen. Zuerst sagen die Lernenden voraus, was ein fertiges Programm tut (Predict), führen es aus (Run), untersuchen, wie es das tut (Investigate), ändern es (Modify) und schreiben erst zuletzt ein eigenes (Make). Das ist PRIMM. Das ältere Use-Modify-Create sagt dasselbe gröber und für Projekte: etwas benutzen, das läuft, es ändern, dann etwas Neues bauen. Beide antworten auf dieselbe Beobachtung: Eine leere Datei am Anfang überfordert viele, und ein fertiges Programm abzuschreiben lehrt wenig.

_person(sentance)

**Im Workspace** folgt fast jede Aktivität dieser Reihenfolge:

- **Predict:** Karten, die nach einer Vermutung fragen, bevor etwas läuft (mehr dazu unter [Vorhersagen](#7-vorhersagen)).
- **Run, Investigate, Modify:** Kästen »Ausprobieren« schlagen eine kleine Änderung vor und sagen, worauf man achten soll; geprüft wird nichts, und die Seite geht mit dem unveränderten Code weiter.
- **Make:** Kästen »Jetzt bist du dran« verlangen die nächste kleine Funktion selbst, nachdem die Seite eine ähnliche gezeigt hat. Ein Beispiel ist [Raytracing in C](/raytracing).
- **Create:** Die meisten Notizen für Lehrkräfte enden mit »Eigene Projekte«; wie man sie begleitet, steht unter [Eigene Projekte begleiten](/lehrmaterial-projekte).

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Teaching computer programming with PRIMM: a sociocultural perspective</strong>, Sue Sentance, Jane Waite und Maria Kallia, Computer Science Education, 2019. Die fünf Stufen, warum über Code gesprochen wird, bevor man ihn schreibt, und was Lehrkräfte in einer Studie damit erlebt haben: <a href='https://suesentance.net/wp-content/uploads/2020/02/teaching_computer_programming_with_primm__a_sociocultural_perspective_author_copy.pdf'>Fassung der Autorinnen</a>, <a href='https://doi.org/10.1080/08993408.2019.1608781'>beim Verlag</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Computational thinking for youth in practice</strong>, Irene Lee und andere, ACM Inroads, 2011. Use-Modify-Create, an Projekten mit Jugendlichen beschrieben: <a href='https://doi.org/10.1145/1929887.1929902'>bei der ACM</a>.</p>
</div>

## Erst lesen, dann schreiben

Eine Studie in mehreren Ländern fand: Wer ein kurzes Programm nicht sicher von Hand nachverfolgen kann, kann auch keines schreiben. Lesen, Nachverfolgen und Schreiben bauen in dieser Reihenfolge aufeinander auf. Spätere Arbeiten machten daraus eine Reihenfolge für den Unterricht: erst lesen und nachverfolgen, dann erklären, dann Lücken füllen, dann schreiben. Lesen zu üben ist also kein Umweg vor dem »richtigen« Programmieren, sondern dessen erster Schritt.

**Im Workspace:**

- [Katas: Schleifen](/kata-loops) gehen genau so vor: drei Vorhersagen, eine Trace-Tabelle, in der die Lernenden eine `while`-Schleife Durchlauf für Durchlauf aufschreiben, dann ein Parsons-Puzzle und erst danach die erste Kata, die man selbst schreibt. Die Notiz für Lehrkräfte nennt den Ablauf.
- In den geführten Projekten erklärt die Seite einen Schritt am Code, bevor »Jetzt bist du dran« den nächsten verlangt.
- Code steht zum Abtippen da, ohne Kopierknopf, außer bei großen Blöcken und bei Code, den man nicht tippen soll: Beim Tippen liest man mit.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>A multi-national study of reading and tracing skills in novice programmers</strong>, Raymond Lister und andere, ACM SIGCSE Bulletin, 2004. Wer Code nicht lesen und nachverfolgen kann, schreibt auch keinen: <a href='https://opus.lib.uts.edu.au/bitstream/10453/4126/3/2004000904.pdf'>frei als PDF</a>, <a href='https://doi.org/10.1145/1041624.1041673'>bei der ACM</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>A theory of instruction for introductory programming skills</strong>, Benjamin Xie und andere, Computer Science Education, 2019. Vier Fähigkeiten in einer Reihenfolge: Code lesen, Code schreiben, Muster lesen, Muster anwenden: <a href='https://doi.org/10.1080/08993408.2019.1565235'>beim Verlag</a>.</p>
</div>

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>The Programmer's Brain</strong>, Felienne Hermans, Manning, 2021. Was beim Lesen von Code im Kopf geschieht, für Lehrkräfte gut zu lesen; auch zu den Rollen von Variablen und zum Arbeitsgedächtnis weiter unten.</p>
</div>

## Parsons-Puzzles

Alle Zeilen eines richtigen Programms sind da, aber durcheinander. Die Lernenden ziehen sie in die richtige Reihenfolge und rücken sie ein. Weil die Syntax schon stimmt, denken sie über das nach, worauf es ankommt: was in die Schleife gehört, was davor steht und was danach. Zwei Zeilen gehören nicht dazu; jede ist ein typischer Fehler, den man erkennen muss. Zwischen dem Lesen eines Programms und dem eigenen Schreiben brauchen viele genau diese Stufe, und ein Puzzle ist schneller gelöst als dasselbe Programm geschrieben.

**Im Workspace** beginnt die Stufe »Einstieg« der Katas zu Schleifen, Bedingungen, Texten, Listen, Dictionaries, Rekursion und Suchen mit einem Puzzle, in allen fünf Sprachen. Geprüft wird das Ergebnis wie jede Kata: Die Prüfung führt die Zeilen in der gelegten Reihenfolge aus. In den [Katas: Schleifen](/kata-loops) kommt das Puzzle nach der Trace-Tabelle und vor der ersten Kata, die man selbst schreibt.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Parson's programming puzzles: a fun and effective learning tool for first programming courses</strong>, Dale Parsons und Patricia Haden, Australasian Computing Education Conference, 2006. Die ersten Puzzles, damals für Turbo Pascal, mit Zeilen, die typische Fehler enthalten: <a href='https://www.semanticscholar.org/paper/bc5e78332739c1ef033f2bbb1aa0824fd18938e0'>bei Semantic Scholar</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Parsons Problems and Beyond: Systematic Literature Review and Empirical Study Designs</strong>, Barbara Ericson und andere, ITiCSE Working Group Reports, 2022. Was die Forschung über Parsons-Puzzles weiß, und wie man sie untersucht: <a href='https://juholeinonen.com/assets/pdf/ericson2022parsons.pdf'>frei als PDF</a>, <a href='https://doi.org/10.1145/3571785.3574127'>bei der ACM</a>.</p>
</div>

## Notional Machine

Um ein Programm zu lesen oder zu schreiben, braucht man ein Bild davon, was der Rechner tut, wenn es läuft: wo eine Variable ihren Wert hat, was eine Zuweisung ändert, was bei einem Aufruf geschieht. Dieses Bild ist nicht die echte Hardware, sondern eine vereinfachte Maschine auf der Ebene der Sprache, die *notional machine* (Benedict du Boulay). Viele Fehler von Anfängerinnen und Anfängern sind Fehler in diesem Bild, nicht in der Syntax. Unterricht macht die Maschine sichtbar: mit Trace-Tabellen, Zeichnungen des Speichers und Werkzeugen, die Schritt für Schritt zeigen, was geschieht.

**Im Workspace:**

- Die Trace-Tabelle in den [Katas: Schleifen](/kata-loops): Die Rückmeldung nennt das erste falsche Feld.
- Der [Query Stepper](/query-stepper) zeigt für SQL, welchen Teil einer Abfrage die Datenbank zuerst ausführt; [Einführung in SQL](/mysql) verlinkt ihn.
- In [Vom Byte zum 3D-Labyrinth](/sandbox-cpu) ist die Maschine wörtlich zu sehen: ein Speicher, Register, Takt für Takt. Die Notiz für Lehrkräfte nennt unter »Worüber sich ein Gespräch lohnt« genau solche Bilder: ein einziger Speicher, ein Label ist eine Adresse, 255 + 1 = 0.
- [Datenstrukturen von innen sehen](/sandbox-datenstrukturen), und Pip und die Schildkröte als kleine Maschinen, deren Zustand man sieht ([Pip unter Funkelbach](/pip-1), [Turtle: Bilder aus Befehlen](/turtle-1)).

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Some difficulties of learning to program</strong>, Benedict du Boulay, Journal of Educational Computing Research, 1986. Woran Anfänger beim Programmieren scheitern, darunter die Maschine, die sie sich vorstellen: <a href='https://doi.org/10.2190/3LFX-9RRF-67T8-UVK9'>beim Verlag</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Notional machines and introductory programming education</strong>, Juha Sorva, ACM Transactions on Computing Education, 2013. Was eine notional machine ist, und wie Unterricht sie sichtbar machen kann: <a href='https://doi.org/10.1145/2483710.2483713'>bei der ACM</a>.</p>
</div>

## Rollen von Variablen

Jorma Sajaniemi fand, dass etwa zehn Rollen fast alle Variablen in Programmen für Anfängerinnen und Anfänger beschreiben: Stepper, Gatherer, Most-wanted Holder, One-way Flag und einige mehr. Jede Rolle bringt ihre Zeilen mit; ein Gatherer etwa braucht einen Anfangswert vor der Schleife und eine Zuweisung darin. Wer die Rollen benennt, gibt Lehrkraft und Klasse eine gemeinsame Sprache für Schleifen, und aus »es geht nicht« wird »der Gatherer fängt beim falschen Wert an«.

_person(sajaniemi)

**Im Workspace:**

- Die Handbuch-Seite [Rollen von Variablen](/reference/variablen-rollen), mit jeder Rolle in Pseudocode.
- In den [Katas: Schleifen](/kata-loops) trennt die Karte »Welche Rolle?« einen Stepper von einem Gatherer, und bei `factorial` nennt die Rückmeldung `product` einen Gatherer, der mit dem Wert anfangen muss, der nichts ändert.
- Die Notiz für Lehrkräfte der Katas: Schleifen nennt unter »Was schiefgeht« den häufigsten Fehler, jeden Zähler für einen Gatherer zu halten.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Using Roles of Variables in Programming Education</strong>, Jorma Sajaniemi. Wie man die Rollen im Unterricht einführt, mit Listen der Rollen für Lernende in Python, Java und C: <a href='https://saja.kapsi.fi/var_roles/teaching.html'>auf seiner Seite</a>.</p>
</div>

## Vorhersagen

Die deutsche Unterrichtsforschung trennt, was man in einer Stunde sieht (Methoden, Sozialformen, die Sichtstruktur), von dem, was Lernen bewirkt (die Tiefenstruktur). Eine ihrer drei Basisdimensionen ist die kognitive Aktivierung: Aufgaben, die alle zum Nachdenken bringen, nicht nur die Schnellsten. Eine Vorhersage ist die einfachste solche Aufgabe im Programmierunterricht. Wer sich auf eine Antwort festgelegt hat, schaut beim Ausprobieren genau hin, und eine falsche Vermutung ist der Anfang eines Gesprächs, kein Versagen.

**Im Workspace:**

- Was die Antwort einer Vorhersage verrät, ein Bild vom Ergebnis oder der Absatz, der es erklärt, wartet, bis eine Vermutung abgegeben ist, richtig oder falsch. Ein Beispiel ist »Die Reihenfolge« in [Einführung in SQL](/mysql).
- Du siehst die Seite wie deine Klasse und öffnest eine solche Stelle mit »Für Lehrkräfte: jetzt zeigen«, etwa für die Besprechung an der Tafel.
- Für die Klassenseite zählt nur der erste Versuch; dort steht, was die Klasse zuerst vermutet hat (mehr dazu unter [Diagnostische Fragen](#9-diagnostische-fragen)).

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Generic dimensions of teaching quality: the German framework of Three Basic Dimensions</strong>, Anna-Katharina Praetorius, Eckhard Klieme, Benjamin Herbert und Petra Pinger, ZDM Mathematics Education, 2018. Klassenführung, konstruktive Unterstützung und kognitive Aktivierung, und was die Forschung über sie weiß: <a href='https://doi.org/10.1007/s11858-018-0918-4'>beim Verlag</a>.</p>
</div>

## Fehlvorstellungen

Fehler von Anfängerinnen und Anfängern sind selten zufällig. Viele kommen aus einer Vorstellung, die in sich stimmig, aber falsch ist: `=` als Gleichung, eine Variable, die mehrere Werte zugleich hält, `return`, das etwas ausgibt, eine Schleife, die einmal zu oft läuft, weil »bis« die Grenze einschließt. Die Forschung hat solche Fehlvorstellungen gesammelt. Wer sie kennt, stellt die Frage, die eine davon ans Licht bringt, statt nur das Symptom zu korrigieren.

Dazu gehört, wie eine Rückmeldung klingt. Ein bloßes »falsch« sagt nur, dass etwas nicht stimmt; eine Rückmeldung hilft, wenn sie sagt, was das Programm tatsächlich tut, und an einem Beispiel zeigt, wo das vom Gewollten abweicht.

**Im Workspace:**

- Die Prüfungen rufen den Code mit eigenen Werten auf und nennen bei einem typischen Fehler, was passiert ist, etwa ein `range(1, n)`, das vor `n` aufhört.
- Die falschen Antworten einer Auswahl sind ausgesuchte Fehlvorstellungen, jede mit ihrer eigenen Rückmeldung.
- Die meisten Notizen für Lehrkräfte haben eine Zeile »Was schiefgeht«: die Fehler, die in dieser Aktivität am häufigsten vorkommen, und was dann hilft.
- Die Zeilen eines Parsons-Puzzles, die nicht dazugehören, sind typische Fehler.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Students' Misconceptions and Other Difficulties in Introductory Programming: A Literature Review</strong>, Yizhou Qian und James Lehman, ACM Transactions on Computing Education, 2017. Die Fehlvorstellungen, die die Forschung kennt, geordnet, und was gegen sie hilft: <a href='https://doi.org/10.1145/3077618'>bei der ACM</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Programming Misconceptions for School Students</strong>, Alaaeddin Swidan, Felienne Hermans und Marileen Smit, ICER 2018. Welche Fehlvorstellungen Schülerinnen und Schüler haben, nicht Studierende: <a href='https://doi.org/10.1145/3230977.3230995'>bei der ACM</a>.</p>
</div>

## Diagnostische Fragen

Formative Beurteilung heißt: während der Stunde herausfinden, was die Klasse verstanden hat, und die Stunde danach ändern. Ihr schärfstes Werkzeug ist die Hinge Question, eine Frage an der Stelle, an der die Stunde weitergeht oder nicht. Sie ist schnell beantwortet und schnell gelesen, meist eine Auswahl, und jede falsche Antwort gehört zu einer Fehlvorstellung. Die Antworten der Klasse entscheiden, ob es weitergeht oder noch einmal zurück.

_person(wiliam)

**Im Workspace** übernimmt die Klassenseite das Zählen:

- Unter der Tabelle im Reiter Fortschritt steht für jede Vorhersage, welche Antworten die Klasse zuerst gegeben hat und wie oft, ohne Namen, und bei jeder falschen die Rückmeldung, die sie bekommt ([Die Klassenseite](/lehrmaterial-klassenseite)). Was viele zuerst vermuten, ist der Anfang der Besprechung.
- Karten nach dem Muster »Vom Beispiel zur Regel« sind Hinge Questions: die richtige Regel neben zwei, drei typisch falschen Verallgemeinerungen. Ein Beispiel ist »Was jeder Baum verrät« in [Ein Rechner für Ausdrücke](/calculator), nach dem Baum zum Durchklicken.
- Eine gelbe Spalte heißt: Viele liegen bei dieser Aufgabe gerade falsch. Mit dem Treffpunkt hältst du die Klasse an einer Aufgabe an, um gemeinsam darüber zu sprechen.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Assessment and Classroom Learning</strong>, Paul Black und Dylan Wiliam, Assessment in Education, 1998. Die Übersicht über die Studien zur formativen Beurteilung, auf der »Inside the Black Box« beruht: <a href='https://doi.org/10.1080/0969595980050102'>beim Verlag</a>.</p>
</div>

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>Embedded Formative Assessment</strong>, Dylan Wiliam, Solution Tree, 2011. Fünf Strategien für formative Beurteilung im Unterricht, die Hinge Questions im Kapitel über das Erheben von Belegen: <a href='https://archive.org/details/embeddedformativ0000wili'>im Internet Archive auszuleihen</a>.</p>
</div>

## Semantic Waves

Ein Begriff ist gelernt, wenn man zwischen dem Allgemeinen und dem Konkreten in beide Richtungen wechseln kann: hinab in ein Beispiel, und wieder hinauf zur allgemeinen Aussage, in eigenen Worten. Erklärungen, die im Allgemeinen bleiben, versteht man nicht; Erklärungen, die beim Beispiel bleiben, überträgt man nicht. Karl Maton beschreibt diese Bewegung als Welle, Paul Curzon und Jane Waite haben sie in den Informatikunterricht gebracht, auch für Aktivitäten ohne Rechner.

**Im Workspace:**

- »Vom Beispiel zur Regel«: Ein Abschnitt, der eine Regel an Beispielen einführt, endet mit einer Karte, auf der die Lernenden die Regel selbst fassen, bevor die Seite sie ausspricht. Die Regel wartet, bis eine Antwort da ist (»Was jeder Baum verrät« in [Ein Rechner für Ausdrücke](/calculator)).
- Bei Aktivitäten ohne Rechner in den Notizen für Lehrkräfte führt die Zeile »Zurück zur Seite« vom Papier wieder hinauf zum Begriff und zum Code.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Using semantic waves to analyse the effectiveness of unplugged computing activities</strong>, Paul Curzon, Jane Waite, Karl Maton und James Donohue, WiPSCE 2020. Warum eine Aktivität ohne Rechner gelingt, wenn sie zum Begriff zurückkehrt: <a href='https://doi.org/10.1145/3421590.3421606'>bei der ACM</a>. Kurz und frei erklärt bei <a href='https://teachinglondoncomputing.org/semantic-waves/'>Teaching London Computing</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Making semantic waves: A key to cumulative knowledge-building</strong>, Karl Maton, Linguistics and Education, 2013. Die Welle zwischen dem Abstrakten und dem Konkreten, ursprünglich für Unterricht überhaupt: <a href='https://doi.org/10.1016/j.linged.2012.11.005'>beim Verlag</a>.</p>
</div>

## Cognitive Load und Lösungsbeispiele

Das Arbeitsgedächtnis hält nur wenige neue Elemente auf einmal. Eine Aufgabe, bei der man die Lösung erst suchen muss, verbraucht den größten Teil davon für die Suche und lässt wenig für das Lernen übrig. Am Anfang helfen durchgerechnete Beispiele mehr, besonders wenn ihre Schritte sagen, was jeder Teil erreicht (Subgoal Labels). Mit wachsendem Können wird die Hilfe weniger: vom Beispiel über eine Lücke zur leeren Datei.

_person(sweller)

**Im Workspace:**

- Die Seiten zeigen meist nur die Zeilen, die sich ändern, mit ein paar Zeilen davor und danach, statt der ganzen Datei. So geht die eine neue Idee nicht in einer kopierten Datei unter; ein Beispiel ist [Ein eigener Webserver](/webserver).
- Schwere Stellen bleiben durchgerechnete Beispiele, die die Seite erklärt (Brechung, Fresnel und Path Tracing in [Raytracing in C](/raytracing)); kleine Stellen werden zu »Jetzt bist du dran«.
- Ein Gerüst mit einer Lücke statt einer leeren Seite.
- Ausdrücklich beschriftete Teilziele (Subgoal Labels) gibt es im Workspace nicht; am nächsten kommen ihnen die Überschriften der Schritte.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Cognitive Load During Problem Solving: Effects on Learning</strong>, John Sweller, Cognitive Science, 1988. Warum das Suchen nach einer Lösung das Lernen bremst: <a href='https://doi.org/10.1207/s15516709cog1202_4'>beim Verlag</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Subgoals, Context, and Worked Examples in Learning Computing Problem Solving</strong>, Briana Morrison, Lauren Margulieux und Mark Guzdial, ICER 2015. Lösungsbeispiele mit beschrifteten Teilzielen im Programmierunterricht: <a href='https://doi.org/10.1145/2787622.2787733'>bei der ACM</a>.</p>
</div>

## Gestufte Hilfen

Hilfe wirkt am besten, wenn sie so wenig gibt wie nötig: zuerst Hilfe, die Aufgabe zu verstehen, dann den Weg, dann ein Gerüst, nie zuerst die ganze Lösung. Die Idee geht auf das *scaffolding* zurück (David Wood, Jerome Bruner und Gail Ross): eine Stütze, die abgebaut wird, je weiter jemand kommt. Barak Rosenshines Grundsätze für den Unterricht, eine Zusammenfassung für Lehrkräfte, ergänzen die andere Seite: einen Schritt vormachen, bevor die Lernenden ihn tun, die ersten Übungen begleiten, oft nachfragen, ob es verstanden ist.

**Im Workspace:**

- Tipps in Stufen: Die nächste erscheint erst eine Minute, nachdem die vorige aufgeklappt wurde, ohne Zahlen und ohne zu verraten, wie viele noch kommen. Beispiele sind [Komprimieren mit Huffman](/huffman) und [Ein Routenplaner für Berlin](/routenplaner).
- Niemand sieht, wer welchen Tipp gebraucht hat, auch du nicht: Was aufgeklappt wurde, merkt sich nur der Browser. Die Lernenden sollen Hilfe nehmen, ohne sich zu fragen, wie das aussieht. Wenn du wissen willst, wer Hilfe braucht, fragst du.
- Vormachen: Die Seite erklärt eine kleine Funktion, bevor »Jetzt bist du dran« die nächste verlangt, und mit dem Treffpunkt hältst du die Klasse an, um einen Schritt an der Tafel vorzumachen.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>Principles of Instruction: Research-Based Strategies That All Teachers Should Know</strong>, Barak Rosenshine, American Educator, 2012. Zehn Grundsätze, kurz und für Lehrkräfte geschrieben, von der Wiederholung zu Beginn bis zum Vormachen und Begleiten: <a href='https://www.aft.org/sites/default/files/Rosenshine.pdf'>frei als PDF</a>.</p>
</div>

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>The role of tutoring in problem solving</strong>, David Wood, Jerome Bruner und Gail Ross, Journal of Child Psychology and Psychiatry, 1976. Der Aufsatz, auf den das scaffolding zurückgeht: <a href='https://doi.org/10.1111/j.1469-7610.1976.tb00381.x'>beim Verlag</a>.</p>
</div>

## Ohne Rechner

Begriffe der Informatik ohne Computer: mit Karten, Schnüren, der Klasse als Netzwerk. Weg vom Bildschirm versteckt sich die Idee nicht hinter Syntax und Werkzeugen, und alle können mitmachen. Die Gefahr, die Studien zeigen: eine Aktivität, die ein Spiel bleibt. Sie wirkt, wenn die Klasse vom Papier zum Begriff und zum Code zurückkehrt.

_person(bell)

**Im Workspace** haben einige Notizen für Lehrkräfte eine Zeile »Ohne Rechner«, immer mit »Was sichtbar wird« und »Zurück zur Seite«: die Codes für MISSISSIPPI in [Komprimieren mit Huffman](/huffman), der Kartentrick mit Paritätsbits in [Prüfziffern](/check-digits), die Schatzsuche in [Endliche Automaten](/automata), die Waage in [Sortieralgorithmen erforschen](/sandbox-sortieren) und weitere. Wo eine Aktivität von CS Unplugged kommt, nennt die Notiz sie als Vorbild.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>CS Unplugged: How Is It Used, and Does It Work?</strong>, Tim Bell und Jan Vahrenhold, in: Adventures Between Lower Bounds and Higher Altitudes, Springer, 2018. Woher die Aktivitäten kommen, wie sie eingesetzt werden und was Studien über ihre Wirkung sagen: <a href='https://doi.org/10.1007/978-3-319-98355-4_29'>beim Verlag</a>.</p>
</div>

Die Aktivitäten selbst stehen frei bei <a href='https://www.csunplugged.org/de/'>CS Unplugged</a>, unter einer Lizenz CC BY-SA, zum Teil auch auf Deutsch.

## Zum Weiterlesen

<div class='book'>
<i class='bi bi-book'></i>
<p><strong>Computer Science Education: Perspectives on Teaching and Learning in School</strong>, herausgegeben von Sue Sentance, Erik Barendsen, Nicol R. Howard und Carsten Schulte, Bloomsbury, 2. Auflage 2023. Die Forschung zum Informatikunterricht an Schulen in einem Band, Kapitel für Kapitel von Fachleuten geschrieben.</p>
</div>
