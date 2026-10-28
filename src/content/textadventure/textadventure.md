<div class='meta'>
image: teaser.webp
image_alt: "Ein Terminal, in dem das Textadventure läuft: die Spielerin schließt mit dem Schlüssel die Tür nach Osten auf und steht im Keller"
kind: projekt
language: Ruby / Python
languages: [ruby, python]
duration: 3 Doppelstunden
curriculum: "Sek I: Algorithmisches Problemlösen (Grundlagen der Objektorientierung); Oberstufe: objektorientierte Modellierung (UML-Klassendiagramme), objektorientierte Programmierung (Objekte, die Objekte kennen, Kapselung, Vererbung, Polymorphie)"
topics: [objektorientierung]
requires: [kata-objects]
reference: [uml, terminal]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Ein Textadventure aus Objekten

<p class='abstract'>
Du baust ein Spiel, das man im Terminal spielt: Räume mit Ausgängen,
Gegenstände, die man aufheben und benutzen kann, eine verschlossene Tür und
einen Schlüssel dazu. Jeder Raum, jeder Gegenstand und die Spielerin sind
Objekte, die einander kennen. Zum Schluss baust du deine eigene Welt, und
jemand aus deiner Klasse spielt sie.
</p>

## Das Spiel

So soll es am Ende aussehen. Was hinter `>` steht, tippt man selbst:

```text
Flur
Ein langer Flur mit knarrenden Dielen.
Ausgänge: norden, osten (verschlossen)
> gehe norden
Küche
Es riecht nach Kaffee.
Hier liegt: schlüssel, apfel
Ausgänge: süden
> nimm schlüssel
Du nimmst schlüssel.
> gehe süden
Flur
Ein langer Flur mit knarrenden Dielen.
Ausgänge: norden, osten (verschlossen)
> benutze schlüssel
Du schließt die Tür auf.
```

Solche Spiele heißen **Textadventures**. Das erste bekannte, »Colossal Cave
Adventure«, schrieb Will Crowther 1975 und 1976, nach seinen eigenen Karten
der Mammoth Cave in Kentucky, durch die er selbst gekrochen war; nach ihm
heißt die ganze Gattung Adventure.

Leg einen Ordner für dieses Projekt an:

<!-- language: ruby -->
```bash_copy
mkdir /workspace/textadventure
cd /workspace/textadventure
touch adventure.rb
```

Deine Klassen schreibst du in `adventure.rb`.
<!-- /language -->
<!-- language: python -->
```bash_copy
mkdir /workspace/textadventure
cd /workspace/textadventure
touch adventure.py
```

Deine Klassen schreibst du in `adventure.py`.
<!-- /language -->

## Ein Raum

Ein Raum hat einen Namen und eine Beschreibung. Und er kennt seine Ausgänge:
In jeder Richtung, in der es weitergeht, liegt ein anderer Raum. Ein
Ausgang ist also ein Paar aus einer Richtung und einem Raum, und dafür gibt
es ein Dictionary: Die Richtung ist der Schlüssel, der Raum der Wert. Im
Dictionary steht dabei nicht eine Kopie des anderen Raums, sondern der Raum
selbst: Zwei Räume, die einander kennen, sind zwei Objekte, die aufeinander
zeigen.

_task(raum)

_task(einweg)

<!-- after: einweg -->
`connect` legt einen Ausgang an, nur in eine Richtung: Der Flur kennt
jetzt die Küche, die Küche aber nicht den Flur. Soll man auch zurückgehen
können, verbindet man beide Räume: `kitchen.connect("süden", hall)`. Das
ist kein Fehler, sondern eine Möglichkeit: Eine Rutsche in den Keller führt
nur nach unten.
<!-- /after -->

## Gegenstände

Ein Gegenstand hat einen Namen und eine Beschreibung, wie ein Raum. Ein Raum
hat eine Liste seiner Gegenstände. Hebt jemand etwas auf, nimmt der Raum es
aus seiner Liste und gibt es heraus.

_task(gegenstaende)

## Ein Raum beschreibt sich

Was die Spielerin sieht, wenn sie einen Raum betritt, setzt der Raum selbst
zusammen, aus seinem Namen, seiner Beschreibung, seinen Gegenständen und
seinen Ausgängen. Nur er kennt alle vier; wer ihn fragt, bekommt fertigen
Text.

_task(beschreibung)

## Die Spielerin

Die Spielerin steht in einem Raum und hat eine Tasche. Geht sie in eine
Richtung, fragt sie ihren Raum nach dem Ausgang; gibt es einen, steht sie
danach im nächsten Raum. Ihr Raum ist ein Attribut wie jedes andere: Sie
wechselt den Raum, indem sie dem Attribut einen anderen Raum zuweist.

_task(spielerin)

## Verschlossene Türen

Jetzt wird es ein Spiel: Eine Tür ist verschlossen, und man braucht den
passenden Schlüssel. Ein Apfel lässt sich essen. Beide sind Gegenstände,
aber `benutze` tut bei jedem etwas anderes. Darum bekommt jede Art von
Gegenstand eine eigene Klasse, die von `Item` erbt und die Methode `use`
überschreibt: Die Spielerin ruft nur `use` auf und muss nicht wissen, was
sie in der Hand hat.

Zeichne den Plan zuerst, in der UML-Werkstatt (wie sie funktioniert, steht
im [Handbuch](/reference/uml)):

_task(diagramm)

Eine Tür ist dabei kein eigenes Objekt: Der Raum merkt sich zu jeder
verschlossenen Richtung, welcher Schlüssel passt. Ob eine Tür offen ist,
weiß nur er; die Spielerin fragt ihn, bevor sie durchgeht, und der Schlüssel
bittet ihn, aufzuschließen.

_task(schloss)

## Deine Welt

Die Spielschleife, die Befehle liest und an die Spielerin weitergibt, ist
schon fertig:

<!-- language: ruby -->
```bash_copy
cd /workspace/textadventure
wget ${web_root}/textadventure/starter/play.rb
```

`play.rb` holt sich mit `require_relative 'adventure'` deine Klassen und
ruft `build_world` auf, eine Funktion in `adventure.rb`, die die Welt baut
und die Spielerin in ihren ersten Raum stellt. So sieht eine kleine Welt
aus, mit zwei Räumen:

<!-- page-code: own -->
```ruby
def build_world
  hall = Room.new("Flur", "Ein langer Flur mit knarrenden Dielen.")
  kitchen = Room.new("Küche", "Es riecht nach Kaffee.")
  hall.connect("norden", kitchen)
  kitchen.connect("süden", hall)
  kitchen.add_item(Food.new("apfel", "Knackig und süß."))
  Player.new(hall)
end
```

Spielen kannst du mit `ruby play.rb`.
<!-- /language -->
<!-- language: python -->
```bash_copy
cd /workspace/textadventure
wget ${web_root}/textadventure/starter/play.py
```

`play.py` holt sich mit `from adventure import build_world` die Funktion
`build_world` aus `adventure.py`, die die Welt baut und die Spielerin in ihren
ersten Raum stellt. So sieht eine kleine Welt aus, mit zwei Räumen:

<!-- page-code: own -->
```python
def build_world():
    hall = Room("Flur", "Ein langer Flur mit knarrenden Dielen.")
    kitchen = Room("Küche", "Es riecht nach Kaffee.")
    hall.connect("norden", kitchen)
    kitchen.connect("süden", hall)
    kitchen.add_item(Food("apfel", "Knackig und süß."))
    return Player(hall)
```

Spielen kannst du mit `python3 play.py`.
<!-- /language -->

Die Befehle sind `gehe`, `nimm`, `benutze`, `schau`, `tasche` und `ende`.
Die Namen der Gegenstände schreibst du klein, so wie man sie im Spiel tippt.

_task(welt)

<div class='hint try'>
<strong>Ausprobieren:</strong> Lass jemanden aus deiner Klasse dein Spiel
spielen, ohne etwas zu erklären. Achte darauf, wo sie oder er hängen bleibt:
bei einem Befehl, den das Spiel nicht versteht, bei einem Ausgang, der in
der Beschreibung fehlt, oder bei einem Schlüssel, den man nicht findet.
</div>

_task(spieltest)

## Abgabe

_task(abgabe)

## Wie geht es weiter?

- **Ein Ziel:** Ein Raum, in dem das Spiel endet, wenn man ihn betritt, mit
  einer Nachricht. Wer weiß, ob er erreicht ist: der Raum, die Spielerin oder
  `play`?
- **Eine Lampe:** dunkle Räume, deren Beschreibung man nur sieht, wenn man
  eine Lampe in der Tasche hat. Welche Klasse fragt wen?
- **Speichern:** Wie ließe sich ein Spielstand in eine Datei schreiben und
  wieder laden, mit allen Räumen, die aufeinander zeigen?

## Zum Nachschlagen

- [Klassendiagramme](/reference/uml) im Handbuch: die Schreibweise der
  UML-Werkstatt.
- [Klassen entwerfen: die Expresskasse](/supermarkt): wie man Klassen aus
  einer Geschichte findet.

<!-- tutorial-scope -->
<!-- tutorial-screenshot
# Bild für die Karte der Aktivität: das fertige Spiel im Terminal, in 16:9;
# dabei prüft es jede Aufgabe mit Code an der Lösung
write-file: textadventure/adventure.rb <- file:loesung/adventure.rb
write-file: textadventure/play.rb <- file:starter/play.rb
write-file: textadventure/adventure.py <- file:loesung/adventure.py
write-file: textadventure/textadventure.uml <- file:loesung/textadventure.uml
viewport: 1029x579
zoom: 1.4
hide-left-sidebar
terminal-maximize
terminal-run: cd /workspace/textadventure && clear
terminal-run: ruby play.rb
wait-for-text: knarrenden
terminal-run: gehe norden
terminal-run: nimm schlüssel
terminal-run: gehe süden
terminal-run: benutze schlüssel
terminal-run: gehe osten
wait-for-text: Hier liegt: kiste
sleep: 1
check-task: raum
check-task: gegenstaende
check-task: beschreibung
check-task: spielerin
check-task: diagramm
check-task: schloss
check-task: welt
check-task: raum python
check-task: gegenstaende python
check-task: beschreibung python
check-task: spielerin python
check-task: schloss python
check-task: welt python
-->
<img class='teaser' hidden src='teaser.webp' alt='Der Workspace mit dem Terminal in großer Schrift, in dem das Textadventure läuft: Im Flur ist der Ausgang nach Osten verschlossen; nach »benutze schlüssel« antwortet das Spiel »Du schließt die Tür auf.«, und nach »gehe osten« steht die Spielerin im Keller, in dem eine Kiste liegt.'>

<!-- /tutorial-scope -->
