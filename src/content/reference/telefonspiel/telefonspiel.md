<div class='meta'>
image: teaser.webp
image_alt: "Das Telefon im Browser während eines Anrufs bei der Höhle: links ein Telefon mit Display, Tastenfeld und rotem Hörer zum Auflegen; rechts das Gespräch, die Sätze des Spiels in Blasen"
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Telefonspiel

<p class='abstract'>
Alle Befehle des Telefons zum Nachschlagen, in Ruby, Python und JavaScript, die
Stimmungen zum Anhören, eigene Aufnahmen statt Thorstens Stimme und wie
Thorsten ein Wort richtig ausspricht. Wie alles
zusammenspielt, zeigt die Aktivität <a href='/telefonspiel'>»Ein Spiel am
Telefon«</a>; fertige Spiele stehen im <a href='/telefonbuch'>Telefonbuch</a>.
</p>

## Ein Spiel starten

<!-- part: erste-zeile -->
<!-- short: Ein Telefonspiel ist ein gewöhnliches Programm; seine erste Zeile holt die Befehle des Telefons. -->
### Die erste Zeile

Ein Telefonspiel ist ein gewöhnliches Programm in Ruby, Python oder
JavaScript. Seine erste Zeile holt die Befehle des Telefons; in Python und
JavaScript nennt sie jeden Befehl, den das Spiel benutzt.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>Ruby</th><th>Python</th><th>JavaScript</th></tr></thead>
<tbody>
<tr><th>Die erste Zeile</th><td><code>require 'telefon'</code></td><td><code>from telefon import keys, music, say, sleep</code></td><td><code>const { keys, music, say, sleep } = require('telefon');</code></td></tr>
</tbody>
</table>
</div>

<!-- /part -->

<!-- part: telefon -->
<!-- short: `telefon spiel.rb` startet das Telefon im Browser auf Port 8040; im Testmodus spielt es das Spiel ohne Telefon durch, mit festen Tasten. -->
### Der Befehl telefon

Im Terminal, im Ordner deines Spiels:

```bash
telefon spiel.rb
```

Das Terminal nennt eine Adresse; sie steht auch im Reiter »Ports« bei Port
8040. Im Browser ruft ein Klick auf den grünen Hörer an. Jeder Anruf startet
das Programm neu aus der Datei, eine Änderung hörst du also beim nächsten
Anruf. Sobald `telefon` läuft, holt es die Musik deines Spiels und bereitet
seine festen Sätze vor; im Terminal steht, wann beides bereit ist. Beenden
mit <kbd>Strg</kbd><kbd>C</kbd>.

Ohne Telefon und ohne Ton spielt `--test` das Spiel durch, mit festen Tasten
für jedes `keys`, durch Kommas getrennt, `-` für keine Taste:

```bash
telefon --test --keys 1,2 spiel.rb
```

Jede Zeile der Ausgabe ist ein Ereignis: `Telefon:` für einen Satz, `Tasten:`
für die Tasten, `Musik:`, `Pause:` und am Schluss `Ende:` mit dem Grund. So
prüfen auch die Aufgaben dein Spiel.
<!-- /part -->

## Alle Befehle

<!-- part: befehle -->
<!-- short: `say`, `play`, `keys`, `clear_keys`, `music`, `sleep`, `hangup`, `transfer`, `call`, `back`, `pick_up`, `title` und `subtitle`, in Ruby, Python und JavaScript nebeneinander. -->
### Auf einen Blick

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>Ruby</th><th>Python</th><th>JavaScript</th></tr></thead>
<tbody>
<tr><th>Etwas sagen</th><td><code>say 'Hallo!'</code></td><td><code>say('Hallo!')</code></td><td><code>say('Hallo!');</code></td></tr>
<tr><th>… mit einer Taste abzubrechen</th><td><code>say '…', interrupt: true</code></td><td><code>say('…', interrupt=True)</code></td><td><code>say('…', { interrupt: true });</code></td></tr>
<tr><th>… in einer Stimmung</th><td><code>say '…', mood: 'whisper'</code></td><td><code>say('…', mood='whisper')</code></td><td><code>say('…', { mood: 'whisper' });</code></td></tr>
<tr><th>Eine eigene Aufnahme abspielen</th><td><code>play 'eingang.mp3'</code></td><td><code>play('eingang.mp3')</code></td><td><code>play('eingang.mp3');</code></td></tr>
<tr><th>Auf eine Taste warten</th><td><code>key = keys(1)</code></td><td><code>key = keys(1)</code></td><td><code>const key = keys(1);</code></td></tr>
<tr><th>Auf bis zu drei Tasten, 5 Sekunden je Taste</th><td><code>code = keys(3, timeout: 5)</code></td><td><code>code = keys(3, timeout=5)</code></td><td><code>const code = keys(3, 5);</code></td></tr>
<tr><th>Musik von YouTube</th><td><code>music 'https://…'</code></td><td><code>music('https://…')</code></td><td><code>music('https://…');</code></td></tr>
<tr><th>Musik aus einer Datei</th><td><code>music 'hoehle.mp3'</code></td><td><code>music('hoehle.mp3')</code></td><td><code>music('hoehle.mp3');</code></td></tr>
<tr><th>Leiser</th><td><code>music '…', volume: 0.3</code></td><td><code>music('…', 0.3)</code></td><td><code>music('…', 0.3);</code></td></tr>
<tr><th>Nur ein Ausschnitt</th><td><code>music '…', start: '1:15', stop: '1:52'</code></td><td><code>music('…', start='1:15', stop='1:52')</code></td><td><code>music('…', { start: '1:15', stop: '1:52' });</code></td></tr>
<tr><th>Musik aus</th><td><code>music nil</code></td><td><code>music(None)</code></td><td><code>music(null);</code></td></tr>
<tr><th>Warten</th><td><code>sleep 3</code></td><td><code>sleep(3)</code></td><td><code>sleep(3);</code></td></tr>
<tr><th>Auflegen</th><td><code>hangup</code></td><td><code>hangup()</code></td><td><code>hangup();</code></td></tr>
<tr><th>Gedrückte Tasten vergessen</th><td><code>clear_keys</code></td><td><code>clear_keys()</code></td><td><code>clearKeys();</code></td></tr>
<tr><th>An ein anderes Spiel weiterleiten</th><td><code>transfer '123'</code></td><td><code>transfer('123')</code></td><td><code>transfer('123');</code></td></tr>
<tr><th>… mit Ziffern dahinter</th><td><code>transfer 'DICE', '6'</code></td><td><code>transfer('DICE', '6')</code></td><td><code>transfer('DICE', '6');</code></td></tr>
<tr><th>Ein Spiel anrufen und auf seine Antwort warten</th><td><code>wurf = call('DICE', '6')</code></td><td><code>wurf = call('DICE', '6')</code></td><td><code>const wurf = call('DICE', '6');</code></td></tr>
<tr><th>Dem Spiel antworten, das angerufen hat</th><td><code>back '4'</code></td><td><code>back('4')</code></td><td><code>back('4');</code></td></tr>
<tr><th>Erst nach acht Sekunden Klingeln abnehmen</th><td><code>pick_up 8</code></td><td><code>pick_up(8)</code></td><td><code>pickUp(8);</code></td></tr>
<tr><th>Der Name des Spiels</th><td><code>title 'Die Höhle'</code></td><td><code>title('Die Höhle')</code></td><td><code>title('Die Höhle');</code></td></tr>
<tr><th>Ein Satz fürs Telefonbuch</th><td><code>subtitle '…'</code></td><td><code>subtitle('…')</code></td><td><code>subtitle('…');</code></td></tr>
</tbody>
</table>
</div>

<!-- /part -->

<!-- part: say -->
<!-- short: `say` sagt einen Text und kehrt zurück, wenn er gesprochen ist; `interrupt` lässt eine Taste ihn abbrechen. -->
### say

`say` sagt einen Text und kehrt erst zurück, wenn er gesprochen ist; dann
läuft das Programm weiter. Ein langer Text wird Satz für Satz gesprochen.

Eine Taste, die jemand drückt, während das Spiel spricht, geht nicht
verloren: Sie wartet auf das nächste `keys`. Den Satz muss man trotzdem bis
zum Ende hören, außer er hat `interrupt`: Dann bricht er bei der ersten Taste
ab, und das nächste `keys` bekommt genau diese Taste. Das passt zu Sätzen, die
man beim zweiten Mal schon kennt.

Ein Satz, in den das Programm erst beim Anruf etwas einsetzt
(`"#{punkte} Punkte"` in Ruby, `f'{punkte} Punkte'` in Python), wird erst dann
gemacht; wer anruft, wartet einen Moment. Die anderen Sätze desselben `say`
sind vorbereitet.
<!-- /part -->

<!-- part: keys -->
<!-- short: `keys(1)` wartet auf eine Taste und gibt sie als Text zurück, `''` nach zehn Sekunden ohne Taste. -->
### keys

`keys(1)` wartet auf eine Taste und gibt sie als Text zurück: `'1'`, nicht die
Zahl 1. Vergleiche darum mit `'1'` in Anführungszeichen (in JavaScript mit
`===`). Drückt zehn Sekunden lang niemand etwas, gibt `keys` einen leeren Text
zurück, `''`.

- `keys(3)` wartet auf bis zu drei Tasten und gibt sie zusammen zurück, etwa
  `'472'`. Kommen weniger, gibt es nach der Wartezeit zurück, was da ist.
- Die Wartezeit gilt für jede Taste neu; die zweite Zahl ändert sie, in
  Sekunden.
- Die Raute `#` beendet eine Eingabe früher und steht nie im Ergebnis. Der
  Stern kommt als `'*'` zurück.
- Tasten, die vor `keys` gedrückt wurden, während das Spiel noch sprach,
  zählen mit (siehe [Tasten im Voraus](#puffer)).
<!-- /part -->

<!-- part: puffer -->
<!-- short: Gedrückte Tasten warten, bis das Spiel sie mit `keys` holt, höchstens 20; jede neue Verbindung beginnt nur mit den Ziffern nach ihrer Nummer; `clear_keys` vergisst sie. -->
### Tasten im Voraus

Eine Taste ist auf der Leitung nur ein kurzer Ton, und die Leitung selbst
merkt sich nichts. Was sich Tasten merkt, ist das Gerät am anderen Ende, das
sie hört. Ein einfacher Baustein, der Tastentöne erkennt, hält genau eine
Ziffer fest; die nächste überschreibt sie ([Datenblatt des MT8870](https://www.da.isy.liu.se/vanheden/pdf/mt8870.pdf)).
Telefonanlagen mit Ansagen sammeln dagegen, was jemand schon während der Ansage
drückt, und geben es weiter, sobald gefragt wird. Wie viel sie sammeln, legt
jede Anlage selbst fest. Die Norm für Sprachmenüs, VoiceXML, schreibt keine
Größe vor; sie sagt nur, wann Tasten gesammelt und wann sie gelöscht werden
([W3C, VoiceXML 2.0, 4.1.5 und 4.1.8](https://www.w3.org/TR/voicexml20/)).
Schon die Vermittlungsstellen der 1920er Jahre hatten ein **Register**, das die
gewählten Ziffern festhielt, bis die Verbindung stand
([Sender](https://en.wikipedia.org/wiki/Sender_(telephony))).

Das Telefonspiel macht es so:

- Jede gedrückte Taste wartet, bis das Spiel sie mit `keys` holt, auch
  während `say`, `play` oder `sleep`. Höchstens 20 Tasten warten; was danach
  kommt, geht verloren.
- Jede neue Verbindung beginnt leer: Wird ein Anruf weitergeleitet
  (`transfer`) oder ruft ein Spiel ein anderes an (`call`), bekommt das neue
  Spiel keine Tasten, die für das vorige gedacht waren. Es bekommt nur, was
  nach seiner Nummer gewählt wurde.
- `clear_keys` vergisst alle wartenden Tasten. Das passt vor eine wichtige
  Frage, damit eine Taste von vorhin nicht als Antwort zählt. Es vergisst
  aber auch Ziffern, die nach der Nummer des Spiels gewählt wurden: Lies sie
  deshalb zuerst und ruf `clear_keys` erst danach auf.
<!-- /part -->

<!-- part: nummern -->
<!-- short: Keine Nummer ist der Anfang einer anderen: Die Leitung verbindet, sobald eine Nummer passt, und was danach gewählt wird, bekommt das Spiel als Tasten. -->
### Nummern und Durchwahl

Eine Nummer hat drei oder vier Ziffern, oder Buchstaben, die man mit den
Tasten wählt, auf denen sie stehen: DICE ist 3423. Keine Nummer ist der Anfang
einer anderen. Gibt es 120, gibt es kein 1205, und umgekehrt. Sonst wüsste die
Leitung nach 120 nicht, ob noch eine Ziffer kommt, und müsste warten. So
weiß sie, dass eine Nummer fertig ist, sobald ein Spiel zu ihr passt. Ein
Netz, in dem keine Nummer der Anfang einer anderen ist, heißt
**präfixfrei**.

Was danach gewählt wird, ist eine **Durchwahl**: Wer 34236 wählt, ist bei
DICE, und das Spiel bekommt die 6 als erste Taste, ohne dass jemand sie noch
einmal drückt. Von einem echten Telefon aus geht das genauso, mit den Ziffern
direkt hinter der Nummer des Telefonbuchs.
<!-- /part -->

<!-- part: music -->
<!-- short: `music` spielt einen YouTube-Link oder eine Datei (MP3, WAV, Ogg, M4A) in einer Schleife, leiser, solange gesprochen wird; `music nil` schaltet sie aus. -->
### music

`music` spielt Musik in einer Schleife, bis das Spiel zu Ende ist oder ein
neues `music` sie ersetzt. Solange Thorsten spricht, wird sie leiser. Die
Quelle ist ein Link zu YouTube, von dem das Telefon nur den Ton nimmt, oder
eine Datei im Ordner des Spiels.

- Eine Datei ist MP3, WAV, Ogg oder M4A und liegt direkt neben dem Spiel,
  nicht in einem Unterordner; sonst gibt `music` einen Fehler, denn beim
  Veröffentlichen käme sie nicht mit. MP3 und WAV spielt jeder Browser, Ogg
  und M4A nicht jeder; das Telefon spielt alle vier.
- Ins Telefonbuch kommt eine Datei bis 20 MB, alle zusammen bis 40 MB.
- Von einem Video holt der Workspace einmal den Ton und hebt ihn auf; von
  einem sehr langen Video nur das Stück ab `start`, höchstens zehn Minuten.

- Die Lautstärke liegt zwischen `0` und `1`, ohne Angabe `1`.
- `start` und `stop` spielen nur einen Ausschnitt, in Sekunden (`75`) oder als
  `'1:15'`; der Ausschnitt wiederholt sich. Ohne `stop` geht es ab `start`
  weiter. Ein Link aus »Teilen« bei YouTube mit `t=75` beginnt ebenfalls dort.
- `music` wartet, bis die Musik läuft. Beim ersten Mal holt der Workspace das
  Video; `telefon` holt es schon beim Start.
- Ohne Quelle schaltet `music` die Musik aus: `music nil` in Ruby,
  `music(None)` in Python, `music(null)` in JavaScript.
<!-- /part -->

<!-- part: warten-auflegen -->
<!-- short: `sleep` wartet, die Musik spielt weiter; `hangup` legt sofort auf, egal in welcher Szene. -->
### sleep und hangup

`sleep` wartet so viele Sekunden, wie du angibst. Die Musik spielt weiter, und
Tasten, die währenddessen gedrückt werden, warten auf das nächste `keys`.
In Python kommt `sleep` aus `telefon`, nicht aus `time`: `from time import
sleep` hielte das Programm an, ohne dass das Telefon davon weiß.

Ein Anruf endet, wenn das Programm zu Ende ist. `hangup` legt sofort auf, egal
in welcher Funktion das Spiel gerade ist; danach läuft nichts mehr, und auf
dem Telefon ertönt das Besetztzeichen.
<!-- /part -->

<!-- part: transfer -->
<!-- short: `transfer` leitet den Anruf an das Spiel mit dieser Nummer weiter, als hätte man sie gewählt; das eigene Spiel endet. -->
### transfer

`transfer` leitet den Anruf an ein anderes Spiel im Telefonbuch weiter, zum
Beispiel an die Hotline, die jemand anderes aus der Klasse geschrieben hat:

```ruby
say 'Wir leiten Sie jetzt an die Hotline weiter.'
transfer '123'
```

Der Anruf geht im Spiel mit dieser Nummer weiter, als hätte man sie gewählt,
und das eigene Spiel endet an dieser Stelle. Die Nummer hat drei oder vier
Ziffern oder Buchstaben; was als zweites dahintersteht, wird danach gewählt,
als Durchwahl (`transfer 'DICE', '6'`). Gibt es kein veröffentlichtes Spiel
mit dieser Nummer, bricht das Spiel mit einem Fehler in dieser Zeile ab. Ein Anruf kann höchstens fünfmal
weitergeleitet werden, und alle Spiele eines Anrufs teilen sich seine Zeit.

Mit `telefon` im Workspace geht der Anruf genauso weiter: Das Telefon auf
der Seite verbindet sich mit dem Telefonbuch, und das andere Spiel spricht im
selben Anruf weiter. So lässt sich ein Spiel mit `transfer` ausprobieren,
bevor es veröffentlicht ist. Nach dem Auflegen ruft der grüne Knopf wieder
das eigene Spiel an.
<!-- /part -->

<!-- part: call -->
<!-- short: `call` ruft ein Spiel an und wartet, bis es mit `back` antwortet; die Antwort sind Ziffern, als Töne auf der Leitung zu hören. -->
### call und back

`call` ruft ein anderes Spiel an, und das eigene wartet so lange, wie eine
Telefonanlage einen Anruf hält, während man bei jemandem nachfragt. Wer
anruft, hört die Nummer wählen und ist dann beim anderen Spiel. Antwortet das
mit `back`, ist er wieder beim ersten Spiel, und `call` gibt die Antwort
zurück:

```ruby
wurf = call('DICE', '6')       # wählt 3423 und dann 6
say "Du hast eine #{wurf} gewürfelt."
```

Das gerufene Spiel liest, was nach seiner Nummer gewählt wurde, mit `keys`,
wie jede andere Taste, und antwortet mit `back`:

```ruby
seiten = keys(3).to_i
back rand(1..seiten).to_s
```

- Hin und zurück geht es nur über Tasten: Was nach der Nummer gewählt wird,
  und was `back` zurückgibt, sind Ziffern, höchstens 20. Beides ist als
  Tastentöne auf der Leitung zu hören; das Ende einer Antwort ist die Raute.
  Nach der Nummer darf auch selbst eine Raute stehen (`call('FACT', '012#')`):
  Dann weiß `keys` im gerufenen Spiel, dass nichts mehr kommt.
- Bis das gerufene Spiel etwas sagt (`say`, `play`, `back` …), klingelt es
  auf der Leitung, mal kurz, mal länger; dann hört man, wie es abnimmt. Die
  Ziffern nach seiner Nummer kann es schon vorher mit `keys` lesen. Legt es
  auf, bevor es etwas gesagt hat, hört man das Besetztzeichen, und `call`
  gibt einen leeren Text zurück.
- Wie lange es klingelt, kann ein Spiel selbst bestimmen: `pick_up 8` nimmt
  erst nach acht Sekunden Klingeln ab (in JavaScript `pickUp(8)`). Das geht
  auch, wenn ein Mensch das Spiel anruft: Dann klingelt es für ihn genauso.
  `pick_up` muss vor dem ersten `say` stehen; danach ist schon abgenommen,
  und es passiert nichts.
- `back` beendet das Spiel. Hat ein Mensch es gewählt, ist der Anruf damit zu
  Ende.
- Endet das gerufene Spiel ohne `back`, legt es auf oder bricht es mit einem
  Fehler ab, gibt `call` einen leeren Text zurück. Gibt es die Nummer nicht,
  hört man »Kein Anschluss unter dieser Nummer«, und die Antwort ist auch
  leer.
- Ein Spiel darf sich selbst anrufen. Höchstens zwanzig Anrufe warten
  übereinander; beim einundzwanzigsten sind alle Leitungen überlastet, und
  der Anruf endet.
- Im Workspace erreicht `call` die Spiele, die du beim Start nennst:
  `telefon spiel.rb --nummer FACT` gibt dem Spiel selbst die Nummer FACT,
  `--nummer DICE=wuerfel/dice.rb` ein anderes deiner Spiele.
<!-- /part -->

<!-- part: name -->
<!-- short: `title` und `subtitle` am Anfang der Datei geben dem Spiel seinen Namen und einen Satz fürs Telefonbuch. -->
### title und subtitle

`title` gibt dem Spiel seinen Namen: Das Telefon zeigt ihn an, sobald das
Spiel ihn sagt, und im Telefonbuch steht er. `subtitle` ist ein Satz, der im
Telefonbuch unter dem Namen steht. Beides gehört an den Anfang der Datei;
eingereicht werden kann ein Spiel erst mit beiden.
<!-- /part -->

## Stimmungen

<!-- part: stimmungen -->
<!-- short: Acht Stimmungen: `neutral`, `amused`, `angry`, `disgusted`, `drunk`, `sleepy`, `surprised` und `whisper`. -->
### Alle acht zum Anhören

Thorsten kann einen Satz in acht Stimmungen sagen; hör sie dir an, jede mit
einem Satz, der zu ihr passt:

<div class='voice-chat'>
<button class='btn btn-sm btn-outline-secondary voice-all' type='button'><i class='bi bi-play'></i> Alle nacheinander</button>
<div class='voice-item'><button class='voice-line' type='button' aria-label='ohne Stimmung: Du stehst vor einer dunklen Höhle. Anhören'><span class='voice-mood'>ohne Stimmung</span><span class='voice-text'>»Du stehst vor einer dunklen Höhle.«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/plain.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='neutral: Der Gang führt nach links und nach rechts. Anhören'><span class='voice-mood'>neutral <code>neutral</code></span><span class='voice-text'>»Der Gang führt nach links und nach rechts.«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/neutral.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='flüsternd: Psst, da hinten bewegt sich etwas. Anhören'><span class='voice-mood'>flüsternd <code>whisper</code></span><span class='voice-text'>»Psst, da hinten bewegt sich etwas.«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/whisper.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='angewidert: Igitt, hier riecht es nach faulen Eiern. Anhören'><span class='voice-mood'>angewidert <code>disgusted</code></span><span class='voice-text'>»Igitt, hier riecht es nach faulen Eiern.«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/disgusted.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='wütend: Wer hat meine Fackel ausgeblasen? Anhören'><span class='voice-mood'>wütend <code>angry</code></span><span class='voice-text'>»Wer hat meine Fackel ausgeblasen?«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/angry.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='betrunken: Alles dreht sich, wo ist denn hier der Ausgang? Anhören'><span class='voice-mood'>betrunken <code>drunk</code></span><span class='voice-text'>»Alles dreht sich, wo ist denn hier der Ausgang?«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/drunk.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='müde: Ich bin so müde, ich lege mich kurz hin. Anhören'><span class='voice-mood'>müde <code>sleepy</code></span><span class='voice-text'>»Ich bin so müde, ich lege mich kurz hin.«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/sleepy.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='überrascht: Was? Die Kiste ist voller Gold! Anhören'><span class='voice-mood'>überrascht <code>surprised</code></span><span class='voice-text'>»Was? Die Kiste ist voller Gold!«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/surprised.mp3'></audio></div>
<div class='voice-item'><button class='voice-line' type='button' aria-label='amüsiert: Na, das hast du dir wohl anders vorgestellt! Anhören'><span class='voice-mood'>amüsiert <code>amused</code></span><span class='voice-text'>»Na, das hast du dir wohl anders vorgestellt!«</span><span class='voice-state' aria-hidden='true'><i class='bi bi-volume-up'></i><span class='voice-bars'><i></i><i></i><i></i></span></span></button><audio preload='none' src='moods/amused.mp3'></audio></div>
</div>
<script src='/include/sound_buttons.js'></script>

Die Sätze mit Stimmung spricht eine zweite Stimme von Thorsten, die etwas
weniger klar klingt als die gewohnte. Setz Stimmungen darum sparsam ein, dort,
wo sie etwas erzählen: Ein einziges Flüstern im dunklen Gang wirkt stärker als
zehn.
<!-- /part -->

## Eigene Aufnahmen

<!-- part: aufnahmen -->
<!-- short: `play 'eingang.mp3'` spielt eine eigene Aufnahme aus dem Ordner des Spiels, so wie `say` einen Satz sagt. -->
### play

Statt Thorsten kann auch deine eigene Stimme sprechen, ein Geräusch erklingen
oder eine Melodie, die du gesummt hast. Nimm sie auf, leg sie als MP3 in den
Ordner deines Spiels (WAV, Ogg und M4A gehen auch, wie bei
[music](#music)), und `play` spielt sie ab, wie `say` einen Satz sagt: Die
Musik wird leiser, das Spiel wartet, bis die Aufnahme zu Ende ist, und mit
`interrupt` bricht eine Taste sie ab.

<div class='scroll-x'><table class='table table-sm reference-table reference-table-wide'>
<thead><tr><th></th><th>Ruby</th><th>Python</th><th>JavaScript</th></tr></thead>
<tbody>
<tr><th>Abspielen</th><td><code>play 'eingang.mp3'</code></td><td><code>play('eingang.mp3')</code></td><td><code>play('eingang.mp3');</code></td></tr>
<tr><th>… mit einer Taste abzubrechen</th><td><code>play 'eingang.mp3', interrupt: true</code></td><td><code>play('eingang.mp3', interrupt=True)</code></td><td><code>play('eingang.mp3', { interrupt: true });</code></td></tr>
</tbody>
</table>
</div>

Aufnehmen kannst du am Computer mit [Audacity](https://www.audacityteam.org),
das nichts kostet: aufnehmen, dann »Datei«, »Exportieren«, »Als MP3
exportieren«. Eine Aufnahme vom Handy öffnet Audacity ebenso und speichert sie
als MP3. Die Datei ziehst du in VS Code in den Ordner `telefon`. Auf der Seite
des Telefons steht an ihrer Stelle ihr Name neben einem Mikrofon.

Reichst du dein Spiel ein, kommen die Aufnahmen mit ins Telefonbuch, und dort
kann sie jeder hören. Nimm darum nur auf, was alle hören dürfen, und die Stimme
von jemand anderem nur, wenn die Person einverstanden ist.
<!-- /part -->

## Aussprache

<!-- part: aussprache -->
<!-- short: Zahlen, Abkürzungen und englische Wörter; Lautschrift in `[[ … ]]` spricht Thorsten genau so. -->
### Wie Thorsten liest

Thorsten liest alles, was in `say` steht, wie einen deutschen Text. Meistens
klingt das richtig. Bei Zahlen, Abkürzungen und englischen Wörtern lohnt es
sich, genau hinzuhören:

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Im Text</th><th>Thorsten sagt</th><th>So klingt es anders</th></tr></thead>
<tbody>
<tr><td><code>Raum 123</code></td><td>»Raum hundertdreiundzwanzig«</td><td><code>Raum 1 2 3</code>: »Raum eins zwei drei«</td></tr>
<tr><td><code>ZDF</code></td><td>»Zett De Eff«: Abkürzungen buchstabiert er</td><td></td></tr>
<tr><td><code>Psst</code>, <code>Hm</code>, <code>Brr</code></td><td>den Laut, nicht die Buchstaben</td><td></td></tr>
<tr><td><code>Game Over</code></td><td>»Gahme Ohver«, deutsch gelesen</td><td><code>[[ ɡˈeɪm ]] over</code></td></tr>
</tbody>
</table>
</div>

Ein Komma macht eine kurze Pause, ein Punkt, ein Fragezeichen oder ein
Ausrufezeichen eine längere. Zwischen zwei doppelten eckigen Klammern steht
Lautschrift, das Internationale Phonetische Alphabet (IPA): Dann spricht
Thorsten genau diese Laute. Das Zeichen `ˈ` steht vor der betonten Silbe, `ː`
macht den Laut davor lang. Meist reicht es, nur das Wort in Lautschrift zu
schreiben, das falsch klingt: »over« spricht Thorsten so, wie man es auf
Deutsch meistens sagt.

```text
Leider verloren. [[ ɡˈeɪm ]] over!
```

Die Lautschrift eines Wortes steht im [Wiktionary](https://de.wiktionary.org)
unter »Aussprache«, bei englischen Wörtern im
[englischen Wiktionary](https://en.wiktionary.org). Die Zeichen kopierst du
von dort. Auf der Seite des Telefons steht die Lautschrift ohne die Klammern.
<!-- /part -->

## Zum Nachschlagen

- [Thorsten-Voice](https://www.thorsten-voice.de): die Stimme, ihre Aufnahmen
  und wie sie entstanden ist
- [Piper](https://github.com/OHF-Voice/piper1-gpl): das Programm, das aus Text
  Thorstens Stimme macht
- [Das Internationale Phonetische Alphabet](https://de.wikipedia.org/wiki/Liste_der_IPA-Zeichen):
  alle Zeichen der Lautschrift
