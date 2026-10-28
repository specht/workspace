<div class='meta'>
image: teaser.webp
image_alt: "Das Funkgerät: eine dunkle Tafel mit der Frequenz 42.7 kHz, darunter fünf runde Lampen in Grün, Gelb, Rot, Blau und Lila mit den Knöpfen 1 bis 5; Grün und Rot leuchten."
kind: labor
duration: 1 Doppelstunde
curriculum: "Sek I: Information und Daten (Daten, Information, Nachricht; Sender-Empfänger-Modell; Codierung), Geschichte der Informatik (technische Entwicklung der Informationsübertragung), Leben in und mit vernetzten Systemen (Datenaustausch im Netzwerk); Oberstufe: Computernetze (Protokolle als Vereinbarungen)"
topics: [information, codierung, netze, geschichte]
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Datenübertragung per Funk

<p class='abstract'>
Zu zweit funkt ihr Wörter von einem Raum in den anderen, nur mit ein paar Lampen, die man ein- und ausschalten kann. Dafür denkt ihr euch ein Protokoll aus: wie aus Buchstaben Lichtzeichen werden, woran man Anfang und Ende erkennt und was ihr tut, wenn etwas schiefgeht. Und dann nimmt euch jemand Lampen weg.
</p>

## Das Funkgerät

Euer Funkgerät hat fünf Knöpfe und fünf Lampen. Hält jemand einen Knopf gedrückt, sendet das Gerät, und an jedem Gerät, das auf dieselbe **Frequenz** eingestellt ist, leuchtet die Lampe dazu, so lange, bis der Knopf losgelassen wird. Mehr kann das Funkgerät nicht: keine Sprache, keine Buchstaben, nur Lampen, die an oder aus sind.

So einfach war Funk am Anfang wirklich. Beim Morsen schaltet man einen Sender ein und aus: Solange die Taste gedrückt ist, sendet er auf seiner Frequenz eine gleichmäßige Welle, Funkamateure sagen dazu CW, nach dem englischen *continuous wave*. Aus kurzen und langen Zeichen werden Buchstaben. Euer Gerät sendet auf fünf Kanälen nebeneinander, einem für jede Lampe.

Funk hat eine Eigenschaft, die ein Kabel nicht hat: Alle, die dieselbe Frequenz eingestellt haben, sehen dieselben Lampen, und alle können darauf senden. Haltet einen Knopf mit der Maus oder dem Finger gedrückt, oder nehmt die Tasten <kbd>1</kbd> bis <kbd>5</kbd>.

<div class='cable' data-cable></div>
<script src='/include/cable.js'></script>

Ohne diese Seite und ohne Anmeldung, etwa auf dem Handy, findet ihr das Funkgerät unter **${web_root}/ether**.

<div class='hint try'>
<strong>Ausprobieren:</strong> Öffnet das Funkgerät auf zwei Geräten mit derselben Frequenz und haltet auf dem einen einen Knopf: Auf beiden leuchtet die Lampe. Stellt dann eines 0.1 kHz höher. Was seht ihr jetzt, und was sieht das andere Gerät, wenn ihr dort einen Knopf drückt?
</div>

## Sender und Empfänger

Findet euch in Zweiergruppen zusammen und macht euch mit dem Funkgerät vertraut. Gleich seid ihr in verschiedenen Räumen:

<div class='cable-rooms'>
<div class='cable-room'><strong><i class='bi bi-door-closed'></i>Raum A: die Senderseite</strong>bekommt von der Lehrkraft ein Wort und funkt es mit den Lampen.</div>
<div class='cable-wire'><span>eure Frequenz</span><span class='cable-wire-line'></span><span>sonst nichts</span></div>
<div class='cable-room'><strong><i class='bi bi-door-closed'></i>Raum B: die Empfängerseite</strong>schreibt mit, was ankommt, und bringt das Wort auf ihrem Blatt mit.</div>
</div>

Die Wörter sind unterschiedlich lang und bestehen nur aus den Großbuchstaben A bis Z. Sie ergeben nicht immer einen Sinn, ihr könnt also nichts erraten: Sie müssen trotzdem fehlerfrei ankommen. Wenn ihr während der Übertragung in den anderen Raum laufen müsst, um euch zu besprechen, ist die Übertragung gescheitert.

Jede und jeder von euch bekommt ein Blatt. Die Lehrkraft sagt der Senderseite ihr Wort leise und schreibt es dort unter »Gesendet«; die Empfängerseite schreibt unter »Empfangen«, was ankommt. Erst wenn die Empfängerseite ihr Blatt bei der Lehrkraft abgegeben hat, vergleicht ihr.

## Euer Protokoll

Bevor ihr euch trennt, einigt ihr euch auf ein **Protokoll**: Regeln, an die sich beide Seiten halten, damit die Übertragung klappt. Beachtet dabei:

- Wie werden Buchstaben übertragen?
- Woher soll die Empfängerseite wissen, wann die Übertragung abgeschlossen ist?
- Welche Probleme können während der Übertragung auf der Sender- und auf der Empfängerseite auftreten, und wie wollt ihr darauf reagieren?

Macht euch einen guten Plan, der möglichst alle Fälle abdeckt, und schreibt ihn oben auf euer Blatt: Im anderen Raum könnt ihr nicht mehr nachfragen.

_task(protokoll)

## Weniger Lampen

Nach einer Weile stellt eure Lehrkraft weniger Lampen ein. Über den Lampen zählt dann eine Zeile die Zeit herunter, und die Lampen, die wegfallen, sind gestrichelt umrandet. Ist die Zeit um, sind sie weg, bei allen auf einmal, auch mitten in einer Übertragung.

Passt euer Protokoll an, ohne euch zu treffen: Was ihr dafür braucht, muss schon in eurem Plan stehen oder über die Lampen gehen. Später werden es vielleicht noch weniger.

_task(weniger-lampen)

## Nach der Übertragung

Wenn ihr wieder zusammen seid, legt eure Blätter nebeneinander und sprecht über diese Fragen:

<div class='hint think'>
Wie viele verschiedene Zeichen könnt ihr mit fünf Lampen auf einmal zeigen, wenn jede an oder aus ist? Wie viele mit vier, drei, zwei, einer? Und wie viele Buchstaben gibt es?
</div>

<div class='hint think'>
Wie merkt die Empfängerseite, dass ein Buchstabe zu Ende ist und der nächste beginnt, vor allem wenn zweimal derselbe kommt, wie in SCHNEE?
</div>

<div class='hint think'>
Alle auf eurer Frequenz sehen eure Lampen und können selbst senden. Merkt ihr, ob jemand mithört? Und was passiert mit eurer Nachricht, wenn jemand dazwischenfunkt?
</div>

## Wie es ein Telegraf gemacht hat

Vor 150 Jahren hatten Telegrafen dasselbe Problem: Über eine Leitung gingen nur »Strom an« und »Strom aus«. Émile Baudot baute dafür eine Tastatur mit fünf Tasten, so wie euer Funkgerät fünf Knöpfe hat.

<div class='gallery centered'>
<figure><img src='baudot-keyboard.webp' alt='Eine alte Zeichnung: ein Holzkasten mit aufgeklapptem Deckel, vorn fünf breite weiße Tasten, zwei links und drei rechts mit einer Lücke dazwischen, links ein kleines Gestell mit einer Spule.'><figcaption>Baudots Tastatur mit fünf Tasten, 1884 (Zeichnung von M. Rothen, gemeinfrei, <a href='https://commons.wikimedia.org/wiki/File:Clavier_Baudot.jpg'>Wikimedia Commons</a>)</figcaption></figure>
</div>

_person(baudot)

Wie viel durch eine Leitung oder über eine Frequenz passt, wenn man so geschickt wie möglich codiert, und wie man trotz Störungen fehlerfrei überträgt, hat Claude Shannon 1948 ausgerechnet. Sein Bild davon ist bis heute das **Sender-Empfänger-Modell**: Eine Quelle hat eine Nachricht, ein Sender codiert sie in Signale, ein Kanal trägt sie, und dabei kommen Störungen dazu, ein Empfänger decodiert sie, und am Ende kommt die Nachricht beim Ziel an. Bei euch war die Quelle die Lehrkraft, der Kanal eure Frequenz, und die Störung jeder, der dazwischengefunkt hat.

<div class='book'>
<i class='bi bi-file-earmark-text'></i>
<p><strong>A Mathematical Theory of Communication</strong>, Claude E. Shannon, Bell System Technical Journal, 1948. Der Aufsatz, mit dem die Informationstheorie anfängt: eine Quelle, ein Sender, ein Kanal mit Störungen, ein Empfänger, und wie viele Bits pro Sekunde durch einen Kanal gehen können, ohne dass Fehler bleiben. Die ersten Seiten sind auch ohne die Mathematik dahinter zu lesen: <a href='https://people.math.harvard.edu/~ctm/home/text/others/shannon/entropy/entropy.pdf'>als PDF</a>.</p>
</div>

_person(shannon)
