<div class='meta'>
kind: reference
search: Deceptive Patterns, Cookie-Banner, Einwilligung, Nudging, Kündigungsbutton
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Dark Patterns

<p class='abstract'>
Eine Gestaltung, die Menschen zu etwas bringt, das sie so nicht wollten: der große Knopf für »Alle akzeptieren« neben einem kleinen Link, das Abo, das sich mit zwei Klicks abschließen und nur per Brief kündigen lässt. Hier stehen die häufigsten Muster, was das Gesetz dazu sagt und wie eine faire Abfrage aussieht.
</p>

## Was ein Dark Pattern ist

2010 gab der UX-Designer Harry Brignull solchen Gestaltungen einen Namen:
**Dark Patterns**. Auf seiner Website, heute
[deceptive.design](https://www.deceptive.design/), sammelt er seitdem Beispiele;
dort heißen sie inzwischen **Deceptive Patterns**, täuschende Muster.

Ein Dark Pattern ist keine Panne. Die Gestaltung ist so gewählt, dass mehr
Menschen tun, was der Anbieter will: zustimmen, kaufen, bleiben. Oft wirkt sie
nur zusammen mit anderen: Eine Farbe allein drängt kaum, eine Farbe, ein
versteckter Link und eine Frage bei jedem Besuch zusammen schon.

<!-- part: dark-pattern-arten -->
<!-- short: Versteckte Ablehnung, vorausgewählte Häkchen, schwer zu kündigen, Beschämen, Nörgeln, falsche Knappheit und Eile, versteckte Kosten, irreführende Wörter, getarnte Werbung. -->
## Die häufigsten Muster

Die englischen Namen sind die von [deceptive.design](https://www.deceptive.design/types).

<div class='scroll-x'><table class='table table-sm reference-table'>
<thead><tr><th>Muster</th><th>So sieht es aus</th></tr></thead>
<tbody>
<tr><td>Versteckte Ablehnung<br><small>Obstruction, Visual interference</small></td><td>»Alle akzeptieren« ist ein großer farbiger Knopf; ablehnen kann man erst hinter »Einstellungen«, auf einer zweiten Seite.</td></tr>
<tr><td>Vorausgewählt<br><small>Preselection</small></td><td>Die Häkchen für den Newsletter oder für die Cookies von Partnern sind schon gesetzt.</td></tr>
<tr><td>Schwer zu kündigen<br><small>Hard to cancel</small></td><td>Abschließen geht mit zwei Klicks, kündigen nur per Brief, am Telefon oder nach fünf Fragen, warum man gehen will.</td></tr>
<tr><td>Beschämen<br><small>Confirmshaming</small></td><td>Der Knopf zum Ablehnen heißt »Nein danke, ich spare nicht gern Geld«.</td></tr>
<tr><td>Nörgeln<br><small>Nagging</small></td><td>Dieselbe Frage kommt bei jedem Besuch wieder, bis man zustimmt.</td></tr>
<tr><td>Falsche Knappheit<br><small>Fake scarcity</small></td><td>»Nur noch 2 Zimmer frei!«, auch wenn es nicht stimmt.</td></tr>
<tr><td>Falsche Eile<br><small>Fake urgency</small></td><td>Ein Countdown für ein Angebot, der nach dem Neuladen wieder von vorn beginnt.</td></tr>
<tr><td>Versteckte Kosten<br><small>Hidden costs</small></td><td>Der Preis wächst erst an der Kasse, um Gebühren, die vorher nirgends standen.</td></tr>
<tr><td>Irreführende Wörter<br><small>Trick wording</small></td><td>»Entfernen Sie das Häkchen nicht, wenn Sie keine Angebote nicht verpassen möchten.«</td></tr>
<tr><td>Getarnte Werbung<br><small>Disguised ads</small></td><td>Eine Anzeige sieht aus wie der Knopf »Download« der Seite.</td></tr>
</tbody>
</table>
</div>
<!-- /part -->

<!-- part: dark-pattern-gesetz -->
<!-- short: Einwilligung nach DSGVO, die Sicht der deutschen Aufsichtsbehörden auf Cookie-Banner, Art. 25 Digital Services Act, der Kündigungsbutton nach § 312k BGB. -->
## Was das Gesetz sagt

- **Einwilligung:** Nach der DSGVO muss eine Einwilligung freiwillig,
  informiert und unmissverständlich sein. Ein Häkchen, das schon gesetzt ist,
  ist keine Einwilligung (Europäischer Gerichtshof, Planet49, 2019), und
  zurücknehmen muss man sie so leicht, wie man sie gegeben hat (Art. 7 Abs. 3
  DSGVO).
- **Cookie-Banner:** Die deutschen Aufsichtsbehörden für den Datenschutz
  verlangen, dass man ohne Mehraufwand an Klicks ablehnen kann. Neben
  »Einwilligung erteilen« gehört ein Knopf, der in Größe, Farbe, Kontrast und
  Schrift vergleichbar ist; ein Knopf »Einstellungen« allein reicht nicht. Eine
  andere Farbe allein macht eine Einwilligung noch nicht unwirksam: Es kommt
  darauf an, ob man die Ablehnung auf einen Blick als Wahl erkennt.
- **Online-Plattformen:** Seit Februar 2024 verbietet der Digital Services Act
  in Artikel 25 Plattformen, ihre Oberfläche so zu gestalten, dass sie
  täuscht oder manipuliert oder freie Entscheidungen erschwert. Was schon die
  DSGVO regelt, etwa die Einwilligung in Cookies, fällt nicht darunter.
- **Kündigen:** Wer im Internet einen Vertrag mit laufenden Kosten anbietet,
  etwa ein Abo, braucht seit Juli 2022 einen Knopf »Verträge hier kündigen«,
  der ständig verfügbar und leicht zu erreichen ist (§ 312k BGB).
<!-- /part -->

<!-- part: faire-abfrage -->
<!-- short: Gleich große Knöpfe auf der ersten Ebene, nichts vorausgewählt, klare Wörter, ein Nein gilt; am einfachsten gar kein Banner. -->
## Eine faire Abfrage

- Ja und Nein stehen nebeneinander, gleich groß und gleich gut zu sehen, auf
  der ersten Seite des Dialogs.
- Nichts ist vorausgewählt.
- Die Knöpfe sagen, was passiert: »Ablehnen«, nicht »Später« oder »Nein
  danke, ich mag keine Vorteile«.
- Ein Nein gilt: Die Frage kommt nicht bei jedem Besuch wieder.

```html
<div class="consent">
  <p>Dürfen wir mit einem Cookie zählen, welche Seiten du liest?</p>
  <button class="choice">Ablehnen</button>
  <button class="choice">Zustimmen</button>
</div>
```

Beide Knöpfe haben dieselbe Klasse und sehen damit gleich aus.

Am einfachsten ist gar kein Banner: Eine Website, die im Browser nur ablegt,
was für ihren Dienst unbedingt nötig ist, etwa die Anmeldung, muss nach § 25
TDDDG nicht fragen.
<!-- /part -->

## Zum Nachschlagen

- [deceptive.design: Types](https://www.deceptive.design/types): alle Muster
  mit Beispielen, gesammelt von Harry Brignull
- [Orientierungshilfe der Aufsichtsbehörden für Anbieter:innen von digitalen Diensten](https://www.datenschutzkonferenz-online.de/media/oh/OH_Digitale_Dienste.pdf),
  Datenschutzkonferenz, November 2024: wie ein Einwilligungsbanner aussehen
  muss (Randnummern 48 bis 60 und 127 bis 136)
- [Guidelines 03/2022 on deceptive design patterns](https://www.edpb.europa.eu/our-work-tools/our-documents/guidelines/guidelines-032022-deceptive-design-patterns-social-media_en),
  Europäischer Datenschutzausschuss: Dark Patterns in sozialen Netzwerken
- [Digital Services Act](https://eur-lex.europa.eu/eli/reg/2022/2065/oj?locale=de),
  Artikel 25: Gestaltung und Organisation der Online-Schnittstelle
- [§ 312k BGB](https://www.gesetze-im-internet.de/bgb/__312k.html): der
  Kündigungsbutton
