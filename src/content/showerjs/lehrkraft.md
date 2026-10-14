Diese Aktivität lebt davon, dass die Lernenden etwas vortragen, das sie ohnehin vortragen müssen. Sie taugt deshalb auch für Referate in anderen Fächern.

<table class='table'>
<tr><th>Art</th><td>Projekt, Texte &amp; Präsentationen</td></tr>
<tr><th>Dauer</th><td>2 bis 3 Doppelstunden, dazu die Vorträge</td></tr>
<tr><th>Vorher</th><td>nichts; wer schon einmal ein Repository geklont hat, ist schneller</td></tr>
<tr><th>Anlass</th><td>ein echtes Referat, gern in einem anderen Fach</td></tr>
<tr><th>Technik</th><td>keine; zum Vortrag reicht ein Rechner mit Browser und USB-Anschluss</td></tr>
</table>

**Ablauf**

1. Doppelstunde 1: Vorlage klonen, Live-Vorschau, Titelfolie und erste eigene Folien. Die Folien der Vorlage sind Anschauungsmaterial: lesen, dann löschen.
2. Doppelstunde 2: Bilder und Aussehen. Ein kurzes Gespräch am Beamer lohnt sich: Was passiert, wenn man `bild.png` in `bild.webp` umbenennt? (Nichts: Das Format steht in den ersten Bytes, `xxd bild.webp | head -1` zeigt es.)
3. Doppelstunde 3: Probelauf vom Stick an einem anderen Rechner. Was dabei kaputtgeht, ist der eigentliche Lernmoment: Bilder aus dem Netz, Schriften, die nicht im Ordner liegen.

**Handbuch**

Wie Text, Listen, Bilder und CSS funktionieren, steht im Handbuch (HTML, CSS, Layout, Folien mit Shower) und erscheint auf der Seite als Kasten; die Beispiele zeigen sich hier als Folie. Wer vorher »Statische Webseiten mit HTML & CSS« gemacht hat, sieht die bekannten Abschnitte nur noch als eine Zeile.

**Stolpersteine**

- Ein vergessenes `</li>` oder `>` lässt mehrere Folien auf einmal verschwinden.
- `Fuji.jpg` und `fuji.jpg`: Das geht auf manchen Windows-Rechnern, auf dem Server nicht.
- Bilder mit 5 bis 20 MB direkt vom Handy.
- CSS in `index.html` statt in `styles.css`.
- Google Fonts per Link statt als Datei: geht im Workspace, nicht am Vortragsrechner ohne Netz.
- Eine Regel wie `.wichtig { color: red; }` wirkt auf Absätze, aber nicht auf Folienüberschriften: Das Theme färbt `.slide h2`, und die genauere Regel gewinnt. `h2.wichtig` hilft (Handbuch, CSS, »Wenn eine Regel nicht wirkt«).
- Formeln erscheinen als LaTeX-Code: KaTeX liegt in der Vorlage, ist aber erst eingebunden, wenn die drei Zeilen aus dem Handbuch im `<head>` stehen.

**Bildrechte**

Gute Gelegenheit, über Urheberrecht und Lizenzen zu sprechen. Die Vorlage zeigt unter der Titelfolie, wie eine Bildquelle angegeben wird. Wikimedia Commons nennt Urheber und Lizenz bei jedem Bild.
