Lernende entwerfen eigene Backrooms, die endlosen gelben Räume einer Internet-Legende: Sie beschreiben Räume als Grundriss in Textdateien, verbinden sie über Türen zu einem Bauwerk, statten sie aus und gehen dann im Browser hindurch, am Rechner, auf dem Handy, mit Rot-Cyan-Brille oder in VR. Programmiert wird nicht; Vorkenntnisse braucht es keine.

<table class='table'>
<tr><th>Die Reihe</th><td>Level 0 einer Reihe aus vier Seiten; jedes Level baut auf dem vorigen auf, und am Ende jeder Seite führt ein Link ins nächste:<ul>
<li><a href='/level-1-7e3a91c4'>Level 1</a>: Rampen, Treppen, Stockwerke, Kollision</li>
<li><a href='/level-2-b52d0f18'>Level 2</a>: Wasser, Portale</li>
<li><a href='/level-3-4c9e6a2d'>Level 3</a>: Licht, Veröffentlichen, am Ende die beiden fertigen Levels der Vorlage, Lobby und Tiefe, deren Räume man dann ganz lesen kann</li>
</ul>Lernende kommen in jedes Level nur über den Link am Ende des vorigen.</td></tr>
<tr><th>Versteckt</th><td><ul>
<li>Die Seite steht in keinem Katalog und lässt sich nicht anheften (Abschnitt <code>hidden</code> in <code>sections.yaml</code>).</li>
<li>Wer auf der Fehlerseite (jede Adresse, die es nicht gibt) <code>noclip</code> tippt, den Cheat aus Quake, oder auf einem Tablet fünfmal schnell auf die große 404 tippt, fällt hinein; ein Kommentar im Quelltext der Fehlerseite deutet es an. In der Klötzchenwelt führt das Wort »hinunterfällst« (bei den unsichtbaren Wänden unter der Welt) auf eine solche Fehlerseite.</li>
<li>Die Adresse ist absichtlich unauffällig, <code>/level-0-c1d8a2f7</code>, damit man sie nicht errät; <code>/backrooms</code> ist eine gewöhnliche Fehlerseite.</li>
<li>Sie können die Adresse auch weitergeben, etwa als Belohnung oder für eine Vertretungsstunde.</li>
</ul></td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> Vorlage klonen, hineingehen, die Automap, Grundriss und Türen, die Kammer, der Saal mit Satteldach, Säulen und Statue.</li>
<li><b>2. Doppelstunde:</b> andere Wände und Böden mit Holz, Leisten und runden Bögen, der Salon (breite und runde Türen, Lambris, Leisten), ein eigener Raum mit Leuchten und Möbeln, Brillen.</li>
<li><b>Maschinenraum</b> für alle, die wissen wollen, wie es funktioniert: wie das Programm aus Textdateien ein Bauwerk macht (Breitensuche über die Türen, ein Gitter aus Feldern, Flächen an den Kanten) und wie Holz, Fliesen und Marmor ohne Bilder entstehen (prozedurale Texturen, ein Shader in GLSL).</li>
</ul></td></tr>
<tr><th>Rechner</th><td>Das Licht berechnet die Grafikkarte des Rechners, auf dem der Browser läuft, nicht der Workspace. Wie lange das dauert, hängt von ihr ab; schwache Grafik braucht länger. <code>detail</code> und <code>bounces</code> in <code>level.ini</code> machen es schneller oder schöner. Beim Laufen zeichnet das Programm nur die Räume, die man durch Türen und Löcher sieht (Portale, im Maschinenraum erklärt).</td></tr>
<tr><th>Brillen</th><td>Rot-Cyan-Brillen aus Pappe reichen (Taste <kbd>3</kbd>). Eine VR-Brille muss die Adresse der Live-Vorschau öffnen können; dann erscheint »ENTER VR«.</td></tr>
<tr><th>Aufgaben</th><td>Ob die Vorlage geklont ist (<code>backrooms-starter/level.ini</code>), eine Frage zu Türen und ob in <code>rooms/</code> ein eigener Raum liegt (außer den Räumen der Vorlage und denen von den Seiten der Reihe). Ob der eigene Raum über eine Tür angeschlossen ist, prüft die Aufgabe nicht; das sieht man in der Automap.</td></tr>
<tr><th>Stolpersteine</th><td>Ein Raum, zu dem keine Tür führt, wird nicht gelesen und fehlt ohne Meldung. Zwei Räume am selben Ort, Böden ohne Wand daneben und verschieden breite Türen meldet die Vorschau oben links mit Datei und Zeile (auf Englisch, wie die ganze Oberfläche). In <code>[map]</code> gibt es keine Kommentare: Jedes Zeichen zählt.</td></tr>
<tr><th>Modelle</th><td>Athena, ein Sofa, Tische, Stühle, Lampen und weitere Möbel von Poly Haven liegen in der Vorlage im Ordner <code>models</code>. Wie man weitere holt, steht in Level 3.</td></tr>
<tr><th>Hintergrund</th><td>Die Backrooms stammen aus einem Forenbeitrag von 2019 und sind seitdem Stoff für Geschichten, Spiele und Videos, manche davon gruselig. Die Seite übernimmt nur die Stimmung (leere Räume, Summen, Fundnotizen), keine Monster.</td></tr>
</table>
