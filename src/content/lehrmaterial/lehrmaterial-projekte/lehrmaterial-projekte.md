<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Eigene Projekte begleiten

<p class='abstract'>
Wie ein eigenes Projekt über ein paar Doppelstunden in der Klasse läuft: vier Meilensteine mit einem Datum, Kriterien, die von Anfang an feststehen, eine Selbsteinschätzung vor der Abgabe, ein Vorführen, das für alle gleich abläuft, und eine Gruppe, die ihre Arbeit mit Git und einer Datei <code>TODO.md</code> teilt. Diese Seite sehen nur Lehrkräfte.
</p>

## Wozu der Rahmen

Viele Notizen für Lehrkräfte enden mit »Eigene Projekte«: Ideen, mit denen aus einer Aktivität etwas Eigenes der Lernenden wird, ein Level, eine Szene, eine kleine Anwendung. Ein offenes Projekt scheitert selten am Können, sondern daran, dass es zu groß gedacht ist, erst in der letzten Stunde läuft oder in der Gruppe eine Person alles macht. Dagegen hilft ein fester Rahmen: Meilensteine, an denen du hinsiehst, bevor es zu spät ist, Kriterien, die alle vorher kennen, und eine Selbsteinschätzung, in der die Lernenden ihr Projekt an diesen Kriterien messen.

Der Rahmen kommt aus der Projektmethode, wie sie in der Ausbildung von Lehrkräften gelehrt wird (etwa nach Karl Frey, »Die Projektmethode«): eine Projektskizze, die gemeinsam beschlossen wird, Fixpunkte, an denen die Gruppe innehält, und ein Abschluss, auf den sie zurückblickt. Hier ist er auf Programmierprojekte von drei bis fünf Doppelstunden zugeschnitten.

In den Notizen nennt jede Idee, was an ihren Meilensteinen steht, und ihre Kriterien in einem Satz. Die Kriterien der Idee kommen zu den allgemeinen unten hinzu; sie ersetzen sie nicht.

## Vier Meilensteine

Ein Meilenstein ist ein Termin mit einem Datum, den die Klasse zu Beginn erfährt, und an ihm liegt etwas vor, das du ansehen kannst. Zwischen den Meilensteinen arbeiten die Lernenden frei. Für ein Projekt über vier Doppelstunden passt diese Verteilung:

<table class='table'>
<tr><th>Meilenstein</th><th>Wann</th><th>Was vorliegt</th><th>Worauf du achtest</th></tr>
<tr><td><b>1. Die Idee auf Papier</b></td><td>Ende der 1. Doppelstunde</td><td>Eine halbe Seite: was das Projekt können soll, was von der Aktivität wiederkommt, was darüber hinausgeht, und die kleinste Fassung, die schon als fertig gilt. In einer Gruppe dazu, wer was übernimmt (die erste <code>TODO.md</code>).</td><td>Ist die kleinste Fassung in zwei Doppelstunden zu schaffen? Ist das Projekt zu klein, schlägst du eine Erweiterung vor, ist es zu groß, streicht ihr gemeinsam. Danach ist die Idee abgesprochen; du zeichnest sie ab oder schreibst sie in die Notizen der Gruppe.</td></tr>
<tr><td><b>2. Eine erste Fassung, die läuft</b></td><td>Ende der 2. Doppelstunde</td><td>Die kleinste Fassung läuft, vielleicht hässlich und mit Lücken, und sie ist committet.</td><td>Läuft sie aus dem Repository heraus, nicht nur im Workspace einer Person? Gibt es mehrere Commits oder einen einzigen großen? Wer hier noch nichts zeigen kann, bekommt ein kurzes Gespräch: Woran hängt es, was lässt sich streichen?</td></tr>
<tr><td><b>3. Eine Fassung, die gezeigt wird</b></td><td>Mitte der 3. Doppelstunde</td><td>Das Projekt wird anderen gezeigt: Jemand aus der Klasse spielt oder benutzt es, die Gruppe schreibt mit, wo es hakt.</td><td>Was funktioniert vor anderen, was nur in der Hand der Gruppe? Was schreibt die Gruppe auf, und was nimmt sie sich für die letzte Doppelstunde vor?</td></tr>
<tr><td><b>4. Die Abgabe</b></td><td>Ende der 4. Doppelstunde</td><td>Der Code im Repository, eine kurze <code>README.md</code> (was es ist, wie man es startet) und darin die Selbsteinschätzung. Danach das Vorführen.</td><td>Passt die Selbsteinschätzung zu dem, was du siehst? Wo nicht, ist das die erste Frage beim Vorführen.</td></tr>
</table>

Ein verpasster Meilenstein ist kein Urteil, sondern ein Anlass zum Gespräch. Meist ist das Projekt zu groß; dann wird die kleinste Fassung kleiner, und das Kriterium »Was es kann« misst sich an der neuen Absprache. Halte fest, was ihr geändert habt, damit es bei der Abgabe noch gilt.

## Kriterien, die von Anfang an feststehen

Die Kriterien bekommen die Lernenden zusammen mit dem Thema, nicht erst bei der Abgabe. Sie sind zweigeteilt: das Produkt, also was am Ende läuft, und der Weg dorthin. Die Stufen sind in Worten beschrieben, damit die Lernenden sich selbst einordnen können; was eine Stufe als Note bedeutet, entscheidest du.

<table class='table'>
<tr><th>Kriterium</th><th>Noch nicht</th><th>Teilweise</th><th>Erfüllt</th><th>Darüber hinaus</th></tr>
<tr><th colspan='5'>Produkt</th></tr>
<tr><td><b>Es läuft</b></td><td>Startet nicht oder bricht gleich ab.</td><td>Läuft, bricht aber bei normaler Bedienung ab.</td><td>Läuft bei normaler Bedienung ohne Absturz.</td><td>Fängt auch falsche Eingaben und Sonderfälle ab.</td></tr>
<tr><td><b>Was es kann</b></td><td>Die kleinste Fassung ist nicht erreicht.</td><td>Ein Teil der kleinsten Fassung geht.</td><td>Die abgesprochene kleinste Fassung geht vollständig.</td><td>Dazu kommt eine Erweiterung, die geplant oder unterwegs dazugekommen ist.</td></tr>
<tr><td><b>Über die Seite hinaus</b></td><td>Kaum etwas, was nicht schon auf der Seite stand.</td><td>Die Seite mit anderen Werten oder Inhalten.</td><td>Etwas Neues, das die Seite nicht zeigt, gebaut auf dem, was sie zeigt.</td><td>Etwas Neues, für das die Person selbst nachgelesen oder sich etwas ausgedacht hat.</td></tr>
<tr><td><b>Den eigenen Code erklären</b></td><td>Kann nicht sagen, was eine Stelle tut.</td><td>Sagt, was eine Stelle tut, aber nicht, warum.</td><td>Erklärt eine Stelle, die du auswählst, und warum sie so ist.</td><td>Sagt auch, was passiert, wenn man sie ändert, und welche andere Lösung es gegeben hätte.</td></tr>
<tr><th colspan='5'>Weg</th></tr>
<tr><td><b>Schritte sichtbar</b></td><td>Ein Commit am Ende oder keiner.</td><td>Wenige große Commits ohne sprechende Nachricht.</td><td>Ein Commit je Schritt, der funktioniert, mit einer Nachricht, die sagt, was er tut.</td><td>Dazu Branches für größere Teile, die erst in <code>main</code> kommen, wenn sie laufen.</td></tr>
<tr><td><b>Notizen</b></td><td>Keine.</td><td>Eine <code>README.md</code>, die nur den Namen nennt.</td><td><code>README.md</code> mit Start und Bedienung; die Änderungen an der Absprache sind festgehalten.</td><td>Dazu, was schiefging und wie es gelöst wurde.</td></tr>
<tr><td><b>Arbeit in der Gruppe</b> (nur für Gruppen)</td><td>Eine Person hat fast alles gemacht.</td><td>Aufgaben verteilt, aber nicht in <code>TODO.md</code> oder nicht eingehalten.</td><td>Jede Aufgabe in <code>TODO.md</code> hat einen Namen; jede Person hat einen Teil, den sie erklärt.</td><td>Die Gruppe hat Arbeit anderer gelesen und verbessert (Review) und Konflikte selbst gelöst.</td></tr>
</table>

Die Kriterien der Idee aus der Notiz kommen als eigene Zeile dazu, etwa »Das Level lässt sich ohne Hilfe zu Ende spielen« beim Raycaster. Lass die Lernenden beim ersten Meilenstein eine Zeile für ihr eigenes Projekt ergänzen: Was muss man sehen, damit es fertig ist?

## Die Selbsteinschätzung vor der Abgabe

Bevor sie abgeben, gehen die Lernenden die Kriterien einzeln durch und schreiben, wo ihr Projekt steht und warum. Das kommt als letzter Abschnitt in die `README.md`, in der Gruppe einmal je Person. Wer sich einordnen muss, sieht selbst, was fehlt, und kann es oft noch beheben; und du hast beim Vorführen eine erste Frage.

Diese Satzanfänge helfen beim Schreiben:

- »Mein Projekt kann …«
- »Über die Seite hinaus geht …«
- »Noch nicht geschafft habe ich …, weil …«
- »Beim Kriterium … sehe ich mein Projekt bei ›…‹, weil …«
- »Am längsten gebraucht hat …; gelöst habe ich es, indem …«
- »In unserer Gruppe habe ich … übernommen; das sieht man an den Commits …«
- »Gegenüber der Absprache vom ersten Meilenstein haben wir … geändert, weil …«
- »Mit einer weiteren Doppelstunde würde ich als Nächstes …«

Eine Selbsteinschätzung, die sich ehrlich schlechter einordnet, als das Projekt ist, oder Lücken selbst benennt, zeigt Überblick; das darf in die Bewertung des Weges eingehen. Wo deine Einschätzung und die der Lernenden auseinandergehen, sprecht ihr darüber, bevor du entscheidest.

## Vorführen und Spieltest

Damit ein Vorführen fair zu bewerten ist, läuft es für alle gleich ab, und die Reihenfolge steht vorher fest. Etwa fünf Minuten je Projekt:

1. **Ein Satz, was es ist.** Mehr Einleitung braucht es nicht.
2. **Der Spieltest:** Jemand aus der Klasse, der das Projekt nicht kennt, spielt oder benutzt es zwei Minuten lang, ohne Erklärung. Die Gruppe schweigt und schreibt mit, wo die Person hängen bleibt. Was ohne Erklärung geht, zählt bei »Es läuft« und »Was es kann«.
3. **Das Neue:** Die Gruppe zeigt, was über die Seite hinausgeht.
4. **Eine Stelle im Code:** Du wählst eine Stelle aus, nicht die Gruppe, und fragst, was sie tut und was passiert, wenn man sie ändert. In einer Gruppe antwortet die Person, deren Teil es ist.

Vorgeführt wird der Stand der Abgabe, also der letzte Commit vor dem Termin, nicht was danach im Workspace geändert wurde. Bricht das Programm beim Vorführen ab, zeigt die Gruppe den letzten Commit, der lief. Schreib deine Einordnung im Raster gleich danach auf, solange du das Vorführen noch vor Augen hast. Die Klasse kann auf einem Zettel festhalten, was ohne Erklärung ging und wo jemand hing; das ist eine Rückmeldung an die Gruppe, keine Bewertung.

Wo kein Spieltest passt (ein Bild, eine Messung, eine Tabelle), tritt an seine Stelle die Probe aus der Notiz der Idee: Jemand aus der Klasse wählt eine Eingabe, ein Bild oder eine Frage, und das Projekt muss damit zurechtkommen.

## Die Arbeit in der Gruppe teilen

Eine Gruppe von zwei oder drei Personen arbeitet in einem gemeinsamen Repository. Unter [Git-Repositorys](/repos) legst du mit **Neues Repository** eines für jede Gruppe an, auch ohne Aktivität (mehr in [Git-Repositorys der Klasse](/lehrmaterial-git)); wie man klont, pusht und einen Konflikt löst, lernt die Klasse in [Im Team mit Git: das Klassenquiz](/class-quiz).

Die Arbeit verteilt die Gruppe wie im Klassenquiz mit einer Datei `TODO.md`:

```markdown
# Was am Projekt noch fehlt

## Offen

- Der Ausgang beendet das Level. Wer: Ida
- Ein Schalter öffnet eine Wand. Wer:

## Fertig

- Die eigene Karte mit Gängen. Wer: Lena
```

Wer eine Aufgabe übernimmt, schreibt den eigenen Namen hinter »Wer:«, committet und pusht, bevor die Arbeit beginnt; ist sie fertig, kommt die Zeile unter »Fertig«. Jede Aufgabe entsteht in einem eigenen Branch und kommt erst in `main`, wenn sie läuft. Beim ersten Meilenstein ist `TODO.md` der Plan der Gruppe, beim zweiten siehst du darin, wo sie steht.

Mit **Ansehen** neben dem Repository siehst du den Verlauf, jeden Commit mit seinen Änderungen und wer ihn geschrieben hat, ohne selbst zu klonen. Die Zahl der Commits ist dabei kein Maß: Wer zu zweit an einer Tastatur arbeitet, schreibt beide Namen in die Nachricht, und eine schwierige Zeile kann mehr Arbeit sein als zwanzig leichte. Ob die Arbeit geteilt war, zeigt sich am sichersten beim Vorführen, wenn jede Person ihren Teil erklärt.
