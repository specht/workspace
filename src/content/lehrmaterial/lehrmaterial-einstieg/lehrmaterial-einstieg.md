<div class='meta'>
image: klasse-fortschritt.webp
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Einstieg für Lehrkräfte

<p class='abstract'>
Was du für die erste Stunde mit dem Workspace brauchst: wie deine Klasse sich anmeldet, was die Klassenseite zeigt, wie du Abgaben ansiehst und wie du alles vorher selbst ausprobierst. Diese Seite sehen nur Lehrkräfte.
</p>

## Was die Lernenden haben

Jede Schülerin und jeder Schüler hat einen eigenen **Workspace**: Visual Studio Code im Browser, mit einem eigenen Ordner, der erhalten bleibt. Auf der Startseite stehen **Aktivitäten**, also Anleitungen zu einem Thema (eine Webseite bauen, eine interaktive Geschichte schreiben, eine Datenbank abfragen). In vielen Aktivitäten stehen **Aufgaben**, die der Workspace selbst prüft: Er sieht in den Dateien nach, führt eine Abfrage aus oder vergleicht eine Antwort. Im **Handbuch** schlagen die Lernenden nach, was sie in mehreren Aktivitäten brauchen.

Du siehst dieselben Seiten wie deine Klasse, mit ein paar Zusätzen: dem Menüpunkt **Klassen** (deine Gruppen, ihr Fortschritt und wer darin ist), auf manchen Aktivitäten dem Knopf **Notiz für Lehrkräfte** und unter den Schritten dem Knopf **Code bis hier** (mehr dazu unten).

## Die erste Stunde: anmelden

Wer sich anmeldet, gibt die eigene E-Mail-Adresse ein und bekommt einen Code. Die ganze Adresse ist dabei nicht nötig: Es reicht ihr Anfang, solange nur eine eingeladene Adresse so beginnt. Meist genügt der Teil vor dem `@`, also `lea.fischer` statt `lea.fischer@schule.de`.

In der Schule kommen viele nicht an ihr Postfach. Dafür gibt es die Klassenseite:

1. Öffne **Klassen** und wähle oben deine Gruppe.
2. Klick auf **Anmelde-Codes zeigen** und zeig die Seite am Beamer.
3. Lass die Klasse die Anmeldung starten. Jeder Name erscheint mit seinem Code, sobald er angefordert ist; Mail-Adressen stehen dort nicht.

<img class='full full-shadow' src='codes-beamer.webp' alt='Die Anmelde-Codes am Beamer: Namen und Codes in großer Schrift.'>

Ein Code gilt einmal und nur kurze Zeit. Wer sich vertippt, fordert einfach einen neuen an. Wartet jemand auf einen Code, siehst du ihn auch ohne Beamer-Ansicht oben auf der Klassenseite.

## Gruppen und Einladungen

Wer sich anmelden kann, steht in einer Gruppe. Unter **Klassen** legst du mit **Neue Gruppe** deine Gruppen an; im Reiter **Lernende** trägst du die E-Mail-Adressen ein (eine pro Zeile, mit oder ohne Namen, auch aus einer Tabelle kopiert) und schickst eine **Willkommensmail**: Sie sagt, wie die Anmeldung geht, und Antworten darauf kommen bei dir an. Namen sind freiwillig und können auch später kommen; ohne Namen zeigt die Klassenseite die Adresse. Eine Kollegin oder einen Kollegen deiner Schule nimmst du mit **Lehrkraft dazu** in eine Gruppe.

Welcher Schule du angehörst, trägt die Administration ein. Hat deine Schule dem Einsatz noch nicht zugestimmt, vermerkst du im Reiter **Lernende**, für wen ein schriftliches Einverständnis vorliegt; anmelden können sich alle schon jetzt (mehr dazu unten).

<div class='hint'>
Steht auf der Klassenseite, dass die Einladungen noch in Dateien stehen, pflegt sie die Administration: Schreib an <a href='mailto:${contact_email}'>${contact_email}</a>, für welche Gruppe, und schick die E-Mail-Adressen mit.
</div>

## Die Klassenseite

Unter **Klassen** wählst du oben die Gruppe. Darunter liegen drei Reiter.

### Fortschritt

Hier wählst du eine Aktivität und siehst, wer welche Aufgabe gelöst hat. Jede Spalte ist eine Aufgabe, ihr Titel steht darüber.

<img class='full full-shadow' src='klasse-fortschritt.webp' alt='Der Reiter Fortschritt: eine Tabelle mit Namen und Aufgaben, Haken für gelöste Aufgaben.'>

<div style='max-width: 100%; overflow-x: auto;'>
<table class='table'>
<thead><tr><th>Zeichen</th><th>Bedeutung</th></tr></thead>
<tbody>
<tr><td>Haken</td><td>gelöst</td></tr>
<tr><td>Papierflieger</td><td>abgegeben, oder eine Antwort in eigenen Worten gespeichert</td></tr>
<tr><td>Kreuz mit Zahl</td><td>noch nicht gelöst, nach so vielen Versuchen; rot ab drei Versuchen</td></tr>
<tr><td>Kreis-Pfeile</td><td>Die Aufgabe hat sich geändert, die Antwort ist von vorher.</td></tr>
<tr><td>Strich</td><td>noch nicht bearbeitet</td></tr>
</tbody>
</table>
</div>

Die Zeile **Klasse** darunter zählt, wie viele fertig sind und wie viele hängen. Eine gelbe Spalte heißt: Viele liegen bei dieser Aufgabe gerade falsch. Das ist oft ein guter Moment, kurz für alle zu erklären.

Darüber stehen zwei Werkzeuge:

- **Jetzt nachsehen** prüft eine Aufgabe sofort für alle, ohne dass jemand »Prüfen« drücken muss, zum Beispiel ob alle ihre Seite angelegt haben. Das Ergebnis erscheint blau umrandet in der Tabelle; gespeichert wird nichts.
- **Vorlage aktuell?** gibt es bei Aktivitäten, die mit einer Vorlage beginnen. Es sagt, wer noch eine alte Version hat und `git pull` ausführen sollte.

Unter der Tabelle stehen die Antworten in eigenen Worten und die **Abgaben**. Ein Klick auf eine Abgabe öffnet sie über der Klassenseite: links die Dateien, rechts der Quelltext. Bei Webseiten und interaktiven Geschichten startet **Starten** die Abgabe so, wie die Person sie abgegeben hat, mit allem, was sich darin bewegt.

<img class='full full-shadow' src='abgabe-ansehen.webp' alt='Eine abgegebene Webseite läuft im Fenster über der Klassenseite; links die Dateien.'>

Eine Abgabe ist eine Kopie. Was die Person danach im Workspace ändert, ändert die Abgabe nicht; sie kann aber jederzeit neu abgeben.

Für eine neue Runde, etwa im nächsten Halbjahr, löscht **Alle Abgaben löschen** unter der Liste die Abgaben dieser Aufgabe in dieser Gruppe. Vorher fragt der Workspace nach; lade herunter, was du behalten willst.

Die Klassenseite merkt sich, welche Gruppe und welche Aktivität du zuletzt angesehen hast, und öffnet beim nächsten Mal dort.

### Lernende

Hier steht jede Person der Gruppe mit ihrem Workspace: ob er läuft, wie viel Rechenzeit und Speicher er gerade braucht. Eine Zeile leuchtet gelb, solange ihr Workspace arbeitet. Darüber zeigt eine Zeile, wie ausgelastet der Server ist. So erkennst du, ob ein Workspace hängt oder ob gerade alle gleichzeitig etwas Großes starten.

Im selben Reiter verwaltest du die Gruppe: Namen und Adressen, Willkommensmails, das Einverständnis, ein Enddatum, weitere Lehrkräfte, Umbenennen und Löschen; unten nimmst du neue Personen auf.

<img class='full full-shadow' src='klasse-lernende.webp' alt='Der Reiter Lernende: oben die Auslastung des Servers, darunter die Lernenden mit ihrem Workspace und den Angaben der Gruppe.'>

Bei jeder Person stehen zwei Knöpfe:

- **Workspace öffnen** öffnet ihren Workspace in einem neuen Tab, etwa um bei einem Fehler zu helfen.
- **Nachahmen** zeigt dir die Seite so, wie die Person sie sieht, mit ihren Aufgaben und Abgaben. Oben steht dann »Zurück zu deinem Konto«.

Bevor du einen Workspace öffnest oder jemanden nachahmst, schreibst du einen Satz dazu, warum, zum Beispiel »Ich sehe mir deine Abgabe zu Aufgabe 3 an.« Die Person liest ihn bei der nächsten Anmeldung. So bleibt nachvollziehbar, wer wann hineingesehen hat.

### Startseite

Was du hier für die Gruppe **anheftest**, steht bei allen in der Gruppe oben auf der Startseite. So findet die Klasse die Aktivität der Stunde sofort.

## Selbst ausprobieren

Für dich gibt es ein **Testkonto**, genau eins: ein Konto wie das einer Schülerin, nur dass es dir gehört. Es steht allein in einer eigenen Gruppe, **Testgruppe (dein Name)**, ganz unten in der Gruppenauswahl der Klassenseite. Diese Gruppe siehst nur du, auch andere Lehrkräfte und Admins sehen sie nicht. Was du mit dem Testkonto tust, landet bei keiner Klasse.

### So kommst du hinein

- **Nachahmen:** Wähl auf der Klassenseite deine Testgruppe, geh auf **Lernende** und klick bei deinem Testkonto auf **Nachahmen**; einen Satz braucht es dafür nicht. Jetzt siehst du die Website genau so, wie eine Schülerin sie sieht. Mit »Zurück zu deinem Konto« in der Leiste oben bist du wieder du selbst. **Workspace öffnen** startet daneben den Workspace des Testkontos, ohne dass du es nachahmst.
- **Direkt anmelden:** Das Testkonto hat deine Adresse mit `+test` vor dem @, aus `name@schule.de` wird `name+test@schule.de`. Den Anmelde-Code dafür schickt der Workspace an deine eigene Adresse, ohne `+test`. Meld dich damit in einem privaten Fenster oder einem zweiten Browser an, dann hast du beides nebeneinander: links die Klassenseite als Lehrkraft, rechts das Testkonto als Schülerin.

### Was du damit ausprobieren kannst

- **Eine Aktivität durcharbeiten,** Aufgabe für Aufgabe: Jede Antwort erscheint im Reiter **Fortschritt** deiner Testgruppe, so wie später die deiner Klasse, mit Haken, Versuchen und »hängt«.
- **Abgaben und Galerie:** Gib als Testkonto eine Webseite oder eine Szene ab und sieh sie dir als Lehrkraft unter **Fortschritt** an, so wie du später die Abgaben der Klasse öffnest.
- **Offene Antworten:** Schreib als Testkonto eine Reflexion. Unter derselben Aufgabe zeigt dir der Kasten »Für Lehrkräfte« mit **Antworten aus Testgruppe** genau, was du später von der Klasse siehst.
- **Den Workspace:** Alles, was Lernende im Workspace tun, auch das, was dort schiefgehen kann, ohne dass ein echter Workspace einer Schülerin leidet.
- **Leistungsüberprüfungen:** Lade im Profil unter **Leistungsüberprüfungen** eine hoch, klick auf **0 Personen** und wähl dort dein Testkonto aus. Starte sie, mach sie als Testkonto mit, beende sie und sieh dir die **Ergebnisse** an. So weißt du vorher, was die Klasse sieht und was am Ende bei dir ankommt.

### Zurücksetzen

Willst du noch einmal von vorn anfangen, klick im Reiter **Lernende** deiner Testgruppe auf **Testkonto zurücksetzen**. Das leert den Workspace des Testkontos und seine Workspaces in Leistungsüberprüfungen, legt seine Datenbanken neu an und löscht seine Antworten, Versuche und Abgaben. Einer Leistungsüberprüfung bleibt es zugewiesen. Alle anderen Konten bleiben, wie sie sind; zurücksetzen kannst du also jederzeit, auch mitten im Schuljahr.

Löst du in deinem eigenen Konto eine Aufgabe, steht sie auf der Klassenseite in einer eigenen Zeile **Du (Lehrkraft)** unter der Klasse. Für einen kurzen Test reicht das oft schon; für alles, was wie bei einer Schülerin aussehen soll, nimm das Testkonto.

## Auf einer Aktivitätsseite

Oben rechts steht bei manchen Aktivitäten der Knopf **Notiz für Lehrkräfte**. Er öffnet Hinweise nur für dich: wie lange die Teile dauern, was die Aufgaben prüfen, wo Lernende meistens hängen bleiben. Geschlossen stört er nicht, auch nicht am Beamer.

Baut eine Aktivität ein Programm Schritt für Schritt auf, steht unter vielen Bildern rechts ein kleiner Knopf **Code bis hier**. Er kopiert das ganze Programm, wie es an dieser Stelle aussieht, damit du ein Ergebnis ausprobieren kannst, ohne alle Schritte davor zu gehen. Deine Klasse sieht ihn nicht.

Steht an einer Aufgabe **Vorschau für Lehrkräfte**, sind Aufgaben auf eurer Installation für Lernende noch ausgeschaltet; du siehst sie trotzdem schon.

## Was der Server speichert

- Für jede Aufgabe die letzte Antwort, ob sie stimmt und wie viele Versuche es waren. Lernende können ihre Antwort selbst löschen.
- Abgaben als Kopie, bis die Person sie löscht oder neu abgibt.
- Wer wann einen Workspace geöffnet oder jemanden nachgeahmt hat, mit deinem Satz dazu.

Noten speichert der Workspace nicht. Die Haken auf der Klassenseite sagen, ob eine Aufgabe gelöst ist, nicht wie gut.

Im Profil unter **Meine Daten** lädt jede Person alles, was zu ihrem Konto gespeichert ist, als ZIP herunter und löscht ihr Konto selbst. Spätestens zum Ende des Schuljahres, in dem ein Kurs endet, werden die Konten gelöscht; sag der Administration, welche Gruppen enden und wer die Schule verlässt. Wer die Schule verlässt und das Konto behalten möchte, kann es weiterführen. Alles Weitere steht in der [Datenschutzerklärung](/impressum#datenschutz).

## Wenn deine Schule zustimmen soll

Unter [Datenschutz: Unterlagen für die Schule](/datenschutz) liegt alles, was eine Schulleitung braucht, um den Workspace im Unterricht einzusetzen: eine Seite mit der Entscheidung zum Unterschreiben, eine Prüfübersicht für die Datenschutzbeauftragte Stelle, der Eintrag für das Verzeichnis von Verarbeitungstätigkeiten, ein Elternbrief und eine Nutzungsordnung, jeweils zum Drucken.

Unterrichtest du an einer anderen Schule als der, die den Workspace betreibt, gilt das auch für dich. Unter **Gruppen** führt **Unterlagen für deine Schule** zu denselben Unterlagen mit deiner Schule als Verantwortlicher: Sie nennen ihre Anschrift und ihre Datenschutzbeauftragte Stelle (das trägt die Administration ein; sag ihr, was fehlt), dich als Ansprechperson, und der Elternbrief trägt deinen Namen. Die Schulleitung unterschreibt die Entscheidung und die Vereinbarung zur Auftragsverarbeitung; schick beides an die Administration, die es einträgt. Damit ist für deine ganze Schule alles geklärt.

Bis dahin genügt für eine Schülerin oder einen Schüler ein schriftliches Einverständnis: Unter den Unterlagen gibt es den **Elternbrief mit Einverständniserklärung**. Du bewahrst die unterschriebenen Zettel auf und trägst unter **Gruppen** bei jeder Person das Datum ein; dafür bist du verantwortlich. Gesperrt wird niemand, solange das noch offen ist; die Gruppe zeigt aber, für wie viele es fehlt.

Leistungsüberprüfungen bereitest du im **Profil** unter **Für Lehrkräfte** vor.
