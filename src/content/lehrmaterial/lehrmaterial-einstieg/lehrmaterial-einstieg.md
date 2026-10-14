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

Du siehst dieselben Seiten wie deine Klasse, mit zwei Zusätzen: dem Menüpunkt **Klassen** und auf manchen Aktivitäten dem Knopf **Notiz für Lehrkräfte** (mehr dazu unten).

## Die erste Stunde: anmelden

Wer sich anmeldet, gibt die eigene E-Mail-Adresse ein und bekommt einen Code. Die ganze Adresse ist dabei nicht nötig: Es reicht ihr Anfang, solange nur eine eingeladene Adresse so beginnt. Meist genügt der Teil vor dem `@`, also `lea.fischer` statt `lea.fischer@schule.de`.

In der Schule kommen viele nicht an ihr Postfach. Dafür gibt es die Klassenseite:

1. Öffne **Klassen** und wähle oben deine Gruppe.
2. Klick auf **Anmelde-Codes zeigen** und zeig die Seite am Beamer.
3. Lass die Klasse die Anmeldung starten. Jeder Name erscheint mit seinem Code, sobald er angefordert ist; Mail-Adressen stehen dort nicht.

<img class='full full-shadow' src='codes-beamer.webp' alt='Die Anmelde-Codes am Beamer: Namen und Codes in großer Schrift.'>

Ein Code gilt einmal und nur kurze Zeit. Wer sich vertippt, fordert einfach einen neuen an. Wartet jemand auf einen Code, siehst du ihn auch ohne Beamer-Ansicht oben auf der Klassenseite.

<div class='hint'>
Fehlt jemand aus deiner Klasse oder fehlt dir eine Gruppe, steht die Person nicht in den Einladungen. Die pflegt die Administration deiner Schule.
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

Hier steht jede Person mit ihrem Workspace: ob er läuft, wie viel Rechenzeit und Speicher er gerade braucht. Eine Zeile leuchtet gelb, solange ihr Workspace arbeitet. Darüber zeigen zwei Karten, wie ausgelastet der Server ist. So erkennst du, ob ein Workspace hängt oder ob gerade alle gleichzeitig etwas Großes starten.

<img class='full full-shadow' src='klasse-lernende.webp' alt='Der Reiter Lernende: Karten zu CPU und Speicher des Servers, darunter die Lernenden mit ihrem Workspace.'>

Bei jeder Person stehen zwei Knöpfe:

- **Workspace öffnen** öffnet ihren Workspace in einem neuen Tab, etwa um bei einem Fehler zu helfen.
- **Nachahmen** zeigt dir die Seite so, wie die Person sie sieht, mit ihren Aufgaben und Abgaben. Oben steht dann »Zurück zu deinem Konto«.

Bevor du einen Workspace öffnest oder jemanden nachahmst, schreibst du einen Satz dazu, warum, zum Beispiel »Ich sehe mir deine Abgabe zu Aufgabe 3 an.« Die Person liest ihn bei der nächsten Anmeldung. So bleibt nachvollziehbar, wer wann hineingesehen hat.

### Startseite

Was du hier für die Gruppe **anheftest**, steht bei allen in der Gruppe oben auf der Startseite. So findet die Klasse die Aktivität der Stunde sofort.

## Selbst ausprobieren

Für dich gibt es ein **Testkonto**. Es steht allein in einer eigenen Gruppe, **Testgruppe (dein Name)**, ganz unten in der Gruppenauswahl. Wähl sie, geh auf **Lernende** und klick bei deinem Testkonto auf **Nachahmen**; einen Satz braucht es dafür nicht. Jetzt siehst du alles wie eine Schülerin: Aufgaben, Abgaben, die Galerie, Leistungsüberprüfungen. Mit »Zurück zu deinem Konto« bist du wieder du selbst.

Löst du in deinem eigenen Konto eine Aufgabe, steht sie auf der Klassenseite in einer eigenen Zeile **Du (Lehrkraft)** unter der Klasse. Für einen kurzen Test reicht das oft schon.

## Auf einer Aktivitätsseite

Oben rechts steht bei manchen Aktivitäten der Knopf **Notiz für Lehrkräfte**. Er öffnet Hinweise nur für dich: wie lange die Teile dauern, was die Aufgaben prüfen, wo Lernende meistens hängen bleiben. Geschlossen stört er nicht, auch nicht am Beamer.

Steht an einer Aufgabe **Vorschau für Lehrkräfte**, sind Aufgaben auf eurer Installation für Lernende noch ausgeschaltet; du siehst sie trotzdem schon.

## Was der Server speichert

- Für jede Aufgabe die letzte Antwort, ob sie stimmt und wie viele Versuche es waren. Lernende können ihre Antwort selbst löschen.
- Abgaben als Kopie, bis die Person sie löscht oder neu abgibt.
- Wer wann einen Workspace geöffnet oder jemanden nachgeahmt hat, mit deinem Satz dazu.

Noten speichert der Workspace nicht. Die Haken auf der Klassenseite sagen, ob eine Aufgabe gelöst ist, nicht wie gut.

Leistungsüberprüfungen bereitest du im **Profil** unter **Für Lehrkräfte** vor.
