# Lehrpläne der Länder

Eine Lehrkraft wählt im Profil (»Für Lehrkräfte«, »Lehrpläne«) oder oben auf
`/lehrplan` ihr Bundesland. Danach sieht sie die Zeile oben auf jeder
Aktivität, die Zeile der Katas auf der Startseite und die Seite `/lehrplan`
in den Namen der Pläne ihres Landes. Wer nichts wählt, sieht Berlin, so wie
bisher. Lernende sehen keinen Lehrplan, wie bisher.

Stand: Berlin und Brandenburg sind fertig. Baden-Württemberg ist ein Entwurf,
den eine Lehrkraft einer Stuttgarter Schule prüft.

## Wie es gebaut ist

**Themen.** Jede Aktivität nennt im Meta-Block ihre Themen, unabhängig von
einem Land:

```yaml
topics: [grundstrukturen, unterprogramme]
```

Die Themen stehen in `src/content/curricula/themen.yaml`, gegliedert nach den
Inhaltsbereichen der Bildungsstandards der Gesellschaft für Informatik
(Information und Daten, Algorithmen, Sprachen und Automaten,
Informatiksysteme, Informatik, Mensch und Gesellschaft), dazu Modellieren und
Programmieren sowie Projekte. Es sind rund fünfzig. Sie sind so fein, dass sie
tragen, was die Namen der Pläne von Berlin und Brandenburg unterscheiden.
Ein Thema nennt, was die Aktivität wirklich übt, nicht alles, was sie
berührt.

**Ein Plan je Datei.** `src/content/curricula/<plan>.yaml` hat:

- `states`: die Länder, für die er gilt, mit ihrem Namen in der Auswahl;
- `status`: `geprüft` oder `Entwurf`, dazu `status_text`, den die Seiten bei
  einem Entwurf zeigen;
- `levels`: die Stufen (Klasse, Fach, Kursstufe), jede mit `source` (Titel
  und Fassung des Plans), `url`, den Gruppen ihrer Namen und `ages`;
- `intro` und `label`: der Text über `/lehrplan` und der Titel der Zeile;
- `topics`: wo jedes Thema im Plan steht, als »Stufe: Name (Einzelheiten)«;
- `lines`: Zeilen, die eine Lehrkraft für eine Aktivität geprüft oder
  geschrieben hat; sie gelten statt der abgeleiteten.

**Die Zeile einer Aktivität** in einem Plan ist ihre eigene unter `lines`,
wenn es eine gibt, sonst die aus ihren Themen abgeleitete: Für jedes Thema
die Stellen im Plan, nur in den Stufen, die das Alter der Aktivität zulässt,
jeder Name einmal, mit den Einzelheiten aller Themen, die zu ihm führen, in
der Reihenfolge des Plans.

**Das Alter** (Sek I, Sek I und Oberstufe, Oberstufe) kommt weiter aus der
Berliner Zeile und `age:`. Es gilt in jedem Land: Eine Stufe sagt mit
`ages`, welche Aktivitäten zu ihr passen. Klasse 7 nimmt keine Aktivität, die
nur für die Oberstufe gedacht ist. Die Farben auf `/lehrplan` und das
gespeicherte Bild der Wege ändern sich mit dem Land nicht.

**Berlin und Brandenburg** teilen sich `berlin-brandenburg.yaml`, sind aber
zwei Einträge in der Auswahl. Ihre Zeile ist die Zeile `curriculum:` im
Meta-Block jeder Aktivität, wie bisher ([lehrplan.md](lehrplan.md)). Den
Rahmenlehrplan der Sek I haben beide Länder gemeinsam. In der Oberstufe sind
die Kurshalbjahre gemeinsam erarbeitet; die Einführungsphase ist die
Berliner, und Brandenburg hat sie in Klasse 10. Eine Lehrkraft aus
Brandenburg liest das unter der Überschrift »Oberstufe« (`source_for`). Ob
dort mehr abweicht, prüft eine Lehrkraft aus Brandenburg.

**Gespeichert** wird das Land als `curriculum_state` am Konto der Lehrkraft
(`POST /api/set_curriculum_state/<land>`). Die Datenschutzerklärung nennt es
unter den Einstellungen (`privacy.rb`).

## Was nur für Berlin gilt

- Der Datenschutz: die Dokumente für die Schulen, die Rechtsgrundlagen, die
  Übergangszeit ([datenschutz.md](datenschutz.md),
  [handoff-datenschutz.md](handoff-datenschutz.md)). Eine Schule in einem
  anderen Land klärt den Einsatz nach ihrem Schulrecht; das Land im Profil
  ändert daran nichts.
- Die Notiz »In Berlin verpflichtend« beim Wahlthemenfeld Datenbanken.
- Die Zeile `curriculum:` selbst. Neue Aktivitäten schreiben sie weiter in den
  Namen von Berlin und Brandenburg, dazu ihre Themen.

## Baden-Württemberg (Entwurf)

Was ein Gymnasium in Baden-Württemberg im Schuljahr 2026/27 für Informatik
hat:

| Stufe | Plan | Stand |
| --- | --- | --- |
| Klasse 7 | [Bildungsplan 2016, Aufbaukurs Informatik](https://www.bildungsplaene-bw.de/,Lde/LS/BP2016BW/ALLG/SEK1/INF7) | gilt für Klasse 7, bis das neue Pflichtfach »Informatik und Medienbildung« (Klassen 5 bis 11, seit 2025/26 von Klasse 5 an eingeführt) einen eigenen Bildungsplan hat |
| Klassen 8–10 | [Bildungsplan 2016, Profilfach IMP, Teil Informatik](https://www.bildungsplaene-bw.de/,Lde/LS/BP2016BW/ALLG/GYM/IMP) | läuft aus; der Nachfolger NIT (Naturwissenschaft, Informatik und Technik) hat noch keinen Bildungsplan |
| Kursstufe | [Bildungsplan 2016, Informatik (Schulversuch), Versuchsfassung 2020](https://www.bildungsplaene-bw.de/,Lde/LS/BP2016BW/ALLG/GYM/INF) | Basisfach und Leistungsfach; die Teilkompetenzen nur des Leistungsfachs sind markiert |

Weitere Quellen: das Kultusministerium zum neuen Fach
([Informatik und Medienbildung](https://km.baden-wuerttemberg.de/de/schule/schulartuebergreifend/mint/schule-und-unterricht/informatik-und-medienbildung)),
der Informatik-Monitor der GI
([Baden-Württemberg 2025/26](https://informatik-monitor.de/2025-26/laendervergleich/baden-wuerttemberg)).
Abgerufen am 27. Oktober 2026.

Die Namen sind die inhaltsbezogenen Kompetenzbereiche der Pläne, ohne
Nummern: in Klasse 7 und IMP »Daten und Codierung«, »Algorithmen«, »Rechner
und Netze«, »Informationsgesellschaft und Datensicherheit«; in der Kursstufe
die zehn Abschnitte von »Bitebene« bis »Projektmanagement«. Bei IMP steht in
den Einzelheiten, in welcher Klasse etwas vorkommt.

Zugeordnet ist nur, was eine Teilkompetenz ausdrücklich nennt. Keine Stelle
haben darum unter anderem: Computergrafik, künstliche Intelligenz,
Geschichte, Programmiersprachen, Turingmaschinen, Webseiten außer in IMP
Klasse 8, Betriebssystem und Dateien. 31 Aktivitäten des Katalogs haben
deshalb in Baden-Württemberg keine Zeile: Oben auf der Seite steht keine,
und die Liste auf `/lehrplan` nennt sie nicht; im Bild der Wege stehen sie,
weil es aus `requires:` entsteht. Auf der anderen Seite übt noch keine Aktivität die
Schaltnetze von IMP Klasse 10.

### Was die Lehrkraft prüft

1. **Die Stufen.** Unterrichtet die Schule Klasse 7 nach dem Aufbaukurs, IMP
   oder schon NIT in 8–10, Informatik in der Kursstufe als Basis- oder
   Leistungsfach? Fehlt eine Stufe, etwa das Wahlfach Informatik in der
   Oberstufe oder Realschule und Gemeinschaftsschule?
2. **Die Zuordnung der Themen** in `baden-wuerttemberg.yaml` unter `topics`:
   Steht jedes Thema dort, wo der Plan es meint, und nirgends sonst?
3. **Die Zeilen der Aktivitäten**, die die Schule nutzen will: unter
   `/lehrplan` mit Baden-Württemberg im Profil, und oben auf jeder Aktivität.
   Passt eine nicht, kommt eine eigene Zeile unter `lines`, zum Beispiel
   `rle: "IMP 8–10: Daten und Codierung (Klasse 9: Lauflängencodierung)"`.
4. **Was fehlt:** Aktivitäten, die in Baden-Württemberg passen, aber keine
   Zeile haben, etwa Aktivitäten zum Programmieren in Klasse 7 mit einer
   visuellen Umgebung.
5. **Ist alles geprüft**, wird aus `status: Entwurf` ein `status: geprüft`,
   und der Hinweis auf den Seiten verschwindet.

## Ein weiteres Land

1. Eine Datei `src/content/curricula/<plan>.yaml` nach dem Muster von
   `baden-wuerttemberg.yaml`: Quellen mit Link und Datum, die Stufen, die
   Zuordnung der Themen, `status: Entwurf`.
2. `curriculum_test.rb` prüft, dass jede Stelle und jede Zeile nur Namen des
   Plans nennt und dass ein Land nur in einem Plan steht.
3. Unter `/lehrplan` mit dem Land im Profil ansehen, dann eine Lehrkraft des
   Landes prüfen lassen.

Neue Themen kommen in `themen.yaml`. Ein neues Thema braucht in jedem Plan
eine Stelle oder bewusst keine.
