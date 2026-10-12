# Lehrkräfte und Lehrmaterial

Die Einladungsliste unter `data/invitations/*.txt` (im Container:
`/invitations/*.txt`) trennt zwei Dinge, die bisher gekoppelt waren:
Lehrkraftstatus und Gruppenadministration.

```text
> Kurs 9a
Max Mustermann <max@example.org>
Lea Lehrerin <lea@example.org>
* lea@example.org
+ max@example.org
```

- Normale Zeilen laden Benutzer ein und ordnen sie ihrer ersten Gruppe zu.
- `* email@example.org` (alternativ `* <email@example.org>`) kennzeichnet
  einen **bereits eingeladenen** Benutzer als Lehrkraft. Dies gewährt nur
  Zugriff auf die Rubrik **Lehrmaterial**, keine administrativen Rechte.
- `+ email@example.org` ist unverändert: Die Person verwaltet die aktuelle
  Gruppe und gilt ebenfalls als Lehrkraft. `*` ist dann überflüssig.
- Die in `ADMIN_USERS` eingetragenen Benutzer gelten ebenfalls als Lehrkräfte.
- Markierungen ohne passende Einladung berechtigen nicht zur Anmeldung.

Die Seite `/lehrmaterial` und ihre Kachel in der Startseitenübersicht sind nur
für angemeldete Lehrkräfte sichtbar. Andere angemeldete Benutzer erhalten auf
der direkten URL einen HTTP-403-Status; nicht angemeldete Benutzer werden zur
Anmeldung weitergeleitet. Die bisherigen Verwaltungsrechte und
Leistungsüberprüfungs-Funktionen bleiben ausschließlich an `+` bzw.
`ADMIN_USERS` gebunden.

Screenshots in der Lehrmaterial-Seite verwenden das vorhandene
`<!-- tutorial-screenshot ... -->`-System und werden im Entwicklungsmodus durch
Aufruf von `/lehrmaterial` erzeugt. Die neuen `preview-port`-Anweisungen
öffnen dafür das laufende Webinterface der Sandbox über die bestehende
Workspace-Portweiterleitung. Die generierten `.webp`-Dateien samt
`.tutorial-screenshots.json` gehören nach Prüfung ins Repository; ohne diese
Bilder sind im Produktivbetrieb zunächst Platzhalter bzw. fehlende Bilder
sichtbar.
