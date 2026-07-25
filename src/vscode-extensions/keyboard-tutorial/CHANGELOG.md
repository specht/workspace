# Changelog

## Unreleased

- Das Tutorial ist als achtteiliges Curriculum abgeschlossen; geplante Kapitel zu Ansicht/Fokus, Terminal und Abschlussaufgabe wurden entfernt.
- Der Zeilenumbruch mit `Alt+Z` ist als Tastaturübung in das Kapitel **Navigieren im Dokument** integriert.
- Die Datei- und Ordnerübungen verwenden einen einzigen stabilen, isolierten Arbeitsordner für das gesamte Kapitel.
- Bereits angelegte, umbenannte oder gelöschte Einträge bleiben beim Wechsel zur nächsten Übung sichtbar.
- **Neu starten** setzt bei verändernden Dateiübungen nur die betroffenen Einträge zurück; beim Kapitelstart kann der gesamte Arbeitsordner neu aufgebaut werden.
- Quick-Open-Beispiele enthalten Dateien in allen Zwischenordnern, damit VS Code den Pfad nicht als kompakte Ordnerkette zusammenfasst.
- Der Umbenennschritt verwendet `bilder-archiv`, damit er nicht mit dem zuvor angelegten Ordner `bilder` kollidiert.
- Dateiübungen verwenden immer einen isolierten Tutorial-Arbeitsbereich.
- Vor dem Explorer-Kapitel wird das Tutorial angeleitet in die sekundäre Seitenleiste verschoben und am Ende gezielt zurückgesetzt.
- Der Tab-Schritt verwendet im Browser nicht `Strg+W`, sondern das Kreuz im Editor-Tab und `Strg+P` zum Wiederöffnen.
- Die Übungen zum Anlegen von Dateien und Ordnern zeigen die passenden Explorer-Symbole.
- Tutorial-Dateien werden pro Schritt getrennt gespeichert und nur über **Neu starten** zurückgesetzt.
- Editor-Ereignisse werden nur für das aktive Übungsdokument und nur bei Bedarf weitergeleitet.
- Bereits erledigte Schritte können ohne erneutes Lösen weitergeklickt werden.
- Fehlerbehandlung und Bereinigung von Event-Listenern verbessert.
- Kapitel **Text markieren** fertiggestellt.
- Kapitel **Text bearbeiten** erweitert.
- Statische Tests für Tutorial-Dateien und referenzierte Übungsdateien ergänzt.
- Das Verschieben der Tutorial-Ansicht übergibt die View-ID explizit und ist nicht mehr vom Fokuszustand abhängig.
