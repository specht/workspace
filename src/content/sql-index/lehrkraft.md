Ein Labor zu Indizes, nach der [Einführung in SQL](/mysql): Lernende füllen ihre eigene Datenbank mit einem ausgedachten Telefonbuch von einer Million Einträgen, messen eine Suche, sehen mit `EXPLAIN` den Full Table Scan, legen einen Index an und messen noch einmal. Danach der B-Baum, warum der Primärschlüssel schon einen Index hat, wann ein Index nicht hilft (`LIKE '%…'`, eine Funktion um die Spalte, nur die zweite Spalte eines zusammengesetzten Index) und was er kostet. Für die Oberstufe; der B-Baum knüpft an Bäume als Datenstruktur an.

<table class='table'>
<tr><th>Ablauf</th><td>Eine Doppelstunde: Telefonbuch anlegen und die erste Messung (20 Minuten), <code>EXPLAIN</code> und der erste Index (25 Minuten), der B-Baum mit der Aufgabe zu den Ebenen (15 Minuten), wann ein Index nicht hilft und der Index über zwei Spalten (20 Minuten), was er kostet (10 Minuten).</td></tr>
<tr><th>Daten</th><td>Die Lernenden legen <code>telefonbuch.sql</code> von der Seite an; die Datei baut die Tabelle <code>person</code> in ihrer eigenen Datenbank, eine Million Zeilen in etwa sechs Sekunden. Die Namen kommen aus <code>CRC32</code> der Zeilennummer, nicht aus dem Zufall, darum sind die Daten bei allen gleich und die Aufgaben haben feste Antworten. Jede Telefonnummer gibt es genau einmal. Die Tabelle braucht etwa 66 MB, mit beiden Indizes etwa 120 MB; <code>DROP TABLE person;</code> gibt den Platz am Ende frei.</td></tr>
<tr><th>Prüfung</th><td>»Das Telefonbuch« zählt die Zeilen, die beiden Index-Aufgaben lesen aus <code>information_schema</code>, ob es einen Index gibt, der mit den verlangten Spalten anfängt; welchen Namen er hat, ist gleich. »Alle Ada Specht« ist eine Abfrage, deren Ergebnis mit dem der Musterabfrage verglichen wird (1.222 Zeilen). Die anderen sind Werte, Auswahl und eine Reflexion.</td></tr>
<tr><th>Lösung</th><td>Jede Aufgabe zeigt nach der richtigen Antwort eine Lösung; die Musterabfragen und Antworten stehen in <a href='https://github.com/specht/workspace/blob/main/src/content/sql-index/tasks.yaml'><code>tasks.yaml</code></a>.</td></tr>
</table>

**Die Messung.** Ohne Index braucht die Suche nach einer Nummer etwa eine Drittelsekunde, mit Index ein paar Millisekunden, und das meiste davon ist `mycli` selbst. Die genauen Zeiten hängen davon ab, wie viele gleichzeitig auf dem Datenbankserver arbeiten; legt eine ganze Klasse gleichzeitig ihr Telefonbuch an, werden alle für ein paar Sekunden langsamer. Deutlich bleibt der Unterschied immer, und `EXPLAIN` ist davon unabhängig: `rows` ist eine Schätzung, die von Lauf zu Lauf etwas schwankt, `type` und `key` nicht.

**Die Aufgaben.** Zwei sind Vorhersagen, bevor man es ausprobiert: welche der drei Bedingungen den Index nutzt und ob ein Index über Nachname und Vorname bei der Suche nach dem Vornamen allein hilft. »Wie hoch ist der Baum?« rechnet mit 100 Schlüsseln pro Knoten: drei Ebenen für eine Million; die Rückmeldung erkennt 20, die Schritte der binären Suche. »Nicht auf jede Spalte« ist eine Reflexion über Platz und Schreibzeit.

**Was schiefgeht.**

- **Die Datei läuft nicht durch.** Wer `telefonbuch.sql` nur teilweise kopiert, bekommt eine Fehlermeldung zu `ELT` oder eine leere Tabelle. Die Datei fängt mit `DROP TABLE` an und lässt sich einfach noch einmal ausführen.
- **`\G` in der Karte.** `\G` am Ende funktioniert in `mycli` und in `mysql`; wer es in die Karte einer SQL-Aufgabe schreibt, bekommt einen Fehler, dort gehört nur die Abfrage hin.
- **Den Index zweimal anlegen.** Ein zweites `CREATE INDEX by_phone` bricht mit »Duplicate key name« ab; die Aufgabe ist trotzdem gelöst.
- **`type: ref` statt `const`.** Mit einem gewöhnlichen Index weiß die Datenbank nicht, dass jede Nummer nur einmal vorkommt. Wer neugierig ist, probiert `CREATE UNIQUE INDEX`; die Seite erwähnt es am Ende.

**Weiter.** In der [Filmdatenbank](/filmdatenbank) haben die Tabellen zwischen zwei anderen einen Primärschlüssel über zwei Spalten; mit `EXPLAIN` lässt sich dort prüfen, welche Abfragen er beschleunigt. Die Sandbox [Datenstrukturen](/sandbox-datenstrukturen) zeigt Bäume von innen.
