# Hackschule Workspace: Plan Datenschutz an Schulen

Stand: 21. Oktober 2026. Erarbeitet als Claude Doc; die Fassung hier im Repository ist eine Kopie.

## Worum es geht

Der Workspace ist eine Lernplattform für den Informatikunterricht. Er wurde von einer Berliner Informatiklehrkraft entwickelt und wird von ihr privat betrieben. Schülerinnen und Schüler programmieren damit im Browser, ohne etwas zu installieren. Er läuft auf einem gemieteten Server, auf dem sonst nichts läuft.

Den Lernenden bringt er echte Werkzeuge, wie sie in der Softwareentwicklung genutzt werden: ein Linux-System mit Terminal, Visual Studio Code, Git, viele Programmiersprachen, Datenbanken wie MySQL und Neo4j, Webserver, LaTeX und App-Entwicklung für Android. Vieles davon lässt sich zu Hause nicht einfach installieren und auf einem Tablet gar nicht. Weil alles im Browser läuft und gespeichert bleibt, arbeiten sie zu Hause genau dort weiter, wo sie in der Schule aufgehört haben.

Er ist aus dem Unterricht entstanden: Vor etwa drei Jahren wurde er für die eigenen Kurse an der Stammschule gebaut. Inzwischen nutzen ihn mehrere Berliner Schulen. Was als Werkzeug für den eigenen Unterricht begann, braucht deshalb jetzt eine saubere rechtliche Grundlage, an jeder Schule. Gespeichert werden Name, E-Mail-Adresse, Kurs, die Programme der Lernenden und ihr Bearbeitungsstand.

Der Quellcode ist öffentlich auf GitHub; jede Schule könnte den Workspace auch selbst betreiben. Das erfordert aber Zeit und Fachwissen. Deshalb wird der Betrieb Lehrkräften anderer Schulen unentgeltlich angeboten. Vertragspartner ist dabei immer die Schule, nicht die einzelne Lehrkraft.

Technisch ist der Datenschutz gut aufgestellt; es fehlt das Formale. Nach § 64 Abs. 11 Schulgesetz und der Verordnung über digitale Lehr- und Lernmittel (DigLLV) braucht die Schule keine Einwilligung jeder Familie. Dafür muss sie selbst prüfen, ihre Datenschutzbeauftragte Stelle beteiligen und einen Vertrag zur Auftragsverarbeitung schließen.

Vorgeschlagen wird eine Übergangszeit bis zum 12. April 2027, nach den Osterferien. Bis dahin sollen alle Schulen ihre Entscheidung und den Vertrag vorliegen haben. Das lässt Zeit für Gespräche, die Prüfung durch die Datenschutzbeauftragte Stelle und die Beschlüsse der Gremien, ohne den laufenden Unterricht zu unterbrechen.

Das ist ein Projektplan, keine Aussetzung der DSGVO: Ein Widerspruch oder ein Löschwunsch wird sofort bearbeitet. Die Unterlagen für die Schulen sind fertig und im Workspace unter »Datenschutz« als PDF abrufbar.

## Schritte

Zuerst die Stammschule als Vorbild, dann die anderen Schulen nach demselben Muster.

![Diagramm: Erst die Stammschule, dann die anderen Schulen nach demselben Muster. Die betreibende Lehrkraft lässt Plan und Vertrag anwaltlich prüfen und klärt die Nebentätigkeit mit der Personalstelle. An der Stammschule bekommen nacheinander Schulleitung, Datenschutzbeauftragte Stelle, Schulaufsicht (nur als LMS), Fach- und Gesamtkonferenz, die unterschreibende Schulleitung sowie Eltern und Lernende ihre Unterlagen. Danach folgen die anderen Schulen, am Ende der Stichtag 12. April 2027.](plan-datenschutz-schulen.png)

1. **Anwaltliche Durchsicht** (bis Ende der Herbstferien, 1. November): dieser Plan und der Entwurf des Vertrags zur Auftragsverarbeitung.
2. **Schulleitung der Stammschule** (ab 2. November): persönliches Gespräch. Den Workspace und den Plan vorstellen, die Rolle als Dienstleister mit Vertrag vorschlagen und darum bitten, die Datenschutzbeauftragte Stelle einzubeziehen.
3. **Datenschutzbeauftragte Stelle der Schule** (November, mit der Schulleitung): Sie bekommt das Unterlagenpaket und die offenen Fragen (unten). Ihre Einschätzung zeigt, welche Schritte noch nötig sind; entscheiden muss die Schule.
4. **Schulaufsicht** (nur wenn der Workspace als Lernmanagementsystem eingestuft wird): Die Schule bittet um ihr Einvernehmen (§ 3 DigLLV). Das dauert vermutlich am längsten, deshalb früh fragen, wie es beantragt wird.
5. **Personalstelle** (November): fragen, ob der private Betrieb als Nebentätigkeit anzuzeigen ist.
6. **Gremien der Schule** (Dezember bis Januar): Die Fachkonferenz Informatik beschließt den Einsatz (§ 16 Abs. 2 SchulG) im Rahmen der Grundsätze der Gesamtkonferenz (§ 79 Abs. 3 Nr. 8). Danach unterschreibt die Schulleitung die Entscheidung und den Vertrag.
7. **Eltern und Lernende** (nach der Entscheidung): Elternbrief und Datenschutzerklärung zur Information. Eine Einwilligung ist für den Unterricht nicht nötig.
8. **Die anderen Schulen** (ab November, über die Lehrkräfte dort an ihre Schulleitung): November und Dezember erste Information, Januar freundliche Erinnerung, Februar Nachfassen, März deutlicher Hinweis auf den Stichtag.
9. **Zwischenstand Ende Januar, Stichtag am 12. April 2027:** Was mit Schulen ohne Unterlagen geschieht, wird vorher entschieden und angekündigt. Konten und Arbeiten der Lernenden werden dabei nicht gelöscht.

## Wer bekommt was

Alle Unterlagen erzeugt der Workspace selbst, mit dem Namen der jeweiligen Schule, als ein ZIP mit PDFs. Die drei Zeilen mit »nur als LMS« braucht die Schule nur, wenn der Workspace als Lernmanagementsystem eingestuft wird.

| Unterlage | An wen | Wozu |
| --- | --- | --- |
| Information und Entscheidung | Schulleitung | erklärt den Workspace auf zwei Seiten; Formular für die Entscheidung der Schule |
| Prüfübersicht Datenschutz | Schulleitung, Datenschutzbeauftragte Stelle | die Prüfung, die die Schule selbst vornehmen muss (§ 2 Abs. 2 DigLLV), Punkt für Punkt |
| Vertrag zur Auftragsverarbeitung (Entwurf) | Schulleitung unterschreibt, Datenschutzbeauftragte Stelle prüft | die Daten werden nur im Auftrag und nach Weisung der Schule verarbeitet (Art. 28 DSGVO) |
| Technische und organisatorische Maßnahmen | Datenschutzbeauftragte Stelle; Anlage zum Vertrag | wie die Daten geschützt werden (Art. 32 DSGVO) |
| Eintrag für das Verzeichnis von Verarbeitungstätigkeiten | Schule, für ihre Akten | das Verzeichnis, das jede Schule führen muss (Art. 30 DSGVO) |
| Löschkonzept | Schule, Lehrkräfte | wann was gelöscht wird (§ 2 Abs. 6 DigLLV) |
| Elternbrief und Datenschutzerklärung | Eltern, Lernende, Lehrkräfte | Information (Art. 13 DSGVO), keine Einwilligung |
| Rollen- und Berechtigungskonzept | Schule, Datenschutzbeauftragte Stelle, Schulaufsicht | nur als LMS: wer was sehen und tun darf (§ 3 DigLLV) |
| Risikoanalyse | Schule, Datenschutzbeauftragte Stelle, Schulaufsicht | nur als LMS: was schiefgehen kann und was dagegen getan ist |
| Datenschutz-Folgenabschätzung (Entwurf) | Schule, Datenschutzbeauftragte Stelle | nur als LMS und wenn die Schule sie nach Beratung durch die Datenschutzbeauftragte Stelle für nötig hält (Art. 35 DSGVO) |
| Nutzungsordnung | alle mit einem Konto | die Regeln für die Nutzung |

## Wenn eine Schule nicht mitmacht

Eine Einwilligung der Eltern ersetzt die Entscheidung der Schule im regulären Unterricht nicht. Sie ist nur wirksam, wenn eine Ablehnung keine Nachteile hat (Art. 7 Abs. 4 und Erwägungsgrund 43 DSGVO). Im Unterricht, der auf dem Workspace aufbaut, gibt es diese freie Wahl nicht.

Mit Einwilligung geht nur, was wirklich freiwillig ist: eine AG, ein Zusatzprojekt, Üben zu Hause, eine Facharbeit. Dafür gilt: schriftlich, informiert, jederzeit widerrufbar, bei Kindern unter 16 Jahren durch die Eltern.

Weitere Wege für eine Schule, die den Vertrag nicht schließen möchte:

- den Workspace selbst betreiben, auf eigenem Server und in eigener Verantwortung
- der Förderverein als Vertragspartner für den Server
- Konten unter Pseudonym: senkt das Risiko, bleibt aber datenschutzrechtlich relevant
- die Aufnahme in die Liste geprüfter Lehr- und Lernmittel der Senatsverwaltung (§ 7 Abs. 2a SchulG): am sichersten, dauert am längsten

## Offene Fragen

Diese Punkte entscheidet die Schule, beraten von ihrer Datenschutzbeauftragten Stelle; die ersten beiden sollen vorher anwaltlich eingeschätzt werden.

1. **Der Betrieb an der Stammschule:** Der Workspace wird von einer Lehrkraft dieser Schule privat betrieben. Gehört der Betrieb zu den Dienstaufgaben, oder wird er als Dienstleistung mit Vertrag erbracht? Vorgeschlagen wird der Vertrag. Kann eine verbeamtete Lehrkraft Auftragsverarbeiter ihrer eigenen Schule sein?
2. **Der Vertrag:** Wer unterschreibt für eine Berliner Schule, die Schulleitung oder das Land? Und passt der Entwurf (Haftung, Weisungen, Unterauftragnehmer wie der Server-Anbieter)?
3. **Lehr- und Lernmittel oder Lernmanagementsystem** (§ 2 oder § 3 DigLLV)? Als Lernmanagementsystem braucht es zusätzlich das Einvernehmen der Schulaufsicht. Gilt die Einschätzung der Datenschutzbeauftragten Stelle nur für ihre Region, oder ist sie mit den anderen schulischen Datenschutzbeauftragten der Senatsverwaltung abgestimmt? Welche Schulaufsicht erteilt das Einvernehmen, und braucht jede Region ein eigenes?
4. **Rechtsgrundlage in der Datenschutzerklärung:** Art. 6 Abs. 1 lit. c oder lit. e DSGVO.
5. **Löschung:** Die DigLLV sagt »in der Regel spätestens zum Ende eines Schuljahres«. Vorgesehen ist, Konten zu behalten, wenn jemand im nächsten Jahr weiter Informatik hat.
6. **Datenschutz-Folgenabschätzung:** nötig oder nicht? Es geht um Kinder und Lernfortschritte, das spricht eher dafür.
7. **Nebentätigkeit:** Die Lehrkraft ist verbeamtet, der Betrieb unentgeltlich. Eine unentgeltliche Nebentätigkeit braucht in Berlin in der Regel keine Genehmigung, außer etwa bei einer gewerblichen Tätigkeit (§ 62 LBG). Offen: Ist eine Anzeige nötig? Und zählt der Betrieb für die Stammschule als Nebentätigkeit, wenn er dort als Dienstleistung mit Vertrag erbracht wird?

## Quellen

Recherche, keine Rechtsberatung; alle Seiten am 21. Oktober 2026 geöffnet.

- [Verordnung über digitale Lehr- und Lernmittel (DigLLV), GVBl. 2023 S. 296](https://pardok.parlament-berlin.de/starweb/adis/citat/VT/19/gvbl/g23220296.pdf)
- [§ 64 Schulgesetz Berlin](https://schulgesetz-berlin.de/schulgesetz/paragraph-64_datenverarbeitung-und-auskunftsrechte.php) (Wortlaut noch gegen [gesetze.berlin.de](https://gesetze.berlin.de) prüfen)
- [Schuldatenverordnung, GVBl. 2023 S. 283](https://pardok.parlament-berlin.de/starweb/adis/citat/VT/19/gvbl/g23220283.pdf)
- [Antwort des Senats, Drs. 19/21945 (27. März 2025)](https://pardok.parlament-berlin.de/starweb/adis/citat/VT/19/SchrAnfr/S19-21945.pdf), mit den Prüfkriterien der Senatsverwaltung
- [Berliner Datenschutzbeauftragte, Pressemitteilung vom 17. September 2021](https://datenschutz-berlin.de/fileadmin/user_upload/pdf/pressemitteilungen/2021/20210917-PM_Schulgesetz.pdf): Einwilligungen für den Lernraum Berlin »nicht mehr nötig«
- Berliner Datenschutzbeauftragte, Pressemitteilung vom 22. Januar 2021: Duldung macht den Einsatz nicht rechtmäßig
- [Europäischer Datenschutzausschuss, Leitlinien 05/2020 zur Einwilligung](https://www.edpb.europa.eu/system/files/documents/files/file1/edpb_guidelines_202005_consent_en.pdf)
- [Europäischer Datenschutzausschuss, Leitlinien 07/2020 zu Verantwortlichen und Auftragsverarbeitern](https://www.edpb.europa.eu/system/files/documents/2023-10/EDPB_guidelines_202007_controllerprocessor_final_en.pdf)
