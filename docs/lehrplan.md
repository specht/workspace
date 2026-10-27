# Die Zeile des Lehrplans

Jede Aktivität im Katalog hat im Meta-Block eine Zeile `curriculum:`, die nur
angemeldete Lehrkräfte sehen: oben auf ihrer Seite (in Lila, der Farbe dessen,
was nur Lehrkräfte sehen), bei den Katas im Streifen auf der Startseite und
gesammelt auf `/lehrplan`. Auf den Karten im Katalog steht sie nicht: Dort
zählt, worum es geht. Sie sagt, wo die Aktivität in den Lehrplänen
steht, damit eine Lehrkraft sie in ihre Planung einordnen kann.

Die Seite `/lehrplan` (verlinkt von der Klassenseite und von der Zeile oben
auf jeder Aktivität) zeigt es andersherum: für jeden Namen der Pläne die
Aktivitäten, deren Zeile ihn nennt, in der Reihenfolge des Katalogs, mit den
Einzelheiten aus ihrer Zeile. Sie entsteht bei jedem Aufruf aus diesen Zeilen
(`src/ruby/include/curriculum_overview.rb`): Eine neue Aktivität steht dort,
sobald sie im Katalog ist und ihre Zeile hat, auf dem Server nach dem
nächsten Update (`./config.rb restart ruby`). Aktivitäten verborgener
Abschnitte stehen nicht darauf. Ein Name ohne Aktivität bleibt mit »Noch
keine Aktivität im Workspace« stehen: Auch eine Lücke ist gut zu wissen.

Über der Liste steht ein Bild der Wege durch die Aktivitäten
(`src/ruby/include/curriculum_graph.rb`): jede Aktivität, die auf einer
anderen aufbaut (`requires:`) oder auf der eine andere aufbaut, als Kasten in
der Farbe ihres Abschnitts im Katalog, mit einem Pfeil von dem, was zuerst
kommt. Von links nach rechts nach der längsten Kette davor; in einer Spalte
stehen die Kästen nahe bei denen, mit denen sie verbunden sind. Wählt eine
Lehrkraft einen Namen der Pläne (über dem Bild, oder mit dem Knopf neben dem
Namen in der Liste) oder klickt sie auf einen Kasten, bleibt hervorgehoben,
was den Namen trägt, und jede Aktivität auf einem Weg dorthin; das Symbol im
Kasten öffnet die Aktivität in einem neuen Tab. Aktivitäten ohne solche
Verbindung stehen nicht im Bild, sondern darunter in einer Liste.

```yaml
curriculum: "Sek I: Algorithmisches Problemlösen (algorithmische Grundstrukturen: Wiederholung); Oberstufe: Algorithmen und Datenstrukturen"
```

## Die Form

- Ein Teil für die Sekundarstufe I (`Sek I: `), einer für die Oberstufe
  (`Oberstufe: `) oder beide, in dieser Reihenfolge, getrennt durch `; `.
- In einem Teil die Namen aus den Listen unten, getrennt durch `, `, jeder auf
  Wunsch mit Einzelheiten in Klammern. Nur die Einzelheiten sind frei; sie
  nehmen, wo es geht, die Worte des Plans (»algorithmische Grundstrukturen«,
  »Codierung«, »Protokolle«).
- Die Namen ohne Nummern und Kürzel (kein »3.5«, kein »in-3«, kein »V3«):
  Lehrkräfte in Berlin und Brandenburg erkennen die Namen, andere können sie
  trotzdem lesen.
- Was die Seite wirklich übt, nicht alles, was sie berührt: zwei, drei Namen
  genügen. Wo die Sek I nichts dazu sagt (Raytracing), nur die Oberstufe.
- In Anführungszeichen, weil der Doppelpunkt sonst YAML stört.

`src/ruby/include/curriculum.rb` kennt die Namen; `curriculum_test.rb` prüft
jede Aktivität des Katalogs (außer »Zurückgestellt«): dass sie eine Zeile hat
und dass jeder Name darin aus den Listen stammt.

## Sekundarstufe I

Rahmenlehrplan Teil C Informatik, Wahlpflichtfach, Jahrgangsstufen 7–10,
Berlin und Brandenburg (2015). Fünf Pflichtthemenfelder und fünf
Wahlthemenfelder; in Berlin ist das Wahlthemenfeld Datenbanken verpflichtend.
Die Inhalte jedes Themenfelds, für die Einzelheiten:

| Name | Inhalte |
| --- | --- |
| Standardsoftware | Umgang mit Textverarbeitungssoftware; mit mindestens einer weiteren Software (Präsentation, Tabellenkalkulation oder Grafikbearbeitung); Umgang mit einem Browser; Nutzung von Hilfesystemen |
| Informatiksysteme | Aufbau und Eigenschaften eines Informatiksystems; Bedienen von Hard- und Software; strukturierte Speicherung von Dateien; Grundprinzipien der Datensicherheit; EVA-Prinzip; Netzwerkkommunikation |
| Leben in und mit vernetzten Systemen | Beschaffung und Prüfung von Informationen; Suchmaschinen; Datenaustausch im Netzwerk; Datenschutz; Netiquette; rechtliche Grundlagen; Gestalten von Internetseiten, Stylesheets; Gefahren bei E-Mails (Phishing, Schadprogramme) |
| Information und Daten | Daten, Information, Nachricht; Sender-Empfänger-Modell; Codierung (ASCII, Unicode, Strichcode, QR-Code); Datensicherheit; Zahlensysteme, Rechnen mit Dualzahlen |
| Algorithmisches Problemlösen | Eigenschaften und Darstellung von Algorithmen; Modellierung einfacher Abläufe; algorithmische Grundstrukturen; Analysieren, Modifizieren und Implementieren von Algorithmen; Variablenkonzept und Prozeduren; Zeichenkettenbearbeitung; Grundlagen der Objektorientierung; Miniwelten |
| Geschichte der Informatik | Lebensläufe und Leistungen bedeutender Persönlichkeiten; technische Entwicklung der Informationsübertragung; Veränderung von Berufsfeldern; aktuelle Entwicklungen; Kontexte: Rechenhilfsmittel, Verschlüsselung, Rechnergenerationen |
| Datenbanken | lebensweltliche Datensammlungen; Attribute mit Name, Typ und Wert, Datensätze; Datensätze suchen, einfügen, löschen, verändern; Datenbanken als Tabellen planen; verknüpfte Tabellen |
| Projektmanagement | Planung und Durchführung eines Projekts (Hard- und/oder Software); Arbeiten und Kooperieren im Team |
| Physical Computing | eingebettete Systeme; Verarbeitung elektrischer Größen; Mikrocontroller als Informatiksystem; einfache elektronische Schaltungen |
| Digitale Bilder und Visualisierung | Bedeutung und Anwendung digitaler Bilder; Digitalisierung von Bildern; Raster- und Vektorgrafik; Farbmodelle; Bildbearbeitung; Manipulation von Bildern und Filmen; Kontexte: Turtle-Grafik, Figuren und Animationen, rekursive Algorithmen für Grafiken |

## Oberstufe

Rahmenlehrplan für die gymnasiale Oberstufe, Teil C Informatik, Berlin
(Einführungsphase überarbeitet 2010; Kapitel 1 ersetzt durch die Teile A und B
von 2021).

| Name | Woher | Inhalte |
| --- | --- | --- |
| Grundlagen informatischen Arbeitens | Einführungsphase | Arbeitsumgebung im Rechnernetz, Zugriffsrechte, Verzeichnisbaum; Betriebssysteme; Geschichte der Informatik |
| Algorithmen und Softwareentwicklung | Einführungsphase | Algorithmik im Kleinen: Sequenz, Auswahl, Wiederholung, Variablenkonzept, einfache Datentypen, Parameterkonzept; Struktogramm, Pseudocode |
| Datenbanken und Datenschutz | Einführungsphase | ein relationales Datenbanksystem benutzen; Datenschutz, Urheberrecht; einfache Abfragen mit SQL |
| Computernetze | Einführungsphase | LAN, WAN, WLAN; Internet; Schichtenmodell, Protokolle; Datensicherheit |
| Datenbanken | 1. und 2. Kurshalbjahr | Datenmodellierung; relationales Datenbankschema; Abfragen (Projektion, Selektion, Join); Datenschutz und Datensicherheit; Normalisierung (Leistungskurs) |
| Algorithmen und Datenstrukturen | 1. und 2. Kurshalbjahr | |
| objektorientierte Modellierung | 1. und 2. Kurshalbjahr | UML-Klassendiagramme |
| objektorientierte Programmierung | 1. und 2. Kurshalbjahr | |
| Syntax und Semantik | 1. und 2. Kurshalbjahr | Syntaxdiagramme |
| Rechner und Netze | 3. Kurshalbjahr | Schichtenarchitektur; Von-Neumann-Architektur; Client-Server-Struktur; Protokolle |
| Sprachen und Automaten | 3. Kurshalbjahr | zustandsorientierte Modellierung; endliche Automaten; natürliche und formale Sprachen |
| Softwareprojekt | 4. Kurshalbjahr | systematische Softwareentwicklung; Ergonomie |
| Vertiefung Applikative Programmierung | 3. Kurshalbjahr, Vertiefungsgebiet | funktional oder logisch |
| Vertiefung Kryptologie und Datensicherheit | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Computergrafik | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Computer-Netze | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Künstliche Intelligenz | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Technische Informatik | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Maschinennahe Programmierung | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Informatik und Gesellschaft | 3. Kurshalbjahr, Vertiefungsgebiet | |
| Vertiefung Theoretische Informatik | 3. Kurshalbjahr, Vertiefungsgebiet | |
