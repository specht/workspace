Lernende fragen eine echte API, den freien Wetterdienst Open-Meteo: erst mit `curl` und `jq`, dann aus Ruby oder Python mit Fehlerbehandlung und Cache, zum Schluss mit `fetch` aus einer eigenen Webseite. Die Aktivität schließt an TCP/IP an und ist die Gegenseite zu »Ein eigener Webserver«: hier der Client, dort der Server.

<table class='table'>
<tr><th>Voraussetzungen</th><td>TCP/IP; HTML aus »Statische Webseiten« hilft bei der Wetterseite. Python braucht nur die Standardbibliothek (<code>urllib.request</code>, <code>json</code>), nichts zu installieren.</td></tr>
<tr><th>Sprache</th><td>Welche Sprache eine Klasse sieht, stellst du auf der <a href='/klasse'>Klassenseite</a> unter »Sprache« ein, ohne Einstellung ist es Ruby. Lernende können auf der Seite selbst umschalten; eine Aufgabe, die sie in einer Sprache gelöst haben, gilt auch in der anderen.</td></tr>
<tr><th>Ablauf</th><td><ul>
<li><b>1. Doppelstunde:</b> was eine API ist, Geocoding und Wetter mit <code>curl</code> und <code>jq</code>, die Query, Fehler (kein Treffer, 400), <code>wetter.rb</code> oder <code>wetter.py</code>.</li>
<li><b>2. Doppelstunde:</b> Grenzen und Cache, die Wetterseite mit <code>fetch</code> und »Go Live«, CORS, eigene Erweiterungen.</li>
</ul></td></tr>
<tr><th>Dateien</th><td>In <code>/workspace/wetter</code>: <code>wetter.rb</code> oder <code>wetter.py</code> (mit dem Ordner <code>cache</code>) und <code>index.html</code>. Die Stufen liegen unter <code>src/content/wetter/stufen/</code> (<code>wetter1.rb</code> bis <code>wetter3.rb</code>, <code>wetter1.py</code> bis <code>wetter3.py</code>, <code>index.html</code>).</td></tr>
<tr><th>Grenzen der API</th><td><ul>
<li>Open-Meteo erlaubt kostenlos höchstens 600 Anfragen pro Minute, 5.000 pro Stunde und 10.000 pro Tag, je Adresse, nur für nicht kommerzielle Zwecke und mit Nennung der Quelle (CC BY 4.0).</li>
<li>Zwei Adressen werden geteilt: Alle Workspaces gehen über den Server ins Netz (<code>curl</code>, <code>wetter.rb</code>, <code>wetter.py</code>), die Wetterseite fragt aus dem Browser, im Schulnetz also über dessen Adresse.</li>
<li>Eine Schleife, die jede Sekunde fragt, braucht das Tageskontingent in gut einer Stunde auf, ohne Pause in Minuten.</li>
<li>Die Wetterseite merkt sich Antworten zehn Minuten im Browser, damit das Neuladen durch »Go Live« bei jedem Speichern nichts kostet.</li>
</ul></td></tr>
<tr><th>Spiegel</th><td><ul>
<li>Unter <code>${web_root}/open-meteo/…</code> spiegelt der Workspace die beiden Endpunkte, mit denselben Pfaden und Parametern. Er merkt sich jede Antwort eine Viertelstunde für alle und gibt die letzte bekannte heraus, wenn Open-Meteo sich weigert.</li>
<li>Die Seite zeigt im Abschnitt »Wenn die API nicht mehr will«, wie man die beiden Konstanten oben in <code>wetter.rb</code> (oder <code>wetter.py</code>) und <code>index.html</code> austauscht.</li>
<li>Meldet Open-Meteo <code>429</code> (etwa <code>Hourly API request limit exceeded</code>), lohnt es sich, die ganze Klasse umstellen zu lassen und nach einer Schleife zu suchen, die ohne Pause fragt (<code>ps aux | grep -E 'ruby|python'</code> im betroffenen Workspace).</li>
</ul></td></tr>
</table>

#### Was schiefgeht

- **Ohne Ort** zeigt `ruby wetter.rb` (oder `python3 wetter.py`) Berlin. Ein Ort mit Leerzeichen gehört in Anführungszeichen: `ruby wetter.rb "Frankfurt am Main"`.
- **Ein langer Traceback in Python:** Lässt jemand den `try`-Block weg, steht eine Fehlermeldung von `urlopen` als `HTTPError` da; der Statuscode steht in der letzten Zeile.
- **Der falsche Ort:** Fragt man nach einem kleinen Ort, findet die Geocoding-API manchmal einen gleichnamigen anderswo; der erste Treffer ist meist der größte.
- **Die Wetterseite als Datei:** Sie lädt nur über »Go Live« oder einen anderen Webserver richtig; als Datei geöffnet funktioniert `fetch` zwar, aber Fehler sind dann schwerer zu finden.
- **Werte ändern sich mit dem Wetter:** Aufgaben fragen darum nur nach dem Breitengrad von Berlin, der sich nicht ändert.

#### Die Aufgaben

- »Wo liegt Berlin?« ist eine Rechenaufgabe mit 52,52.
- »Das Wetter aus deinem Programm«, »Ein Cache« und »Die Wetterseite« lesen `wetter.rb` und `index.html` und suchen die beiden APIs, `JSON.parse` und `ARGV`, `File.write` und `File.mtime` (in `wetter.py`: `json.load`, `sys.argv`, `write_bytes` und `st_mtime`), `fetch`, `.json()` und die Nennung von Open-Meteo. Sie erkennen beide Adressen, die von Open-Meteo und die des Spiegels.
- »Ein Ort, den es nicht gibt« ist eine Vorhersage (die API antwortet mit 200 und ohne `results`).
- Die übrigen fragen nach den Parametern für sieben Tage, wohin ein API-Schlüssel gehört und warum der Spiegel das Kontingent der ganzen Klasse schont.

#### Jetzt bist du dran

- Im Programm für jeden Ort fehlen zwei Stellen, die die Lernenden zuerst selbst schreiben: die Meldung, wenn die Geocoding-API keinen Ort findet (die Antwort ohne `results` kennen sie aus »Wenn etwas schiefgeht«), und die Zeile für jeden der drei Tage aus den parallelen Listen in `daily`. Die Seite geht danach mit ihrem eigenen Code weiter.
- Vor dem Code der Seite stehen Tipps, ein Denkanstoß und ein Gerüst mit Lücken. Jede Stufe erscheint erst eine Minute nach der vorigen, ohne Uhr, damit die Lernenden mit dem Tipp arbeiten, statt auf den nächsten zu warten; der Code der Seite ist die letzte Stufe. Wer welchen Tipp aufgeklappt hat, sieht niemand außer der Person selbst.
- Wer mehr will: stündliche Werte als Kurve auf einem `<canvas>`, ein Vergleich mehrerer Städte oder eine andere freie API. Über API-Schlüssel und warum sie nicht in eine Webseite gehören, lohnt sich ein kurzes Gespräch.
