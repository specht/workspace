<div class='meta'>
image: teaser.webp
image_alt: "Ein Graph von Commits: Von der blauen Linie main zweigen die Branches lena, ida, prozent und tipps in Grün, Orange, Lila und Rot ab und führen wieder in sie zurück."
kind: projekt
duration: 2 Doppelstunden
languages: [ruby, python]
curriculum: "Sek I: Projektmanagement (Arbeiten und Kooperieren im Team); Oberstufe: Softwareprojekt (systematische Softwareentwicklung: Versionsverwaltung im Team)"
reference: [git, gitlab]
class_repo: group
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Im Team mit Git: das Klassenquiz

<p class='abstract'>
Eure Klasse baut zusammen ein Quiz: Jede und jeder schreibt eine Runde Fragen, alle in einem gemeinsamen Git-Repository. Dabei passiert, womit Teams jeden Tag umgehen: Ein Push wird abgelehnt, zwei ändern dieselbe Zeile, ein Konflikt will gelöst werden. Danach verteilt ihr die Arbeit am Programm, und jede und jeder erledigt eine Aufgabe in einem eigenen Branch.
</p>

## Worum es geht

Allein an einem Programm zu arbeiten ist einfach: Was in deinem Ordner liegt,
gilt. Arbeiten zwanzig Leute an demselben Programm, braucht es eine Stelle, an
der alles zusammenkommt, und Regeln, damit niemand die Arbeit der anderen
überschreibt. Diese Stelle ist ein **gemeinsames Git-Repository**: Jede und
jeder hat einen Klon davon, arbeitet darin, holt sich die Commits der anderen
mit `git pull` und gibt die eigenen mit `git push` ab.

<!-- if: class_repo_outside -->
Euer Repository liegt auf dem Git-Server [${git_host}](${git_web}). Darin liegt
schon das Quiz: ein kleines Programm, eine Runde zum Ausprobieren und eine
Liste mit Aufgaben. Deine Lehrkraft lädt dich auf den Server ein; mit der
Einladung kannst du dort mitmachen.
<!-- /if -->
<!-- if: class_repo_local -->
Euer Repository liegt auf dem Git-Server des Workspace, `git.workspace`. Darin
liegt schon das Quiz: ein kleines Programm, eine Runde zum Ausprobieren und
eine Liste mit Aufgaben. Ein Konto brauchst du dort nicht.
<!-- /if -->
<!-- if: !class_repo -->
Ohne Git-Server spielst du das Team in deinem Workspace nach: Ein Repository
in einem eigenen Ordner ist der »Server«, und neben deinem Klon gibt es einen
zweiten, den von Ida, einer ausgedachten Mitschülerin. Was Ida tut, tippst du
in ihrem Ordner selbst.
<!-- /if -->

Die Befehle für jeden Tag stehen im Handbuch unter [Git](/reference/git), was
ein Git-Server dazu braucht unter [Git-Server](/reference/gitlab).

## Einrichten

<!-- if: class_repo -->
### Einmalig: Zugang zum Git-Server
<!-- /if -->
<!-- if: class_repo_ssh -->

_include(gitlab#ssh-schluessel)

_task(ssh-schluessel)
<!-- /if -->
<!-- if: class_repo_local_keys -->

_include(gitlab#ssh-workspace)

_task(ssh-schluessel)
<!-- /if -->
<!-- if: class_repo_local_auto -->

_include(gitlab#ssh-workspace-auto)
<!-- /if -->
<!-- if: class_repo_https -->

_include(gitlab#https-anmelden)
<!-- /if -->

_include(gitlab#git-name, done: git-name)

<!-- if: !class_repo -->
Ohne Git-Server ist jede E-Mail-Adresse recht; Git schreibt sie nur in deine
Commits.
<!-- /if -->

_task(git-name)

### Einmalig: Klonen

<!-- if: class_repo -->
Klone das Repository der Klasse in deinen Workspace:

```bash_copy
git clone ${class_repo} ~/class-quiz
```
<!-- /if -->
<!-- if: !class_repo -->
Lege zuerst den »Server« an: ein leeres Repository ohne Arbeitsordner
(`--bare`), wie es auf einem Git-Server liegt. Dann klonst du es, holst die
Dateien des Quiz hinein und gibst sie mit dem ersten Commit ab:

```bash_copy
mkdir -p ~/class-quiz-server
git init --bare -b main ~/class-quiz-server/quiz.git
git clone ~/class-quiz-server/quiz.git ~/class-quiz
cd ~/class-quiz
mkdir questions
for f in quiz.rb quiz.py rounds.txt TODO.md README.md questions/beispiel.txt; do wget -q -O $f ${web_root}/class-quiz/starter/$f; done
git add -A
git commit -m "Das Quiz"
git push -u origin HEAD
```

Ida klont denselben »Server« in ihren eigenen Ordner:

```bash_copy
git clone ~/class-quiz-server/quiz.git ~/class-quiz-ida
```
<!-- /if -->

_task(geklont)

Im Repository liegen:

<table class='table'>
<tr><th>Datei</th><th>Was darin steht</th></tr>
<tr><td><code>quiz.rb</code>, <code>quiz.py</code></td><td>das Quiz, in Ruby und in Python</td></tr>
<tr><td><code>questions/</code></td><td>eine Datei je Runde; bisher nur <code>beispiel.txt</code></td></tr>
<tr><td><code>rounds.txt</code></td><td>die Runden, eine pro Zeile, in der Reihenfolge, in der das Quiz sie spielt</td></tr>
<tr><td><code>TODO.md</code></td><td>was am Programm noch fehlt, und wer daran arbeitet</td></tr>
</table>

## Deine Runde

Sieh dir `questions/beispiel.txt` an. Eine Frage beginnt mit `? `, die Antwort
steht darunter mit `= `; gelten mehrere Antworten, trennt sie ein `|`. Zeilen
mit `#` sind Kommentare.

```text
# Eine Runde zum Ausprobieren
? Wie viele Bits hat ein Byte?
= 8 | acht
```

Leg in `questions/` eine Datei für deine Runde an, mit deinem Vornamen klein
und ohne Umlaute, etwa `questions/lena.txt`, und schreib mindestens drei
Fragen hinein, zu einem Thema, das du gut kennst.

<!-- language: ruby -->
Spiel deine Runde allein, bevor sie ins Quiz kommt:

```bash
cd ~/class-quiz
ruby quiz.rb questions/lena.txt
```

Steht etwas in der Datei, das das Quiz nicht versteht, sagt es, in welcher
Zeile.

Trag deine Datei dann in einer neuen Zeile **am Ende** von `rounds.txt` ein und
prüf das ganze Quiz:

```bash
ruby quiz.rb --check
```
<!-- /language -->
<!-- language: python -->
Spiel deine Runde allein, bevor sie ins Quiz kommt:

```bash
cd ~/class-quiz
python3 quiz.py questions/lena.txt
```

Steht etwas in der Datei, das das Quiz nicht versteht, sagt es, in welcher
Zeile.

Trag deine Datei dann in einer neuen Zeile **am Ende** von `rounds.txt` ein und
prüf das ganze Quiz:

```bash
python3 quiz.py --check
```
<!-- /language -->

_task(eigene-runde)

## Abgeben

Ein Commit sollte nur enthalten, was zu ihm gehört. Darum nimmst du die beiden
Dateien einzeln auf, statt mit `git add -A` alles, was sich irgendwo geändert
hat:

```bash
git add questions/lena.txt rounds.txt
git commit -m "Runde von Lena"
```

<!-- if: !class_repo -->
Bevor du pushst, ist Ida schneller. Tipp, was sie tut, in ihrem Ordner:

```bash_copy
cd ~/class-quiz-ida
printf '? Wie heißt die Hauptstadt von Frankreich?\n= Paris\n' > questions/ida.txt
echo questions/ida.txt >> rounds.txt
git add questions/ida.txt rounds.txt
git commit -m "Runde von Ida"
git push
cd ~/class-quiz
```
<!-- /if -->

_task(push-vorhersage)

<!-- after: push-vorhersage -->
Jetzt pushst du:

```bash
git push
```

<!-- if: class_repo -->
Geht der Push durch, war niemand schneller als du; bei den nächsten wird es
anders sein. Meist lehnt Git ihn ab:
<!-- /if -->
<!-- if: !class_repo -->
Git lehnt ihn ab:
<!-- /if -->

```text
 ! [rejected]        main -> main (fetch first)
error: failed to push some refs to '…'
hint: Updates were rejected because the remote contains work that you do not
hint: have locally.
```

Auf dem Server liegt ein Commit, den dein Klon noch nicht kennt. Dein Commit
baut auf dem Stand davor auf; nähme der Server ihn einfach an, wäre der andere
verloren. Darum holst du zuerst, was fehlt:

```bash
git pull --no-edit
```

`--no-edit` nimmt für den Merge die Nachricht, die Git vorschlägt, statt dafür
einen Editor zu öffnen.
<!-- /after -->

## Ein Konflikt

_task(konflikt-vorhersage)

<!-- after: konflikt-vorhersage -->
`git pull` führt die fremden Commits mit deinen zusammen (merge). Neue Dateien
in `questions/` sind kein Problem. Aber in `rounds.txt` habt ihr beide an
derselben Stelle eine Zeile angehängt, und Git weiß nicht, welche zuerst kommen
soll:

```text
CONFLICT (content): Merge conflict in rounds.txt
Automatic merge failed; fix conflicts and then commit the result.
```

In `rounds.txt` stehen jetzt beide Fassungen, eingerahmt von Markierungen:

```text
questions/beispiel.txt
<<<<<<< HEAD
questions/lena.txt
=======
questions/ida.txt
>>>>>>> 04a80e1422d4a1092423ad66fde49833572198e3
```

Oben steht deine Fassung (`HEAD`), unten die vom Server mit der Nummer ihres
Commits. Hier sollen beide
Runden ins Quiz, also bleiben beide Zeilen, und die drei Zeilen mit den
Markierungen kommen weg. Erst dann ist der Konflikt gelöst. Ein Programm, das
`rounds.txt` liest, käme mit den Markierungen nicht zurecht; das Quiz meldet sie
bei `--check`.
<!-- /after -->

So löst du ihn auf:

1. Öffne `rounds.txt`, lass beide Zeilen stehen und lösche die Zeilen mit
   `<<<<<<<`, `=======` und `>>>>>>>`. VS Code zeigt über dem Konflikt
   »Accept Both Changes«; das tut dasselbe.
2. Prüf das Quiz noch einmal mit `--check`.
3. Schließ den Merge ab und gib ihn ab:

```bash
git add rounds.txt
git commit -m "Runden von Lena und Ida"
git push
```

Lehnt Git den Push wieder ab, war in der Zwischenzeit noch jemand schneller:
noch einmal `git pull --no-edit`, dann `git push`.

_task(konflikt)

Ein Konflikt ist kein Fehler, sondern eine Frage, die Git nicht allein
beantworten kann: Welche Fassung soll gelten? Hier war die Antwort leicht.
Ändern zwei Leute dieselbe Zeile Code verschieden, müssen sie miteinander
reden.

_task(force)

## Die Arbeit verteilen

In `TODO.md` steht, was am Quiz noch fehlt. So eine Liste ist das einfachste
Werkzeug, um Arbeit im Team zu verteilen: Jede und jeder sieht, was offen ist,
wer woran arbeitet und was fertig ist.

### Eine Aufgabe übernehmen

Such dir eine Aufgabe aus, die noch niemand hat, und übernimm sie **sofort**:
Hol den neuesten Stand, schreib deinen Namen hinter ihr »Wer:«, und gib das
gleich ab, bevor du anfängst. So sehen die anderen, dass sie vergeben ist.

```bash
git pull --no-edit
git add TODO.md
git commit -m "Lena übernimmt: Punkte in Prozent"
git push
```

Wollten zwei dieselbe Aufgabe, gibt es beim zweiten einen Konflikt in
`TODO.md`. Dann redet ihr miteinander, wer sie macht.

<!-- if: !class_repo -->
Ohne Klasse gibt es nur dich und Ida: Übernimm eine Aufgabe für dich und lass
Ida eine andere übernehmen, in ihrem Ordner.
<!-- /if -->

### In einem eigenen Branch

Deine Aufgabe erledigst du in einem eigenen **Branch**: einem Zweig des
Repositorys, in dem du Commits machen kannst, ohne dass sie `main` ändern.
Was in `main` liegt, funktioniert so immer, auch während du arbeitest.

```bash
git switch -c prozent
```

<!-- language: ruby -->
Ändere `quiz.rb`, probier das Quiz aus, und committe, sobald ein Schritt
funktioniert, gern mehrmals:

```bash
ruby quiz.rb
git add quiz.rb
git commit -m "Punkte am Ende auch in Prozent"
```
<!-- /language -->
<!-- language: python -->
Ändere `quiz.py`, probier das Quiz aus, und committe, sobald ein Schritt
funktioniert, gern mehrmals:

```bash
python3 quiz.py
git add quiz.py
git commit -m "Punkte am Ende auch in Prozent"
```
<!-- /language -->

Ist die Aufgabe fertig, holst du den neuesten Stand von `main` und führst
deinen Branch dort hinein:

```bash
git switch main
git pull --no-edit
git merge --no-edit prozent
```

Hat jemand anderes dieselben Zeilen geändert, gibt es wieder einen Konflikt,
diesmal im Code. Dann stellst du eine Fassung her, die beides kann, probierst
sie aus und schließt den Merge mit `git add` und `git commit` ab. Zum Schluss
verschiebst du deine Zeile in `TODO.md` unter »Fertig«, committest das und
gibst alles ab:

```bash
git push
```

_task(verteilt)

### Einen Commit lesen

In einem Team liest man die Arbeit der anderen, bevor sie bleibt. Das heißt
**Review**. `git log --oneline` zeigt die Commits, `git show` einen davon mit
allem, was er geändert hat:

```bash
git log --oneline
git show a3f9c21
```

Statt `a3f9c21` nimmst du die Nummer eines Commits aus deiner Liste.

_task(review)

## Regeln fürs Team

Was ihr gerade erlebt habt, gilt in jedem Team, das mit Git arbeitet:

- **Erst holen, dann arbeiten:** `git pull`, bevor du anfängst.
- **Kleine Commits mit klaren Nachrichten:** Ein Commit tut eine Sache, und
  seine Nachricht sagt, welche.
- **Nur aufnehmen, was dazugehört:** `git add` mit Dateinamen statt `-A`.
- **Abgeben, wenn es funktioniert:** In `main` liegt nur, was läuft.
- **Nie `--force`** in einem gemeinsamen Repository.
- **Reden statt überschreiben:** Bei einem Konflikt im Code fragst du die
  Person, deren Fassung es ist.
