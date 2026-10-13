<div class='meta'>
image: cpu-sandbox.webp
kind: projekt
topic: linux
reference: [gitlab]
template: https://github.com/specht/cpu-sandbox.git
class_repo: git@git.nhcham.org:specht/cpu-sandbox-programs-2026.git
template_name: der CPU Sandbox
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2">
</div>

# Mit der CPU Sandbox arbeiten

<p class='abstract'>
In der CPU Sandbox programmierst du einen kleinen 8-Bit-Prozessor in Assembler oder C und siehst bei jedem Schritt, was im Prozessor passiert. Diese Seite begleitet dich beim Einrichten und zeigt dir, ob deine Programme wirklich bei deiner Lehrkraft ankommen.
</p>

<img class='full' src='cpu-sandbox.webp' alt='CPU Sandbox mit laufendem 3D-Raycaster, dem C-Quelltext mit Maschinenbytes, Registern, Geräten und RAM.'>

Du arbeitest mit **zwei Git-Repositories**. Im Workspace ist `~` dein Ordner `/workspace`.

<table class='table'>
<tr><th>Ordner</th><th>Inhalt</th><th>Wer ändert es?</th></tr>
<tr><td><code>~/cpu-sandbox/</code></td><td>die App mit den Beispielen</td><td>nur deine Lehrkraft</td></tr>
<tr><td><code>~/cpu-sandbox/programs/</code></td><td>das Klassen-Repository mit euren Programmen</td><td>ihr, jede und jeder im eigenen Ordner</td></tr>
</table>

Die Karten auf dieser Seite schauen in deinem Workspace nach und sagen dir, was sie finden. Sie ändern nichts.

## Einmalig: Zugang zu GitLab

_include(gitlab#ssh-schluessel)

_task(ssh-schluessel)

_include(gitlab#git-name)

## Einmalig: Klonen

```bash
cd ~
git clone ${template}
cd cpu-sandbox
git clone ${class_repo} programs
```

Das Klassen-Repository muss **in** der App liegen und genau `programs` heißen. Gibt es `programs/` schon, klone es nicht noch einmal und lösche nichts, sondern frag nach.

_task(geklont)

_task(klassen-repository)

## Arbeiten

Starte die App in einem Terminal und lass es offen. Für Git öffnest du ein zweites Terminal.

```bash
cd ~/cpu-sandbox
./run
```

Ein neues Programm legst du in der App mit **+** neben der Programmauswahl an. Gib als **Student folder** deinen Ordnernamen an, zum Beispiel `anna`, dann einen Dateinamen, die Sprache und die CPU. Das Programm entsteht in `programs/DEIN_NAME/`. Wie Aufgaben, **Check** und **Class progress** funktionieren, steht in der [Anleitung der App](https://github.com/specht/cpu-sandbox#readme).

## Veröffentlichen

Deine Lehrkraft sieht deine Programme erst, wenn sie committet **und** gepusht sind. Das machst du im Ordner `programs`, nicht im Ordner der App:

```bash
cd ~/cpu-sandbox/programs
git pull
git add DEIN_NAME/
git commit -m "Aufgabe add von DEIN_NAME"
git push
```

_task(nur-dein-ordner)

_task(veroeffentlicht)

_include(gitlab#push-abgelehnt)

## Beim nächsten Mal

```bash
cd ~/cpu-sandbox && git pull
cd programs && git pull
cd .. && ./run
```

Und am Ende der Stunde: Aufgabe 5 noch einmal prüfen.
