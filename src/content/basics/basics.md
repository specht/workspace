<div class='meta'>
image: vs-code-welcome.webp:0:50
</div>

<style>
    .basics-icon {
        border-radius: 0.5em;
        background-color: #222;
        display: inline-block;

        svg {
            width: 3em;
            height: 3em;
            padding: 0.6em;
        }
        path {
            fill: #d8d8d8;
        }
    }
    html[data-bs-theme="dark"] {
        .basics-icon {
            border: 1px solid rgba(255, 255, 255, 0.15);
            background-color: rgba(0, 0, 0, 0.25);
        }
    }
</style>

# Die Basics

<p class='abstract'>
Eine kleine Tour durch die Benutzeroberfläche von Visual Studio Code, gefolgt von grundlegenden Hinweisen zur Arbeit mit Dateien, Ordnern und Projekten. Lerne wichtige Shortcuts und Funktionen zum Bearbeiten von Text kennen und erfahre, wie du mit Checkpoints sichere Zwischenstände deiner Arbeit anlegst.
</p>

Wenn du den Workspace öffnest, siehst du die Entwicklungsumgebung Visual Studio Code:

<!-- tutorial-screenshot
# Workspace ohne geöffneten Ordner
show-left-sidebar
-->

<img class='full' src='vs-code-welcome.webp' data alt=''>

## Die Benutzeroberfläche

Am linken Rand siehst du einige Icons, die wichtig sind:

<table class='table'>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M7.5 22.5H17.595C17.07 23.4 16.11 24 15 24H7.5C4.185 24 1.5 21.315 1.5 18V6C1.5 4.89 2.1 3.93 3 3.405V18C3 20.475 5.025 22.5 7.5 22.5ZM21 8.121V18C21 19.6545 19.6545 21 18 21H7.5C5.8455 21 4.5 19.6545 4.5 18V3C4.5 1.3455 5.8455 0 7.5 0H12.879C13.4715 0 14.0505 0.24 14.4705 0.6585L20.3415 6.5295C20.766 6.954 21 7.5195 21 8.121ZM13.5 6.75C13.5 7.164 13.8375 7.5 14.25 7.5H19.1895L13.5 1.8105V6.75ZM19.5 18V9H14.25C13.0095 9 12 7.9905 12 6.75V1.5H7.5C6.672 1.5 6 2.1735 6 3V18C6 18.8265 6.672 19.5 7.5 19.5H18C18.828 19.5 19.5 18.8265 19.5 18Z"/></svg></div>
</td>
<td>Explorer (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>E</kbd>) &ndash; hier siehst du alle Dateien und Unterordner des aktuell geöffneten Projekts</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M22.281 21.219L16.0875 15.0255C17.2815 13.5945 18 11.754 18 9.74854C18 5.19904 14.298 1.49854 9.75 1.49854C5.202 1.49854 1.5 5.20054 1.5 9.75004C1.5 14.2995 5.202 18 9.75 18C11.7555 18 13.5945 17.28 15.027 16.0875L21.2205 22.281C21.3675 22.428 21.5595 22.5 21.7515 22.5C21.9435 22.5 22.1355 22.4265 22.2825 22.281C22.575 21.9885 22.575 21.513 22.2825 21.2205L22.281 21.219ZM9.75 16.5C6.0285 16.5 3 13.4715 3 9.75004C3 6.02853 6.0285 3.00004 9.75 3.00004C13.4715 3.00004 16.5 6.02853 16.5 9.75004C16.5 13.4715 13.4715 16.5 9.75 16.5Z"/></svg></div></td>
<td>Search (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>F</kbd>) &ndash; hier kannst du in allen Dateien innerhalb deines Projektes suchen</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M21 8.25C21 6.1815 19.3185 4.5 17.25 4.5C15.1815 4.5 13.5 6.1815 13.5 8.25C13.5 10.023 14.739 11.5035 16.395 11.892C16.116 12.819 15.2655 13.5 14.25 13.5H9.75C8.9025 13.5 8.1285 13.7925 7.5 14.268V7.4235C9.21 7.0755 10.5 5.5605 10.5 3.75C10.5 1.6815 8.8185 0 6.75 0C4.6815 0 3 1.6815 3 3.75C3 5.562 4.29 7.0755 6 7.4235V16.575C4.29 16.923 3 18.438 3 20.2485C3 22.317 4.6815 23.9985 6.75 23.9985C8.8185 23.9985 10.5 22.317 10.5 20.2485C10.5 18.4755 9.261 16.995 7.605 16.6065C7.884 15.6795 8.7345 14.9985 9.75 14.9985H14.25C16.0845 14.9985 17.61 13.6725 17.931 11.9295C19.674 11.607 21 10.0845 21 8.25ZM4.5 3.75C4.5 2.5095 5.5095 1.5 6.75 1.5C7.9905 1.5 9 2.5095 9 3.75C9 4.9905 7.9905 6 6.75 6C5.5095 6 4.5 4.9905 4.5 3.75ZM9 20.25C9 21.4905 7.9905 22.5 6.75 22.5C5.5095 22.5 4.5 21.4905 4.5 20.25C4.5 19.0095 5.5095 18 6.75 18C7.9905 18 9 19.0095 9 20.25ZM17.25 10.5C16.0095 10.5 15 9.4905 15 8.25C15 7.0095 16.0095 6 17.25 6C18.4905 6 19.5 7.0095 19.5 8.25C19.5 9.4905 18.4905 10.5 17.25 10.5Z"/></svg></div></td>
<td>Source Control (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>G</kbd>) &ndash; hier geht es um Versionsverwaltung mit Git</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M19.854 13.9605L13.2105 17.697C12.954 17.22 12.5505 16.8345 12.039 16.641L12.054 16.626L19.1175 12.6525C19.6275 12.366 19.6275 11.6325 19.1175 11.3445L7.11751 4.59599C6.61801 4.31399 6.00001 4.67549 6.00001 5.24999V10.5C5.46901 10.5 4.97401 10.6215 4.50001 10.791V5.24999C4.50001 3.52949 6.35251 2.44499 7.85251 3.28949L19.8525 10.0395C21.381 10.899 21.381 13.101 19.8525 13.962L19.854 13.9605ZM10.5 16.0605V18H11.25C11.664 18 12 18.336 12 18.75C12 19.164 11.664 19.5 11.25 19.5H10.5C10.5 20.076 10.3905 20.625 10.1925 21.132L11.781 22.7205C12.0735 23.013 12.0735 23.4885 11.781 23.781C11.634 23.928 11.442 24 11.25 24C11.058 24 10.866 23.9265 10.719 23.781L9.39151 22.4535C8.56651 23.4 7.35151 24.0015 6.00001 24.0015C4.64851 24.0015 3.43351 23.4015 2.60851 22.4535L1.28101 23.781C1.13401 23.928 0.942009 24 0.750009 24C0.558009 24 0.366009 23.9265 0.219009 23.781C-0.0734912 23.4885 -0.0734912 23.013 0.219009 22.7205L1.80751 21.132C1.60951 20.625 1.50001 20.076 1.50001 19.5H0.750009C0.336009 19.5 8.78423e-06 19.164 8.78423e-06 18.75C8.78423e-06 18.336 0.336009 18 0.750009 18H1.50001V16.0605L0.219009 14.7795C-0.0734912 14.487 -0.0734912 14.0115 0.219009 13.719C0.511509 13.4265 0.987009 13.4265 1.27951 13.719L2.56051 15H3.00001C3.00001 13.3455 4.34551 12 6.00001 12C7.65451 12 9.00001 13.3455 9.00001 15H9.43951L10.7205 13.719C11.013 13.4265 11.4885 13.4265 11.781 13.719C12.0735 14.0115 12.0735 14.487 11.781 14.7795L10.5 16.0605ZM4.50001 15H7.50001C7.50001 14.172 6.82801 13.5 6.00001 13.5C5.17201 13.5 4.50001 14.172 4.50001 15ZM9.00001 16.5H3.00001V19.5C3.00001 21.1545 4.34551 22.5 6.00001 22.5C7.65451 22.5 9.00001 21.1545 9.00001 19.5V16.5Z"/></svg></div></td>
<td>Run and Debug (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>D</kbd>) &ndash; hier können selbst geschriebene Programme ausgeführt werden</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M23 7.21878C23 6.63842 22.7741 6.09232 22.3644 5.68323L18.3008 1.61583C17.4814 0.794722 16.0519 0.794722 15.2325 1.61583L12 4.85135V4.64974C12 3.4352 11.0134 2.44771 9.8 2.44771H3.2C1.98658 2.44771 1 3.4352 1 4.64974V20.798C1 22.0125 1.98658 23 3.2 23H19.3333C20.5468 23 21.5333 22.0125 21.5333 20.798V14.1919C21.5333 12.9773 20.5468 11.9898 19.3333 11.9898H19.1319L22.3644 8.75531C22.7741 8.34524 23 7.80012 23 7.21878ZM3.2 3.91573H9.8C10.2038 3.91573 10.5333 4.24457 10.5333 4.64974V11.9898H2.46667V4.64974C2.46667 4.24457 2.79618 3.91573 3.2 3.91573ZM2.46667 20.798V13.4579H10.5333V21.532H3.2C2.79618 21.532 2.46667 21.2022 2.46667 20.798ZM20.0667 14.1919V20.798C20.0667 21.2022 19.7372 21.532 19.3333 21.532H12V13.4579H19.3333C19.7372 13.4579 20.0667 13.7867 20.0667 14.1919ZM12 11.9898V9.58523L14.4024 11.9898H12ZM21.327 7.71595L17.2634 11.7833C16.9974 12.0495 16.5359 12.0495 16.269 11.7833L12.2053 7.71595C12.0724 7.58383 11.999 7.40669 11.999 7.21878C11.999 7.03087 12.0724 6.85471 12.2053 6.72161L16.269 2.65421C16.402 2.52111 16.578 2.44771 16.7657 2.44771C16.9534 2.44771 17.1294 2.52111 17.2624 2.65421L21.326 6.72161C21.459 6.85373 21.5324 7.03087 21.5324 7.21878C21.5324 7.40669 21.46 7.58285 21.327 7.71595Z"/></svg></div></td>
<td>Extensions (<kbd>Strg</kbd><kbd>Shift</kbd><kbd>X</kbd>) &ndash; hier findest du viele nützliche Erweiterungen</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="24" height="24" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M 4 5.25 C 2.4901005 5.2500001 1.2500001 6.4901005 1.25 8 L 1.25 16 C 1.25 17.5099 2.4901003 18.75 4 18.75 L 20 18.75 C 21.509899 18.749999 22.75 17.509899 22.75 16 L 22.75 8 C 22.75 6.4901004 21.5099 5.25 20 5.25 L 4 5.25 z M 4 6.75 L 20 6.75 C 20.699239 6.75 21.25 7.3007606 21.25 8 L 21.25 16 C 21.25 16.699239 20.699239 17.25 20 17.25 L 4 17.25 C 3.3007605 17.25 2.75 16.699239 2.75 16 L 2.75 8 C 2.75 7.3007606 3.3007606 6.75 4 6.75 z M 6 9.25 A 0.75 0.75 0 0 0 5.25 10 L 5.25 10.009766 A 0.75 0.75 0 0 0 6 10.759766 A 0.75 0.75 0 0 0 6.75 10.009766 L 6.75 10 A 0.75 0.75 0 0 0 6 9.25 z M 10 9.25 A 0.75 0.75 0 0 0 9.25 10 L 9.25 10.009766 A 0.75 0.75 0 0 0 10 10.759766 A 0.75 0.75 0 0 0 10.75 10.009766 L 10.75 10 A 0.75 0.75 0 0 0 10 9.25 z M 14 9.25 A 0.75 0.75 0 0 0 13.25 10 L 13.25 10.011719 A 0.75 0.75 0 0 0 14 10.759766 A 0.75 0.75 0 0 0 14.75 10.009766 L 14.75 10 A 0.75 0.75 0 0 0 14 9.25 z M 18 9.25 A 0.75 0.75 0 0 0 17.25 10 L 17.25 10.011719 A 0.75 0.75 0 0 0 18 10.759766 A 0.75 0.75 0 0 0 18.75 10.009766 L 18.75 10 A 0.75 0.75 0 0 0 18 9.25 z M 6 13.25 A 0.75 0.75 0 0 0 5.25 14 L 5.25 14.009766 A 0.75 0.75 0 0 0 6 14.759766 A 0.75 0.75 0 0 0 6.75 14.009766 L 6.75 14 A 0.75 0.75 0 0 0 6 13.25 z M 10.001953 13.25 A 0.75 0.75 0 0 0 9.25 13.998047 A 0.75 0.75 0 0 0 9.9980469 14.75 L 13.998047 14.759766 A 0.75 0.75 0 0 0 14.75 14.011719 A 0.75 0.75 0 0 0 14.001953 13.259766 L 10.001953 13.25 z M 18 13.25 A 0.75 0.75 0 0 0 17.25 14 L 17.25 14.009766 A 0.75 0.75 0 0 0 18 14.759766 A 0.75 0.75 0 0 0 18.75 14.009766 L 18.75 14 A 0.75 0.75 0 0 0 18 13.25 z " /></svg></div></td>
<td>Hackschule Keyboard-Tutorial &ndash; hier lernst du die wichtigsten Funktionen der Tastatur kennen</td>
</tr>
</table>

Rechts oben siehst du mehrere Buttons, mit denen du das Layout anpassen oder verschiedene Bereiche ein- und ausblenden kannst:

<table class='table'>
<tr>
<td><div class='basics-icon'><svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path fill-rule="evenodd" clip-rule="evenodd" d="M5.5 1C6.327 1 7 1.673 7 2.5V13.5C7 14.327 6.327 15 5.5 15H2.5C1.673 15 1 14.327 1 13.5V2.5C1 1.673 1.673 1 2.5 1H5.5ZM2.5 2C2.225 2 2 2.225 2 2.5V13.5C2 13.775 2.225 14 2.5 14H5.5C5.775 14 6 13.775 6 13.5V2.5C6 2.225 5.775 2 5.5 2H2.5Z"/><path fill-rule="evenodd" clip-rule="evenodd" d="M13.5 9C14.327 9 15 9.673 15 10.5V13.5C15 14.327 14.327 15 13.5 15H10.5C9.673 15 9 14.327 9 13.5V10.5C9 9.673 9.673 9 10.5 9H13.5ZM10.5 10C10.225 10 10 10.225 10 10.5V13.5C10 13.775 10.225 14 10.5 14H13.5C13.775 14 14 13.775 14 13.5V10.5C14 10.225 13.775 10 13.5 10H10.5Z"/><path fill-rule="evenodd" clip-rule="evenodd" d="M13.5 1C14.327 1 15 1.673 15 2.5V5.5C15 6.327 14.327 7 13.5 7H10.5C9.673 7 9 6.327 9 5.5V2.5C9 1.673 9.673 1 10.5 1H13.5ZM10.5 2C10.225 2 10 2.225 10 2.5V5.5C10 5.775 10.225 6 10.5 6H13.5C13.775 6 14 5.775 14 5.5V2.5C14 2.225 13.775 2 13.5 2H10.5Z"/></svg></div></td>
<td>Layout anpassen</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M12.5 1C13.881 1 15 2.119 15 3.5V12.5C15 13.881 13.881 15 12.5 15H3.5C2.119 15 1 13.881 1 12.5V3.5C1 2.119 2.119 1 3.5 1H12.5ZM12.5 14C13.328 14 14 13.328 14 12.5V3.5C14 2.672 13.328 2 12.5 2H7V14H12.5Z"/></svg></div></td>
<td>Linke Seitenleiste (<kbd>Strg</kbd><kbd>B</kbd>)</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M15 12.5C15 13.881 13.881 15 12.5 15H3.5C2.119 15 1 13.881 1 12.5V3.5C1 2.119 2.119 1 3.5 1H12.5C13.881 1 15 2.119 15 3.5V12.5ZM2 10H14V3.5C14 2.672 13.328 2 12.5 2H3.5C2.672 2 2 2.672 2 3.5V10Z"/></svg></div></td>
<td>Panel (<kbd>Strg</kbd><kbd>J</kbd>) &ndash; hier ist für uns vor allem das Terminal interessant</td>
</tr>
<tr>
<td><div class='basics-icon'><svg width="16" height="16" viewBox="0 0 16 16" xmlns="http://www.w3.org/2000/svg" fill="currentColor"><path d="M12.5 1C13.881 1 15 2.119 15 3.5V12.5C15 13.881 13.881 15 12.5 15H3.5C2.119 15 1 13.881 1 12.5V3.5C1 2.119 2.119 1 3.5 1H12.5ZM9 14V2H3.5C2.672 2 2 2.672 2 3.5V12.5C2 13.328 2.672 14 3.5 14H9Z"/></svg></div></td>
<td>Rechte Seitenleiste (<kbd>Strg</kbd><kbd>Alt</kbd><kbd>B</kbd>)</td>
</tr>
</table>

## Dateien, Projekte, Verzeichnisse und Pfade

<img src='directory-tree.webp' class='r' style='width: 15em;' alt=''>

Wenn du am Computer arbeitest, speicherst du deine Arbeit in Dateien. Jede Datei hat einen Dateinamen, der normalerweise auch eine Erweiterung wie `.jpg`, `.html`, `.py` oder `.rb` enthält. Die Dateiendung ist ein Teil des Namens und zeigt oft, um welche Art von Datei es sich handelt.

Ein Projekt besteht normalerweise aus einem Ordner und den darin enthaltenen Dateien und Unterordnern. Im Explorer wird diese Struktur als Verzeichnisbaum dargestellt. Die Abbildung rechts zeigt ein Beispiel mit mehreren Dateien und Unterordnern.

Der Pfad `pictures/monkey.jpg` beschreibt, wo sich die Datei `monkey.jpg` innerhalb eines Projektordners befindet. Der vollständige Pfad zu einer Datei setzt sich aus allen übergeordneten Verzeichnissen und dem Dateinamen zusammen, zum Beispiel:

`/workspace/shower.js/pictures/monkey.jpg`

Unter Linux ist das oberste Verzeichnis immer `/`, unter Windows beginnt ein Pfad meistens mit einem Laufwerksbuchstaben wie `C:\`. Außerdem verwendet Windows normalerweise `\` statt `/`, um Verzeichnisse voneinander zu trennen.

Im Hackschule Workspace legst du deine Projekte und Dateien im Verzeichnis `/workspace` ab. Dort kannst du dir eine beliebige Ordnerstruktur anlegen. Deine Dateien bleiben gespeichert, wenn du den Browser schließt, und du kannst später von zu Hause oder von einem anderen Gerät aus weiterarbeiten.

## Checkpoints: Zwischenstände sichern

Mit der vorinstallierten Erweiterung **Checkpoints** kannst du einen Zwischenstand deines Projekts festhalten. Das ist besonders nützlich, bevor du eine größere Änderung ausprobierst.

Ein guter Zeitpunkt für einen Checkpoint ist zum Beispiel:

- bevor du funktionierenden Code grundlegend umbaust,
- bevor du mehrere Dateien gleichzeitig änderst,
- nachdem du eine wichtige Teilaufgabe abgeschlossen hast,
- oder bevor du eine neue Idee ausprobierst, bei der du noch nicht weißt, ob sie funktioniert.

Gib einem Checkpoint einen kurzen, verständlichen Namen, zum Beispiel:

```text
Navigation funktioniert
```

oder:

```text
Vor dem Umbau des Menüs
```

Wenn später etwas schiefgeht, kannst du zu einem früheren Zwischenstand zurückkehren.

Um einen Checkpoint zu erstellen, wechsle links in den Explorer und klappe den Bereich **Checkpoints** auf. Klicke auf das Plus-Symbol, um einen neuen Checkpoint zu erstellen.

<img src='create-checkpoint.webp' class='full' alt=''>

Du kannst einen Namen für den Checkpoint eingeben und mit <kbd>Enter</kbd> bestätigen.

<img src='enter-checkpoint-name.webp' class='full' alt=''>

Der Checkpoint wird dann gespeichert:

<img src='checkpoint-created.webp' class='full' alt=''>

Falls du später zu einem Checkpoint zurückkehren möchtest, klicke auf den entsprechenden Eintrag in der Liste:

<img src='restore-checkpoint.webp' class='full' alt=''>


<div class='hint'>
<p>
Intern verwendet die Checkpoints-Erweiterung Git, um die Zwischenstände zu speichern, ohne dass du dich um die Details kümmern musst. Du kannst Checkpoints jederzeit wieder löschen, wenn du sie nicht mehr brauchst. Die gespeicherten Checkpoints werden separat von eventuell vorhandenen Commits in einem Git-Repository verwaltet.
</p>
</div>

Wenn du sicher im Umgang mit Git bist, kannst du auch direkt Git verwenden, um deine Arbeit zu sichern.

## Tastatur und Textbearbeitung

Gerade am Anfang kann es sein, dass du beim Tippen, Markieren und Navigieren in einer Datei noch relativ langsam bist. Das ist völlig normal: Mit der Zeit kommt die Übung. 🤠 Wenn du etwas Erfahrung gesammelt hast, bist du mit der Tastatur oft schneller, als wenn du für jeden Arbeitsschritt erst zur Maus greifen musst.

### Das Keyboard-Tutorial starten

Im Hackschule Workspace ist das **Hackschule Keyboard-Tutorial** bereits installiert. Dort kannst du die wichtigsten Funktionen der Tastatur direkt in Visual Studio Code ausprobieren. Das Tutorial führt dich Schritt für Schritt durch Themen wie:

- Navigation in einem Dokument,
- Markieren und Bearbeiten von Text,
- Suchen und Ersetzen,
- Arbeiten mit mehreren Cursorpositionen,
- Bearbeiten von Code,
- sowie den Umgang mit Dateien, Ordnern und Tabs.

Klicke dazu links in der Aktivitätsleiste auf das Tastatur-Symbol. Anschließend öffnet sich der Bereich mit dem Keyboard-Tutorial:

<img class='full' src='keyboard-tutorial-pane.webp' alt=''>

Beginne mit dem ersten Kapitel und arbeite die Übungen der Reihe nach durch. Das Tutorial erkennt viele deiner Arbeitsschritte automatisch und markiert erledigte Aufgaben mit einem grünen Haken. Deinen Fortschritt speichert es, sodass du später an derselben Stelle weitermachen kannst.

Wenn du die Übungen im Keyboard-Tutorial abgeschlossen hast, kannst du außerdem Webseiten wie [keybr.com](https://www.keybr.com/) nutzen, um deine Tippgeschwindigkeit zu trainieren.
