<div class='meta'>
image: git-logo.png
kind: reference
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Git und GitLab: Zugang und Veröffentlichen

<p class='abstract'>
Was du einmal einrichtest, damit dein Workspace bei GitLab Repositories klonen und deine Arbeit veröffentlichen kann, und was du tust, wenn Git einen Push ablehnt. Die Sandboxes für Datenstrukturen, Sortieren und die CPU binden diese Abschnitte ein.
</p>

Im Workspace ist `~` dein Ordner `/workspace`. `~/.ssh` ist also `/workspace/.ssh`.

## Einmal einrichten

<!-- part: ssh-schluessel -->
### SSH-Schlüssel für GitLab

Mit einem SSH-Schlüssel meldet sich dein Workspace beim GitLab-Server an, ohne dass du jedes Mal ein Passwort eingibst. Melde dich zuerst auf [git.nhcham.org](https://git.nhcham.org/) an. Prüfe dann im Terminal, ob du schon einen Schlüssel hast:

```bash
ls ~/.ssh/id_ed25519.pub
```

Erscheint `No such file or directory`, erzeugst du einen neuen. Bestätige den vorgeschlagenen Speicherort mit Enter; die Passphrase darfst du im Workspace leer lassen und noch zweimal Enter drücken:

```bash
ssh-keygen -t ed25519 -C "Vorname Nachname"
```

Zeige nun den **öffentlichen** Schlüssel an und kopiere die ganze Zeile, die mit `ssh-ed25519` beginnt:

```bash
cat ~/.ssh/id_ed25519.pub
```

In GitLab gehst du oben rechts auf dein Profilbild und dann auf **Edit profile → Access → SSH keys → Add new key**. Füge die Zeile bei **Key** ein, gib als Titel zum Beispiel `Workspace` an und speichere.

Die Datei `~/.ssh/id_ed25519` **ohne** `.pub` ist dein privater Schlüssel. Den kopierst du nie und lädst ihn nirgendwo hoch.

Teste die Verbindung. Beim ersten Mal fragt SSH nach, ob du dem Server vertraust; antworte mit `yes`. Danach begrüßt dich GitLab mit deinem Benutzernamen:

```bash
ssh -T git@git.nhcham.org
```

Meldet SSH `Permission denied (publickey)`, fehlt der Schlüssel in GitLab, oder du hast eine andere Zeile kopiert als die aus `id_ed25519.pub`.
<!-- /part -->

<!-- part: git-name -->
### Git sagen, wer du bist

Jeder Commit trägt einen Namen und eine E-Mail-Adresse. Trage deinen echten Namen und die E-Mail-Adresse deines GitLab-Kontos ein:

```bash
git config --global user.name "Vorname Nachname"
git config --global user.email "deine.mail@example.org"
```

Das ist in jedem Workspace nur einmal nötig. Ohne diese Angaben weigert sich Git zu committen.
<!-- /part -->

<!-- part: push-abgelehnt -->
## Wenn Git den Push ablehnt

In einem Klassen-Repository veröffentlichen viele. Hat jemand kurz vor dir gepusht, lehnt Git deinen Push mit `rejected` ab, weil dir dessen Commits fehlen. Hol sie zuerst und push dann noch einmal:

```bash
git pull
git push
```

Öffnet `git pull` einen Editor für eine Commit-Nachricht, speichere sie unverändert und schließe den Editor. Meldet Git einen **Konflikt**, hat jemand dieselbe Datei geändert wie du. Dann gilt: nichts löschen, nichts überschreiben, nach Hilfe fragen. Solange du nur Dateien in deinem eigenen Ordner änderst, passiert das nicht.
<!-- /part -->

## Zum Nachschlagen

- [Pro Git](https://git-scm.com/book/de/v2): das offizielle Buch zu Git, auf Deutsch. [Mit Remotes arbeiten](https://git-scm.com/book/de/v2/Git-Grundlagen-Mit-Remotes-arbeiten) erklärt `pull` und `push`, [Einfaches Branching und Merging](https://git-scm.com/book/de/v2/Git-Branching-Einfaches-Branching-und-Merging), was bei einem Konflikt passiert.
- [Git-Referenz](https://git-scm.com/docs): jeder Befehl mit allen Optionen, auf Englisch.
- [GitLab: SSH-Schlüssel](https://docs.gitlab.com/user/ssh/): Schlüssel anlegen, eintragen und prüfen, wenn die Verbindung nicht klappt, auf Englisch.
- [GitLab-Dokumentation](https://docs.gitlab.com/): alles zu Projekten, Gruppen und Rechten in GitLab.
