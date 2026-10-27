<div class='meta'>
image: git-logo.png
kind: reference
search: GitLab, Gitea, Forgejo, GitHub, SSH-Schlüssel, Token
</div>

<div
    class="autotoc-secondary-trigger"
    data-title="Auf dieser Seite"
    data-levels="h2,h3">
</div>

# Git-Server: Zugang und Veröffentlichen

<p class='abstract'>
Was du einmal einrichtest, damit dein Workspace Repositories von einem Git-Server wie GitLab, Gitea, Forgejo oder GitHub klonen und deine Arbeit dort veröffentlichen kann, und was du tust, wenn Git einen Push ablehnt. Die Sandboxes für Datenstrukturen, Sortieren und die CPU binden diese Abschnitte ein.
</p>

Im Workspace ist `~` dein Ordner `/workspace`. `~/.ssh` ist also `/workspace/.ssh`.

Wie sich dein Workspace beim Server anmeldet, verrät die Adresse des Repositorys: Beginnt sie mit `git@` oder `ssh://`, mit einem SSH-Schlüssel; beginnt sie mit `https://`, mit einem Zugangstoken.

## Einmal einrichten

<!-- part: ssh-schluessel -->
### SSH-Schlüssel

Mit einem SSH-Schlüssel meldet sich dein Workspace beim Git-Server an, ohne dass du jedes Mal ein Passwort eingibst. Melde dich zuerst im Browser auf [${git_host}](${git_web}) an${git_server_hint}. Prüfe dann im Terminal, ob du schon einen Schlüssel hast:

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

Trage ihn auf dem Git-Server ein. Wo das geht, hängt vom Server ab:

<table class='table'>
<tr><th>GitLab</th><td>Profilbild oben rechts → <strong>Edit profile</strong> → <strong>Access → SSH keys</strong> → <strong>Add new key</strong></td></tr>
<tr><th>Gitea, Forgejo</th><td>Profilbild oben rechts → <strong>Settings</strong> → <strong>SSH / GPG Keys</strong> → <strong>Add key</strong></td></tr>
<tr><th>GitHub</th><td>Profilbild oben rechts → <strong>Settings</strong> → <strong>SSH and GPG keys</strong> → <strong>New SSH key</strong></td></tr>
</table>

Füge die Zeile bei **Key** ein, gib als Titel zum Beispiel `Workspace` an und speichere.

Die Datei `~/.ssh/id_ed25519` **ohne** `.pub` ist dein privater Schlüssel. Den kopierst du nie und lädst ihn nirgendwo hoch.

Teste die Verbindung. Beim ersten Mal fragt SSH nach, ob du dem Server vertraust; antworte mit `yes`. Danach begrüßt dich der Server mit deinem Benutzernamen:

```bash
ssh -T ${git_ssh}
```

Meldet SSH `Permission denied (publickey)`, fehlt der Schlüssel auf dem Server, oder du hast eine andere Zeile kopiert als die aus `id_ed25519.pub`.
<!-- /part -->

<!-- feature: git_server -->
<!-- part: ssh-workspace -->
### SSH-Schlüssel für den Git-Server des Workspace

Euer Repository liegt auf dem Git-Server des Workspace, `git.workspace`. Ein Konto brauchst du dort nicht: Der Server erkennt dich an deinem SSH-Schlüssel, den du ihm einmal gibst. Prüfe zuerst im Terminal, ob du schon einen hast:

```bash
ls ~/.ssh/id_ed25519.pub
```

Erscheint `No such file or directory`, erzeugst du einen neuen. Bestätige den vorgeschlagenen Speicherort mit Enter; die Passphrase darfst du im Workspace leer lassen und noch zweimal Enter drücken:

```bash
ssh-keygen -t ed25519 -C "Vorname Nachname"
```

Ein Schlüssel besteht aus zwei Dateien. `~/.ssh/id_ed25519` ist der **private** Schlüssel: Er bleibt in deinem Workspace, und niemand bekommt ihn. `~/.ssh/id_ed25519.pub` ist der **öffentliche**: Mit ihm prüft der Server, dass du es bist. Zeige ihn an und kopiere die ganze Zeile, die mit `ssh-ed25519` beginnt:

```bash
cat ~/.ssh/id_ed25519.pub
```

Füge sie hier ein, so wie bei jedem Git-Server:

<div class='git-key' data-git-key></div>
<script src='/include/git_key.js'></script>

Teste dann die Verbindung:

```bash
ssh -T git@git.workspace
```

Beim ersten Mal fragt SSH, ob du dem Server vertraust, und zeigt seinen Fingerabdruck: `ED25519 key fingerprint is SHA256:…`. Vergleiche ihn mit dem im Kasten oben. Stimmen sie überein, antworte mit `yes`; so weißt du, dass am anderen Ende wirklich der Git-Server sitzt. Danach begrüßt er dich und nennt die Repositorys, die du benutzen darfst.

Meldet SSH `Permission denied (publickey)`, hast du den Schlüssel noch nicht eingefügt, eine andere Zeile kopiert als die aus `id_ed25519.pub`, oder danach einen neuen erzeugt; dann füg ihn noch einmal ein.
<!-- /part -->

<!-- part: ssh-workspace-auto -->
### Der Schlüssel für den Git-Server des Workspace

Euer Repository liegt auf dem Git-Server des Workspace, `git.workspace`. Er erkennt dich an einem SSH-Schlüssel, den deine Lehrkraft für deinen Workspace hat einrichten lassen. Er liegt in `~/.ssh/workspace_git`, und `~/.ssh/config` sagt SSH, dass es ihn für diesen Server nehmen soll.

<div class='git-key' data-git-key></div>
<script src='/include/git_key.js'></script>

Teste die Verbindung:

```bash
ssh -T git@git.workspace
```

Der Server begrüßt dich und nennt die Repositorys, die du benutzen darfst. Meldet SSH `Permission denied (publickey)`, kennt der Server deinen Schlüssel noch nicht; sag deiner Lehrkraft Bescheid.
<!-- /part -->
<!-- /feature -->

<!-- part: https-anmelden -->
### Anmelden über HTTPS

Beginnt die Adresse des Repositorys mit `https://`, fragt Git nach einem Benutzernamen und einem Passwort, spätestens beim ersten `git push`, im Terminal oder in einem Feld oben im Fenster. Der Benutzername ist dein Name auf dem Git-Server. Statt deines Passworts wollen die meisten Server ein **Zugangstoken**, eine lange Zeichenfolge, die du dort erzeugst; GitHub nimmt nur ein Token.

Melde dich im Browser auf [${git_host}](${git_web}) an${git_server_hint} und erzeuge ein Token, das in Repositories schreiben darf:

<table class='table'>
<tr><th>GitLab</th><td>Profilbild oben rechts → <strong>Edit profile</strong> → <strong>Access → Personal access tokens</strong> → <strong>Generate token</strong> (Legacy token), mit dem Scope <code>write_repository</code></td></tr>
<tr><th>Gitea, Forgejo</th><td>Profilbild oben rechts → <strong>Settings</strong> → <strong>Applications</strong> → <strong>Generate New Token</strong>, mit <code>repository</code>: Read and Write</td></tr>
<tr><th>GitHub</th><td>Profilbild oben rechts → <strong>Settings</strong> → <strong>Developer settings</strong> → <strong>Personal access tokens → Fine-grained tokens</strong> → <strong>Generate new token</strong>, mit <strong>Contents</strong>: Read and write</td></tr>
</table>

Kopiere das Token gleich: Der Server zeigt es nur einmal. Es gehört wie ein Passwort nur dir; schreib es in keine Datei im Repository.

Damit Git nicht bei jedem Push fragt, merkt es sich die Anmeldung acht Stunden lang im Arbeitsspeicher des Workspace:

```bash
git config --global credential.helper 'cache --timeout=28800'
```

Danach, und nachdem dein Workspace neu gestartet ist, fragt Git wieder nach dem Token.
<!-- /part -->

<!-- part: git-name -->
### Git sagen, wer du bist

Jeder Commit trägt einen Namen und eine E-Mail-Adresse. Trage deinen echten Namen und die E-Mail-Adresse deines Kontos auf dem Git-Server ein:

```bash
git config --global user.name "Vorname Nachname"
git config --global user.email "deine.mail@example.org"
git config --global pull.rebase false
```

Die dritte Zeile sagt `git pull`, was es tun soll, wenn andere inzwischen gepusht haben und du auch schon committet hast: die Commits zusammenführen (merge). Ohne sie bricht `git pull` dann mit `Need to specify how to reconcile divergent branches` ab.

Liegt das Repository auf dem Git-Server des Workspace (`git.workspace`), gibt es dort kein Konto; dann nimm eine Adresse, die du auch anderen zeigen magst, denn sie steht in jedem deiner Commits. Welche Adresse dein Konto auf einem anderen Git-Server hat, steht bei GitLab unter **Edit profile → Emails**, bei Gitea und Forgejo unter **Settings → Account**, bei GitHub unter **Settings → Emails** (wer dort die Adresse privat hält, nimmt die `noreply`-Adresse, die GitHub anzeigt).

Das ist in jedem Workspace nur einmal nötig. Ohne diese Angaben weigert sich Git zu committen.
<!-- /part -->

<!-- part: push-abgelehnt -->
## Wenn Git den Push ablehnt

In einem Klassen-Repository veröffentlichen viele. Hat jemand kurz vor dir gepusht, lehnt Git deinen Push mit `rejected` ab, weil dir dessen Commits fehlen. Hol sie zuerst und push dann noch einmal:

```bash
git pull
git push
```

Öffnet `git pull` einen Editor für eine Commit-Nachricht, speichere sie unverändert und schließe den Editor. Bricht `git pull` stattdessen mit `Need to specify how to reconcile divergent branches` ab, fehlt eine Einstellung; trag sie einmal ein und hol dann noch einmal:

```bash
git config --global pull.rebase false
git pull
```

Meldet Git einen **Konflikt**, hat jemand dieselbe Datei geändert wie du. Dann gilt: nichts löschen, nichts überschreiben, nach Hilfe fragen. Solange du nur Dateien in deinem eigenen Ordner änderst, passiert das nicht.
<!-- /part -->

## Zum Nachschlagen

- [Pro Git](https://git-scm.com/book/de/v2): das offizielle Buch zu Git, auf Deutsch. [Mit Remotes arbeiten](https://git-scm.com/book/de/v2/Git-Grundlagen-Mit-Remotes-arbeiten) erklärt `pull` und `push`, [Einfaches Branching und Merging](https://git-scm.com/book/de/v2/Git-Branching-Einfaches-Branching-und-Merging), was bei einem Konflikt passiert.
- [Git-Referenz](https://git-scm.com/docs): jeder Befehl mit allen Optionen, auf Englisch; [git-credential-cache](https://git-scm.com/docs/git-credential-cache) erklärt, wie Git sich eine Anmeldung merkt.
- GitLab, auf Englisch: [SSH-Schlüssel](https://docs.gitlab.com/user/ssh/), [Zugangstoken](https://docs.gitlab.com/user/profile/personal_access_tokens/) und ihre [Scopes](https://docs.gitlab.com/security/tokens/access_token_scopes/), [E-Mail-Adressen](https://docs.gitlab.com/user/profile/).
- Gitea und Forgejo, auf Englisch, in der Anleitung von Codeberg (einem Forgejo-Server): [SSH-Schlüssel](https://docs.codeberg.org/security/ssh-key/), [Zugangstoken](https://docs.codeberg.org/advanced/access-token/); die [Scopes eines Tokens](https://forgejo.org/docs/latest/user/authentication/token-scope/) bei Forgejo.
- GitHub, auf Englisch: [SSH-Schlüssel eintragen](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account), [Zugangstoken](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens), [E-Mail-Adresse für Commits](https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-email-preferences/setting-your-commit-email-address).
