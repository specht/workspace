# Lehrkräfte, Gruppen und Einladungen

Die Einladungslisten liegen unter `data/invitations/*.txt` (im Container:
`/invitations/*.txt`). Die Dateien werden in alphabetischer Reihenfolge
gelesen.

```text
> Klasse 12
Lena Muster <lena@example.org>
Tom Beispiel <tom@example.org>
+ lehrer@example.org

> LK Informatik
lena@example.org
Mia Test <mia@example.org>
+ lehrer@example.org

> Lehrkräfte
Dr. Erika Beispiel <beispiel@example.org>
Lea Lehrerin <lea@example.org>
* lea@example.org
```

- `> Name` beginnt eine Gruppe.
- Normale Zeilen laden eine Person ein und machen sie zum Mitglied der Gruppe.
  Eine Person darf in mehreren Gruppen stehen (Klasse 12 und LK). Ihr Name
  muss nur in einer Zeile stehen; eine Zeile mit nur der Adresse behält ihn.
- `+ email` macht die Person zur Lehrkraft, die diese Gruppe betreut. Sie
  muss außerdem irgendwo als normale Zeile eingeladen sein, sonst kann sie
  sich nicht anmelden.
- `* email` kennzeichnet eine eingeladene Person als Lehrkraft ohne Gruppe:
  Sie sieht die Notizen für Lehrkräfte, betreut aber niemanden.
- `ADMIN_USERS` in `credentials.rb` sind Administratorinnen und
  Administratoren: Sie sehen alles.

## Einladungen importieren

Bis zum Import sind die Dateien die Einladungslisten, wie oben beschrieben.
Danach stehen Gruppen, Einladungen und Schulen in Neo4j
(`include/roster_store.rb`); Lehrkräfte verwalten ihre Gruppen dann im
Browser. Dateien, deren Name mit `_` beginnt (`_e2e.txt`,
`_tutorial_screenshots.txt`), schreiben Werkzeuge; sie werden weiter gelesen.

So geht es auf einer laufenden Installation:

1. Eine Kopie von `/invitations` auf den eigenen Rechner holen und prüfen:
   `src/scripts/check-invitation-import.rb ORDNER`. Das Skript liest die
   Dateien mit dem alten und dem neuen Leser und prüft, dass die Datensätze
   des Imports dasselbe ergeben: wer eingeladen ist, mit welchem Namen, in
   welcher Gruppe, wer was betreut, wer Lehrkraft ist. Die Reihenfolge der
   Gruppen zählt nicht. `INVITATIONS_DIR=ORDNER ruby
   src/ruby/test/roster_test.rb` macht dasselbe als Test.
2. Unter »Administration › Einladungslisten« die Vorschau ansehen: Dateien,
   Gruppen mit der Zahl ihrer Mitglieder, Einladungen, Lehrkräfte.
3. »Einladungslisten importieren«. Der Import schreibt in einer Transaktion,
   liest zurück und vergleicht mit den Dateien; bei einer Abweichung wird
   nichts übernommen, und die Abweichungen stehen auf der Seite. Die Schule
   der Installation (`OPERATOR_SCHOOL`) wird angelegt, aber nicht als
   zugestimmt: Das ist sie erst, wenn unter `/personen` beide Daten stehen.
4. Der Import ist endgültig: Einen Weg zurück zu den Dateien gibt es nicht,
   nur die Sicherung von Neo4j. Die Dateien bleiben liegen, werden aber
   nicht mehr gelesen. Auf einer Kopie (Entwicklung) lässt sich der Import
   proben, indem man die Datenbank leert und den Dump neu einspielt
   (docs/entwicklung.md, »Datenbanken«).

Ein Konto (`User`) entsteht, sobald jemand zum ersten Mal einen Anmelde-Code
anfordert (`ensure_user_account` in `/api/request_login`), nicht erst mit
der Anmeldung; wer den Code nie eingibt, erscheint unter `/personen` als
»nicht bekannt«. Eine Einladung ist
kein Konto.

Nach der Anmeldung (mit Code oder QR-Code) geht es dorthin zurück, wo die
Person war: `/login?next=/rle#aufgabe-von-hand`. So verlinken »Melde dich
an« auf einer Aufgabenkarte und »Anmelden« in der Navigation (`code.js`),
und so leitet eine Seite nur für Angemeldete (`/profil`, `/klasse` …) zur
Anmeldung weiter. `next` gilt nur als Pfad auf dieser Seite
(`Authentication.safe_next_path`), alles andere führt zur Startseite. Eine
Anmeldung gilt ein Jahr (`SESSION_LIFETIME_DAYS`); »Abmelden« steht in der
Navigation des Profils (`/profil`) neben dem Namen, auf den anderen Seiten
nicht. Dort meldet man sich auch in anderen Browsern ab.

## Zustimmung der Schule, Einverständnis, ungeklärt

Wer eingeladen ist, kann sich anmelden; ob die Schule zugestimmt hat,
sperrt während der Übergangszeit niemanden (Entscheidung des Betreibers:
bis zum 12. April 2027 werden die Unterlagen aller Schulen geklärt, der
Unterricht bleibt, wie er ist; datenschutz.md, »Die Übergangszeit«, und
handoff-datenschutz.md). Die Seiten zeigen, was offen ist:

1. Die Schule hat zugestimmt: Die Lehrkraft schickt der Administration die
   unterschriebene Entscheidung der Schulleitung und die Vereinbarung zur
   Auftragsverarbeitung; die Administration trägt beide Daten unter
   `/personen` ein. Dann ist für die ganze Schule alles geklärt.
2. Bis dahin: Liegt für eine Schülerin oder einen Schüler ein schriftliches
   Einverständnis vor (Elternbrief mit Einverständniserklärung,
   `/datenschutz/elternbrief?einverstaendnis=1`), bewahrt die Lehrkraft es
   auf und trägt das Datum unter `/gruppen` ein (`consent_at`, `consent_by`
   an der Einladung). Dafür ist sie verantwortlich; die Administration hat
   damit nichts zu tun. Es trägt eine freiwillige Nutzung, nicht den
   Unterricht: Dort ist die Entscheidung der Schule der Weg.
3. Weder noch: während der Übergangszeit geduldet, aber sichtbar
   (»ungeklärt« auf `/personen`, »Einverständnis für x von y« an der
   Gruppe), damit es geklärt wird. Wie weit das Gespräch mit der Schule ist,
   hält die Administration unter `/personen` › Schulen › »Übergang« fest;
   das ändert nichts an der Zustimmung oder am Zugang.

Ein ausdrücklicher Widerspruch oder der Widerruf einer Schule ist etwas
anderes als fehlende Unterlagen: Er wird sofort bearbeitet (Sperre unter
`/personen`), nicht erst zum Stichtag.

## Gruppen im Browser (Klassenseite und `/gruppen`)

Nach dem Import verwaltet jede Lehrkraft ihre Gruppen selbst, auf der
Klassenseite: »Neue Gruppe« neben der Auswahl der Gruppe, und im Reiter
»Lernende« die Gruppe als Karte (`static/include/groups.js`, dieselbe wie
auf `/gruppen`), jede Zeile mit dem Workspace der Person (`class_board.js`
verschiebt die Zellen des Servers hinein). `/gruppen` ist die Übersicht
der Administration über alle Gruppen; Lehrkräfte landen von dort im Reiter
»Lernende«. Die Routen (`include/groups_page.rb`):

- Eine Gruppe anlegen; sie gehört zur Schule der Lehrkraft. Namen müssen
  frei sein; `Administrator`, `Lehrkräfte`, die Testgruppen und Gruppen aus
  den `_`-Dateien sind reserviert.
- Adressen eintragen, eine pro Zeile, mit oder ohne Namen
  (`Roster.parse_member_lines`, auch aus einer Tabelle kopiert); ein Name,
  der später kommt, wird ergänzt, ein vorhandener nicht überschrieben.
- Lehrkräfte derselben Schule in eine Gruppe aufnehmen; eine andere Schule
  wird abgelehnt. Nur die Administration ordnet Lehrkräfte einer Schule zu.
- Willkommensmails an die Mitglieder, die noch keine bekommen haben
  (`MailTexts.welcome_student`); Antworten gehen an die Lehrkraft
  (`welcomed_at` an der Einladung). Begrenzt auf 600 je Lehrkraft und Stunde.
- Die Gruppe löschen (Name zur Bestätigung). Wer danach in keiner Gruppe
  mehr steht und keine Lehrkraft ist, ist nicht mehr eingeladen; ein Konto
  löscht das nicht (»Personen und Schulen › Aufräumen«). Mit der Gruppe
  gehen ihre Einstellungen (`GroupSetting`: Sprache, Klassen-Repositorys)
  und ihre angehefteten Aktivitäten (`Pin`); eine neue Gruppe gleichen
  Namens beginnt leer. Ebenso, wenn eine Gruppe beim erneuten Lesen der
  Einladungsdateien fehlt, und bevor eine Gruppe im Browser angelegt wird,
  fallen Reste unter ihrem Namen weg; der Import der Dateien behält sie für
  die importierten Gruppen, Testgruppen und Werkzeug-Gruppen und löscht die
  übrigen (`include/group_data.rb`).
- Der Link zu den Unterlagen für die eigene Schule (datenschutz.md,
  »Partnerschulen«): auf der Klassenseite im Reiter »Einstellungen« unter
  »Schule« (`class_school_html` in `include/class_page.rb`), mit der Schule
  der Gruppe und ob sie zugestimmt hat; bei einer Schule, die noch nicht
  zugestimmt hat, auch der Elternbrief mit Einverständniserklärung. Gruppen
  aus den Einladungsdateien zeigen dort nichts.
- Was sich hier nicht tun lässt, sagt die Seite statt einer Lücke: Einer
  Lehrkraft ohne Schule steht an der Stelle von »Neue Gruppe«, dass die
  Administration die Schule zuordnet und an wen sie schreibt
  (`new_group_note_html`); eine Gruppe aus den Einladungsdateien (vor dem
  Import alle, in der Entwicklung die Werkzeug-Gruppen) zeigt im Reiter
  »Lernende«, dass die Administration sie pflegt (`class_board.js`), statt
  die Karte wortlos wegzulassen.

Administratorinnen und Administratoren sehen alle Gruppen und wählen beim
Anlegen die Schule, auch »Gäste«. Sie finden `/gruppen` und `/personen` im
Menü »Administration«; Lehrkräfte ohne Administration haben einen eigenen
Knopf »Gruppen«. Alle Routen unter `/api/groups/*` verlangen eine Lehrkraft
oder die Administration (`teacher_or_admin!`, sonst 403) und prüfen danach
die Gruppe (`managed_group!`) oder die Person (`not_your_student`); die unter
`/api/people/*` nur die Administration (`admin_only!`, 403).

- Jede Gruppe ist eine geschlossene Zeile (Mitglieder, mit Konto,
  Einverständnis, Lehrkräfte; für die Administration zuletzt da und
  Speicher); ein Klick öffnet sie. Gruppen stehen unter ihrer Schule, die
  eigene zuerst, Gäste zuletzt; ein Suchfeld findet Gruppen, Namen und
  Adressen.
- Umbenennen (Lehrkräfte der Gruppe und Administration): Der Name ist auch
  Schlüssel der angehefteten Aktivitäten (`Pin`), der Programmiersprache
  und der Klassen-Repositorys der Gruppe (`GroupSetting`) und der Auswahl auf der Klassenseite
  (`User.class_prefs`); alles zieht in einer Transaktion mit
  (`/api/groups/rename`).
- Hinzufügen mit Vorschlägen beim Tippen, nach Name oder Adresse
  (`/api/groups/suggest`): für Lehrkräfte nur Eingeladene aus Gruppen ihrer
  Schule und Lehrkräfte ihrer Schule, für die Administration alle. Eine
  Schülerin oder ein Schüler, die schon in einer Gruppe einer anderen Schule
  stehen, werden nicht aufgenommen (wie bei »Schule wechseln«); Gruppen der
  Gäste zählen dabei nicht als andere Schule, dort dürfen dieselben Personen
  auch stehen (Workshops, Firmen, Veranstaltungen).
- Namen lassen sich in der Tabelle ändern, auch unter `/personen`.

**Schule wechseln** (nur Administration, an jeder Gruppe): für Gruppen, die
beim Import unter der Schule der Installation gelandet sind, aber zu einer
anderen gehören. Die Zielschule muss es unter `/personen` schon geben. Eine
Vorschau zeigt beide Schulen mit ihrem Stand, die Zahl der Mitglieder und
was zu entscheiden ist; erst die Bestätigung ändert etwas, und dann nur die
Schule der Gruppe (`moved_from`, `moved_at`, `moved_by` an der Gruppe, eine
Zeile im Log). Mitglieder, Namen, Konten, Workspaces, Aufgaben und Abgaben
bleiben.

- Lehrkräfte der Gruppe, die zu einer anderen Schule gehören: entweder mit
  zur Zielschule nehmen (geht nur, wenn sie keine Gruppe einer anderen
  Schule betreuen) oder aus der Gruppe nehmen. Ihre Mitgliedschaften in
  anderen Gruppen (etwa einem Informatiktag) bleiben. Die Administration
  bleibt, wie sie ist.
- Steht eine Schülerin oder ein Schüler auch in einer Gruppe einer anderen
  Schule, verweigert der Server den Wechsel: Eine Lehrkraft der Zielschule
  sähe sonst jemanden einer anderen Schule (ein Konto je Adresse). Erst aus
  einer der Gruppen nehmen.
- Ein vermerktes Einverständnis bleibt beim Wechsel erhalten.

**Gäste** (Gruppen der Schule `guests`) sind Erwachsene: Teilnehmende an
Workshops, Firmen, Ehemalige, die ihr Konto behalten. Minderjährige nehmen
nur über ihre Schule teil. Ihre Grundlage ist die Nutzungsordnung: Bei der
ersten Anmeldung zeigt die Seite eine Leiste (`Main#terms_bar_html`), und
der eigene Workspace startet erst nach »Ich bin einverstanden«
(`/api/accept_terms`, `User.terms_accepted_at` und `terms_version`).
Administration und Testkonten sind ausgenommen; wer in einen Workspace
hineinsieht, ist nicht betroffen.

## Sprachen der Klasse

Unter »Einstellungen« ordnet eine Lehrkraft die Programmiersprachen ihrer
Gruppe (`class_languages.js`, `/api/set_group_code_languages`): Jede
Aktivität, die es in mehreren Sprachen gibt, zeigt die erste Sprache dieser
Liste, die sie anbietet, sonst Ruby, sonst ihre erste (`CodeLanguages.resolve`).
Alle fünf Sprachen sind erlaubt; darunter stehen die übrigen, die am
häufigsten angebotenen zuerst, mit der Zahl der Aktivitäten. Ziehen (Maus,
Stift, auf dem Handy am Griff), die Pfeile oder die Tastatur (↑/↓, Entf,
Enter) ändern die Liste, gespeichert wird sofort; leer wird sie nie. Ein Satz
darunter sagt, welche Aktivitäten dann welche Sprache zeigen. Gespeichert ist
die Liste als `GroupSetting.code_languages`; eine Gruppe von vorher mit einer
Sprache (`code_language`) gilt als Liste mit dieser einen, bis die Liste zum
ersten Mal gespeichert wird. Wer in mehreren Gruppen ist, bekommt die Liste
der ersten Gruppe mit einer Einstellung. Die eigene Wahl auf einer Seite
(Ruby oder Python, `u.code_language`, und JavaScript, Dart oder Java auf den
Seiten, die sie anbieten) geht vor.

## Kopierknöpfe

Voreingestellt hat jeder Codeblock einen Kopierknopf, für alle. Auf der
Klassenseite neben »Sprachen der Klasse« stellt »Kopierknöpfe: Wo sinnvoll« eine Gruppe
um: Ihre Lernenden bekommen Kopierknöpfe nur an Blöcken, bei denen Abtippen
nichts lehrt (docs/aktivitaet-schreiben.md), und tippen kurzen Code ab
(`GroupSetting.copy_all_code = false`, sofort gespeichert). Wer in mehreren
Gruppen ist, bekommt alle Knöpfe, solange nicht jede seiner Gruppen »Wo
sinnvoll« gewählt hat. Lehrkräfte und Besucher haben die Voreinstellung,
beim Nachahmen sieht die Lehrkraft die Seite wie die Person.

## Tipps in Stufen

Manche Aufgaben und Kästen »Jetzt bist du dran« haben Tipps in Stufen
(docs/didaktik.md). Wer welchen Tipp aufgeklappt hat, sehen Lehrkräfte
nicht, mit Absicht: Lernende sollen Hilfe nehmen, ohne sich zu fragen, wie
das aussieht. Der Server erfährt es nicht; nur der Browser der Person merkt
es sich, für jedes Konto getrennt. Auch »Nachahmen« zeigt es nicht, denn
dort gilt, was der Browser der Lehrkraft gespeichert hat. Hängen viele an
derselben Stelle, merkt die Lehrkraft das im Raum, und es ist ein Thema für
die ganze Gruppe.

## Klassen-Repository

Die Sandboxes (Sortieren, Datenstrukturen, CPU) können mit einem
gemeinsamen Git-Repository der Klasse arbeiten. Welches das ist, trägt eine
Lehrkraft je Gruppe ein: auf der Klassenseite im Reiter »Einstellungen«
(neben der Sprache und den Kopierknöpfen der Gruppe) unter
»Git-Repositorys«, ein Feld je Seite, deren Meta-Block `class_repo: group`
sagt, in der Reihenfolge des Katalogs, gespeichert mit »Adressen speichern«
(`class_board.js`, `/api/set_group_class_repos`).

- Eintragen dürfen die Lehrkräfte der Gruppe und die Administration
  (`teacher_or_admin!` und `class_groups`, sonst 403).
- Angenommen werden nur `user@server:pfad(.git)`, `ssh://[user@]server[:port]/pfad`
  und `https://server[:port]/pfad`; der Server ein einfacher Hostname, ohne
  Leerzeichen, Anführungszeichen, `..` oder Zeichen, die in der Shell etwas
  bedeuten, denn die Adresse steht in einem Befehl, den Lernende kopieren
  (`ClassRepos.normalize`). Ist eine Adresse ungültig, wird
  nichts gespeichert und das Feld markiert. Ein leeres Feld entfernt die Adresse.
- Gespeichert als JSON `{ slug => Adresse }` in `GroupSetting.class_repos`,
  neben der Sprache der Gruppe: Umbenennen nimmt es mit, Löschen der Gruppe
  löscht es.
- Lernende sehen auf der Seite die Adresse der ersten ihrer Gruppen, die
  eine für diese Seite hat, im Befehl zum Klonen, dazu den Server und die
  Aufgaben zu SSH-Schlüssel, Git und Veröffentlichen. Bei einer
  `https://`-Adresse fällt der SSH-Schlüssel samt Aufgabe weg; die Seite
  erklärt stattdessen das Zugangstoken. Ohne Adresse sehen sie
  die Seite ohne Git: nur die App klonen, den Ordner selbst anlegen, die
  Arbeit bleibt im Workspace; die Git-Aufgaben fallen weg, auch in der
  Klassenübersicht der Gruppe und auf ihrer Startseite (docs/aufgaben.md,
  »Teile mit und ohne Klassen-Repository«).
- Lehrkräfte sehen die Seite mit Platzhaltern und darüber einen Kasten mit
  den Gruppen, die eine Adresse haben, und dieser Adresse; jeder
  Gruppenname führt zu den Einstellungen auf der Klassenseite, das Feld der
  Seite gewählt (`/klasse?group=…&repos=SLUG`).
  Gruppen ohne Adresse fehlen darin. Hat keine Gruppe eine, steht dort statt
  des Kastens ein leiser Hinweis, wo man sie einträgt (Klassenseite, »Einstellungen«).
- Der Workspace legt auf dem Git-Server keine Konten an und lädt niemanden
  ein; das macht die Lehrkraft dort selbst. Die Seite sagt den Lernenden:
  »Deine Lehrkraft lädt dich dorthin ein«.

## Repositorys im Workspace

Mit `GIT_SERVER = true` in `credentials.rb` hält der Workspace die
Repositorys der Gruppen selbst (`GitServer` in
`src/ruby/include/git_server.rb`, der Dienst `git_server` aus
`docker/git-server`, der Plan in docs/plan-git-gruppen.md). Ein Konto auf
einem Git-Server im Netz braucht dann niemand.

- **Anlegen:** Unter »Git-Repositorys« in den Einstellungen der Klassenseite
  steht neben jeder Seite mit `class_repo: group` »Im Workspace anlegen«. Hat die Seite einen Ordner
  `starter/` (das Quiz der Klasse), kommt dessen Inhalt als erster Commit
  hinein, sonst ist das Repository leer. Die Adresse,
  `git@git.workspace:<Ordner>/<Seite>.git`, wird gleich als
  Klassen-Repository der Seite eingetragen; alles andere wie oben.
- **Der Ordner der Gruppe** ist eine zufällige Kennung (`GroupSetting.git_id`),
  nicht ihr Name: Eine umbenannte Gruppe behält ihre Adressen.
- **Schlüssel:** Die Lehrkraft wählt je Gruppe (`GroupSetting.git_keys`).
  »Abgeben«: Lernende erzeugen ein Schlüsselpaar und fügen die Zeile ihres
  öffentlichen Schlüssels (`~/.ssh/id_ed25519.pub`) in den Kasten auf der
  Seite ein, wie bei jedem Git-Server; einen privaten Schlüssel weist der
  Kasten zurück, ohne ihn zu speichern. Liegt im Workspace ein anderer
  öffentlicher Schlüssel als der abgegebene, sagt der Kasten das; beim ersten `ssh -T` vergleichen sie den
  Fingerabdruck des Servers mit dem im Kasten. »Automatisch«: Der Workspace
  schreibt jedem Mitglied ein Schlüsselpaar `~/.ssh/workspace_git`, einen
  Abschnitt für `git.workspace` in `~/.ssh/config` und den Schlüssel des
  Servers in `~/.ssh/known_hosts`, beide Abschnitte zwischen Markierungen
  (`# >>> Workspace-Git …`). Er tut das sofort für jeden, der schon einen
  Workspace hat, nach jedem Einlesen der Gruppen und vor jedem Start eines
  Workspace; vorhandene Schlüssel bleiben.
- **Wer darf:** Mitglieder und Lehrkräfte der Gruppe, nur mit Git (clone,
  fetch, pull, push); keine Shell, keine Weiterleitung. Der Server lehnt
  `git push --force` und das Löschen eines Branches ab und nimmt keinen Push
  über 50 MB an. Nach jeder Änderung an den Gruppen, an einem Repository oder
  an einem Schlüssel schreibt das Backend die Liste der Schlüssel und der
  Zugriffe neu (`data/git-server/config`).
- **Ansehen:** `/git/<Ordner>/<Seite>` (main.rb, »Reading a group's
  repository in the browser«; `GitBrowse`, `git_browse.js`) ist ein
  Code-Browser: links der Dateibaum des Stands (Ordner öffnen sich an Ort
  und Stelle und bleiben beim Weiterklicken offen, Pfeiltasten und Enter,
  `t` sucht eine Datei), rechts ein Ordner mit dem letzten Commit jedes
  Eintrags und seinem README, eine Datei mit Zeilennummern und Links pro
  Zeile (`#L12`), im Code-Stil der Seite, ein Bild (auch SVG, mit
  »Quelltext«), der Verlauf nach Tagen (Tags markiert) oder ein Commit mit
  seinen Änderungen. Oben Branches und Tags in einem Menü, »Klonen« (die
  Adresse in einem Feld mit Kopierknopf) und »ZIP«; auf dem Handy liegt der
  Baum über dem Inhalt. Hat ein Stand mehr als `GitBrowse::TREE_MAX`
  Einträge, liest der Baum seine Ordner einzeln (`…/entries/<ref>/<Pfad>`,
  JSON). Jeder git-Aufruf ist in Zeit und Ausgabe begrenzt. Dürfen:
  Mitglieder und Lehrkräfte der Gruppe und Admins (`git_may_browse?`),
  für Seiten, JSON und Dateien gleich. Dateien für sich (`…/raw/…`) kommen
  als Bild oder Text mit `Content-Security-Policy: sandbox` und `nosniff`:
  HTML, SVG und Markdown-HTML aus den Repositorys laufen nie. Ein README
  zeigt nur Bilder aus dem Repository selbst; ein Bild von anderswo
  verriete dessen Server, wer liest, statt seiner steht der Alt-Text.
- **Wo man hinfindet:** Lernende über »Das Repository im Browser ansehen«
  unter dem Klon-Befehl der Aktivität (nur in den Varianten für den
  Git-Server des Workspace, `ClassRepos.mark_browse`); Lehrkräfte über
  »Ansehen« neben dem Feld in den Einstellungen, »ansehen« in der Zeile
  »Klassen-Repository« über der Aktivität, »Repository« neben »Zur Seite«
  im Fortschritt der Klassenseite und die Übersicht `/git`. Ohne
  Berechtigung oder ohne `GIT_SERVER` erscheint keiner dieser Links.
- **Übersicht:** `/git` (`GitOverview` in `src/ruby/include/git_overview.rb`)
  listet jedes Repository auf dem Server in den Gruppen der Lehrkraft
  (`class_groups`, nur Gruppen mit einem Ordner): Gruppe, Aktivität,
  Adresse, Art der Schlüssel, der letzte Commit mit dem Namen seines Autors
  (nie die E-Mail-Adresse), »Ansehen« und »Einstellungen«
  (`/klasse?group=…&repos=…`). »Nicht eingetragen« heißt, die Gruppe hat für
  die Seite eine andere Adresse oder keine. Admins sehen alle Gruppen mit
  einem Ordner und dazu deren Lehrkräfte; Lernende sehen die Repositorys
  ihrer Gruppen zum Ansehen, ohne Einstellungen und ohne Löschen, ohne
  `GIT_SERVER` gibt es die Seite nicht (404). Verlinkt unter den Feldern der
  Einstellungen (»Übersicht«) und in der Kopfzeile jeder Seite eines
  Repositorys (»Repositorys«). Ein Aufruf von git je Repository, kein Cache.
- **Löschen:** »Löschen« neben dem Feld (nach einer Rückfrage) löscht das
  Repository mit allem darin. Wird eine Gruppe gelöscht oder fehlt sie im
  eingelesenen Verzeichnis, gehen ihre Repositorys mit (`GroupData`). Wird ein
  Konto gelöscht, verschwinden seine Schlüssel; seine Commits bleiben als
  Arbeit der Gruppe in deren Repositorys.
- **Freie Repositorys einer Gruppe:** »Neues Repository« auf `/git`
  (Lehrkräfte und Admins, für ihre Gruppen; `/api/git/create_group_repo`):
  Gruppe, Name (die Regeln der Adresse, einmal je Ordner, nicht der Name
  einer Aktivität mit Klassen-Repository, `GitServer.name_problem`),
  Beschreibung (in git's Datei `description`), leer oder mit einer
  `README.md` (ein Commit von »Workspace«). Es liegt im Ordner der Gruppe
  und gehört zu keiner Aktivität (»frei« in der Übersicht); Zugang wie bei
  einem Klassen-Repository. Gelöscht wird es auf `/git` nach dem Eintippen
  seines Namens (`/api/git/delete_group_repo`); eins einer Aktivität weiter
  auf der Klassenseite.
- **Eigene Repositorys zum Üben** (»Meine Repositorys« auf `/git` und im
  Profil unter »Werkzeuge«, »Git«): jede Person, Lernende wie Lehrkräfte,
  höchstens `GitServer::PERSONAL_MAX` (10), in einem eigenen Ordner
  (`User.git_id`, angelegt beim ersten). **Nur die Person selbst** darf sie
  klonen, pushen und ansehen; nicht die Lehrkräfte ihrer Gruppen, nicht
  andere Lernende. Admins sehen auf `/git` nur, dass es sie gibt (Person,
  Name, Größe, Datum) und können eins löschen, aber nicht hineinsehen. So
  entschieden, weil Üben privat ist; was gemeinsam entsteht, gehört in ein
  Repository der Gruppe (docs/plan-git-gruppen.md). Getestet in
  `git_server_test.rb` (`access_map`, `may_read?`) und
  `git-repos.browser.spec.ts`. Schlüssel: die, die die Person schon hat;
  ohne einen legt der Workspace auf Wunsch ein Schlüsselpaar in ihrem
  Workspace an (`/api/git/install_my_key`, wie bei »Automatisch«) oder sie
  gibt einen öffentlichen Schlüssel ab. Gelöscht mit dem Konto
  (`Main#delete_account`). Das Profil hat dazu »Git im Menü anzeigen«
  (`show_git`), wie phpMyAdmin und Neo4j; alles davon nur mit
  `GIT_SERVER`.
- **Ein vorhandenes Repository umziehen:** ein leeres im Workspace anlegen,
  dann aus einem Klon des alten `git push --mirror` an die neue Adresse.

## Personen und Schulen (`/personen`, Administration)

`include/people_page.rb`, `static/personen.html`:

- Schulen: Name, Anschrift, Datenschutzbeauftragte Stelle, Vorschrift,
  Aufsichtsbehörde (für die Unterlagen der Schule) und die beiden Daten, mit
  denen eine Schule zugestimmt hat: Entscheidung der Schulleitung und
  Vereinbarung zur Auftragsverarbeitung (`decision_at`, `agreement_at`;
  `approved_at`, sobald beide da sind), für die Schule der Installation
  genauso. Daneben, wie viele Schülerinnen und Schüler ein Einverständnis
  haben und wie viele ungeklärt sind. Die Zahl der Lehrkräfte klappt die
  Lehrkräfte der Schule auf (`Main#teachers_by_school`, eine Abfrage für alle
  Schulen): Name und Adresse, die Gruppen, die sie betreuen, wie viele
  Lernende darin sind und wann sie zuletzt da waren. »Gäste anlegen« macht die Schule
  `guests` für Erwachsene von außerhalb (Nutzungsordnung); ihre Gruppen
  entstehen unter `/gruppen`.
- Lehrkräfte ohne Konto (»Aufräumen«): eingeladene Lehrkräfte ohne Konto,
  mit Schule und betreuten Gruppen (allein betreute hervorgehoben);
  entfernen nimmt die ganze Einladung heraus (`/api/people/remove_unused_teachers`,
  bestätigt mit der Zahl). Administration nie.
- Einladungen ohne Konto: Gruppen mit Eingeladenen, zu deren Adresse es kein
  Konto gibt (etwa aus der Liste der ganzen Schule); Lehrkräfte zählen nie
  dazu. Ausgewählte Gruppen lassen sich davon leeren, bestätigt mit der Zahl
  der Personen; wer danach in keiner Gruppe steht, ist nicht mehr
  eingeladen. Ein Konto wird dabei nicht gelöscht, es gibt keins.
- Lehrkräfte: Adresse, Name (freiwillig) und Schule (Pflicht); eine
  Willkommensmail (`MailTexts.welcome_teacher`) mit den Links zum Einstieg,
  zu `/gruppen` und zu den Unterlagen ihrer Schule und, wenn `COST_NOTE`
  gesetzt ist, dem Hinweis zu den Kosten. »Keine Lehrkraft mehr« nimmt
  Schule und betreute Gruppen weg.
- Alle Personen mit Rolle, Schule, Gruppen, bei Lernenden und Gästen den
  Lehrkräften ihrer Gruppen (`Roster.teachers_of_members` aus `@@teachers`
  und `@@group_members`, ohne die Testgruppen; »–«, wenn keine Gruppe eine
  hat), wann sie zuletzt da waren und
  dem Speicher ihres Workspace (`Main#workspace_disk_usage`, aus der Messung
  von `housekeeping.rb`; hervorgehoben ab dem Fünffachen des Medians und
  mindestens 200 MB), filterbar nach Schule und Rolle, sortierbar nach »am
  längsten nicht da« und »am meisten Speicher«. Die Suche findet auch die
  Lehrkraft: Ihr Name oder ihre Adresse zeigt sie und alle, die sie betreut;
  dahin führt auch ein Klick auf eine Lehrkraft, hier und unter »Schulen«.
  Namen lassen sich dort ändern;
  eine einzelne Einladung ohne Konto lässt sich entfernen.
  »Zuletzt da« ist `User.last_seen` (der Tag des letzten Besuchs, seit
  2026-10-18 erfasst, höchstens ein Schreibzugriff am Tag, nicht beim
  Hineinsehen); fehlt er, die letzte Anmeldung (Ablauf der neuesten Sitzung
  minus `SESSION_LIFETIME_DAYS`) oder die letzte Antwort auf eine Aufgabe,
  was später war (`Main#activity_by_email`).

Auf der Klassenseite stehen die Gruppen unter ihrer Schule, sobald es mehr
als eine gibt; Testgruppen und Gruppen ohne Schule zuletzt.

## Treffpunkt

Im Reiter »Fortschritt« setzt eine Lehrkraft für ihre Gruppe einen
Treffpunkt nach einer Aufgabe der Aktivität (»Treffpunkt«, über der
Tabelle), verschiebt ihn oder hebt ihn auf (»keiner«). Wer die Seite offen
hat, sieht es sofort, ohne neu zu laden: Nach der Aufgabe steht ein Band
»Hier trifft sich deine Klasse«; wer alle Aufgaben bis dahin gelöst hat
(Extras zählen nicht), liest dort »Du bist da« und was bis zum Gespräch zu
tun ist. Versteckt wird nichts; was danach kommt, ist blasser. Über der
Tabelle steht, wie viele da sind und wer noch fehlt, und es zählt neu,
sobald jemand eine Aufgabe löst; wer heute fehlt, nimmt die Lehrkraft mit
»fehlt heute« aus der Zählung (nur ihr Browser merkt es sich, bis
Mitternacht). Sind alle da, wird die Zeile grün. Die Tabelle hat nach der
Spalte der Aufgabe eine gestrichelte Linie. Auf der Seite der Aktivität setzt,
verschiebt und hebt die Lehrkraft den Treffpunkt auch direkt auf (für die
Gruppe, die auf der Klassenseite gewählt ist). Wer in zwei Gruppen ist, sieht
den früheren Treffpunkt.
Plan und Aufbau: [plan-treffpunkt.md](plan-treffpunkt.md).

## Wer still ist, wie weit alle sind

Im Reiter »Fortschritt« steht in jeder Zeile unter »zuletzt«, wann die
Person zuletzt eine Aufgabe der Aktivität geprüft, beantwortet oder
abgegeben hat (das `updated` ihrer `TaskState`s; gespeichert wird dafür
nichts Neues). Arbeitet die Klasse gerade daran (mindestens zwei in den
letzten 20 Minuten), nennt eine Zeile über der Tabelle, wer seit über zehn
Minuten nichts geprüft oder abgegeben hat, gezählt vom Beginn der Stunde an
(der ersten Prüfung von jemandem in den letzten 90 Minuten); wer alles
gelöst hat, zählt nicht. Ihre Namen sind leicht hinterlegt, ohne
Alarmfarbe: Was im Workspace geschieht, sieht die Seite nicht. Die Regel
steht in `ClassBoard.quiet`; `class_last_active.js` lässt die Zeiten
weiterlaufen und fragt jede Minute `/api/class/last_active` (nur für die
eigenen Gruppen). Wer für den Treffpunkt »fehlt heute« ist, steht nicht in
der Zeile.

Der Reiter »Überblick« zeigt jede Person mit allen Aktivitäten der Gruppe:
angeheftete zuerst, dann jede, an der jemand der Gruppe gearbeitet hat, in
der Reihenfolge des Katalogs; je Zelle gelöst von allen (ohne Extras, wie
im Fortschritt) mit einem Balken, dazu »zuletzt« über alle Aktivitäten
(`ClassBoard.overview`). Noten und Export gibt es nicht; die Zahlen sagen,
wie weit jemand ist.

## Rückmeldung zu einer Aktivität

Lehrkräfte sagen der Administration, wie eine Aktivität mit einer Gruppe
lief (`include/activity_feedback.rb`, die Seite in
`include/activity_feedback_page.rb`, `activity_feedback.js`). Im Reiter
»Fortschritt« steht dafür immer »Rückmeldung geben« (danach »Deine
Rückmeldung«); es öffnet ein Formular über der Tabelle, nie ein Fenster
von selbst. Die Fragen: wie es insgesamt lief (gut, teils, teils, nicht
gut, dazu ein Satz), die Dauer gemessen an `duration:` (kürzer, passte,
länger), das Niveau für die Gruppe (zu leicht, passend, zu schwer), in
welcher der fünf Sprachen der Klassen (`CodeLanguages::LANGUAGES`) die
Aktivität zusätzlich gewünscht ist (die, die sie schon hat, sind als »gibt
es schon« gesperrt; dazu ein Feld für eine andere), was gefehlt, gestört
oder gut funktioniert hat, und ob die Administration an die Adresse
schreiben darf. Alles ist freiwillig, eine Antwort muss sein. Eine
Rückmeldung je Lehrkraft, Gruppe und Aktivität; sie lässt sich ändern.

**Wann die Seite fragt** (`ActivityFeedback.prompt`): Mindestens drei der
Lernenden der Gruppe (in einer kleineren Gruppe alle; ohne Lehrkräfte,
Admins und Testkonten) haben eine Aufgabe der Aktivität geprüft,
beantwortet oder abgegeben, das neueste `updated` ihrer `TaskState`s ist
höchstens 60 Tage alt, und die Klasse ist entweder weitergegangen und
hier seit 7 Tagen still, oder hier seit 21 Tagen still (`QUIET_DAYS`,
`QUIET_ALONE_DAYS`). Weitergegangen heißt: In einer anderen Aktivität
haben mindestens die Hälfte der Lernenden, die hier gearbeitet haben, und
mindestens drei eine Aufgabe nach der letzten von hier
(`ActivityFeedback.moved_on_count`); ein paar, die anderswo stöbern,
zählen nicht. Eine Woche Ferien oder eine ausgefallene Stunde mitten in
einer Aktivität bringt die Karte also nicht. Dann steht über der Tabelle
die Karte »Wie lief die Aktivität?« mit »Rückmeldung geben«, »Nicht jetzt«
(14 Tage ausgeblendet, `snoozed_until`) und »Nicht mehr fragen« (`never`,
für diese Gruppe und Aktivität). Ältere Aktivitäten fragen nicht mehr von
selbst; der Knopf bleibt. Wartet eine Karte, zeigt die Startseite der
Lehrkraft einen kleinen Hinweis, nur die neueste; »Nicht jetzt« dort blendet
diese Karte 14 Tage aus und im selben Browser eine Woche lang jeden Hinweis
auf der Startseite (lokaler Speicher).

**Wer**: Rückmeldungen geben nur Lehrkräfte einer Gruppe (`+`), auch Admins
nur für Gruppen, die sie selbst betreuen; nicht für Testgruppen und nicht
beim Nachahmen (`feedback_group!` in `include/access.rb`, sonst 403). Lesen
kann alle die Administration unter `/rueckmeldungen` (Menü
»Administration«, unter »Inhalte«), um zu sehen, welche Aktivitäten Arbeit
brauchen: zuerst je Aktivität eine Karte (wie es lief als Balken in Grün,
Gelb und Rot mit Zeichen und Zahlen, Dauer und Niveau als »2× länger, 1×
passte« mit der angesetzten Dauer, die gewünschten Sprachen), die neueste
Rückmeldung zuerst; darunter jede Antwort mit ihren Texten, Gruppe,
Lehrkraft und Datum, die Adresse nur, wenn das Häkchen gesetzt ist. Filter:
Name der Aktivität, »Nur mit Text«, »Kontakt erwünscht«. Lernende sehen
nichts davon.

**Gespeichert** als `(:ActivityFeedback {group, activity, …})-[:BY]->(:User)`
(Datenschutzerklärung, Verzeichnis und Löschkonzept aus `privacy.rb`). Sie
geht mit dem Konto der Lehrkraft (`AccountDeletion`) und steht in ihrem
Download unter `rueckmeldungen`. Beim Umbenennen der Gruppe zieht sie mit;
wird die Gruppe gelöscht, bleibt eine beantwortete unter `former_group`
(»Gruppe gelöscht« auf `/rueckmeldungen`, eine neue Gruppe gleichen Namens
wird neu gefragt), »Nicht jetzt« und »Nicht mehr fragen« fallen weg
(`GroupData`).

## Wer was sieht

| | Lernende | Lehrkraft (`*`) | Lehrkraft (`+`) | Admin |
| --- | --- | --- | --- | --- |
| Notizen für Lehrkräfte (auf den Aktivitäten) | | ✓ | ✓ | ✓ |
| Der ganze Code an jedem Schritt (Knopf »Code bis hier«, nicht beim Nachahmen) | | ✓ | ✓ | ✓ |
| Klassenseite `/klasse` für die eigenen Gruppen | | | ✓ | ✓ alle |
| Auf der Startseite am Fuß jeder Aktivität: wie oft ihre Aufgaben gelöst wurden (ein Band mit einem Feld je Aufgabe, dazu Lösungen und Personen), ohne Lehrkräfte, Admins und Testkonten; gezählt höchstens alle 10 Minuten (`activity_usage`) | | | | ✓ |
| Anmelde-Codes der eigenen Gruppen (Klassenseite) | | | ✓ | ✓ |
| Workspace öffnen, Nachahmen | | | ✓ eigene Gruppen | ✓ alle |
| Leistungsüberprüfungen | | | ✓ eigene Gruppen | ✓ |
| Verwaltungsseite `/admin` (Server, alle Workspaces) | | | | ✓ |
| Rückmeldung zu einer Aktivität geben und ändern (Klassenseite, »Fortschritt«) | | | ✓ eigene Gruppen | ✓ selbst betreute Gruppen |
| Alle Rückmeldungen lesen (`/rueckmeldungen`) | | | | ✓ |
| Dauer einer Aktivität (`duration`, im Streifen über der Seite) | | | ✓ | ✓ |
| Zeile des Lehrplans (`curriculum`, im Streifen über der Seite, in Lila) | | | ✓ | ✓ |
| Die Übersicht `/lehrplan` (docs/lehrplan.md); Gäste wählen das Land über `?land=`, Lehrkräfte speichern es im Profil | ✓ | ✓ | ✓ | ✓ |
| »Anpinnen« im Streifen über einer Aktivität: dieselben Anheftungen wie auf der Klassenseite, bei einer Gruppe mit einem Klick, bei mehreren in einer Liste mit Häkchen | | | ✓ eigene Gruppen | ✓ |
| »Lösung einsetzen« neben den Knöpfen einer Kata | | ✓ | ✓ | ✓ |
| Startseite: »Weiter mit«, »So geht’s« | ✓ (auch Testkonten) | | | |

Eine Lehrkraft sieht eine Person, sobald sie **eine** ihrer Gruppen betreut,
nicht nur die erste der Person (`GroupVisibility`). Andere Lehrkräfte und
Admins kann sie weder öffnen noch ansehen (`LookIn.allowed?`).

## Hineinschauen

Auf der Klassenseite stehen bei jeder Person zwei Knöpfe:

- **Workspace öffnen** öffnet ihren Workspace in VS Code.
- **Nachahmen** zeigt die Website so, wie die Person sie sieht. Eine Leiste
  oben sagt das und führt mit »Zurück zu deinem Konto« zurück.

Beides gilt nur, solange die Lehrkraft hineinschauen darf: Verliert sie die
Gruppe, ihre Rolle oder ihren eigenen Zugang, endet das Nachahmen mit der
nächsten Anfrage (sie ist abgemeldet), und den geöffneten Workspace sieht sie
nach dem nächsten Neuladen von nginx nicht mehr (`Main.may_still_look_in?`).

Vorher fragt ein Dialog nach einem Satz, warum (»Ich sehe mir deine Abgabe zu
Aufgabe 3 an.«). Der letzte Satz ist vorausgefüllt; beim Durchsehen einer
ganzen Klasse genügt er also einmal. Die Person findet den Satz bei der
nächsten Anmeldung in einer ruhigen Leiste (»Verstanden«) und danach in
ihrem Profil. Noten gehören nie hierher. Für Admins gilt dasselbe (auch auf
`/admin`): Sie geben einen Grund an, und die Person sieht den Hinweis;
zusätzlich steht jeder Zugriff der Administration ein Jahr lang in
`/internal/admin-access.jsonl`. Nur das eigene Testkonto braucht keinen Satz.

## Testkonto

Jede Lehrkraft hat genau ein Testkonto, `adresse+test@domain`, ob mit `+`,
mit `*` oder als Admin, allein in einer eigenen **Testgruppe (Name)**. Nur
sie sieht diese Gruppe; sie steht in der Gruppenauswahl der Klassenseite
zuletzt, und auch Admins sehen die Testgruppen der anderen nicht. Damit
probiert sie aus, was Lernende sehen: Aufgaben, Abgaben, die Galerie,
Leistungsüberprüfungen; im Profil kann sie das Testkonto einer eigenen
Leistungsüberprüfung zuweisen. »Nachahmen« braucht dort keinen Satz.

**Testkonto zurücksetzen** (Klassenseite, Reiter »Lernende« der Testgruppe)
leert den Workspace des Testkontos und seine Workspaces in
Leistungsüberprüfungen, legt seine Datenbanken neu an und löscht seine
Antworten, Versuche und Abgaben. Zugewiesen bleibt es; andere Konten ändern
sich nicht. Wer sich direkt als Testkonto anmeldet, bekommt den Anmelde-Code an die
eigene Adresse ohne `+test`; nicht jedes Postfach nimmt Adressen mit `+` an.

## Einstieg für Lehrkräfte

`/lehrmaterial-einstieg` (`src/content/lehrmaterial/lehrmaterial-einstieg/`)
erklärt neuen Lehrkräften die erste Stunde: Anmeldung mit den Codes am Beamer,
die Reiter der Klassenseite, Abgaben ansehen, das Testkonto und »Nachahmen«,
die Notiz für Lehrkräfte und was der Server speichert. Die Klassenseite weist
mit »Neu hier?« darauf hin, bis die Lehrkraft den Hinweis ausblendet (in
diesem Browser); der Link neben der Überschrift bleibt. `/lehrmaterial`
führt ebenfalls dorthin. Die Bilder (16:9, doppelte Pixeldichte)
nimmt ein Skript mit den E2E-Konten unter erfundenen Namen auf und räumt
danach wieder auf; nach Änderungen an der Klassenseite neu aufnehmen:

```bash
cd e2e && HS_PICTURES=1 npx playwright test --project=pictures
```
