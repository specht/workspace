# Lehrkräfte, Gruppen und Einladungen

Die Einladungslisten liegen unter `data/invitations/*.txt` (im Container:
`/invitations/*.txt`). Die Dateien werden in alphabetischer Reihenfolge
gelesen.

```text
> Klasse 12
Lena Muster <lena@example.org>
Tom Beispiel <tom@example.org>
+ specht@example.org

> LK Informatik
lena@example.org
Mia Test <mia@example.org>
+ specht@example.org

> Lehrkräfte
Dr. Michael Specht <specht@example.org>
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

## Wer was sieht

| | Lernende | Lehrkraft (`*`) | Lehrkraft (`+`) | Admin |
| --- | --- | --- | --- | --- |
| Notizen für Lehrkräfte (Profil, Aktivitäten) | | ✓ | ✓ | ✓ |
| Klassenseite `/klasse` für die eigenen Gruppen | | | ✓ | ✓ alle |
| Anmelde-Codes der eigenen Gruppen (Klassenseite) | | | ✓ | ✓ |
| Workspace öffnen, Nachahmen | | | ✓ eigene Gruppen | ✓ alle |
| Leistungsüberprüfungen | | | ✓ eigene Gruppen | ✓ |
| Verwaltungsseite `/admin` (Server, alle Workspaces) | | | | ✓ |

Eine Lehrkraft sieht eine Person, sobald sie **eine** ihrer Gruppen betreut,
nicht nur die erste der Person (`GroupVisibility`). Andere Lehrkräfte und
Admins kann sie weder öffnen noch ansehen (`LookIn.allowed?`).

## Hineinschauen

Auf der Klassenseite stehen bei jeder Person zwei Knöpfe:

- **Workspace öffnen** öffnet ihren Workspace in VS Code.
- **Nachahmen** zeigt die Website so, wie die Person sie sieht. Eine Leiste
  oben sagt das und führt mit »Zurück zu deinem Konto« zurück.

Vorher fragt ein Dialog nach einem Satz, warum (»Ich sehe mir deine Abgabe zu
Aufgabe 3 an.«). Der letzte Satz ist vorausgefüllt; beim Durchsehen einer
ganzen Klasse genügt er also einmal. Die Person findet den Satz bei der
nächsten Anmeldung in einer ruhigen Leiste (»Verstanden«) und danach in
ihrem Profil. Noten gehören nie hierher. Admins dürfen ohne Satz
hineinschauen (Support); dann erfährt die Person nichts.

## Testkonto

Jede Lehrkraft mit `+` hat ein Testkonto, `adresse+test@domain`, allein in
einer eigenen **Testgruppe (Name)**, die nur sie sieht und die in der
Gruppenauswahl der Klassenseite zuletzt steht. Damit probiert sie aus, was
Lernende sehen: Aufgaben, Abgaben, die Galerie, Leistungsüberprüfungen.
»Nachahmen« braucht dort keinen Satz. Bei den meisten Mail-Anbietern kommen
Anmelde-Codes für `+test` im eigenen Postfach an; man kann sich also auch
direkt als Testkonto anmelden.
