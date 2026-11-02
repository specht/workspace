# What the curriculum still lacks, and what could fill it

`/lehrplan` lists, for every name of the Berlin-Brandenburg curricula
([lehrplan.md](lehrplan.md)), the activities whose `curriculum:` line names
it. This plan is about what is still missing, as of 2 November 2026.

Filled between 27 and 31 October 2026, and live: Computernetze (TCP/IP, DNS,
the web server), object orientation (`supermarkt`, `textadventure`,
`kata-objects`), Künstliche Intelligenz (`sprachmodell`, `perzeptron`,
`naechste-nachbarn`), Theoretische Informatik (`kellerautomat`,
`turingmaschine`, `grammatiken`, `regex`, `laufzeit`), Technische Informatik
(`logikgatter`, `logikwerkstatt`), Informatik und Gesellschaft (`tracking`,
`suchmaschine`), Applikative Programmierung (`kata-functional`, `prolog`),
Projektmanagement and Softwareprojekt (`check-digits`, `class-quiz`,
`kata-tests`, `testen`), and the chapters on QR codes, Normalisierung, LAN
and WLAN, changing data in SQL and adding in binary. Which page names what
is on `/lehrplan`; how each was decided is in the git history of this file.

## Open

| Name | Left out | Proposal |
| --- | --- | --- |
| Oberstufe: Datenbanken und Datenschutz | Urheberrecht | a section on licences, perhaps with the picture rights of the Workspace's own pages (`/picture-rights`) |
| Sek I: Geschichte der Informatik | Rechenhilfsmittel, Rechnergenerationen | a chapter in `sandbox-cpu` |
| Sek I: Digitale Bilder und Visualisierung | Manipulation von Bildern und Filmen | an activity or chapter on manipulated pictures: filters and retouching in code, how a fake is made and noticed |
| Oberstufe: Softwareprojekt | Ergonomie | a chapter of `class-quiz` or of `svelte` |

Shelved or decided:

- **Sek I: Physical Computing** (shelved 28 October 2026): the schools have
  no boards. The Workspace could build MicroPython for a Calliope mini or
  micro:bit as a `.hex` to drag onto the board.
- **Sek I: Leben in und mit vernetzten Systemen**, Gefahren bei E-Mails
  (shelved 29 October 2026): a mail server with a forged sender was
  proposed; it is unclear what students could do there that is not very
  complicated.
- **Sek I: Standardsoftware:** office programs are not what the Workspace is
  for; LaTeX stays the only page.

## Decided along the way

Decisions of the maintainer that still shape the pages:

- **Object orientation** (27 October 2026) as one activity with modelling
  rather than a Kata set alone, since the curriculum names modelling (UML
  class diagrams) and programming side by side. Its story is a simulation
  of supermarket checkouts, not a library: a library is about data that must
  be kept, which is what an ER model is for; objects pay where they live
  while a program runs, change state and act differently by class. The
  diagram is drawn in an own extension, the UML-Werkstatt (`.uml`, so that
  state and sequence diagrams can join), so that it is checked like the
  ER-Werkstatt's; names in it are written as in the student's code
  (docs/aufgaben.md, »classes«).
- **Struktogramme** use the English keywords of the Handbuch's pseudocode
  (`while`, `for`, `until`) and »wahr« and »falsch« (27 October 2026).
- **Künstliche Intelligenz** from scratch without libraries, so that
  students write the small functions themselves; TensorFlow in the image
  would let `digits-experiment` into a catalog later.
- **The logic editor** lives on the page, not in VS Code: no Workspace
  needed, a tablet is enough.
- **The front page:** a section takes as many activities as belong in it
  (29 October 2026); the language tiles keep full rows of four. Parked
  activities may come back and activities may move to close a gap.
