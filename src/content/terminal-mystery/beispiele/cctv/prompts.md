# Prompts für die Kamerabilder

Die Bilder kommen aus ChatGPT, ein Chat pro Kamera. Was sich bewährt hat:

- Zuerst das leere Bild der Kamera, als neues Bild (»Create a new image (not
  an edit)«). Jedes weitere ist eine Bearbeitung eines bestimmten Bildes aus
  dem Chat, nie »das nächste«. Notfalls das Ausgangsbild noch einmal
  hochladen und mit »Select« nur die Stelle markieren, die sich ändert.
- Eine Haltung beschreiben, keine Bewegung: »steht …, mit dem Rücken zur
  Kamera, zur Tür gewandt«, nicht »geht zur Tür«.
- Türen nur geschlossen, ganz offen oder halb offen mit genauer Angabe:
  öffnet nach außen, Bänder rechts, Türblatt draußen.
- »Not sharper or brighter than the walls and floor«, sonst wirkt die Person
  eingeklebt.
- Jedes Bild auf Gehrichtung, verschobene Gegenstände und Türen prüfen. Ein
  falsches Bild wird neu erzeugt, nie nachbearbeitet.

## Kamera 1, Ausstellung

Stil, für jedes Bild:

```text
Photorealistic still from a cheap ceiling-mounted security camera, high
corner angle looking down, slightly wide-angle with mild barrel distortion,
night, dim artificial light, realistic shadows, slightly underexposed, 4:3
landscape. No text, no timestamp, no logos, no watermark, no readable signs.
```

`empty.jpg`:

```text
A small exhibition room of a modest town museum in a northern German
coastal town at night. In the middle a glass display case on a wooden
plinth; inside, on a small stand, a golden 3.5-inch floppy disk. Old
maritime objects on the walls, a ship model, framed old photographs. Only
emergency lighting and one display light.
```

Die anderen Bilder sind Bearbeitungen von `empty.jpg`: »Keep the camera
position, framing, lighting and every object exactly as they are; change
only what is described. The person: a slim adult in a dark hooded jacket
with the hood up, dark trousers and a dark sports bag, seen from behind or
from the side. Small in the frame.« und dazu:

- `at-case.jpg`: The person stands at the case, the bag on the floor beside them, both hands on the glass hood.
- `lifting-hood.jpg`: The person lifts the glass hood off the plinth.
- `reaching-in.jpg`: The glass hood stands on the floor next to the plinth. The person reaches into the case.
- `into-bag.jpg`: The person puts the golden disk into the open sports bag. The stand is empty.
- `leaving.jpg`: The person walks back toward the doorway, bag in hand. The glass hood is back on the plinth, the stand inside is empty.

Es fehlen noch: die Person beim Hereinkommen (Diskette noch da) und der
Raum danach (Haube drauf, Ständer leer, niemand).

## Kamera 2, Hintereingang

`empty.jpg`, als neues Bild:

```text
Create a new image (not an edit): a photorealistic still from a cheap
security camera mounted high in a corner of a corridor, looking down at an
angle. Slightly wide-angle with mild barrel distortion. Night: one ceiling
lamp, slightly underexposed. 4:3 landscape. No text anywhere: no timestamp,
no logos, no watermark, no readable signs.

The scene: the inside of the narrow back entrance of an old brick museum
building in Germany. On the right side of the image, a plain grey steel door
in a grey steel frame: a lever handle at hip height on the left side of the
door, a lock cylinder below it, and an overhead door closer at the top. No
push bar, no glass. The door opens outward, away from the camera, so its
hinges are on the outside and NOT visible from here. Above the door, a small
green emergency-exit pictogram sign without text. A red fire extinguisher on
the wall to the left of the door, a dark doormat in front of the door. On
the left side of the image, a corridor leads away into the building. The
door is closed. Nobody there.
```

Jedes weitere Bild beginnt mit »Edit the very FIRST image in this chat …
Keep the camera position, framing, lighting and every object exactly as they
are; change only what is described.« Die Person: »a slim adult in a dark
hooded jacket with the hood up and pulled forward, head down so the face
stays in shadow, dark trousers, a dark sports bag«.

Bearbeitungen von `empty.jpg`, Tür geschlossen:

- `off-mat.jpg`: The person stands on the doormat in front of the closed door, seen from the side, body and head turned toward the corridor on the left, mid-step toward it.
- `corridor-away.jpg`: In the corridor on the left, about halfway to the far end, seen from behind, walking away from the camera.
- `corridor-back.jpg`: Coming back along the corridor toward the door, about a third of the way from the door, hood pulled far forward so the face is not visible, bag in one hand, seen at an angle from the side.
- `at-handle.jpg`: Mit »Select« nur Fußmatte und linken Teil der Tür markiert: »The fire extinguisher, the door, the door frame, the silver lever handle and the doormat must stay exactly where they are … The person stands on the doormat, seen from behind, facing the closed door, standing to the RIGHT of the fire extinguisher so the extinguisher stays fully visible. The right hand rests on the existing silver lever handle.«

Tür ganz offen:

- `in-doorway.jpg` (von `empty.jpg`): The grey door is now wide open, swung outward, away from the camera: the door leaf has moved outside and is NOT visible at all. Where the door was, there is an empty dark doorway showing the night outside: dark wet paving and the edge of a bush. In the doorway stands a person stepping in from outside, seen from the side, body turned toward the corridor on the left.
- `open-nobody.jpg` (von `in-doorway.jpg`): Remove only the person and their bag; show what was behind them. Kein Bild der Folge (eine Tür mit Türschließer steht nicht offen), sondern Ausgangsbild für die folgenden.
- `stepping-out.jpg` (von `open-nobody.jpg`): One person stepping out through the doorway, seen from behind, facing out into the night, one foot already outside on the wet paving, bag in one hand.

Tür halb offen, alle von `open-nobody.jpg`:

```text
The door: it opens OUTWARD and its hinges are on the RIGHT side of the door
frame, so the grey door leaf swings out of the building, hinged at the right
edge of the doorway. … From this camera we see the inside face of the leaf
at a steep angle: its right edge sits at the hinges on the right side of the
frame, its left edge reaches out into the night, away from the camera.
```

- `door-closing.jpg`: About two thirds closed, swinging shut by itself a few seconds after someone went out. No person anywhere.
- `on-mat-door-closing.jpg` (von `door-closing.jpg`): The person has just come in, standing on the doormat, seen from the side, turned toward the corridor on the left, dark shoes with dark soles, arms hanging naturally; »The person must match the rest of the picture: the same slightly soft focus, the same low contrast and dim light, lit only by the ceiling lamp above, with a soft shadow on the floor … Not sharper or brighter than the walls and floor.« (Der Schatten fällt trotzdem zur falschen Seite, nach der Bearbeitung kaum zu sehen.)
- `pushing-door.jpg` (von `door-closing.jpg`): The person stands in the open left part of the doorway, on the threshold, seen from behind, facing out into the night, the right arm stretched out against the inside of the door leaf, pushing it further open; dieselben Sätze zu Schärfe und Licht.

## Kamera 3, Parkplatz

Noch offen. Das Auto des Täters ist laut `make_case.rb` ein gelber **Kombi**
(`BS-KR 2417`), kein Kleinwagen: die gelben Kleinwagen gehören anderen.
Ohne Markenzeichen, das Kennzeichen unlesbar.
