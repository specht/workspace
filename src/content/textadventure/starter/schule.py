# Nachts in der Schule: eine Welt aus deinen Klassen. Spielen mit
# python3 play.py schule.
from adventure import Room, DarkRoom, GoalRoom, Item, Key, Food, Lamp, Player


def build_school():
    classroom = Room("Raum 204", "Du bist über deinem Heft eingeschlafen. Draußen ist es dunkel, die Schule ist still.")
    hall = Room("Flur", "Nur die grünen Notausgangsschilder leuchten.")
    staff = Room("Lehrerzimmer", "Stapel von Heften, eine kalte Kaffeemaschine.")
    stairs = Room("Treppenhaus", "Deine Schritte hallen.")
    entrance = Room("Eingangshalle", "Hinter der großen Glastür liegt der Schulhof.")
    caretaker = Room("Hausmeisterraum", "Werkzeug an der Wand, ein Radio, das leise rauscht.")
    cellar = DarkRoom("Keller", "Heizungsrohre, alte Stühle und ein Schlüsselbrett.")
    outside = GoalRoom("Schulhof", "Kühle Nachtluft. Du bist draußen. Geschafft!")

    classroom.connect("westen", hall)
    hall.connect("osten", classroom)
    hall.connect("norden", staff)
    staff.connect("süden", hall)
    hall.connect("unten", stairs)
    stairs.connect("oben", hall)
    stairs.connect("unten", entrance)
    entrance.connect("oben", stairs)
    entrance.connect("westen", caretaker)
    caretaker.connect("osten", entrance)
    entrance.connect("unten", cellar)
    cellar.connect("oben", entrance)
    entrance.connect("süden", outside)

    hall.lock("norden", "lehrerschlüssel")
    entrance.lock("süden", "haustürschlüssel")

    classroom.add_item(Item("zettel", "»Der Hausmeister lässt seine Taschenlampe nie aus dem Raum.«"))
    classroom.add_item(Food("apfel", "Knackig und süß."))
    caretaker.add_item(Lamp("taschenlampe", "Eine schwere Taschenlampe. Sie leuchtet."))
    cellar.add_item(Key("lehrerschlüssel", "Am Anhänger steht: Lehrerzimmer."))
    staff.add_item(Key("haustürschlüssel", "Ein großer Schlüssel mit einem roten Anhänger."))

    return Player(classroom)
