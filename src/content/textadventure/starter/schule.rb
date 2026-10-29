# Nachts in der Schule: eine Welt aus deinen Klassen. Spielen mit
# ruby play.rb schule.
def build_school
  classroom = Room.new("Raum 204", "Du bist über deinem Heft eingeschlafen. Draußen ist es dunkel, die Schule ist still.")
  hall = Room.new("Flur", "Nur die grünen Notausgangsschilder leuchten.")
  staff = Room.new("Lehrerzimmer", "Stapel von Heften, eine kalte Kaffeemaschine.")
  stairs = Room.new("Treppenhaus", "Deine Schritte hallen.")
  entrance = Room.new("Eingangshalle", "Hinter der großen Glastür liegt der Schulhof.")
  caretaker = Room.new("Hausmeisterraum", "Werkzeug an der Wand, ein Radio, das leise rauscht.")
  cellar = DarkRoom.new("Keller", "Heizungsrohre, alte Stühle und ein Schlüsselbrett.")
  outside = GoalRoom.new("Schulhof", "Kühle Nachtluft. Du bist draußen. Geschafft!")

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

  classroom.add_item(Item.new("zettel", "»Der Hausmeister lässt seine Taschenlampe nie aus dem Raum.«"))
  classroom.add_item(Food.new("apfel", "Knackig und süß."))
  caretaker.add_item(Lamp.new("taschenlampe", "Eine schwere Taschenlampe. Sie leuchtet."))
  cellar.add_item(Key.new("lehrerschlüssel", "Am Anhänger steht: Lehrerzimmer."))
  staff.add_item(Key.new("haustürschlüssel", "Ein großer Schlüssel mit einem roten Anhänger."))

  Player.new(classroom)
end
