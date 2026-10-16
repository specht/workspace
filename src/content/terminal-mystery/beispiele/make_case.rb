#!/usr/bin/env ruby
# Makes the case files of »Ein Krimi im Terminal«: the folder bitstein/
# with the crime scene report, the people, cars, streets, interviews, club
# lists and the alarm system's Git repository, packed as bitstein.tar.gz
# next to this script (for the recipes and task-examples.yaml) and copied to
# src/static/terminal-mystery/, where the page offers it for download.
#
# Everything comes from a fixed seed, so the numbers in ../tasks.yaml stay
# right as long as nothing here changes; the script prints them at the end.
# The Git history has fixed authors and times, so its commit hashes are the
# same on every run.
#
#   ruby make_case.rb [OUTPUT_DIR]
#
# Without OUTPUT_DIR it builds in a temporary folder and only writes the
# archive. The chain of clues (spoiler):
#
#   tatort.txt, lines starting with HINWEIS: a scarf of the chess club, a
#   witness in Möwenweg 23, the night guard Ole Brandt, the alarm system.
#   strassen/moewenweg.txt line 23 -> befragungen/befragung-3817.txt: a
#   yellow car, plate BS-K…, last digit 7. fahrzeuge.txt -> 4 cars.
#   grep -l "Ole Brandt" befragungen/* -> befragung-1290.txt: a bag of the
#   gym Muskelkater. Chess club and gym -> 6 people, two of them with one of
#   the cars: Konstantin Reuter and Marlene Voss. alarmanlage/: the commit
#   »Wartung beendet« deletes two lines of card 4471, which karten.txt gives
#   to Elektro Reuter. personen.txt: Konstantin Reuter is the electrician.
require 'fileutils'
require 'open3'
require 'tmpdir'

SEED = 2026
HERE = __dir__
ARCHIVE = File.join(HERE, 'bitstein.tar.gz')
PUBLISHED = File.expand_path('../../../static/terminal-mystery/bitstein.tar.gz', HERE)

RNG = Random.new(SEED)

FEMALE = %w[
    Anna Clara Elif Greta Ida Kira Mia Paula Romy Tilda Vera Yara Zoe Aylin Carla
    Emma Gesa Irina Katja Lina Nele Pia Selin Ulla Wiebke Berit Dana Frieda Heike
    Ilka Karin Marta Olga Renate Tamara Valentina Xenia Zeynep Birte Chiara Edith
    Gisela Inga Jule Lea Nadia Petra Sabine Ute Waltraud Bettina Cornelia Fiona
    Helga Leila Nora Rieke Silke Ursula
]
MALE = %w[
    Ben David Felix Hannes Jonas Leon Noah Oskar Quentin Samuel Umut Willi Bastian
    Dennis Finn Henrik Jan Lars Malte Ole Rasmus Theo Viktor Yusuf Amir Cem Eike
    Gregor Jasper Lukas Nils Piet Sven Uwe Wolfgang Arne Dirk Fabian Hamza Kai
    Moritz Ingo Rolf Thore Volker Anton Detlef Enno Gunnar Jannik Klaus Mehmet
    Oliver Paul Tim
]
LAST = %w[
    Albers Bauer Clausen Dittmer Ehlers Fischer Gerdes Hansen Iversen Jacobs
    Kühl Lorenzen Martens Nissen Ohlsen Petersen Quast Rieck Schröder Thiessen
    Ulrich Vogt Wagner Yilmaz Zander Arslan Böhm Carstens Dreyer Eggers Feddersen
    Grote Hinz Janssen Kaya Lange Möller Nowak Oltmanns Peters Rademacher Siebert
    Tamm Unger Voß Wulf Ahrens Brandes Christiansen Dahl Engel Friedrichsen
    Gosch Hoffmann Ivanov Jürgensen Köhler Lüders Meyer Nguyen Otte Paulsen Ramm
    Sommer Thomsen Uhlig Vollmer Weiß Asmussen Behrens Celik Detlefsen Ernst
    Frahm Gülen Heller Jensen Krüger Lammers Mahler Neumann Popescu Reimers
    Schulz Timm Ulbricht Wendt Zimmermann Andersen Becker Dose Evers Fuchs
    Gerke Hamann Jessen Kowalski Lindner Mertens Nielsen Özdemir Pohl Rohde
    Stahl Tiedemann Vierck Witt
]
# Each job as [male, female]
JOBS = [
    %w[Lehrer Lehrerin], %w[Bäcker Bäckerin], %w[Fischer Fischerin], %w[Krankenpfleger Krankenpflegerin],
    %w[Arzt Ärztin], %w[Busfahrer Busfahrerin], %w[Verkäufer Verkäuferin], %w[Gärtner Gärtnerin],
    %w[Informatiker Informatikerin], %w[Tischler Tischlerin], %w[Student Studentin], %w[Koch Köchin],
    %w[Hafenarbeiter Hafenarbeiterin], %w[Friseur Friseurin], %w[Postbote Postbotin],
    %w[Bibliothekar Bibliothekarin], %w[Mechaniker Mechanikerin], %w[Bankkaufmann Bankkauffrau],
    %w[Polizist Polizistin], %w[Maler Malerin], %w[Physiotherapeut Physiotherapeutin],
    %w[Erzieher Erzieherin], %w[Apotheker Apothekerin], %w[Landwirt Landwirtin]
].freeze
STREETS = [
    ['Möwenweg', 'moewenweg', 41], ['Hafenstraße', 'hafenstrasse', 64], ['Am Markt', 'am-markt', 22],
    ['Lindenallee', 'lindenallee', 48], ['Ahornweg', 'ahornweg', 30], ['Bahnhofstraße', 'bahnhofstrasse', 57],
    ['Deichstraße', 'deichstrasse', 35], ['Kirchgasse', 'kirchgasse', 18], ['Mühlenweg', 'muehlenweg', 39],
    ['Schulstraße', 'schulstrasse', 44], ['Museumsplatz', 'museumsplatz', 12], ['Fischerweg', 'fischerweg', 27],
    ['Rosenstraße', 'rosenstrasse', 51], ['Gartenweg', 'gartenweg', 33], ['Leuchtturmweg', 'leuchtturmweg', 16],
    ['Bergstraße', 'bergstrasse', 46], ['Sandweg', 'sandweg', 25], ['Birkenweg', 'birkenweg', 38],
    ['Uferstraße', 'uferstrasse', 42], ['Kastanienweg', 'kastanienweg', 29]
]

Person = Struct.new(:name, :age, :job, :street, :house)

# --- The people with a part in the story -----------------------------------

KEY = {
    thief: Person.new('Konstantin Reuter', 34, 'Elektriker', 'Hafenstraße', 12),
    father: Person.new('Dieter Reuter', 71, 'Elektromeister im Ruhestand', 'Lindenallee', 4),
    sister: Person.new('Paula Reuter', 29, 'Tierärztin', 'Rosenstraße', 31),
    cousin: Person.new('Jonas Reuter', 16, 'Schüler', 'Birkenweg', 9),
    teacher: Person.new('Marlene Voss', 41, 'Sportlehrerin', 'Ahornweg', 7),
    baker: Person.new('Emil Schütte', 58, 'Bäcker', 'Am Markt', 3),
    physio: Person.new('Ayşe Demir', 27, 'Physiotherapeutin', 'Deichstraße', 18),
    witness: Person.new('Hannelore Kästner', 79, 'Rentnerin', 'Möwenweg', 23),
    neighbour: Person.new('Bernd Kowalczyk', 52, 'Taxifahrer', 'Möwenweg', 32),
    guard: Person.new('Ole Brandt', 62, 'Nachtwächter', 'Fischerweg', 5),
    cleaner: Person.new('Fatma Yıldız', 45, 'Reinigungskraft', 'Bahnhofstraße', 40),
    director: Person.new('Ingrid Hollerbach', 56, 'Museumsleiterin', 'Museumsplatz', 2),
    cashier: Person.new('Timo Brückner', 23, 'Kassierer im Museum', 'Schulstraße', 17)
}.freeze

WITNESS_FILE = 3817
NEIGHBOUR_FILE = 2290
GUARD_FILE = 1290
CARD = 4471

def pick(list) = list[RNG.rand(list.size)]

# --- People ------------------------------------------------------------------

def make_people
    taken = KEY.values.map { |p| [p.street, p.house] }
    names = KEY.values.map(&:name)
    people = KEY.values.dup
    while people.size < 1800
        female = RNG.rand < 0.5
        first, last = pick(female ? FEMALE : MALE), pick(LAST)
        name = "#{first} #{last}"
        next if names.include?(name) || last == 'Reuter'

        street, _, houses = pick(STREETS)
        house = RNG.rand(1..houses)
        # The houses of the story belong to their people alone
        next if taken.include?([street, house])

        age = RNG.rand(6..92)
        form = female ? 1 : 0
        job = if age < 19 then %w[Schüler Schülerin][form]
              elsif age > 67 then %w[Rentner Rentnerin][form]
              else pick(JOBS)[form]
              end
        names << name
        people << Person.new(name, age, job, street, house)
    end
    people
end

def people_file(people)
    lines = people.shuffle(random: RNG).map do |p|
        format('%-24s %5d   %-30s %s %d', p.name, p.age, p.job, p.street, p.house)
    end
    header = format('%-24s %5s   %-30s %s', 'Name', 'Alter', 'Beruf', 'Adresse')
    ([header] + lines).join("\n") + "\n"
end

# --- Cars ----------------------------------------------------------------------

COLOURS = (%w[schwarz] * 9 + %w[weiß] * 8 + %w[silber] * 7 + %w[grau] * 7 + %w[blau] * 5 +
           %w[rot] * 4 + %w[grün] * 2 + %w[gelb] * 1 + %w[braun] * 1 + %w[orange] * 1).freeze
KINDS = %w[Kleinwagen Kombi Limousine SUV Transporter Cabrio Kleinbus].freeze
LETTERS = ('A'..'Z').to_a - %w[Q]

def plate(first = nil, last_digit = nil)
    letters = (first || pick(LETTERS)) + (RNG.rand < 0.7 ? pick(LETTERS) : '')
    digits = RNG.rand(1..9999).to_s
    digits = digits[0...-1] + last_digit.to_s if last_digit
    "BS-#{letters} #{digits}"
end

def car_line(plate, colour, kind, owner)
    format('%-12s %-8s %-12s %s', plate, colour, kind, owner)
end

# The witness saw a yellow car, BS-K…, last digit 7.
def matches_witness?(plate, colour)
    plate.start_with?('BS-K') && colour == 'gelb' && plate.end_with?('7')
end

def make_cars(people)
    owners = people.reject { |p| p.age < 18 || KEY.values.include?(p) }
    cars = []
    plates = {}
    add = lambda do |pl, colour, kind, owner|
        return false if plates[pl]

        plates[pl] = true
        cars << car_line(pl, colour, kind, owner)
        true
    end
    while cars.size < 1500
        pl, colour = plate, pick(COLOURS)
        next if matches_witness?(pl, colour)

        add.(pl, colour, pick(KINDS), pick(owners).name)
    end
    # Near misses: yellow BS-K without a 7 at the end, yellow with a 7 elsewhere
    6.times { pl = plate('K'); add.(pl, 'gelb', pick(KINDS), pick(owners).name) unless pl.end_with?('7') }
    5.times { add.(plate(pick(LETTERS - %w[K]), 7), 'gelb', pick(KINDS), pick(owners).name) }
    [['BS-KR 2417', 'Kombi', :thief], ['BS-KM 3107', 'Kleinwagen', :teacher],
     ['BS-KE 8807', 'Transporter', :baker], ['BS-KD 1147', 'Kleinwagen', :physio]].each do |pl, kind, who|
        add.(pl, 'gelb', kind, KEY[who].name)
    end
    # The father has a car too, a silver one
    add.('BS-DR 1953', 'silber', 'Limousine', KEY[:father].name)
    cars.shuffle(random: RNG)
end

# --- Clubs ------------------------------------------------------------------------

def make_clubs(people)
    pool = people.reject { |p| KEY.values.include?(p) || p.age < 12 }.shuffle(random: RNG)
    both = pool.shift(4)
    chess = pool.shift(86)
    gym = pool.shift(240)
    choir_and_bowling = pool.shift(3)
    choir = pool.shift(55)
    bowling = pool.shift(38)
    chess += both + [KEY[:thief], KEY[:teacher], KEY[:baker], KEY[:father]]
    gym += both + [KEY[:thief], KEY[:teacher], KEY[:physio]]
    choir += choir_and_bowling + [KEY[:witness]]
    bowling += choir_and_bowling + [KEY[:guard]]
    {
        'schachclub.txt' => chess, 'muskelkater.txt' => gym,
        'chor.txt' => choir, 'kegelclub.txt' => bowling
    }.transform_values { |list| list.shuffle(random: RNG).map(&:name).join("\n") + "\n" }
end

# --- Interviews and streets ----------------------------------------------------------

BORING = [
    'Ich habe die ganze Nacht geschlafen, wie ein Stein.',
    'Ich war bis Mitternacht auf dem Sofa und habe einen Krimi geschaut. Ironie, oder?',
    'Der Hund der Nachbarn hat einmal gebellt, so gegen zehn. Sonst nichts.',
    'Es war windig, die Mülltonnen sind umgefallen. Das war aber vor elf.',
    'Ich habe Nachtschicht gehabt und war gar nicht zu Hause.',
    'Bei uns im Haus war Geburtstag, es war laut. Gesehen habe ich nichts.',
    'Ich habe nur das Nebelhorn vom Hafen gehört, wie jede Nacht.',
    'Ich war im Kino und danach direkt im Bett.',
    'Ich habe noch lange am Computer gesessen, mit Kopfhörern. Keine Ahnung, was draußen war.',
    'Um halb elf ist bei uns der Strom kurz ausgefallen, aber nur für eine Minute.',
    'Ich bin um neun ins Bett gegangen. Ich stehe um vier auf, ich backe Brötchen.',
    'Die Goldene Diskette? Die habe ich mal beim Schulausflug gesehen. Sah aus wie eine normale Diskette, nur gelb.',
    'Ich habe eine Katze auf dem Garagendach gesehen. Mehr nicht.',
    'Ich wusste gar nicht, dass das Museum so etwas Wertvolles hat.',
    'Mein Mann schnarcht, ich habe also eher wenig geschlafen, aber aus dem Fenster geschaut habe ich nicht.',
    'Ich habe meine Steuererklärung gemacht. Das war schlimm genug.',
    'Es hat geregnet, glaube ich. Oder war das die Nacht davor?',
    'Ich habe Schach gespielt, online, bis ein Uhr. Ich habe verloren.',
    'Wir haben Besuch aus Hamburg gehabt und lange gegessen.',
    'Ich habe nichts gehört. Ich höre aber auch schlecht.'
].freeze
MENTIONS_GUARD = [
    'Ole Brandt kenne ich vom Kegeln. Ein netter Mann, der würde nie etwas stehlen.',
    'Der Nachtwächter, Ole Brandt, grüßt immer freundlich, wenn er um neun zur Arbeit radelt.',
    'Ich habe gehört, Ole Brandt hat die ganze Aufregung verschlafen. Aber das ist nur Gerede.'
].freeze

def interview(number, person, date, officer, text)
    <<~TEXT
        Befragung #{number}
        Befragt: #{person.name}, #{person.street} #{person.house}
        Datum: #{date}
        Befragt von: #{officer}

        #{text}
    TEXT
end

OFFICERS = ['Kommissarin Weber', 'Kommissar Okafor', 'Polizeimeisterin Nguyen'].freeze

def interview_date
    format('%d. März 2026, %02d:%02d Uhr', RNG.rand(15..17), RNG.rand(8..18), RNG.rand(0..11) * 5)
end

def make_interviews(people)
    numbers = (1000..9999).to_a - [WITNESS_FILE, NEIGHBOUR_FILE, GUARD_FILE]
    numbers = numbers.shuffle(random: RNG).first(1500)
    plain = people.reject { |p| KEY.values.include?(p) }
    files = {}
    by_house = {}
    numbers.each_with_index do |number, i|
        person = pick(plain)
        sentences = BORING.sample(RNG.rand(1..3), random: RNG)
        sentences << MENTIONS_GUARD[i] if i < MENTIONS_GUARD.size
        files[number] = interview(number, person, interview_date, pick(OFFICERS), sentences.join(' '))
        by_house[[person.street, person.house]] ||= [person, number]
    end
    files[WITNESS_FILE] = interview(WITNESS_FILE, KEY[:witness], '15. März 2026, 09:40 Uhr', 'Kommissarin Weber', <<~TEXT.strip)
        Kurz nach elf bin ich noch einmal mit Krümel raus, das ist mein Dackel.
        Da kam vom Museum her ein Auto die Straße herunter, ohne Licht, und
        ziemlich schnell. Es war gelb, das habe ich unter der Laterne genau
        gesehen. Vom Kennzeichen habe ich nur den Anfang erkannt, BS-K, und
        die letzte Ziffer, eine 7. Wer am Steuer saß, konnte ich nicht sehen.
        Krümel hat gebellt wie verrückt.
    TEXT
    files[NEIGHBOUR_FILE] = interview(NEIGHBOUR_FILE, KEY[:neighbour], '15. März 2026, 10:05 Uhr', 'Kommissar Okafor', <<~TEXT.strip)
        Ein Auto? Ja, meins. Ich bin um neun von der letzten Taxifahrt
        gekommen und habe es vor dem Haus geparkt. Danach war Ruhe.
    TEXT
    files[GUARD_FILE] = interview(GUARD_FILE, KEY[:guard], '15. März 2026, 08:30 Uhr', 'Kommissarin Weber', <<~TEXT.strip)
        Um elf habe ich meine Runde im Erdgeschoss gemacht, alles ruhig. Kurz
        nach halb zwölf hörte ich hinten etwas klappern. Als ich am
        Hintereingang war, rannte gerade jemand über den Hof, groß, mit
        Kapuze und einer Sporttasche. Auf der Tasche stand MUSKELKATER, das
        ist das Fitnessstudio am Markt. Ich hinterher, aber der war schneller.
        Die Alarmanlage hat keinen Mucks gemacht. Das verstehe ich bis heute
        nicht, die Tür ist doch gesichert.
    TEXT
    [files, by_house]
end

NOBODY = [
    'Niemand angetroffen.', 'Niemand angetroffen.', 'Haus steht leer.', 'Ferienwohnung, gerade nicht vermietet.',
    'Niemand angetroffen, Zettel in den Briefkasten geworfen.'
].freeze
QUIET = [
    ['hat geschlafen, nichts gehört.', 'haben geschlafen, nichts gehört.'],
    ['war verreist.', 'waren verreist.'],
    ['hat nichts bemerkt.', 'haben nichts bemerkt.'],
    ['hatte Nachtdienst im Krankenhaus.', 'hatten beide Nachtdienst.'],
    ['hat ferngesehen, nichts bemerkt.', 'haben ferngesehen, nichts bemerkt.'],
    ['schläft immer mit Ohrstöpseln.', 'schlafen immer mit Ohrstöpseln.']
].freeze

def make_streets(people, by_house)
    residents = people.group_by { |p| [p.street, p.house] }
    STREETS.to_h do |street, file, houses|
        lines = (1..houses).map do |house|
            if [street, house] == [KEY[:witness].street, KEY[:witness].house]
                "#{KEY[:witness].name}: hat in der Nacht ein Auto gesehen, siehe befragungen/befragung-#{WITNESS_FILE}.txt"
            elsif [street, house] == [KEY[:neighbour].street, KEY[:neighbour].house]
                "#{KEY[:neighbour].name}: hat ein Auto gesehen, siehe befragungen/befragung-#{NEIGHBOUR_FILE}.txt"
            elsif by_house[[street, house]]
                person, number = by_house[[street, house]]
                what = pick(['hat etwas gehört', 'hat ein Auto gehört', 'will etwas gesehen haben', 'hat etwas zu erzählen', 'hat Licht gesehen'])
                "#{person.name}: #{what}, siehe befragungen/befragung-#{number}.txt"
            elsif (here = residents[[street, house]]) && RNG.rand < 0.8
                names = here.first(2).map(&:name)
                "#{names.join(' und ')}: #{pick(QUIET)[names.size - 1]}"
            else
                pick(NOBODY)
            end
        end
        ["#{file}.txt", lines.join("\n") + "\n"]
    end
end

# --- The crime scene report --------------------------------------------------------

ROOMS = ['Foyer', 'Lesesaal', 'Saal 1', 'Saal 2', 'Saal 3', 'Treppenhaus', 'Büro der Museumsleitung', 'Hof', 'Garderobe', 'Keller'].freeze
THINGS = ['Vitrine %d', 'Fenster %d', 'Tür %d', 'Bilderrahmen %d', 'Heizkörper %d', 'Lichtschalter %d'].freeze

def thing = format(pick(THINGS), RNG.rand(1..12))

FILLER = [
    -> { "#{pick(ROOMS)}, #{thing}: Staub, keine Spuren." },
    -> { "#{pick(ROOMS)}, #{thing}: untersucht, kein Hinweis auf Gewalt." },
    -> { "Fingerabdrücke an #{thing} (#{pick(ROOMS)}) gesichert, gehen ins Labor." },
    -> { "Foto von #{thing} im Raum #{pick(ROOMS)} gemacht." },
    -> { "Besucherbuch durchgesehen, Seite #{RNG.rand(1..240)}: nichts Auffälliges." },
    -> { "Fußabdruck im Raum #{pick(ROOMS)} vermessen, Größe #{RNG.rand(36..46)}, vermutlich vom Reinigungspersonal." },
    -> { "Spur Nr. #{RNG.rand(100..999)} beschriftet und eingepackt." },
    -> { "#{pick(ROOMS)}: Bewegungsmelder geprüft, funktioniert." },
    -> { "Mit der Versicherung telefoniert, kein Hinweis auf frühere Einbrüche." },
    -> { "#{pick(ROOMS)}: Mülleimer durchsucht, Bonbonpapier und ein Busfahrschein." },
    -> { "#{pick(ROOMS)}: Boden abgeklebt, Haare gesichert." },
    -> { "Vitrine der Goldenen Diskette vermessen: #{RNG.rand(40..60)} cm breit." },
    -> { "Schlüsselbund des Hausmeisters geprüft, alle #{RNG.rand(8..14)} Schlüssel da." },
    -> { "#{pick(ROOMS)}: Kamera geprüft, seit Jahren außer Betrieb." }
].freeze
COFFEE = [
    'Kaffee aus dem Automaten im Foyer geholt. Schmeckt furchtbar.',
    'Zweiter Kaffee. Schmeckt immer noch furchtbar.',
    'Neuer Kaffee aus der Bäckerei am Markt, viel besser.',
    'Kaffee über den Lageplan gekippt. Neuen Lageplan bestellt.'
].freeze
HINTS = [
    'HINWEIS: Am Fenster des Lesesaals hing ein Faden von einem Schal, schwarz-weiß kariert. Das sind die Farben des Schachclubs Bitstein. Die Mitgliederlisten der Vereine liegen in vereine/.',
    "HINWEIS: Eine Anwohnerin aus dem Möwenweg hat in der Nacht ein Auto gesehen. Sie wohnt in Hausnummer #{KEY[:witness].house}. Was die Leute in jeder Straße gesagt haben, steht in strassen/, eine Zeile pro Haus, in der Reihenfolge der Hausnummern.",
    'HINWEIS: Wer in Bitstein ein Auto hat, steht in fahrzeuge.txt, wer hier wohnt, in personen.txt.',
    'HINWEIS: Der Nachtwächter Ole Brandt wurde auch befragt. Welche der Befragungen in befragungen/ seine ist, hat leider niemand notiert.',
    'HINWEIS: Die Alarmanlage hat nicht ausgelöst. Sie schreibt alles, was sie tut, in ein Git-Repository: alarmanlage/.'
].freeze
SIGN = 'Schild »HINWEIS: Bitte nicht berühren!« an Vitrine 4 im Saal 2 fotografiert.'

def make_report
    officers = ['Weber', 'Okafor', 'Nguyen', 'Albers (Spurensicherung)']
    minutes = 8 * 60 + 15
    lines = []
    1260.times do
        minutes += RNG.rand(0..1)
        lines << format('%02d:%02d %s: %s', minutes / 60, minutes % 60, pick(officers), FILLER.sample(random: RNG).call)
    end
    COFFEE.each_with_index { |text, i| lines[150 + i * 300] = lines[150 + i * 300][0, 6] + 'Okafor: ' + text }
    lines[611] = lines[611][0, 6] + 'Nguyen: ' + SIGN
    [88, 402, 517, 840, 1103].zip(HINTS).reverse_each { |at, hint| lines.insert(at, hint) }
    lines.join("\n") + "\n"
end

README = <<~TEXT
    POLIZEI BITSTEIN, KRIMINALKOMMISSARIAT
    Fall: Diebstahl der Goldenen Diskette aus dem Stadtmuseum

    Liebe Kollegin, lieber Kollege,

    in der Nacht vom 14. auf den 15. März ist aus dem Stadtmuseum Bitstein
    die Goldene Diskette verschwunden: eine Diskette aus Gold, auf der das
    erste Programm steht, das je in Bitstein geschrieben wurde (Computer-AG
    der Hafenschule, 1987). Die Vitrine ist leer, die Alarmanlage hat
    geschwiegen, und die Bürgermeisterin ruft alle zehn Minuten an.

    Alles, was wir bisher haben, liegt in diesem Ordner:

      tatort.txt        der Bericht vom Tatort
      befragungen/      alle Befragungen, eine Datei pro Befragung
      strassen/         was die Leute in jeder Straße gesagt haben
      personen.txt      alle, die in Bitstein wohnen
      fahrzeuge.txt     alle Autos in Bitstein
      vereine/          Mitgliederlisten der Vereine
      alarmanlage/      das Protokoll der Alarmanlage

    Fang mit dem Bericht vom Tatort an. Er ist lang. Die wichtigen Stellen
    haben wir mit HINWEIS markiert, am Anfang der Zeile.

    Viel Erfolg!
    Kommissarin Weber
TEXT

# --- The alarm system's Git repository ----------------------------------------------

SYSTEM = ['Alarmanlage', 'anlage@museum-bitstein.de'].freeze
SERVICE = ['Wartung', 'wartung@museum-bitstein.de'].freeze

CARDS = <<~TEXT
    # Schlüsselkarten für das Stadtmuseum Bitstein
    1001  Ingrid Hollerbach, Museumsleitung
    1002  Ole Brandt, Nachtwache
    1003  Fatma Yıldız, Reinigung
    1004  Timo Brückner, Kasse
    1005  Wartung, Firma Nordlicht Sicherheitstechnik
    #{CARD}  Elektro Reuter, Einbau der Anlage 2024 (Karte nie zurückgegeben)
TEXT

SETTINGS_ON = <<~TEXT
    # Einstellungen der Alarmanlage. Die Anlage übernimmt sie mit jedem Commit.
    alarm = an
    hintereingang = gesichert
    bewegungsmelder = an
TEXT
SETTINGS_SENSITIVE = SETTINGS_ON.sub("bewegungsmelder = an\n", "bewegungsmelder = an\nempfindlichkeit = mittel\n")
SETTINGS_OFF = SETTINGS_SENSITIVE.sub('alarm = an', 'alarm = aus').sub('hintereingang = gesichert', 'hintereingang = offen')

REPO_README = <<~TEXT
    Alarmanlage Stadtmuseum Bitstein

    Die Anlage schreibt jeden Zugang mit einer Schlüsselkarte in
    zugaenge.log und macht dafür sofort einen Commit. Einstellungen stehen in
    einstellungen.txt; die Anlage übernimmt sie mit dem nächsten Commit.
    Einstellungen ändert nur die Wartung. Wem welche Karte gehört, steht in
    karten.txt.
TEXT

class Repo
    def initialize(dir)
        @dir = dir
        FileUtils.mkdir_p(dir)
        git('init', '-q', '-b', 'main')
    end

    def git(*args, env: {})
        base = { 'GIT_CONFIG_NOSYSTEM' => '1', 'GIT_CONFIG_GLOBAL' => '/dev/null', 'TZ' => 'Europe/Berlin' }
        out, status = Open3.capture2e(base.merge(env), 'git', '-c', 'commit.gpgsign=false', *args, :chdir => @dir)
        raise "git #{args.join(' ')}: #{out}" unless status.success?

        out
    end

    def write(name, text)
        File.write(File.join(@dir, name), text)
    end

    def read(name)
        File.read(File.join(@dir, name))
    end

    def commit(time, author, message)
        name, mail = author
        date = "#{time} +0100"
        git('add', '-A')
        git('commit', '-q', '--allow-empty', '-m', message, :env => {
            'GIT_AUTHOR_NAME' => name, 'GIT_AUTHOR_EMAIL' => mail, 'GIT_AUTHOR_DATE' => date,
            'GIT_COMMITTER_NAME' => name, 'GIT_COMMITTER_EMAIL' => mail, 'GIT_COMMITTER_DATE' => date
        })
    end

    def log_entry(time, card, door)
        write('zugaenge.log', read('zugaenge.log') + "#{time[0, 16]}  Karte #{card}  #{door}\n")
        commit(time, SYSTEM, 'Protokoll: neuer Eintrag')
    end
end

def make_repo(dir)
    repo = Repo.new(dir)
    repo.write('LIESMICH.txt', REPO_README)
    repo.write('einstellungen.txt', SETTINGS_ON)
    repo.write('karten.txt', CARDS.sub(/^1005.*\n/, '').sub(/^#{CARD}.*\n/o, '') + "#{CARD}  Elektro Reuter, Einbau der Anlage\n")
    repo.write('zugaenge.log', '')
    repo.commit('2024-05-06 14:20:00', ['Elektro Reuter', 'service@elektro-reuter.de'], 'Anlage eingerichtet')
    repo.write('karten.txt', CARDS)
    repo.commit('2025-01-09 10:02:00', ['Ingrid Hollerbach', 'hollerbach@museum-bitstein.de'], 'Wartung geht an Nordlicht Sicherheitstechnik')
    repo.write('zugaenge.log', '')
    repo.commit('2026-03-01 00:00:00', SYSTEM, 'Protokoll bis Februar archiviert')

    # Everything that happens, in the order it happens: [time, card, door]
    # for a card at a door, [time, proc] for anything else.
    timeline = []
    (1..15).each do |day|
        date = format('2026-03-%02d', day)
        timeline << ["#{date} 06:0#{RNG.rand(0..9)}:00", 1002, 'Haupteingang'] if day > 1
        break if day == 15 && timeline.push(["#{date} 07:48:00", 1004, 'Haupteingang'], ["#{date} 08:04:00", 1001, 'Haupteingang'])

        unless [2, 9].include?(day) # montags zu
            timeline << ["#{date} 07:#{RNG.rand(40..55)}:00", 1004, 'Haupteingang']
            timeline << ["#{date} 08:#{format('%02d', RNG.rand(0..20))}:00", 1001, 'Haupteingang']
        end
        if day == 3
            timeline << ["#{date} 10:12:00", 1005, 'Hintereingang']
            timeline << ["#{date} 10:15:00", lambda { |t|
                repo.write('einstellungen.txt', SETTINGS_SENSITIVE)
                repo.commit(t, SERVICE, 'Wartung: Bewegungsmelder im Lesesaal neu eingestellt')
            }]
        end
        timeline << ["#{date} 20:5#{RNG.rand(0..9)}:00", 1003, 'Hintereingang']
        timeline << ["#{date} 21:#{RNG.rand(40..59)}:00", 1002, 'Haupteingang']
        timeline << ["#{date} 22:#{RNG.rand(20..35)}:00", 1003, 'Hintereingang']
    end
    timeline << ['2026-03-14 23:09:00', lambda { |t|
        repo.write('einstellungen.txt', SETTINGS_OFF)
        repo.commit(t, SERVICE, 'Wartung: Alarm vorübergehend aus')
    }]
    timeline << ['2026-03-14 23:12:00', CARD, 'Hintereingang']
    timeline << ['2026-03-14 23:38:00', CARD, 'Hintereingang']
    # Alarm back on, and the two lines of the card quietly gone
    timeline << ['2026-03-14 23:41:00', lambda { |t|
        repo.write('einstellungen.txt', SETTINGS_SENSITIVE)
        repo.write('zugaenge.log', repo.read('zugaenge.log').lines.reject { |l| l.include?("Karte #{CARD}") }.join)
        repo.commit(t, SERVICE, 'Wartung beendet')
    }]
    timeline << ['2026-03-15 08:31:00', lambda { |t|
        repo.write('NOTIZ.txt', "Die Goldene Diskette ist weg! Die Polizei ist verständigt. I. H.\n")
        repo.commit(t, ['Ingrid Hollerbach', 'hollerbach@museum-bitstein.de'], 'Die Diskette ist weg')
    }]
    timeline.sort_by(&:first).each do |time, what, door|
        what.is_a?(Proc) ? what.call(time) : repo.log_entry(time, what, door)
    end
    repo
end

# --- Everything together -------------------------------------------------------------

def build(root)
    FileUtils.rm_rf(root)
    %w[strassen befragungen vereine].each { |d| FileUtils.mkdir_p(File.join(root, d)) }
    people = make_people
    interviews, by_house = make_interviews(people)
    File.write(File.join(root, 'LIESMICH.txt'), README)
    File.write(File.join(root, 'tatort.txt'), make_report)
    File.write(File.join(root, 'personen.txt'), people_file(people))
    File.write(File.join(root, 'fahrzeuge.txt'), make_cars(people).join("\n") + "\n")
    make_clubs(people).each { |name, text| File.write(File.join(root, 'vereine', name), text) }
    make_streets(people, by_house).each { |name, text| File.write(File.join(root, 'strassen', name), text) }
    interviews.each { |number, text| File.write(File.join(root, 'befragungen', "befragung-#{number}.txt"), text) }
    make_repo(File.join(root, 'alarmanlage'))
end

def pack(root)
    tar = ['tar', '--sort=name', '--mtime=2026-03-15 12:00', '--owner=0', '--group=0', '--numeric-owner',
           '--mode=u+rw,go+r,go-w', '-C', File.dirname(root), '-cf', '-', File.basename(root)]
    data, status = Open3.capture2(*tar, :binmode => true)
    raise 'tar failed' unless status.success?

    gz, status = Open3.capture2('gzip', '-n', '-9', :stdin_data => data, :binmode => true)
    raise 'gzip failed' unless status.success?

    File.binwrite(ARCHIVE, gz)
    FileUtils.mkdir_p(File.dirname(PUBLISHED))
    FileUtils.cp(ARCHIVE, PUBLISHED)
end

# The numbers the tasks need, worked out the way students do it.
def report(root)
    sh = ->(cmd) { Open3.capture2('sh', '-c', cmd, :chdir => root).first }
    puts "Archive: #{ARCHIVE} (#{File.size(ARCHIVE) / 1024} kB)"
    puts "tatort.txt: #{sh.('wc -l < tatort.txt').strip} lines, HINWEIS anywhere: #{sh.('grep -c HINWEIS tatort.txt').strip}, " \
         "^HINWEIS: #{sh.("grep -c '^HINWEIS' tatort.txt").strip}, -i hinweis: #{sh.('grep -ic hinweis tatort.txt').strip}"
    puts "Kaffee: #{sh.('grep -c Kaffee tatort.txt').strip}"
    puts "Möwenweg 23: #{sh.('head -n 23 strassen/moewenweg.txt | tail -n 1')}"
    puts "Ole Brandt in: #{sh.('grep -l "Ole Brandt" befragungen/*').split.join(' ')}"
    puts "Kegeln in: #{sh.('grep -l Kegeln befragungen/* | wc -l').strip} files"
    puts "Cars: #{sh.('wc -l < fahrzeuge.txt').strip}, matching:\n#{sh.("grep '^BS-K' fahrzeuge.txt | grep gelb | grep '7 '")}"
    puts "BS-K gelb: #{sh.("grep '^BS-K' fahrzeuge.txt | grep -c gelb").strip}"
    puts "Chess and gym:\n#{sh.('sort vereine/schachclub.txt vereine/muskelkater.txt | uniq -d')}"
    puts "Choir and bowling:\n#{sh.('sort vereine/chor.txt vereine/kegelclub.txt | uniq -d')}"
    puts "Reuter: #{sh.('grep Reuter personen.txt')}"
    puts "Commits: #{sh.('git -C alarmanlage rev-list --count HEAD').strip}"
    puts sh.('git -C alarmanlage log --oneline --author=Wartung')
end

if __FILE__ == $0
    if ARGV[0]
        root = File.expand_path(File.join(ARGV[0], 'bitstein'))
        build(root)
        pack(root)
        report(root)
    else
        Dir.mktmpdir('bitstein') do |tmp|
            root = File.join(tmp, 'bitstein')
            build(root)
            pack(root)
            report(root)
        end
    end
end
