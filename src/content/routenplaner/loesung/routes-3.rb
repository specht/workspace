# Ein Routenplaner: deine Funktionen. server.rb ruft sie auf und zeigt im
# Browser, was sie schon können.

EARTH_RADIUS = 6_371_000 # Meter

# Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
# durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
# sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
#
# Gibt ein Array von Hashes zurück, einen pro Weg:
#   { kind: 'residential', oneway: '', name: 'Schloßstraße',
#     points: [['26735834', 13.3266, 52.4567], …] }
def read_ways(path)
    File.readlines(path, chomp: true).map do |line|
        kind, oneway, name, points = line.split("\t", 4)
        points = points.split(' ').each_slice(2).map do |id, position|
            lon, lat = position.split(',').map(&:to_f)
            [id, lon, lat]
        end
        { kind: kind, oneway: oneway, name: name, points: points }
    end
end

# Der Abstand zweier Punkte in Metern: ein Grad Breite ist überall gleich
# lang, ein Grad Länge wird zu den Polen hin kürzer (um den Cosinus der
# Breite). Für die Strecken einer Stadt genau genug.
def distance(lon1, lat1, lon2, lat2)
    radians = Math::PI / 180
    x = (lon2 - lon1) * radians * Math.cos((lat1 + lat2) / 2 * radians)
    y = (lat2 - lat1) * radians
    Math.sqrt(x * x + y * y) * EARTH_RADIUS
end

# Der Graph aus den Wegen:
#   nodes: { ID => [Länge, Breite] } für jeden Punkt
#   edges: { ID => [[Nachbar-ID, Meter], …] } für jeden Punkt, in beide Richtungen
def build_graph(ways)
    nodes = {}
    edges = Hash.new { |hash, id| hash[id] = [] }
    ways.each do |way|
        way[:points].each_cons(2) do |(a, lon1, lat1), (b, lon2, lat2)|
            nodes[a] = [lon1, lat1]
            nodes[b] = [lon2, lat2]
            metres = distance(lon1, lat1, lon2, lat2)
            edges[a] << [b, metres]
            edges[b] << [a, metres]
        end
    end
    { nodes: nodes, edges: edges }
end
