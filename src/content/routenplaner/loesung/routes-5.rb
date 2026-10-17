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

# Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
# gefundenen Knoten, von welchem aus er gefunden wurde (der Start von nil).
# nil, wenn das Ziel nie gefunden wurde.
def path_to(previous, goal)
    return nil unless previous.key?(goal)

    path = []
    node = goal
    while node
        path.unshift(node)
        node = previous[node]
    end
    path
end

# Breitensuche: der Weg über die wenigsten Kanten. Gibt den Weg zurück (die
# IDs vom Start bis zum Ziel, nil ohne Weg) und alle besuchten Knoten in der
# Reihenfolge, in der die Suche sie aus der Warteschlange genommen hat.
def breadth_first(graph, start, goal)
    previous = { start => nil }
    queue = [start]
    visited = []
    until queue.empty?
        node = queue.shift
        visited << node
        break if node == goal

        graph[:edges][node].each do |neighbour, _metres|
            next if previous.key?(neighbour)

            previous[neighbour] = node
            queue << neighbour
        end
    end
    { path: path_to(previous, goal), visited: visited }
end

# Dijkstra: der kürzeste Weg in Metern. Als Nächstes ist immer der Knoten
# dran, der vom Start aus am wenigsten weit weg ist.
def dijkstra(graph, start, goal)
    metres = { start => 0 }
    previous = { start => nil }
    frontier = { start => 0 } # gefunden, aber noch nicht fertig
    visited = []
    until frontier.empty?
        node, so_far = frontier.min_by { |_id, m| m }
        frontier.delete(node)
        visited << node
        break if node == goal

        graph[:edges][node].each do |neighbour, length|
            total = so_far + length
            next if metres.key?(neighbour) && metres[neighbour] <= total

            metres[neighbour] = total
            previous[neighbour] = node
            frontier[neighbour] = total
        end
    end
    { path: path_to(previous, goal), visited: visited }
end
