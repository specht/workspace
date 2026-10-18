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
# Gibt die IDs vom Start bis zum Ziel zurück, nil, wenn das Ziel nicht dabei ist.
def path_to(previous, goal)
    raise NotImplementedError, 'path_to'
end

# Breitensuche: der Weg über die wenigsten Kanten. Gibt zurück:
#   { path: [Start-ID, …, Ziel-ID] (oder nil, wenn es keinen Weg gibt),
#     visited: [ID, …] }  die besuchten Knoten, in der Reihenfolge der Suche
def breadth_first(graph, start, goal)
    raise NotImplementedError, 'breadth_first'
end

# Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadth_first.
def dijkstra(graph, start, goal)
    raise NotImplementedError, 'dijkstra'
end

# Eine Prioritätswarteschlange: push legt einen Wert mit einer Priorität
# hinein, pop gibt das Paar [Priorität, Wert] mit der kleinsten Priorität
# zurück und nimmt es heraus (nil, wenn sie leer ist).
class MinHeap
    def initialize
        @items = [] # [Priorität, Wert], als binärer Heap
    end

    def empty?
        @items.empty?
    end

    def size
        @items.size
    end

    def push(priority, value)
        raise NotImplementedError, 'MinHeap#push'
    end

    def pop
        raise NotImplementedError, 'MinHeap#pop'
    end
end

# A*: wie Dijkstra, mit der Luftlinie zum Ziel als Schätzung für den Rest.
# Gibt dasselbe zurück wie breadth_first.
def a_star(graph, start, goal)
    raise NotImplementedError, 'a_star'
end

# Wie schnell man auf welcher Art von Weg vorankommt, in km/h. Was in der
# Liste einer Art zu reisen fehlt, darf man so nicht benutzen.
SPEEDS = {
    'foot' => { 'footway' => 5, 'path' => 5, 'pedestrian' => 5, 'steps' => 2, 'living_street' => 5,
                'residential' => 5, 'service' => 5, 'unclassified' => 5, 'tertiary' => 5, 'secondary' => 5,
                'primary' => 5, 'track' => 5, 'cycleway' => 5, 'bridleway' => 5, 'ferry' => 5 },
    'bike' => { 'cycleway' => 18, 'residential' => 15, 'living_street' => 10, 'service' => 12,
                'unclassified' => 15, 'tertiary' => 16, 'secondary' => 16, 'primary' => 16,
                'tertiary_link' => 15, 'secondary_link' => 15, 'primary_link' => 15,
                'path' => 12, 'track' => 10, 'bridleway' => 10, 'ferry' => 10 },
    'car' => { 'motorway' => 90, 'motorway_link' => 50, 'trunk' => 60, 'trunk_link' => 40,
               'primary' => 50, 'primary_link' => 30, 'secondary' => 45, 'secondary_link' => 30,
               'tertiary' => 40, 'tertiary_link' => 30, 'unclassified' => 30, 'residential' => 25,
               'living_street' => 7, 'service' => 10, 'ferry' => 10 }
}.freeze

# Der Graph für eine Art zu reisen ('foot', 'bike', 'car'): wie build_graph,
# aber nur mit den Wegen, die man so benutzen darf, und mit Sekunden statt
# Metern an den Kanten. Für das Auto gelten die Einbahnstraßen.
#   { nodes: …, edges: …, fastest: das größte Tempo in Metern je Sekunde }
def build_travel_graph(ways, mode)
    raise NotImplementedError, 'build_travel_graph'
end

# Eine Rundfahrt durch mehrere Orte: costs[i][j] sagt, wie weit es von Ort i
# zu Ort j ist. Eine Reihenfolge ist ein Array von Nummern der Orte; sie
# beginnt bei 0 und kehrt am Ende zu ihrem Anfang zurück.

# Wie lang eine Rundfahrt in dieser Reihenfolge ist, mit dem Weg zurück
def tour_length(costs, order)
    raise NotImplementedError, 'tour_length'
end

# Die beste Reihenfolge, durch Ausprobieren aller
def best_tour(costs)
    raise NotImplementedError, 'best_tour'
end

# Immer zum nächsten Ort, an dem die Rundfahrt noch nicht war
def nearest_tour(costs)
    raise NotImplementedError, 'nearest_tour'
end

# Eine Reihenfolge mit 2-opt verbessern: ein Stück umdrehen, solange die
# Rundfahrt dadurch kürzer wird
def improve_tour(costs, order)
    raise NotImplementedError, 'improve_tour'
end
