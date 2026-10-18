# Ein Routenplaner: deine Funktionen. server.rb ruft sie auf und zeigt im
# Browser, was sie schon können. (Die Lösung aller Schritte.)

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
# Reihenfolge, in der die Suche sie aus der Queue genommen hat.
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

# Eine Priority Queue als binärer Heap: Was hineinkommt, hat eine
# Priorität, und pop gibt immer das mit der kleinsten zurück. Die Einträge
# stehen in einem Array, als Baum: Die Kinder von i stehen bei 2i+1 und 2i+2,
# und kein Kind ist kleiner als sein Elternteil.
class MinHeap
    def initialize
        @items = [] # [Priorität, Wert]
    end

    def empty?
        @items.empty?
    end

    def size
        @items.size
    end

    def push(priority, value)
        @items << [priority, value]
        i = @items.size - 1
        # nach oben, solange das Elternteil größer ist
        while i > 0
            parent = (i - 1) / 2
            break if @items[parent][0] <= @items[i][0]

            @items[parent], @items[i] = @items[i], @items[parent]
            i = parent
        end
    end

    def pop
        return nil if @items.empty?

        top = @items[0]
        last = @items.pop
        unless @items.empty?
            @items[0] = last
            i = 0
            # nach unten, solange ein Kind kleiner ist
            loop do
                smallest = i
                [2 * i + 1, 2 * i + 2].each do |child|
                    smallest = child if child < @items.size && @items[child][0] < @items[smallest][0]
                end
                break if smallest == i

                @items[smallest], @items[i] = @items[i], @items[smallest]
                i = smallest
            end
        end
        top
    end
end

# Dijkstra: der kürzeste Weg, mit einer Priority Queue. Ein Knoten
# kann mehrmals darin stehen, wenn er einen besseren Weg bekommt; der alte
# Eintrag ist dann veraltet und wird übersprungen.
def dijkstra(graph, start, goal)
    costs = { start => 0 }
    previous = { start => nil }
    done = {}
    heap = MinHeap.new
    heap.push(0, start)
    visited = []
    until heap.empty?
        so_far, node = heap.pop
        next if done[node]

        done[node] = true
        visited << node
        break if node == goal

        graph[:edges][node].each do |neighbour, length|
            total = so_far + length
            next if costs.key?(neighbour) && costs[neighbour] <= total

            costs[neighbour] = total
            previous[neighbour] = node
            heap.push(total, neighbour)
        end
    end
    { path: path_to(previous, goal), visited: visited }
end

# A*: wie Dijkstra, aber die Priorität ist der Weg bis hier plus eine
# Schätzung für den Rest, die Luftlinie zum Ziel. Die Schätzung ist nie zu
# groß, darum findet A* trotzdem den kürzesten Weg. In einem Graphen mit
# Sekunden statt Metern teilt man die Luftlinie durch das größte Tempo.
def a_star(graph, start, goal)
    goal_lon, goal_lat = graph[:nodes][goal]
    fastest = graph.fetch(:fastest, 1.0)
    estimate = lambda do |id|
        lon, lat = graph[:nodes][id]
        distance(lon, lat, goal_lon, goal_lat) / fastest
    end
    costs = { start => 0 }
    previous = { start => nil }
    done = {}
    heap = MinHeap.new
    heap.push(estimate.(start), start)
    visited = []
    until heap.empty?
        _priority, node = heap.pop
        next if done[node]

        done[node] = true
        visited << node
        break if node == goal

        graph[:edges][node].each do |neighbour, length|
            total = costs[node] + length
            next if costs.key?(neighbour) && costs[neighbour] <= total

            costs[neighbour] = total
            previous[neighbour] = node
            heap.push(total + estimate.(neighbour), neighbour)
        end
    end
    { path: path_to(previous, goal), visited: visited }
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

# Der Graph für eine Art zu reisen: nur die Wege, die man so benutzen darf,
# und als Länge jeder Kante, wie viele Sekunden man für sie braucht. Für das
# Auto gelten die Einbahnstraßen. fastest: das größte Tempo, in Metern je
# Sekunde, für die Schätzung von A*.
def build_travel_graph(ways, mode)
    speeds = SPEEDS.fetch(mode)
    nodes = {}
    edges = Hash.new { |hash, id| hash[id] = [] }
    ways.each do |way|
        speed = speeds[way[:kind]]
        next unless speed

        metres_per_second = speed / 3.6
        way[:points].each_cons(2) do |(a, lon1, lat1), (b, lon2, lat2)|
            nodes[a] = [lon1, lat1]
            nodes[b] = [lon2, lat2]
            seconds = distance(lon1, lat1, lon2, lat2) / metres_per_second
            forward = mode != 'car' || way[:oneway] != '-1'
            backward = mode != 'car' || way[:oneway] == ''
            edges[a] << [b, seconds] if forward
            edges[b] << [a, seconds] if backward
        end
    end
    { nodes: nodes, edges: edges, fastest: speeds.values.max / 3.6 }
end

# Eine Rundfahrt: costs[i][j] sagt, wie weit es von Ort i zu Ort j ist. Eine
# Reihenfolge beginnt bei Ort 0 und kehrt am Ende zu ihm zurück.
def tour_length(costs, order)
    (order + [order[0]]).each_cons(2).sum { |a, b| costs[a][b] }
end

# Alle Reihenfolgen ausprobieren: sicher die beste, aber bei n Orten sind es
# (n − 1)! viele
def best_tour(costs)
    rest = (1...costs.size).to_a
    rest.permutation.map { |order| [0] + order }.min_by { |order| tour_length(costs, order) } || [0]
end

# Immer zum nächsten Ort, an dem man noch nicht war: schnell, aber oft nicht
# die beste
def nearest_tour(costs)
    order = [0]
    left = (1...costs.size).to_a
    until left.empty?
        nearest = left.min_by { |i| costs[order.last][i] }
        order << nearest
        left.delete(nearest)
    end
    order
end

# 2-opt: ein Stück der Rundfahrt umdrehen, wenn sie dadurch kürzer wird, und
# das so lange, bis kein Umdrehen mehr hilft
def improve_tour(costs, order)
    order = order.dup
    better = true
    while better
        better = false
        (1...order.size - 1).each do |i|
            (i + 1...order.size).each do |j|
                turned = order[0...i] + order[i..j].reverse + order[j + 1..]
                if tour_length(costs, turned) < tour_length(costs, order) - 1e-9
                    order = turned
                    better = true
                end
            end
        end
    end
    order
end
