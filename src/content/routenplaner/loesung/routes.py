# Ein Routenplaner: deine Funktionen. server.py ruft sie auf und zeigt im
# Browser, was sie schon können. (Die Lösung aller Schritte.)
import itertools
import math

EARTH_RADIUS = 6_371_000  # Meter


# Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
# durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
# sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
#
# Gibt eine Liste von Dictionaries zurück, eines pro Weg:
#   {'kind': 'residential', 'oneway': '', 'name': 'Schloßstraße',
#    'points': [['26735834', 13.3266, 52.4567], …]}
def read_ways(path):
    ways = []
    with open(path, encoding='utf-8') as f:
        for line in f:
            kind, oneway, name, points = line.rstrip('\n').split('\t', 3)
            items = points.split(' ')
            points = []
            for id, position in zip(items[0::2], items[1::2]):
                lon, lat = position.split(',')
                points.append([id, float(lon), float(lat)])
            ways.append({'kind': kind, 'oneway': oneway, 'name': name, 'points': points})
    return ways


# Der Abstand zweier Punkte in Metern: ein Grad Breite ist überall gleich
# lang, ein Grad Länge wird zu den Polen hin kürzer (um den Cosinus der
# Breite). Für die Strecken einer Stadt genau genug.
def distance(lon1, lat1, lon2, lat2):
    radians = math.pi / 180
    x = (lon2 - lon1) * radians * math.cos((lat1 + lat2) / 2 * radians)
    y = (lat2 - lat1) * radians
    return math.sqrt(x * x + y * y) * EARTH_RADIUS


# Der Graph aus den Wegen:
#   'nodes': {ID: [Länge, Breite]} für jeden Punkt
#   'edges': {ID: [[Nachbar-ID, Meter], …]} für jeden Punkt, in beide Richtungen
def build_graph(ways):
    nodes = {}
    edges = {}
    for way in ways:
        for (a, lon1, lat1), (b, lon2, lat2) in zip(way['points'], way['points'][1:]):
            nodes[a] = [lon1, lat1]
            nodes[b] = [lon2, lat2]
            metres = distance(lon1, lat1, lon2, lat2)
            edges.setdefault(a, []).append([b, metres])
            edges.setdefault(b, []).append([a, metres])
    return {'nodes': nodes, 'edges': edges}


# Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
# gefundenen Knoten, von welchem aus er gefunden wurde (der Start von None).
# None, wenn das Ziel nie gefunden wurde.
def path_to(previous, goal):
    if goal not in previous:
        return None
    path = []
    node = goal
    while node is not None:
        path.insert(0, node)
        node = previous[node]
    return path


# Breitensuche: der Weg über die wenigsten Kanten. Gibt den Weg zurück (die
# IDs vom Start bis zum Ziel, None ohne Weg) und alle besuchten Knoten in der
# Reihenfolge, in der die Suche sie aus der Queue genommen hat.
def breadth_first(graph, start, goal):
    previous = {start: None}
    queue = [start]
    first = 0  # queue[first] ist als Nächstes dran
    visited = []
    while first < len(queue):
        node = queue[first]
        first += 1
        visited.append(node)
        if node == goal:
            break
        for neighbour, _metres in graph['edges'].get(node, []):
            if neighbour in previous:
                continue
            previous[neighbour] = node
            queue.append(neighbour)
    return {'path': path_to(previous, goal), 'visited': visited}


# Eine Priority Queue als binärer Heap: Was hineinkommt, hat eine
# Priorität, und pop gibt immer das mit der kleinsten zurück. Die Einträge
# stehen in einer Liste, als Baum: Die Kinder von i stehen bei 2i+1 und 2i+2,
# und kein Kind ist kleiner als sein Elternteil.
class MinHeap:
    def __init__(self):
        self.items = []  # (Priorität, Wert)

    def empty(self):
        return not self.items

    def __len__(self):
        return len(self.items)

    def push(self, priority, value):
        items = self.items
        items.append((priority, value))
        i = len(items) - 1
        # nach oben, solange das Elternteil größer ist
        while i > 0:
            parent = (i - 1) // 2
            if items[parent][0] <= items[i][0]:
                break
            items[parent], items[i] = items[i], items[parent]
            i = parent

    def pop(self):
        items = self.items
        if not items:
            return None
        top = items[0]
        last = items.pop()
        if items:
            items[0] = last
            i = 0
            # nach unten, solange ein Kind kleiner ist
            while True:
                smallest = i
                for child in (2 * i + 1, 2 * i + 2):
                    if child < len(items) and items[child][0] < items[smallest][0]:
                        smallest = child
                if smallest == i:
                    break
                items[smallest], items[i] = items[i], items[smallest]
                i = smallest
        return top


# Dijkstra: der kürzeste Weg, mit einer Priority Queue. Ein Knoten
# kann mehrmals darin stehen, wenn er einen besseren Weg bekommt; der alte
# Eintrag ist dann veraltet und wird übersprungen.
def dijkstra(graph, start, goal):
    costs = {start: 0}
    previous = {start: None}
    done = set()
    heap = MinHeap()
    heap.push(0, start)
    visited = []
    while not heap.empty():
        so_far, node = heap.pop()
        if node in done:
            continue
        done.add(node)
        visited.append(node)
        if node == goal:
            break
        for neighbour, length in graph['edges'].get(node, []):
            total = so_far + length
            if neighbour in costs and costs[neighbour] <= total:
                continue
            costs[neighbour] = total
            previous[neighbour] = node
            heap.push(total, neighbour)
    return {'path': path_to(previous, goal), 'visited': visited}


# A*: wie Dijkstra, aber die Priorität ist der Weg bis hier plus eine
# Schätzung für den Rest, die Luftlinie zum Ziel. Die Schätzung ist nie zu
# groß, darum findet A* trotzdem den kürzesten Weg. In einem Graphen mit
# Sekunden statt Metern teilt man die Luftlinie durch das größte Tempo.
def a_star(graph, start, goal):
    goal_lon, goal_lat = graph['nodes'][goal]
    fastest = graph.get('fastest', 1.0)

    def estimate(id):
        lon, lat = graph['nodes'][id]
        return distance(lon, lat, goal_lon, goal_lat) / fastest

    costs = {start: 0}
    previous = {start: None}
    done = set()
    heap = MinHeap()
    heap.push(estimate(start), start)
    visited = []
    while not heap.empty():
        _priority, node = heap.pop()
        if node in done:
            continue
        done.add(node)
        visited.append(node)
        if node == goal:
            break
        for neighbour, length in graph['edges'].get(node, []):
            total = costs[node] + length
            if neighbour in costs and costs[neighbour] <= total:
                continue
            costs[neighbour] = total
            previous[neighbour] = node
            heap.push(total + estimate(neighbour), neighbour)
    return {'path': path_to(previous, goal), 'visited': visited}


# Wie schnell man auf welcher Art von Weg vorankommt, in km/h. Was in der
# Liste einer Art zu reisen fehlt, darf man so nicht benutzen.
SPEEDS = {
    'foot': {'footway': 5, 'path': 5, 'pedestrian': 5, 'steps': 2, 'living_street': 5,
             'residential': 5, 'service': 5, 'unclassified': 5, 'tertiary': 5, 'secondary': 5,
             'primary': 5, 'track': 5, 'cycleway': 5, 'bridleway': 5, 'ferry': 5},
    'bike': {'cycleway': 18, 'residential': 15, 'living_street': 10, 'service': 12,
             'unclassified': 15, 'tertiary': 16, 'secondary': 16, 'primary': 16,
             'tertiary_link': 15, 'secondary_link': 15, 'primary_link': 15,
             'path': 12, 'track': 10, 'bridleway': 10, 'ferry': 10},
    'car': {'motorway': 90, 'motorway_link': 50, 'trunk': 60, 'trunk_link': 40,
            'primary': 50, 'primary_link': 30, 'secondary': 45, 'secondary_link': 30,
            'tertiary': 40, 'tertiary_link': 30, 'unclassified': 30, 'residential': 25,
            'living_street': 7, 'service': 10, 'ferry': 10},
}


# Der Graph für eine Art zu reisen: nur die Wege, die man so benutzen darf,
# und als Länge jeder Kante, wie viele Sekunden man für sie braucht. Für das
# Auto gelten die Einbahnstraßen. 'fastest': das größte Tempo, in Metern je
# Sekunde, für die Schätzung von A*.
def build_travel_graph(ways, mode):
    speeds = SPEEDS[mode]
    nodes = {}
    edges = {}
    for way in ways:
        speed = speeds.get(way['kind'])
        if not speed:
            continue
        metres_per_second = speed / 3.6
        forward = mode != 'car' or way['oneway'] != '-1'
        backward = mode != 'car' or way['oneway'] == ''
        for (a, lon1, lat1), (b, lon2, lat2) in zip(way['points'], way['points'][1:]):
            nodes[a] = [lon1, lat1]
            nodes[b] = [lon2, lat2]
            seconds = distance(lon1, lat1, lon2, lat2) / metres_per_second
            if forward:
                edges.setdefault(a, []).append([b, seconds])
            if backward:
                edges.setdefault(b, []).append([a, seconds])
    return {'nodes': nodes, 'edges': edges, 'fastest': max(speeds.values()) / 3.6}


# Eine Rundfahrt: costs[i][j] sagt, wie weit es von Ort i zu Ort j ist. Eine
# Reihenfolge beginnt bei Ort 0 und kehrt am Ende zu ihm zurück.
def tour_length(costs, order):
    stops = list(order) + [order[0]]
    return sum(costs[a][b] for a, b in zip(stops, stops[1:]))


# Alle Reihenfolgen ausprobieren: sicher die beste, aber bei n Orten sind es
# (n − 1)! viele
def best_tour(costs):
    orders = ([0] + list(rest) for rest in itertools.permutations(range(1, len(costs))))
    return min(orders, key=lambda order: tour_length(costs, order))


# Immer zum nächsten Ort, an dem man noch nicht war: schnell, aber oft nicht
# die beste
def nearest_tour(costs):
    order = [0]
    left = list(range(1, len(costs)))
    while left:
        nearest = min(left, key=lambda i: costs[order[-1]][i])
        order.append(nearest)
        left.remove(nearest)
    return order


# 2-opt: ein Stück der Rundfahrt umdrehen, wenn sie dadurch kürzer wird, und
# das so lange, bis kein Umdrehen mehr hilft
def improve_tour(costs, order):
    order = list(order)
    better = True
    while better:
        better = False
        for i in range(1, len(order) - 1):
            for j in range(i + 1, len(order)):
                turned = order[:i] + order[i:j + 1][::-1] + order[j + 1:]
                if tour_length(costs, turned) < tour_length(costs, order) - 1e-9:
                    order = turned
                    better = True
    return order
