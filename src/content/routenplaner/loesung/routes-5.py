# Ein Routenplaner: deine Funktionen. server.py ruft sie auf und zeigt im
# Browser, was sie schon können. (Die Lösung bis Schritt 5.)
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
# Reihenfolge, in der die Suche sie aus der Warteschlange genommen hat.
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


# Dijkstra: der kürzeste Weg in Metern. Als Nächstes ist immer der Knoten
# dran, der vom Start aus am wenigsten weit weg ist.
def dijkstra(graph, start, goal):
    metres = {start: 0}
    previous = {start: None}
    frontier = {start: 0}  # gefunden, aber noch nicht fertig
    visited = []
    while frontier:
        node = min(frontier, key=frontier.get)
        so_far = frontier.pop(node)
        visited.append(node)
        if node == goal:
            break
        for neighbour, length in graph['edges'].get(node, []):
            total = so_far + length
            if neighbour in metres and metres[neighbour] <= total:
                continue
            metres[neighbour] = total
            previous[neighbour] = node
            frontier[neighbour] = total
    return {'path': path_to(previous, goal), 'visited': visited}


# Eine Prioritätswarteschlange: push legt einen Wert mit einer Priorität
# hinein, pop gibt das Paar (Priorität, Wert) mit der kleinsten Priorität
# zurück und nimmt es heraus (None, wenn sie leer ist).
class MinHeap:
    def __init__(self):
        self.items = []  # (Priorität, Wert), als binärer Heap

    def empty(self):
        return not self.items

    def __len__(self):
        return len(self.items)

    def push(self, priority, value):
        raise NotImplementedError('MinHeap.push')

    def pop(self):
        raise NotImplementedError('MinHeap.pop')


# A*: wie Dijkstra, mit der Luftlinie zum Ziel als Schätzung für den Rest.
# Gibt dasselbe zurück wie breadth_first.
def a_star(graph, start, goal):
    raise NotImplementedError('a_star')


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


# Der Graph für eine Art zu reisen ('foot', 'bike', 'car'): wie build_graph,
# aber nur mit den Wegen, die man so benutzen darf, und mit Sekunden statt
# Metern an den Kanten. Für das Auto gelten die Einbahnstraßen.
#   {'nodes': …, 'edges': …, 'fastest': das größte Tempo in Metern je Sekunde}
def build_travel_graph(ways, mode):
    raise NotImplementedError('build_travel_graph')


# Eine Rundfahrt durch mehrere Orte: costs[i][j] sagt, wie weit es von Ort i
# zu Ort j ist. Eine Reihenfolge ist eine Liste von Nummern der Orte; sie
# beginnt bei 0 und kehrt am Ende zu ihrem Anfang zurück.

# Wie lang eine Rundfahrt in dieser Reihenfolge ist, mit dem Weg zurück
def tour_length(costs, order):
    raise NotImplementedError('tour_length')


# Die beste Reihenfolge, durch Ausprobieren aller
def best_tour(costs):
    raise NotImplementedError('best_tour')


# Immer zum nächsten Ort, an dem die Rundfahrt noch nicht war
def nearest_tour(costs):
    raise NotImplementedError('nearest_tour')


# Eine Reihenfolge mit 2-opt verbessern: ein Stück umdrehen, solange die
# Rundfahrt dadurch kürzer wird
def improve_tour(costs, order):
    raise NotImplementedError('improve_tour')
