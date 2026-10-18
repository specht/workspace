# Ein Routenplaner: deine Funktionen. server.py ruft sie auf und zeigt im
# Browser, was sie schon können.
import math


# Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
# durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
# sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
#
# Gibt eine Liste von Dictionaries zurück, eines pro Weg:
#   {'kind': 'residential', 'oneway': '', 'name': 'Schloßstraße',
#    'points': [['26735834', 13.3266, 52.4567], …]}
def read_ways(path):
    raise NotImplementedError('read_ways')


# Der Abstand zweier Punkte in Metern
def distance(lon1, lat1, lon2, lat2):
    raise NotImplementedError('distance')


# Der Graph aus den Wegen:
#   'nodes': {ID: [Länge, Breite]} für jeden Punkt
#   'edges': {ID: [[Nachbar-ID, Meter], …]} für jeden Punkt, in beide Richtungen
def build_graph(ways):
    raise NotImplementedError('build_graph')


# Der Weg zum Ziel, rückwärts aus den Vorgängern: previous sagt für jeden
# gefundenen Knoten, von welchem aus er gefunden wurde (der Start von None).
# Gibt die IDs vom Start bis zum Ziel zurück, None, wenn das Ziel nicht dabei ist.
def path_to(previous, goal):
    raise NotImplementedError('path_to')


# Breitensuche: der Weg über die wenigsten Kanten. Gibt zurück:
#   {'path': [Start-ID, …, Ziel-ID] (oder None, wenn es keinen Weg gibt),
#    'visited': [ID, …]}  die besuchten Knoten, in der Reihenfolge der Suche
def breadth_first(graph, start, goal):
    raise NotImplementedError('breadth_first')


# Dijkstra: der kürzeste Weg in Metern. Gibt dasselbe zurück wie breadth_first.
def dijkstra(graph, start, goal):
    raise NotImplementedError('dijkstra')


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
