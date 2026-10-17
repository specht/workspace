# Ein Routenplaner: deine Funktionen. server.rb ruft sie auf und zeigt im
# Browser, was sie schon können.

# Liest die Wege einer Datei aus data/. Jede Zeile ist ein Weg, die Teile sind
# durch Tabulatoren getrennt: Art, Einbahnstraße, Name, Punkte. Die Punkte
# sind »ID LÄNGE,BREITE ID LÄNGE,BREITE …«.
#
# Gibt ein Array von Hashes zurück, einen pro Weg:
#   { kind: 'residential', oneway: '', name: 'Schloßstraße',
#     points: [['26735834', 13.3266, 52.4567], …] }
def read_ways(path)
    raise NotImplementedError, 'read_ways'
end

# Der Abstand zweier Punkte in Metern
def distance(lon1, lat1, lon2, lat2)
    raise NotImplementedError, 'distance'
end

# Der Graph aus den Wegen:
#   nodes: { ID => [Länge, Breite] } für jeden Punkt
#   edges: { ID => [[Nachbar-ID, Meter], …] } für jeden Punkt, in beide Richtungen
def build_graph(ways)
    raise NotImplementedError, 'build_graph'
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
