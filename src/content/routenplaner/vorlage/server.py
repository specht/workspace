# Der Server des Routenplaners: Er liefert die Karte an den Browser und fragt
# deine Funktionen in routes.py nach Nachbarn und Routen.
#
#   python3 server.py                      # Steglitz-Zehlendorf
#   AREA=mitte python3 server.py           # ein anderer Bezirk, oder AREA=berlin für ganz Berlin
#   AREA=mitte,pankow python3 server.py    # mehrere Bezirke zusammen
#
# Nach jeder Änderung an routes.py: mit Strg+C beenden und neu starten.

import gzip
import json
import logging
import math
import os
import sys
import time
import urllib.request

import flask

import routes

HERE = os.path.dirname(os.path.abspath(__file__))

# Die zwölf Bezirke und ganz Berlin; was nicht in data/ liegt, lädt der
# Server beim ersten Mal herunter
AREAS = ['berlin', 'charlottenburg-wilmersdorf', 'friedrichshain-kreuzberg', 'lichtenberg', 'marzahn-hellersdorf',
         'mitte', 'neukoelln', 'pankow', 'reinickendorf', 'spandau', 'steglitz-zehlendorf', 'tempelhof-schoeneberg',
         'treptow-koepenick']
DOWNLOADS = 'https://github.com/specht/workspace-files/raw/main/street-archive'


def say(text):
    print(text, flush=True)  # jede Meldung sofort, nicht erst am Ende


def data_file(name, suffix=''):
    return os.path.join(HERE, 'data', f'{name}{suffix}.tsv')


def download(area):
    if area not in AREAS:
        sys.exit(f'Kein Gebiet {area}. Es gibt: {", ".join(AREAS)}')
    say(f'Lade {area} herunter …')
    with urllib.request.urlopen(f'{DOWNLOADS}/{area}.tsv.gz') as response:
        with open(data_file(area) + '.part', 'wb') as f:
            f.write(gzip.decompress(response.read()))
    with urllib.request.urlopen(f'{DOWNLOADS}/{area}-places.tsv') as response:
        with open(data_file(area, '-places'), 'wb') as f:
            f.write(response.read())
    os.rename(data_file(area) + '.part', data_file(area))


def unique_lines(paths):
    seen = {}
    for path in paths:
        with open(path, encoding='utf-8') as f:
            for line in f:
                seen.setdefault(line, None)
    return ''.join(seen)


# Mehrere Gebiete, durch Kommas getrennt, werden zu einer Datei: Wege an
# einer Grenze stehen in beiden Bezirken, in der Datei nur einmal
PARTS = sorted({part.strip() for part in os.environ.get('AREA', 'steglitz-zehlendorf').split(',') if part.strip()})
for part in PARTS:
    if not os.path.exists(data_file(part)):
        download(part)
AREA = '+'.join(PARTS)
FILE = data_file(AREA)
PLACES_FILE = data_file(AREA, '-places')
if len(PARTS) > 1:
    with open(FILE, 'w', encoding='utf-8') as f:
        f.write(unique_lines(data_file(part) for part in PARTS))
    with open(PLACES_FILE, 'w', encoding='utf-8') as f:
        f.write(unique_lines(data_file(part, '-places') for part in PARTS))

# Die Karte liest der Server selbst, damit sie schon da ist, bevor routes.py
# etwas kann: für jeden Weg seine Art, seine Punkte und die Nummer seines
# Namens (-1 ohne Namen).
KINDS = []
NAMES = []
NAME_INDEX = {}
MAP_WAYS = []
with open(FILE, encoding='utf-8') as f:
    for line in f:
        kind, _oneway, name, points = line.rstrip('\n').split('\t', 3)
        if kind not in KINDS:
            KINDS.append(kind)
        if name and name not in NAME_INDEX:
            NAME_INDEX[name] = len(NAMES)
            NAMES.append(name)
        coordinates = [float(x) for position in points.split(' ')[1::2] for x in position.split(',')]
        MAP_WAYS.append([KINDS.index(kind), coordinates, NAME_INDEX[name] if name else -1])
# Die Orte zum Starten und Ankommen: Sehenswürdigkeiten, Plätze und Schulen, nur
# öffentliche Orte
PLACES = []
if os.path.exists(PLACES_FILE):
    with open(PLACES_FILE, encoding='utf-8') as f:
        for line in f:
            kind, name, position = line.rstrip('\n').split('\t')
            lon, lat = map(float, position.split(','))
            PLACES.append({'kind': kind, 'name': name, 'lon': lon, 'lat': lat})
MAP_JSON = json.dumps({'area': AREA, 'program': 'routes.py', 'kinds': KINDS, 'names': NAMES,
                       'ways': MAP_WAYS, 'places': PLACES}, separators=(',', ':'))
say(f'{AREA}: {len(MAP_WAYS)} Wege, {len(PLACES)} Orte')


# Was routes.py schon kann: der Graph, einmal beim Start gebaut
def attempt(function, *args):
    try:
        return function(*args)
    except NotImplementedError as e:
        say(f'Noch nicht fertig: {e}')
        return None


WAYS = attempt(routes.read_ways, FILE)
GRAPH = WAYS and attempt(routes.build_graph, WAYS)
if GRAPH:
    say(f'Graph: {len(GRAPH["nodes"])} Knoten')


# Der Knoten eines Graphen, der einem Punkt am nächsten liegt. Ein Grad Länge
# ist in Berlin nur gut 0,6-mal so lang wie ein Grad Breite.
def nearest(graph, lon, lat):
    scale = math.cos(lat * math.pi / 180)
    return min(graph['nodes'].items(), key=lambda item: ((item[1][0] - lon) * scale) ** 2 + (item[1][1] - lat) ** 2)[0]


# Die Graphen für die Arten zu reisen (build_travel_graph), jeder erst gebaut,
# wenn ihn jemand braucht; »length« ist der Graph aus build_graph, in Metern
MODES = {'length': 'Strecke', 'foot': 'zu Fuß', 'bike': 'mit dem Rad', 'car': 'mit dem Auto'}
TRAVEL = {}


def graph_for(mode):
    if mode == 'length' or mode not in MODES:
        return GRAPH
    if mode not in TRAVEL:
        TRAVEL[mode] = routes.build_travel_graph(WAYS, mode)
        say(f'Graph {MODES[mode]}: {len(TRAVEL[mode]["nodes"])} Knoten')
    return TRAVEL[mode]


# Meter zwischen zwei Punkten, wie in distance, für die Länge eines Weges in
# jedem Graphen
def metres_between(a, b):
    radians = math.pi / 180
    x = (b[0] - a[0]) * radians * math.cos((a[1] + b[1]) / 2 * radians)
    y = (b[1] - a[1]) * radians
    return math.sqrt(x * x + y * y) * 6_371_000


# Ein Klick mit Strg auf die Adresse öffnet die Karte; VS Code leitet sie
# an eine Adresse weiter, die der Browser erreicht
say('Die Karte: http://127.0.0.1:4567 (mit Strg anklicken)')
say('Beenden mit Strg+C.')

app = flask.Flask(__name__, static_folder=os.path.join(HERE, 'public'), static_url_path='')


def answer(**values):
    return flask.Response(json.dumps(values, separators=(',', ':')), mimetype='application/json')


@app.get('/')
def index():
    return app.send_static_file('index.html')


@app.get('/map.json')
def map_json():
    return flask.Response(MAP_JSON, mimetype='application/json')


# Ein Knoten und seine Nachbarn, mit Abstand: für einen Klick auf die Karte
@app.get('/neighbours')
def neighbours():
    if not GRAPH:
        return answer(missing='build_graph')
    id = nearest(GRAPH, float(flask.request.args['lon']), float(flask.request.args['lat']))
    return answer(id=id, at=GRAPH['nodes'][id],
                  neighbours=[GRAPH['nodes'][other] + [metres] for other, metres in GRAPH['edges'].get(id, [])])


# Der ganze Graph, für die Ansicht »Graph«: die Knoten als Länge und Breite
# hintereinander, die Kanten als Paare von Nummern der Knoten, jede nur einmal
@app.get('/graph.json')
def graph_json():
    if not GRAPH:
        return answer(missing='build_graph')
    index = {id: i for i, id in enumerate(GRAPH['nodes'])}
    edges = []
    seen = set()
    for id, others in GRAPH['edges'].items():
        for other, _metres in others:
            if id not in index or other not in index:
                continue
            pair = tuple(sorted((index[id], index[other])))
            if pair in seen:
                continue
            seen.add(pair)
            edges.extend(pair)
    return answer(nodes=[x for position in GRAPH['nodes'].values() for x in position], edges=edges)


# Die Suchen in routes.py, wie die Seite sie nennt
SEARCHES = {'bfs': ('breadth_first', 'Breitensuche'), 'dijkstra': ('dijkstra', 'Dijkstra'), 'astar': ('a_star', 'A*')}


# Was ein Weg aus IDs in einem Graphen kostet: die Kanten zwischen je zwei
# Knoten, in Metern oder Sekunden
def cost_of(graph, path):
    total = 0
    for a, b in zip(path, path[1:]):
        costs = [cost for id, cost in graph['edges'].get(a, []) if id == b]
        total += min(costs) if costs else 0
    return total


def metres_of(graph, path):
    return sum(metres_between(graph['nodes'][a], graph['nodes'][b]) for a, b in zip(path, path[1:]))


# Die Kanten, die die Suche angesehen hat: Kommt ein Knoten dran, leuchten
# seine Kanten zu den Nachbarn auf, die schon vor ihm dran waren. Als Paare
# von Nummern in visited, in der Reihenfolge, in der sie aufleuchten.
def explored(graph, visited):
    order = {}
    for i, id in enumerate(visited):
        order.setdefault(id, i)
    edges = []
    for i, id in enumerate(visited):
        if order[id] != i:
            continue
        for other, _cost in graph['edges'].get(id, []):
            j = order.get(other)
            if j is not None and j < i:
                edges.extend((j, i))
    return edges


# Der Graph und die Suche, die eine Anfrage will, oder was dafür noch fehlt
def prepare(args):
    if not GRAPH:
        return None, None, 'build_graph'
    function, name = SEARCHES.get(args.get('search'), SEARCHES['bfs'])
    mode = args.get('mode') if args.get('mode') in MODES else 'length'
    try:
        graph = graph_for(mode)
    except NotImplementedError:
        return None, None, 'build_travel_graph'
    return graph, (getattr(routes, function), name, mode), None


def point(text):
    lon, lat = map(float, text.split(','))
    return lon, lat


# Eine Route von einem Punkt zum anderen, gesucht mit deiner Funktion: der
# Weg, die besuchten Knoten (für das Bild der Suche) und wie lange es gedauert hat
@app.get('/route')
def route():
    graph, search, missing = prepare(flask.request.args)
    if missing:
        return answer(missing=missing)
    function, name, mode = search
    start = nearest(graph, *point(flask.request.args['from']))
    goal = nearest(graph, *point(flask.request.args['to']))
    began = time.perf_counter()
    try:
        result = function(graph, start, goal)
    except NotImplementedError as e:
        return answer(missing=str(e))
    took = time.perf_counter() - began
    path = result.get('path')
    visited = result.get('visited') or []
    metres = path and metres_of(graph, path)
    cost = path and cost_of(graph, path)
    time_text = '' if mode == 'length' or not path else f', {round(cost / 60)} min {MODES[mode]}'
    say(f'{name}: {f"{len(path)} Knoten im Weg, {round(metres)} m{time_text}" if path else "kein Weg"}, '
        f'{len(visited)} besucht, {took:.3f} s')
    return answer(path=path and [graph['nodes'][id] for id in path], metres=metres, mode=mode,
                  seconds=None if mode == 'length' else cost, took=took,
                  visited=[round(x, 6) for id in visited for x in graph['nodes'][id]],
                  edges=explored(graph, visited))


# Eine Rundfahrt durch mehrere Orte: erst mit der Suche, wie weit es von
# jedem zu jedem ist (das dauert), dann mit deiner Funktion die Reihenfolge.
# Die Tabelle der Abstände bleibt für die nächste Frage mit denselben Orten.
TOURS = {'best': ('best_tour', 'Alle Reihenfolgen'), 'nearest': ('nearest_tour', 'Nächster Nachbar'),
         'improved': ('improve_tour', '2-opt')}
UNREACHABLE = 1e12
LAST_TABLE = {}


@app.get('/tour')
def tour():
    graph, search, missing = prepare(flask.request.args)
    if missing:
        return answer(missing=missing)
    function, name, mode = search
    stops = [nearest(graph, *point(text)) for text in flask.request.args.get('stops', '').split(';') if text]
    if len(stops) < 3:
        return answer(error='Eine Rundfahrt braucht mindestens drei Orte.')

    key = (tuple(stops), function.__name__, mode)
    if LAST_TABLE.get('key') != key:
        began = time.perf_counter()
        paths = {}
        costs = []
        for i, a in enumerate(stops):
            row = []
            for j, b in enumerate(stops):
                if i == j:
                    row.append(0)
                    continue
                try:
                    path = function(graph, a, b).get('path')
                except NotImplementedError as e:
                    return answer(missing=str(e))
                paths[i, j] = path
                row.append(cost_of(graph, path) if path else UNREACHABLE)
            costs.append(row)
        LAST_TABLE.clear()
        LAST_TABLE.update(key=key, costs=costs, paths=paths, took=time.perf_counter() - began)
        say(f'Rundfahrt: {len(stops)} Orte, {len(stops) * (len(stops) - 1)} Suchen mit {name}, '
            f'{LAST_TABLE["took"]:.2f} s')
    costs = LAST_TABLE['costs']

    method, method_name = TOURS.get(flask.request.args.get('method'), TOURS['nearest'])
    if method == 'best_tour' and len(stops) > 9:
        return answer(error='Alle Reihenfolgen sind bei mehr als 9 Orten zu viele.')
    began = time.perf_counter()
    try:
        if method == 'improve_tour':
            order = routes.improve_tour(costs, routes.nearest_tour(costs))
        else:
            order = getattr(routes, method)(costs)
        total = routes.tour_length(costs, order)
    except NotImplementedError as e:
        return answer(missing=str(e))
    took = time.perf_counter() - began
    if total >= UNREACHABLE:
        return answer(error='Einen der Orte erreicht die Suche nicht.')

    order = list(order)
    pairs = list(zip(order, order[1:] + order[:1]))
    legs = [[graph['nodes'][id] for id in LAST_TABLE['paths'][i, j]] for i, j in pairs]
    metres = sum(metres_of(graph, LAST_TABLE['paths'][i, j]) for i, j in pairs)
    say(f'{method_name}: {" → ".join(str(i + 1) for i in order)} → {order[0] + 1}, {metres / 1000:.1f} km')
    return answer(order=order, legs=legs, metres=metres, mode=mode,
                  seconds=None if mode == 'length' else total, took=took, table=LAST_TABLE['took'])


if __name__ == '__main__':
    # Flask ohne seine Meldungen, damit deine zu sehen sind
    logging.getLogger('werkzeug').setLevel(logging.ERROR)
    flask.cli.show_server_banner = lambda *args: None
    app.run(host='0.0.0.0', port=4567, threaded=True)
