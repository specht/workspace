# Die Brücke zwischen dem Server und deinen Funktionen in routes.py. Der
# Server startet sie (./run python) und schickt ihr Fragen, eine Zeile JSON
# je Frage, über eine Verbindung auf diesem Rechner; sie antwortet ebenso. Was
# routes.py ausgibt, erscheint im Terminal. Hier musst du nichts ändern.
import json
import os
import socket
import sys
import time
import traceback

import routes

connection = socket.create_connection(('127.0.0.1', int(os.environ['ROUTES_PORT'])))
reader = connection.makefile('r', encoding='utf-8')
ways = {}    # Datei -> Wege
graphs = {}  # Art zu reisen -> Graph


def graph_of(mode, file):
    if mode not in graphs:
        if file not in ways:
            ways[file] = routes.read_ways(file)
        graphs[mode] = routes.build_graph(ways[file]) if mode == 'length' else routes.build_travel_graph(ways[file], mode)
    return graphs[mode]


def answer(request):
    op = request['op']
    if op == 'graph':
        return {'graph': graph_of(request['mode'], request['file'])}
    if op == 'search':
        graph = graphs.get(request['mode'])
        if graph is None:
            raise RuntimeError('Kein Graph für ' + request['mode'])
        began = time.perf_counter()
        result = getattr(routes, request['fn'])(graph, request['start'], request['goal'])
        took = time.perf_counter() - began
        if not isinstance(result, dict):  # walk und depth_first geben nur den Weg zurück
            result = {'path': result, 'visited': []}
        return {'path': result.get('path'), 'visited': result.get('visited') or [], 'took': took}
    if op == 'call':
        began = time.perf_counter()
        result = getattr(routes, request['fn'])(*request['args'])
        return {'result': result, 'took': time.perf_counter() - began}
    return {'error': 'Unbekannte Frage: ' + str(op)}


for line in reader:
    try:
        reply = answer(json.loads(line))
    except NotImplementedError as e:
        reply = {'missing': str(e)}
    except (Exception, RecursionError) as e:
        traceback.print_exc()
        reply = {'error': (type(e).__name__ + ': ' + str(e))[:300]}
    connection.sendall((json.dumps(reply) + '\n').encode('utf-8'))
