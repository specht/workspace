// Die Brücke zwischen dem Server und deinen Funktionen in routes.dart. Der
// Server startet sie (./run dart) und schickt ihr Fragen, eine Zeile JSON je
// Frage, über eine Verbindung auf diesem Rechner; sie antwortet ebenso. Was
// routes.dart ausgibt, erscheint im Terminal. Hier musst du nichts ändern.
import 'dart:convert';
import 'dart:io';

import 'routes.dart';

final ways = <String, List<Way>>{}; // Datei -> Wege
final graphs = <String, Graph>{}; // Art zu reisen -> Graph

Graph graphOf(String mode, String file) => graphs.putIfAbsent(mode, () {
      final w = ways.putIfAbsent(file, () => readWays(file));
      return mode == 'length' ? buildGraph(w) : buildTravelGraph(w, mode);
    });

// Der Graph als JSON für den Server: Knoten als [Länge, Breite], Kanten als [Nachbar, Kosten]
Map<String, Object?> graphJson(Graph g) => {
      'nodes': {for (final e in g.nodes.entries) e.key: [e.value.lon, e.value.lat]},
      'edges': {for (final e in g.edges.entries) e.key: [for (final edge in e.value) [edge.to, edge.cost]]},
      'fastest': g.fastest,
    };

List<List<double>> costsOf(Object? value) =>
    [for (final row in value as List) [for (final x in row as List) (x as num).toDouble()]];
List<int> orderOf(Object? value) => [for (final x in value as List) (x as num).toInt()];

Map<String, Object?> answer(Map<String, dynamic> request) {
  switch (request['op']) {
    case 'graph':
      return {'graph': graphJson(graphOf(request['mode'], request['file']))};
    case 'search':
      final graph = graphs[request['mode']];
      if (graph == null) throw StateError('Kein Graph für ${request['mode']}');
      // jede Suche gibt ihren Weg zurück; welche Knoten dran waren, meldet sie mit visit
      final List<String>? Function(Graph, String, String) search = switch (request['fn']) {
        'walk' => walk,
        'depth_first' => depthFirst,
        'breadth_first' => breadthFirst,
        'dijkstra' => dijkstra,
        'a_star' => aStar,
        _ => throw ArgumentError('Keine Suche ${request['fn']}'),
      };
      visits.clear();
      final clock = Stopwatch()..start();
      final path = search(graph, request['start'], request['goal']);
      final took = clock.elapsedMicroseconds / 1e6;
      return {'path': path, 'visited': List.of(visits), 'took': took};
    case 'call':
      final args = request['args'] as List;
      final clock = Stopwatch()..start();
      final Object result = switch (request['fn']) {
        'tour_length' => tourLength(costsOf(args[0]), orderOf(args[1])),
        'best_tour' => bestTour(costsOf(args[0])),
        'nearest_tour' => nearestTour(costsOf(args[0])),
        'improve_tour' => improveTour(costsOf(args[0]), orderOf(args[1])),
        _ => throw ArgumentError('Keine Funktion ${request['fn']}'),
      };
      return {'result': result, 'took': clock.elapsedMicroseconds / 1e6};
  }
  return {'error': 'Unbekannte Frage: ${request['op']}'};
}

Future<void> main() async {
  final socket = await Socket.connect('127.0.0.1', int.parse(Platform.environment['ROUTES_PORT']!));
  await for (final line in utf8.decoder.bind(socket).transform(const LineSplitter())) {
    Map<String, Object?> reply;
    try {
      reply = answer(jsonDecode(line) as Map<String, dynamic>);
    } on UnimplementedError catch (e) {
      reply = {'missing': e.message ?? 'eine Funktion'};
    } catch (e, stack) {
      stderr.writeln(e);
      stderr.writeln(stack.toString().split('\n').where((l) => !l.contains('bridge.dart')).take(8).join('\n'));
      final text = '$e';
      reply = {'error': text.length > 300 ? text.substring(0, 300) : text};
    }
    socket.write('${jsonEncode(reply)}\n');
    await socket.flush();
  }
}
