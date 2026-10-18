# Der Server des Routenplaners: Er liefert die Karte an den Browser und fragt
# deine Funktionen in routes.rb nach Nachbarn und Routen.
#
#   ruby server.rb                 # Steglitz-Zehlendorf
#   AREA=mitte ruby server.rb      # ein anderer Bezirk, oder AREA=berlin für ganz Berlin
#   AREA=mitte,pankow ruby server.rb   # mehrere Bezirke zusammen
#
# Nach jeder Änderung an routes.rb: mit Strg+C beenden und neu starten.

require 'json'
require 'open-uri'
require 'sinatra'
require 'zlib'

$stdout.sync = true # jede Meldung sofort, nicht erst am Ende
require_relative 'routes'

# Die zwölf Bezirke und ganz Berlin; was nicht in data/ liegt, lädt der
# Server beim ersten Mal herunter
AREAS = %w[berlin charlottenburg-wilmersdorf friedrichshain-kreuzberg lichtenberg marzahn-hellersdorf mitte
           neukoelln pankow reinickendorf spandau steglitz-zehlendorf tempelhof-schoeneberg treptow-koepenick].freeze
DOWNLOADS = 'https://github.com/specht/workspace-files/raw/main/street-archive'

def data_file(name, suffix = '') = File.join(__dir__, 'data', "#{name}#{suffix}.tsv")

def download(area)
    abort "Kein Gebiet #{area}. Es gibt: #{AREAS.join(', ')}" unless AREAS.include?(area)

    puts "Lade #{area} herunter …"
    URI.open("#{DOWNLOADS}/#{area}.tsv.gz") do |download|
        File.write("#{data_file(area)}.part", Zlib::GzipReader.new(download).read)
    end
    File.write(data_file(area, '-places'), URI.open("#{DOWNLOADS}/#{area}-places.tsv", &:read))
    File.rename("#{data_file(area)}.part", data_file(area))
end

# Mehrere Gebiete, durch Kommas getrennt, werden zu einer Datei: Wege an
# einer Grenze stehen in beiden Bezirken, in der Datei nur einmal
PARTS = ENV.fetch('AREA', 'steglitz-zehlendorf').split(',').map(&:strip).reject(&:empty?).uniq.sort
PARTS.each { |area| download(area) unless File.exist?(data_file(area)) }
AREA = PARTS.join('+')
FILE = data_file(AREA)
PLACES_FILE = data_file(AREA, '-places')
if PARTS.size > 1
    File.write(FILE, PARTS.flat_map { |area| File.readlines(data_file(area)) }.uniq.join)
    File.write(PLACES_FILE, PARTS.flat_map { |area| File.readlines(data_file(area, '-places')) }.uniq.join)
end

set :port, 4567
set :bind, '0.0.0.0'
# Die Vorschau des Workspaces kommt über eine eigene Adresse, nicht localhost
set :host_authorization, { permitted_hosts: [] }
set :public_folder, File.join(__dir__, 'public')
set :logging, false
# Puma ohne seine Meldungen beim Start, damit deine zu sehen sind
set :server_settings, { Silent: true }

# Die Karte liest der Server selbst, damit sie schon da ist, bevor routes.rb
# etwas kann: für jeden Weg seine Art, seine Punkte und die Nummer seines
# Namens (-1 ohne Namen).
KINDS = []
NAMES = []
NAME_INDEX = {}
MAP_WAYS = File.foreach(FILE).map do |line|
    kind, _oneway, name, points = line.chomp.split("\t", 4)
    KINDS << kind unless KINDS.include?(kind)
    named = name.empty? ? -1 : NAME_INDEX[name] ||= (NAMES << name).size - 1
    coordinates = points.split(' ').each_slice(2).flat_map { |_id, position| position.split(',').map(&:to_f) }
    [KINDS.index(kind), coordinates, named]
end
# Die Orte zum Starten und Ankommen: Sehenswürdigkeiten, Plätze und Schulen, nur
# öffentliche Orte
PLACES = []
if File.exist?(PLACES_FILE)
    File.foreach(PLACES_FILE) do |line|
        kind, name, position = line.chomp.split("\t")
        lon, lat = position.split(',').map(&:to_f)
        PLACES << { kind: kind, name: name, lon: lon, lat: lat }
    end
end
MAP_JSON = JSON.generate(area: AREA, program: 'routes.rb', kinds: KINDS, names: NAMES, ways: MAP_WAYS, places: PLACES)
puts "#{AREA}: #{MAP_WAYS.size} Wege, #{PLACES.size} Orte"

# Was routes.rb schon kann: der Graph, einmal beim Start gebaut
def try
    yield
rescue NotImplementedError => e
    puts "Noch nicht fertig: #{e.message}"
    nil
end

WAYS = try { read_ways(FILE) }
GRAPH = WAYS && try { build_graph(WAYS) }
puts "Graph: #{GRAPH[:nodes].size} Knoten" if GRAPH

# Der Knoten eines Graphen, der einem Punkt am nächsten liegt. Ein Grad Länge
# ist in Berlin nur gut 0,6-mal so lang wie ein Grad Breite.
def nearest(graph, lon, lat)
    scale = Math.cos(lat * Math::PI / 180)
    graph[:nodes].min_by { |_id, (x, y)| ((x - lon) * scale)**2 + (y - lat)**2 }.first
end

# Die Graphen für die Arten zu reisen (build_travel_graph), jeder erst gebaut,
# wenn ihn jemand braucht; »length« ist der Graph aus build_graph, in Metern
MODES = { 'length' => 'Strecke', 'foot' => 'zu Fuß', 'bike' => 'mit dem Rad', 'car' => 'mit dem Auto' }.freeze
TRAVEL = {}

def graph_for(mode)
    return GRAPH if mode == 'length' || !MODES.key?(mode)

    TRAVEL[mode] ||= begin
        graph = build_travel_graph(WAYS, mode)
        puts "Graph #{MODES[mode]}: #{graph[:nodes].size} Knoten"
        graph
    end
end

# Meter zwischen zwei Punkten, wie in distance, für die Länge eines Weges in
# jedem Graphen
def metres_between((lon1, lat1), (lon2, lat2))
    radians = Math::PI / 180
    x = (lon2 - lon1) * radians * Math.cos((lat1 + lat2) / 2 * radians)
    y = (lat2 - lat1) * radians
    Math.sqrt(x * x + y * y) * 6_371_000
end

# Ein Klick mit Strg auf die Adresse öffnet die Karte; VS Code leitet sie
# an eine Adresse weiter, die der Browser erreicht
puts 'Die Karte: http://127.0.0.1:4567 (mit Strg anklicken)'
puts 'Beenden mit Strg+C.'

get '/' do
    send_file File.join(settings.public_folder, 'index.html')
end

get '/map.json' do
    content_type :json
    MAP_JSON
end

# Ein Knoten und seine Nachbarn, mit Abstand: für einen Klick auf die Karte
get '/neighbours' do
    content_type :json
    return JSON.generate(missing: GRAPH ? nil : 'build_graph') unless GRAPH

    id = nearest(GRAPH, params['lon'].to_f, params['lat'].to_f)
    JSON.generate(id: id, at: GRAPH[:nodes][id],
                  neighbours: (GRAPH[:edges][id] || []).map { |other, metres| GRAPH[:nodes][other] + [metres] })
end

# Der ganze Graph, für die Ansicht »Graph«: die Knoten als Länge und Breite
# hintereinander, die Kanten als Paare von Nummern der Knoten, jede nur einmal
get '/graph.json' do
    content_type :json
    return JSON.generate(missing: 'build_graph') unless GRAPH

    index = {}
    GRAPH[:nodes].each_key { |id| index[id] = index.size }
    edges = []
    seen = {}
    GRAPH[:edges].each do |id, neighbours|
        neighbours.each do |other, _metres|
            next unless index[id] && index[other]

            a, b = [index[id], index[other]].sort
            next if seen[[a, b]]

            seen[[a, b]] = true
            edges.push(a, b)
        end
    end
    JSON.generate(nodes: GRAPH[:nodes].values.flatten, edges: edges)
end

# Die Suchen in routes.rb, wie die Seite sie nennt
SEARCHES = { 'bfs' => [:breadth_first, 'Breitensuche'], 'dijkstra' => [:dijkstra, 'Dijkstra'],
             'astar' => [:a_star, 'A*'] }.freeze

# Was ein Weg aus IDs in einem Graphen kostet: die Kanten zwischen je zwei
# Knoten, in Metern oder Sekunden
def cost_of(graph, path)
    path.each_cons(2).sum { |a, b| (graph[:edges][a] || []).select { |id, _| id == b }.map(&:last).min || 0 }
end

def metres_of(graph, path)
    path.each_cons(2).sum { |a, b| metres_between(graph[:nodes][a], graph[:nodes][b]) }
end

# Die Kanten, die die Suche angesehen hat: Kommt ein Knoten dran, leuchten
# seine Kanten zu den Nachbarn auf, die schon vor ihm dran waren. Als Paare
# von Nummern in visited, in der Reihenfolge, in der sie aufleuchten.
def explored(graph, visited)
    order = {}
    visited.each_with_index { |id, i| order[id] ||= i }
    edges = []
    visited.each_with_index do |id, i|
        next unless order[id] == i

        (graph[:edges][id] || []).each do |other, _cost|
            j = order[other]
            edges.push(j, i) if j && j < i
        end
    end
    edges
end

def clock = Process.clock_gettime(Process::CLOCK_MONOTONIC)

# Der Graph und die Suche, die eine Anfrage will, oder was dafür noch fehlt
def prepare(params)
    return [nil, nil, 'build_graph'] unless GRAPH

    function, name = SEARCHES.fetch(params['search'], SEARCHES['bfs'])
    mode = MODES.key?(params['mode']) ? params['mode'] : 'length'
    begin
        graph = graph_for(mode)
    rescue NotImplementedError
        return [nil, nil, 'build_travel_graph']
    end
    [graph, [function, name, mode], nil]
end

# Eine Route von einem Punkt zum anderen, gesucht mit deiner Funktion: der
# Weg, die besuchten Knoten (für das Bild der Suche) und wie lange es gedauert hat
get '/route' do
    content_type :json
    graph, (function, name, mode), missing = prepare(params)
    return JSON.generate(missing: missing) if missing

    start = nearest(graph, *params['from'].split(',').map(&:to_f))
    goal = nearest(graph, *params['to'].split(',').map(&:to_f))
    began = clock
    begin
        result = send(function, graph, start, goal)
    rescue NotImplementedError => e
        return JSON.generate(missing: e.message)
    end

    took = clock - began
    path = result[:path]
    visited = result[:visited] || []
    metres = path && metres_of(graph, path)
    cost = path && cost_of(graph, path)
    time = mode == 'length' ? '' : ", #{(cost / 60).round} min #{MODES[mode]}" if path
    puts "#{name}: #{path ? "#{path.size} Knoten im Weg, #{metres.round} m#{time}" : 'kein Weg'}, " \
         "#{visited.size} besucht, #{format('%.3f', took)} s"
    JSON.generate(path: path&.map { |id| graph[:nodes][id] }, metres: metres, mode: mode,
                  seconds: mode == 'length' ? nil : cost, took: took,
                  visited: visited.flat_map { |id| graph[:nodes][id].map { |x| x.round(6) } },
                  edges: explored(graph, visited))
end

# Eine Rundfahrt durch mehrere Orte: erst mit der Suche, wie weit es von
# jedem zu jedem ist (das dauert), dann mit deiner Funktion die Reihenfolge.
# Die Tabelle der Abstände bleibt für die nächste Frage mit denselben Orten.
TOURS = { 'best' => [:best_tour, 'Alle Reihenfolgen'], 'nearest' => [:nearest_tour, 'Nächster Nachbar'],
          'improved' => [:improve_tour, '2-opt'] }.freeze
UNREACHABLE = 1e12
LAST_TABLE = {}

get '/tour' do
    content_type :json
    graph, (function, name, mode), missing = prepare(params)
    return JSON.generate(missing: missing) if missing

    stops = params['stops'].to_s.split(';').map { |point| nearest(graph, *point.split(',').map(&:to_f)) }
    return JSON.generate(error: 'Eine Rundfahrt braucht mindestens drei Orte.') if stops.size < 3

    key = [stops, function, mode]
    unless LAST_TABLE[:key] == key
        began = clock
        paths = {}
        costs = stops.each_with_index.map do |a, i|
            stops.each_with_index.map do |b, j|
                next 0 if i == j

                begin
                    path = send(function, graph, a, b)[:path]
                rescue NotImplementedError => e
                    return JSON.generate(missing: e.message)
                end
                paths[[i, j]] = path
                path ? cost_of(graph, path) : UNREACHABLE
            end
        end
        LAST_TABLE.replace(key: key, costs: costs, paths: paths, took: clock - began)
        puts "Rundfahrt: #{stops.size} Orte, #{stops.size * (stops.size - 1)} Suchen mit #{name}, " \
             "#{format('%.2f', LAST_TABLE[:took])} s"
    end
    costs = LAST_TABLE[:costs]

    method, method_name = TOURS.fetch(params['method'], TOURS['nearest'])
    return JSON.generate(error: 'Alle Reihenfolgen sind bei mehr als 9 Orten zu viele.') if method == :best_tour && stops.size > 9

    began = clock
    begin
        order = method == :improve_tour ? improve_tour(costs, nearest_tour(costs)) : send(method, costs)
        total = tour_length(costs, order)
    rescue NotImplementedError => e
        return JSON.generate(missing: e.message)
    end
    took = clock - began
    return JSON.generate(error: 'Einen der Orte erreicht die Suche nicht.') if total >= UNREACHABLE

    legs = (order + [order[0]]).each_cons(2).map { |i, j| LAST_TABLE[:paths][[i, j]].map { |id| graph[:nodes][id] } }
    metres = (order + [order[0]]).each_cons(2).sum { |i, j| metres_of(graph, LAST_TABLE[:paths][[i, j]]) }
    puts "#{method_name}: #{order.map { |i| i + 1 }.join(' → ')} → #{order[0] + 1}, #{(metres / 1000).round(1)} km"
    JSON.generate(order: order, legs: legs, metres: metres, mode: mode,
                  seconds: mode == 'length' ? nil : total, took: took, table: LAST_TABLE[:took])
end
