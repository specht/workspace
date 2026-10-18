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
MAP_JSON = JSON.generate(area: AREA, kinds: KINDS, names: NAMES, ways: MAP_WAYS, places: PLACES)
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

# Der Knoten des Graphen, der einem Punkt am nächsten liegt. Ein Grad Länge
# ist in Berlin nur gut 0,6-mal so lang wie ein Grad Breite.
def nearest(lon, lat)
    scale = Math.cos(lat * Math::PI / 180)
    GRAPH[:nodes].min_by { |_id, (x, y)| ((x - lon) * scale)**2 + (y - lat)**2 }.first
end

puts 'Die Karte: Port 4567 (Reiter »Ports«). Beenden mit Strg+C.'

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

    id = nearest(params['lon'].to_f, params['lat'].to_f)
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
SEARCHES = { 'bfs' => [:breadth_first, 'Breitensuche'], 'dijkstra' => [:dijkstra, 'Dijkstra'] }.freeze

# Wie lang ein Weg aus IDs ist: die Kanten zwischen je zwei Knoten
def length_of(path)
    path.each_cons(2).sum { |a, b| GRAPH[:edges][a].select { |id, _| id == b }.map(&:last).min || 0 }
end

# Die Kanten, die die Suche angesehen hat: Kommt ein Knoten dran, leuchten
# seine Kanten zu den Nachbarn auf, die schon vor ihm dran waren. Als Paare
# von Nummern in visited, in der Reihenfolge, in der sie aufleuchten.
def explored(visited)
    order = {}
    visited.each_with_index { |id, i| order[id] ||= i }
    edges = []
    visited.each_with_index do |id, i|
        next unless order[id] == i

        (GRAPH[:edges][id] || []).each do |other, _metres|
            j = order[other]
            edges.push(j, i) if j && j < i
        end
    end
    edges
end

# Eine Route von einem Punkt zum anderen, gesucht mit deiner Funktion: der
# Weg, die besuchten Knoten (für das Bild der Suche) und wie lange es gedauert hat
get '/route' do
    content_type :json
    return JSON.generate(missing: 'build_graph') unless GRAPH

    function, name = SEARCHES.fetch(params['search'], SEARCHES['bfs'])
    start = nearest(*params['from'].split(',').map(&:to_f))
    goal = nearest(*params['to'].split(',').map(&:to_f))
    began = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    begin
        result = send(function, GRAPH, start, goal)
    rescue NotImplementedError
        return JSON.generate(missing: function)
    end

    seconds = Process.clock_gettime(Process::CLOCK_MONOTONIC) - began
    path = result[:path]
    visited = result[:visited] || []
    metres = path && length_of(path)
    puts "#{name}: #{path ? "#{path.size} Knoten im Weg, #{metres.round} m" : 'kein Weg'}, " \
         "#{visited.size} besucht, #{format('%.3f', seconds)} s"
    JSON.generate(path: path&.map { |id| GRAPH[:nodes][id] }, metres: metres, seconds: seconds,
                  visited: visited.flat_map { |id| GRAPH[:nodes][id].map { |x| x.round(6) } },
                  edges: explored(visited))
end
