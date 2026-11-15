# Der Server des Routenplaners: Er liefert die Karte an den Browser und fragt
# deine Funktionen nach Nachbarn und Routen. Gestartet wird er mit ./run.
#
#   ./run                      # die Sprache, deren Ordner du zuletzt geändert hast
#   ./run dart                 # ruby, python, dart, javascript oder java
#   AREA=mitte ./run           # ein anderer Bezirk, oder AREA=berlin für ganz Berlin
#   AREA=mitte,pankow ./run    # mehrere Bezirke zusammen
#
# Deine Funktionen stehen im Ordner deiner Sprache (ruby/routes.rb,
# python/routes.py, dart/routes.dart, javascript/routes.js, java/Routes.java).
# Der Server startet sie als eigenes Programm, die Brücke (bridge.* im selben
# Ordner), und fragt sie über eine Verbindung auf diesem Rechner. Änderst du
# etwas im Ordner, startet er sie von selbst neu, sobald du speicherst.

LANGUAGE_WANTED = ARGV.shift # vor Sinatra, das ARGV sonst selbst liest

require 'fileutils'
require 'json'
require 'monitor'
require 'open-uri'
require 'open3'
require 'sinatra'
require 'socket'
require 'tmpdir'
require 'zlib'

$stdout.sync = true # jede Meldung sofort, nicht erst am Ende

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

# Die Sprachen: für jede die Datei mit deinen Funktionen und wie die Brücke
# startet, im Ordner der Sprache
LANGUAGES = {
    'ruby' => ['ruby/routes.rb', %w[ruby bridge.rb]],
    'python' => ['python/routes.py', %w[python3 -u bridge.py]],
    'dart' => ['dart/routes.dart', %w[dart run bridge.dart]],
    'javascript' => ['javascript/routes.js', %w[node bridge.js]],
    'java' => ['java/Routes.java', %w[java Bridge]],
}.freeze

# Ohne Angabe die Sprache, deren Datei zuletzt geändert wurde; gleich alt
# (gerade geklont): Ruby
def chosen_language
    if LANGUAGE_WANTED
        abort "Keine Sprache #{LANGUAGE_WANTED}. Es gibt: #{LANGUAGES.keys.join(', ')}" unless LANGUAGES.key?(LANGUAGE_WANTED)
        return LANGUAGE_WANTED
    end
    times = LANGUAGES.to_h { |name, (program, _)| [name, File.mtime(File.join(__dir__, program)).to_f] }
    newest, time = times.max_by(&:last)
    time - times.values.min < 1 ? 'ruby' : newest
end

LANGUAGE = chosen_language
PROGRAM = LANGUAGES[LANGUAGE][0]

# Die Brücke: deine Funktionen als eigenes Programm. Der Server schickt ihr
# eine Frage als eine Zeile JSON und liest die Antwort als eine Zeile, über
# eine Verbindung auf 127.0.0.1; was dein Programm ausgibt, erscheint im
# Terminal. Ändert sich eine Datei im Ordner, startet sie neu (Java: erst
# javac). Die Graphen, die sie gebaut hat, merkt sich der Server, bis sie
# neu startet.
class Bridge
    attr_reader :failure

    def initialize(language)
        @language = language
        @program, @command = LANGUAGES[language]
        @folder = File.join(__dir__, language)
        @listener = TCPServer.new('127.0.0.1', 0)
        @lock = Monitor.new
        @graphs = {}
        @output = []
    end

    # Die Dateien im Ordner mit Zeit und Größe, ohne was beim Übersetzen entsteht
    def fingerprint
        Dir.glob('**/*', base: @folder).sort.filter_map do |name|
            path = File.join(@folder, name)
            next if !File.file?(path) || name.end_with?('.class', '.pyc') || name.start_with?('.dart_tool/')

            [name, File.mtime(path).to_f, File.size(path)]
        end
    end

    def changed? = @lock.synchronize { fingerprint != @print }

    # Eine Frage an die Brücke; startet sie vorher, wenn nötig. Antwortet sie
    # nicht (abgestürzt, zu lange), steht der Grund unter 'error'.
    def ask(request, timeout: 300)
        @lock.synchronize do
            start unless running?
            return { 'error' => @failure } if @failure

            @socket.write("#{JSON.generate(request)}\n")
            line = IO.select([@socket], nil, nil, timeout) ? @socket.gets : :late
            if line.nil? || line == :late
                why = line == :late ? "keine Antwort nach #{timeout} Sekunden" : 'beendet'
                stop
                return { 'error' => "#{@program}: #{why}. #{last_error}".strip }
            end
            JSON.parse(line)
        end
    end

    # Der Graph für eine Art zu reisen ('length': build_graph), gebaut von
    # deinen Funktionen: { 'graph' => … } oder 'missing' oder 'error'
    def graph(mode)
        @lock.synchronize do
            start unless running?
            return { 'graph' => @graphs[mode] } if @graphs[mode]

            answer = ask({ 'op' => 'graph', 'mode' => mode, 'file' => FILE })
            return answer unless answer['graph']

            g = answer['graph']
            @graphs[mode] = { nodes: g['nodes'] || {}, edges: g['edges'] || {}, fastest: g['fastest'] }
            puts "Graph#{mode == 'length' ? '' : " #{MODES[mode]}"}: #{@graphs[mode][:nodes].size} Knoten"
            { 'graph' => @graphs[mode] }
        end
    end

    def running?
        return false unless @print == fingerprint
        return true if @failure # ein Fehler bleibt, bis sich eine Datei ändert

        !@pid.nil? && Process.waitpid(@pid, Process::WNOHANG).nil?
    rescue Errno::ECHILD
        false
    end

    def start
        again = !@print.nil?
        stop
        @print = fingerprint
        @graphs.clear
        @failure = nil
        @output = []
        puts again ? "#{@program} geändert, die Brücke startet neu …" : "Deine Funktionen: #{@program}"
        command = @command
        if @language == 'java'
            build = File.join(Dir.tmpdir, "routenplaner-java-#{Process.pid}")
            FileUtils.rm_rf(build)
            FileUtils.mkdir_p(build)
            out, status = Open3.capture2e('javac', '-encoding', 'UTF-8', '-d', build, *Dir[File.join(@folder, '*.java')].sort)
            unless status.success?
                puts out
                @output = out.lines
                return @failure = "#{@program} lässt sich nicht übersetzen. #{last_error}".strip
            end
            command = ['java', '-cp', build, 'Bridge']
        end
        reader, writer = IO.pipe
        begin
            @pid = Process.spawn({ 'ROUTES_PORT' => @listener.addr[1].to_s }, *command, chdir: @folder, out: writer, err: writer)
        rescue SystemCallError => e
            return @failure = "#{command.first} lässt sich nicht starten: #{e.message}"
        ensure
            writer.close
        end
        Thread.new do
            reader.each_line do |line|
                print line
                @output << line
                @output.shift while @output.size > 40
            end
        end
        # warten, bis sie sich meldet, oder bis sie aufgibt (ein Fehler beim Laden)
        deadline = Process.clock_gettime(Process::CLOCK_MONOTONIC) + 120
        until IO.select([@listener], nil, nil, 0.2)
            if Process.waitpid(@pid, Process::WNOHANG) || Process.clock_gettime(Process::CLOCK_MONOTONIC) > deadline
                sleep 0.2 # die letzten Zeilen ihrer Ausgabe
                @pid = nil
                return @failure = "#{@program}: Fehler beim Start. #{last_error}".strip
            end
        end
        @socket = @listener.accept
        nil
    end

    def stop
        @socket&.close rescue nil
        @socket = nil
        return unless @pid

        Process.kill('TERM', @pid) rescue nil
        20.times { break if (Process.waitpid(@pid, Process::WNOHANG) rescue true); sleep 0.05 }
        Process.kill('KILL', @pid) rescue nil
        Process.waitpid(@pid) rescue nil
        @pid = nil
    end

    # Die Zeile der Ausgabe, die den Fehler am besten nennt
    def last_error
        lines = @output.map { |l| l.strip.gsub("#{__dir__}/", '') }.reject(&:empty?)
        lines.find { |l| l.match?(/error|exception|fehler|cannot|undefined|not found/i) } || lines.last.to_s
    end
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
MAP_JSON = JSON.generate(area: AREA, program: PROGRAM, kinds: KINDS, names: NAMES, ways: MAP_WAYS, places: PLACES)
puts "#{AREA}: #{MAP_WAYS.size} Wege, #{PLACES.size} Orte"

BRIDGE = Bridge.new(LANGUAGE)
at_exit { BRIDGE.stop }

# Was deine Funktionen schon können: der Graph, beim Start und nach jeder
# Änderung gleich gebaut, damit die Karte nicht warten muss
def warm_up
    answer = BRIDGE.graph('length')
    puts "Noch nicht fertig: #{answer['missing']}" if answer['missing']
end
Thread.new do
    warm_up
    loop do
        sleep 1
        warm_up if BRIDGE.changed?
    end
end

# Ein Fehler deines Programms: an die Seite, die ihn anzeigt
def failed(message)
    halt 500, JSON.generate(error: message)
end

# Der Knoten eines Graphen, der einem Punkt am nächsten liegt. Ein Grad Länge
# ist in Berlin nur gut 0,6-mal so lang wie ein Grad Breite.
def nearest(graph, lon, lat)
    scale = Math.cos(lat * Math::PI / 180)
    graph[:nodes].min_by { |_id, (x, y)| ((x - lon) * scale)**2 + (y - lat)**2 }.first
end

# Die Graphen für die Arten zu reisen (build_travel_graph), jeder erst gebaut,
# wenn ihn jemand braucht; »length« ist der Graph aus build_graph, in Metern
MODES = { 'length' => 'Strecke', 'foot' => 'zu Fuß', 'bike' => 'mit dem Rad', 'car' => 'mit dem Auto' }.freeze

# { 'graph' => … } oder was fehlt ('missing') oder schiefging ('error')
def graph_for(mode)
    BRIDGE.graph(MODES.key?(mode) ? mode : 'length')
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
    answer = graph_for('length')
    return JSON.generate(missing: answer['missing']) if answer['missing']
    failed(answer['error']) if answer['error']

    graph = answer['graph']
    id = nearest(graph, params['lon'].to_f, params['lat'].to_f)
    JSON.generate(id: id, at: graph[:nodes][id],
                  neighbours: (graph[:edges][id] || []).filter_map { |other, metres| graph[:nodes][other] && graph[:nodes][other] + [metres] })
end

# Der ganze Graph, für die Ansicht »Graph«: die Knoten als Länge und Breite
# hintereinander, die Kanten als Paare von Nummern der Knoten, jede nur einmal
get '/graph.json' do
    content_type :json
    answer = graph_for('length')
    return JSON.generate(missing: answer['missing']) if answer['missing']
    failed(answer['error']) if answer['error']

    graph = answer['graph']
    index = {}
    graph[:nodes].each_key { |id| index[id] = index.size }
    edges = []
    seen = {}
    graph[:edges].each do |id, neighbours|
        neighbours.each do |other, _metres|
            next unless index[id] && index[other]

            a, b = [index[id], index[other]].sort
            next if seen[[a, b]]

            seen[[a, b]] = true
            edges.push(a, b)
        end
    end
    JSON.generate(nodes: graph[:nodes].values.flatten, edges: edges)
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
    function, name = SEARCHES.fetch(params['search'], SEARCHES['bfs'])
    mode = MODES.key?(params['mode']) ? params['mode'] : 'length'
    answer = graph_for('length')
    answer = graph_for(mode) if answer['graph'] && mode != 'length'
    failed(answer['error']) if answer['error']
    return [nil, nil, answer['missing']] if answer['missing']

    [answer['graph'], [function, name, mode], nil]
end

# Eine Suche deiner Funktionen: { path:, visited:, took: } in Sekunden, gemessen
# in der Brücke, ohne den Weg dorthin. Fehlt etwas: { missing: … }.
def search(function, mode, start, goal, graph)
    answer = BRIDGE.ask({ 'op' => 'search', 'fn' => function.to_s, 'mode' => mode, 'start' => start, 'goal' => goal })
    return { missing: answer['missing'] } if answer['missing']
    failed(answer['error']) if answer['error']

    path = answer['path']
    visited = answer['visited'] || []
    unknown = [*path, *visited].find { |id| !graph[:nodes].key?(id) }
    failed("#{function} gibt eine ID zurück, die nicht im Graphen ist: #{unknown.inspect}") if unknown

    { path: path, visited: visited, took: answer['took'].to_f }
end

# Eine deiner Funktionen für die Rundfahrt mit ihren Werten
def call_function(function, *args)
    answer = BRIDGE.ask({ 'op' => 'call', 'fn' => function.to_s, 'args' => args })
    throw :missing, answer['missing'] if answer['missing']
    failed(answer['error']) if answer['error']

    answer['result']
end

# Eine Route von einem Punkt zum anderen, gesucht mit deiner Funktion: der
# Weg, die besuchten Knoten (für das Bild der Suche) und wie lange es gedauert hat
get '/route' do
    content_type :json
    graph, (function, name, mode), missing = prepare(params)
    return JSON.generate(missing: missing) if missing

    start = nearest(graph, *params['from'].split(',').map(&:to_f))
    goal = nearest(graph, *params['to'].split(',').map(&:to_f))
    result = search(function, mode, start, goal, graph)
    return JSON.generate(missing: result[:missing]) if result[:missing]

    took = result[:took]
    path = result[:path]
    visited = result[:visited]
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

                result = search(function, mode, a, b, graph)
                return JSON.generate(missing: result[:missing]) if result[:missing]

                path = result[:path]
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
    order = total = nil
    missing = catch(:missing) do
        order = method == :improve_tour ? call_function(:improve_tour, costs, call_function(:nearest_tour, costs)) : call_function(method, costs)
        total = call_function(:tour_length, costs, order)
        nil
    end
    return JSON.generate(missing: missing) if missing

    took = clock - began
    return JSON.generate(error: 'Einen der Orte erreicht die Suche nicht.') if total >= UNREACHABLE

    legs = (order + [order[0]]).each_cons(2).map { |i, j| LAST_TABLE[:paths][[i, j]].map { |id| graph[:nodes][id] } }
    metres = (order + [order[0]]).each_cons(2).sum { |i, j| metres_of(graph, LAST_TABLE[:paths][[i, j]]) }
    puts "#{method_name}: #{order.map { |i| i + 1 }.join(' → ')} → #{order[0] + 1}, #{(metres / 1000).round(1)} km"
    JSON.generate(order: order, legs: legs, metres: metres, mode: mode,
                  seconds: mode == 'length' ? nil : total, took: took, table: LAST_TABLE[:took])
end
