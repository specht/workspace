# Die Brücke zwischen dem Server und deinen Funktionen in routes.rb. Der
# Server startet sie (./run) und schickt ihr Fragen, eine Zeile JSON je
# Frage, über eine Verbindung auf diesem Rechner; sie antwortet ebenso. Was
# routes.rb ausgibt, erscheint im Terminal. Hier musst du nichts ändern.

require 'json'
require 'socket'

$stdout.sync = true
require_relative 'visits'
require_relative 'routes'

socket = TCPSocket.new('127.0.0.1', Integer(ENV.fetch('ROUTES_PORT')))
ways = {}   # Datei => Wege
graphs = {} # Art zu reisen => Graph

graph_of = lambda do |mode, file|
    graphs[mode] ||= begin
        ways[file] ||= read_ways(file)
        mode == 'length' ? build_graph(ways[file]) : build_travel_graph(ways[file], mode)
    end
end

clock = -> { Process.clock_gettime(Process::CLOCK_MONOTONIC) }

while (line = socket.gets)
    request = JSON.parse(line)
    answer = begin
        case request['op']
        when 'graph'
            { graph: graph_of.call(request['mode'], request['file']) }
        when 'search'
            graph = graphs[request['mode']] or raise "Kein Graph für #{request['mode']}"
            # jede Suche gibt ihren Weg zurück; welche Knoten dran waren, meldet sie mit visit
            VISITS.clear
            began = clock.call
            path = send(request['fn'], graph, request['start'], request['goal'])
            took = clock.call - began
            { path: path, visited: VISITS.dup, took: took }
        when 'call'
            began = clock.call
            result = send(request['fn'], *request['args'])
            { result: result, took: clock.call - began }
        else
            { error: "Unbekannte Frage: #{request['op']}" }
        end
    rescue NotImplementedError => e
        { missing: e.message }
    rescue StandardError, SystemStackError, NoMemoryError => e
        warn "#{e.class}: #{e.message}"
        warn(e.backtrace.reject { |l| l.include?('bridge.rb') }.first(8).map { |l| "    #{l}" })
        { error: "#{e.class}: #{e.message}"[0, 300] }
    end
    socket.write("#{JSON.generate(answer)}\n")
end
