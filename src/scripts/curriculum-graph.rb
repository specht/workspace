#!/usr/bin/env ruby
# The picture »Wege durch die Aktivitäten« on /lehrplan (curriculum_graph.rb):
# searches a good order of the boxes within each column and stores it in
# src/ruby/include/curriculum-graph.json, which the server only reads. Run it
# whenever an activity enters or leaves the catalog (sections.yaml) or its
# requires: changes (a curriculum: line only colours its box, which the
# server does itself); test/curriculum_graph_test.rb fails until
# then. It runs on the host (the container sees src read-only)
# and takes about one and a half minutes for each of the two pictures: the one of
# production and, where drafts (.slug in sections.yaml) change it, the one
# of development. The same activities give the same order.
#
#   src/scripts/curriculum-graph.rb          # search and store
#   src/scripts/curriculum-graph.rb --check  # only say whether it is current
require_relative '../ruby/include/curriculum_graph'

ROOT = File.expand_path('../..', __dir__)
check = ARGV.include?('--check')

layouts = []
stale = false
[['production', false], ['development', true]].each do |name, development|
    content, slugs = CurriculumGraph.read_content(ROOT, :development => development)
    nodes, edges = CurriculumGraph.inputs(content, slugs, :development => development)
    key = CurriculumGraph.key(nodes, edges)
    next if layouts.any? { |layout| layout['key'] == key }

    orders = CurriculumGraph.stored(key)
    if check
        puts "#{name}: #{orders ? 'current' : 'stale'} (#{key})"
        stale ||= orders.nil?
        next
    end
    started = Process.clock_gettime(Process::CLOCK_MONOTONIC)
    result = CurriculumGraph.search(nodes, edges)
    seconds = Process.clock_gettime(Process::CLOCK_MONOTONIC) - started
    puts format('%s: %d boxes, %d arrows, cost %.1f instead of %.1f (quick order), %.1f s',
                name, nodes.size - CurriculumGraph.frame(nodes, edges)[:alone].size, edges.size, result[:cost], result[:quick], seconds)
    layouts << { 'name' => name, 'key' => key, 'cost' => result[:cost].round(2), 'quick' => result[:quick].round(2),
                 'orders' => CurriculumGraph.dump_orders(result[:orders]) }
end
exit(stale ? 1 : 0) if check

File.write(CurriculumGraph::STORE, JSON.pretty_generate({ 'layouts' => layouts }) + "\n")
puts "Stored in #{CurriculumGraph::STORE.delete_prefix("#{ROOT}/")}."
