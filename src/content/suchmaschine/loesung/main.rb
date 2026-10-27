require_relative 'search'

pages = crawl('/index.html', method(:fetch_page))
puts "#{pages.size} Seiten gefunden: #{pages.keys.join(', ')}"
index = build_index(pages)
counts = incoming(pages)

loop do
  print 'Suche: '
  query = gets
  break if query.nil? || query.strip.empty?

  results = rank_by_links(search(index, query), counts)
  puts '  Nichts gefunden.' if results.empty?
  results.each { |path| puts "  #{path}" }
end
